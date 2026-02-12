package gov.cookcounty.taxextension.clreb020;

import gov.cookcounty.taxextension.common.CobolFileReader;
import gov.cookcounty.taxextension.common.CobolFileWriter;
import gov.cookcounty.taxextension.common.CobolPrintWriter;

import java.io.IOException;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * Java conversion of CLREB020 — Equalization Factor Editor.
 *
 * <p>Original COBOL program written by Russ Dober and Ron Urbaniak, September 25, 1978.
 * This program edits and lists equalization factors, writing them to a disk file.
 *
 * <p>The program:
 * <ol>
 *   <li>Reads 80-character card records containing tax year, quadrant, and factor</li>
 *   <li>Validates each card (numeric checks, valid quadrant 1-4)</li>
 *   <li>Checks ascending sequence of cards (year + quadrant)</li>
 *   <li>Writes valid factors to a 21-character output file (consumed by ASHMA850/855/857)</li>
 *   <li>Produces a formatted print report listing all cards with error annotations</li>
 * </ol>
 *
 * <p>Each method in this class corresponds to a COBOL paragraph in the original source.
 * COBOL line references are included in comments for traceability.
 *
 * @see <a href="../../tax_extension_working_copy/CLREB020.cbl">CLREB020.cbl</a>
 * @see CardRecord
 * @see FactorRecord
 */
public class Clreb020 {

    // --- File paths (replace COBOL SELECT...ASSIGN TO DD names) ---
    private final Path cardFilePath;     // UT-S-CARDS
    private final Path printFilePath;    // UT-S-PRINT
    private final Path factorFilePath;   // UT-S-FACTOR

    // --- File handles ---
    private CobolFileReader cardReader;
    private CobolPrintWriter printWriter;
    private CobolFileWriter factorWriter;

    // --- Working storage (COBOL lines 61-70) ---

    /** Line counter for page break detection. COBOL: LINE-CNT PIC S999 COMP-3 VALUE +60 (line 61) */
    private int lineCount = 60;

    /** Error message constant. COBOL: ERR-MESG PIC X(11) VALUE 'NOT NUMERIC' (line 62) */
    private static final String ERR_MESG = "NOT NUMERIC";

    /** Input record counter. COBOL: IN-CNT PIC S999 COMP-3 VALUE +0 (line 63) */
    private int inCount = 0;

    /** Output record counter. COBOL: OUT-CNT PIC S999 COMP-3 VALUE +0 (line 64) */
    private int outCount = 0;

    /** Error record counter. COBOL: ERROR-CNT PIC S999 COMP-3 VALUE +0 (line 65) */
    private int errorCount = 0;

    /** Page counter. COBOL: PAGE-CNT PIC S999 COMP-3 VALUE +0 (line 66) */
    private int pageCount = 0;

    /** End-of-file flag. COBOL: CARD-EOF PIC 9 VALUE 0 / 88 END-OF-CARD-FILE VALUE 1 (lines 67-68) */
    private boolean endOfCardFile = false;

    /** Sequence error flag. COBOL: SEQ-CHECK PIC X / 88 SEQ-ERROR VALUE 'E' (lines 69-70) */
    private boolean sequenceError = false;

    /** Previous card key for sequence checking. COBOL: PREV-CARD (lines 76-78), initialized to LOW-VALUE */
    private String previousCardKey = "\u0000\u0000\u0000"; // 3 chars of LOW-VALUE

    /** Current card record. COBOL: CARD-REC */
    private CardRecord currentCard;

    /** Current error message (set for invalid cards). COBOL: WK-MESG PIC X(11) VALUE SPACES (line 105) */
    private String errorMessage = "";

    /** Return code. COBOL: RETURN-CODE special register */
    private int returnCode = 0;

    /** Current date string. COBOL: DATE-DT PIC X(8) (line 81), from FUNCTION CURRENT-DATE */
    private String currentDateStr;

    /**
     * Creates a new CLREB020 program instance with the specified file paths.
     *
     * @param cardFilePath   path to the input card file (COBOL: ASSIGN TO UT-S-CARDS)
     * @param printFilePath  path to the output print file (COBOL: ASSIGN TO UT-S-PRINT)
     * @param factorFilePath path to the output factor file (COBOL: ASSIGN TO UT-S-FACTOR)
     */
    public Clreb020(Path cardFilePath, Path printFilePath, Path factorFilePath) {
        this.cardFilePath = cardFilePath;
        this.printFilePath = printFilePath;
        this.factorFilePath = factorFilePath;
    }

    /**
     * Main entry point — executes the CLREB020 program logic.
     *
     * <p>COBOL: 010-BEGIN (lines 110-124)
     * <pre>
     *   OPEN INPUT CARD-FILE OUTPUT PRINT-FILE FACTOR-FILE  (lines 111-113)
     *   MOVE FUNCTION CURRENT-DATE TO DATE-DT               (line 114)
     *   PERFORM 020-MAIN-LINE THRU 020-EXIT                 (lines 115-117)
     *       UNTIL END-OF-CARD-FILE OR SEQ-ERROR
     *   DISPLAY 'NO. OF INPUT RECORDS  = ' IN-CNT           (line 118)
     *   DISPLAY 'NO. OF OUTPUT RECORDS = ' OUT-CNT          (line 119)
     *   DISPLAY 'NO. OF ERROR RECORDS  = ' ERROR-CNT        (line 120)
     *   CLOSE CARD-FILE PRINT-FILE FACTOR-FILE              (lines 121-123)
     *   STOP RUN                                            (line 124)
     * </pre>
     *
     * @return the program return code (0 = success, 16 = sequence error)
     * @throws IOException if a file I/O error occurs
     */
    public int run() throws IOException {
        // OPEN INPUT CARD-FILE OUTPUT PRINT-FILE FACTOR-FILE (lines 111-113)
        cardReader = new CobolFileReader(cardFilePath, CardRecord.RECORD_LENGTH);
        printWriter = new CobolPrintWriter(printFilePath, 133);
        factorWriter = new CobolFileWriter(factorFilePath, FactorRecord.RECORD_LENGTH);

        // MOVE FUNCTION CURRENT-DATE TO DATE-DT (line 114)
        // GnuCOBOL's FUNCTION CURRENT-DATE returns "YYYYMMDDHHMMSS..." (21 chars).
        // Moving to DATE-DT PIC X(8) takes the first 8 characters: "YYYYMMDD".
        currentDateStr = LocalDate.now().format(DateTimeFormatter.BASIC_ISO_DATE); // "YYYYMMDD"

        try {
            // PERFORM 020-MAIN-LINE THRU 020-EXIT UNTIL END-OF-CARD-FILE OR SEQ-ERROR
            // (lines 115-117)
            while (!endOfCardFile && !sequenceError) {
                mainLine();
            }

            // DISPLAY count summaries (lines 118-120)
            System.out.println("NO. OF INPUT RECORDS  = " + inCount);
            System.out.println("NO. OF OUTPUT RECORDS = " + outCount);
            System.out.println("NO. OF ERROR RECORDS  = " + errorCount);
        } finally {
            // CLOSE CARD-FILE PRINT-FILE FACTOR-FILE (lines 121-123)
            cardReader.close();
            printWriter.close();
            factorWriter.close();
        }

        return returnCode; // STOP RUN (line 124)
    }

    /**
     * Main processing loop — reads and validates one card per invocation.
     *
     * <p>COBOL: 020-MAIN-LINE (lines 126-143)
     * <pre>
     *   PERFORM 030-READ-CARD THRU 030-READ-EXIT           (line 127)
     *   IF NOT END-OF-CARD-FILE AND NOT SEQ-ERROR           (lines 128-129)
     *     IF CD-YR NUMERIC AND CD-YR > 0                    (lines 130-131)
     *        AND CD-FACTOR NUMERIC AND CD-FACTOR > 0        (lines 132-133)
     *        AND VALID-QUAD                                 (line 134)
     *       PERFORM 040-CREATE-FACTOR THRU 040-EXIT         (line 135)
     *       PERFORM 050-WRITE THRU 050-EXIT                 (line 136)
     *     ELSE
     *       ADD +1 TO ERROR-CNT                             (line 138)
     *       MOVE ERR-MESG TO WK-MESG                        (line 139)
     *       PERFORM 050-WRITE THRU 050-EXIT                 (line 140)
     * </pre>
     */
    private void mainLine() throws IOException {
        readCard(); // PERFORM 030-READ-CARD THRU 030-READ-EXIT (line 127)

        if (!endOfCardFile && !sequenceError) { // lines 128-129
            if (currentCard.isYearNumericAndPositive()     // lines 130-131
                    && currentCard.isFactorNumericAndPositive() // lines 132-133
                    && currentCard.isValidQuad()) {             // line 134
                createFactor();     // PERFORM 040-CREATE-FACTOR (line 135)
                writeReportLine();  // PERFORM 050-WRITE (line 136)
            } else {
                errorCount++;                // ADD +1 TO ERROR-CNT (line 138)
                errorMessage = ERR_MESG;     // MOVE ERR-MESG TO WK-MESG (line 139)
                writeReportLine();           // PERFORM 050-WRITE (line 140)
            }
        }
    }

    /**
     * Reads the next card record and performs sequence checking.
     *
     * <p>COBOL: 030-READ-CARD (lines 145-160)
     * <pre>
     *   READ CARD-FILE AT END MOVE 1 TO CARD-EOF            (lines 146-147)
     *   IF NOT END-OF-CARD-FILE                              (line 148)
     *     IF CARD LESS THAN PREV-CARD                        (line 149)
     *       MOVE 16 TO RETURN-CODE                           (line 150)
     *       DISPLAY 'CARDS OUT OF SEQUENCE'                  (line 151)
     *       DISPLAY 'CURRENT CARD ' CARD                     (line 152)
     *       DISPLAY 'PREVIOUS CARD ' PREV-CARD               (line 153)
     *       MOVE 'E' TO SEQ-CHECK                            (line 154)
     *     ELSE
     *       MOVE CARD TO PREV-CARD                           (line 156)
     *       ADD +1 TO IN-CNT                                 (line 157)
     * </pre>
     */
    private void readCard() throws IOException {
        String line = cardReader.readRecord(); // READ CARD-FILE (line 146)

        if (line == null) {
            endOfCardFile = true; // AT END MOVE 1 TO CARD-EOF (line 147)
            return;
        }

        currentCard = CardRecord.parse(line);
        String currentKey = currentCard.getKey(); // CARD group item: CD-YR + CD-QUAD (3 chars)

        // IF NOT END-OF-CARD-FILE (line 148)
        // IF CARD LESS THAN PREV-CARD (line 149) — alphanumeric comparison
        if (currentKey.compareTo(previousCardKey) < 0) {
            returnCode = 16;                              // MOVE 16 TO RETURN-CODE (line 150)
            System.out.println("CARDS OUT OF SEQUENCE");  // DISPLAY (line 151)
            System.out.println("CURRENT CARD " + currentKey);   // DISPLAY (line 152)
            System.out.println("PREVIOUS CARD " + previousCardKey); // DISPLAY (line 153)
            sequenceError = true;                         // MOVE 'E' TO SEQ-CHECK (line 154)
        } else {
            previousCardKey = currentKey;                 // MOVE CARD TO PREV-CARD (line 156)
            inCount++;                                    // ADD +1 TO IN-CNT (line 157)
        }
    }

    /**
     * Creates a factor output record from the current valid card.
     *
     * <p>COBOL: 040-CREATE-FACTOR (lines 162-168)
     * <pre>
     *   MOVE SPACES TO FACTOR-REC                          (line 163)
     *   MOVE CD-YR TO FT-TAXYR                             (line 164)
     *   MOVE CD-QUAD TO FT-QUAD                            (line 165)
     *   MOVE CD-FACTOR-RD TO FT-EQFACT                    (line 166)
     *   WRITE FACTOR-REC                                   (line 167)
     *   ADD +1 TO OUT-CNT                                  (line 168)
     * </pre>
     */
    private void createFactor() throws IOException {
        FactorRecord factor = new FactorRecord(currentCard);
        factorWriter.writeRecord(factor.toFixedWidth()); // WRITE FACTOR-REC (line 167)
        outCount++;                                       // ADD +1 TO OUT-CNT (line 168)
    }

    /**
     * Writes a report line to the print file for the current card.
     *
     * <p>COBOL: 050-WRITE (lines 173-187)
     * <pre>
     *   IF LINE-CNT GREATER THAN +55                       (line 174)
     *     PERFORM 060-HDG-ROUTINE THRU 060-EXIT            (line 175)
     *   MOVE CD-YR TO WK-YR                                (line 176)
     *   MOVE CD-QUAD TO WK-QUAD                            (line 177)
     *   MOVE CD-FT1 TO WK-FACT1                            (line 178)
     *   MOVE CD-FT4 TO WK-FACT4                            (line 179)
     *   MOVE WORK-FACTOR TO WK-FACT                        (line 180)
     *   WRITE PRINT-REC FROM WORK-LINE AFTER ADVANCING 2   (lines 181-182)
     *   MOVE SPACES TO WK-MESG                             (line 183)
     *   ADD +2 TO LINE-CNT                                 (line 184)
     * </pre>
     */
    private void writeReportLine() throws IOException {
        // IF LINE-CNT GREATER THAN +55 (line 174)
        if (lineCount > 55) {
            writeHeading(); // PERFORM 060-HDG-ROUTINE (line 175)
        }

        // Build WORK-LINE: spaces(52) + year(2) + spaces(8) + quad(1) + spaces(7) + factor(6) + spaces(5) + message(11)
        // COBOL layout (lines 97-105):
        //   FILLER PIC X(52) VALUE SPACES
        //   WK-YR PIC XX
        //   FILLER PIC X(8) VALUE SPACES
        //   WK-QUAD PIC X
        //   FILLER PIC X(7) VALUE SPACES
        //   WK-FACT PIC X(6) — built from WORK-FACTOR: WK-FACT1 + '.' + WK-FACT4
        //   FILLER PIC X(5) VALUE SPACES
        //   WK-MESG PIC X(11) VALUE SPACES

        // MOVE CD-FT1 TO WK-FACT1, MOVE CD-FT4 TO WK-FACT4 → WORK-FACTOR → WK-FACT (lines 178-180)
        String factorDisplay = currentCard.getFactorForDisplay(); // "2.9744" (6 chars)

        String workLine = String.format("%-52s%-2s%-8s%-1s%-7s%-6s%-5s%-11s",
                "",                              // FILLER (52 spaces)
                currentCard.getYear(),           // WK-YR (line 176)
                "",                              // FILLER (8 spaces)
                currentCard.getQuad(),           // WK-QUAD (line 177)
                "",                              // FILLER (7 spaces)
                factorDisplay,                   // WK-FACT (line 180) — "N.NNNN"
                "",                              // FILLER (5 spaces)
                errorMessage);                   // WK-MESG (line 139 or spaces)

        // WRITE PRINT-REC FROM WORK-LINE AFTER ADVANCING 2 (lines 181-182)
        printWriter.writeAfterAdvancing(workLine, 2);

        // MOVE SPACES TO WK-MESG (line 183)
        errorMessage = "";

        // ADD +2 TO LINE-CNT (line 184)
        lineCount += 2;
    }

    /**
     * Writes the page heading to the print file (date, title, column headers).
     *
     * <p>COBOL: 060-HDG-ROUTINE (lines 189-203)
     * <pre>
     *   ADD +1 TO PAGE-CNT                                  (line 190)
     *   MOVE PAGE-CNT TO HDG-PG                             (line 191)
     *   WRITE PRINT-REC FROM DATE-LINE AFTER PAGE           (lines 192-193)
     *   WRITE PRINT-REC FROM HDG-LINE AFTER ADVANCING 2     (lines 194-195)
     *   WRITE PRINT-REC FROM TTL-LINE AFTER ADVANCING 2     (lines 196-197)
     *   WRITE PRINT-REC FROM TTL-LINE2 AFTER ADVANCING 3    (lines 198-199)
     *   MOVE +8 TO LINE-CNT                                 (line 200)
     * </pre>
     */
    private void writeHeading() throws IOException {
        pageCount++; // ADD +1 TO PAGE-CNT (line 190)

        // DATE-LINE: 11 spaces + date (8 chars) — COBOL lines 79-81
        String dateLine = String.format("%-11s%-8s", "", currentDateStr);

        // HDG-LINE: 11 spaces + "CLREB020" (padded to 40) + "OFFICE  OF  THE COUNTY  CLERK" (padded to 70)
        //           + "PAGE  " (6) + page number (ZZ9 = 3 chars)
        // COBOL lines 82-88
        String hdgLine = String.format("%-11s%-40s%-70s%-6s%3d",
                "",
                "CLREB020",
                "OFFICE  OF  THE COUNTY  CLERK",
                "PAGE  ",
                pageCount);

        // TTL-LINE: 51 spaces + "EQUALIZATION      FACTORS" — COBOL lines 89-92
        String ttlLine = String.format("%-51s%-25s", "", "EQUALIZATION      FACTORS");

        // TTL-LINE2: 51 spaces + "YEAR      QUAD     FACTOR" — COBOL lines 93-96
        String ttlLine2 = String.format("%-51s%-25s", "", "YEAR      QUAD     FACTOR");

        // Write with carriage control
        printWriter.writeAfterPage(dateLine);           // AFTER PAGE (lines 192-193)
        printWriter.writeAfterAdvancing(hdgLine, 2);    // AFTER ADVANCING 2 (lines 194-195)
        printWriter.writeAfterAdvancing(ttlLine, 2);    // AFTER ADVANCING 2 (lines 196-197)
        printWriter.writeAfterAdvancing(ttlLine2, 3);   // AFTER ADVANCING 3 (lines 198-199)

        lineCount = 8; // MOVE +8 TO LINE-CNT (line 200)
    }

    /**
     * CLI entry point.
     *
     * <p>Usage: {@code java -jar tax-extension.jar <card_file> <print_file> <factor_file>}
     *
     * <p>File paths can also be provided via environment variables:
     * {@code CARDS}, {@code PRINT}, {@code FACTOR}
     *
     * @param args command-line arguments: card_file, print_file, factor_file
     */
    public static void main(String[] args) {
        Path cardPath;
        Path printPath;
        Path factorPath;

        if (args.length >= 3) {
            cardPath = Path.of(args[0]);
            printPath = Path.of(args[1]);
            factorPath = Path.of(args[2]);
        } else {
            // Fall back to environment variables (similar to GnuCOBOL DD name mapping)
            String cards = System.getenv("CARDS");
            String print = System.getenv("PRINT");
            String factor = System.getenv("FACTOR");

            if (cards == null || print == null || factor == null) {
                System.err.println("Usage: java -jar tax-extension.jar <card_file> <print_file> <factor_file>");
                System.err.println("  Or set environment variables: CARDS, PRINT, FACTOR");
                System.exit(1);
                return;
            }

            cardPath = Path.of(cards);
            printPath = Path.of(print);
            factorPath = Path.of(factor);
        }

        try {
            Clreb020 program = new Clreb020(cardPath, printPath, factorPath);
            int rc = program.run();
            System.exit(rc);
        } catch (IOException e) {
            System.err.println("CLREB020 I/O error: " + e.getMessage());
            System.exit(12);
        }
    }
}
