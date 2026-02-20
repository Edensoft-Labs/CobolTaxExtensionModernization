package gov.cookcounty.taxextension.equalizationfactor;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Program-level integration tests for {@link ClerkRealEstateBoardEqualizationFactorProgram}.
 *
 * <p>These tests run the full program (CLREB020) with controlled inputs and verify outputs.
 * They mirror and expand on the 50 test cases in the COBOL/Java test harnesses
 * at {@code Testing/CLREB020/}.
 *
 * <p>Test categories follow Code Complete 2nd Edition Ch. 22:
 * equivalence partitioning, boundary analysis, structured basis testing,
 * data-flow testing, error guessing, requirements-based, and design-based testing.
 */
class ClerkRealEstateBoardEqualizationFactorProgramTest
{
    // ========== CONSTANTS ==========

    /**
     * Number of assessment quadrants in Cook County (1 through 4).
     * Each tax year has exactly 4 quadrants of equalization factors.
     */
    private static final int QUADRANT_COUNT = 4;
    /**
     * Return code indicating successful program completion with no sequence errors.
     * COBOL: {@code MOVE 0 TO RETURN-CODE} (implicit default).
     */
    private static final int RETURN_CODE_SUCCESS = 0;
    /**
     * Return code indicating a card sequence error was detected.
     * COBOL: {@code MOVE 16 TO RETURN-CODE} in the sequence error paragraph.
     */
    private static final int RETURN_CODE_SEQUENCE_ERROR = 16;

    // ========== MEMBER VARIABLES ==========

    /** Temporary directory created by JUnit for each test; cleaned up automatically after each test. */
    @TempDir
    private Path temporaryDirectoryPath;
    /** Path to the input card file (COBOL CARD-FILE). Written by test setup with card records to process. */
    private Path cardFilePath;
    /** Path to the output print report file (COBOL PRINT-FILE). Contains the formatted equalization factor report. */
    private Path printReportFilePath;
    /** Path to the output equalization factor file (COBOL FACTOR-FILE). Contains fixed-width factor records for downstream programs. */
    private Path equalizationFactorFilePath;

    // ========== HELPER METHODS ==========

    /**
     * Initializes file paths for the three COBOL program files within the
     * JUnit-managed temporary directory. Each test gets a fresh directory,
     * so file paths are re-created before every test method.
     */
    @BeforeEach
    void setUp()
    {
        cardFilePath = temporaryDirectoryPath.resolve("cards.dat");
        printReportFilePath = temporaryDirectoryPath.resolve("print.dat");
        equalizationFactorFilePath = temporaryDirectoryPath.resolve("factor.dat");
    }

    /**
     * Builds an 80-character card record string from individual field values.
     *
     * <p>The card layout matches the COBOL FD CARD-FILE record:
     * <pre>
     *   Positions 1-2:  CD-YR       PIC XX        (tax year)
     *   Position  3:    CD-QUAD     PIC X         (assessment quadrant)
     *   Positions 4-8:  CD-FACTOR   PIC X(5)      (equalization factor)
     *   Positions 9-80: CD-FILLER   PIC X(72)     (unused, spaces)
     * </pre>
     *
     * <p>Each field is truncated or space-padded to its exact COBOL width using
     * {@code %-N.Ns} format specifiers, so callers can pass short or long strings
     * without worrying about overflow.
     *
     * @param year   2-character tax year (e.g., "25" for 2025). Truncated to 2 characters.
     * @param assessmentQuadrant 1-character assessment quadrant code ("1" through "4"). Truncated to 1 character.
     * @param factor 5-character equalization factor (e.g., "29744" for 2.9744). Truncated to 5 characters.
     * @return An 80-character string matching the COBOL CARD-REC layout.
     */
    private String makeCardWithYearQuadrantFactor(
        String year,
        String assessmentQuadrant,
        String factor)
    {
        // DETERMINE THE AMOUNT OF FILLER NEEDED AFTER SPECIFIC FIELD VALUES.
        // CD-YR PIC XX at positions 1-2.
        final int CARD_YEAR_FIELD_LENGTH_IN_CHARACTERS = 2;
        // CD-QUAD PIC X at position 3.
        final int CARD_ASSESSMENT_QUADRANT_FIELD_LENGTH_IN_CHARACTERS = 1;
        // CD-FACTOR PIC X(5) at positions 4-8.
        final int CARD_FACTOR_FIELD_LENGTH_IN_CHARACTERS = 5;
        // 80-character card minus year (2), quadrant (1), and factor (5) = 72 characters of filler.
        final int CARD_FILLER_LENGTH_IN_CHARACTERS = 
            CardRecord.RECORD_LENGTH_IN_CHARACTERS
            - CARD_YEAR_FIELD_LENGTH_IN_CHARACTERS
            - CARD_ASSESSMENT_QUADRANT_FIELD_LENGTH_IN_CHARACTERS
            - CARD_FACTOR_FIELD_LENGTH_IN_CHARACTERS;

        // FORMAT THE FIELD VALUES INTO A FIXED-WIDTH 80-CHARACTER COBOL CARD IMAGE.
        // Empty string expands to spaces via the left-justified filler format specifier.
        final String EMPTY_FILLER_CONTENT = "";
        // Format specifier "%-N.Ns" means:
        //   '-' = left-align the string within the field
        //   N   = minimum field width (pad with spaces on the right if shorter)
        //   .N  = maximum field width (truncate if longer)
        //   s   = string conversion type
        // Using both N.N ensures each field is exactly N characters wide.
        String cardRecordFormatPattern =
            // CD-YR: 2 characters (%-2.2s)
            // CD-QUAD: 1 character (%-1.1s)
            // CD-FACTOR: 5 characters (%-5.5s)
            // FILLER: 72 spaces (%-72s)
            "%-2.2s%-1.1s%-5.5s%-" + CARD_FILLER_LENGTH_IN_CHARACTERS + "s";
        String cardRecord = String.format(
            cardRecordFormatPattern,
            year,
            assessmentQuadrant,
            factor,
            EMPTY_FILLER_CONTENT);
        return cardRecord;
    }

    /**
     * Writes card records as a contiguous binary stream (no newlines or delimiters),
     * matching COBOL RECORDING MODE F for fixed-length sequential files.
     *
     * <p>Each card string is encoded as ISO-8859-1 bytes and concatenated directly.
     * The COBOL runtime reads these as consecutive 80-byte blocks without any
     * record separator characters.
     *
     * @param cardFilePath File path to write the binary card data to.
     * @param cards        List of 80-character card record strings to write sequentially.
     * @throws IOException If a file I/O error occurs during writing.
     */
    private void writeCardFile(Path cardFilePath, List<String> cards) throws IOException
    {
        // WRITE ALL CARDS TO THE OUTPUT FILE..
        try (var cardOutputFile = Files.newOutputStream(cardFilePath))
        {
            // WRITE EACH CARD TO THE OUTPUT FILE.
            for (String card : cards)
            {
                // WRITE THE CARD TO THE OUTPUT FILE.
                // Encode the card string as single-byte ISO-8859-1, matching COBOL EBCDIC-to-ASCII semantics.
                byte[] cardBytes = card.getBytes(StandardCharsets.ISO_8859_1);
                cardOutputFile.write(cardBytes);
            }
        }
    }

    /**
     * Reads the binary factor output file and splits it into individual
     * 21-character records.
     *
     * <p>The factor file is written by CLREB020 as a fixed-length sequential file
     * with no record delimiters. Each record is exactly
     * {@link FactorRecord#RECORD_LENGTH_IN_CHARACTERS} (21) bytes.
     *
     * @param equalizationFactorFilePath File path to the binary factor output file.
     * @return List of 21-character strings, one per factor record.
     * @throws IOException If a file I/O error occurs during reading.
     */
    private List<String> readFactorRecords(Path equalizationFactorFilePath) throws IOException
    {
        // READ ALL BYTES OF THE FACTOR FILE.
        byte[] factorFileBytes = Files.readAllBytes(equalizationFactorFilePath);

        // SPLIT THE ENTIRE FILE INTO INDIVIDUAL FIXED-WIDTH FACTOR RECORDS.
        List<String> factorRecords = new ArrayList<>();
        for (int byteOffset = 0;
             byteOffset + FactorRecord.RECORD_LENGTH_IN_CHARACTERS <= factorFileBytes.length;
             byteOffset += FactorRecord.RECORD_LENGTH_IN_CHARACTERS)
        {
            // GET THE CURRENT FACTOR RECORD.
            // Decode each 21-byte block as an ISO-8859-1 string matching the COBOL character encoding.
            String factorRecord = new String(
                factorFileBytes,
                byteOffset,
                FactorRecord.RECORD_LENGTH_IN_CHARACTERS,
                StandardCharsets.ISO_8859_1);
            factorRecords.add(factorRecord);
        }
        return factorRecords;
    }

    /**
     * Instantiates and runs the CLREB020 program against the test fixture files
     * (cardFilePath, printReportFilePath, equalizationFactorFilePath) set up in {@link #setUp()}.
     *
     * @return The program return code: {@link #RETURN_CODE_SUCCESS} (0) for normal
     *         completion, {@link #RETURN_CODE_SEQUENCE_ERROR} (16) if cards were
     *         out of ascending year-quadrant order.
     * @throws IOException If a file I/O error occurs during program execution.
     */
    private int runClerkRealEstateBoardEqualizationFactorProgram() throws IOException
    {
        // RUN THE PROGRAM WITH APPROPRIATE FILE PATHS.
        ClerkRealEstateBoardEqualizationFactorProgram program = new ClerkRealEstateBoardEqualizationFactorProgram(
            cardFilePath,
            printReportFilePath,
            equalizationFactorFilePath);
        int returnCode = program.run();
        return returnCode;
    }

    // ========== CORE HAPPY-PATH TESTS (1-10) ==========

    /**
     * Verifies the basic end-to-end flow: 4 valid cards (one per quadrant for year 25)
     * produce 4 factor records with the correct year+quadrant+factor layout, and
     * the print report is non-empty.
     */
    @Test
    void testValidBasicInput() throws IOException
    {
        // VERIFY MULTIPLE VALID CARDS.
        List<String> cards = List.of(
            makeCardWithYearQuadrantFactor("25", "1", "29744"),
            makeCardWithYearQuadrantFactor("25", "2", "29744"),
            makeCardWithYearQuadrantFactor("25", "3", "29744"),
            makeCardWithYearQuadrantFactor("25", "4", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(QUADRANT_COUNT, factorRecordCount);

        // VERIFY EACH FACTOR RECORD.
        // Each record should have a concatentation of the data given to it,
        // plus some additional padding.
        final int FIRST_FACTOR_RECORD_INDEX = 0;
        String firstFactorRecord = factorRecords.get(FIRST_FACTOR_RECORD_INDEX);
        assertEquals("25129744             ", firstFactorRecord);

        final int SECOND_FACTOR_RECORD_INDEX = 1;
        String secondFactorRecord = factorRecords.get(SECOND_FACTOR_RECORD_INDEX);
        assertEquals("25229744             ", secondFactorRecord);

        final int THIRD_FACTOR_RECORD_INDEX = 2;
        String thirdFactorRecord = factorRecords.get(THIRD_FACTOR_RECORD_INDEX);
        assertEquals("25329744             ", thirdFactorRecord);

        final int FOURTH_FACTOR_RECORD_INDEX = 3;
        String fourthFactorRecord = factorRecords.get(FOURTH_FACTOR_RECORD_INDEX);
        assertEquals("25429744             ", fourthFactorRecord);

        // VERIFY THE PRINT REPORT FILE WAS POPULATED.
        long printReportFileSizeInBytes = Files.size(printReportFilePath);
        boolean printReportFilePopulated = printReportFileSizeInBytes > 0;
        assertTrue(printReportFilePopulated);
    }

    /**
     * Verifies that a large input stream (64 cards spanning 16 years) triggers
     * the COBOL pagination logic, producing multiple page headers in the print report.
     */
    @Test
    void testMultiplePages() throws IOException
    {
        // GENERATE MANY CARDS SPANNING MULTIPLE YEARS.
        // The exact year range is arbitrary but somewhat intended to be relatively large.
        final int MIN_TEST_2_DIGIT_YEAR = 10;
        final int MAX_TEST_2_DIGIT_YEAR = 25;
        var cards = new ArrayList<String>();
        for (int year = MIN_TEST_2_DIGIT_YEAR; year <= MAX_TEST_2_DIGIT_YEAR; ++year)
        {
            // CREATE A CARD FOR EACH ASSESSMENT QUADRANT FOR THE CURRENT YEAR.
            final int MIN_ASSESSMENT_QUADRANT = 1;
            for (int assessmentQuadrant = MIN_ASSESSMENT_QUADRANT; assessmentQuadrant <= QUADRANT_COUNT; ++assessmentQuadrant)
            {
                // ADD THE CARD FOR THE YEAR.
                // The year needs to be exactly 2 digits.
                String yearString = String.format("%02d", year);
                // The assessment quadrant needs to be exactly 1 digit.
                String assessmentQuadrantString = String.valueOf(assessmentQuadrant);
                // The factor value is arbitrary.
                final String ARBITRARY_FACTOR_VALUE = "29744";
                cards.add(makeCardWithYearQuadrantFactor(yearString, assessmentQuadrantString, ARBITRARY_FACTOR_VALUE));
            }
        }
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        // The initial year should be added back in since it would be excluded with the subtraction.
        final int INCLUDE_INITIAL_YEAR = 1;
        int testedYearCount = MAX_TEST_2_DIGIT_YEAR - MIN_TEST_2_DIGIT_YEAR + INCLUDE_INITIAL_YEAR;
        // For each year, there should be card per quadrant.
        int expectedCardCount = testedYearCount * QUADRANT_COUNT;
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(expectedCardCount, factorRecordCount);

        // VERIFY THE PRINT REPORT CONTAINS MULTIPLE PAGE HEADERS.
        // 64 cards should span more than one page in the report.
        String printReportContent = Files.readString(printReportFilePath);
        final String PAGE_HEADER_IDENTIFIER = "CLREB020";
        long pageHeaderCount = printReportContent.lines()
            .filter(line -> line.contains(PAGE_HEADER_IDENTIFIER))
            .count();
        boolean multiplePageHeadersPresent = pageHeaderCount > 1;
        assertTrue(multiplePageHeadersPresent, "Should have multiple page headers for 64 cards");
    }

    /**
     * Verifies that a non-numeric year (letters "AB") is rejected by the
     * COBOL NUMERIC test, producing zero factor records and a "NOT NUMERIC"
     * error annotation on the print report.
     */
    @Test
    void testInvalidNonNumericYear() throws IOException
    {
        // VERIFY A NON-NUMERIC YEAR IS REJECTED WITH A PRINT REPORT ERROR ANNOTATION.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("AB", "1", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed even with invalid input data.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY NO FACTOR RECORDS WERE PRODUCED.
        long factorFileSizeInBytes = Files.size(equalizationFactorFilePath);
        assertEquals(0, factorFileSizeInBytes);

        // VERIFY THE PRINT REPORT CONTAINS THE EXPECTED ERROR.
        String printReportContent = Files.readString(printReportFilePath);
        boolean printReportContainsNotNumericError = printReportContent.contains("NOT NUMERIC");
        assertTrue(printReportContainsNotNumericError);
    }

    /**
     * Verifies that an out-of-range quadrant ("5") is rejected. COBOL only accepts
     * quadrant values 1-4, so quadrant 5 produces an error with no factor output.
     */
    @Test
    void testInvalidBadQuadrant() throws IOException
    {
        // VERIFY AN OUT-OF-RANGE QUADRANT IS REJECTED WITH NO FACTOR OUTPUT.
        // Quadrants only range from 1 to 4.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "5", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed even with invalid input data.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY NO FACTOR RECORDS WERE PRODUCED.
        long factorFileSizeInBytes = Files.size(equalizationFactorFilePath);
        assertEquals(0, factorFileSizeInBytes);

        // VERIFY THE PRINT REPORT CONTAINS THE EXPECTED ERROR.
        String printReportContent = Files.readString(printReportFilePath);
        boolean printReportContainsNotNumericError = printReportContent.contains("NOT NUMERIC");
        assertTrue(printReportContainsNotNumericError);
    }

    /**
     * Verifies that factor "00000" is accepted, preserving the original COBOL behavior.
     * The COBOL {@code CD-FACTOR GREATER THAN 0} comparison is alphanumeric (not numeric),
     * so "00000" > "0    " is TRUE because '0' (0x30) > ' ' (0x20) in position 2.
     */
    @Test
    void testZeroFactorAcceptedMatchingCobol() throws IOException
    {
        // VERIFY ALL-ZEROS FACTOR IS ACCEPTED UNDER COBOL-COMPATIBLE ALPHANUMERIC COMPARISON.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "1", "00000"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY ONLY A SINGLE FACTOR RECORD WAS PRODUCED.
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(1, factorRecordCount);

        // VERIFY THE FACTOR RECORD.
        final int FIRST_FACTOR_RECORD_INDEX = 0;
        String firstFactorRecord = factorRecords.get(FIRST_FACTOR_RECORD_INDEX);
        assertEquals("25100000             ", firstFactorRecord);
    }

    /**
     * Verifies that the COBOL ascending-sequence check on the year+quadrant key
     * detects out-of-order cards and returns RC=16. The second card ("25","1")
     * is less than the first card ("25","2").
     */
    @Test
    void testSequenceError() throws IOException
    {
        // VERIFY OUT-OF-ORDER CARDS TRIGGER A SEQUENCE ERROR WITH THE EXPECTED RETURN CODE.
        // Quadrants within a year should be in ascending order.
        List<String> cards = List.of(
            makeCardWithYearQuadrantFactor("25", "2", "29744"),
            makeCardWithYearQuadrantFactor("25", "1", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to detect a sequence error.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SEQUENCE_ERROR, returnCode, "Sequence error should set RC=16");
    }

    /**
     * Verifies that an empty input file (zero cards) is handled gracefully:
     * the program returns success and produces an empty factor file.
     */
    @Test
    void testEmptyInput() throws IOException
    {
        // VERIFY AN EMPTY INPUT FILE IS HANDLED GRACEFULLY WITH NO FACTOR OUTPUT.
        Files.writeString(cardFilePath, "");

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY NO FACTOR RECORDS WERE PRODUCED.
        long factorFileSizeInBytes = Files.size(equalizationFactorFilePath);
        assertEquals(0, factorFileSizeInBytes);
    }

    /**
     * Verifies the exact byte-level layout of a single factor record against the
     * COBOL FD FACTOR-REC definition: FT-TAXYR(2) + FT-QUAD(1) + FT-EQFACT(5) + FILLER(13).
     * Each field is checked at its precise substring position.
     */
    @Test
    void testFactorRecordExactFormat() throws IOException
    {
        // WRITE A SINGLE CARD TO THE INPUT FILE.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("24", "3", "31500"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        runClerkRealEstateBoardEqualizationFactorProgram();

        // VERIFY ONLY A SINGLE FACTOR RECORD WAS PRODUCED.
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(1, factorRecordCount);

        // VERIFY THE FACTOR RECORD LENGTH.
        final int FIRST_FACTOR_RECORD_INDEX = 0;
        String factorRecord = factorRecords.get(FIRST_FACTOR_RECORD_INDEX);
        int factorRecordLength = factorRecord.length();
        assertEquals(FactorRecord.RECORD_LENGTH_IN_CHARACTERS, factorRecordLength);

        // VERIFY THE YEAR FIELD.
        // FT-TAXYR PIC 99 at positions 1-2 (start inclusive, end exclusive).
        final int FACTOR_YEAR_START_POSITION = 0;
        final int FACTOR_YEAR_END_POSITION = 2;
        String yearField = factorRecord.substring(FACTOR_YEAR_START_POSITION, FACTOR_YEAR_END_POSITION);
        assertEquals("24", yearField, "FT-TAXYR");

        // VERIFY THE QUADRANT FIELD.
        // FT-QUAD PIC 9 at position 3.
        final int FACTOR_QUADRANT_START_POSITION = 2;
        final int FACTOR_QUADRANT_END_POSITION = 3;
        String quadrantField = factorRecord.substring(FACTOR_QUADRANT_START_POSITION, FACTOR_QUADRANT_END_POSITION);
        assertEquals("3", quadrantField, "FT-QUAD");

        // VERIFY THE EQUALIZATION FACTOR FIELD.
        // FT-EQFACT PIC 9V9999 at positions 4-8.
        final int FACTOR_EQFACT_START_POSITION = 3;
        final int FACTOR_EQFACT_END_POSITION = 8;
        String equalizationFactorField = factorRecord.substring(FACTOR_EQFACT_START_POSITION, FACTOR_EQFACT_END_POSITION);
        assertEquals("31500", equalizationFactorField, "FT-EQFACT");

        // VERIFY THE FILLER FIELD.
        // FILLER PIC X(13) at positions 9-21.
        final int FACTOR_FILLER_START_POSITION = 8;
        // End of filler equals the full 21-byte record length.
        final int FACTOR_FILLER_END_POSITION = FactorRecord.RECORD_LENGTH_IN_CHARACTERS;
        String fillerField = factorRecord.substring(FACTOR_FILLER_START_POSITION, FACTOR_FILLER_END_POSITION);
        assertEquals("             ", fillerField, "FILLER");
    }

    /**
     * Verifies that the print report contains the expected fixed heading text:
     * program ID, office name, report title, and column headers. These strings
     * are hard-coded in the COBOL 060-HDG-ROUTINE paragraph.
     */
    @Test
    void testPrintLineFormat() throws IOException
    {
        // CREATE AN ARBITRARY CARD.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "1", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        runClerkRealEstateBoardEqualizationFactorProgram();

        // VERIFY THE PRINT REPORT CONTAINS EACH EXPECTED HEADING ELEMENT.
        String printReportContent = Files.readString(printReportFilePath);

        boolean printReportContainsProgramId = printReportContent.contains("CLREB020");
        assertTrue(printReportContainsProgramId);

        boolean printReportContainsOfficeName = printReportContent.contains("OFFICE  OF  THE COUNTY  CLERK");
        assertTrue(printReportContainsOfficeName);

        boolean printReportContainsReportTitle = printReportContent.contains("EQUALIZATION      FACTORS");
        assertTrue(printReportContainsReportTitle);

        boolean printReportContainsColumnHeaders = printReportContent.contains("YEAR      QUAD     FACTOR");
        assertTrue(printReportContainsColumnHeaders);
    }

    /**
     * Verifies that the print report heading includes today's date in YYYYMMDD format,
     * matching the COBOL behavior of calling ACCEPT DATE-DT FROM DATE and formatting
     * it into the first heading line.
     */
    @Test
    void testDateInHeader() throws IOException
    {
        // CREATE AN ARBITRARY CARD.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "1", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        runClerkRealEstateBoardEqualizationFactorProgram();

        // VERIFY THE PRINT REPORT CONTAINS TODAY'S DATE.
        java.time.LocalDate todayDate = java.time.LocalDate.now();
        String todayFormatted = todayDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
        String printReportContent = Files.readString(printReportFilePath);
        boolean printReportContainsTodayDate = printReportContent.contains(todayFormatted);
        assertTrue(printReportContainsTodayDate);
    }

    // ========== BOUNDARY TESTS (YEAR, QUADRANT, FACTOR LIMITS) ==========

    /**
     * Verifies that year "01" (minimum practical tax year) is accepted.
     * Boundary: smallest non-zero 2-digit year.
     */
    @Test
    void testBoundaryYear01() throws IOException
    {
        // CREATE A CARD WITH THE MINIMUM PRACTICAL TWO-DIGIT TAX YEAR.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("01", "1", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY A SINGLE FACTOR RECORD WAS PRODUCED.
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(1, factorRecordCount);

        // VERIFY THE FACTOR RECORD.
        final int FIRST_FACTOR_RECORD_INDEX = 0;
        String firstFactorRecord = factorRecords.get(FIRST_FACTOR_RECORD_INDEX);
        assertEquals("01129744             ", firstFactorRecord);
    }

    /**
     * Verifies that year "99" (maximum 2-digit year) is accepted.
     * Boundary: largest representable year in the PIC XX field.
     */
    @Test
    void testBoundaryYear99() throws IOException
    {
        // CREATE A CARD WITH THE MAXIMUM TWO-DIGIT TAX YEAR.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("99", "1", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY A SINGLE FACTOR RECORD WAS PRODUCED.
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(1, factorRecordCount);

        // VERIFY THE FACTOR RECORD CONTAINS THE MAXIMUM YEAR.
        final int FIRST_FACTOR_RECORD_INDEX = 0;
        String firstFactorRecord = factorRecords.get(FIRST_FACTOR_RECORD_INDEX);
        assertEquals("99129744             ", firstFactorRecord);
    }

    /**
     * Verifies that year "00" is accepted, preserving the original COBOL behavior.
     * The COBOL {@code CD-YR GREATER THAN 0} comparison is alphanumeric (not numeric),
     * so "00" > "0 " is TRUE because '0' (0x30) > ' ' (0x20) in position 2.
     */
    @Test
    void testBoundaryYear00AcceptedMatchingCobol() throws IOException
    {
        // CREATE A CARD WITH THE YEAR "00".
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("00", "1", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(1, factorRecordCount);

        // VERIFY THE FACTOR RECORD.
        final int FIRST_FACTOR_RECORD_INDEX = 0;
        String firstFactorRecord = factorRecords.get(FIRST_FACTOR_RECORD_INDEX);
        assertEquals("00129744             ", firstFactorRecord);
    }

    /**
     * Verifies that factor "00001" (smallest positive equalization factor) is accepted.
     * Represents 0.0001 with the implied decimal after the first digit.
     */
    @Test
    void testBoundaryFactor00001() throws IOException
    {
        // CREATE A CARD WITH THE SMALLEST POSITIVE EQUALIZATION FACTOR.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "1", "00001"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(1, factorRecordCount);

        // VERIFY THE FACTOR RECORD.
        final int FIRST_FACTOR_RECORD_INDEX = 0;
        String firstFactorRecord = factorRecords.get(FIRST_FACTOR_RECORD_INDEX);
        assertEquals("25100001             ", firstFactorRecord);
    }

    /**
     * Verifies that factor "99999" (maximum equalization factor) is accepted.
     * Represents 9.9999 with the implied decimal -- the largest value the
     * PIC 9V9999 field can hold.
     */
    @Test
    void testBoundaryFactor99999() throws IOException
    {
        // CREATE A CARD WITH THE MAXIMUM EQUALIZATION FACTOR.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "1", "99999"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(1, factorRecordCount);

        // VERIFY THE FACTOR RECORD.
        final int FIRST_FACTOR_RECORD_INDEX = 0;
        String firstFactorRecord = factorRecords.get(FIRST_FACTOR_RECORD_INDEX);
        assertEquals("25199999             ", firstFactorRecord);
    }

    /**
     * Verifies that factor "10000" (unity factor, meaning 1.0000) is accepted.
     * A unity factor means no equalization adjustment is applied -- assessed
     * values pass through unchanged.
     */
    @Test
    void testBoundaryFactor10000Unity() throws IOException
    {
        // CREATE A CARD WITH THE UNITY FACTOR.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "1", "10000"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(1, factorRecordCount);

        // VERIFY THE FACTOR RECORD.
        final int FIRST_FACTOR_RECORD_INDEX = 0;
        String firstFactorRecord = factorRecords.get(FIRST_FACTOR_RECORD_INDEX);
        assertEquals("25110000             ", firstFactorRecord);
    }

    // ========== VALIDATION COMBINATORIAL TESTS ==========

    /**
     * Verifies that a year with mixed digits and letters ("2A") fails the
     * COBOL NUMERIC test. The NUMERIC test requires every character to be
     * a digit; a single non-digit character makes the entire field non-numeric.
     */
    @Test
    void testInvalidYearMixed() throws IOException
    {
        // CREATE A CARD WITH A YEAR CONTAINING BOTH DIGITS AND LETTERS.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("2A", "1", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed even with invalid input data.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY NO FACTOR RECORDS WERE PRODUCED.
        long factorFileSizeInBytes = Files.size(equalizationFactorFilePath);
        assertEquals(0, factorFileSizeInBytes);

        // VERIFY THE PRINT REPORT CONTAINS THE EXPECTED ERROR.
        String printReportContent = Files.readString(printReportFilePath);
        boolean printReportContainsNotNumericError = printReportContent.contains("NOT NUMERIC");
        assertTrue(printReportContainsNotNumericError);
    }

    /**
     * Verifies that an all-spaces year ("  ") is rejected. Spaces are not digits,
     * so the COBOL NUMERIC test fails. The factor file should remain empty.
     */
    @Test
    void testInvalidYearSpaces() throws IOException
    {
        // CREATE A CARD WITH AN ALL-SPACES YEAR.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("  ", "1", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed even with invalid input data.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY NO FACTOR RECORDS WERE PRODUCED.
        long factorFileSizeInBytes = Files.size(equalizationFactorFilePath);
        assertEquals(0, factorFileSizeInBytes);
    }

    /**
     * Verifies that a factor with an embedded letter ("29A44") fails the
     * COBOL NUMERIC test, even though the surrounding characters are digits.
     */
    @Test
    void testInvalidFactorMixed() throws IOException
    {
        // CREATE A CARD WITH A FACTOR CONTAINING AN EMBEDDED LETTER.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "1", "29A44"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed even with invalid input data.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY NO FACTOR RECORDS WERE PRODUCED.
        long factorFileSizeInBytes = Files.size(equalizationFactorFilePath);
        assertEquals(0, factorFileSizeInBytes);

        // VERIFY THE PRINT REPORT CONTAINS THE EXPECTED ERROR.
        String printReportContent = Files.readString(printReportFilePath);
        boolean printReportContainsNotNumericError = printReportContent.contains("NOT NUMERIC");
        assertTrue(printReportContainsNotNumericError);
    }

    /**
     * Verifies that a factor with an embedded space ("2 744") is rejected.
     * Spaces are not digits, so the NUMERIC test fails.
     */
    @Test
    void testInvalidFactorSpaces() throws IOException
    {
        // CREATE A CARD WITH A FACTOR CONTAINING AN EMBEDDED SPACE.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "1", "2 744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed even with invalid input data.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY NO FACTOR RECORDS WERE PRODUCED.
        long factorFileSizeInBytes = Files.size(equalizationFactorFilePath);
        assertEquals(0, factorFileSizeInBytes);
    }

    /**
     * Verifies that quadrant "9" is rejected. Only quadrants 1-4 are valid
     * for Cook County's assessment structure.
     */
    @Test
    void testInvalidQuadrant9() throws IOException
    {
        // CREATE A CARD WITH A QUADRANT OUTSIDE THE VALID RANGE.
        // Quadrants only range from 1 to 4.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "9", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed even with invalid input data.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY NO FACTOR RECORDS WERE PRODUCED.
        long factorFileSizeInBytes = Files.size(equalizationFactorFilePath);
        assertEquals(0, factorFileSizeInBytes);
    }

    /**
     * Verifies that a space character quadrant is rejected. The COBOL validation
     * requires CD-QUAD to be one of '1' through '4'.
     */
    @Test
    void testInvalidQuadrantSpace() throws IOException
    {
        // CREATE A CARD WITH A SPACE CHARACTER QUADRANT.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", " ", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed even with invalid input data.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY NO FACTOR RECORDS WERE PRODUCED.
        long factorFileSizeInBytes = Files.size(equalizationFactorFilePath);
        assertEquals(0, factorFileSizeInBytes);
    }

    /**
     * Verifies that a letter character quadrant ("A") is rejected.
     * Only digit characters '1' through '4' are accepted.
     */
    @Test
    void testInvalidQuadrantLetter() throws IOException
    {
        // CREATE A CARD WITH A LETTER CHARACTER QUADRANT.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "A", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed even with invalid input data.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY NO FACTOR RECORDS WERE PRODUCED.
        long factorFileSizeInBytes = Files.size(equalizationFactorFilePath);
        assertEquals(0, factorFileSizeInBytes);
    }

    /**
     * Verifies behavior when all three fields (year, quadrant, factor) contain
     * non-numeric data simultaneously. The program should still return RC=0
     * (validation errors are not fatal) but produce zero factor output.
     */
    @Test
    void testInvalidAllFieldsBad() throws IOException
    {
        // CREATE A CARD WITH ALL NON-NUMERIC FIELDS.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("AB", "X", "ZZZZZ"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed even with invalid input data.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY NO FACTOR RECORDS WERE PRODUCED.
        long factorFileSizeInBytes = Files.size(equalizationFactorFilePath);
        assertEquals(0, factorFileSizeInBytes);

        // VERIFY THE PRINT REPORT CONTAINS THE EXPECTED ERROR.
        String printReportContent = Files.readString(printReportFilePath);
        boolean printReportContainsNotNumericError = printReportContent.contains("NOT NUMERIC");
        assertTrue(printReportContainsNotNumericError);
    }

    // ========== SEQUENCE ORDERING TESTS ==========

    /**
     * Verifies that a properly ascending sequence spanning two years with all
     * four quadrants each (8 cards total) is accepted without sequence errors.
     */
    @Test
    void testSequenceAscendingAllQuadrants() throws IOException
    {
        // CREATE A PROPERLY ASCENDING MULTI-YEAR MULTI-QUADRANT SEQUENCE.
        final int ARBITRARY_START_YEAR = 24;
        final int ARBITRARY_END_YEAR = 25;
        var cards = new ArrayList<String>();
        for (int year = ARBITRARY_START_YEAR; year <= ARBITRARY_END_YEAR; ++year)
        {
            // CREATE A CARD FOR EACH ASSESSMENT QUADRANT FOR THE CURRENT YEAR.
            final int MIN_ASSESSMENT_QUADRANT = 1;
            for (int assessmentQuadrant = MIN_ASSESSMENT_QUADRANT; assessmentQuadrant <= QUADRANT_COUNT; ++assessmentQuadrant)
            {
                // ADD THE CARD FOR THE YEAR AND ASSESSMENT QUADRANT.
                // The year needs to be exactly 2 digits.
                String yearString = String.format("%02d", year);
                // The assessment quadrant needs to be exactly 1 digit.
                String assessmentQuadrantString = String.valueOf(assessmentQuadrant);
                // The factor value is arbitrary.
                final String ARBITRARY_FACTOR_VALUE = "29744";
                cards.add(makeCardWithYearQuadrantFactor(yearString, assessmentQuadrantString, ARBITRARY_FACTOR_VALUE));
            }
        }
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        // For each year, there should be card per quadrant.
        final int COUNT_INITIAL_YEAR = 1;
        final int YEAR_COUNT = ARBITRARY_END_YEAR - ARBITRARY_START_YEAR + COUNT_INITIAL_YEAR;
        final int EXPECTED_CARD_COUNT = YEAR_COUNT * QUADRANT_COUNT;
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(EXPECTED_CARD_COUNT, factorRecordCount);
    }

    /**
     * Verifies that a year transition from 24 to 25 in the middle of the stream
     * is accepted. The COBOL sequence check compares the 3-character year+quadrant key
     * alphanumerically, so "244" < "251" is a valid ascending transition.
     */
    @Test
    void testSequenceYearTransition() throws IOException
    {
        // VERIFY A YEAR BOUNDARY TRANSITION IS ACCEPTED AS A VALID ASCENDING SEQUENCE.
        List<String> cards = List.of(
            makeCardWithYearQuadrantFactor("24", "3", "29744"),
            makeCardWithYearQuadrantFactor("24", "4", "29744"),
            // The year transitions, which should allow for resetting the quadrant sequence.
            makeCardWithYearQuadrantFactor("25", "1", "29744"),
            makeCardWithYearQuadrantFactor("25", "2", "29744"),
            makeCardWithYearQuadrantFactor("25", "3", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        // There should be one factor per record.
        int expectedFactorRecordCount = cards.size();
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(expectedFactorRecordCount, factorRecordCount);
    }

    /**
     * Verifies that duplicate year+quadrant keys are allowed. The COBOL comparison
     * uses {@code LESS THAN} (not {@code LESS THAN OR EQUAL}), so equal keys
     * do not trigger a sequence error. This allows multiple factor corrections
     * for the same year+quadrant combination.
     */
    @Test
    void testSequenceDuplicateKeyAllowed() throws IOException
    {
        // VERIFY DUPLICATE YEAR-QUADRANT KEYS ARE ALLOWED WITHOUT TRIGGERING A SEQUENCE ERROR.
        List<String> cards = List.of(
            makeCardWithYearQuadrantFactor("25", "1", "29744"),
            makeCardWithYearQuadrantFactor("25", "1", "30000"),
            makeCardWithYearQuadrantFactor("25", "1", "31000"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        // There should be one factor per record.
        int expectedFactorRecordCount = cards.size();
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(expectedFactorRecordCount, factorRecordCount);
    }

    /**
     * Verifies that a sequence error detected mid-stream (after 2 valid cards)
     * returns RC=16 but still writes the records processed before the error.
     * The COBOL program stops immediately on sequence error without processing
     * the offending card.
     */
    @Test
    void testSequenceErrorAfterValid() throws IOException
    {
        // VERIFY A MID-STREAM SEQUENCE ERROR PRESERVES RECORDS PROCESSED BEFORE THE ERROR.
        List<String> cards = List.of(
            makeCardWithYearQuadrantFactor("25", "1", "29744"),
            makeCardWithYearQuadrantFactor("25", "2", "29744"),
            makeCardWithYearQuadrantFactor("24", "1", "29744")); // Out of order
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to detect a sequence error.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SEQUENCE_ERROR, returnCode);

        // VERIFY ONLY THE RECORDS BEFORE THE ERROR WERE WRITTEN.
        // The first 2 records should be written before the error is detected.
        final int VALID_RECORD_COUNT = 2;
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(VALID_RECORD_COUNT, factorRecordCount);
    }

    /**
     * Verifies that a sequence error on the very second card (immediately after
     * the first) still returns RC=16 and writes only the first valid record.
     */
    @Test
    void testSequenceErrorImmediate() throws IOException
    {
        // VERIFY A SEQUENCE ERROR ON THE SECOND CARD WRITES ONLY THE FIRST VALID RECORD.
        List<String> cards = List.of(
            makeCardWithYearQuadrantFactor("25", "2", "29744"),
            makeCardWithYearQuadrantFactor("25", "1", "29744")); // Immediately out of order
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to detect a sequence error.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SEQUENCE_ERROR, returnCode);

        // VERIFY ONLY THE FIRST RECORD WAS WRITTEN BEFORE THE ERROR.
        final int VALID_RECORD_COUNT = 1;
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(VALID_RECORD_COUNT, factorRecordCount);
    }

    // ========== DATA-FLOW AND STATE TESTS ==========

    /**
     * Verifies that validation errors on individual cards do not corrupt the
     * program's internal state. A stream of valid-invalid-valid-invalid-valid-valid
     * cards should produce exactly 4 factor records (the valid ones), with no
     * cross-contamination between error handling and normal processing.
     */
    @Test
    void testMixedValidInvalidStream() throws IOException
    {
        // VERIFY VALIDATION ERRORS DO NOT CORRUPT PROCESSING OF SUBSEQUENT VALID CARDS.
        List<String> cards = List.of(
            makeCardWithYearQuadrantFactor("25", "1", "29744"),   // valid
            makeCardWithYearQuadrantFactor("25", "2", "2A744"),   // invalid factor
            makeCardWithYearQuadrantFactor("25", "3", "29744"),   // valid
            makeCardWithYearQuadrantFactor("25", "4", "XXXXX"),   // invalid factor
            makeCardWithYearQuadrantFactor("26", "1", "29744"),   // valid
            makeCardWithYearQuadrantFactor("26", "2", "29744"));  // valid
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY ONLY THE VALID CARDS PRODUCED FACTOR RECORDS.
        final int VALID_RECORD_COUNT = 4;
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(VALID_RECORD_COUNT, factorRecordCount);
    }

    /**
     * Verifies that the COBOL error message working storage (WK-MESG) is properly
     * cleared between cards. If the error message from card 2 leaked into card 3's
     * report line, the "NOT NUMERIC" count would be wrong.
     */
    @Test
    void testErrorMessageCleanup() throws IOException
    {
        // VERIFY ERROR MESSAGES ARE PROPERLY CLEARED BETWEEN CARDS AND DO NOT LEAK.
        List<String> cards = List.of(
            makeCardWithYearQuadrantFactor("25", "1", "29744"),   // valid
            makeCardWithYearQuadrantFactor("25", "2", "ABCDE"),   // invalid
            makeCardWithYearQuadrantFactor("25", "3", "29744"));  // valid
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY ONLY THE VALID CARDS PRODUCED FACTOR RECORDS.
        final int VALID_RECORD_COUNT = 2;
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(VALID_RECORD_COUNT, factorRecordCount);

        // VERIFY ONLY ONE ERROR MESSAGE APPEARS IN THE PRINT REPORT.
        // If error messages leaked between cards, there would be more than one.
        String printReportContent = Files.readString(printReportFilePath);
        long notNumericErrorCount = printReportContent.lines()
            .filter(line -> line.contains("NOT NUMERIC"))
            .count();
        assertEquals(1, notNumericErrorCount, "Only the invalid card should show NOT NUMERIC");
    }

    /**
     * Verifies the COBOL counter invariant: input_count = output_count + error_count.
     * With 10 cards (7 valid + 3 invalid), exactly 7 factor records should be written.
     */
    @Test
    void testCounterInvariant() throws IOException
    {
        // CREATE A MIXTURE OF VALID AND INVALID CARDS.
        var cards = new ArrayList<String>();
        // Multiple valid cards for an initial year are created.
        final int MIN_ASSESSMENT_QUADRANT = 1;
        for (int assessmentQuadrant = MIN_ASSESSMENT_QUADRANT; assessmentQuadrant <= QUADRANT_COUNT; ++assessmentQuadrant)
        {
            // ADD THE CARD FOR THE YEAR AND ASSESSMENT QUADRANT.
            // The year needs to be exactly 2 digits.
            final int ARBITRARY_START_YEAR = 24;
            String yearString = String.format("%02d", ARBITRARY_START_YEAR);
            // The assessment quadrant needs to be exactly 1 digit.
            String assessmentQuadrantString = String.valueOf(assessmentQuadrant);
            // The factor value is arbitrary.
            final String ARBITRARY_FACTOR_VALUE = "29744";
            cards.add(makeCardWithYearQuadrantFactor(yearString, assessmentQuadrantString, ARBITRARY_FACTOR_VALUE));
        }
        cards.add(makeCardWithYearQuadrantFactor("26", "1", "ABCDE")); // invalid
        cards.add(makeCardWithYearQuadrantFactor("26", "2", "29744"));  // valid
        cards.add(makeCardWithYearQuadrantFactor("26", "3", "XXXXX")); // invalid
        cards.add(makeCardWithYearQuadrantFactor("26", "4", "29744"));  // valid
        cards.add(makeCardWithYearQuadrantFactor("27", "1", "29744"));  // valid
        cards.add(makeCardWithYearQuadrantFactor("27", "2", "ZZZZZ")); // invalid
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY ONLY THE VALID CARDS PRODUCED FACTOR RECORDS.
        // 10 cards total: 7 valid + 3 invalid = 7 factor records expected.
        final int VALID_RECORD_COUNT = 7;
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(VALID_RECORD_COUNT, factorRecordCount);
    }

    // ========== PAGE BREAK TESTS ==========

    /**
     * Verifies that exactly 24 cards fit on the first page without triggering a
     * page break. The COBOL LINE-CNT starts at 8 after the heading, each detail
     * line advances by 2 (double-spacing), and the page break triggers when
     * LINE-CNT exceeds 55. After 24 detail lines: 8 + (24 * 2) = 56, which means
     * the 25th card would see LINE-CNT=56 > 55 and trigger page 2.
     */
    @Test
    void testPageBreakAt24Cards() throws IOException
    {
        // CREATE THE MAXIMUM NUMBER OF CARDS THAT SHOULD FIT ON THE FIRST PAGE.
        final int CARDS_PER_FIRST_PAGE = 24;
        var cards = new ArrayList<String>();
        for (int cardIndex = 0; cardIndex < CARDS_PER_FIRST_PAGE; ++cardIndex)
        {
            // CREATE ARBITRARY VALUES FOR THE CARD THAT VARY.
            // The values need to increase slightly to remain valid in an expected sequence order.
            final int ARBITRARY_START_YEAR = 10;
            int year = ARBITRARY_START_YEAR + (cardIndex / QUADRANT_COUNT);
            String yearString = String.format("%02d", year);

            final int MIN_ASSESSMENT_QUADRANT = 1;
            int assessmentQuadrant = MIN_ASSESSMENT_QUADRANT + (cardIndex % QUADRANT_COUNT);
            String assessmentQuadrantString = String.valueOf(assessmentQuadrant);

            final String ARBITRARY_FACTOR_VALUE = "29744";
            cards.add(makeCardWithYearQuadrantFactor(yearString, assessmentQuadrantString, ARBITRARY_FACTOR_VALUE));
        }
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(CARDS_PER_FIRST_PAGE, factorRecordCount);

        // VERIFY THE PRINT REPORT CONTAINS EXACTLY ONE PAGE HEADER.
        // 24 cards should fit on a single page without triggering a page break.
        // LINE_COUNT_AFTER_HEADING=8, MAX=55, SPACING=2: (55 - 8) / 2 = 23.5 => 24 fit.
        String printReportContent = Files.readString(printReportFilePath);
        long pageHeaderCount = printReportContent.lines()
            .filter(line -> line.contains("CLREB020"))
            .count();
        assertEquals(1, pageHeaderCount, "24 cards fits on exactly 1 page");
    }

    /**
     * Verifies that 72 cards (well beyond a single page) produce at least 3 page
     * headers in the print report, confirming the pagination logic works correctly
     * across multiple page boundaries.
     */
    @Test
    void testPageBreakThreePages() throws IOException
    {
        // CREATE ENOUGH CARDS TO SPAN THREE PAGES.
        final int CARDS_PER_PAGE = 24;
        final int PAGE_COUNT = 3;
        final int TOTAL_CARD_COUNT = CARDS_PER_PAGE * PAGE_COUNT;
        var cards = new ArrayList<String>();
        for (int cardIndex = 0; cardIndex < TOTAL_CARD_COUNT; ++cardIndex)
        {
            // CREATE ARBITRARY VALUES FOR THE CARD THAT VARY.
            // The values need to increase slightly to remain valid in an expected sequence order.
            final int ARBITRARY_START_YEAR = 10;
            int year = ARBITRARY_START_YEAR + (cardIndex / QUADRANT_COUNT);
            String yearString = String.format("%02d", year);

            final int MIN_ASSESSMENT_QUADRANT = 1;
            int assessmentQuadrant = MIN_ASSESSMENT_QUADRANT + (cardIndex % QUADRANT_COUNT);
            String assessmentQuadrantString = String.valueOf(assessmentQuadrant);

            final String ARBITRARY_FACTOR_VALUE = "29744";
            cards.add(makeCardWithYearQuadrantFactor(yearString, assessmentQuadrantString, ARBITRARY_FACTOR_VALUE));
        }
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(TOTAL_CARD_COUNT, factorRecordCount);

        // VERIFY THE PRINT REPORT CONTAINS MULTIPLE PAGE HEADERS.
        // 72 cards should span at least three report pages.
        String printReportContent = Files.readString(printReportFilePath);
        long pageHeaderCount = printReportContent.lines()
            .filter(line -> line.contains("CLREB020"))
            .count();
        boolean atLeastThreePagesPresent = pageHeaderCount >= 3;
        assertTrue(atLeastThreePagesPresent, "72 cards should produce 3+ pages");
    }

    // ========== BUSINESS SCENARIO TESTS ==========

    /**
     * Simulates a typical annual production run: one tax year with all 4 quadrants,
     * each having a different equalization factor. Verifies that the factor values
     * are preserved exactly as entered (no rounding or transformation).
     */
    @Test
    void testBusinessAnnualRun() throws IOException
    {
        // CREATE CARDS FOR A TYPICAL ANNUAL PRODUCTION RUN WITH DISTINCT FACTORS PER QUADRANT.
        List<String> cards = List.of(
            makeCardWithYearQuadrantFactor("25", "1", "29744"),
            makeCardWithYearQuadrantFactor("25", "2", "31205"),
            makeCardWithYearQuadrantFactor("25", "3", "25000"),
            makeCardWithYearQuadrantFactor("25", "4", "28133"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(QUADRANT_COUNT, factorRecordCount);

        // VERIFY EACH FACTOR RECORD.
        // Each record should preserve the exact factor value without rounding.
        final int FIRST_FACTOR_RECORD_INDEX = 0;
        String firstFactorRecord = factorRecords.get(FIRST_FACTOR_RECORD_INDEX);
        assertEquals("25129744             ", firstFactorRecord);

        final int SECOND_FACTOR_RECORD_INDEX = 1;
        String secondFactorRecord = factorRecords.get(SECOND_FACTOR_RECORD_INDEX);
        assertEquals("25231205             ", secondFactorRecord);

        final int THIRD_FACTOR_RECORD_INDEX = 2;
        String thirdFactorRecord = factorRecords.get(THIRD_FACTOR_RECORD_INDEX);
        assertEquals("25325000             ", thirdFactorRecord);

        final int FOURTH_FACTOR_RECORD_INDEX = 3;
        String fourthFactorRecord = factorRecords.get(FOURTH_FACTOR_RECORD_INDEX);
        assertEquals("25428133             ", fourthFactorRecord);
    }

    /**
     * Simulates loading factors for multiple consecutive tax years (23-25), which
     * is common during historical reprocessing or catch-up runs. All 12 cards
     * (3 years x 4 quadrants) should produce 12 factor records.
     */
    @Test
    void testBusinessMultiYear() throws IOException
    {
        // CREATE CARDS FOR A MULTI-YEAR HISTORICAL REPROCESSING RUN.
        var cards = new ArrayList<String>();
        final int START_YEAR = 23;
        final int END_YEAR = 25;
        for (int year = START_YEAR; year <= END_YEAR; ++year)
        {
            // CREATE A CARD FOR EACH ASSESSMENT QUADRANT FOR THE CURRENT YEAR.
            final int MIN_ASSESSMENT_QUADRANT = 1;
            for (int assessmentQuadrant = MIN_ASSESSMENT_QUADRANT; assessmentQuadrant <= QUADRANT_COUNT; ++assessmentQuadrant)
            {
                // ADD THE CARD FOR THE YEAR AND ASSESSMENT QUADRANT.
                // The year needs to be exactly 2 digits.
                String yearString = String.format("%02d", year);
                String assessmentQuadrantString = String.valueOf(assessmentQuadrant);
                final String ARBITRARY_FACTOR_VALUE = "29744";
                cards.add(makeCardWithYearQuadrantFactor(yearString, assessmentQuadrantString, ARBITRARY_FACTOR_VALUE));
            }
        }
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        final int EXPECTED_CARD_COUNT = 12;
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(EXPECTED_CARD_COUNT, factorRecordCount);
    }

    /**
     * Simulates a data-entry typo (letter "X" in factor for quadrant 3) embedded
     * in an otherwise valid stream. The program should reject only the bad card,
     * writing 4 factor records from the other 4 valid cards, and annotate the
     * error in the print report.
     */
    @Test
    void testBusinessDataEntryError() throws IOException
    {
        // CREATE CARDS WHERE ONE HAS A DATA-ENTRY TYPO.
        List<String> cards = List.of(
            makeCardWithYearQuadrantFactor("25", "1", "29744"),
            makeCardWithYearQuadrantFactor("25", "2", "31205"),
            makeCardWithYearQuadrantFactor("25", "3", "2X000"),  // typo
            makeCardWithYearQuadrantFactor("25", "4", "28133"),
            makeCardWithYearQuadrantFactor("26", "1", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed even with invalid input data.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY ONLY THE VALID CARDS PRODUCED FACTOR RECORDS.
        final int VALID_RECORD_COUNT = 4;
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(VALID_RECORD_COUNT, factorRecordCount);

        // VERIFY THE PRINT REPORT CONTAINS THE EXPECTED ERROR.
        String printReportContent = Files.readString(printReportFilePath);
        boolean printReportContainsNotNumericError = printReportContent.contains("NOT NUMERIC");
        assertTrue(printReportContainsNotNumericError);
    }

    /**
     * Verifies that a unity factor (1.0000, encoded as "10000") flows through
     * the program unchanged. A unity factor means no equalization adjustment
     * is applied to assessed values for that quadrant.
     */
    @Test
    void testBusinessFactorUnity() throws IOException
    {
        // CREATE A CARD WITH A UNITY FACTOR.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "1", "10000"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        final int EXPECTED_RECORD_COUNT = 1;
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(EXPECTED_RECORD_COUNT, factorRecordCount);

        // VERIFY THE FACTOR RECORD.
        final int FIRST_FACTOR_RECORD_INDEX = 0;
        String firstFactorRecord = factorRecords.get(FIRST_FACTOR_RECORD_INDEX);
        assertEquals("25110000             ", firstFactorRecord);
    }

    // ========== OUTPUT FORMAT VERIFICATION TESTS ==========

    /**
     * Verifies that the print report formats factors with the implied decimal point
     * inserted for display. COBOL CD-FACTOR-RD is PIC 9V9999, so "29744" displays
     * as "2.9744". This tests several representative values including boundary cases.
     */
    @Test
    void testFactorDisplayFormatting() throws IOException
    {
        // CREATE CARDS WITH DIVERSE FACTORS TO VERIFY THE PRINT REPORT DISPLAYS EACH WITH THE IMPLIED DECIMAL POINT.
        List<String> cards = List.of(
            makeCardWithYearQuadrantFactor("25", "1", "29744"),
            makeCardWithYearQuadrantFactor("25", "2", "10000"),
            makeCardWithYearQuadrantFactor("25", "3", "00001"),
            makeCardWithYearQuadrantFactor("25", "4", "99999"),
            makeCardWithYearQuadrantFactor("26", "1", "45000"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE PRINT REPORT CONTAINS EACH FACTOR WITH THE IMPLIED DECIMAL POINT.
        String printReportContent = Files.readString(printReportFilePath);

        boolean printReportContainsFactor29744 = printReportContent.contains("2.9744");
        assertTrue(printReportContainsFactor29744);

        boolean printReportContainsFactor10000 = printReportContent.contains("1.0000");
        assertTrue(printReportContainsFactor10000);

        boolean printReportContainsFactor00001 = printReportContent.contains("0.0001");
        assertTrue(printReportContainsFactor00001);

        boolean printReportContainsFactor99999 = printReportContent.contains("9.9999");
        assertTrue(printReportContainsFactor99999);

        boolean printReportContainsFactor45000 = printReportContent.contains("4.5000");
        assertTrue(printReportContainsFactor45000);
    }

    /**
     * Verifies that an invalid card produces a "NOT NUMERIC" annotation in the
     * print report error line layout. The COBOL WK-MESG field holds the error
     * text that appears on the same line as the offending card data.
     */
    @Test
    void testPrintErrorLineLayout() throws IOException
    {
        // CREATE A CARD WITH AN INVALID FACTOR.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "1", "ABCDE"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed even with invalid input data.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE PRINT REPORT CONTAINS THE EXPECTED ERROR.
        String printReportContent = Files.readString(printReportFilePath);
        boolean printReportContainsNotNumericError = printReportContent.contains("NOT NUMERIC");
        assertTrue(printReportContainsNotNumericError);
    }

    /**
     * Verifies the binary structure of the factor output file: 2 records should
     * produce exactly 2 x 21 = 42 bytes of contiguous data with no newlines
     * or carriage returns, matching COBOL fixed-length sequential file format.
     */
    @Test
    void testFactorRecordBinaryFormat() throws IOException
    {
        // CREATE TWO CARDS TO VERIFY THE FACTOR OUTPUT FILE IS CONTIGUOUS FIXED-LENGTH BINARY WITH NO LINE TERMINATORS.
        List<String> cards = List.of(
            makeCardWithYearQuadrantFactor("25", "1", "29744"),
            makeCardWithYearQuadrantFactor("25", "2", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        runClerkRealEstateBoardEqualizationFactorProgram();

        // VERIFY THE FACTOR FILE HAS THE EXPECTED TOTAL SIZE.
        int expectedRecordCount = cards.size();
        int expectedFileSizeInBytes = expectedRecordCount * FactorRecord.RECORD_LENGTH_IN_CHARACTERS;
        byte[] factorFileBytes = Files.readAllBytes(equalizationFactorFilePath);
        int factorFileSizeInBytes = factorFileBytes.length;
        assertEquals(expectedFileSizeInBytes, factorFileSizeInBytes, "2 records * 21 bytes = 42 bytes");

        // VERIFY THE FACTOR FILE CONTAINS NO LINE TERMINATOR CHARACTERS.
        for (byte currentByte : factorFileBytes)
        {
            assertNotEquals((byte) '\n', currentByte, "Factor file should not contain newlines");
            assertNotEquals((byte) '\r', currentByte, "Factor file should not contain carriage returns");
        }
    }

    // ========== SINGLE-CARD EDGE CASES ==========

    /**
     * Verifies that the program handles the minimal valid input (a single card)
     * correctly, producing exactly one factor record.
     */
    @Test
    void testSingleValidCard() throws IOException
    {
        // CREATE A CARD WITH A VALID FACTOR.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("25", "1", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed for this test.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY THE CORRECT NUMBER OF FACTOR RECORDS WERE PRODUCED.
        final int EXPECTED_RECORD_COUNT = 1;
        List<String> factorRecords = readFactorRecords(equalizationFactorFilePath);
        int factorRecordCount = factorRecords.size();
        assertEquals(EXPECTED_RECORD_COUNT, factorRecordCount);
    }

    /**
     * Verifies that a single invalid card (non-numeric year "XX") produces zero
     * factor output and a "NOT NUMERIC" error in the print report, without
     * causing any unexpected program failure.
     */
    @Test
    void testSingleInvalidCard() throws IOException
    {
        // CREATE A CARD WITH AN INVALID YEAR.
        List<String> cards = List.of(makeCardWithYearQuadrantFactor("XX", "1", "29744"));
        writeCardFile(cardFilePath, cards);

        // RUN THE PROGRAM TO PROCESS THE CARDS.
        // The program is expected to succeed even with invalid input data.
        int returnCode = runClerkRealEstateBoardEqualizationFactorProgram();
        assertEquals(RETURN_CODE_SUCCESS, returnCode);

        // VERIFY NO FACTOR RECORDS WERE PRODUCED.
        long factorFileSizeInBytes = Files.size(equalizationFactorFilePath);
        assertEquals(0, factorFileSizeInBytes);

        // VERIFY THE PRINT REPORT CONTAINS THE EXPECTED ERROR.
        String printReportContent = Files.readString(printReportFilePath);
        boolean printReportContainsNotNumericError = printReportContent.contains("NOT NUMERIC");
        assertTrue(printReportContainsNotNumericError);
    }
}
