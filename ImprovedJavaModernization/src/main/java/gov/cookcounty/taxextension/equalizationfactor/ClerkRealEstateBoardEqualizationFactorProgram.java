package gov.cookcounty.taxextension.equalizationfactor;

import gov.cookcounty.taxextension.common.CobolSequentialFile;

import java.io.IOException;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * Java conversion of CLREB020 — Equalization Factor Editor.
 *
 * <p>Original COBOL program (CLREB020) written by Russ Dober and Ron Urbaniak,
 * September 25, 1978. This program edits and lists equalization factors, writing
 * them to a disk file.
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
 * @see <a href="../../tax_extension_working_copy/CLREB020.cbl">CLREB020.cbl</a>
 */
public class ClerkRealEstateBoardEqualizationFactorProgram
{
    /** Path to the input card file. COBOL: {@code SELECT CARD-FILE ASSIGN TO UT-S-CARDS}. */
    private final Path cardFilePath;
    /** Path to the print report file. COBOL: {@code SELECT PRINT-FILE ASSIGN TO UT-S-PRINT}. */
    private final Path printReportFilePath;
    /** Path to the output equalization factor file. COBOL: {@code SELECT FACTOR-FILE ASSIGN TO UT-S-FACTOR}. */
    private final Path equalizationFactorFilePath;

    /** 
     * Previous card key for sequence checking.
     * Initialized to the equivalent of a COBOL LOW-VALUE.
     * LOW-VALUE is effectively all zeroes, padded to a particular width.
     * The year is 2 digits and quadrant is 1 digit, meaning 3 zero characters.
     */
    private String previousYearQuadrantSequenceKey = "\u0000\u0000\u0000";

    /**
     * CLI entry point.  @see ClerkRealEstateBoardEqualizationFactorProgram
     *
     * <p>Usage: {@code java -jar tax-extension.jar <card_file> <print_file> <factor_file>}
     *
     * <p>File paths can also be provided via environment variables:
     * {@code CARDS}, {@code PRINT}, {@code FACTOR}
     *
     * @param commandLineArguments Command-line arguments: card_file, print_file, factor_file.
     */
    public static void main(String[] commandLineArguments)
    {
        // RESOLVE FILE PATHS FROM COMMAND-LINE ARGUMENTS OR ENVIRONMENT VARIABLES.
        Path cardFilePath;
        Path printReportFilePath;
        Path equalizationFactorFilePath;

        // Command line arguments are prioritized over environment variables since
        // they tend to be more explicitly specified.
        final int MINIMUM_COMMAND_LINE_ARGUMENT_COUNT = 3;
        boolean hasCommandLineArguments = (commandLineArguments.length >= MINIMUM_COMMAND_LINE_ARGUMENT_COUNT);
        if (hasCommandLineArguments)
        {
            // READ IN FILE PATHS FROM COMMAND LINE ARGUMENTS.
            final int CARD_FILE_PATH_ARGUMENT_INDEX = 0;
            final int PRINT_REPORT_FILE_PATH_ARGUMENT_INDEX = 1;
            final int EQUALIZATION_FACTOR_FILE_PATH_ARGUMENT_INDEX = 2;
            cardFilePath = Path.of(commandLineArguments[CARD_FILE_PATH_ARGUMENT_INDEX]);
            printReportFilePath = Path.of(commandLineArguments[PRINT_REPORT_FILE_PATH_ARGUMENT_INDEX]);
            equalizationFactorFilePath = Path.of(commandLineArguments[EQUALIZATION_FACTOR_FILE_PATH_ARGUMENT_INDEX]);
        }
        else
        {
            // FALL BACK TO ENVIRONMENT VARIABLES (SIMILAR TO GNUCOBOL DD NAME MAPPING).
            String cardFilePathString = System.getenv("CARDS");
            String printReportFilePathString = System.getenv("PRINT");
            String equalizationFactorFilePathString = System.getenv("FACTOR");
            // All filepaths need to be specified for the program to run.
            boolean hasAllEnvironmentVariables =
                (cardFilePathString != null) &&
                (printReportFilePathString != null) &&
                (equalizationFactorFilePathString != null);
            if (!hasAllEnvironmentVariables)
            {
                // PROVIDE VISIBILITY INTO THE ERROR.
                System.err.println("Usage: java -jar tax-extension.jar <card_file> <print_file> <factor_file>");
                System.err.println("  Or set environment variables: CARDS, PRINT, FACTOR");
                final int MISSING_FILE_PATHS_EXIT_CODE = 1;
                System.exit(MISSING_FILE_PATHS_EXIT_CODE);
                return;
            }

            // CONVERT THE PATH STRINGS TO ACTUAL PATHS.
            cardFilePath = Path.of(cardFilePathString);
            printReportFilePath = Path.of(printReportFilePathString);
            equalizationFactorFilePath = Path.of(equalizationFactorFilePathString);
        }

        // RUN THE PROGRAM AND EXIT WITH ITS RETURN CODE.
        try
        {
            ClerkRealEstateBoardEqualizationFactorProgram program = new ClerkRealEstateBoardEqualizationFactorProgram(
                cardFilePath,
                printReportFilePath,
                equalizationFactorFilePath);
            int returnCode = program.run();
            System.exit(returnCode);
        }
        catch (IOException exception)
        {
            // PROVIDE VISIBILITY INTO THE ERROR.
            System.err.println("CLREB020 I/O error: " + exception.getMessage());
            final int IO_ERROR_EXIT_CODE = 12;
            System.exit(IO_ERROR_EXIT_CODE);
        }
    }

    /**
     * Creates a new CLREB020 program with the specified file paths.
     *
     * @param cardFilePath   See {@link #cardFilePath}.
     * @param printReportFilePath  See {@link #printReportFilePath}.
     * @param equalizationFactorFilePath See {@link #equalizationFactorFilePath}.
     */
    public ClerkRealEstateBoardEqualizationFactorProgram(
        Path cardFilePath,
        Path printReportFilePath,
        Path equalizationFactorFilePath)
    {
        this.cardFilePath = cardFilePath;
        this.printReportFilePath = printReportFilePath;
        this.equalizationFactorFilePath = equalizationFactorFilePath;
    }

    /**
     * Main entry point — executes the CLREB020 program logic.
     * @see ClerkRealEstateBoardEqualizationFactorProgram
     *
     * @return The program return code (0 = success, 16 = sequence error).
     * @throws IOException If a file I/O error occurs.
     */
    public int run() throws IOException
    {
        // OPEN ALL FILES FOR PROCESSING.
        CobolSequentialFile cardFile = CobolSequentialFile.openForInput(cardFilePath, CardRecord.RECORD_LENGTH_IN_CHARACTERS);
        CobolSequentialFile equalizationFactorFile = CobolSequentialFile.openForOutput(equalizationFactorFilePath, FactorRecord.RECORD_LENGTH_IN_CHARACTERS);
        // The output print report needs the current date for headings in "YYYYMMDD" format.
        String currentDateString = LocalDate.now().format(DateTimeFormatter.BASIC_ISO_DATE);
        PrintReportFile printReport = new PrintReportFile(printReportFilePath, currentDateString);

        // ENSURE ALL FILES ARE CLOSED SHOULD AN EXCEPTION OCCUR.
        // We also need to track the last readiig result for the purposes of determining the return code.
        CardFileReadResult lastCardFileReadResult = null;
        try
        {
            // PROCESS ALL CARD RECORDS UNTIL END-OF-FILE OR SEQUENCE ERROR.
            // We also need to track some statistics on overall number of records processed.
            final int NO_INITIAL_RECORD_COUNT = 0;
            RecordProcessingStatistics entireProgramRunRecordStatistics = new RecordProcessingStatistics(
                NO_INITIAL_RECORD_COUNT, 
                NO_INITIAL_RECORD_COUNT, 
                NO_INITIAL_RECORD_COUNT);
            boolean hasMoreCardsToProcess = true;
            while (hasMoreCardsToProcess)
            {
                lastCardFileReadResult = readCardAndWriteOutput(cardFile, equalizationFactorFile, printReport);

                // ACCUMULATE PER-ITERATION COUNTS INTO RUNNING TOTALS.
                entireProgramRunRecordStatistics.inputCount += lastCardFileReadResult.statistics.inputCount;
                entireProgramRunRecordStatistics.outputCount += lastCardFileReadResult.statistics.outputCount;
                entireProgramRunRecordStatistics.errorCount += lastCardFileReadResult.statistics.errorCount;

                hasMoreCardsToProcess = (!lastCardFileReadResult.isEndOfFile && !lastCardFileReadResult.isSequenceError);
            }

            // DISPLAY PROCESSING SUMMARY COUNTS.
            System.out.println("NO. OF INPUT RECORDS  = " + entireProgramRunRecordStatistics.inputCount);
            System.out.println("NO. OF OUTPUT RECORDS = " + entireProgramRunRecordStatistics.outputCount);
            System.out.println("NO. OF ERROR RECORDS  = " + entireProgramRunRecordStatistics.errorCount);
        }
        finally
        {
            // CLOSE ALL FILES.
            cardFile.close();
            printReport.close();
            equalizationFactorFile.close();
        }

        // DETERMINE THE RETURN CODE BASED ON PROCESSING OUTCOME.
        // Sequence errors are the only other unique remaining return code.
        int returnCode = 0;
        boolean hadSequenceError = (lastCardFileReadResult != null && lastCardFileReadResult.isSequenceError);
        if (hadSequenceError)
        {
            final int SEQUENCE_ERROR_RETURN_CODE = 16;
            returnCode = SEQUENCE_ERROR_RETURN_CODE;
        }
        return returnCode;
    }

    /**
     * Reads the next card, validates it, and writes to the output files.
     *
     * <p>If the card is valid, writes an equalization factor record to the factor file
     * and a detail line to the print report. If the card is invalid, writes an error
     * line to the print report instead. Returns early without writing if end-of-file
     * or a sequence error is detected.
     *
     * @param cardFile                  The input card file to read from.
     * @param equalizationFactorFile    The output factor file to write valid records to.
     * @param printReport               The output print report to write detail/error lines to.
     * @return The result of reading and processing the card, including EOF/sequence error
     *         flags and per-iteration record counts.
     * @throws IOException If a file I/O error occurs during reading or writing.
     */
    private CardFileReadResult readCardAndWriteOutput(
        CobolSequentialFile cardFile,
        CobolSequentialFile equalizationFactorFile,
        PrintReportFile printReport) throws IOException
    {
        // READ THE NEXT CARD RECORD.
        CardFileReadResult cardReadResult = readCard(cardFile);

        // VALIDATE AND PROCESS THE CARD IF POSSIBLE.
        // Statistics need to be tracked.
        final int NO_INITIAL_RECORD_COUNT = 0;
        RecordProcessingStatistics statistics = new RecordProcessingStatistics(NO_INITIAL_RECORD_COUNT, NO_INITIAL_RECORD_COUNT, NO_INITIAL_RECORD_COUNT);
        boolean canProcessCard = (!cardReadResult.isEndOfFile && !cardReadResult.isSequenceError);
        if (canProcessCard)
        {
            // COUNT THE INPUT CARD BEING PROCESSED.
            statistics.inputCount = 1;

            // CHECK IF THE CARD IS VALID.
            boolean isYearValid = cardReadResult.card.isYearNumericAndNonNegativeOrPositive();
            boolean isEqualizationFactorValid = cardReadResult.card.isEqualizationFactorNumericAndNonNegativeOrPositive();
            boolean isAssessmentQuadrantValid = cardReadResult.card.isValidAssessmentQuadrant();
            boolean cardIsValid = isYearValid && isEqualizationFactorValid && isAssessmentQuadrantValid;
            if (cardIsValid)
            {
                // WRITE THE VALID EQUALIZATION FACTOR TO OUTPUT FILE.
                createFactor(equalizationFactorFile, cardReadResult.card);
                statistics.outputCount = 1;

                // WRITE THE CARD TO THE REPORT AS WELL.
                final String NO_ERROR_MESSAGE = "";
                printReport.writeReportLine(cardReadResult.card, NO_ERROR_MESSAGE);
            }
            else
            {
                // COUNT THE INVALID CARD.
                statistics.errorCount = 1;
                
                // WRITE AN ERROR TO THE REPORT.
                final String ERROR_MESSAGE = "NOT NUMERIC";
                printReport.writeReportLine(cardReadResult.card, ERROR_MESSAGE);
            }
        }

        // RETURN THE RESULT OF READING THE CARD.
        CardFileReadResult card_file_read_result = new CardFileReadResult(
            cardReadResult.isEndOfFile,
            cardReadResult.isSequenceError,
            cardReadResult.card,
            statistics);
        return card_file_read_result;
    }

    /**
     * Reads the next card record from the input file and checks sequence ordering.
     *
     * <p>Cards must arrive in ascending year-quadrant order. If a card's key is
     * less than the previous card's key, a sequence error is reported to stdout
     * and the result is flagged accordingly. On successful read, the previous
     * key is advanced for the next comparison.
     *
     * @param cardFile The input card file to read the next record from.
     * @return The result of the read, including the parsed card (if not EOF),
     *         end-of-file and sequence error flags, and empty statistics
     *         (counts are tracked by the caller).
     * @throws IOException If a file I/O error occurs while reading.
     */
    private CardFileReadResult readCard(CobolSequentialFile cardFile) throws IOException
    {
        // INITIAL SOME INITIAL RECORD STATISTICS.
        // This method does not track record counts — the caller handles counting,
        // but they're needed for return values.
        final int NO_RECORD_COUNT = 0;
        RecordProcessingStatistics noRecordsCounted = new RecordProcessingStatistics(
            NO_RECORD_COUNT,
            NO_RECORD_COUNT,
            NO_RECORD_COUNT);
            
        // READ THE NEXT RECORD FROM THE CARD FILE.
        String line = cardFile.readRecord();

        // CHECK IF THE END OF THE FILE HAS BEEN REACHED.
        boolean isEndOfFile = (line == null);
        if (isEndOfFile)
        {
            // INDICATE THE END OF THE FILE WAS REACHED.
            final boolean NO_SEQUENCE_ERROR = false;
            final CardRecord NO_CARD_READ = null;
            CardFileReadResult endOfFileResult = new CardFileReadResult(
                isEndOfFile,
                NO_SEQUENCE_ERROR,
                NO_CARD_READ,
                noRecordsCounted);
            return endOfFileResult;
        }

        // PARSE THE RAW LINE INTO A CARD RECORD.
        CardRecord card = CardRecord.parse(line);
        
        // CHECK THAT CARDS ARRIVE IN ASCENDING YEAR-QUADRANT ORDER.
        String currentYearQuadrantSequenceKey = card.getYearQuadrantSequenceKey();
        boolean isOutOfSequence = (currentYearQuadrantSequenceKey.compareTo(previousYearQuadrantSequenceKey) < 0);
        if (isOutOfSequence)
        {
            // INDICATE A SEQUENCE ERROR OCCURRED.
            System.out.println("CARDS OUT OF SEQUENCE");
            System.out.println("CURRENT CARD " + currentYearQuadrantSequenceKey);
            System.out.println("PREVIOUS CARD " + previousYearQuadrantSequenceKey);
            final boolean NOT_END_OF_FILE = false;
            final boolean IS_SEQUENCE_ERROR = true;
            CardFileReadResult sequenceErrorResult = new CardFileReadResult(
                NOT_END_OF_FILE,
                IS_SEQUENCE_ERROR,
                card,
                noRecordsCounted);
            return sequenceErrorResult;
        }

        // ADVANCE THE SEQUENCE KEY FOR THE NEXT CARD.
        previousYearQuadrantSequenceKey = currentYearQuadrantSequenceKey;

        // RETURN THE CARD READING RESULT.
        final boolean NOT_END_OF_FILE = false;
        final boolean NO_SEQUENCE_ERROR = false;
        CardFileReadResult successfulCardReadResult = new CardFileReadResult(
            NOT_END_OF_FILE,
            NO_SEQUENCE_ERROR,
            card,
            noRecordsCounted);
        return successfulCardReadResult;
    }

    /**
     * Creates an equalization factor record from the card and writes it to the output file.
     *
     * @param equalizationFactorFile The output file to write the factor record to.
     * @param card                   The validated card to create the factor record from.
     * @throws IOException If a file I/O error occurs while writing.
     */
    private void createFactor(CobolSequentialFile equalizationFactorFile, CardRecord card) throws IOException
    {
        // WRITE A FACTOR RECORD TO THE OUTPUT FILE.
        FactorRecord factorRecord = new FactorRecord(card);
        String fixedWidthFactorRecord = factorRecord.toFixedWidth();
        equalizationFactorFile.writeRecord(fixedWidthFactorRecord);
    }
}
