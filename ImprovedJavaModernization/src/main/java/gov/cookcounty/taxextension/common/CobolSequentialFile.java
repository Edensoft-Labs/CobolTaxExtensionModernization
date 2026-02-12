package gov.cookcounty.taxextension.common;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Represents a COBOL sequential file ({@code ORGANIZATION IS SEQUENTIAL}),
 * supporting both input (READ) and output (WRITE) operations depending on the
 * open mode.
 *
 * <p>In COBOL, a sequential file contains fixed-length records with no
 * delimiters. Records are exactly {@code RECORD CONTAINS n} bytes of raw data,
 * which may include binary, packed decimal, or any other character data. This
 * distinguishes sequential files from line-sequential files, which are
 * text-oriented and delimited by newline characters.
 *
 * <p>This class models the COBOL concept of a single file entity that can be
 * opened for reading or writing — matching the COBOL verbs {@code OPEN INPUT}
 * and {@code OPEN OUTPUT}. The class name uses official COBOL terminology
 * ("sequential file") rather than Java I/O terminology ("reader"/"writer")
 * to maintain alignment with the source COBOL programs being modernized.
 *
 * <h3>Input mode (OPEN INPUT)</h3>
 * <p>Reads fixed-width records via {@link #readRecord()}, handling both binary
 * fixed-width files (no delimiters, records are exactly {@code recordLengthInCharacters}
 * bytes) and line-delimited files (records terminated by LF or CR+LF). Records
 * shorter than the expected length at EOF are right-padded with spaces.
 *
 * <h3>Output mode (OPEN OUTPUT)</h3>
 * <p>Writes fixed-width records via {@link #writeRecord(String)} with NO line
 * terminators, matching GnuCOBOL's binary sequential mode (RECORDING MODE F).
 * Records are padded with trailing spaces or truncated to ensure the exact
 * width.
 *
 * @see CobolLineSequentialFile
 * @see <a href="https://www.ibm.com/docs/en/cobol-zos/6.2.0?topic=files-file-organization-input-output-devices">
 *      IBM Enterprise COBOL — File organization and input-output devices</a>
 * @see <a href="https://gnucobol.sourceforge.io/HTML/gnucobpg.html">
 *      GnuCOBOL Programmer's Guide — Sequential file organization</a>
 */
public class CobolSequentialFile implements Closeable
{
    /** The access mode this file was opened with. */
    public enum OpenMode
    {
        /** The file is open for reading (COBOL: {@code OPEN INPUT}). */
        INPUT,
        /** The file is open for writing (COBOL: {@code OPEN OUTPUT}). */
        OUTPUT
    }

    /** The access mode this file was opened with (INPUT or OUTPUT). */
    private final OpenMode openMode;
    /** Fixed record width from COBOL {@code RECORD CONTAINS n CHARACTERS}. */
    private final int recordLengthInCharacters;
    /**
     * The underlying file handle for INPUT mode; {@code null} in OUTPUT mode.
     * Uses BufferedReader (not InputStream) because readRecord() must read
     * exactly N characters and handle CR/LF line endings transparently.
     */
    private final BufferedReader inputFile;
    /**
     * The underlying file handle for OUTPUT mode; {@code null} in INPUT mode.
     * Uses OutputStream (not Writer) because sequential output must be raw
     * bytes with no character encoding transformations or line-ending translations.
     */
    private final OutputStream outputFile;

    /**
     * Opens a sequential file for reading.
     *
     * <p>Corresponds to COBOL: {@code OPEN INPUT file-name}
     *
     * @param filePath                 Path to the input file.
     * @param recordLengthInCharacters Expected record width (from COBOL RECORD CONTAINS).
     * @return A file opened for reading.
     * @throws IOException If the file cannot be opened.
     */
    public static CobolSequentialFile openForInput(Path filePath, int recordLengthInCharacters) throws IOException
    {
        // OPEN THE FILE IN INPUT MODE.
        // ISO-8859-1 maps each byte (0x00–0xFF) to the identical Unicode code point,
        // preserving all 256 byte values through Java's char-based Reader API.
        // This is necessary because COBOL sequential files may contain packed-decimal
        // and binary fields where every byte value is meaningful.
        InputStream inputFileBytes = Files.newInputStream(filePath);
        InputStreamReader inputFileAsCharacters = new InputStreamReader(inputFileBytes, StandardCharsets.ISO_8859_1);
        BufferedReader inputFile = new BufferedReader(inputFileAsCharacters);
        final OutputStream NO_OUTPUT_FILE = null;
        CobolSequentialFile cobolInputFile = new CobolSequentialFile(
            OpenMode.INPUT,
            recordLengthInCharacters,
            inputFile,
            NO_OUTPUT_FILE);
        return cobolInputFile;
    }

    /**
     * Opens a sequential file for writing.
     *
     * <p>Corresponds to COBOL: {@code OPEN OUTPUT file-name}
     *
     * @param filePath                 Path to the output file.
     * @param recordLengthInCharacters Expected record width (from COBOL RECORD CONTAINS).
     * @return A file opened for writing.
     * @throws IOException If the file cannot be created.
     */
    public static CobolSequentialFile openForOutput(Path filePath, int recordLengthInCharacters) throws IOException
    {
        // OPEN THE FILE IN OUTPUT MODE.
        final BufferedReader NO_INPUT_FILE = null;
        OutputStream outputFile = Files.newOutputStream(filePath);
        CobolSequentialFile cobolOutputFile = new CobolSequentialFile(
            OpenMode.OUTPUT,
            recordLengthInCharacters,
            NO_INPUT_FILE,
            outputFile);
        return cobolOutputFile;
    }

    /**
     * Pads a string record with trailing spaces or truncates it to exactly the given length.
     * This matches COBOL's fixed-width record behavior where short records are
     * space-filled and long records are truncated.
     *
     * @param record            The record to pad or truncate.
     * @param lengthInCharacters The target length in characters.
     * @return A string of exactly {@code lengthInCharacters} characters.
     */
    public static String padOrTruncate(String record, int lengthInCharacters)
    {
        // CHECK IF THE RECORD IS ALREADY OF THE APPROPRIATE LENGTH.
        int recordLengthInCharacters = record.length();
        boolean recordIsExactSize = (recordLengthInCharacters == lengthInCharacters);
        if (recordIsExactSize)
        {
            // RETURN THE RECORD AS-IS.
            return record;
        }

        // CHECK IF THE RECORD IS TOO LONG.
        boolean recordIsTooLong = (recordLengthInCharacters > lengthInCharacters);
        if (recordIsTooLong)
        {
            // RETURN THE RECORD TRUNCATED TO THE MAX LENGTH.
            final int BEGINNING_OF_RECORD_INDEX = 0;
            String truncatedRecord = record.substring(BEGINNING_OF_RECORD_INDEX, lengthInCharacters);
            return truncatedRecord;
        }

        // PAD THE RECORD WITH SPACES UNTIL IT REACHES THE REQUIRED LENGTH.
        // COBOL SPACES = 0x20.
        // String.format() with "%-Ns" left-justifies the string within an N-character
        // field, filling any remaining positions on the right with spaces. The '-' flag
        // means left-align, and 's' is the string conversion type.
        String leftJustifiedPaddingFormat = "%-" + lengthInCharacters + "s";
        String paddedRecord = String.format(leftJustifiedPaddingFormat, record);
        return paddedRecord;
    }

    /**
     * Closes the underlying file handle, releasing system resources.
     *
     * <p>Corresponds to COBOL: {@code CLOSE file-name}
     *
     * <p>Safe to call regardless of the open mode — only the handle that was
     * actually opened (input or output) will be closed.
     *
     * @throws IOException If an I/O error occurs while closing the file.
     */
    @Override
    public void close() throws IOException
    {
        // CLOSE THE INPUT FILE HANDLE IF ONE WAS OPENED.
        boolean inputFileExists = (inputFile != null);
        if (inputFileExists)
        {
            inputFile.close();
        }

        // CLOSE THE OUTPUT FILE HANDLE IF ONE WAS OPENED.
        boolean outputFileExists = (outputFile != null);
        if (outputFileExists)
        {
            outputFile.close();
        }
    }

    /**
     * Reads the next record from the file.
     *
     * <p>Corresponds to COBOL: {@code READ file-name AT END ...}
     *
     * <p>Reads exactly {@code recordLengthInCharacters} characters, then consumes any
     * trailing CR/LF characters. This handles both binary fixed-width files
     * (GnuCOBOL RECORDING MODE F) and line-sequential files.
     *
     * @return The record as a fixed-width string (padded/truncated to recordLengthInCharacters),
     *         or {@code null} if end-of-file has been reached.
     * @throws IOException           If a read error occurs.
     * @throws IllegalStateException If the file was opened for OUTPUT.
     */
    public String readRecord() throws IOException
    {
        // ENSURE THE FILE IS OPENED FOR READING.
        boolean isOpenForReading = (openMode == OpenMode.INPUT);
        if (!isOpenForReading)
        {
            throw new IllegalStateException("Cannot READ a file opened for " + openMode);
        }

        // READ IN THE ENTIRE RECORD.
        final int END_OF_FILE = -1;
        char[] recordCharacters = new char[recordLengthInCharacters];
        int totalCharactersRead = 0;
        boolean hasMoreCharactersToRead = (totalCharactersRead < recordLengthInCharacters);
        while (hasMoreCharactersToRead)
        {
            // TRY AND READ AS MANY REMAINING CHARACTERS AS POSSIBLE.
            int remainingCharacterCount = recordLengthInCharacters - totalCharactersRead;
            int readCharacterCount = inputFile.read(recordCharacters, totalCharactersRead, remainingCharacterCount);
            boolean isEndOfFile = (readCharacterCount == END_OF_FILE);
            if (isEndOfFile)
            {
                // CHECK IF WE'RE AT A VALID FILE POSITION.
                boolean hasReadAnyCharacters = (totalCharactersRead > 0);
                if (!hasReadAnyCharacters)
                {
                    // INDICATE THAT NO VALID RECORD COULD BE RETURNED.
                    // Without any characters read, there isn't a valid record to return.
                    // This corresponds to COBOL AT END condition.
                    return null;
                }
                // Partial record at EOF - we can stop looping and pad with spaces.
                break;
            }

            // TRACK THE NUMBER OF CHARACTERS READ.
            totalCharactersRead += readCharacterCount;
            hasMoreCharactersToRead = (totalCharactersRead < recordLengthInCharacters);
        }

        // CONSUME ANY TRAILING LINE-ENDING CHARACTERS (CR, LF, OR CR+LF).
        // This allows the reader to work with both binary files (no line endings)
        // and line-sequential files (LF or CR+LF after each record).
        // Mark the current position so we can reset() if the next characters aren't line endings.
        // CR+LF is the longest possible line ending.
        final int MAX_LINE_ENDING_CHARACTER_COUNT = 2;
        inputFile.mark(MAX_LINE_ENDING_CHARACTER_COUNT);
        int nextCharacter = inputFile.read();
        boolean isCarriageReturn = (nextCharacter == '\r');
        if (isCarriageReturn)
        {
            // CHECK IF THE CARRIAGE RETURN IS FOLLOWED BY A LINE FEED (CR+LF).
            // Mark the current position so we can reset() if the next character isn't a line feed.
            final int SINGLE_CHARACTER_LOOKAHEAD = 1;
            inputFile.mark(SINGLE_CHARACTER_LOOKAHEAD);
            int characterAfterCarriageReturn = inputFile.read();
            boolean isLineFeedOrEndOfFile = (characterAfterCarriageReturn == '\n' || characterAfterCarriageReturn == END_OF_FILE);
            if (!isLineFeedOrEndOfFile)
            {
                // The character after CR wasn't LF, so put it back.
                inputFile.reset();
            }
        }
        else
        {
            boolean isLineFeedOrEndOfFile = (nextCharacter == '\n' || nextCharacter == END_OF_FILE);
            if (!isLineFeedOrEndOfFile)
            {
                // The character wasn't a line ending, so put it back.
                inputFile.reset();
            }
        }

        // RETURN THE LINE FOR THE RECORD.
        // A partial record at EOF may be shorter than the expected width,
        // so pad it with trailing spaces to match the COBOL fixed-width behavior.
        final int BEGINNING_OF_RECORD = 0;
        String line = new String(recordCharacters, BEGINNING_OF_RECORD, totalCharactersRead);
        String paddedRecord = padOrTruncate(line, recordLengthInCharacters);
        return paddedRecord;
    }

    /**
     * Writes a record to the file.
     *
     * <p>Corresponds to COBOL: {@code WRITE record-name}
     *
     * <p>The record is padded with spaces or truncated to exactly {@code recordLengthInCharacters}
     * characters. No line terminator is appended (binary sequential mode).
     *
     * @param recordContent The record content.
     * @throws IOException           If a write error occurs.
     * @throws IllegalStateException If the file was opened for INPUT.
     */
    public void writeRecord(String recordContent) throws IOException
    {
        // ENSURE THE FILE IS OPENED FOR WRITING.
        boolean isOpenForWriting = (openMode == OpenMode.OUTPUT);
        if (!isOpenForWriting)
        {
            throw new IllegalStateException("Cannot WRITE to a file opened for " + openMode);
        }

        // WRITE THE RECORD TO THE FILE.
        // It must be padded to ensure it fits the fills the full record size.
        // Since this is a raw "binary" file, appropriate bytes for the record must be written.
        // ISO-8859-1 maps each byte (0x00–0xFF) to the identical Unicode code point,
        // preserving all 256 byte values through Java's char-based APIs.
        // This is necessary because COBOL sequential files may contain packed-decimal
        // and binary fields where every byte value is meaningful.
        String paddedRecord = padOrTruncate(recordContent, recordLengthInCharacters);
        byte[] recordBytes = paddedRecord.getBytes(StandardCharsets.ISO_8859_1);
        outputFile.write(recordBytes);
    }

    /**
     * Internal constructor used by static creation methods {@link #openForInput} and {@link #openForOutput}.
     *
     * @param openMode                 See {@link #openMode}.
     * @param recordLengthInCharacters See {@link #recordLengthInCharacters}.
     * @param inputFile                See {@link #inputFile}.
     * @param outputFile               See {@link #outputFile}.
     */
    private CobolSequentialFile(
        OpenMode openMode,
        int recordLengthInCharacters,
        BufferedReader inputFile,
        OutputStream outputFile)
    {
        this.openMode = openMode;
        this.recordLengthInCharacters = recordLengthInCharacters;
        this.inputFile = inputFile;
        this.outputFile = outputFile;
    }
}
