package gov.cookcounty.taxextension.common;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Reads fixed-width records from a sequential file, emulating COBOL sequential
 * file READ operations.
 *
 * <p>In COBOL, sequential files contain fixed-length records defined by the FD
 * (File Description) RECORD CONTAINS clause. This reader handles both
 * binary fixed-width files (no delimiters, records are exactly recordLength
 * bytes) and line-delimited files (records terminated by LF or CR+LF).
 *
 * <p>The reader reads exactly {@code recordLength} characters per record, then
 * consumes any trailing line-ending characters (CR, LF, or CR+LF). This
 * allows the same reader to handle both binary and line-sequential formats.
 *
 * <p>Records shorter than the expected length (only possible at EOF) are
 * right-padded with spaces.
 */
public class CobolFileReader implements Closeable {

    private final BufferedReader reader;
    private final int recordLength;

    /**
     * Opens a sequential file for reading.
     *
     * @param path         path to the input file
     * @param recordLength expected record length in characters (from COBOL RECORD CONTAINS)
     * @throws IOException if the file cannot be opened
     */
    public CobolFileReader(Path path, int recordLength) throws IOException {
        // BufferedReader is required for mark()/reset() support used in
        // line-ending consumption after each record read.
        this.reader = new BufferedReader(new InputStreamReader(
                Files.newInputStream(path), StandardCharsets.ISO_8859_1));
        this.recordLength = recordLength;
    }

    /**
     * Reads the next record from the file.
     *
     * <p>Corresponds to COBOL: {@code READ file-name AT END ...}
     *
     * <p>Reads exactly {@code recordLength} characters, then consumes any
     * trailing CR/LF characters. This handles both binary fixed-width files
     * (GnuCOBOL RECORDING MODE F) and line-sequential files.
     *
     * @return the record as a fixed-width string (padded/truncated to recordLength),
     *         or {@code null} if end-of-file has been reached
     * @throws IOException if a read error occurs
     */
    public String readRecord() throws IOException {
        char[] buf = new char[recordLength];
        int totalRead = 0;

        // Read exactly recordLength characters
        while (totalRead < recordLength) {
            int n = reader.read(buf, totalRead, recordLength - totalRead);
            if (n == -1) {
                if (totalRead == 0) {
                    return null; // EOF — corresponds to COBOL AT END condition
                }
                // Partial record at EOF — pad with spaces
                break;
            }
            totalRead += n;
        }

        // Consume any trailing line-ending characters (CR, LF, or CR+LF).
        // This allows the reader to work with both binary files (no line endings)
        // and line-sequential files (LF or CR+LF after each record).
        reader.mark(2);
        int ch = reader.read();
        if (ch == '\r') {
            // Check for CR+LF
            reader.mark(1);
            int next = reader.read();
            if (next != '\n' && next != -1) {
                reader.reset(); // Not LF — put it back
            }
        } else if (ch != '\n' && ch != -1) {
            reader.reset(); // Not a line ending — put it back
        }

        String line = new String(buf, 0, totalRead);
        return padOrTruncate(line, recordLength);
    }

    /**
     * Pads a string with trailing spaces or truncates it to exactly the given length.
     * This matches COBOL's fixed-width record behavior where short records are
     * space-filled and long records are truncated.
     */
    static String padOrTruncate(String s, int length) {
        if (s.length() == length) {
            return s;
        } else if (s.length() > length) {
            return s.substring(0, length);
        } else {
            // Pad with spaces (COBOL SPACES = 0x20)
            return String.format("%-" + length + "s", s);
        }
    }

    @Override
    public void close() throws IOException {
        reader.close();
    }
}
