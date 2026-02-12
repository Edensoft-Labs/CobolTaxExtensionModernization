package gov.cookcounty.taxextension.common;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Writes fixed-width records to a sequential file, emulating COBOL sequential
 * file WRITE operations.
 *
 * <p>Records are written as exactly {@code recordLength} bytes with NO line
 * terminators, matching GnuCOBOL's binary sequential mode (RECORDING MODE F).
 * This produces byte-identical output to the compiled COBOL program.
 *
 * <p>Records are padded with trailing spaces or truncated to ensure the exact
 * width, matching COBOL's WRITE behavior for fixed-length records.
 */
public class CobolFileWriter implements Closeable {

    private final OutputStream outputStream;
    private final int recordLength;

    /**
     * Opens a sequential file for writing.
     *
     * @param path         path to the output file
     * @param recordLength record length in characters (from COBOL RECORD CONTAINS)
     * @throws IOException if the file cannot be created
     */
    public CobolFileWriter(Path path, int recordLength) throws IOException {
        this.outputStream = Files.newOutputStream(path);
        this.recordLength = recordLength;
    }

    /**
     * Writes a record to the file.
     *
     * <p>Corresponds to COBOL: {@code WRITE record-name}
     *
     * <p>The record is padded with spaces or truncated to exactly {@code recordLength}
     * characters. No line terminator is appended (binary sequential mode).
     *
     * @param content the record content
     * @throws IOException if a write error occurs
     */
    public void writeRecord(String content) throws IOException {
        String padded = CobolFileReader.padOrTruncate(content, recordLength);
        outputStream.write(padded.getBytes(StandardCharsets.ISO_8859_1));
    }

    @Override
    public void close() throws IOException {
        outputStream.close();
    }
}
