package gov.cookcounty.taxextension.common;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Writes print records with ASA carriage control semantics, emulating COBOL's
 * WRITE ... AFTER ADVANCING and WRITE ... AFTER PAGE operations.
 *
 * <p>In COBOL, print files use ASA carriage control where the number of blank
 * lines before a record is controlled by AFTER ADVANCING n (for n blank lines)
 * or AFTER PAGE (for a page eject / form feed).
 *
 * <p>GnuCOBOL's line sequential mode uses a deferred-LF model:
 * <ul>
 *   <li>AFTER ADVANCING n: write n LF bytes then the record content (no trailing LF)</li>
 *   <li>AFTER PAGE: write a form feed (0x0C) then the record content (no trailing LF);
 *       the deferred LF from the previous record is suppressed</li>
 *   <li>At file close: write a final trailing LF</li>
 * </ul>
 *
 * <p>This model produces byte-identical output to GnuCOBOL for both single-page
 * and multi-page reports. The key insight is that AFTER PAGE replaces the
 * previous record's deferred trailing LF with a form feed, so no LF appears
 * between the last detail line on a page and the form feed.
 *
 * <p>This writer uses explicit LF (0x0A) characters, NOT the platform line
 * separator, to match GnuCOBOL's output on all platforms.
 */
public class CobolPrintWriter implements Closeable {

    private final OutputStream outputStream;
    private final int recordLength;
    private boolean hasDeferredLF = false;

    /**
     * Opens a print file for writing.
     *
     * @param path         path to the output print file
     * @param recordLength record length in characters (from COBOL RECORD CONTAINS)
     * @throws IOException if the file cannot be created
     */
    public CobolPrintWriter(Path path, int recordLength) throws IOException {
        this.outputStream = Files.newOutputStream(path);
        this.recordLength = recordLength;
    }

    /**
     * Writes a record after advancing the specified number of lines.
     *
     * <p>Corresponds to COBOL: {@code WRITE record FROM source AFTER ADVANCING n}
     *
     * <p>Writes the deferred LF from the previous record (counting as the first
     * advance), plus (n-1) additional LFs, then the record content. For the
     * first write (no deferred LF), no leading LFs are written.
     *
     * @param content the print record content
     * @param lines   number of lines to advance (1=single, 2=double, 3=triple)
     * @throws IOException if a write error occurs
     */
    public void writeAfterAdvancing(String content, int lines) throws IOException {
        if (hasDeferredLF) {
            // Deferred LF + (n-1) additional = n total LFs
            for (int i = 0; i < lines; i++) {
                outputStream.write('\n');
            }
        }
        writeRecord(content);
        hasDeferredLF = true;
    }

    /**
     * Writes a record after a page eject (form feed).
     *
     * <p>Corresponds to COBOL: {@code WRITE record FROM source AFTER PAGE}
     *
     * <p>Writes a form feed (0x0C) before the record content. The deferred LF
     * from the previous record is suppressed — the form feed replaces it.
     * This matches GnuCOBOL's behavior where no LF appears between the last
     * record on a page and the form feed character.
     *
     * @param content the print record content
     * @throws IOException if a write error occurs
     */
    public void writeAfterPage(String content) throws IOException {
        // FF replaces the deferred LF (or starts the file for the first write)
        outputStream.write('\f'); // Form feed (0x0C)
        writeRecord(content);
        hasDeferredLF = true;
    }

    /**
     * Writes a padded record (no trailing LF — LF is deferred).
     */
    private void writeRecord(String content) throws IOException {
        String padded = CobolFileReader.padOrTruncate(content, recordLength);
        outputStream.write(padded.getBytes(StandardCharsets.ISO_8859_1));
    }

    @Override
    public void close() throws IOException {
        if (hasDeferredLF) {
            outputStream.write('\n'); // Write final trailing LF at close
        }
        outputStream.close();
    }
}
