package gov.cookcounty.taxextension.common;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Represents a COBOL line-sequential file ({@code ORGANIZATION IS LINE SEQUENTIAL}),
 * used for producing human-readable reports and printer output with ASA carriage
 * control semantics.
 *
 * <p>Line-sequential files differ from sequential files ({@link CobolSequentialFile})
 * in several important ways:
 * <ul>
 *   <li>Records contain <b>only printable characters</b> — no binary or packed data</li>
 *   <li>Records are delimited by newline characters, not fixed-width with no delimiters</li>
 *   <li>The {@code WRITE ... AFTER ADVANCING} clause controls vertical spacing</li>
 *   <li>The {@code WRITE ... AFTER PAGE} clause inserts form feed characters for page breaks</li>
 * </ul>
 *
 * <p>This class emulates COBOL's {@code WRITE ... AFTER ADVANCING} and
 * {@code WRITE ... AFTER PAGE} operations using GnuCOBOL's deferred-LF model:
 * <ul>
 *   <li>{@code AFTER ADVANCING n}: write n LF bytes then the record content (no trailing LF)</li>
 *   <li>{@code AFTER PAGE}: write a form feed (0x0C) then the record content (no trailing LF);
 *       the deferred LF from the previous record is suppressed</li>
 *   <li>At file close: write a final trailing LF</li>
 * </ul>
 *
 * <p>This model produces byte-identical output to GnuCOBOL for both single-page
 * and multi-page reports. The key insight is that {@code AFTER PAGE} replaces the
 * previous record's deferred trailing LF with a form feed, so no LF appears
 * between the last detail line on a page and the form feed character.
 *
 * <p>This class uses explicit LF (0x0A) characters, NOT the platform line
 * separator, to match GnuCOBOL's output on all platforms.
 *
 * <p>The class name uses official COBOL terminology ("line sequential") rather
 * than Java I/O terminology ("print writer") to maintain alignment with the
 * source COBOL programs being modernized and the COBOL standard's file
 * organization taxonomy.
 *
 * @see CobolSequentialFile
 * @see <a href="https://www.ibm.com/docs/en/cobol-linux-x86/1.2.0?topic=mode-line-sequential-file-organization">
 *      IBM COBOL — Line-sequential file organization</a>
 * @see <a href="https://www.ibm.com/docs/en/cobol-zos/6.2.0?topic=files-defining-line-sequential-records-in-cobol">
 *      IBM Enterprise COBOL — Defining line-sequential files and records</a>
 * @see <a href="https://www.ibm.com/docs/en/cobol-zos/6.2?topic=statement-write-sequential-files">
 *      IBM Enterprise COBOL — WRITE for sequential files (AFTER ADVANCING)</a>
 * @see <a href="https://gnucobol.sourceforge.io/doc/gnucobol.html">
 *      GnuCOBOL Manual — Line-sequential file handling</a>
 */
public class CobolLineSequentialFile implements Closeable
{
    /**
     * The underlying file handle for writing.
     * Uses OutputStream (not Writer) because line-sequential output requires
     * explicit control over LF (0x0A) and form feed (0x0C) bytes with no
     * platform line-ending translations.
     */
    private final OutputStream outputFile;
    /** Fixed record width from COBOL {@code RECORD CONTAINS n CHARACTERS}. */
    private final int recordLengthInCharacters;
    /** Whether a trailing LF is deferred from the previous write, per GnuCOBOL's deferred-LF model. */
    private boolean hasDeferredLF = false;

    /**
     * Opens a line-sequential file for writing.
     *
     * <p>Corresponds to COBOL: {@code OPEN OUTPUT file-name}
     * (where file-name is declared with {@code ORGANIZATION IS LINE SEQUENTIAL})
     *
     * @param filePath                 Path to the output print file.
     * @param recordLengthInCharacters Expected record width (from COBOL RECORD CONTAINS).
     * @throws IOException If the file cannot be created.
     */
    public CobolLineSequentialFile(Path filePath, int recordLengthInCharacters) throws IOException
    {
        // OPEN THE OUTPUT FILE.
        this.outputFile = Files.newOutputStream(filePath);
        this.recordLengthInCharacters = recordLengthInCharacters;
    }

    /**
     * Closes the file, writing any deferred trailing LF before releasing resources.
     *
     * <p>Corresponds to COBOL: {@code CLOSE file-name}
     *
     * @throws IOException If an I/O error occurs while closing the file.
     */
    @Override
    public void close() throws IOException
    {
        // WRITE THE FINAL TRAILING LINE FEED IF ONE WAS DEFERRED FROM THE LAST RECORD.
        if (hasDeferredLF)
        {
            outputFile.write('\n');
        }

        // CLOSE THE UNDERLYING OUTPUT STREAM.
        outputFile.close();
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
     * @param recordContent The print record content.
     * @param lineCount     Number of lines to advance (1=single, 2=double, 3=triple).
     * @throws IOException If a write error occurs.
     */
    public void writeAfterAdvancing(String recordContent, int lineCount) throws IOException
    {
        // WRITE THE DEFERRED LF PLUS ANY ADDITIONAL BLANK LINES.
        // The deferred LF from the previous record counts as the first advance,
        // so a total of lineCount LFs are written. On the first write (no deferred
        // LF), no leading LFs are written.
        if (hasDeferredLF)
        {
            for (int lineIndex = 0; lineIndex < lineCount; lineIndex++)
            {
                outputFile.write('\n');
            }
        }

        // WRITE THE PADDED RECORD CONTENT AND DEFER ITS TRAILING LF.
        writeRecord(recordContent);
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
     * @param recordContent The print record content.
     * @throws IOException If a write error occurs.
     */
    public void writeAfterPage(String recordContent) throws IOException
    {
        // WRITE A FORM FEED CHARACTER TO START A NEW PAGE.
        // Form feed (0x0C) replaces the deferred LF from the previous record,
        // or starts the file for the very first write.
        outputFile.write('\f');

        // WRITE THE PADDED RECORD CONTENT AND DEFER ITS TRAILING LF.
        writeRecord(recordContent);
        hasDeferredLF = true;
    }

    /**
     * Writes a padded record (no trailing LF — LF is deferred).
     *
     * @param recordContent The record content to pad and write.
     * @throws IOException If a write error occurs.
     */
    private void writeRecord(String recordContent) throws IOException
    {
        // PAD OR TRUNCATE THE RECORD TO THE FIXED WIDTH, THEN WRITE THE BYTES.
        // ISO-8859-1 preserves all byte values — used here for consistency with
        // CobolSequentialFile, even though line-sequential records contain only
        // printable characters.
        String paddedRecord = CobolSequentialFile.padOrTruncate(recordContent, recordLengthInCharacters);
        byte[] recordBytes = paddedRecord.getBytes(StandardCharsets.ISO_8859_1);
        outputFile.write(recordBytes);
    }
}