package gov.cookcounty.taxextension.equalizationfactor;

import gov.cookcounty.taxextension.common.CobolLineSequentialFile;

import java.io.IOException;
import java.nio.file.Path;

/**
 * Encapsulates the print report file and its formatting/pagination state
 * for the equalization factor report.
 *
 * <p>Manages page breaks, heading generation, and detail line formatting.
 * This corresponds to the COBOL PRINT-FILE and its associated working storage
 * fields (LINE-CNT, PAGE-CNT, DATE-DT) and report formatting paragraphs
 * (050-WRITE, 060-HDG-ROUTINE).
 */
public class PrintReportFile
{
    /** Record length in characters as defined by COBOL FD RECORD CONTAINS 133 CHARACTERS. */
    public static final int RECORD_LENGTH_IN_CHARACTERS = 133;

    /**
     * The underlying line-sequential file for report output.
     * Handles the physical writing and ASA carriage control.
     */
    private final CobolLineSequentialFile file;
    /**
     * Current date string for report headings, in "YYYYMMDD" format.
     * Set once at construction time and used in every page heading.
     */
    private final String currentDateString;
    /**
     * Line counter for page break detection.
     * Initialized to 60 to force a heading on the very first detail line
     * (since 60 > 55 triggers a page break).
     */
    private int lineCount = 60;
    /**
     * Page counter, incremented each time a new heading is written.
     */
    private int pageCount = 0;

    /**
     * Creates a print report file for the equalization factor report.
     *
     * @param filePath          Path to the output report file.
     * @param currentDateString The current date in "YYYYMMDD" format for report headings.
     */
    public PrintReportFile(Path filePath, String currentDateString) throws IOException
    {
        this.file = new CobolLineSequentialFile(filePath, RECORD_LENGTH_IN_CHARACTERS);
        this.currentDateString = currentDateString;
    }

    /**
     * Writes a detail report line for one card record.
     *
     * @param card         The card record to format into the detail line.
     * @param errorMessage The error annotation for this card, or empty string for valid cards.
     */
    public void writeReportLine(CardRecord card, String errorMessage) throws IOException
    {
        // START A NEW PAGE IF THE CURRENT PAGE IS FULL.
        final int MAXIMUM_LINE_COUNT_BEFORE_PAGE_BREAK = 55;
        boolean isPageFull = (lineCount > MAXIMUM_LINE_COUNT_BEFORE_PAGE_BREAK);
        if (isPageFull)
        {
            // WRITE THE HEADING FOR THE NEW PAGE.
            writeHeading();
        }

        // BUILD THE DETAIL REPORT LINE FROM THE CURRENT CARD FIELDS.
        // Equalization factors need to be in a printable display form: "29744" -> "2.9744" (6 chars)
        String equalizationFactorForDisplay = card.getEqualizationFactorForDisplay();
        // Format specifier "%-Ns" means: left-align the string within an N-character
        // field, padding with spaces on the right. Empty strings ("") produce N spaces,
        // matching COBOL FILLER fields.
        final String EMPTY_SPACE_FILLER = "";
        String workLine = String.format(
            // FILLER (52 spaces) - %-52s
            // WK-YR - %-2s
            // FILLER (8 spaces) - %-8s
            // WK-QUAD - %-1s
            // FILLER (7 spaces) - %-7s
            // WK-FACT — "N.NNNN" - %-6s
            // FILLER (5 spaces) - %-5s
            // WK-MESG - %-11s
            "%-52s%-2s%-8s%-1s%-7s%-6s%-5s%-11s",
            EMPTY_SPACE_FILLER,
            card.taxYear,
            EMPTY_SPACE_FILLER,
            card.assessmentQuadrant,
            EMPTY_SPACE_FILLER,
            equalizationFactorForDisplay,
            EMPTY_SPACE_FILLER,
            errorMessage);

        // WRITE THE DETAIL LINE WITH DOUBLE-SPACING AND ADVANCE THE LINE COUNTER.
        // "AFTER ADVANCING 2" in COBOL means skip 2 lines before printing,
        // producing double-spaced output so each detail line has a blank line above it.
        final int DETAIL_LINE_SPACING = 2;
        file.writeAfterAdvancing(workLine, DETAIL_LINE_SPACING);
        lineCount += DETAIL_LINE_SPACING;
    }

    /**
     * Closes the underlying report file.
     *
     * @throws IOException If an I/O error occurs while closing.
     */
    public void close() throws IOException
    {
        file.close();
    }

    /**
     * Writes the page heading to the print report file (date, title, column headers).
     *
     * @throws IOException If an I/O error occurs.
     */
    private void writeHeading() throws IOException
    {
        // COUNT THE NEW PAGE BEING ADDED.
        ++pageCount;

        // WRITE THE DATE HEADING LINE.
        // This is 11 spaces + date (8 chars).
        // Each format specifier "%-Ns" left-aligns the string within an N-character field,
        // padding with spaces on the right. "%Nd" right-aligns an integer within N digits.
        final String EMPTY_SPACE_FILLER = "";
        String dateReportLine = String.format("%-11s%-8s", EMPTY_SPACE_FILLER, currentDateString);
        file.writeAfterPage(dateReportLine);

        // WRITE THE HEADING LINE WITH CARRIAGE CONTROL.
        // 11 spaces + "CLREB020" (padded to 40) + "OFFICE  OF  THE COUNTY  CLERK" (padded to 70)
        // + "PAGE  " (6) + page number (ZZ9 = 3 chars).
        String headingReportLine = String.format(
            "%-11s%-40s%-70s%-6s%3d",
            EMPTY_SPACE_FILLER,
            "CLREB020",
            "OFFICE  OF  THE COUNTY  CLERK",
            "PAGE  ",
            pageCount);
        // 2 lines between heading lines creates double-spaced output.
        final int HEADING_LINE_SPACING = 2;
        file.writeAfterAdvancing(headingReportLine, HEADING_LINE_SPACING);

        // WRITE THE TITLE LINE WITH CARRIAGE CONTROL.
        // 51 spaces + "EQUALIZATION      FACTORS".
        String titleReportLine = String.format("%-51s%-25s", EMPTY_SPACE_FILLER, "EQUALIZATION      FACTORS");
        file.writeAfterAdvancing(titleReportLine, HEADING_LINE_SPACING);

        // WRITE THE COLUMN HEADER LINE.
        // 51 spaces + "YEAR      QUAD     FACTOR".
        String columnHeaderReportLine = String.format("%-51s%-25s", EMPTY_SPACE_FILLER, "YEAR      QUAD     FACTOR");
        // Triple-spacing before the column headers creates extra visual separation
        // between the title and the start of the data rows on the printed page.
        final int COLUMN_HEADER_LINE_SPACING = 3;
        file.writeAfterAdvancing(columnHeaderReportLine, COLUMN_HEADER_LINE_SPACING);

        // RESET THE LINE COUNTER TO ACCOUNT FOR THE HEADING LINES JUST WRITTEN.
        final int LINE_COUNT_AFTER_HEADING = 8;
        lineCount = LINE_COUNT_AFTER_HEADING;
    }
}