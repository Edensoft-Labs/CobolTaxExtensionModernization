package gov.cookcounty.taxextension.clreb020;

/**
 * Represents an input card record for CLREB020 (Equalization Factor Editor).
 *
 * <p>Maps to the COBOL FD CARD-FILE record (CARD-REC), which is an 80-character
 * fixed-width record with this layout:
 * <pre>
 *   Positions 1-2:  CD-YR       PIC XX        (tax year, e.g., "25")
 *   Position  3:    CD-QUAD     PIC X         (quadrant: '1' thru '4')
 *   Positions 4-8:  CD-FACTOR   PIC X(5)      (factor as characters, e.g., "29744")
 *                   CD-FACTOR-RD REDEFINES CD-FACTOR PIC 9V9999
 *   Positions 9-80: CD-FILLER   PIC X(72)     (unused, spaces)
 * </pre>
 *
 * <p>The CD-FACTOR field has two views via REDEFINES:
 * <ul>
 *   <li>Alphanumeric view (CD-FACTOR): 5 characters, used for NUMERIC test</li>
 *   <li>Numeric view (CD-FACTOR-RD): PIC 9V9999 (implied decimal after first digit)</li>
 * </ul>
 *
 * @see <a href="../../tax_extension_working_copy/CLREB020.cbl">CLREB020.cbl lines 27-36</a>
 */
public class CardRecord {

    /** Record length as defined by COBOL FD RECORD CONTAINS 80 CHARACTERS */
    public static final int RECORD_LENGTH = 80;

    // Field values stored as raw character strings matching COBOL PIC definitions
    private final String year;    // CD-YR: PIC XX (positions 1-2)
    private final String quad;    // CD-QUAD: PIC X (position 3)
    private final String factor;  // CD-FACTOR: PIC X(5) (positions 4-8)

    /**
     * Creates a card record from individual field values.
     *
     * @param year   2-character year string (CD-YR)
     * @param quad   1-character quadrant string (CD-QUAD)
     * @param factor 5-character factor string (CD-FACTOR)
     */
    public CardRecord(String year, String quad, String factor) {
        this.year = year;
        this.quad = quad;
        this.factor = factor;
    }

    /**
     * Parses a card record from an 80-character fixed-width line.
     *
     * <p>Corresponds to reading the CARD-REC record in COBOL.
     *
     * @param line the 80-character input line
     * @return parsed card record
     */
    public static CardRecord parse(String line) {
        // Ensure line is at least 8 characters (year + quad + factor)
        String padded = line.length() >= 8 ? line : String.format("%-80s", line);
        String year = padded.substring(0, 2);     // CD-YR: positions 1-2
        String quad = padded.substring(2, 3);     // CD-QUAD: position 3
        String factor = padded.substring(3, 8);   // CD-FACTOR: positions 4-8
        return new CardRecord(year, quad, factor);
    }

    /**
     * Returns the sequence key used for ascending-order check.
     *
     * <p>In COBOL (line 149): {@code IF CARD LESS THAN PREV-CARD}
     * The CARD group item consists of CD-YR + CD-QUAD (3 characters).
     * Comparison is alphanumeric (character-by-character, left to right).
     *
     * @return 3-character key (year + quad) for sequence checking
     */
    public String getKey() {
        return year + quad;
    }

    // --- Validation methods (map to COBOL IF conditions, lines 130-134) ---

    /**
     * Tests if the year field is numeric and greater than zero.
     *
     * <p>COBOL: {@code IF CD-YR NUMERIC AND CD-YR GREATER THAN 0} (lines 130-131)
     *
     * @return true if year is a 2-digit number > 0
     */
    public boolean isYearNumericAndPositive() {
        return year.matches("\\d{2}") && Integer.parseInt(year) > 0;
    }

    /**
     * Tests if the factor field is numeric and greater than zero.
     *
     * <p>COBOL: {@code IF CD-FACTOR NUMERIC AND CD-FACTOR GREATER THAN 0} (lines 132-133)
     * Uses the alphanumeric view (CD-FACTOR PIC X(5)) for the NUMERIC test.
     *
     * @return true if factor is a 5-digit number > 0
     */
    public boolean isFactorNumericAndPositive() {
        return factor.matches("\\d{5}") && Integer.parseInt(factor) > 0;
    }

    /**
     * Tests if the quadrant is valid (1 through 4).
     *
     * <p>COBOL: {@code 88 VALID-QUAD VALUE '1' THRU '4'} (line 31)
     *
     * @return true if quad is '1', '2', '3', or '4'
     */
    public boolean isValidQuad() {
        return quad.length() == 1 && quad.charAt(0) >= '1' && quad.charAt(0) <= '4';
    }

    // --- Field accessors ---

    public String getYear() { return year; }
    public String getQuad() { return quad; }
    public String getFactor() { return factor; }

    /**
     * Returns the first digit of the factor (integer part before implied decimal).
     *
     * <p>COBOL: CD-FT1 PIC X (line 33) — the first character of CD-FACTOR.
     *
     * @return single character (e.g., '2' from "29744")
     */
    public String getFactorIntegerPart() {
        return factor.substring(0, 1);
    }

    /**
     * Returns the last 4 digits of the factor (fractional part after implied decimal).
     *
     * <p>COBOL: CD-FT4 PIC X(4) (line 34) — characters 2-5 of CD-FACTOR.
     *
     * @return 4-character string (e.g., "9744" from "29744")
     */
    public String getFactorDecimalPart() {
        return factor.substring(1, 5);
    }

    /**
     * Returns the factor formatted for display with a decimal point.
     *
     * <p>COBOL: WORK-FACTOR is built by moving CD-FT1 to WK-FACT1, then a literal '.',
     * then CD-FT4 to WK-FACT4 (lines 73-75, 178-180). So "29744" becomes "2.9744".
     *
     * @return factor with decimal point inserted (e.g., "2.9744")
     */
    public String getFactorForDisplay() {
        return getFactorIntegerPart() + "." + getFactorDecimalPart();
    }
}
