package gov.cookcounty.taxextension.equalizationfactor;

/**
 * Represents an output factor record for CLREB020 (Equalization Factor Editor).
 *
 * <p>Maps to the COBOL FD FACTOR-FILE record (FACTOR-REC), which is a 21-character
 * fixed-width record with this layout:
 * <pre>
 *   Positions 1-2:   FT-TAXYR   PIC 99        (tax year)
 *   Position  3:     FT-QUAD    PIC 9         (quadrant)
 *   Positions 4-8:   FT-EQFACT  PIC 9V9999    (equalization factor, display numeric)
 *   Positions 9-21:  FILLER     PIC X(13)     (spaces)
 * </pre>
 *
 * <p>This record is consumed downstream by ASHMA850, ASHMA855, and ASHMA857 via
 * the REBEQFRD01.cpy copybook, which defines the same 21-byte layout:
 * {@code EQ-YEAR(99) + EQ-QUAD(9) + EQ-FACTOR(9V9999) + FILLER(X13)}.
 *
 * <p>The factor is stored as display numeric (character digits), NOT packed decimal.
 * "29744" in the file represents 2.9744 (implied decimal after first digit).
 *
 * @see <a href="../../tax_extension_working_copy/CLREB020.cbl">CLREB020.cbl lines 46-56</a>
 * @see <a href="../../tax_extension_working_copy/include/REBEQFRD01.cpy">REBEQFRD01.cpy</a>
 */
public class FactorRecord
{
    /** Record length in characters as defined by COBOL FD RECORD CONTAINS 21 CHARACTERS. */
    public static final int RECORD_LENGTH_IN_CHARACTERS = 21;

    /**
     * Tax year.
     * Stored as String (not int) because this is a COBOL display-numeric field —
     * the two character digits are written directly as bytes to the fixed-width
     * output file. Using int would lose the leading zero (e.g., "05" → 5) and
     * require reformatting for output.
     */
    public final String taxYear;
    /**
     * Assessment quadrant code.
     * Stored as String (not int) because this is a COBOL display-numeric field —
     * the single character digit is written directly as a byte to the fixed-width
     * output file. No arithmetic is performed on this value.
     */
    public final String assessmentQuadrant;
    /**
     * Equalization factor as display numeric.
     * Stored as String (not BigDecimal) because COBOL display-numeric stores the
     * factor as 5 character digits with an implied (not stored) decimal point after
     * the first digit. The raw character representation "29744" is written directly
     * as bytes to the output file — the downstream consumers (ASHMA850/855/857) expect
     * this exact character layout, not a numeric encoding.
     */
    public final String equalizationFactor;

    /**
     * Creates a factor record from a validated card record.
     *
     * @param card The validated input card record.
     */
    public FactorRecord(CardRecord card)
    {
        this.taxYear = card.taxYear;
        this.assessmentQuadrant = card.assessmentQuadrant;
        this.equalizationFactor = card.equalizationFactor;
    }

    /**
     * Serializes this record to a 21-character fixed-width string.
     *
     * <p>The output must be byte-identical to what the COBOL program produces:
     * 2 chars (year) + 1 char (quad) + 5 chars (factor) + 13 chars (spaces) = 21 chars.
     *
     * @return Exactly 21 characters matching the COBOL FACTOR-REC layout.
     */
    public String toFixedWidth()
    {
        // SERIALIZE THE RECORD AS A 21-CHARACTER FIXED-WIDTH STRING.
        // Result: taxYear(2) + assessmentQuadrant(1) + equalizationFactor(5) + spaces(13)
        //
        // Format specifier "%-N.Ns" means:
        //   '-' = left-align the string within the field
        //   N   = minimum field width (pad with spaces on the right if shorter)
        //   .N  = maximum field width (truncate if longer)
        //   s   = string conversion type
        // Using both N.N ensures each field is exactly N characters wide.
        final String EMPTY_SPACE_FILLER = "";
        String fixedWidthRecord = String.format(
            // FT-TAXYR: 2 characters (%-2.2s)
            // FT-QUAD: 1 character (%-1.1s)
            // FT-EQFACT: 5 characters (%-5.5s)
            // FILLER: 13 spaces (%-13s)
            "%-2.2s%-1.1s%-5.5s%-13s",
            taxYear,              
            assessmentQuadrant,   
            equalizationFactor,   
            EMPTY_SPACE_FILLER);                  
        return fixedWidthRecord;
    }

}
