package gov.cookcounty.taxextension.clreb020;

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
public class FactorRecord {

    /** Record length as defined by COBOL FD RECORD CONTAINS 21 CHARACTERS */
    public static final int RECORD_LENGTH = 21;

    private final String taxYear;   // FT-TAXYR: PIC 99 (2 chars)
    private final String quad;      // FT-QUAD: PIC 9 (1 char)
    private final String eqFactor;  // FT-EQFACT: PIC 9V9999 (5 chars, display numeric)

    /**
     * Creates a factor record from a validated card record.
     *
     * <p>COBOL: 040-CREATE-FACTOR (lines 162-168)
     * <pre>
     *   MOVE SPACES TO FACTOR-REC     (line 163)
     *   MOVE CD-YR TO FT-TAXYR        (line 164)
     *   MOVE CD-QUAD TO FT-QUAD       (line 165)
     *   MOVE CD-FACTOR-RD TO FT-EQFACT (line 166)
     * </pre>
     *
     * @param card the validated input card record
     */
    public FactorRecord(CardRecord card) {
        this.taxYear = card.getYear();
        this.quad = card.getQuad();
        this.eqFactor = card.getFactor();
    }

    /**
     * Serializes this record to a 21-character fixed-width string.
     *
     * <p>The output must be byte-identical to what the COBOL program produces:
     * 2 chars (year) + 1 char (quad) + 5 chars (factor) + 13 chars (spaces) = 21 chars.
     *
     * @return exactly 21 characters matching the COBOL FACTOR-REC layout
     */
    public String toFixedWidth() {
        // COBOL: MOVE SPACES TO FACTOR-REC first, then individual field MOVEs
        // Result: taxYear(2) + quad(1) + eqFactor(5) + spaces(13)
        return String.format("%-2.2s%-1.1s%-5.5s%-13s", taxYear, quad, eqFactor, "");
    }

    public String getTaxYear() { return taxYear; }
    public String getQuad() { return quad; }
    public String getEqFactor() { return eqFactor; }
}
