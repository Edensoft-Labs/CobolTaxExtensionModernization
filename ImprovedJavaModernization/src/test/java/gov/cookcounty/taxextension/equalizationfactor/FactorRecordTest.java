package gov.cookcounty.taxextension.equalizationfactor;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link FactorRecord} — serialization of output factor records.
 *
 * <p>The factor file must be byte-identical to what the COBOL program (CLREB020) produces.
 * Record layout: FT-TAXYR(2) + FT-QUAD(1) + FT-EQFACT(5) + FILLER(13 spaces) = 21 chars.
 */
class FactorRecordTest
{
    // ========== FIELD POSITION CONSTANTS ==========
    // Positions within the 21-character FACTOR-REC layout (COBOL copybook CLREB020).

    /** Start position of FT-TAXYR (2-char tax year) in the 21-character FACTOR-REC. COBOL position 1. */
    private static final int TAX_YEAR_START_POSITION = 0;
    /** End position (exclusive) of FT-TAXYR. COBOL position 2. */
    private static final int TAX_YEAR_END_POSITION = 2;

    /** Start position of FT-QUAD (1-char assessment quadrant code) in FACTOR-REC. COBOL position 3. */
    private static final int QUADRANT_START_POSITION = 2;
    /** End position (exclusive) of FT-QUAD. COBOL position 3. */
    private static final int QUADRANT_END_POSITION = 3;

    /** Start position of FT-EQFACT (5-char equalization factor, implied V.9999) in FACTOR-REC. COBOL position 4. */
    private static final int FACTOR_START_POSITION = 3;
    /** End position (exclusive) of FT-EQFACT. COBOL position 8. */
    private static final int FACTOR_END_POSITION = 8;


    // ========== RECORD LENGTH TESTS ==========

    /**
     * Verifies that {@link FactorRecord#toFixedWidth()} produces output whose length
     * matches the COBOL FACTOR-REC layout (21 characters).
     */
    @Test
    void testToFixedWidthFormat()
    {
        // VERIFY THE OUTPUT LENGTH MATCHES THE COBOL RECORD LAYOUT.
        CardRecord card = new CardRecord("25", "1", "29744");
        FactorRecord factor = new FactorRecord(card);
        String fixedWidthFactorRecord = factor.toFixedWidth();

        int actualFactorLengthInCharacters = fixedWidthFactorRecord.length();
        assertEquals(
            FactorRecord.RECORD_LENGTH_IN_CHARACTERS,
            actualFactorLengthInCharacters,
            "Factor record must be exactly " + FactorRecord.RECORD_LENGTH_IN_CHARACTERS + " characters");
    }

    // ========== FIELD POSITION TESTS ==========

    /**
     * Verifies that each field within the fixed-width output occupies the correct
     * byte positions per the COBOL FACTOR-REC layout.
     */
    @Test
    void testToFixedWidthContent()
    {
        // VERIFY EACH FIELD OCCUPIES THE CORRECT POSITION IN THE OUTPUT RECORD.
        CardRecord card = new CardRecord("25", "1", "29744");
        FactorRecord factor = new FactorRecord(card);
        String fixedWidthFactorRecord = factor.toFixedWidth();

        String actualTaxYear = fixedWidthFactorRecord.substring(TAX_YEAR_START_POSITION, TAX_YEAR_END_POSITION);
        assertEquals("25", actualTaxYear, "FT-TAXYR at positions 1-2");

        String actualQuadrant = fixedWidthFactorRecord.substring(QUADRANT_START_POSITION, QUADRANT_END_POSITION);
        assertEquals("1", actualQuadrant, "FT-QUAD at position 3");

        String actualFactor = fixedWidthFactorRecord.substring(FACTOR_START_POSITION, FACTOR_END_POSITION);
        assertEquals("29744", actualFactor, "FT-EQFACT at positions 4-8");

        // Start of FILLER (13 trailing spaces), COBOL positions 9-21.
        final int FILLER_START_POSITION = 8;
        String actualFiller = fixedWidthFactorRecord.substring(FILLER_START_POSITION, FactorRecord.RECORD_LENGTH_IN_CHARACTERS);
        assertEquals("             ", actualFiller, "FILLER (13 spaces) at positions 9-21");
    }

    // ========== BYTE-OUTPUT TESTS ==========

    /**
     * Verifies exact byte-for-byte output for a known input, ensuring the Java
     * implementation produces output identical to the COBOL program.
     */
    @Test
    void testExactByteOutput()
    {
        // VERIFY BYTE-FOR-BYTE OUTPUT MATCHES THE EXPECTED COBOL OUTPUT.
        CardRecord card = new CardRecord("25", "1", "29744");
        FactorRecord factor = new FactorRecord(card);
        // Year + quadrant + factor are combined and padded with spaces to fixed width.
        String fixedWidthFactorRecord = factor.toFixedWidth();
        assertEquals("25129744             ", fixedWidthFactorRecord);
    }

    // ========== ASSESSMENT QUADRANT TESTS ==========

    /**
     * Verifies that all four Cook County quadrant codes (1-4) are correctly placed
     * in position 3 of the output record.
     */
    @Test
    void testDifferentAssessmentQuadrants()
    {
        // VERIFY ALL FOUR QUADRANT CODES PRODUCE CORRECT OUTPUT.
        // Cook County has four assessment quadrants (1 through 4).
        final int FIRST_QUADRANT = 1;
        final int QUADRANT_COUNT = 4;
        for (int quadrantNumber = FIRST_QUADRANT; quadrantNumber <= QUADRANT_COUNT; ++quadrantNumber)
        {
            // VERIFY THE FACTOR RECORD FOR THIS QUADRANT IS CORRECTLY PARSED.
            String quadrantNumberString = String.valueOf(quadrantNumber);
            CardRecord card = new CardRecord("25", quadrantNumberString, "29744");
            FactorRecord factor = new FactorRecord(card);

            // Each field of the fixed-width factor record should be correct.
            String fixedWidthFactorRecord = factor.toFixedWidth();

            String actualTaxYear = fixedWidthFactorRecord.substring(TAX_YEAR_START_POSITION, TAX_YEAR_END_POSITION);
            assertEquals("25", actualTaxYear);

            String actualQuadrant = fixedWidthFactorRecord.substring(QUADRANT_START_POSITION, QUADRANT_END_POSITION);
            assertEquals(quadrantNumberString, actualQuadrant);

            String actualFactor = fixedWidthFactorRecord.substring(FACTOR_START_POSITION, FACTOR_END_POSITION);
            assertEquals("29744", actualFactor);
        }
    }

    // ========== FIELD ACCESSOR TESTS ==========

    /**
     * Verifies that the public fields on {@link FactorRecord} preserve the values
     * from the source {@link CardRecord}.
     */
    @Test
    void testFieldAccessors()
    {
        // VERIFY FIELDS PRESERVE THE VALUES FROM THE SOURCE CARD.
        CardRecord card = new CardRecord("24", "3", "31500");
        FactorRecord factor = new FactorRecord(card);
        assertEquals("24", factor.taxYear);
        assertEquals("3", factor.assessmentQuadrant);
        assertEquals("31500", factor.equalizationFactor);
    }

    // ========== BOUNDARY VALUE TESTS ==========

    /**
     * Verifies output for the smallest valid equalization factor (00001 = 0.0001).
     */
    @Test
    void testMinimumFactor()
    {
        // VERIFY OUTPUT FOR THE SMALLEST VALID FACTOR.
        CardRecord card = new CardRecord("25", "1", "00001");
        FactorRecord factor = new FactorRecord(card);
        // The card components are combined and then padded to a fixed width.
        String fixedWidthFactorRecord = factor.toFixedWidth();
        assertEquals("25100001             ", fixedWidthFactorRecord);
    }

    /**
     * Verifies output for the largest valid equalization factor (99999 = 9.9999).
     */
    @Test
    void testMaximumFactor()
    {
        // VERIFY OUTPUT FOR THE LARGEST VALID FACTOR.
        CardRecord card = new CardRecord("25", "4", "99999");
        FactorRecord factor = new FactorRecord(card);
        // The card components are combined and then padded to a fixed width.
        String fixedWidthFactorRecord = factor.toFixedWidth();
        assertEquals("25499999             ", fixedWidthFactorRecord);
    }

    /**
     * Verifies output for a unity factor (10000 = 1.0000), meaning no equalization
     * adjustment is applied.
     */
    @Test
    void testUnityFactor()
    {
        // VERIFY OUTPUT FOR A UNITY FACTOR (NO ADJUSTMENT).
        CardRecord card = new CardRecord("25", "2", "10000");
        FactorRecord factor = new FactorRecord(card);
        // The card components are combined and then padded to a fixed width.
        String fixedWidthFactorRecord = factor.toFixedWidth();
        assertEquals("25210000             ", fixedWidthFactorRecord);
    }
}
