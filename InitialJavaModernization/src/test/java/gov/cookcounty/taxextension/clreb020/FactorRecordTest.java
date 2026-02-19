package gov.cookcounty.taxextension.clreb020;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link FactorRecord} — serialization of output factor records.
 *
 * <p>The factor file must be byte-identical to what the COBOL program produces.
 * Record layout: FT-TAXYR(2) + FT-QUAD(1) + FT-EQFACT(5) + FILLER(13 spaces) = 21 chars.
 */
class FactorRecordTest {

    @Test
    void testToFixedWidthFormat() {
        CardRecord card = new CardRecord("25", "1", "29744");
        FactorRecord factor = new FactorRecord(card);
        String output = factor.toFixedWidth();

        assertEquals(FactorRecord.RECORD_LENGTH, output.length(),
                "Factor record must be exactly 21 characters");
    }

    @Test
    void testToFixedWidthContent() {
        CardRecord card = new CardRecord("25", "1", "29744");
        FactorRecord factor = new FactorRecord(card);
        String output = factor.toFixedWidth();

        // Verify field positions match COBOL layout
        assertEquals("25", output.substring(0, 2), "FT-TAXYR at positions 1-2");
        assertEquals("1", output.substring(2, 3), "FT-QUAD at position 3");
        assertEquals("29744", output.substring(3, 8), "FT-EQFACT at positions 4-8");
        assertEquals("             ", output.substring(8, 21), "FILLER (13 spaces) at positions 9-21");
    }

    @Test
    void testExactByteOutput() {
        // The critical test: verify the exact string matches what COBOL produces
        CardRecord card = new CardRecord("25", "1", "29744");
        FactorRecord factor = new FactorRecord(card);
        assertEquals("25129744             ", factor.toFixedWidth());
    }

    @Test
    void testDifferentQuads() {
        for (int q = 1; q <= 4; q++) {
            CardRecord card = new CardRecord("25", String.valueOf(q), "29744");
            FactorRecord factor = new FactorRecord(card);
            String output = factor.toFixedWidth();
            assertEquals("25", output.substring(0, 2));
            assertEquals(String.valueOf(q), output.substring(2, 3));
            assertEquals("29744", output.substring(3, 8));
        }
    }

    @Test
    void testFieldAccessors() {
        CardRecord card = new CardRecord("24", "3", "31500");
        FactorRecord factor = new FactorRecord(card);
        assertEquals("24", factor.getTaxYear());
        assertEquals("3", factor.getQuad());
        assertEquals("31500", factor.getEqFactor());
    }
}
