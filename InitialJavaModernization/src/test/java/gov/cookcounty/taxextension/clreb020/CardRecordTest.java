package gov.cookcounty.taxextension.clreb020;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link CardRecord} — parsing and validation of input card records.
 */
class CardRecordTest {

    @Test
    void testParseValidCard() {
        // "25" + "1" + "29744" + 72 spaces = 80 chars
        String line = "25129744" + " ".repeat(72);
        CardRecord card = CardRecord.parse(line);
        assertEquals("25", card.getYear());
        assertEquals("1", card.getQuad());
        assertEquals("29744", card.getFactor());
    }

    @Test
    void testGetKey() {
        CardRecord card = new CardRecord("25", "1", "29744");
        assertEquals("251", card.getKey());
    }

    @Test
    void testValidCard() {
        CardRecord card = new CardRecord("25", "1", "29744");
        assertTrue(card.isYearNumericAndPositive());
        assertTrue(card.isFactorNumericAndPositive());
        assertTrue(card.isValidQuad());
    }

    @Test
    void testYearNonNumeric() {
        CardRecord card = new CardRecord("AB", "1", "29744");
        assertFalse(card.isYearNumericAndPositive());
    }

    @Test
    void testYearZero() {
        CardRecord card = new CardRecord("00", "1", "29744");
        assertFalse(card.isYearNumericAndPositive());
    }

    @Test
    void testFactorNonNumeric() {
        CardRecord card = new CardRecord("25", "1", "2A744");
        assertFalse(card.isFactorNumericAndPositive());
    }

    @Test
    void testFactorZero() {
        CardRecord card = new CardRecord("25", "1", "00000");
        assertFalse(card.isFactorNumericAndPositive());
    }

    @Test
    void testInvalidQuad() {
        CardRecord card = new CardRecord("25", "5", "29744");
        assertFalse(card.isValidQuad());
        // Also test '0'
        CardRecord card0 = new CardRecord("25", "0", "29744");
        assertFalse(card0.isValidQuad());
    }

    @Test
    void testValidQuads() {
        for (String q : new String[]{"1", "2", "3", "4"}) {
            CardRecord card = new CardRecord("25", q, "29744");
            assertTrue(card.isValidQuad(), "Quad " + q + " should be valid");
        }
    }

    @Test
    void testFactorForDisplay() {
        CardRecord card = new CardRecord("25", "1", "29744");
        assertEquals("2.9744", card.getFactorForDisplay());
    }

    @Test
    void testFactorIntegerAndDecimalParts() {
        CardRecord card = new CardRecord("25", "1", "29744");
        assertEquals("2", card.getFactorIntegerPart());
        assertEquals("9744", card.getFactorDecimalPart());
    }

    @Test
    void testParseShortLine() {
        // Lines shorter than 80 chars should be padded
        CardRecord card = CardRecord.parse("25129744");
        assertEquals("25", card.getYear());
        assertEquals("1", card.getQuad());
        assertEquals("29744", card.getFactor());
    }
}
