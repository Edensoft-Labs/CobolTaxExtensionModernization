package gov.cookcounty.taxextension.equalizationfactor;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link CardRecord} — parsing and validation of input card records.
 *
 * <p>Tests cover equivalence partitioning, boundary analysis, and structured basis
 * testing per Code Complete 2nd Edition Ch. 22.
 */
class CardRecordTest
{
    // ===== Parsing =====

    @Test
    void testParseValidCard()
    {
        // "25" + "1" + "29744" + 72 spaces = 80 chars
        String line = "25129744" + " ".repeat(72);
        CardRecord card = CardRecord.parse(line);
        assertEquals("25", card.taxYear);
        assertEquals("1", card.assessmentQuadrant);
        assertEquals("29744", card.equalizationFactor);
    }

    @Test
    void testParseShortLine()
    {
        CardRecord card = CardRecord.parse("25129744");
        assertEquals("25", card.taxYear);
        assertEquals("1", card.assessmentQuadrant);
        assertEquals("29744", card.equalizationFactor);
    }

    @Test
    void testGetKey()
    {
        CardRecord card = new CardRecord("25", "1", "29744");
        assertEquals("251", card.getYearQuadrantSequenceKey());
    }

    @Test
    void testValidCard()
    {
        CardRecord card = new CardRecord("25", "1", "29744");
        assertTrue(card.isYearNumericAndNonNegativeOrPositive());
        assertTrue(card.isEqualizationFactorNumericAndNonNegativeOrPositive());
        assertTrue(card.isValidAssessmentQuadrant());
    }

    // ===== Year Validation =====

    @ParameterizedTest
    @ValueSource(strings = {"01", "10", "25", "50", "99"})
    void testYearValidNumeric(String year)
    {
        CardRecord card = new CardRecord(year, "1", "29744");
        assertTrue(card.isYearNumericAndNonNegativeOrPositive(),
                "Year '" + year + "' should be valid");
    }

    @Test
    void testYearZeroDefaultAcceptsMatchingCobol()
    {
        // Default (COBOL-compatible) behavior: "00" is accepted because the original
        // COBOL GREATER THAN 0 comparison is alphanumeric, not numeric, and "00" > "0 "
        // is TRUE in COBOL (0x30 > 0x20 in the second position).
        CardRecord card = new CardRecord("00", "1", "29744");
        assertTrue(card.isYearNumericAndNonNegativeOrPositive());
    }

    @Test
    void testYearZeroStrictPositiveRejects()
    {
        // Strict positive behavior: "00" is rejected because parseInt("00") == 0,
        // and 0 > 0 is false.
        CardRecord card = new CardRecord("00", "1", "29744");
        final boolean MUST_BE_STRICT_POSITIVE = true;
        assertFalse(card.isYearNumericAndNonNegativeOrPositive(MUST_BE_STRICT_POSITIVE));
    }

    @Test
    void testYearNonNumeric()
    {
        CardRecord card = new CardRecord("AB", "1", "29744");
        assertFalse(card.isYearNumericAndNonNegativeOrPositive());
    }

    @Test
    void testYearInvalidMixed()
    {
        CardRecord card = new CardRecord("2A", "1", "29744");
        assertFalse(card.isYearNumericAndNonNegativeOrPositive());
    }

    @Test
    void testYearInvalidSpaces()
    {
        CardRecord card = new CardRecord("  ", "1", "29744");
        assertFalse(card.isYearNumericAndNonNegativeOrPositive());
    }

    // ===== Quadrant Validation =====

    @ParameterizedTest
    @ValueSource(strings = {"1", "2", "3", "4"})
    void testQuadValid(String quad)
    {
        CardRecord card = new CardRecord("25", quad, "29744");
        assertTrue(card.isValidAssessmentQuadrant(), "Quad '" + quad + "' should be valid");
    }

    @Test
    void testInvalidQuad0()
    {
        CardRecord card = new CardRecord("25", "0", "29744");
        assertFalse(card.isValidAssessmentQuadrant());
    }

    @Test
    void testInvalidQuad5()
    {
        CardRecord card = new CardRecord("25", "5", "29744");
        assertFalse(card.isValidAssessmentQuadrant());
    }

    @Test
    void testInvalidQuad9()
    {
        CardRecord card = new CardRecord("25", "9", "29744");
        assertFalse(card.isValidAssessmentQuadrant());
    }

    @Test
    void testInvalidQuadSpace()
    {
        CardRecord card = new CardRecord("25", " ", "29744");
        assertFalse(card.isValidAssessmentQuadrant());
    }

    @Test
    void testInvalidQuadLetter()
    {
        CardRecord card = new CardRecord("25", "A", "29744");
        assertFalse(card.isValidAssessmentQuadrant());
    }

    // ===== Factor Validation =====

    @ParameterizedTest
    @ValueSource(strings = {"00001", "10000", "29744", "45000", "99999"})
    void testFactorValidNumeric(String factor)
    {
        CardRecord card = new CardRecord("25", "1", factor);
        assertTrue(card.isEqualizationFactorNumericAndNonNegativeOrPositive(),
                "Factor '" + factor + "' should be valid");
    }

    @Test
    void testFactorZeroDefaultAcceptsMatchingCobol()
    {
        // Default (COBOL-compatible) behavior: "00000" is accepted because the original
        // COBOL GREATER THAN 0 comparison is alphanumeric, not numeric, and "00000" > "0    "
        // is TRUE in COBOL (0x30 > 0x20 in the second position).
        CardRecord card = new CardRecord("25", "1", "00000");
        assertTrue(card.isEqualizationFactorNumericAndNonNegativeOrPositive());
    }

    @Test
    void testFactorZeroStrictPositiveRejects()
    {
        // Strict positive behavior: "00000" is rejected because parseInt("00000") == 0,
        // and 0 > 0 is false.
        CardRecord card = new CardRecord("25", "1", "00000");
        final boolean MUST_BE_STRICT_POSITIVE = true;
        assertFalse(card.isEqualizationFactorNumericAndNonNegativeOrPositive(MUST_BE_STRICT_POSITIVE));
    }

    @Test
    void testFactorNonNumeric()
    {
        CardRecord card = new CardRecord("25", "1", "2A744");
        assertFalse(card.isEqualizationFactorNumericAndNonNegativeOrPositive());
    }

    @Test
    void testFactorInvalidMixed()
    {
        CardRecord card = new CardRecord("25", "1", "29A44");
        assertFalse(card.isEqualizationFactorNumericAndNonNegativeOrPositive());
    }

    @Test
    void testFactorInvalidSpaces()
    {
        CardRecord card = new CardRecord("25", "1", "2 744");
        assertFalse(card.isEqualizationFactorNumericAndNonNegativeOrPositive());
    }

    // ===== Factor Display Formatting =====

    @Test
    void testFactorForDisplay()
    {
        CardRecord card = new CardRecord("25", "1", "29744");
        assertEquals("2.9744", card.getEqualizationFactorForDisplay());
    }

    @Test
    void testFactorDisplayUnity()
    {
        CardRecord card = new CardRecord("25", "1", "10000");
        assertEquals("1.0000", card.getEqualizationFactorForDisplay());
    }

    @Test
    void testFactorDisplayMinimum()
    {
        CardRecord card = new CardRecord("25", "1", "00001");
        assertEquals("0.0001", card.getEqualizationFactorForDisplay());
    }

    @Test
    void testFactorDisplayMaximum()
    {
        CardRecord card = new CardRecord("25", "1", "99999");
        assertEquals("9.9999", card.getEqualizationFactorForDisplay());
    }

    @Test
    void testFactorIntegerAndDecimalParts()
    {
        CardRecord card = new CardRecord("25", "1", "29744");
        assertEquals("2", card.getEqualizationFactorIntegerPart());
        assertEquals("9744", card.getEqualizationFactorDecimalPart());
    }
}
