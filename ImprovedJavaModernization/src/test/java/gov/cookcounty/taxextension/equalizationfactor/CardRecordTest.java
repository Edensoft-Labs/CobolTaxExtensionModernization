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
 *
 * <p>Card record layout (80-byte COBOL card image):
 * CD-YR(2) + CD-QUAD(1) + CD-FACTOR(5) + FILLER(72 spaces) = 80 chars.
 */
class CardRecordTest
{
    // ========== PARSING TESTS ==========

    /**
     * Verifies that a full 80-character card line is parsed into the correct
     * year, quadrant, and factor fields.
     */
    @Test
    void testParseValidCard()
    {
        // VERIFY A VALID 80-CHARACTER CARD WITH FILLER CAN BE PROPERLY PARSED.
        // An 80-character COBOL card image: "25129744" is year(2) + quadrant(1) + factor(5);
        // the remaining 72 characters are space filler.
        final int CARD_FILLER_LENGTH_IN_CHARACTERS = 72;
        String line = "25129744" + " ".repeat(CARD_FILLER_LENGTH_IN_CHARACTERS);
        CardRecord card = CardRecord.parse(line);
        assertEquals("25", card.taxYear);
        assertEquals("1", card.assessmentQuadrant);
        assertEquals("29744", card.equalizationFactor);
    }

    /**
     * Verifies that a short line (no trailing filler) is still parsed correctly.
     * The COBOL program reads fixed-width, but Java parsing should handle truncated input.
     */
    @Test
    void testParseShortLine()
    {
        // VERIFY A SHORT CARD WITHOUT FILLER CAN BE PROPERLY PARSED.
        // "25129744" is year(2) + quadrant(1) + factor(5), with no trailing filler.
        CardRecord card = CardRecord.parse("25129744");
        assertEquals("25", card.taxYear);
        assertEquals("1", card.assessmentQuadrant);
        assertEquals("29744", card.equalizationFactor);
    }

    // ========== SEQUENCE KEY TESTS ==========

    /**
     * Verifies that the year-quadrant sequence key concatenates year + quadrant,
     * used by the program to detect duplicate or out-of-sequence cards.
     */
    @Test
    void testGetKey()
    {
        // VERIFY THE YEAR-QUADRANT SEQUENCE KEY IS CORRECTLY FORMED.
        // Card with year "25" and quadrant "1"; key should concatenate to "251".
        CardRecord card = new CardRecord("25", "1", "29744");
        String actualYearQuadrantSequenceKey = card.getYearQuadrantSequenceKey();
        assertEquals("251", actualYearQuadrantSequenceKey);
    }

    // ========== COMBINED VALIDATION TESTS ==========

    /**
     * Verifies that a card with valid numeric year, valid quadrant (1-4), and
     * valid numeric factor passes all three validation checks.
     */
    @Test
    void testValidCard()
    {
        // VERIFY A VALID CARD PASSES ALL VALIDATION CHECKS.
        // Numeric year "25", valid quadrant "1", numeric factor "29744".
        CardRecord card = new CardRecord("25", "1", "29744");

        boolean isYearValid = card.isYearNumericAndNonNegativeOrPositive();
        assertTrue(isYearValid);

        boolean isFactorValid = card.isEqualizationFactorNumericAndNonNegativeOrPositive();
        assertTrue(isFactorValid);

        boolean isQuadrantValid = card.isValidAssessmentQuadrant();
        assertTrue(isQuadrantValid);
    }

    // ========== YEAR VALIDATION TESTS ==========

    /**
     * Verifies that representative valid numeric years (01-99) pass validation.
     *
     * @param year two-character numeric year string
     */
    @ParameterizedTest
    @ValueSource(strings = {"01", "10", "25", "50", "99"})
    void testYearValidNumeric(String year)
    {
        // VERIFY VALID NUMERIC YEARS ARE ACCEPTED.
        CardRecord card = new CardRecord(year, "1", "29744");
        boolean isYearValid = card.isYearNumericAndNonNegativeOrPositive();
        assertTrue(isYearValid, "Year '" + year + "' should be valid");
    }

    /**
     * Verifies that year "00" is accepted under default (COBOL-compatible) behavior.
     * The original COBOL {@code GREATER THAN 0} comparison is alphanumeric, not numeric,
     * and "00" > "0 " is TRUE in COBOL (0x30 > 0x20 in the second position).
     */
    @Test
    void testYearZeroDefaultAcceptsMatchingCobol()
    {
        // VERIFY YEAR "00" IS ACCEPTED UNDER COBOL-COMPATIBLE BEHAVIOR.
        CardRecord card = new CardRecord("00", "1", "29744");
        boolean isYearValid = card.isYearNumericAndNonNegativeOrPositive();
        assertTrue(isYearValid);
    }

    /**
     * Verifies that year "00" is rejected under strict-positive mode, where
     * {@code parseInt("00") == 0} and {@code 0 > 0} is false.
     */
    @Test
    void testYearZeroStrictPositiveRejects()
    {
        // VERIFY YEAR "00" IS REJECTED UNDER STRICT-POSITIVE MODE.
        CardRecord card = new CardRecord("00", "1", "29744");
        final boolean MUST_BE_STRICT_POSITIVE = true;
        boolean isYearValid = card.isYearNumericAndNonNegativeOrPositive(MUST_BE_STRICT_POSITIVE);
        assertFalse(isYearValid);
    }

    /** Verifies that fully alphabetic year ("AB") fails numeric validation. */
    @Test
    void testYearNonNumeric()
    {
        // VERIFY AN ALPHABETIC YEAR IS REJECTED.
        CardRecord card = new CardRecord("AB", "1", "29744");
        boolean isYearValid = card.isYearNumericAndNonNegativeOrPositive();
        assertFalse(isYearValid);
    }

    /** Verifies that a mixed alpha-numeric year ("2A") fails validation. */
    @Test
    void testYearInvalidMixed()
    {
        // VERIFY A MIXED ALPHA-NUMERIC YEAR IS REJECTED.
        CardRecord card = new CardRecord("2A", "1", "29744");
        boolean isYearValid = card.isYearNumericAndNonNegativeOrPositive();
        assertFalse(isYearValid);
    }

    /** Verifies that an all-spaces year fails validation. */
    @Test
    void testYearInvalidSpaces()
    {
        // VERIFY AN ALL-SPACES YEAR IS REJECTED.
        CardRecord card = new CardRecord("  ", "1", "29744");
        boolean isYearValid = card.isYearNumericAndNonNegativeOrPositive();
        assertFalse(isYearValid);
    }

    // ========== QUADRANT VALIDATION TESTS ==========

    /**
     * Verifies that all four valid quadrant codes (1-4) pass validation.
     * Cook County is divided into four assessment quadrants.
     *
     * @param assessmentQuadrant single-character quadrant code
     */
    @ParameterizedTest
    @ValueSource(strings = {"1", "2", "3", "4"})
    void testAssessmentQuadrantValid(String assessmentQuadrant)
    {
        // VERIFY ALL FOUR VALID QUADRANT CODES ARE ACCEPTED.
        CardRecord card = new CardRecord("25", assessmentQuadrant, "29744");
        boolean isQuadrantValid = card.isValidAssessmentQuadrant();
        assertTrue(
            isQuadrantValid,
            "Assessment quadrant '" + assessmentQuadrant + "' should be valid");
    }

    /** Verifies that quadrant "0" (below valid range 1-4) is rejected. */
    @Test
    void testInvalidAssessmentQuadrant0()
    {
        // VERIFY A QUADRANT BELOW VALID RANGE (0) IS REJECTED.
        CardRecord card = new CardRecord("25", "0", "29744");
        boolean isQuadrantValid = card.isValidAssessmentQuadrant();
        assertFalse(isQuadrantValid);
    }

    /** Verifies that quadrant "5" (above valid range 1-4) is rejected. */
    @Test
    void testInvalidAssessmentQuadrant5()
    {
        // VERIFY A QUADRANT ABOVE VALID RANGE (5) IS REJECTED.
        CardRecord card = new CardRecord("25", "5", "29744");
        boolean isQuadrantValid = card.isValidAssessmentQuadrant();
        assertFalse(isQuadrantValid);
    }

    /** Verifies that quadrant "9" (far above valid range) is rejected. */
    @Test
    void testInvalidAssessmentQuadrant9()
    {
        // VERIFY A QUADRANT FAR ABOVE VALID RANGE (9) IS REJECTED.
        CardRecord card = new CardRecord("25", "9", "29744");
        boolean isQuadrantValid = card.isValidAssessmentQuadrant();
        assertFalse(isQuadrantValid);
    }

    /** Verifies that a space character as quadrant is rejected. */
    @Test
    void testInvalidAssessmentQuadrantSpace()
    {
        // VERIFY A SPACE QUADRANT IS REJECTED.
        CardRecord card = new CardRecord("25", " ", "29744");
        boolean isQuadrantValid = card.isValidAssessmentQuadrant();
        assertFalse(isQuadrantValid);
    }

    /** Verifies that an alphabetic character as quadrant is rejected. */
    @Test
    void testInvalidAssessmentQuadrantLetter()
    {
        // VERIFY AN ALPHABETIC QUADRANT IS REJECTED.
        CardRecord card = new CardRecord("25", "A", "29744");
        boolean isQuadrantValid = card.isValidAssessmentQuadrant();
        assertFalse(isQuadrantValid);
    }

    // ========== FACTOR VALIDATION TESTS ==========

    /**
     * Verifies that representative valid numeric factors pass validation.
     * Factor is a 5-digit number with implied V.9999 decimal (e.g., 29744 = 2.9744).
     *
     * @param factor five-character numeric factor string
     */
    @ParameterizedTest
    @ValueSource(strings = {"00001", "10000", "29744", "45000", "99999"})
    void testFactorValidNumeric(String factor)
    {
        // VERIFY VALID NUMERIC FACTORS ARE ACCEPTED.
        CardRecord card = new CardRecord("25", "1", factor);
        boolean isFactorValid = card.isEqualizationFactorNumericAndNonNegativeOrPositive();
        assertTrue(isFactorValid, "Factor '" + factor + "' should be valid");
    }

    /**
     * Verifies that factor "00000" is accepted under default (COBOL-compatible) behavior.
     * The original COBOL {@code GREATER THAN 0} comparison is alphanumeric, not numeric,
     * and "00000" > "0    " is TRUE in COBOL (0x30 > 0x20 in the second position).
     */
    @Test
    void testFactorZeroDefaultAcceptsMatchingCobol()
    {
        // VERIFY FACTOR "00000" IS ACCEPTED UNDER COBOL-COMPATIBLE BEHAVIOR.
        CardRecord card = new CardRecord("25", "1", "00000");
        boolean isFactorValid = card.isEqualizationFactorNumericAndNonNegativeOrPositive();
        assertTrue(isFactorValid);
    }

    /**
     * Verifies that factor "00000" is rejected under strict-positive mode, where
     * {@code parseInt("00000") == 0} and {@code 0 > 0} is false.
     */
    @Test
    void testFactorZeroStrictPositiveRejects()
    {
        // VERIFY FACTOR "00000" IS REJECTED UNDER STRICT-POSITIVE MODE.
        CardRecord card = new CardRecord("25", "1", "00000");
        final boolean MUST_BE_STRICT_POSITIVE = true;
        boolean isFactorValid = card.isEqualizationFactorNumericAndNonNegativeOrPositive(MUST_BE_STRICT_POSITIVE);
        assertFalse(isFactorValid);
    }

    /** Verifies that a factor with an alpha character at the start ("2A744") fails. */
    @Test
    void testFactorNonNumeric()
    {
        // VERIFY A FACTOR WITH AN ALPHABETIC CHARACTER IS REJECTED.
        CardRecord card = new CardRecord("25", "1", "2A744");
        boolean isFactorValid = card.isEqualizationFactorNumericAndNonNegativeOrPositive();
        assertFalse(isFactorValid);
    }

    /** Verifies that a factor with an alpha character in the middle ("29A44") fails. */
    @Test
    void testFactorInvalidMixed()
    {
        // VERIFY A FACTOR WITH AN EMBEDDED ALPHABETIC CHARACTER IS REJECTED.
        CardRecord card = new CardRecord("25", "1", "29A44");
        boolean isFactorValid = card.isEqualizationFactorNumericAndNonNegativeOrPositive();
        assertFalse(isFactorValid);
    }

    /** Verifies that a factor containing a space ("2 744") fails validation. */
    @Test
    void testFactorInvalidSpaces()
    {
        // VERIFY A FACTOR CONTAINING AN EMBEDDED SPACE IS REJECTED.
        CardRecord card = new CardRecord("25", "1", "2 744");
        boolean isFactorValid = card.isEqualizationFactorNumericAndNonNegativeOrPositive();
        assertFalse(isFactorValid);
    }

    // ========== FACTOR DISPLAY FORMATTING TESTS ==========

    /**
     * Verifies that the display format inserts a decimal point after the first digit,
     * producing "2.9744" from the stored value "29744" (COBOL PIC 9V9999).
     */
    @Test
    void testFactorForDisplay()
    {
        // VERIFY THE DISPLAY FORMAT CORRECTLY INSERTS THE DECIMAL POINT.
        // Factor "29744" represents 2.9744 (COBOL PIC 9V9999).
        CardRecord card = new CardRecord("25", "1", "29744");
        String actualDisplayFactor = card.getEqualizationFactorForDisplay();
        assertEquals("2.9744", actualDisplayFactor);
    }

    /**
     * Verifies display formatting for a unity factor (10000 = 1.0000),
     * meaning assessed values pass through unchanged.
     */
    @Test
    void testFactorDisplayUnity()
    {
        // VERIFY DISPLAY FORMAT FOR A UNITY FACTOR.
        // Factor "10000" represents 1.0000 — no equalization adjustment.
        CardRecord card = new CardRecord("25", "1", "10000");
        String actualDisplayFactor = card.getEqualizationFactorForDisplay();
        assertEquals("1.0000", actualDisplayFactor);
    }

    /**
     * Verifies display formatting for the smallest possible factor (00001 = 0.0001).
     */
    @Test
    void testFactorDisplayMinimum()
    {
        // VERIFY DISPLAY FORMAT FOR THE MINIMUM FACTOR.
        // Factor "00001" represents 0.0001 — smallest possible value.
        CardRecord card = new CardRecord("25", "1", "00001");
        String actualDisplayFactor = card.getEqualizationFactorForDisplay();
        assertEquals("0.0001", actualDisplayFactor);
    }

    /**
     * Verifies display formatting for the largest possible factor (99999 = 9.9999).
     */
    @Test
    void testFactorDisplayMaximum()
    {
        // VERIFY DISPLAY FORMAT FOR THE MAXIMUM FACTOR.
        // Factor "99999" represents 9.9999 — largest possible value.
        CardRecord card = new CardRecord("25", "1", "99999");
        String actualDisplayFactor = card.getEqualizationFactorForDisplay();
        assertEquals("9.9999", actualDisplayFactor);
    }

    /**
     * Verifies that the integer and decimal parts of the factor are correctly
     * separated (e.g., "29744" splits into integer "2" and decimal "9744").
     */
    @Test
    void testFactorIntegerAndDecimalParts()
    {
        // VERIFY THE FACTOR SPLITS INTO CORRECT INTEGER AND DECIMAL PARTS.
        // Factor "29744" should split into integer "2" and decimal "9744".
        CardRecord card = new CardRecord("25", "1", "29744");
        String actualIntegerPart = card.getEqualizationFactorIntegerPart();
        String actualDecimalPart = card.getEqualizationFactorDecimalPart();
        assertEquals("2", actualIntegerPart);
        assertEquals("9744", actualDecimalPart);
    }
}
