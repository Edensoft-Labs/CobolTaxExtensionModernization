package gov.cookcounty.taxextension.equalizationfactor;

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
public class CardRecord
{
    /** Record length as defined by COBOL FD RECORD CONTAINS 80 CHARACTERS. */
    public static final int RECORD_LENGTH_IN_CHARACTERS = 80;

    /**
     * Tax year from the input card.
     * Stored as String (not int) because the COBOL PIC XX type is alphanumeric —
     * the field may contain non-digit characters in invalid records, and leading
     * zeros are significant (e.g., "05" must not become 5). Validation is performed
     * separately by {@link #isYearNumericAndNonNegativeOrPositive()}.
     */
    public final String taxYear;
    /**
     * Assessment quadrant code (1-4).
     * Stored as String (not char or int) because the COBOL PIC X type is
     * alphanumeric — invalid records may contain any character (spaces, letters),
     * and validation is performed separately by {@link #isValidAssessmentQuadrant()}.
     */
    public final String assessmentQuadrant;
    /**
     * Equalization factor as raw characters.
     * In COBOL, this field has two views via REDEFINES: an alphanumeric view
     * (CD-FACTOR PIC X(5)) used for the NUMERIC validity test, and a display-numeric
     * view (CD-FACTOR-RD PIC 9V9999) with an implied decimal after the first digit.
     * Stored as String (not BigDecimal) because:
     * <ul>
     *   <li>The program validates this field before treating it as numeric — invalid
     *       records may contain non-digit characters that BigDecimal cannot represent.</li>
     *   <li>The implied decimal position is handled by {@link #getEqualizationFactorIntegerPart()}
     *       and {@link #getEqualizationFactorDecimalPart()} rather than by a numeric type.</li>
     *   <li>The raw character representation is written directly to the output file.</li>
     * </ul>
     */
    public final String equalizationFactor;

    /**
     * Parses a card record from an 80-character fixed-width line.
     *
     * @param line The 80-character input line.
     * @return Parsed card record.
     */
    public static CardRecord parse(String line)
    {
        // PAD THE LINE IF IT'S TOO SHORT TO CONTAIN ALL REQUIRED FIELDS.
        // The minimum parseable length covers year (2) + quadrant (1) + factor (5) = 8 characters.
        final int YEAR_CHARACTER_COUNT = 2;
        final int QUADRANT_CHARACTER_COUNT = 1;
        final int FACTOR_CHARACTER_COUNT = 5;
        final int MINIMUM_PARSEABLE_LENGTH_IN_CHARACTERS = YEAR_CHARACTER_COUNT + QUADRANT_CHARACTER_COUNT + FACTOR_CHARACTER_COUNT;
        int lineLengthInCharacters = line.length();
        boolean lineIsTooShort = (lineLengthInCharacters < MINIMUM_PARSEABLE_LENGTH_IN_CHARACTERS);
        String paddedLine;
        if (lineIsTooShort)
        {
            // PAD THE LINE TO AN APPROPRIATE LENGTH.
            // String.format() with "%-Ns" left-justifies the string within an N-character
            // field, filling any remaining positions on the right with spaces. The '-' flag
            // means left-align, and 's' is the string conversion type.
            String leftJustifiedPaddingFormat = "%-" + RECORD_LENGTH_IN_CHARACTERS + "s";
            paddedLine = String.format(leftJustifiedPaddingFormat, line);
        }
        else
        {
            // USE THE LINE AS-IS.
            paddedLine = line;
        }

        // EXTRACT THE INDIVIDUAL FIELDS FROM THEIR FIXED POSITIONS.
        // CD-YR: positions 1-2.
        final int YEAR_START_POSITION = 0;
        final int YEAR_END_POSITION = 2;
        String taxYear = paddedLine.substring(YEAR_START_POSITION, YEAR_END_POSITION);

        // CD-QUAD: position 3.
        final int QUADRANT_START_POSITION = 2;
        final int QUADRANT_END_POSITION = 3;
        String assessmentQuadrant = paddedLine.substring(QUADRANT_START_POSITION, QUADRANT_END_POSITION);
        
        // CD-FACTOR: positions 4-8.
        final int FACTOR_START_POSITION = 3;
        final int FACTOR_END_POSITION = 8;
        String equalizationFactor = paddedLine.substring(FACTOR_START_POSITION, FACTOR_END_POSITION);

        // RETURN THE PARSED CARD RECORD.
        CardRecord parsedRecord = new CardRecord(taxYear, assessmentQuadrant, equalizationFactor);
        return parsedRecord;
    }

    /**
     * Creates a card record from individual field values.
     *
     * @param taxYear             See {@link #taxYear}.
     * @param assessmentQuadrant  See {@link #assessmentQuadrant}.
     * @param equalizationFactor See {@link #equalizationFactor}.
     */
    public CardRecord(String taxYear, String assessmentQuadrant, String equalizationFactor)
    {
        this.taxYear = taxYear;
        this.assessmentQuadrant = assessmentQuadrant;
        this.equalizationFactor = equalizationFactor;
    }

    /**
     * Returns the year-quadrant sequence key used for ascending-order checking.
     *
     * <p>COBOL (line 149): {@code IF CARD LESS THAN PREV-CARD} — the CARD group item
     * is defined as CD-YR (2 chars) + CD-QUAD (1 char), so the comparison orders
     * records first by tax year, then by assessment quadrant within the same year.
     * The comparison is alphanumeric (character-by-character, left to right).
     *
     * @return 3-character key (tax year + assessment quadrant) for sequence checking.
     */
    public String getYearQuadrantSequenceKey()
    {
        // BUILD THE YEAR-QUADRANT SEQUENCE KEY FOR ASCENDING-ORDER CHECKING.
        String yearQuadrantSequenceKey = taxYear + assessmentQuadrant;
        return yearQuadrantSequenceKey;
    }

    /**
     * Tests if the year field is numeric and non-negative (matching original COBOL behavior).
     *
     * <p>This is an overloaded convenience method that delegates to
     * {@link #isYearNumericAndNonNegativeOrPositive(boolean)} with
     * {@code mustBeStrictPositive = false}, preserving the original COBOL behavior
     * where year "00" is accepted.
     *
     * @return True if year is a 2-digit number greater than or equal to 0.
     * @see #isYearNumericAndNonNegativeOrPositive(boolean)
     */
    public boolean isYearNumericAndNonNegativeOrPositive()
    {
        // DELEGATE TO THE PARAMETERIZED VERSION WITH COBOL-COMPATIBLE BEHAVIOR.
        final boolean USE_COBOL_COMPATIBLE_BEHAVIOR = false;
        boolean isYearNumericAndNonNegative = isYearNumericAndNonNegativeOrPositive(USE_COBOL_COMPATIBLE_BEHAVIOR);
        return isYearNumericAndNonNegative;
    }

    /**
     * Tests if the year field is numeric and either non-negative or strictly positive,
     * depending on the {@code mustBeStrictPositive} parameter.
     *
     * <p><b>COBOL behavior note:</b> The original COBOL code (CLREB020 line 783) tests
     * {@code CD-YR NUMERIC AND CD-YR GREATER THAN 0}. Because CD-YR is a PIC X(2)
     * alphanumeric field, the {@code GREATER THAN 0} comparison is an alphanumeric
     * string comparison, not a numeric one. COBOL converts the literal {@code 0} to
     * {@code "0 "} (padded with spaces to the field width), so {@code "00" > "0 "}
     * is TRUE because {@code '0'} (0x30) &gt; {@code ' '} (0x20) in the second position.
     * This means the COBOL code effectively accepts any all-digit year including "00".
     *
     * <p>The default behavior ({@code mustBeStrictPositive = false}) preserves this
     * original COBOL behavior by using a {@code >= 0} comparison, which accepts "00".
     * Setting {@code mustBeStrictPositive = true} uses a {@code > 0} comparison,
     * which rejects "00" — a theoretically more correct validation that could be
     * adopted once the business decides year "00" should be rejected.
     *
     * @param mustBeStrictPositive When {@code false}, uses {@code >= 0} (COBOL-compatible,
     *                             accepts "00"). When {@code true}, uses {@code > 0}
     *                             (rejects "00").
     * @return True if year is a 2-digit number meeting the positivity requirement.
     */
    public boolean isYearNumericAndNonNegativeOrPositive(boolean mustBeStrictPositive)
    {
        // CHECK IF THE YEAR IS A VALID NUMERIC VALUE.
        final String TWO_DIGIT_NUMERIC_PATTERN = "\\d{2}";
        boolean isNumeric = taxYear.matches(TWO_DIGIT_NUMERIC_PATTERN);
        if (!isNumeric)
        {
            return false;
        }

        // CHECK IF THE YEAR MEETS THE POSITIVITY REQUIREMENT.
        int yearValue = Integer.parseInt(taxYear);
        if (mustBeStrictPositive)
        {
            // STRICT POSITIVE: REJECT YEAR "00".
            boolean isStrictlyPositive = (yearValue > 0);
            return isStrictlyPositive;
        }
        else
        {
            // COBOL-COMPATIBLE: ACCEPT YEAR "00".
            boolean isNonNegative = (yearValue >= 0);
            return isNonNegative;
        }
    }

    /**
     * Tests if the assessment quadrant is valid (1 through 4).
     *
     * @return True if assessment quadrant is '1', '2', '3', or '4'; false if not.
     */
    public boolean isValidAssessmentQuadrant()
    {
        // CHECK IF THE ASSESSMENT QUADRANT IS A SINGLE DIGIT.
        final int EXPECTED_QUADRANT_LENGTH_IN_CHARACTERS = 1;
        boolean isSingleCharacter = (assessmentQuadrant.length() == EXPECTED_QUADRANT_LENGTH_IN_CHARACTERS);
        if (!isSingleCharacter)
        {
            return false;
        }

        // CHECK IF THE ASSSESSMENT QUADRANT IS WITHIN THE VALID RANGE.
        final char MINIMUM_VALID_QUADRANT = '1';
        final char MAXIMUM_VALID_QUADRANT = '4';
        final int FIRST_CHARACTER_INDEX = 0;
        char quadrantCharacter = assessmentQuadrant.charAt(FIRST_CHARACTER_INDEX);
        boolean isWithinValidRange = (
            (MINIMUM_VALID_QUADRANT <= quadrantCharacter) &&
            (quadrantCharacter <= MAXIMUM_VALID_QUADRANT));
        return isWithinValidRange;
    }

    /**
     * Tests if the equalization factor field is numeric and non-negative (matching original COBOL behavior).
     *
     * <p>This is an overloaded convenience method that delegates to
     * {@link #isEqualizationFactorNumericAndNonNegativeOrPositive(boolean)} with
     * {@code mustBeStrictPositive = false}, preserving the original COBOL behavior
     * where factor "00000" is accepted.
     *
     * @return True if equalization factor is a 5-digit number greater than or equal to 0.
     * @see #isEqualizationFactorNumericAndNonNegativeOrPositive(boolean)
     */
    public boolean isEqualizationFactorNumericAndNonNegativeOrPositive()
    {
        // DELEGATE TO THE PARAMETERIZED VERSION WITH COBOL-COMPATIBLE BEHAVIOR.
        final boolean USE_COBOL_COMPATIBLE_BEHAVIOR = false;
        boolean isEqualizationFactorNumericAndNonNegative = isEqualizationFactorNumericAndNonNegativeOrPositive(USE_COBOL_COMPATIBLE_BEHAVIOR);
        return isEqualizationFactorNumericAndNonNegative;
    }

    /**
     * Tests if the equalization factor field is numeric and either non-negative or strictly positive,
     * depending on the {@code mustBeStrictPositive} parameter.
     *
     * <p><b>COBOL behavior note:</b> The original COBOL code (CLREB020 line 783) tests
     * {@code CD-FACTOR NUMERIC AND CD-FACTOR GREATER THAN 0}. Because CD-FACTOR is a
     * PIC X(5) alphanumeric field, the {@code GREATER THAN 0} comparison is an alphanumeric
     * string comparison, not a numeric one. COBOL converts the literal {@code 0} to
     * {@code "0    "} (padded with spaces to the field width), so {@code "00000" > "0    "}
     * is TRUE because {@code '0'} (0x30) &gt; {@code ' '} (0x20) in the second position.
     * This means the COBOL code effectively accepts any all-digit factor including "00000".
     *
     * <p>The default behavior ({@code mustBeStrictPositive = false}) preserves this
     * original COBOL behavior by using a {@code >= 0} comparison, which accepts "00000".
     * Setting {@code mustBeStrictPositive = true} uses a {@code > 0} comparison,
     * which rejects "00000" — a theoretically more correct validation that could be
     * adopted once the business decides factor "00000" should be rejected.
     *
     * @param mustBeStrictPositive When {@code false}, uses {@code >= 0} (COBOL-compatible,
     *                             accepts "00000"). When {@code true}, uses {@code > 0}
     *                             (rejects "00000").
     * @return True if equalization factor is a 5-digit number meeting the positivity requirement.
     */
    public boolean isEqualizationFactorNumericAndNonNegativeOrPositive(boolean mustBeStrictPositive)
    {
        // CHECK IF THE EQUALIZATION FACTOR IS A VALID NUMERIC VALUE.
        final String FIVE_DIGIT_NUMERIC_PATTERN = "\\d{5}";
        boolean isNumeric = equalizationFactor.matches(FIVE_DIGIT_NUMERIC_PATTERN);
        if (!isNumeric)
        {
            return false;
        }

        // CHECK IF THE EQUALIZATION FACTOR MEETS THE POSITIVITY REQUIREMENT.
        int equalizationFactorValue = Integer.parseInt(equalizationFactor);
        if (mustBeStrictPositive)
        {
            // STRICT POSITIVE: REJECT FACTOR "00000".
            boolean isStrictlyPositive = (equalizationFactorValue > 0);
            return isStrictlyPositive;
        }
        else
        {
            // COBOL-COMPATIBLE: ACCEPT FACTOR "00000".
            boolean isNonNegative = (equalizationFactorValue >= 0);
            return isNonNegative;
        }
    }

    /**
     * Returns the first digit of the equalization factor (integer part before implied decimal).
     *
     * @return Single character (e.g., '2' from "29744") for the integer part of the equalization factor.
     */
    public String getEqualizationFactorIntegerPart()
    {
        // EXTRACT THE INTEGER PART (FIRST DIGIT) OF THE EQUALIZATION FACTOR.
        final int INTEGER_PART_START_POSITION = 0;
        final int INTEGER_PART_END_POSITION = 1;
        String equalizationFactorIntegerPart = equalizationFactor.substring(INTEGER_PART_START_POSITION, INTEGER_PART_END_POSITION);
        return equalizationFactorIntegerPart;
    }

    /**
     * Returns the last 4 digits of the equalization factor (fractional part after implied decimal).
     *
     * @return 4-character string (e.g., "9744" from "29744") for the decimal part of the equalization factor.
     */
    public String getEqualizationFactorDecimalPart()
    {
        // EXTRACT THE DECIMAL PART (LAST 4 DIGITS) OF THE EQUALIZATION FACTOR.
        final int DECIMAL_PART_START_POSITION = 1;
        final int DECIMAL_PART_END_POSITION = 5;
        String equalizationFactorDecimalPart = equalizationFactor.substring(DECIMAL_PART_START_POSITION, DECIMAL_PART_END_POSITION);
        return equalizationFactorDecimalPart;
    }

    /**
     * Returns the equalization factor formatted for display with a decimal point.
     *
     * @return Equalization factor with decimal point inserted (e.g., "2.9744").
     */
    public String getEqualizationFactorForDisplay()
    {
        // FORMAT THE EQUALIZATION FACTOR FOR PROPER DISPLAY.
        String equalizationFactorIntegerPart = getEqualizationFactorIntegerPart();
        String equalizationFactorDecimalPart = getEqualizationFactorDecimalPart();
        String equalizationFactorForDisplay = equalizationFactorIntegerPart + "." + equalizationFactorDecimalPart;
        return equalizationFactorForDisplay;
    }
}