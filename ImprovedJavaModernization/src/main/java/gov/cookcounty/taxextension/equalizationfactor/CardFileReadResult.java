package gov.cookcounty.taxextension.equalizationfactor;

/**
 * Result of reading and processing one card from the input file.
 *
 * <p>Returned by the main processing loop to communicate both the read
 * outcome and the processing statistics for a single iteration, replacing
 * the COBOL pattern of setting shared flags (CARD-EOF, SEQ-CHECK) and
 * incrementing shared counters (IN-CNT, OUT-CNT, ERROR-CNT) as side effects.
 *
 * @see ClerkRealEstateBoardEqualizationFactorProgram
 */
public class CardFileReadResult
{
    /**
     * True when the card file has been exhausted (no more records to read).
     * When true, {@link #card} is null and {@link #isSequenceError} is false.
     */
    public final boolean isEndOfFile;
    /**
     * True when the current card arrived out of ascending year-quadrant sequence.
     * When true, {@link #card} is non-null (the card was parsed but should not be processed)
     * and {@link #isEndOfFile} is false.
     */
    public final boolean isSequenceError;
    /**
     * The parsed card record.
     * Non-null when a record was successfully read from the file (regardless of
     * whether a sequence error was detected). Null when {@link #isEndOfFile} is true.
     */
    public final CardRecord card;
    /**
     * Record counts for this single processing iteration.
     * Each count is 0 or 1. The caller accumulates these into running totals.
     */
    public final RecordProcessingStatistics statistics;

    /**
     * Creates a read result with the specified outcome and statistics.
     *
     * @param isEndOfFile    See {@link #isEndOfFile}.
     * @param isSequenceError See {@link #isSequenceError}.
     * @param card           See {@link #card}.
     * @param statistics     See {@link #statistics}.
     */
    public CardFileReadResult(boolean isEndOfFile, boolean isSequenceError, CardRecord card, RecordProcessingStatistics statistics)
    {
        this.isEndOfFile = isEndOfFile;
        this.isSequenceError = isSequenceError;
        this.card = card;
        this.statistics = statistics;
    }
}