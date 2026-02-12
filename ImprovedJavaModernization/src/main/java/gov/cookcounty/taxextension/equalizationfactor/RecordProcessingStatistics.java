package gov.cookcounty.taxextension.equalizationfactor;

/**
 * Counts of records processed.
 * Can be for a single iteration of the main loop or aggregated up
 * across an entire program run.
 */
public class RecordProcessingStatistics
{
    /**
     * Number of input records.
     * 1 for each card that was successfully read (not EOF, not sequence error).
     */
    public int inputCount;
    /**
     * Number of output records written.
     * 1 for each valid card was written to the factor file.
     */
    public int outputCount;
    /**
     * Number of error records.
     * 1 for each card that failed validation.
     */
    public int errorCount;

    /**
     * Creates a statistics snapshot.
     *
     * @param inputCount  See {@link #inputCount}.
     * @param outputCount See {@link #outputCount}.
     * @param errorCount  See {@link #errorCount}.
     */
    public RecordProcessingStatistics(int inputCount, int outputCount, int errorCount)
    {
        this.inputCount = inputCount;
        this.outputCount = outputCount;
        this.errorCount = errorCount;
    }
}