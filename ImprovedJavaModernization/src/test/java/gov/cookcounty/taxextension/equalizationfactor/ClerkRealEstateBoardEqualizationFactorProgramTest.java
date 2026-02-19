package gov.cookcounty.taxextension.equalizationfactor;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Program-level integration tests for {@link ClerkRealEstateBoardEqualizationFactorProgram}.
 *
 * <p>These tests run the full program (CLREB020) with controlled inputs and verify outputs.
 * They mirror and expand on the 50 test cases in the COBOL/Java test harnesses
 * at {@code Testing/CLREB020/}.
 *
 * <p>Test categories follow Code Complete 2nd Edition Ch. 22:
 * equivalence partitioning, boundary analysis, structured basis testing,
 * data-flow testing, error guessing, requirements-based, and design-based testing.
 */
class ClerkRealEstateBoardEqualizationFactorProgramTest
{
    @TempDir
    Path tempDir;

    private Path cardFile;
    private Path printFile;
    private Path factorFile;

    @BeforeEach
    void setUp()
    {
        cardFile = tempDir.resolve("cards.dat");
        printFile = tempDir.resolve("print.dat");
        factorFile = tempDir.resolve("factor.dat");
    }

    /** Creates an 80-char card line from year, quad, and factor. */
    private String makeCard(String year, String quad, String factor)
    {
        return String.format("%-2.2s%-1.1s%-5.5s%-72s", year, quad, factor, "");
    }

    /** Writes card records as binary fixed-width (no newlines), matching COBOL RECORDING MODE F. */
    private void writeCardFile(Path path, List<String> cards) throws IOException
    {
        try (var out = Files.newOutputStream(path))
        {
            for (String card : cards)
            {
                out.write(card.getBytes(StandardCharsets.ISO_8859_1));
            }
        }
    }

    /** Reads 21-byte factor records from binary file. */
    private List<String> readFactorRecords(Path path) throws IOException
    {
        byte[] data = Files.readAllBytes(path);
        List<String> records = new ArrayList<>();
        for (int i = 0; i + FactorRecord.RECORD_LENGTH_IN_CHARACTERS <= data.length;
             i += FactorRecord.RECORD_LENGTH_IN_CHARACTERS)
        {
            records.add(new String(
                data,
                i,
                FactorRecord.RECORD_LENGTH_IN_CHARACTERS,
                StandardCharsets.ISO_8859_1));
        }
        return records;
    }

    /** Runs the program and returns the return code. */
    private int runProgram() throws IOException
    {
        return new ClerkRealEstateBoardEqualizationFactorProgram(cardFile, printFile, factorFile).run();
    }

    // =====================================================================
    // Existing Core Tests (1-10)
    // =====================================================================

    @Test
    void testValidBasicInput() throws IOException
    {
        writeCardFile(cardFile, List.of(
                makeCard("25", "1", "29744"),
                makeCard("25", "2", "29744"),
                makeCard("25", "3", "29744"),
                makeCard("25", "4", "29744")));

        assertEquals(0, runProgram());

        List<String> records = readFactorRecords(factorFile);
        assertEquals(4, records.size());
        assertEquals("25129744             ", records.get(0));
        assertEquals("25229744             ", records.get(1));
        assertEquals("25329744             ", records.get(2));
        assertEquals("25429744             ", records.get(3));
        assertTrue(Files.size(printFile) > 0);
    }

    @Test
    void testMultiplePages() throws IOException
    {
        var cards = new ArrayList<String>();
        for (int yr = 10; yr <= 25; yr++)
        {
            for (int q = 1; q <= 4; q++)
            {
                cards.add(makeCard(String.format("%02d", yr), String.valueOf(q), "29744"));
            }
        }
        writeCardFile(cardFile, cards); // 64 cards

        assertEquals(0, runProgram());
        assertEquals(64, readFactorRecords(factorFile).size());

        String printContent = Files.readString(printFile);
        long pageHeaders = printContent.lines()
                .filter(line -> line.contains("CLREB020"))
                .count();
        assertTrue(pageHeaders > 1, "Should have multiple page headers for 64 cards");
    }

    @Test
    void testInvalidNonNumericYear() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("AB", "1", "29744")));

        assertEquals(0, runProgram());
        assertEquals(0, Files.size(factorFile));

        String printContent = Files.readString(printFile);
        assertTrue(printContent.contains("NOT NUMERIC"));
    }

    @Test
    void testInvalidBadQuad() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", "5", "29744")));

        assertEquals(0, runProgram());
        assertEquals(0, Files.size(factorFile));
        assertTrue(Files.readString(printFile).contains("NOT NUMERIC"));
    }

    @Test
    void testZeroFactorAcceptedMatchingCobol() throws IOException
    {
        // Default (COBOL-compatible) behavior: factor "00000" is accepted because the
        // original COBOL GREATER THAN 0 comparison is alphanumeric, not numeric.
        writeCardFile(cardFile, List.of(makeCard("25", "1", "00000")));

        assertEquals(0, runProgram());
        List<String> records = readFactorRecords(factorFile);
        assertEquals(1, records.size());
        assertEquals("25100000             ", records.get(0));
    }

    @Test
    void testSequenceError() throws IOException
    {
        writeCardFile(cardFile, List.of(
                makeCard("25", "2", "29744"),
                makeCard("25", "1", "29744")));

        assertEquals(16, runProgram(), "Sequence error should set RC=16");
    }

    @Test
    void testEmptyInput() throws IOException
    {
        Files.writeString(cardFile, "");

        assertEquals(0, runProgram());
        assertEquals(0, Files.size(factorFile));
    }

    @Test
    void testFactorRecordExactFormat() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("24", "3", "31500")));

        runProgram();

        List<String> records = readFactorRecords(factorFile);
        assertEquals(1, records.size());
        String rec = records.get(0);
        assertEquals(21, rec.length());
        assertEquals("24", rec.substring(0, 2), "FT-TAXYR");
        assertEquals("3", rec.substring(2, 3), "FT-QUAD");
        assertEquals("31500", rec.substring(3, 8), "FT-EQFACT");
        assertEquals("             ", rec.substring(8, 21), "FILLER");
    }

    @Test
    void testPrintLineFormat() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", "1", "29744")));

        runProgram();

        String printContent = Files.readString(printFile);
        assertTrue(printContent.contains("CLREB020"));
        assertTrue(printContent.contains("OFFICE  OF  THE COUNTY  CLERK"));
        assertTrue(printContent.contains("EQUALIZATION      FACTORS"));
        assertTrue(printContent.contains("YEAR      QUAD     FACTOR"));
    }

    @Test
    void testDateInHeader() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", "1", "29744")));

        runProgram();

        String today = java.time.LocalDate.now()
                .format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
        assertTrue(Files.readString(printFile).contains(today));
    }

    // =====================================================================
    // Boundary Tests (Year, Quad, Factor boundaries)
    // =====================================================================

    @Test
    void testBoundaryYear01() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("01", "1", "29744")));
        assertEquals(0, runProgram());
        assertEquals(1, readFactorRecords(factorFile).size());
    }

    @Test
    void testBoundaryYear99() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("99", "1", "29744")));
        assertEquals(0, runProgram());
        assertEquals(1, readFactorRecords(factorFile).size());
    }

    @Test
    void testBoundaryYear00AcceptedMatchingCobol() throws IOException
    {
        // Default (COBOL-compatible) behavior: year "00" is accepted because the
        // original COBOL GREATER THAN 0 comparison is alphanumeric, not numeric.
        writeCardFile(cardFile, List.of(makeCard("00", "1", "29744")));
        assertEquals(0, runProgram());
        List<String> records = readFactorRecords(factorFile);
        assertEquals(1, records.size());
        assertEquals("00129744             ", records.get(0));
    }

    @Test
    void testBoundaryFactor00001() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", "1", "00001")));
        assertEquals(0, runProgram());
        List<String> records = readFactorRecords(factorFile);
        assertEquals(1, records.size());
        assertEquals("25100001             ", records.get(0));
    }

    @Test
    void testBoundaryFactor99999() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", "1", "99999")));
        assertEquals(0, runProgram());
        List<String> records = readFactorRecords(factorFile);
        assertEquals(1, records.size());
        assertEquals("25199999             ", records.get(0));
    }

    @Test
    void testBoundaryFactor10000Unity() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", "1", "10000")));
        assertEquals(0, runProgram());
        List<String> records = readFactorRecords(factorFile);
        assertEquals(1, records.size());
        assertEquals("25110000             ", records.get(0));
    }

    // =====================================================================
    // Validation Combinatorial Tests
    // =====================================================================

    @Test
    void testInvalidYearMixed() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("2A", "1", "29744")));
        assertEquals(0, runProgram());
        assertEquals(0, Files.size(factorFile));
        assertTrue(Files.readString(printFile).contains("NOT NUMERIC"));
    }

    @Test
    void testInvalidYearSpaces() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("  ", "1", "29744")));
        assertEquals(0, runProgram());
        assertEquals(0, Files.size(factorFile));
    }

    @Test
    void testInvalidFactorMixed() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", "1", "29A44")));
        assertEquals(0, runProgram());
        assertEquals(0, Files.size(factorFile));
        assertTrue(Files.readString(printFile).contains("NOT NUMERIC"));
    }

    @Test
    void testInvalidFactorSpaces() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", "1", "2 744")));
        assertEquals(0, runProgram());
        assertEquals(0, Files.size(factorFile));
    }

    @Test
    void testInvalidQuad9() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", "9", "29744")));
        assertEquals(0, runProgram());
        assertEquals(0, Files.size(factorFile));
    }

    @Test
    void testInvalidQuadSpace() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", " ", "29744")));
        assertEquals(0, runProgram());
        assertEquals(0, Files.size(factorFile));
    }

    @Test
    void testInvalidQuadLetter() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", "A", "29744")));
        assertEquals(0, runProgram());
        assertEquals(0, Files.size(factorFile));
    }

    @Test
    void testInvalidAllFieldsBad() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("AB", "X", "ZZZZZ")));
        assertEquals(0, runProgram());
        assertEquals(0, Files.size(factorFile));
        assertTrue(Files.readString(printFile).contains("NOT NUMERIC"));
    }

    // =====================================================================
    // Sequence Tests
    // =====================================================================

    @Test
    void testSequenceAscendingAllQuads() throws IOException
    {
        var cards = new ArrayList<String>();
        for (int yr = 24; yr <= 25; yr++)
        {
            for (int q = 1; q <= 4; q++)
            {
                cards.add(makeCard(String.format("%02d", yr), String.valueOf(q), "29744"));
            }
        }
        writeCardFile(cardFile, cards); // 8 cards

        assertEquals(0, runProgram());
        assertEquals(8, readFactorRecords(factorFile).size());
    }

    @Test
    void testSequenceYearTransition() throws IOException
    {
        writeCardFile(cardFile, List.of(
                makeCard("24", "3", "29744"),
                makeCard("24", "4", "29744"),
                makeCard("25", "1", "29744"),
                makeCard("25", "2", "29744"),
                makeCard("25", "3", "29744")));

        assertEquals(0, runProgram());
        assertEquals(5, readFactorRecords(factorFile).size());
    }

    @Test
    void testSequenceDuplicateKeyAllowed() throws IOException
    {
        // COBOL uses LESS THAN (not LESS THAN OR EQUAL), so duplicates are allowed
        writeCardFile(cardFile, List.of(
                makeCard("25", "1", "29744"),
                makeCard("25", "1", "30000"),
                makeCard("25", "1", "31000")));

        assertEquals(0, runProgram());
        assertEquals(3, readFactorRecords(factorFile).size());
    }

    @Test
    void testSequenceErrorAfterValid() throws IOException
    {
        writeCardFile(cardFile, List.of(
                makeCard("25", "1", "29744"),
                makeCard("25", "2", "29744"),
                makeCard("24", "1", "29744"))); // Out of order

        int rc = runProgram();
        assertEquals(16, rc);
        // First 2 records written before error
        assertEquals(2, readFactorRecords(factorFile).size());
    }

    @Test
    void testSequenceErrorImmediate() throws IOException
    {
        writeCardFile(cardFile, List.of(
                makeCard("25", "2", "29744"),
                makeCard("25", "1", "29744"))); // Immediately out of order

        assertEquals(16, runProgram());
        // Only first record written
        assertEquals(1, readFactorRecords(factorFile).size());
    }

    // =====================================================================
    // Data-Flow and State Tests
    // =====================================================================

    @Test
    void testMixedValidInvalidStream() throws IOException
    {
        // V, I, V, I, V, V — tests that errors don't corrupt state
        writeCardFile(cardFile, List.of(
                makeCard("25", "1", "29744"),   // valid
                makeCard("25", "2", "2A744"),   // invalid factor
                makeCard("25", "3", "29744"),   // valid
                makeCard("25", "4", "XXXXX"),   // invalid factor
                makeCard("26", "1", "29744"),   // valid
                makeCard("26", "2", "29744"))); // valid

        assertEquals(0, runProgram());
        assertEquals(4, readFactorRecords(factorFile).size());
    }

    @Test
    void testErrorMessageCleanup() throws IOException
    {
        // Valid -> Invalid -> Valid: error message must be cleared between cards
        writeCardFile(cardFile, List.of(
                makeCard("25", "1", "29744"),   // valid
                makeCard("25", "2", "ABCDE"),   // invalid
                makeCard("25", "3", "29744"))); // valid

        assertEquals(0, runProgram());
        assertEquals(2, readFactorRecords(factorFile).size());

        String printContent = Files.readString(printFile);
        long errorCount = printContent.lines()
                .filter(line -> line.contains("NOT NUMERIC"))
                .count();
        assertEquals(1, errorCount, "Only the invalid card should show NOT NUMERIC");
    }

    @Test
    void testCounterInvariant() throws IOException
    {
        // 10 cards: 7 valid + 3 invalid. in_count = out_count + error_count
        var cards = new ArrayList<String>();
        for (int q = 1; q <= 4; q++)
        {
            cards.add(makeCard("25", String.valueOf(q), "29744")); // 4 valid
        }
        cards.add(makeCard("26", "1", "ABCDE")); // invalid
        cards.add(makeCard("26", "2", "29744"));  // valid
        cards.add(makeCard("26", "3", "XXXXX")); // invalid
        cards.add(makeCard("26", "4", "29744"));  // valid
        cards.add(makeCard("27", "1", "29744"));  // valid
        cards.add(makeCard("27", "2", "ZZZZZ")); // invalid
        writeCardFile(cardFile, cards);

        assertEquals(0, runProgram());
        assertEquals(7, readFactorRecords(factorFile).size());
    }

    // =====================================================================
    // Page Break Tests
    // =====================================================================

    @Test
    void testPageBreakAt24Cards() throws IOException
    {
        // 24 cards fits on 1 page: LINE-CNT starts at 8 after first heading,
        // each detail adds 2, so after 24 cards LC = 8 + 48 = 56.
        // The 25th card would see LC=56 > 55 and trigger page 2.
        var cards = new ArrayList<String>();
        for (int i = 0; i < 24; i++)
        {
            int yr = 10 + (i / 4);
            int q = (i % 4) + 1;
            cards.add(makeCard(String.format("%02d", yr), String.valueOf(q), "29744"));
        }
        writeCardFile(cardFile, cards);

        assertEquals(0, runProgram());
        assertEquals(24, readFactorRecords(factorFile).size());

        String printContent = Files.readString(printFile);
        long pageHeaders = printContent.lines()
                .filter(line -> line.contains("CLREB020"))
                .count();
        assertEquals(1, pageHeaders, "24 cards fits on exactly 1 page");
    }

    @Test
    void testPageBreakThreePages() throws IOException
    {
        var cards = new ArrayList<String>();
        for (int i = 0; i < 72; i++)
        {
            int yr = 10 + (i / 4);
            int q = (i % 4) + 1;
            cards.add(makeCard(String.format("%02d", yr), String.valueOf(q), "29744"));
        }
        writeCardFile(cardFile, cards);

        assertEquals(0, runProgram());
        assertEquals(72, readFactorRecords(factorFile).size());

        String printContent = Files.readString(printFile);
        long pageHeaders = printContent.lines()
                .filter(line -> line.contains("CLREB020"))
                .count();
        assertTrue(pageHeaders >= 3, "72 cards should produce 3+ pages");
    }

    // =====================================================================
    // Business Scenario Tests
    // =====================================================================

    @Test
    void testBusinessAnnualRun() throws IOException
    {
        writeCardFile(cardFile, List.of(
                makeCard("25", "1", "29744"),
                makeCard("25", "2", "31205"),
                makeCard("25", "3", "25000"),
                makeCard("25", "4", "28133")));

        assertEquals(0, runProgram());

        List<String> records = readFactorRecords(factorFile);
        assertEquals(4, records.size());
        assertEquals("25129744             ", records.get(0));
        assertEquals("25231205             ", records.get(1));
        assertEquals("25325000             ", records.get(2));
        assertEquals("25428133             ", records.get(3));
    }

    @Test
    void testBusinessMultiYear() throws IOException
    {
        var cards = new ArrayList<String>();
        for (int yr = 23; yr <= 25; yr++)
        {
            for (int q = 1; q <= 4; q++)
            {
                cards.add(makeCard(String.format("%02d", yr), String.valueOf(q), "29744"));
            }
        }
        writeCardFile(cardFile, cards); // 12 cards

        assertEquals(0, runProgram());
        assertEquals(12, readFactorRecords(factorFile).size());
    }

    @Test
    void testBusinessDataEntryError() throws IOException
    {
        writeCardFile(cardFile, List.of(
                makeCard("25", "1", "29744"),
                makeCard("25", "2", "31205"),
                makeCard("25", "3", "2X000"),  // typo
                makeCard("25", "4", "28133"),
                makeCard("26", "1", "29744")));

        assertEquals(0, runProgram());
        assertEquals(4, readFactorRecords(factorFile).size());
        assertTrue(Files.readString(printFile).contains("NOT NUMERIC"));
    }

    @Test
    void testBusinessFactorUnity() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", "1", "10000")));

        assertEquals(0, runProgram());
        List<String> records = readFactorRecords(factorFile);
        assertEquals(1, records.size());
        assertEquals("25110000             ", records.get(0));
    }

    // =====================================================================
    // Output Format Verification Tests
    // =====================================================================

    @Test
    void testFactorDisplayFormatting() throws IOException
    {
        writeCardFile(cardFile, List.of(
                makeCard("25", "1", "29744"),
                makeCard("25", "2", "10000"),
                makeCard("25", "3", "00001"),
                makeCard("25", "4", "99999"),
                makeCard("26", "1", "45000")));

        assertEquals(0, runProgram());

        String printContent = Files.readString(printFile);
        assertTrue(printContent.contains("2.9744"));
        assertTrue(printContent.contains("1.0000"));
        assertTrue(printContent.contains("0.0001"));
        assertTrue(printContent.contains("9.9999"));
        assertTrue(printContent.contains("4.5000"));
    }

    @Test
    void testPrintErrorLineLayout() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", "1", "ABCDE")));

        assertEquals(0, runProgram());
        assertTrue(Files.readString(printFile).contains("NOT NUMERIC"));
    }

    @Test
    void testFactorRecordBinaryFormat() throws IOException
    {
        writeCardFile(cardFile, List.of(
                makeCard("25", "1", "29744"),
                makeCard("25", "2", "29744")));

        runProgram();

        byte[] data = Files.readAllBytes(factorFile);
        assertEquals(42, data.length, "2 records * 21 bytes = 42 bytes");
        for (byte b : data)
        {
            assertNotEquals((byte) '\n', b, "Factor file should not contain newlines");
            assertNotEquals((byte) '\r', b, "Factor file should not contain carriage returns");
        }
    }

    // =====================================================================
    // Single-Card Edge Cases
    // =====================================================================

    @Test
    void testSingleValidCard() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("25", "1", "29744")));

        assertEquals(0, runProgram());
        assertEquals(1, readFactorRecords(factorFile).size());
    }

    @Test
    void testSingleInvalidCard() throws IOException
    {
        writeCardFile(cardFile, List.of(makeCard("XX", "1", "29744")));

        assertEquals(0, runProgram());
        assertEquals(0, Files.size(factorFile));
        assertTrue(Files.readString(printFile).contains("NOT NUMERIC"));
    }
}
