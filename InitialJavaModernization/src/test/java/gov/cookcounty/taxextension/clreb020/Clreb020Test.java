package gov.cookcounty.taxextension.clreb020;

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
 * Integration tests for {@link Clreb020} — runs the full program with test inputs.
 *
 * <p>These tests mirror the 7 test cases in the COBOL test harness at
 * {@code Testing/CLREB020/cobol_test_harness.py}.
 */
class Clreb020Test {

    @TempDir
    Path tempDir;

    private Path cardFile;
    private Path printFile;
    private Path factorFile;

    @BeforeEach
    void setUp() {
        cardFile = tempDir.resolve("cards.dat");
        printFile = tempDir.resolve("print.dat");
        factorFile = tempDir.resolve("factor.dat");
    }

    /**
     * Helper: creates an 80-char card line from year, quad, and factor.
     */
    private String makeCard(String year, String quad, String factor) {
        return String.format("%-2.2s%-1.1s%-5.5s%-72s", year, quad, factor, "");
    }

    /**
     * Helper: writes card records as a binary fixed-width file (no newlines),
     * matching the COBOL RECORDING MODE F format.
     */
    private void writeCardFile(Path path, List<String> cards) throws IOException {
        try (var out = Files.newOutputStream(path)) {
            for (String card : cards) {
                out.write(card.getBytes(StandardCharsets.ISO_8859_1));
            }
        }
    }

    /**
     * Helper: reads factor records from a binary file (21 bytes per record,
     * no newlines). Returns a list of 21-character strings.
     */
    private List<String> readFactorRecords(Path path) throws IOException {
        byte[] data = Files.readAllBytes(path);
        List<String> records = new ArrayList<>();
        for (int i = 0; i + FactorRecord.RECORD_LENGTH <= data.length;
             i += FactorRecord.RECORD_LENGTH) {
            records.add(new String(data, i, FactorRecord.RECORD_LENGTH,
                    StandardCharsets.ISO_8859_1));
        }
        return records;
    }

    // --- Test 1: Valid basic input (4 cards, quads 1-4) ---

    @Test
    void testValidBasicInput() throws IOException {
        List<String> cards = List.of(
                makeCard("25", "1", "29744"),
                makeCard("25", "2", "29744"),
                makeCard("25", "3", "29744"),
                makeCard("25", "4", "29744")
        );
        writeCardFile(cardFile, cards);

        Clreb020 program = new Clreb020(cardFile, printFile, factorFile);
        int rc = program.run();

        assertEquals(0, rc, "Return code should be 0 for valid input");

        // Verify factor file has 4 records (binary: 4 * 21 = 84 bytes)
        List<String> factorRecords = readFactorRecords(factorFile);
        assertEquals(4, factorRecords.size(), "Should produce 4 factor records");

        // Verify each factor record is 21 characters
        for (String rec : factorRecords) {
            assertEquals(21, rec.length(), "Each factor record should be 21 chars");
        }

        // Verify factor record content
        assertEquals("25129744             ", factorRecords.get(0));
        assertEquals("25229744             ", factorRecords.get(1));
        assertEquals("25329744             ", factorRecords.get(2));
        assertEquals("25429744             ", factorRecords.get(3));

        // Verify print file was created with content
        assertTrue(Files.size(printFile) > 0, "Print file should have content");
    }

    // --- Test 2: Multiple pages (60+ cards trigger page break at line > 55) ---

    @Test
    void testMultiplePages() throws IOException {
        var cards = new ArrayList<String>();
        for (int yr = 10; yr <= 25; yr++) {
            for (int q = 1; q <= 4; q++) {
                cards.add(makeCard(String.format("%02d", yr), String.valueOf(q), "29744"));
            }
        }
        // 16 years * 4 quads = 64 cards
        writeCardFile(cardFile, cards);

        Clreb020 program = new Clreb020(cardFile, printFile, factorFile);
        int rc = program.run();

        assertEquals(0, rc);

        // Verify all 64 factor records written (binary: 64 * 21 = 1344 bytes)
        List<String> factorRecords = readFactorRecords(factorFile);
        assertEquals(64, factorRecords.size());

        // Verify print file contains multiple "PAGE" references (page headers)
        String printContent = Files.readString(printFile);
        long pageHeaders = printContent.lines()
                .filter(line -> line.contains("CLREB020"))
                .count();
        assertTrue(pageHeaders > 1, "Should have multiple page headers for 64 cards");
    }

    // --- Test 3: Invalid non-numeric year ---

    @Test
    void testInvalidNonNumericYear() throws IOException {
        List<String> cards = List.of(
                makeCard("AB", "1", "29744")
        );
        writeCardFile(cardFile, cards);

        Clreb020 program = new Clreb020(cardFile, printFile, factorFile);
        int rc = program.run();

        assertEquals(0, rc, "Non-numeric year is an error but not a sequence error");

        // Factor file should be empty (0 bytes)
        assertEquals(0, Files.size(factorFile),
                "No factor records should be written for invalid input");

        // Print file should contain "NOT NUMERIC"
        String printContent = Files.readString(printFile);
        assertTrue(printContent.contains("NOT NUMERIC"),
                "Print file should show error message");
    }

    // --- Test 4: Invalid bad quad (value = 5) ---

    @Test
    void testInvalidBadQuad() throws IOException {
        List<String> cards = List.of(
                makeCard("25", "5", "29744")
        );
        writeCardFile(cardFile, cards);

        Clreb020 program = new Clreb020(cardFile, printFile, factorFile);
        int rc = program.run();

        assertEquals(0, rc);

        // No factor records
        assertEquals(0, Files.size(factorFile));

        // Error message in print
        String printContent = Files.readString(printFile);
        assertTrue(printContent.contains("NOT NUMERIC"));
    }

    // --- Test 5: Invalid zero factor ---

    @Test
    void testInvalidZeroFactor() throws IOException {
        List<String> cards = List.of(
                makeCard("25", "1", "00000")
        );
        writeCardFile(cardFile, cards);

        Clreb020 program = new Clreb020(cardFile, printFile, factorFile);
        int rc = program.run();

        assertEquals(0, rc);

        // Java rejects "00000" as not > 0 (numeric comparison).
        // COBOL accepts it (alphanumeric comparison quirk). This is a known difference.
        assertEquals(0, Files.size(factorFile));
    }

    // --- Test 6: Sequence error (descending cards) ---

    @Test
    void testSequenceError() throws IOException {
        List<String> cards = List.of(
                makeCard("25", "2", "29744"),
                makeCard("25", "1", "29744")  // Out of order: "251" < "252"
        );
        writeCardFile(cardFile, cards);

        Clreb020 program = new Clreb020(cardFile, printFile, factorFile);
        int rc = program.run();

        assertEquals(16, rc, "Sequence error should set return code to 16");
    }

    // --- Test 7: Empty input file ---

    @Test
    void testEmptyInput() throws IOException {
        Files.writeString(cardFile, "");

        Clreb020 program = new Clreb020(cardFile, printFile, factorFile);
        int rc = program.run();

        assertEquals(0, rc, "Empty input should not be an error");

        // No factor records
        assertEquals(0, Files.size(factorFile));
    }

    // --- Test 8: Factor record format verification ---

    @Test
    void testFactorRecordExactFormat() throws IOException {
        List<String> cards = List.of(
                makeCard("24", "3", "31500")
        );
        writeCardFile(cardFile, cards);

        Clreb020 program = new Clreb020(cardFile, printFile, factorFile);
        program.run();

        List<String> factorRecords = readFactorRecords(factorFile);
        assertEquals(1, factorRecords.size());
        String rec = factorRecords.get(0);

        // Exact byte-level verification matching REBEQFRD01.cpy layout
        assertEquals(21, rec.length(), "Record must be 21 chars");
        assertEquals("24", rec.substring(0, 2), "EQ-YEAR");
        assertEquals("3", rec.substring(2, 3), "EQ-QUAD");
        assertEquals("31500", rec.substring(3, 8), "EQ-FACTOR");
        assertEquals("             ", rec.substring(8, 21), "FILLER");
    }

    // --- Test 9: Print line format verification ---

    @Test
    void testPrintLineFormat() throws IOException {
        List<String> cards = List.of(
                makeCard("25", "1", "29744")
        );
        writeCardFile(cardFile, cards);

        Clreb020 program = new Clreb020(cardFile, printFile, factorFile);
        program.run();

        // Print file should have content
        String printContent = Files.readString(printFile);
        assertFalse(printContent.isEmpty(), "Print file should have content");

        // Check that heading contains expected text
        assertTrue(printContent.contains("CLREB020"), "Should contain program name");
        assertTrue(printContent.contains("OFFICE  OF  THE COUNTY  CLERK"), "Should contain office name");
        assertTrue(printContent.contains("EQUALIZATION      FACTORS"), "Should contain title");
        assertTrue(printContent.contains("YEAR      QUAD     FACTOR"), "Should contain column headers");
    }

    // --- Test 10: Date in header ---

    @Test
    void testDateInHeader() throws IOException {
        List<String> cards = List.of(
                makeCard("25", "1", "29744")
        );
        writeCardFile(cardFile, cards);

        Clreb020 program = new Clreb020(cardFile, printFile, factorFile);
        program.run();

        String printContent = Files.readString(printFile);
        // Date should be in YYYYMMDD format (today's date)
        String today = java.time.LocalDate.now().format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
        assertTrue(printContent.contains(today),
                "Print file should contain today's date in YYYYMMDD format");
    }
}
