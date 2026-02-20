#!/usr/bin/env python3
## \file test_utilities.py
## Shared utilities for CLREB020 test harnesses.
##
## Contains constants, test case definitions, and comparison functions used by
## both the COBOL and Java test harnesses. Extracted to eliminate ~400 lines of
## duplication between cobol_test_harness.py and java_test_harness.py.
##
## These utilities are specific to the CLREB020 Equalization Factor Edit and
## List program. They encode knowledge of the program's record formats, output
## conventions, and validation behavior.

import os
import pathlib

# ============================================================================
# PATH CONFIGURATION (runtime-computed globals)
# ============================================================================
## Absolute path to the directory containing this script (Testing/CLREB020/).
g_test_harness_directory_absolute_path: pathlib.Path = pathlib.Path(__file__).resolve().parent
## Absolute path to the project root (CobolTaxProgramModernization/).
g_project_root_absolute_path: pathlib.Path = g_test_harness_directory_absolute_path.parent.parent
## Directory containing test case input files, one subdirectory per test.
g_test_cases_directory_path: pathlib.Path = g_test_harness_directory_absolute_path / "test_cases"
## Directory containing expected output baselines, one subdirectory per test.
g_expected_output_directory_path: pathlib.Path = g_test_harness_directory_absolute_path / "expected_output"

# ============================================================================
# RECORD FORMAT CONSTANTS
# ============================================================================
## Size of each input card record. Matches the COBOL FD:
## RECORD CONTAINS 80 CHARACTERS. Layout:
## - position 1-2 = CD-YR (year)
## - position 3 = CD-QUAD (quadrant 1-4)
## - position 4-8 = CD-FACTOR (5 digits as 9V9999)
## - position 9-80 = filler spaces
INPUT_RECORD_SIZE_IN_BYTES: int = 80
## Size of each output factor record. Matches the COBOL FACTOR-REC layout:
## 2-byte year + 1-byte quad + 5-byte factor + 13-byte filler = 21 bytes.
FACTOR_RECORD_SIZE_IN_BYTES: int = 21

# ============================================================================
# EXECUTION CONSTANTS
# ============================================================================
## Maximum wall-clock time for a single CLREB020 invocation. The program
## processes cards sequentially with no I/O waits, so even large inputs
## complete well within this limit.
SUBPROCESS_TIMEOUT_IN_SECONDS: int = 30

# ============================================================================
# FILE NAME CONVENTIONS
# ============================================================================
## Binary fixed-length input card file within each test case directory.
INPUT_FILENAME: str = "input.dat"
## Binary fixed-length output factor file (21-byte records, no terminators).
FACTOR_FILENAME: str = "factor.dat"
## Line-sequential print output with ASA carriage control characters.
PRINT_FILENAME: str = "print.dat"
## Captured stdout (DISPLAY output: input/output/error record counts).
STDOUT_FILENAME: str = "stdout.txt"
## Expected return code (0 for success, 16 for sequence error).
RETURN_CODE_FILENAME: str = "returncode.txt"

# ============================================================================
# PRINT FILE DATE MASKING CONSTANTS
# ============================================================================
# The print output contains page headers with the current date that changes
# daily. When comparing print output against expected baselines, we mask the
# date field so tests are repeatable.
#
# Page header layout (relative to the form feed character):
#   Offset 0:     Form feed character (\x0c)
#   Offsets 1-11: Spaces
#   Offsets 12-19: 8-digit date (YYYYMMDD)
#   Offset 20+:   Rest of header line
#
# On the first page, the form feed is at position 0 of the line.
# On subsequent pages, the form feed is appended to the last detail line
# of the previous page at position 133 (the print record width).
## ASCII form feed character (0x0C) used by COBOL ASA carriage control.
FORM_FEED_CHARACTER: str = "\x0c"
## Offset from the form feed character to the start of the 8-digit date.
DATE_OFFSET_FROM_FORM_FEED: int = 12
## Length of the YYYYMMDD date field in the page header.
DATE_FIELD_LENGTH_IN_CHARACTERS: int = 8
## Fixed placeholder string that replaces dynamic YYYYMMDD dates in comparisons.
DATE_MASK_PLACEHOLDER: str = "XXXXXXXX"

# ============================================================================
# TEST CASE DEFINITIONS
# ============================================================================
## Shared test case definitions for all 50 CLREB020 test cases.
##
## Each entry describes a test case by name, human-readable description, and
## expected summary outcomes. Both the COBOL and Java harnesses iterate over
## this shared list so that any new test case is automatically picked up by
## both.
TEST_CASES: list[dict[str, object]] = [
    # ===== ORIGINAL TESTS (1-7) =====
    {
        "name": "valid_basic",
        "description": "4 valid cards (year 25, quads 1-4, factor 29744)",
        "expect_rc": 0,
        "expect_factor_records": 4,
        "expect_error_count": 0,
    },
    {
        "name": "valid_multipage",
        "description": "64 valid cards to trigger page breaks",
        "expect_rc": 0,
        "expect_factor_records": 64,
        "expect_error_count": 0,
    },
    {
        "name": "invalid_not_numeric",
        "description": "Non-numeric factor 'ABCDE' and year 'AB'",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 2,
    },
    {
        "name": "invalid_bad_quad",
        "description": "Quad = '5' (invalid, must be 1-4)",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
    {
        "name": "invalid_zero_factor",
        "description": "Factor '00000' accepted (alphanumeric comparison quirk)",
        "expect_rc": 0,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    {
        "name": "sequence_error",
        "description": "Two cards descending (252 then 251)",
        "expect_rc": 16,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    {
        "name": "empty_input",
        "description": "Empty file (0 bytes)",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 0,
    },
    # ===== FIELD BOUNDARY TESTS (8-17) =====
    {
        "name": "boundary_year_01",
        "description": "Lowest valid year '01'",
        "expect_rc": 0,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    {
        "name": "boundary_year_99",
        "description": "Highest valid year '99'",
        "expect_rc": 0,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    {
        "name": "boundary_year_00",
        "description": "Year '00' passes COBOL (alphanumeric comparison quirk)",
        "expect_rc": 0,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    {
        "name": "boundary_quad_1",
        "description": "Lower bound valid quad '1'",
        "expect_rc": 0,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    {
        "name": "boundary_quad_4",
        "description": "Upper bound valid quad '4'",
        "expect_rc": 0,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    {
        "name": "boundary_quad_0",
        "description": "Quad '0' just below valid range",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
    {
        "name": "boundary_quad_5",
        "description": "Quad '5' just above valid range",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
    {
        "name": "boundary_factor_00001",
        "description": "Minimum nonzero factor '00001'",
        "expect_rc": 0,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    {
        "name": "boundary_factor_99999",
        "description": "Maximum factor '99999'",
        "expect_rc": 0,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    {
        "name": "boundary_factor_10000",
        "description": "Factor '10000' = 1.0 (no equalization)",
        "expect_rc": 0,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    # ===== VALIDATION COMBINATORIAL TESTS (18-26) =====
    {
        "name": "invalid_year_letters",
        "description": "Year 'AB' - pure non-numeric",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
    {
        "name": "invalid_year_mixed",
        "description": "Year '2A' - mixed numeric/alpha",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
    {
        "name": "invalid_year_spaces",
        "description": "Year '  ' - spaces fail NUMERIC test",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
    {
        "name": "invalid_factor_mixed",
        "description": "Factor '29A44' - letter embedded in digits",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
    {
        "name": "invalid_factor_spaces",
        "description": "Factor '2 744' - space embedded in digits",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
    {
        "name": "invalid_quad_9",
        "description": "Quad '9' - numeric but out of range",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
    {
        "name": "invalid_quad_space",
        "description": "Quad ' ' - not in '1'..'4'",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
    {
        "name": "invalid_quad_letter",
        "description": "Quad 'A' - not in '1'..'4'",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
    {
        "name": "invalid_all_fields_bad",
        "description": "All fields invalid: 'XY', 'Z', 'ABCDE'",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
    # ===== SEQUENCE TESTS (27-31) =====
    {
        "name": "sequence_ascending_all_quads",
        "description": "2 years x 4 quads ascending",
        "expect_rc": 0,
        "expect_factor_records": 8,
        "expect_error_count": 0,
    },
    {
        "name": "sequence_year_transition",
        "description": "Cards spanning year boundary (24->25->26)",
        "expect_rc": 0,
        "expect_factor_records": 5,
        "expect_error_count": 0,
    },
    {
        "name": "sequence_duplicate_key",
        "description": "Duplicate year+quad allowed (LESS THAN check, not <=)",
        "expect_rc": 0,
        "expect_factor_records": 3,
        "expect_error_count": 0,
    },
    {
        "name": "sequence_error_after_valid",
        "description": "2 valid then out-of-sequence (RC=16)",
        "expect_rc": 16,
        "expect_factor_records": 2,
        "expect_error_count": 0,
    },
    {
        "name": "sequence_error_immediate",
        "description": "Second card out of sequence (RC=16)",
        "expect_rc": 16,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    # ===== DATA-FLOW AND STATE TESTS (32-34) =====
    {
        "name": "mixed_valid_invalid_stream",
        "description": "V,I,V,I,V,V - tests WK-MESG cleanup between cards",
        "expect_rc": 0,
        "expect_factor_records": 4,
        "expect_error_count": 2,
    },
    {
        "name": "error_message_cleanup",
        "description": "V-I-V: verify error message doesn't bleed into next card",
        "expect_rc": 0,
        "expect_factor_records": 2,
        "expect_error_count": 1,
    },
    {
        "name": "counter_invariant",
        "description": "10 cards (7V+3I): verify IN=OUT+ERR",
        "expect_rc": 0,
        "expect_factor_records": 7,
        "expect_error_count": 3,
    },
    # ===== PAGE BREAK TESTS (35-39) =====
    {
        "name": "page_break_at_23_cards",
        "description": "23 cards - just below page break threshold",
        "expect_rc": 0,
        "expect_factor_records": 23,
        "expect_error_count": 0,
    },
    {
        "name": "page_break_at_24_cards",
        "description": "24 cards - exact page break trigger",
        "expect_rc": 0,
        "expect_factor_records": 24,
        "expect_error_count": 0,
    },
    {
        "name": "page_break_at_25_cards",
        "description": "25 cards - one past page break",
        "expect_rc": 0,
        "expect_factor_records": 25,
        "expect_error_count": 0,
    },
    {
        "name": "page_break_three_pages",
        "description": "72 cards - spans 3+ printed pages",
        "expect_rc": 0,
        "expect_factor_records": 72,
        "expect_error_count": 0,
    },
    {
        "name": "page_break_with_errors",
        "description": "30 cards (25V+5I) with page breaks and errors interleaved",
        "expect_rc": 0,
        "expect_factor_records": 25,
        "expect_error_count": 5,
    },
    # ===== BUSINESS SCENARIO TESTS (40-43) =====
    {
        "name": "business_annual_run",
        "description": "Standard annual run: year 25, quads 1-4",
        "expect_rc": 0,
        "expect_factor_records": 4,
        "expect_error_count": 0,
    },
    {
        "name": "business_multi_year",
        "description": "3 years x 4 quads = 12 cards",
        "expect_rc": 0,
        "expect_factor_records": 12,
        "expect_error_count": 0,
    },
    {
        "name": "business_data_entry_error",
        "description": "5 cards: 4 valid + 1 data-entry typo",
        "expect_rc": 0,
        "expect_factor_records": 4,
        "expect_error_count": 1,
    },
    {
        "name": "business_factor_unity",
        "description": "Factor '10000' = 1.0000 (no equalization adjustment)",
        "expect_rc": 0,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    # ===== OUTPUT FORMAT TESTS (44-48) =====
    {
        "name": "factor_record_exact_bytes",
        "description": "4 cards for byte-level factor file verification",
        "expect_rc": 0,
        "expect_factor_records": 4,
        "expect_error_count": 0,
    },
    {
        "name": "print_detail_exact_layout",
        "description": "1 valid card for print detail line layout check",
        "expect_rc": 0,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    {
        "name": "print_header_exact_layout",
        "description": "1 valid card for header line verification",
        "expect_rc": 0,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    {
        "name": "print_error_line_layout",
        "description": "1 invalid card for error line layout check",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
    {
        "name": "factor_display_formatting",
        "description": "5 factors for N.NNNN display format verification",
        "expect_rc": 0,
        "expect_factor_records": 5,
        "expect_error_count": 0,
    },
    # ===== SINGLE-CARD EDGE CASES (49-50) =====
    {
        "name": "single_valid_card",
        "description": "Minimum valid input: 1 card",
        "expect_rc": 0,
        "expect_factor_records": 1,
        "expect_error_count": 0,
    },
    {
        "name": "single_invalid_card",
        "description": "Single invalid card (all fields bad)",
        "expect_rc": 0,
        "expect_factor_records": 0,
        "expect_error_count": 1,
    },
]

# ============================================================================
# STDOUT PARSING
# ============================================================================

## Parse the DISPLAY output from CLREB020 to extract record counts.
##
## CLREB020 writes three summary lines to stdout at the end of execution:
##     NO. OF INPUT RECORDS  = +NNN
##     NO. OF OUTPUT RECORDS = +NNN
##     NO. OF ERROR RECORDS  = +NNN
##
## The COBOL program formats these with a leading '+' sign (COMP-3 DISPLAY).
## The Java conversion writes plain integers, but both formats are handled
## here by stripping the '+' before parsing.
##
## \param[in] stdout_text - The captured stdout text from program execution.
## \return dict with keys 'input_count', 'output_count', 'error_count'.
##         Each value is an int if parsed successfully, or None if not found.
def parse_stdout_counts(stdout_text: str) -> dict[str, object]:
    # EXTRACT RECORD COUNTS FROM THE THREE SUMMARY LINES IN STDOUT.
    keyword_to_record_category: dict[str, str] = {
        "INPUT RECORDS": "input_count",
        "OUTPUT RECORDS": "output_count",
        "ERROR RECORDS": "error_count",
    }
    record_category_to_count: dict[str, object] = {"input_count": None, "output_count": None, "error_count": None}
    for line in stdout_text.splitlines():
        # STRIP THE LINE OF LEADING/TRAILING WHITESPACE.
        # COBOL DISPLAY output may have leading spaces from COMP-3 formatting.
        stripped_line: str = line.strip()

        # SEARCH FOR KEYWORDS AND EXTRACT THE CORRESPONDING RECORD COUNT.
        for keyword, record_category in keyword_to_record_category.items():
            # CHECK IF THE TYPE OF RECORD ACCOUNT APPEARS ON THE LINE.
            # Both the record count category keyword and equals sign indicate the count should appear on the line.
            has_matching_keyword: bool = keyword in stripped_line and "=" in stripped_line
            if has_matching_keyword:
                # The value after "=" may be malformed or missing, so guard
                # against non-integer text (ValueError) or a missing right-hand
                # side (IndexError).
                try:
                    # EXTRACT THE RECORD COUNT FOR THE CURRENT RECORD CATEGORY.
                    # The line format is "NO. OF INPUT RECORDS  = +004".
                    # Split on "=" to isolate the right-hand side value.
                    RIGHT_HAND_SIDE_INDEX: int = 1
                    right_hand_side: str = stripped_line.split("=")[RIGHT_HAND_SIDE_INDEX]
                    # Strip whitespace around the value, then remove the leading
                    # "+" sign that COBOL's COMP-3 DISPLAY format prepends.
                    raw_value: str = right_hand_side.strip().lstrip("+")
                    record_category_to_count[record_category] = int(raw_value)
                except (ValueError, IndexError):
                    # In the event of an error, there's nothing to do.
                    pass
                # Once we've found and processed the count for one category,
                # we can break out of the inner loop to avoid processing the
                # same line multiple times.
                break

    return record_category_to_count

# ============================================================================
# FILE COMPARISON FUNCTIONS
# ============================================================================

## Count the number of fixed-length records in the factor output file.
##
## Each factor record is exactly FACTOR_RECORD_SIZE_IN_BYTES (21) bytes.
## The file contains no record separators or line terminators.
##
## \param[in] factor_file_path - Path to the binary factor output file.
## \return 0 if the file does not exist or is empty.
##         The record count if the file size is evenly divisible by record size.
##         -1 if the file size is not evenly divisible (indicates file corruption).
def count_factor_records(factor_file_path: str) -> int:
    # CHECK WHETHER THE FILE EXISTS ON DISK.
    RECORD_COUNT_FOR_MISSING_OR_EMPTY_FILE: int = 0
    file_exists: bool = os.path.exists(factor_file_path)
    if not file_exists:
        # INDICATE THERE ARE NO RECORDS.
        return RECORD_COUNT_FOR_MISSING_OR_EMPTY_FILE

    # CHECK WHETHER THE FILE IS EMPTY (ZERO BYTES).
    file_size_in_bytes: int = os.path.getsize(factor_file_path)
    file_is_empty: bool = file_size_in_bytes == 0
    if file_is_empty:
        # INDICATE THERE ARE NO RECORDS.
        return RECORD_COUNT_FOR_MISSING_OR_EMPTY_FILE

    # CHECK WHETHER THE FILE SIZE IS EVENLY DIVISIBLE BY THE FIXED RECORD LENGTH.
    # An indivisible size indicates file corruption or truncation.
    ZERO_REMAINDER: int = 0
    remaining_byte_count_after_potential_records: int = file_size_in_bytes % FACTOR_RECORD_SIZE_IN_BYTES
    file_size_is_evenly_divisible: bool = remaining_byte_count_after_potential_records == ZERO_REMAINDER
    if not file_size_is_evenly_divisible:
        # INDICATE THE FILE IS CORRUPTED OR TRUNCATED.
        RECORD_COUNT_FOR_CORRUPTED_FILE: int = -1
        return RECORD_COUNT_FOR_CORRUPTED_FILE

    # COMPUTE THE RECORD COUNT BY DIVIDING THE TOTAL FILE SIZE BY THE FIXED LENGTH OF EACH RECORD.
    record_count: int = file_size_in_bytes // FACTOR_RECORD_SIZE_IN_BYTES
    return record_count

## Compare two files byte-for-byte.
##
## Handles the case where one or both files do not exist:
##     - Both missing: considered a match (both produced no output).
##     - One missing, one empty: considered a match (empty is equivalent
##       to not-created for CLREB020's output files).
##     - One missing, one non-empty: not a match.
##
## \param[in] actual_file_path - Path to the actual output file.
## \param[in] expected_file_path - Path to the expected baseline file.
## \return True if the files are identical (or both absent/empty), False otherwise.
def files_match_binary(actual_file_path: str, expected_file_path: str) -> bool:
    # CHECK WHICH FILES EXIST.
    actual_file_exists: bool = os.path.exists(actual_file_path)
    expected_file_exists: bool = os.path.exists(expected_file_path)
    both_files_missing: bool = not actual_file_exists and not expected_file_exists
    if both_files_missing:
        # If both files are missing, they're both equal.
        return True
    
    # CHECK IF ONLY ONE FILE IS MISSING.
    one_file_missing: bool = not actual_file_exists or not expected_file_exists
    if one_file_missing:
        # DETERMINE WHICH FILE EXISTS.
        existing_file_path: str = actual_file_path if actual_file_exists else expected_file_path

        # CHECK IF THE EXISTING FILE IS EMPTY.
        # If the existing file is empty, that's considered equivalent to both missing.
        existing_file_size_in_bytes: int = os.path.getsize(existing_file_path)
        existing_file_is_empty: bool = existing_file_size_in_bytes == 0
        return existing_file_is_empty

    # COMPARE THE FILE CONTENTS BYTE-FOR-BYTE.
    with open(actual_file_path, "rb") as actual_file, open(expected_file_path, "rb") as expected_file:
        # CHECK IF THE FILE CONTENTS ARE EXACTLY EQUAL.
        actual_file_contents: bytes = actual_file.read()
        expected_file_contents: bytes = expected_file.read()
        files_are_identical: bool = actual_file_contents == expected_file_contents
        return files_are_identical

## Read a print file and mask all date fields for repeatable comparison.
##
## The CLREB020 print output includes page headers with the current date
## in YYYYMMDD format. This date changes daily, making byte-for-byte
## comparison unreliable. We replace every 8-character date with a fixed
## placeholder so that expected baselines remain valid over time.
##
## The form feed character can appear in two positions:
##     - Page 1: at position 0 of the line (the line IS the header).
##     - Page 2+: at position 133 of a line (appended to the last detail
##       line of the previous page, because GnuCOBOL's line-sequential
##       output concatenates the ASA form feed with the preceding record).
##
## In all cases, the date starts DATE_OFFSET_FROM_FORM_FEED (12) characters
## after the form feed and is DATE_FIELD_LENGTH_IN_CHARACTERS (8) characters long.
##
## \param[in] print_file_path - Path to the print output file.
## \return The normalized print content with dates masked, as a string.
def normalize_print_content(print_file_path: str) -> str:
    # CHECK IF THE PRINT REPORT FILE EXISTS.
    print_file_exists: bool = os.path.exists(print_file_path)
    if not print_file_exists:
        # INDICATE THERE ARE NO FILE CONTENTS.
        return ""

    # READ THE LINES FROM THE FILE.
    lines: list[str] = []
    with open(print_file_path, "r", errors = "replace") as print_file:
        lines = print_file.readlines()

    # MASK THE DATE FIELD IN ANY LINE THAT CONTAINS A FORM FEED CHARACTER.
    normalized_lines: list[str] = []
    for line in lines:
        # STRIP TRAILING WHITESPACE FROM THE LINE.
        # Trailing whitespace is removed to make it easier to extract out the date.
        stripped_line: str = line.rstrip()

        # CHECK IF THE LINE CONTAINS A FORM FEED.
        form_feed_position: int = stripped_line.find(FORM_FEED_CHARACTER)
        line_contains_form_feed: bool = form_feed_position >= 0
        if line_contains_form_feed:
            # DETERMINE IF THE LINE MIGHT CONTAIN A DATE.
            date_start_position: int = form_feed_position + DATE_OFFSET_FROM_FORM_FEED
            date_end_position: int = date_start_position + DATE_FIELD_LENGTH_IN_CHARACTERS
            stripped_line_character_count: int = len(stripped_line)
            line_is_long_enough_for_date: bool = stripped_line_character_count >= date_end_position
            if line_is_long_enough_for_date:
                # MASK THE DATE FIELD IN THE LINE.
                line_before_date: str = stripped_line[:date_start_position]
                line_after_date: str = stripped_line[date_end_position:]
                stripped_line = line_before_date + DATE_MASK_PLACEHOLDER + line_after_date
                    
        # TRACK THE NORMALIZED LINE.
        normalized_lines.append(stripped_line)

    # RETURN THE FULLY NORMALIZED PRINT FILE CONTENTS.
    normalized_file_contents: str = "\n".join(normalized_lines)
    return normalized_file_contents

## Compare two print files, masking the date line that changes daily.
##
## Uses normalize_print_content() to replace the dynamic date field with
## a fixed placeholder before comparison.
##
## \param[in] actual_file_path - Path to the actual print output file.
## \param[in] expected_file_path - Path to the expected print baseline file.
## \return True if the normalized content is identical, False otherwise.
def text_files_match(actual_file_path: str, expected_file_path: str) -> bool:
    actual_content: str = normalize_print_content(actual_file_path)
    expected_content: str = normalize_print_content(expected_file_path)
    files_are_identical: bool = actual_content == expected_content
    return files_are_identical
