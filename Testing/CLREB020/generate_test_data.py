#!/usr/bin/env python3
## \file generate_test_data.py
## Test Data Generator for CLREB020 Exhaustive Test Suite.
##
## Generates input card files (80-byte fixed-width records) for all 43 test cases
## added by Task 003. Each test case gets a directory under test_cases/ with an
## input.dat file.
##
## After generating input files, run:
##     python cobol_test_harness.py --generate-expected
## to create expected output baselines from the COBOL executable.
##
## Usage:
##     python Testing/CLREB020/generate_test_data.py

import os
import pathlib

import test_utilities

# ============================================================================
# CARD CONSTRUCTION CONSTANTS
# ============================================================================
## Default equalization factor used in most test cases (represents 2.9744).
## Factor values use implicit V9999 decimal: 29744 = 2.9744.
DEFAULT_FACTOR: str = "29744"
## Number of quadrants (1-4) per tax year.
QUADRANTS_PER_YEAR: int = 4

# ============================================================================
# CARD BUILDING HELPERS
# ============================================================================

## Build an 80-byte binary card record from individual field values.
##
## Pads each field to its required width and fills the remainder with spaces.
## Returns raw bytes suitable for writing directly to a binary input file.
##
## \param[in] year - 2-character year string (e.g., "25" for 2025).
## \param[in] quadrant - 1-character quadrant string ("1" through "4").
## \param[in] factor - 5-character factor string (e.g., "29744" for 2.9744).
## \return An 80-byte record in latin-1 encoding.
def make_card(year: str, quadrant: str, factor: str) -> bytes:
    # BUILD THE FIXED-WIDTH CARD TEXT FROM YEAR, QUADRANT, FACTOR, AND FILLER.
    # Width of the CD-YR field in the card record (columns 1-2).
    YEAR_FIELD_WIDTH_IN_CHARACTERS: int = 2
    # Width of the CD-QUAD field in the card record (column 3).
    QUADRANT_FIELD_WIDTH_IN_CHARACTERS: int = 1
    # Width of the CD-FACTOR field in the card record (columns 4-8).
    FACTOR_FIELD_WIDTH_IN_CHARACTERS: int = 5
    # Width of filler spaces (columns 9-80) = 72 bytes.
    FILLER_WIDTH_IN_CHARACTERS: int = test_utilities.INPUT_RECORD_SIZE_IN_BYTES - YEAR_FIELD_WIDTH_IN_CHARACTERS - QUADRANT_FIELD_WIDTH_IN_CHARACTERS - FACTOR_FIELD_WIDTH_IN_CHARACTERS
    card_text: str = (
        f"{year:<{YEAR_FIELD_WIDTH_IN_CHARACTERS}.{YEAR_FIELD_WIDTH_IN_CHARACTERS}s}"
        f"{quadrant:<{QUADRANT_FIELD_WIDTH_IN_CHARACTERS}.{QUADRANT_FIELD_WIDTH_IN_CHARACTERS}s}"
        f"{factor:<{FACTOR_FIELD_WIDTH_IN_CHARACTERS}.{FACTOR_FIELD_WIDTH_IN_CHARACTERS}s}"
        f"{'':<{FILLER_WIDTH_IN_CHARACTERS}s}"
    )

    # VERIFY THE CARD IS OF THE APPROPRIATE LENGTH.
    assert len(card_text) == test_utilities.INPUT_RECORD_SIZE_IN_BYTES, f"Card length {len(card_text)} != {test_utilities.INPUT_RECORD_SIZE_IN_BYTES}"

    # RETURN THE PROPERLY ENCODED CARD.
    encoded_card_text: bytes = card_text.encode("latin-1")
    return encoded_card_text

## Write a list of 80-byte card records to test_cases/<name>/input.dat.
##
## Creates the test case directory if it does not exist.
##
## \param[in] test_name - Name of the test case (used as subdirectory name).
## \param[in] cards - List of bytes objects, each exactly test_utilities.INPUT_RECORD_SIZE_IN_BYTES.
def write_cards(test_name: str, cards: list[bytes]):
    # WRITE ALL CARD RECORDS TO THE TEST CASE INPUT FILE.
    # The directory needs to exist before being able to write to it.
    output_directory_path: pathlib.Path = test_utilities.g_test_cases_directory_path / test_name
    output_directory_path.mkdir(parents = True, exist_ok = True)
    with open(output_directory_path / "input.dat", "wb") as test_case_input_file:
        # WRITE EACH CARD TO THE FILE.
        for card in cards:
            test_case_input_file.write(card)

    # PRINT OUT STATISTICS ABOUT THE CARD SIZES.
    total_size_in_bytes: int = len(cards) * test_utilities.INPUT_RECORD_SIZE_IN_BYTES
    print(f"  {test_name}: {len(cards)} cards, {total_size_in_bytes} bytes")

## Build a list of cards in ascending year+quadrant order.
##
## Generates cards by cycling through quadrants 1-4 within incrementing years,
## starting at year 10. This produces a valid ascending sequence suitable
## for tests that need many cards without triggering sequence errors.
##
## The starting year of 10 is chosen to leave room below for sequence-error
## tests that need earlier years.
##
## \param[in] card_count - Number of cards to generate.
## \param[in] factor - Factor value for all cards (default: DEFAULT_FACTOR).
## \return list[bytes]: Cards in ascending year+quadrant order.
def build_ascending_cards(card_count: int, factor: str = DEFAULT_FACTOR) -> list[bytes]:
    # GENERATE CARDS BY CYCLING THROUGH QUADRANTS WITHIN INCREMENTING YEARS.
    starting_year: int = 10
    cards: list[bytes] = []
    for card_index in range(card_count):
        # DETERMINE THE YEAR FOR THE CURRENT CARD.
        # Each group of 4 consecutive cards belongs to the same year.
        completed_year_cycles: int = card_index // QUADRANTS_PER_YEAR
        year_number: int = starting_year + completed_year_cycles
        year: str = f"{year_number:02d}"

        # DETERMINE THE QUADRANT FOR THE CURRENT CARD.
        # Quadrants cycle 1-4 within each year.
        FIRST_QUADRANT: int = 1
        zero_based_quadrant_index: int = card_index % QUADRANTS_PER_YEAR
        quadrant_number: int = FIRST_QUADRANT + zero_based_quadrant_index
        quadrant: str = str(quadrant_number)

        # CREATE THE CARD.
        cards.append(make_card(year, quadrant, factor))
    return cards

# ============================================================================
# TEST DATA GENERATION — BY CATEGORY
# ============================================================================

## Generate input data for field boundary tests (tests 8-17).
##
## Tests the edges of each card field's valid range: year 01/99/00,
## quadrant 0/1/4/5, and factor 00001/10000/99999.
def generate_boundary_tests():
    # Year boundaries
    write_cards("boundary_year_01", [make_card("01", "1", DEFAULT_FACTOR)])
    write_cards("boundary_year_99", [make_card("99", "1", DEFAULT_FACTOR)])
    # Year "00" passes COBOL's alphanumeric "GREATER THAN 0" check
    write_cards("boundary_year_00", [make_card("00", "1", DEFAULT_FACTOR)])
    # Quad boundaries (valid range: 1-4)
    write_cards("boundary_quad_1", [make_card("25", "1", DEFAULT_FACTOR)])
    write_cards("boundary_quad_4", [make_card("25", "4", DEFAULT_FACTOR)])
    write_cards("boundary_quad_0", [make_card("25", "0", DEFAULT_FACTOR)])
    write_cards("boundary_quad_5", [make_card("25", "5", DEFAULT_FACTOR)])
    # Factor boundaries (displayed as N.NNNN, so 10000 = 1.0000)
    write_cards("boundary_factor_00001", [make_card("25", "1", "00001")])
    write_cards("boundary_factor_99999", [make_card("25", "1", "99999")])
    write_cards("boundary_factor_10000", [make_card("25", "1", "10000")])

## Generate input data for validation combinatorial tests (tests 18-26).
##
## Tests various invalid field values: non-numeric years, mixed characters,
## spaces, out-of-range quadrants, and combinations of multiple bad fields.
def generate_validation_tests():
    # Invalid year values
    write_cards("invalid_year_letters", [make_card("AB", "1", DEFAULT_FACTOR)])
    write_cards("invalid_year_mixed", [make_card("2A", "1", DEFAULT_FACTOR)])
    write_cards("invalid_year_spaces", [make_card("  ", "1", DEFAULT_FACTOR)])
    # Invalid factor values
    write_cards("invalid_factor_mixed", [make_card("25", "1", "29A44")])
    write_cards("invalid_factor_spaces", [make_card("25", "1", "2 744")])
    # Invalid quadrant values (numeric but out of range, space, letter)
    write_cards("invalid_quad_9", [make_card("25", "9", DEFAULT_FACTOR)])
    write_cards("invalid_quad_space", [make_card("25", " ", DEFAULT_FACTOR)])
    write_cards("invalid_quad_letter", [make_card("25", "A", DEFAULT_FACTOR)])
    # All fields invalid simultaneously
    write_cards("invalid_all_fields_bad", [make_card("XY", "Z", "ABCDE")])

## Generate input data for sequence tests (tests 27-31).
##
## CLREB020 enforces ascending order on the 3-character sort key (year+quadrant).
## These tests verify correct behavior for ascending sequences, year
## transitions, duplicate keys, and sequence errors at different positions.
def generate_sequence_tests():
    # Ascending: 2 years x 4 quadrants each
    write_cards("sequence_ascending_all_quadrants", [
        make_card("25", "1", DEFAULT_FACTOR),
        make_card("25", "2", "31205"),
        make_card("25", "3", "25000"),
        make_card("25", "4", "28103"),
        make_card("26", "1", "30000"),
        make_card("26", "2", "31000"),
        make_card("26", "3", "26000"),
        make_card("26", "4", "29000"),
    ])
    # Year boundary transition: 24 -> 25 -> 26
    write_cards("sequence_year_transition", [
        make_card("24", "3", DEFAULT_FACTOR),
        make_card("24", "4", "31205"),
        make_card("25", "1", "25000"),
        make_card("25", "2", "28103"),
        make_card("26", "1", "30000"),
    ])
    # Duplicate year+quadrant is allowed (COBOL uses LESS THAN, not LESS OR EQUAL)
    write_cards("sequence_duplicate_key", [
        make_card("25", "1", DEFAULT_FACTOR),
        make_card("25", "1", "30000"),
        make_card("25", "2", "31205"),
    ])
    # Sequence error after 2 valid cards (card 3 goes backward)
    write_cards("sequence_error_after_valid", [
        make_card("25", "1", DEFAULT_FACTOR),
        make_card("25", "3", "31205"),
        make_card("25", "2", "25000"),
        make_card("25", "4", "28103"),
    ])
    # Sequence error on second card (immediate)
    write_cards("sequence_error_immediate", [
        make_card("25", "2", DEFAULT_FACTOR),
        make_card("25", "1", "31205"),
    ])

## Generate input data for data-flow and state tests (tests 32-34).
##
## Tests internal state management: WK-MESG cleanup between valid and
## invalid cards, and the counter invariant (IN = OUT + ERR).
def generate_data_flow_tests():
    # Interleaved valid and invalid: V, I, V, I, V, V
    write_cards("mixed_valid_invalid_stream", [
        make_card("25", "1", DEFAULT_FACTOR),
        make_card("25", "2", "ABCDE"),
        make_card("25", "3", "31205"),
        make_card("25", "4", "2X744"),
        make_card("26", "1", "25000"),
        make_card("26", "2", "28103"),
    ])
    # Valid-Invalid-Valid: error message must not bleed into next valid card
    write_cards("error_message_cleanup", [
        make_card("25", "1", DEFAULT_FACTOR),
        make_card("25", "2", "XXXXX"),
        make_card("25", "3", "31205"),
    ])
    # 10 cards (7 valid + 3 invalid): verifies IN-CNT = OUT-CNT + ERROR-CNT
    write_cards("counter_invariant", [
        make_card("25", "1", DEFAULT_FACTOR),
        make_card("25", "2", "ABCDE"),
        make_card("25", "3", "31205"),
        make_card("25", "4", "25000"),
        make_card("26", "1", "XX000"),
        make_card("26", "2", "28103"),
        make_card("26", "3", "30000"),
        make_card("26", "4", "31000"),
        make_card("27", "1", "ZZZZZ"),
        make_card("27", "2", "26000"),
    ])

## Generate input data for page break tests (tests 35-39).
##
## CLREB020 prints a page header when LINE-CNT exceeds 55. LINE-CNT starts
## at 60 (forcing a header before the first detail). After the header,
## LINE-CNT resets to 8. Each detail line adds 2 to LINE-CNT. Therefore:
##     - 23 cards: LINE-CNT = 8 + 23*2 = 54 (no second page)
##     - 24 cards: LINE-CNT = 8 + 24*2 = 56 (triggers second page header)
##     - 25 cards: one card on second page
def generate_page_break_tests():
    # WRITE CARDS FOR AROUND THE FIRST COMMON PAGE BREAK.
    write_cards("page_break_at_23_cards", build_ascending_cards(23))
    write_cards("page_break_at_24_cards", build_ascending_cards(24))
    write_cards("page_break_at_25_cards", build_ascending_cards(25))

    # WRITE CARDS FOR AROUND THE 3+ PAGE BREAK.
    # 72 cards: spans 3+ printed pages
    write_cards("page_break_three_pages", build_ascending_cards(72))

    # BUILD 30 CARDS WITH INVALID FACTORS AT EVERY 5TH POSITION.
    TOTAL_CARD_COUNT: int = 30
    error_positions: set[int] = {5, 10, 15, 20, 25}
    starting_year: int = 10
    cards_with_errors: list[bytes] = []
    for card_index in range(TOTAL_CARD_COUNT):
        # DETERMINE THE YEAR FOR THE CURRENT CARD.
        completed_year_cycles: int = card_index // QUADRANTS_PER_YEAR
        year_number: int = starting_year + completed_year_cycles
        year: str = f"{year_number:02d}"

        # DETERMINE THE QUADRANT FOR THE CURRENT CARD.
        FIRST_QUADRANT: int = 1
        zero_based_quadrant_index: int = card_index % QUADRANTS_PER_YEAR
        quadrant_number: int = FIRST_QUADRANT + zero_based_quadrant_index
        quadrant: str = str(quadrant_number)

        # CHECK IF THE CARD SHOULD HAVE AN ERROR OR NOT.
        is_error_position: bool = card_index in error_positions
        if is_error_position:
            cards_with_errors.append(make_card(year, quadrant, "XXXXX"))
        else:
            cards_with_errors.append(make_card(year, quadrant, DEFAULT_FACTOR))

    write_cards("page_break_with_errors", cards_with_errors)

## Generate input data for business scenario tests (tests 40-43).
##
## Realistic inputs that mirror actual Cook County tax extension operations:
## annual runs, multi-year batches, data entry errors, and unity factors.
def generate_business_scenario_tests():
    # Standard annual run: 1 year, 4 quadrants with typical factors
    write_cards("business_annual_run", [
        make_card("25", "1", DEFAULT_FACTOR),
        make_card("25", "2", "31205"),
        make_card("25", "3", "25000"),
        make_card("25", "4", "28103"),
    ])
    # Multi-year batch: 3 years x 4 quadrants = 12 cards
    write_cards("business_multi_year", [
        make_card("23", "1", "28500"),
        make_card("23", "2", "30100"),
        make_card("23", "3", "24500"),
        make_card("23", "4", "27800"),
        make_card("24", "1", "29200"),
        make_card("24", "2", "30800"),
        make_card("24", "3", "25200"),
        make_card("24", "4", "28500"),
        make_card("25", "1", DEFAULT_FACTOR),
        make_card("25", "2", "31205"),
        make_card("25", "3", "25800"),
        make_card("25", "4", "28103"),
    ])
    # Data entry error: 4 valid cards + 1 typo ('S' instead of '5')
    write_cards("business_data_entry_error", [
        make_card("25", "1", DEFAULT_FACTOR),
        make_card("25", "2", "31205"),
        make_card("25", "3", "2S000"),
        make_card("25", "4", "28103"),
        make_card("26", "1", "30000"),
    ])
    # Factor of exactly 1.0000 (no equalization adjustment)
    write_cards("business_factor_unity", [make_card("25", "1", "10000")])

## Generate input data for output format tests (tests 44-48).
##
## Tests that verify exact byte-level output formatting: factor record
## layout, print detail lines, headers, error lines, and the N.NNNN
## display format for factors.
def generate_output_format_tests():
    # 4 cards with diverse factors for byte-level verification
    write_cards("factor_record_exact_bytes", [
        make_card("25", "1", DEFAULT_FACTOR),
        make_card("25", "2", "10000"),
        make_card("25", "3", "99999"),
        make_card("25", "4", "00001"),
    ])
    # Single valid card for print layout and header verification
    write_cards("print_detail_exact_layout", [make_card("25", "1", DEFAULT_FACTOR)])
    write_cards("print_header_exact_layout", [make_card("25", "1", DEFAULT_FACTOR)])
    # Single invalid card (all fields bad) for error line layout
    write_cards("print_error_line_layout", [make_card("AB", "5", "XXXXX")])
    # 5 different factors to verify N.NNNN display formatting
    write_cards("factor_display_formatting", [
        make_card("25", "1", "10000"),
        make_card("25", "2", "00001"),
        make_card("25", "3", "99999"),
        make_card("25", "4", DEFAULT_FACTOR),
        make_card("26", "1", "50000"),
    ])

## Generate input data for single-card edge cases (tests 49-50).
##
## Minimal inputs: one valid card and one completely invalid card.
def generate_edge_case_tests():
    write_cards("single_valid_card", [make_card("25", "1", DEFAULT_FACTOR)])
    write_cards("single_invalid_card", [make_card("XX", "9", "ABCDE")])

# ============================================================================
# MAIN ENTRY POINT
# ============================================================================

## Generate input data for all 43 test cases added by Task 003.
##
## The original 7 test cases (1-7) were created manually and are not
## regenerated here. This function generates tests 8-50 organized into
## seven categories: boundary, validation, sequence, data-flow, page-break,
## business scenario, and output format tests.
def generate_all():
    # GENERATE INPUT DATA FOR ALL TEST CATEGORIES.
    # Number of test cases generated (tests 8-50, excluding original tests 1-7).
    GENERATED_TEST_COUNT: int = 43
    print(f"Generating test data for {GENERATED_TEST_COUNT} test cases...")
    print()
    generate_boundary_tests()
    generate_validation_tests()
    generate_sequence_tests()
    generate_data_flow_tests()
    generate_page_break_tests()
    generate_business_scenario_tests()
    generate_output_format_tests()
    generate_edge_case_tests()
    print()
    print(f"Done. Generated {GENERATED_TEST_COUNT} test case input files.")
    print("Next: run 'python cobol_test_harness.py --generate-expected' to create baselines.")


if __name__ == "__main__":
    generate_all()
