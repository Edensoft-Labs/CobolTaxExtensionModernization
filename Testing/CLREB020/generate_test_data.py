#!/usr/bin/env python3
"""
Test Data Generator for CLREB020 Exhaustive Test Suite

Generates input card files (80-byte fixed-width records) for all 43 new test cases
defined in Task 003. Each test case gets a directory under test_cases/ with an
input.dat file.

After generating input files, run:
    python cobol_test_harness.py --generate-expected
to create expected output baselines from the COBOL executable.

Usage:
    python Testing/CLREB020/generate_test_data.py
"""

import os
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
TEST_CASES_DIR = SCRIPT_DIR / "test_cases"

RECORD_LEN = 80


def make_card(year: str, quad: str, factor: str) -> bytes:
    """Build an 80-byte card record from year (2), quad (1), factor (5), filler (72)."""
    raw = f"{year:<2.2s}{quad:<1.1s}{factor:<5.5s}{'':<72s}"
    assert len(raw) == RECORD_LEN, f"Card length {len(raw)} != {RECORD_LEN}"
    return raw.encode("latin-1")


def write_cards(test_name: str, cards: list[bytes]):
    """Write a list of 80-byte card records to test_cases/<name>/input.dat."""
    out_dir = TEST_CASES_DIR / test_name
    out_dir.mkdir(parents=True, exist_ok=True)
    with open(out_dir / "input.dat", "wb") as f:
        for card in cards:
            f.write(card)
    print(f"  {test_name}: {len(cards)} cards, {len(cards)*RECORD_LEN} bytes")


def generate_all():
    print("Generating test data for 43 new test cases...")
    print()

    # ================================================================
    # Field Boundary Tests (tests 8-17)
    # ================================================================

    # 8: boundary_year_01 - Lowest unambiguously valid year
    write_cards("boundary_year_01", [make_card("01", "1", "29744")])

    # 9: boundary_year_99 - Highest valid year
    write_cards("boundary_year_99", [make_card("99", "1", "29744")])

    # 10: boundary_year_00 - COBOL accepts (alphanumeric quirk), Java rejects
    write_cards("boundary_year_00", [make_card("00", "1", "29744")])

    # 11: boundary_quad_1 - Lower bound of valid range
    write_cards("boundary_quad_1", [make_card("25", "1", "29744")])

    # 12: boundary_quad_4 - Upper bound of valid range
    write_cards("boundary_quad_4", [make_card("25", "4", "29744")])

    # 13: boundary_quad_0 - Just below valid range
    write_cards("boundary_quad_0", [make_card("25", "0", "29744")])

    # 14: boundary_quad_5 - Just above valid range
    write_cards("boundary_quad_5", [make_card("25", "5", "29744")])

    # 15: boundary_factor_00001 - Minimum nonzero factor
    write_cards("boundary_factor_00001", [make_card("25", "1", "00001")])

    # 16: boundary_factor_99999 - Maximum factor
    write_cards("boundary_factor_99999", [make_card("25", "1", "99999")])

    # 17: boundary_factor_10000 - Factor 1.0 (no equalization)
    write_cards("boundary_factor_10000", [make_card("25", "1", "10000")])

    # ================================================================
    # Validation Combinatorial Tests (tests 18-26)
    # ================================================================

    # 18: invalid_year_letters - Year field has letters
    write_cards("invalid_year_letters", [make_card("AB", "1", "29744")])

    # 19: invalid_year_mixed - Year field has letter+digit mix
    write_cards("invalid_year_mixed", [make_card("2A", "1", "29744")])

    # 20: invalid_year_spaces - Year field has spaces
    write_cards("invalid_year_spaces", [make_card("  ", "1", "29744")])

    # 21: invalid_factor_mixed - Factor has mixed chars
    write_cards("invalid_factor_mixed", [make_card("25", "1", "29A44")])

    # 22: invalid_factor_spaces - Factor has spaces
    write_cards("invalid_factor_spaces", [make_card("25", "1", "2 744")])

    # 23: invalid_quad_9 - Quad digit out of range
    write_cards("invalid_quad_9", [make_card("25", "9", "29744")])

    # 24: invalid_quad_space - Quad is space
    write_cards("invalid_quad_space", [make_card("25", " ", "29744")])

    # 25: invalid_quad_letter - Quad is letter
    write_cards("invalid_quad_letter", [make_card("25", "A", "29744")])

    # 26: invalid_all_fields_bad - All three fields invalid
    write_cards("invalid_all_fields_bad", [make_card("XY", "Z", "ABCDE")])

    # ================================================================
    # Sequence Tests (tests 27-31)
    # ================================================================

    # 27: sequence_ascending_all_quads - Two years, all quads each
    write_cards("sequence_ascending_all_quads", [
        make_card("25", "1", "29744"),
        make_card("25", "2", "31205"),
        make_card("25", "3", "25000"),
        make_card("25", "4", "28103"),
        make_card("26", "1", "30000"),
        make_card("26", "2", "31000"),
        make_card("26", "3", "26000"),
        make_card("26", "4", "29000"),
    ])

    # 28: sequence_year_transition - Cards spanning year boundary
    write_cards("sequence_year_transition", [
        make_card("24", "3", "29744"),
        make_card("24", "4", "31205"),
        make_card("25", "1", "25000"),
        make_card("25", "2", "28103"),
        make_card("26", "1", "30000"),
    ])

    # 29: sequence_duplicate_key - Same year+quad allowed (LESS THAN, not <=)
    write_cards("sequence_duplicate_key", [
        make_card("25", "1", "29744"),
        make_card("25", "1", "30000"),
        make_card("25", "2", "31205"),
    ])

    # 30: sequence_error_after_valid - Valid cards then out-of-order
    #     First 2 cards processed, then sequence error on card 3
    write_cards("sequence_error_after_valid", [
        make_card("25", "1", "29744"),
        make_card("25", "3", "31205"),
        make_card("25", "2", "25000"),  # Out of sequence: "252" < "253"
        make_card("25", "4", "28103"),  # Never reached
    ])

    # 31: sequence_error_immediate - Error on second card
    write_cards("sequence_error_immediate", [
        make_card("25", "2", "29744"),
        make_card("25", "1", "31205"),  # "251" < "252" -> error
    ])

    # ================================================================
    # Data-Flow and State Tests (tests 32-34)
    # ================================================================

    # 32: mixed_valid_invalid_stream - Interleaved valid and invalid
    #     V, I, V, I, V, V (tests WK-MESG cleanup between cards)
    write_cards("mixed_valid_invalid_stream", [
        make_card("25", "1", "29744"),  # Valid
        make_card("25", "2", "ABCDE"),  # Invalid (non-numeric factor)
        make_card("25", "3", "31205"),  # Valid - must NOT show "NOT NUMERIC"
        make_card("25", "4", "2X744"),  # Invalid (mixed factor)
        make_card("26", "1", "25000"),  # Valid
        make_card("26", "2", "28103"),  # Valid
    ])

    # 33: error_message_cleanup - Invalid then valid, verify cleanup
    write_cards("error_message_cleanup", [
        make_card("25", "1", "29744"),  # Valid
        make_card("25", "2", "XXXXX"),  # Invalid
        make_card("25", "3", "31205"),  # Valid - WK-MESG must be spaces
    ])

    # 34: counter_invariant - 10 cards: 7 valid + 3 invalid
    #     Verifies IN-CNT = OUT-CNT + ERROR-CNT
    write_cards("counter_invariant", [
        make_card("25", "1", "29744"),  # Valid
        make_card("25", "2", "ABCDE"),  # Invalid
        make_card("25", "3", "31205"),  # Valid
        make_card("25", "4", "25000"),  # Valid
        make_card("26", "1", "XX000"),  # Invalid
        make_card("26", "2", "28103"),  # Valid
        make_card("26", "3", "30000"),  # Valid
        make_card("26", "4", "31000"),  # Valid
        make_card("27", "1", "ZZZZZ"),  # Invalid
        make_card("27", "2", "26000"),  # Valid
    ])

    # ================================================================
    # Page Break Tests (tests 35-39)
    # ================================================================

    # 35: page_break_at_23_cards - Just below page break
    #     LINE-CNT starts at 60 (>55) so heading fires before first detail.
    #     After heading: LINE-CNT = 8. Each detail adds 2.
    #     After 23 details: LINE-CNT = 8 + 23*2 = 54, which is <= 55. No second page.
    cards_23 = []
    for i in range(23):
        yr = f"{10 + i // 4:02d}"
        qd = str(1 + i % 4)
        cards_23.append(make_card(yr, qd, "29744"))
    write_cards("page_break_at_23_cards", cards_23)

    # 36: page_break_at_24_cards - Exact page break trigger
    #     After 24 details: LINE-CNT = 8 + 24*2 = 56, > 55. Second page header fires.
    cards_24 = []
    for i in range(24):
        yr = f"{10 + i // 4:02d}"
        qd = str(1 + i % 4)
        cards_24.append(make_card(yr, qd, "29744"))
    write_cards("page_break_at_24_cards", cards_24)

    # 37: page_break_at_25_cards - One past page break
    cards_25 = []
    for i in range(25):
        yr = f"{10 + i // 4:02d}"
        qd = str(1 + i % 4)
        cards_25.append(make_card(yr, qd, "29744"))
    write_cards("page_break_at_25_cards", cards_25)

    # 38: page_break_three_pages - 72 cards (should produce 3+ pages)
    cards_72 = []
    for i in range(72):
        yr = f"{10 + i // 4:02d}"
        qd = str(1 + i % 4)
        cards_72.append(make_card(yr, qd, "29744"))
    write_cards("page_break_three_pages", cards_72)

    # 39: page_break_with_errors - 30 cards, 25 valid + 5 invalid
    #     Errors at positions 5, 10, 15, 20, 25 (0-indexed)
    cards_30 = []
    error_positions = {5, 10, 15, 20, 25}
    for i in range(30):
        yr = f"{10 + i // 4:02d}"
        qd = str(1 + i % 4)
        if i in error_positions:
            cards_30.append(make_card(yr, qd, "XXXXX"))
        else:
            cards_30.append(make_card(yr, qd, "29744"))
    write_cards("page_break_with_errors", cards_30)

    # ================================================================
    # Business Scenario Tests (tests 40-43)
    # ================================================================

    # 40: business_annual_run - Standard 4-quad run for one year
    write_cards("business_annual_run", [
        make_card("25", "1", "29744"),
        make_card("25", "2", "31205"),
        make_card("25", "3", "25000"),
        make_card("25", "4", "28103"),
    ])

    # 41: business_multi_year - 3 years x 4 quads = 12 cards
    write_cards("business_multi_year", [
        make_card("23", "1", "28500"),
        make_card("23", "2", "30100"),
        make_card("23", "3", "24500"),
        make_card("23", "4", "27800"),
        make_card("24", "1", "29200"),
        make_card("24", "2", "30800"),
        make_card("24", "3", "25200"),
        make_card("24", "4", "28500"),
        make_card("25", "1", "29744"),
        make_card("25", "2", "31205"),
        make_card("25", "3", "25800"),
        make_card("25", "4", "28103"),
    ])

    # 42: business_data_entry_error - 3 valid + 1 typo + 1 valid
    write_cards("business_data_entry_error", [
        make_card("25", "1", "29744"),
        make_card("25", "2", "31205"),
        make_card("25", "3", "2S000"),  # Typo: S instead of 5
        make_card("25", "4", "28103"),  # Valid, still processed
        make_card("26", "1", "30000"),  # Valid
    ])

    # 43: business_factor_unity - Factor of exactly 1.0000
    write_cards("business_factor_unity", [make_card("25", "1", "10000")])

    # ================================================================
    # Output Format Tests (tests 44-48)
    # ================================================================

    # 44: factor_record_exact_bytes - 4 cards for byte-level factor verification
    write_cards("factor_record_exact_bytes", [
        make_card("25", "1", "29744"),
        make_card("25", "2", "10000"),
        make_card("25", "3", "99999"),
        make_card("25", "4", "00001"),
    ])

    # 45: print_detail_exact_layout - Single valid card for print layout check
    write_cards("print_detail_exact_layout", [make_card("25", "1", "29744")])

    # 46: print_header_exact_layout - Single valid card (for header verification)
    write_cards("print_header_exact_layout", [make_card("25", "1", "29744")])

    # 47: print_error_line_layout - Single invalid card for error line check
    write_cards("print_error_line_layout", [make_card("AB", "5", "XXXXX")])

    # 48: factor_display_formatting - 5 different factors to verify N.NNNN format
    write_cards("factor_display_formatting", [
        make_card("25", "1", "10000"),  # Should display "1.0000"
        make_card("25", "2", "00001"),  # Should display "0.0001"
        make_card("25", "3", "99999"),  # Should display "9.9999"
        make_card("25", "4", "29744"),  # Should display "2.9744"
        make_card("26", "1", "50000"),  # Should display "5.0000"
    ])

    # ================================================================
    # Single-Card Edge Cases (tests 49-50)
    # ================================================================

    # 49: single_valid_card - Minimum valid input (1 card)
    write_cards("single_valid_card", [make_card("25", "1", "29744")])

    # 50: single_invalid_card - Single invalid card
    write_cards("single_invalid_card", [make_card("XX", "9", "ABCDE")])

    print()
    print(f"Done. Generated 43 new test case input files.")
    print(f"Next: run 'python cobol_test_harness.py --generate-expected' to create baselines.")


if __name__ == "__main__":
    generate_all()
