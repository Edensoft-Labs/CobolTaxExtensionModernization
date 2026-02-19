#!/usr/bin/env python3
"""
COBOL Test Harness for CLREB020 - Equalization Factor Edit and List Program

This harness runs the compiled GnuCOBOL executable CLREB020.exe against a suite
of test cases and validates the output.

Usage:
    python Testing/CLREB020/cobol_test_harness.py [--generate-expected] [--verbose]

Options:
    --generate-expected   Run all tests and save outputs as expected baselines
    --verbose             Show detailed output for each test case

GnuCOBOL DD Name Mapping:
    The COBOL source uses SELECT...ASSIGN TO UT-S-CARDS, UT-S-FACTOR, UT-S-PRINT.
    GnuCOBOL strips the "UT-S-" prefix and uses the remainder as the environment
    variable name (with DD_ prefix). Therefore:
        DD_CARDS  -> CARD-FILE   (input, 80-byte fixed-length records)
        DD_FACTOR -> FACTOR-FILE (output, 21-byte fixed-length records)
        DD_PRINT  -> PRINT-FILE  (output, 133-char print records, line sequential)

File Format Notes:
    - Input cards are BINARY fixed-length 80-byte records with NO line terminators.
      GnuCOBOL reads them as RECORDING MODE F with RECORD CONTAINS 80 CHARACTERS.
    - Factor output is BINARY fixed-length 21-byte records with NO line terminators.
    - Print output uses GnuCOBOL's default line sequential mode with ASA carriage
      control characters converted to newlines and form feeds.

CLREB020 Program Behavior:
    - Reads 80-char card records from CARD-FILE
    - Card layout: pos 1-2 = CD-YR (year), pos 3 = CD-QUAD (1-4),
      pos 4-8 = CD-FACTOR (5 digits as 9V9999), pos 9-80 = filler spaces
    - Validates: year is numeric & >0, factor is numeric & >0, quad is 1-4
    - Sequence checks cards (ascending order on first 3 chars: year+quad)
    - Writes 21-char factor records to FACTOR-FILE
    - Writes 133-char print records to PRINT-FILE
    - Sets RETURN-CODE to 16 on sequence error
    - DISPLAYs input/output/error counts to console (stdout)

Note on zero-factor behavior:
    The COBOL code checks "CD-FACTOR GREATER THAN 0" using an alphanumeric
    comparison (CD-FACTOR is PIC X(5)). "00000" compared to "0" (space-padded
    to "0    ") is GREATER because '0' (0x30) > ' ' (0x20) in ASCII. Therefore,
    a factor of "00000" is treated as VALID by the original program. This is a
    faithful reproduction of the original mainframe behavior.
"""

import os
import sys
import subprocess
import tempfile
import shutil
import argparse
from pathlib import Path


# ============================================================================
# Configuration
# ============================================================================

# Resolve paths relative to this script's location
SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent

BUILD_DIR = PROJECT_ROOT / "Build"
EXE_PATH = BUILD_DIR / "output" / "CLREB020.exe"
TEST_CASES_DIR = SCRIPT_DIR / "test_cases"
EXPECTED_OUTPUT_DIR = SCRIPT_DIR / "expected_output"

# GnuCOBOL DD name environment variables (determined by experimentation)
# For SELECT CARD-FILE ASSIGN TO UT-S-CARDS, GnuCOBOL uses "CARDS"
# Environment variable convention: DD_<name>
DD_INPUT_VAR = "DD_CARDS"
DD_FACTOR_VAR = "DD_FACTOR"
DD_PRINT_VAR = "DD_PRINT"

# File names within each test case directory
INPUT_FILENAME = "input.dat"
FACTOR_FILENAME = "factor.dat"
PRINT_FILENAME = "print.dat"
STDOUT_FILENAME = "stdout.txt"
RETURN_CODE_FILENAME = "returncode.txt"

# Record sizes (bytes)
INPUT_RECORD_SIZE = 80
FACTOR_RECORD_SIZE = 21


# ============================================================================
# Test Case Definitions
# ============================================================================

TEST_CASES = [
    # === Existing Tests (1-7) ===
    {"name": "valid_basic", "description": "4 valid cards (year 25, quads 1-4, factor 29744)", "expect_rc": 0, "expect_factor_records": 4, "expect_error_count": 0},
    {"name": "valid_multipage", "description": "64 valid cards to trigger page breaks", "expect_rc": 0, "expect_factor_records": 64, "expect_error_count": 0},
    {"name": "invalid_not_numeric", "description": "Non-numeric factor 'ABCDE' and year 'AB'", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 2},
    {"name": "invalid_bad_quad", "description": "Quad = '5' (invalid, must be 1-4)", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_zero_factor", "description": "Factor '00000' accepted (alphanumeric quirk)", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "sequence_error", "description": "Two cards descending (252 then 251)", "expect_rc": 16, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "empty_input", "description": "Empty file (0 bytes)", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 0},
    # === Field Boundary Tests (8-17) ===
    {"name": "boundary_year_01", "description": "Lowest valid year '01'", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_year_99", "description": "Highest valid year '99'", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_year_00", "description": "Year '00' passes COBOL (alphanumeric quirk)", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_quad_1", "description": "Lower bound valid quad '1'", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_quad_4", "description": "Upper bound valid quad '4'", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_quad_0", "description": "Quad '0' just below valid range", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "boundary_quad_5", "description": "Quad '5' just above valid range", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "boundary_factor_00001", "description": "Minimum nonzero factor '00001'", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_factor_99999", "description": "Maximum factor '99999'", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_factor_10000", "description": "Factor '10000' = 1.0 (no equalization)", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    # === Validation Combinatorial Tests (18-26) ===
    {"name": "invalid_year_letters", "description": "Year 'AB' - pure non-numeric", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_year_mixed", "description": "Year '2A' - mixed numeric/alpha", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_year_spaces", "description": "Year '  ' - spaces fail NUMERIC", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_factor_mixed", "description": "Factor '29A44' - letter in digits", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_factor_spaces", "description": "Factor '2 744' - space in digits", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_quad_9", "description": "Quad '9' - numeric but out of range", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_quad_space", "description": "Quad ' ' - not in '1'..'4'", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_quad_letter", "description": "Quad 'A' - not in '1'..'4'", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_all_fields_bad", "description": "All fields invalid: 'XY', 'Z', 'ABCDE'", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    # === Sequence Tests (27-31) ===
    {"name": "sequence_ascending_all_quads", "description": "2 years x 4 quads ascending", "expect_rc": 0, "expect_factor_records": 8, "expect_error_count": 0},
    {"name": "sequence_year_transition", "description": "Cards spanning year boundary (24->25->26)", "expect_rc": 0, "expect_factor_records": 5, "expect_error_count": 0},
    {"name": "sequence_duplicate_key", "description": "Duplicate year+quad allowed (LESS THAN check)", "expect_rc": 0, "expect_factor_records": 3, "expect_error_count": 0},
    {"name": "sequence_error_after_valid", "description": "2 valid then out-of-sequence (RC=16)", "expect_rc": 16, "expect_factor_records": 2, "expect_error_count": 0},
    {"name": "sequence_error_immediate", "description": "Second card out of sequence (RC=16)", "expect_rc": 16, "expect_factor_records": 1, "expect_error_count": 0},
    # === Data-Flow and State Tests (32-34) ===
    {"name": "mixed_valid_invalid_stream", "description": "V,I,V,I,V,V - tests WK-MESG cleanup", "expect_rc": 0, "expect_factor_records": 4, "expect_error_count": 2},
    {"name": "error_message_cleanup", "description": "V-I-V: verify error msg doesn't bleed", "expect_rc": 0, "expect_factor_records": 2, "expect_error_count": 1},
    {"name": "counter_invariant", "description": "10 cards (7V+3I): verify IN=OUT+ERR", "expect_rc": 0, "expect_factor_records": 7, "expect_error_count": 3},
    # === Page Break Tests (35-39) ===
    {"name": "page_break_at_23_cards", "description": "23 cards - just below page break", "expect_rc": 0, "expect_factor_records": 23, "expect_error_count": 0},
    {"name": "page_break_at_24_cards", "description": "24 cards - exact page break trigger", "expect_rc": 0, "expect_factor_records": 24, "expect_error_count": 0},
    {"name": "page_break_at_25_cards", "description": "25 cards - one past page break", "expect_rc": 0, "expect_factor_records": 25, "expect_error_count": 0},
    {"name": "page_break_three_pages", "description": "72 cards - 3+ pages", "expect_rc": 0, "expect_factor_records": 72, "expect_error_count": 0},
    {"name": "page_break_with_errors", "description": "30 cards (25V+5I) with page breaks", "expect_rc": 0, "expect_factor_records": 25, "expect_error_count": 5},
    # === Business Scenario Tests (40-43) ===
    {"name": "business_annual_run", "description": "Standard annual run: yr 25, quads 1-4", "expect_rc": 0, "expect_factor_records": 4, "expect_error_count": 0},
    {"name": "business_multi_year", "description": "3 years x 4 quads = 12 cards", "expect_rc": 0, "expect_factor_records": 12, "expect_error_count": 0},
    {"name": "business_data_entry_error", "description": "5 cards: 4 valid + 1 typo", "expect_rc": 0, "expect_factor_records": 4, "expect_error_count": 1},
    {"name": "business_factor_unity", "description": "Factor '10000' = 1.0000", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    # === Output Format Tests (44-48) ===
    {"name": "factor_record_exact_bytes", "description": "4 cards for byte-level factor check", "expect_rc": 0, "expect_factor_records": 4, "expect_error_count": 0},
    {"name": "print_detail_exact_layout", "description": "1 valid card for print layout check", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "print_header_exact_layout", "description": "1 valid card for header verification", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "print_error_line_layout", "description": "1 invalid card for error line check", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "factor_display_formatting", "description": "5 factors for N.NNNN display check", "expect_rc": 0, "expect_factor_records": 5, "expect_error_count": 0},
    # === Single-Card Edge Cases (49-50) ===
    {"name": "single_valid_card", "description": "Minimum valid: 1 card", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "single_invalid_card", "description": "Single invalid card (all fields bad)", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
]


# ============================================================================
# Helper Functions
# ============================================================================

def parse_stdout_counts(stdout_text):
    """Parse the DISPLAY output from CLREB020 to extract counts.

    Expected format:
        NO. OF INPUT RECORDS  = +NNN
        NO. OF OUTPUT RECORDS = +NNN
        NO. OF ERROR RECORDS  = +NNN

    Returns dict with keys: input_count, output_count, error_count.
    Returns None for any value that cannot be parsed.
    """
    counts = {"input_count": None, "output_count": None, "error_count": None}
    for line in stdout_text.splitlines():
        line = line.strip()
        if "INPUT RECORDS" in line and "=" in line:
            try:
                val = line.split("=")[1].strip().lstrip("+")
                counts["input_count"] = int(val)
            except (ValueError, IndexError):
                pass
        elif "OUTPUT RECORDS" in line and "=" in line:
            try:
                val = line.split("=")[1].strip().lstrip("+")
                counts["output_count"] = int(val)
            except (ValueError, IndexError):
                pass
        elif "ERROR RECORDS" in line and "=" in line:
            try:
                val = line.split("=")[1].strip().lstrip("+")
                counts["error_count"] = int(val)
            except (ValueError, IndexError):
                pass
    return counts


def count_factor_records(factor_file_path):
    """Count the number of 21-byte records in the factor file."""
    if not os.path.exists(factor_file_path):
        return 0
    size = os.path.getsize(factor_file_path)
    if size == 0:
        return 0
    if size % FACTOR_RECORD_SIZE != 0:
        return -1  # Indicates corrupt file
    return size // FACTOR_RECORD_SIZE


def files_match(file1, file2):
    """Compare two files byte-for-byte. Returns True if identical."""
    if not os.path.exists(file1) and not os.path.exists(file2):
        return True
    if not os.path.exists(file1) or not os.path.exists(file2):
        # One exists and the other doesn't -- check if both are effectively empty
        existing = file1 if os.path.exists(file1) else file2
        return os.path.getsize(existing) == 0
    with open(file1, "rb") as f1, open(file2, "rb") as f2:
        return f1.read() == f2.read()


def text_files_match(file1, file2):
    """Compare two text files, ignoring the date line (which changes daily).

    The print file contains a date line with the current date (YYYYMMDD format).
    We strip that dynamic content before comparison so tests are repeatable.
    """
    def normalize_print_content(filepath):
        if not os.path.exists(filepath):
            return ""
        with open(filepath, "r", errors="replace") as f:
            lines = f.readlines()
        normalized = []
        for line in lines:
            stripped = line.rstrip()
            # The date line starts with form-feed + spaces + 8-digit date
            # e.g., "\x0c           20260211"
            # Replace the date portion with a placeholder
            if stripped and stripped[0] == "\x0c" and len(stripped) >= 20:
                # Replace the 8-char date at positions 12-19 with "XXXXXXXX"
                normalized.append(stripped[:12] + "XXXXXXXX" + stripped[20:])
            else:
                normalized.append(stripped)
        return "\n".join(normalized)

    content1 = normalize_print_content(file1)
    content2 = normalize_print_content(file2)
    return content1 == content2


# ============================================================================
# Test Runner
# ============================================================================

def run_test(test_case, verbose=False):
    """Run a single test case and return results.

    Returns a dict with:
        passed: bool
        failures: list of failure reason strings
        return_code: int
        stdout: str
        stderr: str
        factor_records: int
        counts: dict from parse_stdout_counts
    """
    name = test_case["name"]
    input_path = TEST_CASES_DIR / name / INPUT_FILENAME
    expected_dir = EXPECTED_OUTPUT_DIR / name

    results = {
        "passed": True,
        "failures": [],
        "return_code": None,
        "stdout": "",
        "stderr": "",
        "factor_records": 0,
        "counts": {},
    }

    # Verify input exists
    if not input_path.exists():
        results["passed"] = False
        results["failures"].append(f"Input file not found: {input_path}")
        return results

    # Create temp directory for output files
    tmpdir = tempfile.mkdtemp(prefix=f"clreb020_test_{name}_")
    factor_path = os.path.join(tmpdir, FACTOR_FILENAME)
    print_path = os.path.join(tmpdir, PRINT_FILENAME)

    try:
        # Build environment with GnuCOBOL DLL path and DD mappings
        env = os.environ.copy()
        env["PATH"] = str(BUILD_DIR) + os.pathsep + env.get("PATH", "")
        env[DD_INPUT_VAR] = str(input_path)
        env[DD_FACTOR_VAR] = factor_path
        env[DD_PRINT_VAR] = print_path

        # Run the executable
        try:
            result = subprocess.run(
                [str(EXE_PATH)],
                env=env,
                capture_output=True,
                text=True,
                timeout=30,
                cwd=tmpdir,
            )
        except subprocess.TimeoutExpired:
            results["passed"] = False
            results["failures"].append("Execution timed out (30 seconds)")
            return results
        except FileNotFoundError:
            results["passed"] = False
            results["failures"].append(f"Executable not found: {EXE_PATH}")
            return results

        results["return_code"] = result.returncode
        results["stdout"] = result.stdout
        results["stderr"] = result.stderr

        # Parse DISPLAY counts from stdout
        results["counts"] = parse_stdout_counts(result.stdout)

        # Count factor records
        results["factor_records"] = count_factor_records(factor_path)

        # ---- Validation checks ----

        # 1. Check return code
        if test_case.get("expect_rc") is not None:
            if result.returncode != test_case["expect_rc"]:
                results["passed"] = False
                results["failures"].append(
                    f"Return code: expected {test_case['expect_rc']}, "
                    f"got {result.returncode}"
                )

        # 2. Check stderr (should be empty for successful runs)
        if result.stderr.strip():
            results["passed"] = False
            results["failures"].append(f"Unexpected stderr: {result.stderr.strip()[:200]}")

        # 3. Check factor record count
        if test_case.get("expect_factor_records") is not None:
            actual_records = results["factor_records"]
            expected_records = test_case["expect_factor_records"]
            if actual_records != expected_records:
                results["passed"] = False
                results["failures"].append(
                    f"Factor records: expected {expected_records}, "
                    f"got {actual_records}"
                )

        # 4. Check error count from DISPLAY output
        if test_case.get("expect_error_count") is not None:
            actual_errors = results["counts"].get("error_count")
            expected_errors = test_case["expect_error_count"]
            if actual_errors != expected_errors:
                results["passed"] = False
                results["failures"].append(
                    f"Error count: expected {expected_errors}, "
                    f"got {actual_errors}"
                )

        # 5. Compare against expected output files (if they exist)
        expected_factor = expected_dir / FACTOR_FILENAME
        expected_print = expected_dir / PRINT_FILENAME
        expected_stdout = expected_dir / STDOUT_FILENAME
        expected_rc_file = expected_dir / RETURN_CODE_FILENAME

        if expected_factor.exists():
            if not files_match(factor_path, str(expected_factor)):
                results["passed"] = False
                results["failures"].append(
                    "Factor file does not match expected output"
                )

        if expected_print.exists():
            if not text_files_match(print_path, str(expected_print)):
                results["passed"] = False
                results["failures"].append(
                    "Print file does not match expected output (ignoring date)"
                )

        if expected_stdout.exists():
            with open(expected_stdout, "r") as f:
                expected_stdout_text = f.read()
            # Compare stdout ignoring leading/trailing whitespace on each line
            actual_lines = [l.strip() for l in result.stdout.splitlines()]
            expected_lines = [l.strip() for l in expected_stdout_text.splitlines()]
            if actual_lines != expected_lines:
                results["passed"] = False
                results["failures"].append(
                    "Stdout does not match expected output"
                )

        if expected_rc_file.exists():
            with open(expected_rc_file, "r") as f:
                expected_rc = int(f.read().strip())
            if result.returncode != expected_rc:
                results["passed"] = False
                results["failures"].append(
                    f"Return code file mismatch: expected {expected_rc}, "
                    f"got {result.returncode}"
                )

    finally:
        # Clean up temp directory
        try:
            shutil.rmtree(tmpdir)
        except OSError:
            pass

    return results


def generate_expected_outputs(verbose=False):
    """Run all test cases and save the outputs as expected baselines."""
    print("=" * 70)
    print("GENERATING EXPECTED OUTPUT BASELINES")
    print("=" * 70)
    print()

    for test_case in TEST_CASES:
        name = test_case["name"]
        input_path = TEST_CASES_DIR / name / INPUT_FILENAME
        expected_dir = EXPECTED_OUTPUT_DIR / name

        print(f"  Generating: {name}...")

        if not input_path.exists():
            print(f"    SKIPPED - Input file not found: {input_path}")
            continue

        # Create expected output directory
        expected_dir.mkdir(parents=True, exist_ok=True)

        # Create temp output files, then copy to expected
        tmpdir = tempfile.mkdtemp(prefix=f"clreb020_gen_{name}_")
        factor_path = os.path.join(tmpdir, FACTOR_FILENAME)
        print_path = os.path.join(tmpdir, PRINT_FILENAME)

        try:
            env = os.environ.copy()
            env["PATH"] = str(BUILD_DIR) + os.pathsep + env.get("PATH", "")
            env[DD_INPUT_VAR] = str(input_path)
            env[DD_FACTOR_VAR] = factor_path
            env[DD_PRINT_VAR] = print_path

            result = subprocess.run(
                [str(EXE_PATH)],
                env=env,
                capture_output=True,
                text=True,
                timeout=30,
                cwd=tmpdir,
            )

            # Save factor file
            if os.path.exists(factor_path) and os.path.getsize(factor_path) > 0:
                shutil.copy2(factor_path, str(expected_dir / FACTOR_FILENAME))
                size = os.path.getsize(factor_path)
                print(f"    Saved factor.dat ({size} bytes, "
                      f"{size // FACTOR_RECORD_SIZE} records)")
            else:
                # Write an empty file to indicate expected empty output
                with open(str(expected_dir / FACTOR_FILENAME), "wb") as f:
                    pass
                print(f"    Saved factor.dat (0 bytes, empty)")

            # Save print file
            if os.path.exists(print_path) and os.path.getsize(print_path) > 0:
                shutil.copy2(print_path, str(expected_dir / PRINT_FILENAME))
                size = os.path.getsize(print_path)
                print(f"    Saved print.dat ({size} bytes)")
            else:
                with open(str(expected_dir / PRINT_FILENAME), "wb") as f:
                    pass
                print(f"    Saved print.dat (0 bytes, empty)")

            # Save stdout
            with open(str(expected_dir / STDOUT_FILENAME), "w") as f:
                f.write(result.stdout)
            print(f"    Saved stdout.txt")

            # Save return code
            with open(str(expected_dir / RETURN_CODE_FILENAME), "w") as f:
                f.write(str(result.returncode))
            print(f"    Saved returncode.txt (RC={result.returncode})")

            if result.stderr.strip():
                print(f"    WARNING: stderr: {result.stderr.strip()[:200]}")

            if verbose:
                print(f"    Stdout: {repr(result.stdout[:200])}")
                counts = parse_stdout_counts(result.stdout)
                print(f"    Counts: {counts}")

        finally:
            try:
                shutil.rmtree(tmpdir)
            except OSError:
                pass

        print()

    print("Expected output generation complete.")
    print()


def run_all_tests(verbose=False):
    """Run all test cases and report results."""
    print("=" * 70)
    print("CLREB020 COBOL TEST HARNESS")
    print("=" * 70)
    print()
    print(f"Executable:    {EXE_PATH}")
    print(f"Test cases:    {TEST_CASES_DIR}")
    print(f"Expected dir:  {EXPECTED_OUTPUT_DIR}")
    print(f"Build DLLs:    {BUILD_DIR}")
    print()

    # Verify executable exists
    if not EXE_PATH.exists():
        print(f"ERROR: Executable not found: {EXE_PATH}")
        print("Please build CLREB020.exe first.")
        sys.exit(1)

    # Check if expected outputs exist
    has_expected = any(
        (EXPECTED_OUTPUT_DIR / tc["name"] / FACTOR_FILENAME).exists()
        for tc in TEST_CASES
    )
    if not has_expected:
        print("WARNING: No expected output files found.")
        print("Run with --generate-expected to create baselines first.")
        print()

    total = len(TEST_CASES)
    passed = 0
    failed = 0
    errors = []

    print("-" * 70)

    for test_case in TEST_CASES:
        name = test_case["name"]
        desc = test_case["description"]

        result = run_test(test_case, verbose=verbose)

        if result["passed"]:
            status = "PASS"
            passed += 1
        else:
            status = "FAIL"
            failed += 1
            errors.append((name, result["failures"]))

        # Format output line
        rc_str = f"RC={result['return_code']}" if result["return_code"] is not None else "RC=?"
        counts = result["counts"]
        count_str = ""
        if counts.get("input_count") is not None:
            count_str = (
                f"in={counts['input_count']} "
                f"out={counts.get('output_count', '?')} "
                f"err={counts.get('error_count', '?')}"
            )

        print(f"  [{status}] {name:<25} {rc_str:<8} {count_str}")

        if verbose:
            print(f"         {desc}")
            if result["stdout"]:
                for line in result["stdout"].strip().splitlines():
                    print(f"         stdout: {line}")
            if result["stderr"]:
                for line in result["stderr"].strip().splitlines():
                    print(f"         stderr: {line}")
            if result["failures"]:
                for failure in result["failures"]:
                    print(f"         FAILURE: {failure}")
            print()

    print("-" * 70)
    print()
    print(f"Results: {passed} passed, {failed} failed, {total} total")
    print()

    if errors:
        print("FAILURES:")
        print()
        for name, failures in errors:
            print(f"  {name}:")
            for failure in failures:
                print(f"    - {failure}")
            print()

    return 0 if failed == 0 else 1


# ============================================================================
# Main Entry Point
# ============================================================================

def main():
    parser = argparse.ArgumentParser(
        description="COBOL Test Harness for CLREB020 - Equalization Factor Editor"
    )
    parser.add_argument(
        "--generate-expected",
        action="store_true",
        help="Run all tests and save outputs as expected baselines",
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Show detailed output for each test case",
    )
    args = parser.parse_args()

    if args.generate_expected:
        generate_expected_outputs(verbose=args.verbose)
        # Also run the tests after generating to show current status
        print()

    exit_code = run_all_tests(verbose=args.verbose)
    sys.exit(exit_code)


if __name__ == "__main__":
    main()
