#!/usr/bin/env python3
"""
Java Test Harness for CLREB020 - Equalization Factor Edit and List Program

Runs the Java conversion (tax-extension JAR) against the full test suite and
validates outputs against Java-specific expected baselines.

This is the Java-side equivalent of cobol_test_harness.py. It tests the Java
implementation independently, without needing the COBOL executable.

Usage:
    python Testing/CLREB020/java_test_harness.py [--generate-expected] [--verbose]

Previously Known COBOL/Java Differences (now resolved):
    - invalid_zero_factor and boundary_year_00 previously differed because Java
      used numeric > 0 comparison while COBOL uses alphanumeric comparison.
    - Fixed by changing Java validation to use >= 0 (COBOL-compatible default).
    - The Java-specific baselines (expected_output_java/) are no longer needed.
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

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent

JAVA_DIR = PROJECT_ROOT / "tax_extension_java"
JAVA_JAR = JAVA_DIR / "target" / "tax-extension-0.1.0-SNAPSHOT.jar"
TEST_CASES_DIR = SCRIPT_DIR / "test_cases"
EXPECTED_OUTPUT_DIR = SCRIPT_DIR / "expected_output"
EXPECTED_OUTPUT_JAVA_DIR = SCRIPT_DIR / "expected_output_java"

# File names within each test case directory
INPUT_FILENAME = "input.dat"
FACTOR_FILENAME = "factor.dat"
PRINT_FILENAME = "print.dat"
STDOUT_FILENAME = "stdout.txt"
RETURN_CODE_FILENAME = "returncode.txt"

FACTOR_RECORD_SIZE = 21

# Tests with known COBOL/Java behavioral differences.
# These use expected_output_java/ instead of expected_output/.
JAVA_SPECIFIC_TESTS = {
}


# ============================================================================
# Test Case Definitions (same 50 tests as COBOL harness, with Java expectations)
# ============================================================================

TEST_CASES = [
    # === Existing Tests (1-7) ===
    {"name": "valid_basic", "description": "4 valid cards", "expect_rc": 0, "expect_factor_records": 4, "expect_error_count": 0},
    {"name": "valid_multipage", "description": "64 valid cards, page breaks", "expect_rc": 0, "expect_factor_records": 64, "expect_error_count": 0},
    {"name": "invalid_not_numeric", "description": "Non-numeric year and factor", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 2},
    {"name": "invalid_bad_quad", "description": "Quad = '5'", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_zero_factor", "description": "Factor '00000' accepted (COBOL-compatible)", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "sequence_error", "description": "Descending order (RC=16)", "expect_rc": 16, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "empty_input", "description": "Empty file", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 0},
    # === Field Boundary Tests (8-17) ===
    {"name": "boundary_year_01", "description": "Year '01'", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_year_99", "description": "Year '99'", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_year_00", "description": "Year '00' accepted (COBOL-compatible)", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_quad_1", "description": "Quad '1'", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_quad_4", "description": "Quad '4'", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_quad_0", "description": "Quad '0' invalid", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "boundary_quad_5", "description": "Quad '5' invalid", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "boundary_factor_00001", "description": "Factor '00001'", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_factor_99999", "description": "Factor '99999'", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "boundary_factor_10000", "description": "Factor '10000' = 1.0", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    # === Validation Combinatorial Tests (18-26) ===
    {"name": "invalid_year_letters", "description": "Year 'AB'", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_year_mixed", "description": "Year '2A'", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_year_spaces", "description": "Year '  '", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_factor_mixed", "description": "Factor '29A44'", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_factor_spaces", "description": "Factor '2 744'", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_quad_9", "description": "Quad '9'", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_quad_space", "description": "Quad ' '", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_quad_letter", "description": "Quad 'A'", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "invalid_all_fields_bad", "description": "All fields invalid", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    # === Sequence Tests (27-31) ===
    {"name": "sequence_ascending_all_quads", "description": "8 cards ascending", "expect_rc": 0, "expect_factor_records": 8, "expect_error_count": 0},
    {"name": "sequence_year_transition", "description": "Year boundary", "expect_rc": 0, "expect_factor_records": 5, "expect_error_count": 0},
    {"name": "sequence_duplicate_key", "description": "Duplicate key allowed", "expect_rc": 0, "expect_factor_records": 3, "expect_error_count": 0},
    {"name": "sequence_error_after_valid", "description": "Seq error after valid", "expect_rc": 16, "expect_factor_records": 2, "expect_error_count": 0},
    {"name": "sequence_error_immediate", "description": "Immediate seq error", "expect_rc": 16, "expect_factor_records": 1, "expect_error_count": 0},
    # === Data-Flow and State Tests (32-34) ===
    {"name": "mixed_valid_invalid_stream", "description": "V,I,V,I,V,V", "expect_rc": 0, "expect_factor_records": 4, "expect_error_count": 2},
    {"name": "error_message_cleanup", "description": "V-I-V cleanup", "expect_rc": 0, "expect_factor_records": 2, "expect_error_count": 1},
    {"name": "counter_invariant", "description": "10 cards 7V+3I", "expect_rc": 0, "expect_factor_records": 7, "expect_error_count": 3},
    # === Page Break Tests (35-39) ===
    {"name": "page_break_at_23_cards", "description": "23 cards", "expect_rc": 0, "expect_factor_records": 23, "expect_error_count": 0},
    {"name": "page_break_at_24_cards", "description": "24 cards, page break", "expect_rc": 0, "expect_factor_records": 24, "expect_error_count": 0},
    {"name": "page_break_at_25_cards", "description": "25 cards", "expect_rc": 0, "expect_factor_records": 25, "expect_error_count": 0},
    {"name": "page_break_three_pages", "description": "72 cards, 3+ pages", "expect_rc": 0, "expect_factor_records": 72, "expect_error_count": 0},
    {"name": "page_break_with_errors", "description": "30 cards 25V+5I", "expect_rc": 0, "expect_factor_records": 25, "expect_error_count": 5},
    # === Business Scenario Tests (40-43) ===
    {"name": "business_annual_run", "description": "Annual run", "expect_rc": 0, "expect_factor_records": 4, "expect_error_count": 0},
    {"name": "business_multi_year", "description": "3 years", "expect_rc": 0, "expect_factor_records": 12, "expect_error_count": 0},
    {"name": "business_data_entry_error", "description": "Typo in data", "expect_rc": 0, "expect_factor_records": 4, "expect_error_count": 1},
    {"name": "business_factor_unity", "description": "Factor 1.0", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    # === Output Format Tests (44-48) ===
    {"name": "factor_record_exact_bytes", "description": "Byte-level factor", "expect_rc": 0, "expect_factor_records": 4, "expect_error_count": 0},
    {"name": "print_detail_exact_layout", "description": "Print layout", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "print_header_exact_layout", "description": "Header layout", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "print_error_line_layout", "description": "Error line", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
    {"name": "factor_display_formatting", "description": "N.NNNN display", "expect_rc": 0, "expect_factor_records": 5, "expect_error_count": 0},
    # === Single-Card Edge Cases (49-50) ===
    {"name": "single_valid_card", "description": "1 valid card", "expect_rc": 0, "expect_factor_records": 1, "expect_error_count": 0},
    {"name": "single_invalid_card", "description": "1 invalid card", "expect_rc": 0, "expect_factor_records": 0, "expect_error_count": 1},
]


# ============================================================================
# Helper Functions
# ============================================================================

def parse_stdout_counts(stdout_text):
    """Parse DISPLAY output to extract counts."""
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
    """Count 21-byte records in factor file."""
    if not os.path.exists(factor_file_path):
        return 0
    size = os.path.getsize(factor_file_path)
    if size == 0:
        return 0
    if size % FACTOR_RECORD_SIZE != 0:
        return -1
    return size // FACTOR_RECORD_SIZE


def files_match(file1, file2):
    """Compare two files byte-for-byte."""
    if not os.path.exists(file1) and not os.path.exists(file2):
        return True
    if not os.path.exists(file1) or not os.path.exists(file2):
        existing = file1 if os.path.exists(file1) else file2
        return os.path.getsize(existing) == 0
    with open(file1, "rb") as f1, open(file2, "rb") as f2:
        return f1.read() == f2.read()


def text_files_match(file1, file2):
    """Compare print files, masking the date line."""
    def normalize_print_content(filepath):
        if not os.path.exists(filepath):
            return ""
        with open(filepath, "r", errors="replace") as f:
            lines = f.readlines()
        normalized = []
        for line in lines:
            stripped = line.rstrip()
            if stripped and stripped[0] == "\x0c" and len(stripped) >= 20:
                normalized.append(stripped[:12] + "XXXXXXXX" + stripped[20:])
            else:
                normalized.append(stripped)
        return "\n".join(normalized)

    return normalize_print_content(file1) == normalize_print_content(file2)


def get_expected_dir(test_name):
    """Return the correct expected output directory for this test.

    Tests with known COBOL/Java differences use expected_output_java/.
    All others use expected_output/ (same baselines as COBOL).
    """
    if test_name in JAVA_SPECIFIC_TESTS:
        java_dir = EXPECTED_OUTPUT_JAVA_DIR / test_name
        if java_dir.exists():
            return java_dir
    return EXPECTED_OUTPUT_DIR / test_name


# ============================================================================
# Test Runner
# ============================================================================

def run_test(test_case, verbose=False):
    """Run a single test case via Java JAR."""
    name = test_case["name"]
    input_path = TEST_CASES_DIR / name / INPUT_FILENAME
    expected_dir = get_expected_dir(name)
    is_java_specific = name in JAVA_SPECIFIC_TESTS

    results = {
        "passed": True,
        "failures": [],
        "return_code": None,
        "stdout": "",
        "stderr": "",
        "factor_records": 0,
        "counts": {},
        "java_specific": is_java_specific,
    }

    if not input_path.exists():
        results["passed"] = False
        results["failures"].append(f"Input file not found: {input_path}")
        return results

    tmpdir = tempfile.mkdtemp(prefix=f"clreb020_java_{name}_")
    factor_path = os.path.join(tmpdir, FACTOR_FILENAME)
    print_path = os.path.join(tmpdir, PRINT_FILENAME)

    try:
        try:
            result = subprocess.run(
                ["java", "-jar", str(JAVA_JAR),
                 str(input_path), print_path, factor_path],
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
            results["failures"].append("Java runtime (java) not found")
            return results

        results["return_code"] = result.returncode
        results["stdout"] = result.stdout
        results["stderr"] = result.stderr
        results["counts"] = parse_stdout_counts(result.stdout)
        results["factor_records"] = count_factor_records(factor_path)

        # 1. Check return code
        if test_case.get("expect_rc") is not None:
            if result.returncode != test_case["expect_rc"]:
                results["passed"] = False
                results["failures"].append(
                    f"Return code: expected {test_case['expect_rc']}, got {result.returncode}")

        # 2. Check stderr
        if result.stderr.strip():
            results["passed"] = False
            results["failures"].append(f"Unexpected stderr: {result.stderr.strip()[:200]}")

        # 3. Check factor record count
        if test_case.get("expect_factor_records") is not None:
            actual = results["factor_records"]
            expected = test_case["expect_factor_records"]
            if actual != expected:
                results["passed"] = False
                results["failures"].append(f"Factor records: expected {expected}, got {actual}")

        # 4. Check error count
        if test_case.get("expect_error_count") is not None:
            actual = results["counts"].get("error_count")
            expected = test_case["expect_error_count"]
            if actual != expected:
                results["passed"] = False
                results["failures"].append(f"Error count: expected {expected}, got {actual}")

        # 5. Compare against expected output files
        expected_factor = expected_dir / FACTOR_FILENAME
        expected_print = expected_dir / PRINT_FILENAME
        expected_stdout = expected_dir / STDOUT_FILENAME
        expected_rc_file = expected_dir / RETURN_CODE_FILENAME

        if expected_factor.exists():
            if not files_match(factor_path, str(expected_factor)):
                results["passed"] = False
                results["failures"].append("Factor file does not match expected output")

        if expected_print.exists():
            if not text_files_match(print_path, str(expected_print)):
                results["passed"] = False
                results["failures"].append("Print file does not match expected output (ignoring date)")

        if expected_stdout.exists():
            with open(expected_stdout, "r") as f:
                expected_stdout_text = f.read()
            # Compare stdout semantically: COBOL uses COMP-3 sign format
            # (e.g., "+004") while Java uses plain int (e.g., "4").
            # Parse counts from both and compare numeric values.
            expected_counts = parse_stdout_counts(expected_stdout_text)
            actual_counts = results["counts"]
            for key in ["input_count", "output_count", "error_count"]:
                exp_val = expected_counts.get(key)
                act_val = actual_counts.get(key)
                if exp_val is not None and act_val is not None:
                    if exp_val != act_val:
                        results["passed"] = False
                        results["failures"].append(
                            f"Stdout {key}: expected {exp_val}, got {act_val}")
                elif exp_val is not None and act_val is None:
                    results["passed"] = False
                    results["failures"].append(f"Stdout missing {key}")
            # For sequence error tests, verify diagnostic messages are present
            if "CARDS OUT OF SEQUENCE" in expected_stdout_text:
                if "CARDS OUT OF SEQUENCE" not in result.stdout:
                    results["passed"] = False
                    results["failures"].append(
                        "Stdout missing 'CARDS OUT OF SEQUENCE' message")
                if "CURRENT CARD" in expected_stdout_text:
                    if "CURRENT CARD" not in result.stdout:
                        results["passed"] = False
                        results["failures"].append(
                            "Stdout missing 'CURRENT CARD' message")

        if expected_rc_file.exists():
            with open(expected_rc_file, "r") as f:
                expected_rc = int(f.read().strip())
            if result.returncode != expected_rc:
                results["passed"] = False
                results["failures"].append(
                    f"Return code file mismatch: expected {expected_rc}, got {result.returncode}")

    finally:
        try:
            shutil.rmtree(tmpdir)
        except OSError:
            pass

    return results


def generate_expected_outputs(verbose=False):
    """Run Java-specific tests and save Java-specific baselines."""
    print("=" * 70)
    print("GENERATING JAVA-SPECIFIC EXPECTED OUTPUT BASELINES")
    print("=" * 70)
    print()
    print("Only generating baselines for tests with known COBOL/Java differences.")
    print(f"Other tests use the COBOL baselines in expected_output/.")
    print()

    for test_case in TEST_CASES:
        name = test_case["name"]
        if name not in JAVA_SPECIFIC_TESTS:
            continue

        input_path = TEST_CASES_DIR / name / INPUT_FILENAME
        expected_dir = EXPECTED_OUTPUT_JAVA_DIR / name

        print(f"  Generating: {name}...")
        print(f"    Reason: {JAVA_SPECIFIC_TESTS[name]}")

        if not input_path.exists():
            print(f"    SKIPPED - Input file not found: {input_path}")
            continue

        expected_dir.mkdir(parents=True, exist_ok=True)

        tmpdir = tempfile.mkdtemp(prefix=f"clreb020_jgen_{name}_")
        factor_path = os.path.join(tmpdir, FACTOR_FILENAME)
        print_path = os.path.join(tmpdir, PRINT_FILENAME)

        try:
            result = subprocess.run(
                ["java", "-jar", str(JAVA_JAR),
                 str(input_path), print_path, factor_path],
                capture_output=True,
                text=True,
                timeout=30,
                cwd=tmpdir,
            )

            # Save factor file
            if os.path.exists(factor_path) and os.path.getsize(factor_path) > 0:
                shutil.copy2(factor_path, str(expected_dir / FACTOR_FILENAME))
                size = os.path.getsize(factor_path)
                print(f"    Saved factor.dat ({size} bytes, {size // FACTOR_RECORD_SIZE} records)")
            else:
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

            if verbose:
                print(f"    Stdout: {repr(result.stdout[:200])}")

        finally:
            try:
                shutil.rmtree(tmpdir)
            except OSError:
                pass

        print()

    print("Java-specific baseline generation complete.")
    print()


def run_all_tests(verbose=False):
    """Run all 50 test cases via Java and report results."""
    print("=" * 70)
    print("CLREB020 JAVA TEST HARNESS")
    print("=" * 70)
    print()
    print(f"Java JAR:      {JAVA_JAR}")
    print(f"Test cases:    {TEST_CASES_DIR}")
    print(f"Expected:      {EXPECTED_OUTPUT_DIR}")
    print(f"Java-specific: {EXPECTED_OUTPUT_JAVA_DIR}")
    print()

    if not JAVA_JAR.exists():
        print(f"ERROR: JAR not found: {JAVA_JAR}")
        print("Build with: cd tax_extension_java && mvn clean package -q")
        sys.exit(1)

    # Check java is available
    try:
        subprocess.run(["java", "-version"], capture_output=True, timeout=10)
    except (FileNotFoundError, subprocess.TimeoutExpired):
        print("ERROR: Java runtime not found. Install JDK 17+.")
        sys.exit(1)

    total = len(TEST_CASES)
    passed = 0
    failed = 0
    java_specific_count = 0
    errors = []

    print("-" * 70)

    for test_case in TEST_CASES:
        name = test_case["name"]
        result = run_test(test_case, verbose=verbose)

        if result["passed"]:
            if result["java_specific"]:
                status = "JSPC"
                java_specific_count += 1
            else:
                status = "PASS"
            passed += 1
        else:
            status = "FAIL"
            failed += 1
            errors.append((name, result["failures"]))

        rc_str = f"RC={result['return_code']}" if result["return_code"] is not None else "RC=?"
        counts = result["counts"]
        count_str = ""
        if counts.get("input_count") is not None:
            count_str = (
                f"in={counts['input_count']} "
                f"out={counts.get('output_count', '?')} "
                f"err={counts.get('error_count', '?')}"
            )

        print(f"  [{status:4s}] {name:<25} {rc_str:<8} {count_str}")

        if verbose and result["failures"]:
            for failure in result["failures"]:
                print(f"         FAILURE: {failure}")

    print("-" * 70)
    print()
    print(f"Results: {passed} passed, {failed} failed, "
          f"{java_specific_count} java-specific baselines, {total} total")
    print()

    if java_specific_count > 0:
        print("JAVA-SPECIFIC TESTS (using Java baselines, not COBOL):")
        for tc_name, desc in JAVA_SPECIFIC_TESTS.items():
            print(f"  {tc_name}: {desc}")
        print()

    if errors:
        print("FAILURES:")
        for name, failures in errors:
            print(f"  {name}:")
            for failure in failures:
                print(f"    - {failure}")
            print()

    return 0 if failed == 0 else 1


def main():
    parser = argparse.ArgumentParser(
        description="Java Test Harness for CLREB020 - Equalization Factor Editor"
    )
    parser.add_argument(
        "--generate-expected",
        action="store_true",
        help="Run Java-specific tests and save Java baselines",
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Show detailed output for each test case",
    )
    args = parser.parse_args()

    if args.generate_expected:
        generate_expected_outputs(verbose=args.verbose)
        print()

    exit_code = run_all_tests(verbose=args.verbose)
    sys.exit(exit_code)


if __name__ == "__main__":
    main()
