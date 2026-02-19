#!/usr/bin/env python3
"""
COBOL-vs-Java Integration Test for CLREB020

Runs both the compiled COBOL executable (CLREB020.exe) and the Java conversion
(tax-extension JAR) against the same test inputs, then compares their outputs
to verify byte-identical behavior.

Usage:
    python Testing/CLREB020/integration_test.py [--verbose] [--java-only] [--cobol-only]

Prerequisites:
    - COBOL: Build/output/CLREB020.exe (compile with Build/build.py --compile)
    - Java:  tax_extension_java/target/tax-extension-0.1.0-SNAPSHOT.jar
             (build with: cd tax_extension_java && mvn clean package -q)
    - Python 3.8+

Known Behavioral Differences (COBOL vs Java):
    1. Zero-factor handling: COBOL accepts "00000" as valid (alphanumeric comparison
       "00000" > "0    " is TRUE because '0' > ' '). Java treats "00000" as invalid
       (numeric parse → 0, which is not > 0). The COBOL behavior is a quirk of
       alphanumeric comparison; the Java behavior is arguably more correct but differs
       from the original.
    2. Date format: Both use YYYYMMDD but the date is the current date, so outputs
       will match if run on the same day. Date fields are masked in comparisons.
"""

import os
import sys
import subprocess
import tempfile
import shutil
import argparse
import re
from pathlib import Path
from typing import Optional


# ============================================================================
# Configuration
# ============================================================================

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent

BUILD_DIR = PROJECT_ROOT / "Build"
COBOL_EXE = BUILD_DIR / "output" / "CLREB020.exe"

JAVA_DIR = PROJECT_ROOT / "tax_extension_java"
JAVA_JAR = JAVA_DIR / "target" / "tax-extension-0.1.0-SNAPSHOT.jar"

TEST_CASES_DIR = SCRIPT_DIR / "test_cases"

# GnuCOBOL DD name environment variables
DD_INPUT_VAR = "DD_CARDS"
DD_FACTOR_VAR = "DD_FACTOR"
DD_PRINT_VAR = "DD_PRINT"

# File names
FACTOR_FILENAME = "factor.dat"
PRINT_FILENAME = "print.dat"

# Known differences between COBOL and Java (test_case_name → description)
KNOWN_DIFFERENCES = {
}


# ============================================================================
# Test Case Definitions (all 50 tests)
# ============================================================================

TEST_CASES = [
    # Existing (1-7)
    {"name": "valid_basic", "description": "4 valid cards"},
    {"name": "valid_multipage", "description": "64 valid cards, page breaks"},
    {"name": "invalid_not_numeric", "description": "Non-numeric year and factor"},
    {"name": "invalid_bad_quad", "description": "Quad = '5'"},
    {"name": "invalid_zero_factor", "description": "Factor '00000' accepted (COBOL-compatible)"},
    {"name": "sequence_error", "description": "Descending order (RC=16)"},
    {"name": "empty_input", "description": "Empty file"},
    # Field Boundary (8-17)
    {"name": "boundary_year_01", "description": "Year '01'"},
    {"name": "boundary_year_99", "description": "Year '99'"},
    {"name": "boundary_year_00", "description": "Year '00' accepted (COBOL-compatible)"},
    {"name": "boundary_quad_1", "description": "Quad '1'"},
    {"name": "boundary_quad_4", "description": "Quad '4'"},
    {"name": "boundary_quad_0", "description": "Quad '0' invalid"},
    {"name": "boundary_quad_5", "description": "Quad '5' invalid"},
    {"name": "boundary_factor_00001", "description": "Factor '00001'"},
    {"name": "boundary_factor_99999", "description": "Factor '99999'"},
    {"name": "boundary_factor_10000", "description": "Factor '10000' = 1.0"},
    # Validation (18-26)
    {"name": "invalid_year_letters", "description": "Year 'AB'"},
    {"name": "invalid_year_mixed", "description": "Year '2A'"},
    {"name": "invalid_year_spaces", "description": "Year '  '"},
    {"name": "invalid_factor_mixed", "description": "Factor '29A44'"},
    {"name": "invalid_factor_spaces", "description": "Factor '2 744'"},
    {"name": "invalid_quad_9", "description": "Quad '9'"},
    {"name": "invalid_quad_space", "description": "Quad ' '"},
    {"name": "invalid_quad_letter", "description": "Quad 'A'"},
    {"name": "invalid_all_fields_bad", "description": "All fields invalid"},
    # Sequence (27-31)
    {"name": "sequence_ascending_all_quads", "description": "8 cards ascending"},
    {"name": "sequence_year_transition", "description": "Year boundary"},
    {"name": "sequence_duplicate_key", "description": "Duplicate key"},
    {"name": "sequence_error_after_valid", "description": "Seq error after valid"},
    {"name": "sequence_error_immediate", "description": "Immediate seq error"},
    # Data-Flow (32-34)
    {"name": "mixed_valid_invalid_stream", "description": "V,I,V,I,V,V"},
    {"name": "error_message_cleanup", "description": "V-I-V cleanup"},
    {"name": "counter_invariant", "description": "10 cards 7V+3I"},
    # Page Break (35-39)
    {"name": "page_break_at_23_cards", "description": "23 cards"},
    {"name": "page_break_at_24_cards", "description": "24 cards, page break"},
    {"name": "page_break_at_25_cards", "description": "25 cards"},
    {"name": "page_break_three_pages", "description": "72 cards, 3+ pages"},
    {"name": "page_break_with_errors", "description": "30 cards 25V+5I"},
    # Business (40-43)
    {"name": "business_annual_run", "description": "Annual run"},
    {"name": "business_multi_year", "description": "3 years"},
    {"name": "business_data_entry_error", "description": "Data entry typo"},
    {"name": "business_factor_unity", "description": "Factor 1.0"},
    # Format (44-48)
    {"name": "factor_record_exact_bytes", "description": "Byte-level factor"},
    {"name": "print_detail_exact_layout", "description": "Print layout"},
    {"name": "print_header_exact_layout", "description": "Header layout"},
    {"name": "print_error_line_layout", "description": "Error line"},
    {"name": "factor_display_formatting", "description": "N.NNNN display"},
    # Edge Cases (49-50)
    {"name": "single_valid_card", "description": "1 valid card"},
    {"name": "single_invalid_card", "description": "1 invalid card"},
]


# ============================================================================
# Runner Functions
# ============================================================================

class RunResult:
    """Captures the result of running either the COBOL or Java program."""

    def __init__(self):
        self.return_code: Optional[int] = None
        self.stdout: str = ""
        self.stderr: str = ""
        self.factor_content: bytes = b""
        self.print_content: str = ""
        self.error: Optional[str] = None

    @property
    def ok(self) -> bool:
        return self.error is None


def run_cobol(input_path: Path, tmpdir: str) -> RunResult:
    """Run the COBOL executable with the given input file."""
    result = RunResult()
    factor_path = os.path.join(tmpdir, "cobol_" + FACTOR_FILENAME)
    print_path = os.path.join(tmpdir, "cobol_" + PRINT_FILENAME)

    env = os.environ.copy()
    env["PATH"] = str(BUILD_DIR) + os.pathsep + env.get("PATH", "")
    env[DD_INPUT_VAR] = str(input_path)
    env[DD_FACTOR_VAR] = factor_path
    env[DD_PRINT_VAR] = print_path

    try:
        proc = subprocess.run(
            [str(COBOL_EXE)],
            env=env,
            capture_output=True,
            text=True,
            timeout=30,
            cwd=tmpdir,
        )
        result.return_code = proc.returncode
        result.stdout = proc.stdout
        result.stderr = proc.stderr
    except subprocess.TimeoutExpired:
        result.error = "COBOL execution timed out (30s)"
        return result
    except FileNotFoundError:
        result.error = f"COBOL executable not found: {COBOL_EXE}"
        return result

    # Read output files
    if os.path.exists(factor_path):
        with open(factor_path, "rb") as f:
            result.factor_content = f.read()
    if os.path.exists(print_path):
        with open(print_path, "r", errors="replace") as f:
            result.print_content = f.read()

    return result


def run_java(input_path: Path, tmpdir: str) -> RunResult:
    """Run the Java program with the given input file."""
    result = RunResult()
    factor_path = os.path.join(tmpdir, "java_" + FACTOR_FILENAME)
    print_path = os.path.join(tmpdir, "java_" + PRINT_FILENAME)

    try:
        proc = subprocess.run(
            ["java", "-jar", str(JAVA_JAR),
             str(input_path), print_path, factor_path],
            capture_output=True,
            text=True,
            timeout=30,
            cwd=tmpdir,
        )
        result.return_code = proc.returncode
        result.stdout = proc.stdout
        result.stderr = proc.stderr
    except subprocess.TimeoutExpired:
        result.error = "Java execution timed out (30s)"
        return result
    except FileNotFoundError:
        result.error = "Java runtime (java) not found. Install JDK 17+."
        return result

    # Read output files
    if os.path.exists(factor_path):
        with open(factor_path, "rb") as f:
            result.factor_content = f.read()
    if os.path.exists(print_path):
        with open(print_path, "r", errors="replace") as f:
            result.print_content = f.read()

    return result


# ============================================================================
# Comparison Functions
# ============================================================================

def mask_date(text: str) -> str:
    """Replace YYYYMMDD date strings (current date) with a placeholder.

    The date appears in the page header. We mask any 8-digit string that
    looks like a date (starts with 20xx) to allow comparison across runs.
    """
    return re.sub(r"20\d{6}", "XXXXXXXX", text)


def normalize_print(text: str) -> str:
    """Normalize print file content for comparison.

    - Strip trailing whitespace from each line
    - Mask date fields
    - Normalize line endings
    """
    lines = text.replace("\r\n", "\n").split("\n")
    normalized = [line.rstrip() for line in lines]
    # Remove trailing empty lines
    while normalized and normalized[-1] == "":
        normalized.pop()
    return mask_date("\n".join(normalized))


def compare_stdout(cobol_stdout: str, java_stdout: str) -> list[str]:
    """Compare DISPLAY output from both programs.

    Both should print the same counts:
        NO. OF INPUT RECORDS  = NNN
        NO. OF OUTPUT RECORDS = NNN
        NO. OF ERROR RECORDS  = NNN

    COBOL uses COMP-3 sign display (e.g., "+004") while Java uses plain int.
    We extract just the numeric values for comparison.
    """
    differences = []

    def extract_counts(text: str) -> dict:
        counts = {}
        for line in text.splitlines():
            line = line.strip()
            if "INPUT RECORDS" in line and "=" in line:
                try:
                    val = line.split("=")[1].strip().lstrip("+")
                    counts["input"] = int(val)
                except (ValueError, IndexError):
                    counts["input"] = None
            elif "OUTPUT RECORDS" in line and "=" in line:
                try:
                    val = line.split("=")[1].strip().lstrip("+")
                    counts["output"] = int(val)
                except (ValueError, IndexError):
                    counts["output"] = None
            elif "ERROR RECORDS" in line and "=" in line:
                try:
                    val = line.split("=")[1].strip().lstrip("+")
                    counts["error"] = int(val)
                except (ValueError, IndexError):
                    counts["error"] = None
        return counts

    cobol_counts = extract_counts(cobol_stdout)
    java_counts = extract_counts(java_stdout)

    for key in ["input", "output", "error"]:
        c_val = cobol_counts.get(key)
        j_val = java_counts.get(key)
        if c_val != j_val:
            differences.append(
                f"{key} count: COBOL={c_val}, Java={j_val}"
            )

    return differences


# ============================================================================
# Test Runner
# ============================================================================

def run_integration_test(test_case: dict, verbose: bool = False,
                         run_cobol_flag: bool = True,
                         run_java_flag: bool = True) -> dict:
    """Run a single integration test case.

    Returns dict with keys: passed, failures, known_diff, cobol, java
    """
    name = test_case["name"]
    input_path = TEST_CASES_DIR / name / "input.dat"
    is_known_diff = name in KNOWN_DIFFERENCES

    result = {
        "passed": True,
        "failures": [],
        "known_diff": is_known_diff,
        "cobol": None,
        "java": None,
    }

    if not input_path.exists():
        result["passed"] = False
        result["failures"].append(f"Input file not found: {input_path}")
        return result

    tmpdir = tempfile.mkdtemp(prefix=f"clreb020_integ_{name}_")

    try:
        # Run both programs
        if run_cobol_flag:
            result["cobol"] = run_cobol(input_path, tmpdir)
            if not result["cobol"].ok:
                result["passed"] = False
                result["failures"].append(f"COBOL: {result['cobol'].error}")
                return result

        if run_java_flag:
            result["java"] = run_java(input_path, tmpdir)
            if not result["java"].ok:
                result["passed"] = False
                result["failures"].append(f"Java: {result['java'].error}")
                return result

        # If only running one side, we can't compare — just report success
        if not run_cobol_flag or not run_java_flag:
            return result

        cobol = result["cobol"]
        java = result["java"]

        # --- Compare return codes ---
        if cobol.return_code != java.return_code:
            result["failures"].append(
                f"Return code: COBOL={cobol.return_code}, Java={java.return_code}"
            )

        # --- Compare factor files (byte-for-byte) ---
        if cobol.factor_content != java.factor_content:
            c_len = len(cobol.factor_content)
            j_len = len(java.factor_content)
            result["failures"].append(
                f"Factor file mismatch: COBOL={c_len} bytes, Java={j_len} bytes"
            )
            if verbose:
                # Show first differing byte
                min_len = min(c_len, j_len)
                for i in range(min_len):
                    if cobol.factor_content[i] != java.factor_content[i]:
                        result["failures"].append(
                            f"  First diff at byte {i}: "
                            f"COBOL=0x{cobol.factor_content[i]:02x}, "
                            f"Java=0x{java.factor_content[i]:02x}"
                        )
                        break

        # --- Compare print files (with date masking) ---
        cobol_print_norm = normalize_print(cobol.print_content)
        java_print_norm = normalize_print(java.print_content)
        if cobol_print_norm != java_print_norm:
            result["failures"].append("Print file mismatch (after date masking)")
            if verbose:
                # Show first differing line
                c_lines = cobol_print_norm.split("\n")
                j_lines = java_print_norm.split("\n")
                max_lines = max(len(c_lines), len(j_lines))
                for i in range(max_lines):
                    c_line = c_lines[i] if i < len(c_lines) else "<EOF>"
                    j_line = j_lines[i] if i < len(j_lines) else "<EOF>"
                    if c_line != j_line:
                        result["failures"].append(f"  Line {i+1} differs:")
                        result["failures"].append(f"    COBOL: {repr(c_line[:80])}")
                        result["failures"].append(f"    Java:  {repr(j_line[:80])}")
                        break

        # --- Compare stdout counts ---
        stdout_diffs = compare_stdout(cobol.stdout, java.stdout)
        if stdout_diffs:
            for diff in stdout_diffs:
                result["failures"].append(f"Stdout: {diff}")

        # --- Determine pass/fail ---
        if result["failures"]:
            if is_known_diff:
                # Known difference: report but don't fail
                result["passed"] = True
            else:
                result["passed"] = False

    finally:
        try:
            shutil.rmtree(tmpdir)
        except OSError:
            pass

    return result


def check_prerequisites() -> tuple[bool, bool]:
    """Check that COBOL exe and Java JAR exist.

    Returns (cobol_available, java_available).
    """
    cobol_ok = COBOL_EXE.exists()
    java_ok = JAVA_JAR.exists()

    # Also check that java runtime is available
    if java_ok:
        try:
            subprocess.run(
                ["java", "-version"],
                capture_output=True,
                timeout=10,
            )
        except (FileNotFoundError, subprocess.TimeoutExpired):
            java_ok = False

    return cobol_ok, java_ok


def main():
    parser = argparse.ArgumentParser(
        description="COBOL-vs-Java Integration Test for CLREB020"
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Show detailed diff output for failures",
    )
    parser.add_argument(
        "--java-only",
        action="store_true",
        help="Run only the Java program (skip COBOL comparison)",
    )
    parser.add_argument(
        "--cobol-only",
        action="store_true",
        help="Run only the COBOL program (skip Java comparison)",
    )
    args = parser.parse_args()

    print("=" * 70)
    print("CLREB020 COBOL-vs-JAVA INTEGRATION TEST")
    print("=" * 70)
    print()

    # Check prerequisites
    cobol_ok, java_ok = check_prerequisites()
    run_cobol_flag = not args.java_only
    run_java_flag = not args.cobol_only

    print(f"  COBOL executable: {COBOL_EXE}")
    print(f"    Status: {'FOUND' if cobol_ok else 'NOT FOUND'}")
    print(f"  Java JAR:         {JAVA_JAR}")
    print(f"    Status: {'FOUND' if java_ok else 'NOT FOUND'}")
    print(f"  Test cases:       {TEST_CASES_DIR}")
    print()

    if run_cobol_flag and not cobol_ok:
        print("ERROR: COBOL executable not found. Build with:")
        print("  python Build/build.py --compile")
        print()
        if not args.java_only:
            sys.exit(1)

    if run_java_flag and not java_ok:
        print("WARNING: Java JAR not found or Java runtime not available.")
        print("  Build with: cd tax_extension_java && mvn clean package -q")
        print("  Install JDK 17+ from: https://adoptium.net/")
        print()
        if not args.cobol_only:
            if not cobol_ok:
                print("Neither COBOL nor Java is available. Cannot run tests.")
                sys.exit(1)
            print("Falling back to COBOL-only mode.")
            run_java_flag = False
            run_cobol_flag = True

    comparison_mode = "COMPARISON" if (run_cobol_flag and run_java_flag) else (
        "COBOL-only" if run_cobol_flag else "Java-only"
    )
    print(f"  Mode: {comparison_mode}")
    print()
    print("-" * 70)

    total = len(TEST_CASES)
    passed = 0
    failed = 0
    known_diff_count = 0
    errors = []

    for test_case in TEST_CASES:
        name = test_case["name"]
        desc = test_case["description"]

        result = run_integration_test(
            test_case,
            verbose=args.verbose,
            run_cobol_flag=run_cobol_flag,
            run_java_flag=run_java_flag,
        )

        if result["passed"]:
            if result["known_diff"] and result["failures"]:
                status = "KNOWN"
                known_diff_count += 1
            else:
                status = "PASS"
            passed += 1
        else:
            status = "FAIL"
            failed += 1
            errors.append((name, result["failures"]))

        # Format return codes
        rc_parts = []
        if result["cobol"] and result["cobol"].ok:
            rc_parts.append(f"COB_RC={result['cobol'].return_code}")
        if result["java"] and result["java"].ok:
            rc_parts.append(f"JAV_RC={result['java'].return_code}")
        rc_str = " ".join(rc_parts)

        print(f"  [{status:5s}] {name:<25} {rc_str}")

        if args.verbose and result["failures"]:
            for failure in result["failures"]:
                prefix = "KNOWN: " if result["known_diff"] else "DIFF:  "
                print(f"           {prefix}{failure}")

    print("-" * 70)
    print()

    if run_cobol_flag and run_java_flag:
        print(f"Results: {passed} passed, {failed} failed, "
              f"{known_diff_count} known differences, {total} total")
    else:
        print(f"Results: {passed} passed, {failed} failed, {total} total")
    print()

    if known_diff_count > 0:
        print("KNOWN DIFFERENCES (not counted as failures):")
        for tc_name, description in KNOWN_DIFFERENCES.items():
            print(f"  {tc_name}: {description}")
        print()

    if errors:
        print("FAILURES:")
        for name, failures in errors:
            print(f"  {name}:")
            for failure in failures:
                print(f"    - {failure}")
            print()

    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
