#!/usr/bin/env python3
## \file integration_test.py
## COBOL-vs-Java Integration Test for CLREB020.
##
## Runs both the compiled COBOL executable (CLREB020.exe) and the Java conversion
## (tax-extension JAR) against the same test inputs, then compares their outputs
## to verify byte-identical behavior.
##
## Usage:
##     python Testing/CLREB020/integration_test.py [--verbose] [--java-only] [--cobol-only]
##
## Prerequisites:
##     - COBOL: Build/output/CLREB020.exe (compile with Build/build.py --compile)
##     - Java:  tax_extension_java/target/tax-extension-0.1.0-SNAPSHOT.jar
##              (build with: cd tax_extension_java && mvn clean package -q)
##     - Python 3.8+
##
## Known Behavioral Differences (COBOL vs Java):
##     1. Zero-factor handling: COBOL accepts "00000" as valid (alphanumeric comparison
##        "00000" > "0    " is TRUE because '0' > ' '). Java treats "00000" as invalid
##        (numeric parse -> 0, which is not > 0). The COBOL behavior is a quirk of
##        alphanumeric comparison; the Java behavior is arguably more correct but differs
##        from the original.
##     2. Date format: Both use YYYYMMDD but the date is the current date, so outputs
##        will match if run on the same day. Date fields are masked in comparisons.

import os
import sys
import subprocess
import tempfile
import shutil
import argparse
import re
import pathlib
from typing import Optional

# ============================================================================
# Configuration
# ============================================================================
## Absolute path to the directory containing this script (Testing/CLREB020/).
g_test_harness_directory_absolute_path: pathlib.Path = pathlib.Path(__file__).resolve().parent
## Absolute path to the project root (CobolTaxProgramModernization/).
g_project_root_absolute_path: pathlib.Path = g_test_harness_directory_absolute_path.parent.parent
## Directory containing GnuCOBOL runtime DLLs and compiled executables.
g_build_directory_path: pathlib.Path = g_project_root_absolute_path / "Build"
## Path to the compiled GnuCOBOL CLREB020 executable.
g_cobol_executable_path: pathlib.Path = g_build_directory_path / "output" / "CLREB020.exe"
## Root directory of the Java tax extension project.
g_java_project_directory_path: pathlib.Path = g_project_root_absolute_path / "tax_extension_java"
## Path to the compiled tax-extension JAR file.
g_java_jar_path: pathlib.Path = g_java_project_directory_path / "target" / "tax-extension-0.1.0-SNAPSHOT.jar"
## Directory containing test case input files, one subdirectory per test.
g_test_cases_directory_path: pathlib.Path = g_test_harness_directory_absolute_path / "test_cases"
## Output filename for the binary factor records.
FACTOR_FILENAME: str = "factor.dat"
## Output filename for the print report.
PRINT_FILENAME: str = "print.dat"
## Known behavioral differences between COBOL and Java (test_case_name to description).
## Currently empty: all differences have been resolved.
KNOWN_DIFFERENCES: dict[str, str] = {}

# ============================================================================
# Test Case Definitions (all 50 tests)
# ============================================================================
## All 50 integration test cases with name and description.
TEST_CASES: list[dict[str, str]] = [
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

## Captures the result of running either the COBOL or Java program.
##
## Holds the process return code, captured stdout/stderr, output file
## contents, and an optional error message if the process failed to run.
class RunResult:
    ## Initialize all result fields to their default (empty/None) values.
    def __init__(self):
        ## Process exit code (None if the process did not run).
        self.return_code: Optional[int] = None
        ## Captured standard output text from the process.
        self.stdout: str = ""
        ## Captured standard error text from the process.
        self.stderr: str = ""
        ## Raw bytes read from the binary factor output file.
        self.factor_content: bytes = b""
        ## Text read from the print report output file.
        self.print_content: str = ""
        ## Human-readable error message if the process failed to launch
        ## or timed out. None indicates the process ran successfully.
        self.error: Optional[str] = None

    ## True if no error occurred, False otherwise.
    @property
    def ok(self) -> bool:
        # CHECK IF AN ERROR EXISTS.
        error_exists: bool = self.error is not None
        return not error_exists

## Run the COBOL executable with the given input file.
##
## \param[in] input_path - Path to the input card file.
## \param[in] temporary_directory_path - Working directory for output files.
## \return Run result with return code, stdout, stderr, and output file contents.
def run_cobol(input_card_file_path: pathlib.Path, temporary_directory_path: str) -> RunResult:
    # BUILD THE ENVIRONMENT FOR RUNNING THE COBOL PROGRAM.
    process_environment: dict = os.environ.copy()
    process_environment["PATH"] = str(g_build_directory_path) + os.pathsep + process_environment.get("PATH", "")

    # Environment variables that COBOL uses for DD-name file assignments.
    DD_INPUT_ENVIRONMENT_VARIABLE_NAME: str = "DD_CARDS"
    process_environment[DD_INPUT_ENVIRONMENT_VARIABLE_NAME] = str(input_card_file_path)

    DD_FACTOR_ENVIRONMENT_VARIABLE_NAME: str = "DD_FACTOR"
    factor_path: str = os.path.join(temporary_directory_path, "cobol_" + FACTOR_FILENAME)
    process_environment[DD_FACTOR_ENVIRONMENT_VARIABLE_NAME] = factor_path

    DD_PRINT_ENVIRONMENT_VARIABLE_NAME: str = "DD_PRINT"
    print_path: str = os.path.join(temporary_directory_path, "cobol_" + PRINT_FILENAME)
    process_environment[DD_PRINT_ENVIRONMENT_VARIABLE_NAME] = print_path

    # RUN THE COBOL PROGRAM TO RETURN THE RESULT.
    # CLREB020 processes at most a few hundred cards, so 30 seconds is
    # generous and guards against hangs without rushing normal runs.
    PROCESS_TIMEOUT_IN_SECONDS: int = 30
    run_result: RunResult = RunResult()
    try:
        # RUN THE COBOL PROGRAM AND GET THE RESULT.
        process_result: subprocess.CompletedProcess = subprocess.run(
            [str(g_cobol_executable_path)],
            env = process_environment,
            capture_output = True,
            text = True,
            timeout = PROCESS_TIMEOUT_IN_SECONDS,
            cwd = temporary_directory_path)
        run_result.return_code = process_result.returncode
        run_result.stdout = process_result.stdout
        run_result.stderr = process_result.stderr
    except subprocess.TimeoutExpired:
        run_result.error = f"COBOL execution timed out ({PROCESS_TIMEOUT_IN_SECONDS}s)"
        return run_result
    except FileNotFoundError:
        run_result.error = f"COBOL executable not found: {g_cobol_executable_path}"
        return run_result

    # READ THE OUTPUT FILES PRODUCED BY THE COBOL PROGRAM.
    factor_file_exists: bool = os.path.exists(factor_path)
    if factor_file_exists:
        with open(factor_path, "rb") as factor_file:
            run_result.factor_content = factor_file.read()

    print_file_exists: bool = os.path.exists(print_path)
    if print_file_exists:
        with open(print_path, "r", errors = "replace") as print_file:
            run_result.print_content = print_file.read()

    return run_result

## Run the Java program with the given input file.
##
## \param[in] input_card_file_path - Path to the input card file.
## \param[in] temporary_directory_path - Working directory for output files.
## \return Run result with return code, stdout, stderr, and output file contents.
def run_java(input_card_file_path: pathlib.Path, temporary_directory_path: str) -> RunResult:
    # EXECUTE THE JAVA JAR WITH INPUT AND OUTPUT FILE PATHS.
    run_result: RunResult = RunResult()
    try:
        # RUN THE JAVA JAR AND GET THE RESULT.
        # CLREB020 processes at most a few hundred cards, so 30 seconds is
        # generous and guards against hangs without rushing normal runs.
        PROCESS_TIMEOUT_IN_SECONDS: int = 30
        factor_path: str = os.path.join(temporary_directory_path, "java_" + FACTOR_FILENAME)
        print_path: str = os.path.join(temporary_directory_path, "java_" + PRINT_FILENAME)
        process_result: subprocess.CompletedProcess = subprocess.run(
            ["java", "-jar", str(g_java_jar_path),
             str(input_card_file_path), print_path, factor_path],
            capture_output = True,
            text = True,
            timeout = PROCESS_TIMEOUT_IN_SECONDS,
            cwd = temporary_directory_path)
        run_result.return_code = process_result.returncode
        run_result.stdout = process_result.stdout
        run_result.stderr = process_result.stderr
    except subprocess.TimeoutExpired:
        run_result.error = f"Java execution timed out ({PROCESS_TIMEOUT_IN_SECONDS}s)"
        return run_result
    except FileNotFoundError:
        run_result.error = "Java runtime (java) not found. Install JDK 17+."
        return run_result

    # READ THE OUTPUT FILES PRODUCED BY THE JAVA PROGRAM.
    factor_file_exists: bool = os.path.exists(factor_path)
    if factor_file_exists:
        with open(factor_path, "rb") as factor_file:
            run_result.factor_content = factor_file.read()

    print_file_exists: bool = os.path.exists(print_path)
    if print_file_exists:
        with open(print_path, "r", errors = "replace") as print_file:
            run_result.print_content = print_file.read()

    return run_result

# ============================================================================
# Comparison Functions
# ============================================================================

## Replace YYYYMMDD date strings (current date) with a placeholder.
##
## The date appears in the page header. We mask any 8-digit string that
## looks like a date (starts with 20xx) to allow comparison across runs.
##
## \param[in] text - The text containing potential date strings.
## \return The text with all YYYYMMDD dates replaced by "XXXXXXXX".
def mask_date(text: str) -> str:
    # Matches "20" followed by exactly 6 digits, i.e. any YYYYMMDD date
    # in the 2000s (e.g. "20260220"). The COBOL and Java programs embed
    # the current date in print headers, which would cause spurious
    # mismatches if not masked.
    DATE_IN_2000S_REGEX_PATTERN: str = r"20\d{6}"
    masked_date_text: str = re.sub(DATE_IN_2000S_REGEX_PATTERN, "XXXXXXXX", text)
    return masked_date_text

## Normalize print file content for comparison.
##
## Strips trailing whitespace from each line, masks date fields,
## and normalizes line endings.
##
## \param[in] text - Raw print file content.
## \return Normalized content suitable for comparison.
def normalize_print(text: str) -> str:
    # GET STRIPPED LINES FROM THE ORIGINAL TEXT.
    # There may be multiple different kind of newlines that need to be normalized.
    lines: list[str] = text.replace("\r\n", "\n").split("\n")
    # Stripping trailing whitespace from each line helps with detecting conceptually empty lines.
    normalized_lines: list[str] = [line.rstrip() for line in lines]

    # REMOVE ANY EMPTY LINES AT THE BOTTOM OF THE TEXT.
    LAST_LINE_INDEX: int = -1
    last_line_is_empty: bool = bool(normalized_lines) and normalized_lines[LAST_LINE_INDEX] == ""
    while last_line_is_empty:
        # REMOVE THE CURRENT EMPTY LINE.
        normalized_lines.pop()

        # NEXT IF THE NEW LAST LINE IS EMPTY.
        last_line_is_empty = bool(normalized_lines) and normalized_lines[LAST_LINE_INDEX] == ""

    # RETURN THE COMPACTED TEXT WITH MASKED DATES.
    normalized_text: str = "\n".join(normalized_lines)
    normalized_text_with_masked_dates: str = mask_date(normalized_text)
    return normalized_text_with_masked_dates

## Compare DISPLAY output from both programs.
##
## Both should print the same counts:
##     NO. OF INPUT RECORDS  = NNN
##     NO. OF OUTPUT RECORDS = NNN
##     NO. OF ERROR RECORDS  = NNN
##
## COBOL uses COMP-3 sign display (e.g., "+004") while Java uses plain int.
## We extract just the numeric values for comparison.
##
## \param[in] cobol_stdout - Captured stdout from the COBOL program.
## \param[in] java_stdout - Captured stdout from the Java program.
## \return list of difference descriptions (empty if counts match).
def compare_stdout(cobol_stdout: str, java_stdout: str) -> list[str]:
    # EXTRACT RECORD COUNTS FROM BOTH PROGRAMS AND COMPARE THEM.
    differences: list[str] = []

    ## Parse input/output/error record counts from DISPLAY output text.
    ##
    ## Handles both COBOL COMP-3 sign format ("+004") and plain integers ("4")
    ## by stripping leading "+" before parsing.
    ##
    ## \param[in] text - Captured stdout text from either program.
    ## \return Mapping of "input"/"output"/"error" to integer counts (or None on parse failure).
    def extract_counts(text: str) -> dict[str, object]:
        # EXTRACT THE RECORD COUNTS FROM THE LINES OF TEXT.
        record_count_by_type: dict[str, object] = {}
        text_lines: list[str] = text.splitlines()
        for line in text_lines:
            # STRIP THE LINE OF LEADING/TRAILING WHITESPACE.
            # This makes parsing easier.
            stripped_line: str = line.strip()

            # DETERMINE WHICH COUNT TYPE THIS LINE REPRESENTS.
            # Both COBOL and Java emit lines like "NO. OF INPUT RECORDS  = NNN".
            INPUT_RECORD_LABEL: str = "INPUT RECORDS"
            OUTPUT_RECORD_LABEL: str = "OUTPUT RECORDS"
            ERROR_RECORD_LABEL: str = "ERROR RECORDS"
            VALUE_SEPARATOR: str = "="
            VALUE_PORTION_INDEX: int = 1
            line_contains_input_label: bool = INPUT_RECORD_LABEL in stripped_line
            line_contains_output_label: bool = OUTPUT_RECORD_LABEL in stripped_line
            line_contains_error_label: bool = ERROR_RECORD_LABEL in stripped_line
            line_contains_separator: bool = VALUE_SEPARATOR in stripped_line
            is_input_record_line: bool = line_contains_input_label and line_contains_separator
            is_output_record_line: bool = line_contains_output_label and line_contains_separator
            is_error_record_line: bool = line_contains_error_label and line_contains_separator

            # PARSE THE INPUT RECORD COUNT.
            if is_input_record_line:
                # A try block is used to catch various parsing errors.
                INPUT_COUNT_KEY: str = "input"
                try:
                    # GET THE TEXT AFTER THE VALUE SEPARATOR.
                    text_after_separator: str = stripped_line.split(VALUE_SEPARATOR)[VALUE_PORTION_INDEX]

                    # EXTRACT THE NUMERIC COUNT FROM THE TEXT.
                    # Strip whitespace, then remove the leading "+" sign that
                    # COBOL COMP-3 DISPLAY format produces (e.g. "+004" -> "004").
                    input_record_count_text: str = text_after_separator.strip().lstrip("+")
                    counts[INPUT_COUNT_KEY] = int(input_record_count_text)
                except (ValueError, IndexError):
                    # INDICATE NO VALID INPUT COUNT WAS FOUND.
                    record_count_by_type[INPUT_COUNT_KEY] = None

            # PARSE THE OUTPUT RECORD COUNT.
            elif is_output_record_line:
                # A try block is used to catch various parsing errors.
                OUTPUT_COUNT_KEY: str = "output"
                try:
                    # GET THE TEXT AFTER THE VALUE SEPARATOR.
                    text_after_separator: str = stripped_line.split(VALUE_SEPARATOR)[VALUE_PORTION_INDEX]

                    # EXTRACT THE NUMERIC COUNT FROM THE TEXT.
                    # Strip whitespace, then remove the leading "+" sign that
                    # COBOL COMP-3 DISPLAY format produces (e.g. "+004" -> "004").
                    output_record_count_text: str = text_after_separator.strip().lstrip("+")
                    counts[OUTPUT_COUNT_KEY] = int(output_record_count_text)
                except (ValueError, IndexError):
                    # INDICATE NO VALID OUTPUT COUNT WAS FOUND.
                    record_count_by_type[OUTPUT_COUNT_KEY] = None

            # PARSE THE ERROR RECORD COUNT.
            elif is_error_record_line:
                # A try block is used to catch various parsing errors.
                ERROR_COUNT_KEY: str = "error"
                try:
                    # GET THE TEXT AFTER THE VALUE SEPARATOR.
                    text_after_separator: str = stripped_line.split(VALUE_SEPARATOR)[VALUE_PORTION_INDEX]

                    # EXTRACT THE NUMERIC COUNT FROM THE TEXT.
                    # Strip whitespace, then remove the leading "+" sign that
                    # COBOL COMP-3 DISPLAY format produces (e.g. "+004" -> "004").
                    error_record_count_text: str = text_after_separator.strip().lstrip("+")
                    counts[ERROR_COUNT_KEY] = int(error_record_count_text)
                except (ValueError, IndexError):
                    # INDICATE NO VALID ERROR COUNT WAS FOUND.
                    record_count_by_type[ERROR_COUNT_KEY] = None

        return record_count_by_type

    # GET RECORD COUNTS FOR BOTH PROGRAMS.
    cobol_record_count_by_type: dict[str, object] = extract_counts(cobol_stdout)
    java_record_count_by_type: dict[str, object] = extract_counts(java_stdout)

    # COMPARE THE RECORD COUNTS FOR EACH TYPE.
    for record_count_type in ["input", "output", "error"]:
        # CHECK IF BOTH COBOL AND JAVA RECORD COUNTS ARE THE SAME.
        cobol_value: object = cobol_record_count_by_type.get(record_count_type)
        java_value: object = java_record_count_by_type.get(record_count_type)
        cobol_and_java_counts_are_the_same: bool = cobol_value == java_value
        if not cobol_and_java_counts_are_the_same:
            # TRACK THE MISMATCH.
            differences.append(f"{record_count_type} count: COBOL={cobol_value}, Java={java_value}")

    return differences

# ============================================================================
# Test Runner
# ============================================================================

## Run a single integration test case.
##
## Runs both the COBOL executable and Java JAR against the same input file,
## then compares their outputs (return code, factor file, print file, stdout
## record counts) to verify behavioral equivalence.
##
## \param[in] test_case - Test case definition dict containing:
##     - "name" (str): Test case directory name under test_cases/.
##     - "description" (str): Human-readable summary of what the test covers.
## \param[in] verbose - If True, show detailed diff output for failures.
## \param[in] run_cobol - If True, run the COBOL program.
## \param[in] run_java - If True, run the Java program.
## \return Result dict containing:
##     - "passed" (bool): True if the test passed (or is a known difference).
##     - "failures" (list[str]): Human-readable descriptions of each mismatch.
##     - "known_diff" (bool): True if this test is in the KNOWN_DIFFERENCES list.
##     - "cobol" (RunResult or None): COBOL run result, None if COBOL was skipped.
##     - "java" (RunResult or None): Java run result, None if Java was skipped.
def run_integration_test(
    test_case: dict,
    verbose: bool = False,
    run_cobol: bool = True,
    run_java: bool = True,
) -> dict:
    # INITIALIZE THE INTEGRATION TEST RESULT BY WHETHER OR NOT THE TEST IS A KNOWN DIFFERENCE.
    name: str = test_case["name"]
    is_known_difference: bool = name in KNOWN_DIFFERENCES
    integration_test_result: dict = {
        "passed": True,
        "failures": [],
        "known_diff": is_known_difference,
        "cobol": None,
        "java": None,
    }

    # GET THE INPUT CARD FILE PATH.
    input_card_file_path: pathlib.Path = g_test_cases_directory_path / name / "input.dat"
    input_card_file_exists: bool = input_card_file_path.exists()
    if not input_card_file_exists:
        # INDICATE THAT THE TEST FAILED BECAUSE THE INPUT CARD FILE WAS NOT FOUND.
        integration_test_result["passed"] = False
        integration_test_result["failures"].append(f"Input file not found: {input_card_file_path}")
        return integration_test_result

    # RUN THE COBOL AND JAVA PROGRAMS TO TEST WHETHER THEY'RE EQUIVALENT.
    # A temporary directory is used to hold some files that will be cleaned up after the test.
    temporary_directory_path: str = tempfile.mkdtemp(prefix = f"clreb020_integ_{name}_")
    try:
        # RUN THE COBOL PROGRAM IF REQUESTED.
        if run_cobol:
            # RUN THE COBOL PROGRAM.
            integration_test_result["cobol"] = run_cobol(input_card_file_path, temporary_directory_path)
            cobol_program_ran_successfully: bool = integration_test_result["cobol"].ok
            if not cobol_program_ran_successfully:
                # INDICATE THAT THE TEST FAILED BECAUSE THE COBOL PROGRAM DID NOT RUN SUCCESSFULLY.
                integration_test_result["passed"] = False
                integration_test_result["failures"].append(f"COBOL: {integration_test_result['cobol'].error}")
                return integration_test_result

        # RUN THE JAVA PROGRAM IF REQUESTED.
        if run_java:
            # RUN THE JAVA PROGRAM.
            integration_test_result["java"] = run_java(input_card_file_path, temporary_directory_path)
            java_program_ran_successfully: bool = integration_test_result["java"].ok
            if not java_program_ran_successfully:
                # INDICATE THAT THE TEST FAILED BECAUSE THE JAVA PROGRAM DID NOT RUN SUCCESSFULLY.
                integration_test_result["passed"] = False
                integration_test_result["failures"].append(f"Java: {integration_test_result['java'].error}")
                return integration_test_result

        # IF ONLY RUNNING ONE SIDE, WE CAN'T COMPARE — JUST REPORT SUCCESS.
        running_only_one_side: bool = not run_cobol or not run_java
        if running_only_one_side:
            return integration_test_result

        # COMPARE RETURN CODES.
        cobol_result: RunResult = integration_test_result["cobol"]
        java_result: RunResult = integration_test_result["java"]
        return_codes_differ: bool = cobol_result.return_code != java_result.return_code
        if return_codes_differ:
            integration_test_result["failures"].append(f"Return code: COBOL={cobol_result.return_code}, Java={java_result.return_code}")

        # COMPARE FACTOR FILES BYTE-FOR-BYTE.
        factor_contents_differ: bool = cobol_result.factor_content != java_result.factor_content
        if factor_contents_differ:
            # TRACK THE MISMATCH.
            cobol_factor_size_in_bytes: int = len(cobol_result.factor_content)
            java_factor_size_in_bytes: int = len(java_result.factor_content)
            integration_test_result["failures"].append(
                f"Factor file mismatch: COBOL={cobol_factor_size_in_bytes} bytes, "
                f"Java={java_factor_size_in_bytes} bytes"
            )
            if verbose:
                # SHOW THE FIRST DIFFERING BYTE.
                minimum_length_in_bytes: int = min(cobol_factor_size_in_bytes, java_factor_size_in_bytes)
                for byte_index in range(minimum_length_in_bytes):
                    # CHECK IF THIS BYTE DIFFERS.
                    cobol_byte: int = cobol_result.factor_content[byte_index]
                    java_byte: int = java_result.factor_content[byte_index]
                    cobol_and_java_bytes_are_the_same: bool = cobol_byte == java_byte
                    if not cobol_and_java_bytes_are_the_same:
                        # TRACK THE MISMATCH.
                        integration_test_result["failures"].append(
                            f"  First diff at byte {byte_index}: "
                            f"COBOL=0x{cobol_byte:02x}, Java=0x{java_byte:02x}"
                        )
                        break

        # COMPARE PRINT FILES WITH DATE MASKING.
        cobol_print_normalized: str = normalize_print(cobol_result.print_content)
        java_print_normalized: str = normalize_print(java_result.print_content)
        print_contents_differ: bool = cobol_print_normalized != java_print_normalized
        if print_contents_differ:
            # TRACK THE MISMATCH.
            integration_test_result["failures"].append("Print file mismatch (after date masking)")
            if verbose:
                # SHOW THE FIRST DIFFERING LINE.
                cobol_lines: list[str] = cobol_print_normalized.split("\n")
                java_lines: list[str] = java_print_normalized.split("\n")
                maximum_line_count: int = max(len(cobol_lines), len(java_lines))
                for line_index in range(maximum_line_count):
                    # GET THE COBOL AND JAVA LINES.
                    # To avoid errors with some lines being out of range, we have <EOF> placeholders.
                    cobol_line: str = cobol_lines[line_index] if line_index < len(cobol_lines) else "<EOF>"
                    java_line: str = java_lines[line_index] if line_index < len(java_lines) else "<EOF>"

                    # CHECK IF THIS LINE DIFFERS.
                    cobol_and_java_lines_are_the_same: bool = cobol_line == java_line
                    if not cobol_and_java_lines_are_the_same:
                        # TRACK THE MISMATCH.
                        integration_test_result["failures"].append(f"  Line {line_index + 1} differs:")
                        integration_test_result["failures"].append(f"    COBOL: {repr(cobol_line[:80])}")
                        integration_test_result["failures"].append(f"    Java:  {repr(java_line[:80])}")
                        break

        # COMPARE STDOUT RECORD COUNTS.
        stdout_differences: list[str] = compare_stdout(cobol_result.stdout, java_result.stdout)
        if stdout_differences:
            # TRACK EACH DIFFERENCE.
            for difference in stdout_differences:
                integration_test_result["failures"].append(f"Stdout: {difference}")

        # DETERMINE PASS/FAIL BASED ON FAILURES AND KNOWN DIFFERENCES.
        if integration_test_result["failures"]:
            if is_known_difference:
                integration_test_result["passed"] = True
            else:
                integration_test_result["passed"] = False

    finally:
        # CLEAN UP THE TEMPORARY DIRECTORY.
        try:
            shutil.rmtree(temporary_directory_path)
        except OSError:
            pass

    return integration_test_result

## Check that COBOL exe and Java JAR exist.
##
## \return Tuple of (cobol_available, java_available) booleans.
def check_prerequisites() -> tuple[bool, bool]:
    # CHECK THAT THE COBOL EXECUTABLE AND JAVA JAR EXIST.
    cobol_available: bool = g_cobol_executable_path.exists()
    java_available: bool = g_java_jar_path.exists()

    # ALSO VERIFY THAT THE JAVA RUNTIME IS ON THE PATH.
    # A quick version check should complete almost instantly; 10 seconds
    # is a generous upper bound for a slow or cold JVM start.
    VERSION_CHECK_TIMEOUT_IN_SECONDS: int = 10
    if java_available:
        try:
            subprocess.run(
                ["java", "-version"],
                capture_output = True,
                timeout = VERSION_CHECK_TIMEOUT_IN_SECONDS,
            )
        except (FileNotFoundError, subprocess.TimeoutExpired):
            java_available = False

    return cobol_available, java_available

## Main entry point for the integration test.
##
## Parses command line arguments, checks prerequisites, runs each test case,
## and prints a summary report.
##
## \return Exit code (0 if all tests passed, 1 if any test failed).
def main() -> int:
    # PARSE THE COMMAND LINE ARGUMENTS.
    command_line_argument_parser: argparse.ArgumentParser = argparse.ArgumentParser(
        description = "COBOL-vs-Java Integration Test for CLREB020")
    command_line_argument_parser.add_argument(
        "--verbose", "-v",
        action = "store_true",
        help = "Show detailed diff output for failures")
    command_line_argument_parser.add_argument(
        "--java-only",
        action = "store_true",
        help = "Run only the Java program (skip COBOL comparison)")
    command_line_argument_parser.add_argument(
        "--cobol-only",
        action = "store_true",
        help = "Run only the COBOL program (skip Java comparison)")
    command_line_arguments: argparse.Namespace = command_line_argument_parser.parse_args()

    # PRINT AN INFORMATIONAL BANNER.
    HEADER_SEPARATOR_CHARACTER: str = "="
    HEADER_SEPARATOR_CHARACTER_COUNT: int = 70
    HEADER_SEPARATOR_LINE: str = HEADER_SEPARATOR_CHARACTER * HEADER_SEPARATOR_CHARACTER_COUNT
    print(HEADER_SEPARATOR_LINE)
    print("CLREB020 COBOL-vs-JAVA INTEGRATION TEST")
    print(HEADER_SEPARATOR_LINE)
    print()

    # CHECK PREREQUISITES.
    cobol_available, java_available = check_prerequisites()
    run_cobol: bool = not command_line_arguments.java_only
    run_java: bool = not command_line_arguments.cobol_only
    print(f"  COBOL executable: {g_cobol_executable_path}")
    print(f"    Status: {'FOUND' if cobol_available else 'NOT FOUND'}")
    print(f"  Java JAR:         {g_java_jar_path}")
    print(f"    Status: {'FOUND' if java_available else 'NOT FOUND'}")
    print(f"  Test cases:       {g_test_cases_directory_path}")
    print()

    # CHECK IF THE COBOL PROGRAM CAN ACTUALLY BE RUN.
    ERROR_EXIT_CODE: int = 1
    cobol_program_can_be_run: bool = cobol_available and run_cobol
    if not cobol_program_can_be_run:
        # PROVIDE VISIBILITY INOT THE ERROR.
        print("ERROR: COBOL executable not found. Build with:")
        print("  python Build/build.py --compile")
        print()

        # EXIT IF WE'RE NOT ONLY RUNNING JAVA TESTS.
        if not command_line_arguments.java_only:
            return ERROR_EXIT_CODE

    # CHECK IF THE JAVA PROGRAM CAN ACTUALLY BE RUN.
    java_program_can_be_run: bool = java_available and run_java
    if not java_program_can_be_run:
        # PROVIDE VISIBILITY INOT THE ERROR.
        print("WARNING: Java JAR not found or Java runtime not available.")
        print("  Build with: cd tax_extension_java && mvn clean package -q")
        print("  Install JDK 17+ from: https://adoptium.net/")
        print()

        # EXIT IF WE'RE NOT ONLY RUNNING COBOL TESTS.
        if not command_line_arguments.cobol_only:
            # CHECK IF COBOL IS AVAILABLE.
            if not cobol_available:
                print("Neither COBOL nor Java is available. Cannot run tests.")
                return ERROR_EXIT_CODE

            print("Falling back to COBOL-only mode.")
            run_java = False
            run_cobol = True

    # DETERMINE AND DISPLAY THE COMPARISON MODE.
    running_both: bool = run_cobol and run_java
    comparison_mode: str = "COMPARISON" if running_both else (
        "COBOL-only" if run_cobol else "Java-only"
    )
    print(f"  Mode: {comparison_mode}")
    print()
    MINOR_SEPARATOR_CHARACTER: str = "-"
    MINOR_SEPARATOR_LINE: str = MINOR_SEPARATOR_CHARACTER * HEADER_SEPARATOR_CHARACTER_COUNT
    print(MINOR_SEPARATOR_LINE)

    # RUN EACH TEST CASE AND ACCUMULATE RESULTS.
    total_test_count: int = len(TEST_CASES)
    passed_count: int = 0
    failed_count: int = 0
    known_difference_count: int = 0
    failure_details: list[tuple[str, list[str]]] = []
    for test_case in TEST_CASES:
        # RUN THE TEST CASE.
        test_case_result: dict = run_integration_test(
            test_case,
            verbose = command_line_arguments.verbose,
            run_cobol = run_cobol,
            run_java = run_java)

        # CHECK IF THE TEST PASSED OR FAILED.
        # Some identifying metadata is need about the test for some reporting purposes.
        test_case_name: str = test_case["name"]
        test_case_description: str = test_case["description"]
        status: str = ""
        if test_case_result["passed"]:
            # CHECK IF THIS IS A KNOWN DIFFERENCE WITH FAILURES.
            is_known_difference_with_failures: bool = test_case_result["known_diff"] and test_case_result["failures"]
            if is_known_difference_with_failures:
                # TRACK THE KNOWN DIFFERENCE.
                status = "KNOWN"
                known_difference_count += 1
            else:
                # INDICATE THIS IS A NORMAL PASS.
                status = "PASS"

            # COUNT THE PASSED TEST.
            passed_count += 1
        else:
            # TRACK THE FAILURE.
            status = "FAIL"
            failed_count += 1
            failure_details.append((test_case_name, test_case_result["failures"]))

        # FORMAT THE RETURN CODES FOR DISPLAY.
        return_code_parts: list[str] = []

        # Any COBOL portions should be included.
        cobol_results_exist: bool = bool(test_case_result["cobol"])
        if cobol_results_exist:
            # CHECK IF THE COBOL TEST WAS OKAY.
            cobol_test_ok: bool = test_case_result["cobol"].ok
            if cobol_test_ok:
                # ADD THE COBOL TEST RETURN CODE INFORMATION.
                return_code_parts.append(f"COB_RC={test_case_result['cobol'].return_code}")

        # Any Java portions should be included.
        java_results_exist: bool = bool(test_case_result["java"])
        if java_results_exist:
            # CHECK IF THE JAVA TEST WAS OKAY.
            java_test_ok: bool = test_case_result["java"].ok
            if java_test_ok:
                # ADD THE JAVA TEST RETURN CODE INFORMATION.
                return_code_parts.append(f"JAV_RC={test_case_result['java'].return_code}")

        # All return code parts need to be combined.
        return_code_string: str = " ".join(return_code_parts)

        # PRINT THE FINAL OVERALL SUMMARY FOR THIS TEST.
        print(f"  [{status:5s}] {test_case_name:<25} {return_code_string}")

        # CHECK IF FAILURE DETAILS SHOULD BE PRINTED.
        failure_details_should_be_printed: bool = command_line_arguments.verbose and test_case_result["failures"]
        if failure_details_should_be_printed:
            for failure in test_case_result["failures"]:
                # PRINT THE FAILURE WITH AN APPROPRIATE PREFIX.
                prefix: str = "KNOWN: " if test_case_result["known_diff"] else "DIFF:  "
                print(f"           {prefix}{failure}")

    # PRINT THE SUMMARY REPORT.
    print(MINOR_SEPARATOR_LINE)
    print()

    # Overall test summary statistics should be printed.
    if running_both:
        print(f"Results: {passed_count} passed, {failed_count} failed, "
              f"{known_difference_count} known differences, {total_test_count} total")
    else:
        print(f"Results: {passed_count} passed, {failed_count} failed, {total_test_count} total")
    print()

    # Known differences should be printed if they exist.
    known_differences_exist: bool = known_difference_count > 0
    if known_differences_exist:
        # PRINT ALL KNOWN DIFFERENCES.
        print("KNOWN DIFFERENCES (not counted as failures):")
        for test_case_name, description in KNOWN_DIFFERENCES.items():
            # PRINT THE CURRENT KNOWN DIFFERENCE.
            print(f"  {test_case_name}: {description}")
        print()

    # Specific failures should also be printed.
    if failure_details:
        # PRINT ALL FAILURES.
        print("FAILURES:")
        for name, failures in failure_details:
            # PRINT ALL FAILURES FOR THE CURRENT TEST.
            print(f"  {name}:")
            for failure in failures:
                print(f"    - {failure}")
            print()

    # RETURN THE APPROPRIATE EXIT CODE.
    SUCCESS_EXIT_CODE: int = 0
    exit_code: int = SUCCESS_EXIT_CODE if failed_count == 0 else ERROR_EXIT_CODE
    return exit_code

if __name__ == "__main__":
    sys.exit(main())
