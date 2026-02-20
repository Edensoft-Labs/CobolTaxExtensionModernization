#!/usr/bin/env python3
## \file cobol_test_harness.py
## COBOL Test Harness for CLREB020 - Equalization Factor Edit and List Program.
##
## This harness runs the compiled GnuCOBOL executable CLREB020.exe against a suite
## of test cases and validates the output.
##
## Usage:
##     python Testing/CLREB020/cobol_test_harness.py [--generate-expected] [--verbose]
##
## Options:
##     --generate-expected   Run all tests and save outputs as expected baselines
##     --verbose             Show detailed output for each test case
##
## GnuCOBOL DD Name Mapping:
##     The COBOL source uses SELECT...ASSIGN TO UT-S-CARDS, UT-S-FACTOR, UT-S-PRINT.
##     GnuCOBOL strips the "UT-S-" prefix and uses the remainder as the environment
##     variable name (with DD_ prefix). Therefore:
##         DD_CARDS  -> CARD-FILE   (input, 80-byte fixed-length records)
##         DD_FACTOR -> FACTOR-FILE (output, 21-byte fixed-length records)
##         DD_PRINT  -> PRINT-FILE  (output, 133-char print records, line sequential)
##
## File Format Notes:
##     - Input cards are BINARY fixed-length 80-byte records with NO line terminators.
##       GnuCOBOL reads them as RECORDING MODE F with RECORD CONTAINS 80 CHARACTERS.
##     - Factor output is BINARY fixed-length 21-byte records with NO line terminators.
##     - Print output uses GnuCOBOL's default line sequential mode with ASA carriage
##       control characters converted to newlines and form feeds.
##
## CLREB020 Program Behavior:
##     - Reads 80-char card records from CARD-FILE
##     - Card layout:
##       - position 1-2 = CD-YR (year)
##       - position 3 = CD-QUAD (1-4)
##       - position 4-8 = CD-FACTOR (5 digits as 9V9999)
##       - position 9-80 = filler spaces
##     - Validates: year is numeric & >0, factor is numeric & >0, quad is 1-4
##     - Sequence checks cards (ascending order on first 3 chars: year+quad)
##     - Writes 21-char factor records to FACTOR-FILE
##     - Writes 133-char print records to PRINT-FILE
##     - Sets RETURN-CODE to 16 on sequence error
##     - DISPLAYs input/output/error counts to console (stdout)
##
## Note on zero-factor behavior:
##     The COBOL code checks "CD-FACTOR GREATER THAN 0" using an alphanumeric
##     comparison (CD-FACTOR is PIC X(5)). "00000" compared to "0" (space-padded
##     to "0    ") is GREATER because '0' (0x30) > ' ' (0x20) in ASCII. Therefore,
##     a factor of "00000" is treated as VALID by the original program. This is a
##     faithful reproduction of the original mainframe behavior.

import os
import sys
import subprocess
import tempfile
import shutil
import argparse
import pathlib

import test_utilities

# ============================================================================
# COBOL-SPECIFIC CONFIGURATION
# ============================================================================
## Directory containing GnuCOBOL runtime DLLs (libcob, etc.) and cobc.exe.
g_build_directory_path: pathlib.Path = test_utilities.g_project_root_absolute_path / "Build"
## Compiled GnuCOBOL executable for CLREB020.
g_cobol_executable_path: pathlib.Path = g_build_directory_path / "output" / "CLREB020.exe"
## Environment variable name for the CARD-FILE input (DD_CARDS).
## GnuCOBOL maps SELECT...ASSIGN TO UT-S-CARDS to this env var.
DD_INPUT_ENVIRONMENT_VARIABLE_NAME: str = "DD_CARDS"
## Environment variable name for the FACTOR-FILE output (DD_FACTOR).
DD_FACTOR_ENVIRONMENT_VARIABLE_NAME: str = "DD_FACTOR"
## Environment variable name for the PRINT-FILE output (DD_PRINT).
DD_PRINT_ENVIRONMENT_VARIABLE_NAME: str = "DD_PRINT"

# ============================================================================
# TEST RUNNER
# ============================================================================

## Run a single COBOL test case and validate all outputs.
##
## Executes CLREB020.exe in a temporary directory with DD environment variables
## pointing to the input file and output locations. Validates the return code,
## stderr (should be empty), factor record count, error count from DISPLAY
## output, and compares all output files against expected baselines.
##
## \param[in] test_case - Test case definition dict containing:
##     - "name" (str): Test case directory name under test_cases/.
##     - "description" (str): Human-readable summary of what the test covers.
##     - "expect_rc" (int or None): Expected process return code, if any.
##     - "expect_factor_records" (int or None): Expected number of factor records, if any.
##     - "expect_error_count" (int or None): Expected error count from DISPLAY output, if any.
## \param[in] verbose - If True, show detailed output for the test case.
## \return Result dict containing:
##     - "passed" (bool): True if all validations succeeded.
##     - "failures" (list[str]): Human-readable descriptions of each validation failure.
##     - "return_code" (int or None): Process exit code, None if the process did not run.
##     - "stdout" (str): Captured standard output text from the process.
##     - "stderr" (str): Captured standard error text from the process.
##     - "factor_records" (int): Number of 21-byte factor records in the output file.
##     - "counts" (dict): Parsed record counts from DISPLAY output (input_count, output_count, error_count).
def run_test(test_case: dict, verbose: bool = False) -> dict:
    # INITIALIZE THE TEST RESULT.
    test_result: dict = {
        "passed": True,
        "failures": [],
        "return_code": None,
        "stdout": "",
        "stderr": "",
        "factor_records": 0,
        "counts": {},
    }

    # CHECK THAT THE INPUT CARD FILE EXISTS.
    name: str = test_case["name"]
    input_card_file_path: pathlib.Path = test_utilities.g_test_cases_directory_path / name / test_utilities.INPUT_FILENAME
    input_card_file_exists: bool = input_card_file_path.exists()
    if not input_card_file_exists:
        # INDICATE THAT THE TEST FAILED BECAUSE THE INPUT CARD FILE WAS NOT FOUND.
        test_result["passed"] = False
        test_result["failures"].append(f"Input file not found: {input_card_file_path}")
        return test_result

    # EXECUTE THE COBOL PROGRAM IN A TEMPORARY DIRECTORY.
    # A temporary directory is used to hold some files that will be cleaned up after the test.
    temporary_directory_path: str = tempfile.mkdtemp(prefix = f"clreb020_test_{name}_")
    try:
        # BUILD THE ENVIRONMENT WITH GNUCOBOL DLL PATH AND DD ENVIRONMENT VARIABLE FILE MAPPINGS.
        process_environment: dict = os.environ.copy()
        process_environment["PATH"] = str(g_build_directory_path) + os.pathsep + process_environment.get("PATH", "")
        process_environment[DD_INPUT_ENVIRONMENT_VARIABLE_NAME] = str(input_card_file_path)
        factor_path: str = os.path.join(temporary_directory_path, test_utilities.FACTOR_FILENAME)
        process_environment[DD_FACTOR_ENVIRONMENT_VARIABLE_NAME] = factor_path
        print_path: str = os.path.join(temporary_directory_path, test_utilities.PRINT_FILENAME)
        process_environment[DD_PRINT_ENVIRONMENT_VARIABLE_NAME] = print_path

        # RUN THE COBOL EXECUTABLE.
        try:
            process_result: subprocess.CompletedProcess = subprocess.run(
                [str(g_cobol_executable_path)],
                env = process_environment,
                capture_output = True,
                text = True,
                timeout = test_utilities.SUBPROCESS_TIMEOUT_IN_SECONDS,
                cwd = temporary_directory_path)
        except subprocess.TimeoutExpired:
            test_result["passed"] = False
            test_result["failures"].append(f"Execution timed out ({test_utilities.SUBPROCESS_TIMEOUT_IN_SECONDS} seconds)")
            return test_result
        except FileNotFoundError:
            test_result["passed"] = False
            test_result["failures"].append(f"Executable not found: {g_cobol_executable_path}")
            return test_result

        # STORE THE PROCESS RESULTS.
        test_result["return_code"] = process_result.returncode
        test_result["stdout"] = process_result.stdout
        test_result["stderr"] = process_result.stderr
        test_result["counts"] = test_utilities.parse_stdout_counts(process_result.stdout)
        test_result["factor_records"] = test_utilities.count_factor_records(factor_path)

        # VALIDATE ALL OUTPUTS AGAINST EXPECTATIONS.
        _validate_return_code(test_result, test_case, process_result.returncode)
        _validate_stderr_empty(test_result, process_result.stderr)
        _validate_factor_record_count(test_result, test_case)
        _validate_error_count(test_result, test_case)
        expected_directory_path: pathlib.Path = test_utilities.g_expected_output_directory_path / name
        _validate_against_expected_baselines(test_result, expected_directory_path, factor_path, print_path, process_result)

    finally:
        # CLEAN UP THE TEMPORARY DIRECTORY.
        try:
            shutil.rmtree(temporary_directory_path)
        except OSError:
            pass

    return test_result

## Check that the process exit code matches the expected value.
##
## If the test case specifies an expected return code ("expect_rc") and the
## actual return code differs, marks the test as failed.
##
## \param[in,out] test_result - Test result dict to update with pass/fail status and failure messages.
## \param[in] test_case - Test case definition dict (may contain "expect_rc").
## \param[in] actual_return_code - The process exit code returned by the COBOL executable.
def _validate_return_code(test_result: dict, test_case: dict, actual_return_code: int):
    # CHECK IF THERE IS AN EXPECTED RETURN CODE.
    expected_return_code: object = test_case.get("expect_rc")
    has_expected_return_code: bool = expected_return_code is not None
    if has_expected_return_code:
        # CHECK IF THE RETURN CODES ARE DIFFERENT.
        return_codes_differ: bool = actual_return_code != expected_return_code
        if return_codes_differ:
            # TRACK THE FAILURE.
            test_result["passed"] = False
            test_result["failures"].append(f"Return code: expected {expected_return_code}, got {actual_return_code}")

## Check that stderr is empty (GnuCOBOL runtime errors go to stderr).
##
## Any non-empty stderr output indicates an unexpected runtime error from the
## GnuCOBOL runtime library and causes the test to fail.
##
## \param[in,out] test_result - Test result dict to update with pass/fail status and failure messages.
## \param[in] stderr_text - Captured standard error text from the process.
def _validate_stderr_empty(test_result: dict, stderr_text: str):
    # CHECK IF THERE IS STDERR OUTPUT.
    has_stderr_output: bool = bool(stderr_text.strip())
    if has_stderr_output:
        # TRACK THE FAILURE.
        # Only a preview of stderr is included to keep failure messages readable.
        STDERR_PREVIEW_LENGTH_IN_CHARACTERS: int = 200
        test_result["passed"] = False
        test_result["failures"].append(f"Unexpected stderr: {stderr_text.strip()[:STDERR_PREVIEW_LENGTH_IN_CHARACTERS]}")

## Check that the number of factor records matches expectations.
##
## If the test case specifies an expected factor record count ("expect_factor_records")
## and the actual count differs, marks the test as failed.
##
## \param[in,out] test_result - Test result dict to update with pass/fail status and failure messages.
## \param[in] test_case - Test case definition dict (may contain "expect_factor_records").
def _validate_factor_record_count(test_result: dict, test_case: dict):
    # CHECK IF THERE IS AN EXPECTED FACTOR RECORD COUNT.
    expected_record_count: object = test_case.get("expect_factor_records")
    has_expected_record_count: bool = expected_record_count is not None
    if has_expected_record_count:
        # CHECK IF THE RECORD COUNTS ARE DIFFERENT.
        actual_record_count: int = test_result["factor_records"]
        record_counts_differ: bool = actual_record_count != expected_record_count
        if record_counts_differ:
            # TRACK THE FAILURE.
            test_result["passed"] = False
            test_result["failures"].append(f"Factor records: expected {expected_record_count}, got {actual_record_count}")

## Check the error count from DISPLAY output against expectations.
##
## If the test case specifies an expected error count ("expect_error_count")
## and the parsed error count from stdout differs, marks the test as failed.
##
## \param[in,out] test_result - Test result dict to update with pass/fail status and failure messages.
## \param[in] test_case - Test case definition dict (may contain "expect_error_count").
def _validate_error_count(test_result: dict, test_case: dict):
    # CHECK IF THERE IS AN EXPECTED ERROR COUNT.
    expected_error_count: object = test_case.get("expect_error_count")
    has_expected_error_count: bool = expected_error_count is not None
    if has_expected_error_count:
        # CHECK IF THE ERROR COUNTS ARE DIFFERENT.
        actual_error_count: object = test_result["counts"].get("error_count")
        error_counts_differ: bool = actual_error_count != expected_error_count
        if error_counts_differ:
            # TRACK THE FAILURE.
            test_result["passed"] = False
            test_result["failures"].append(f"Error count: expected {expected_error_count}, got {actual_error_count}")

## Compare actual outputs against saved expected baselines.
##
## Checks factor file (binary), print file (text with date masking),
## stdout content (line-by-line ignoring whitespace), and return code file.
## Only checks each baseline if the expected file exists; missing baselines
## are silently skipped (allows incremental baseline generation).
##
## \param[in,out] test_result - Test result dict to update with pass/fail status and failure messages.
## \param[in] expected_directory_path - Path to the directory containing expected baseline files.
## \param[in] factor_path - Path to the actual factor output file produced by the COBOL program.
## \param[in] print_path - Path to the actual print output file produced by the COBOL program.
## \param[in] process_result - The completed process result from running the COBOL executable.
def _validate_against_expected_baselines(
    test_result: dict,
    expected_directory_path: pathlib.Path,
    factor_path: str,
    print_path: str,
    process_result: subprocess.CompletedProcess):
    # COMPARE THE FACTOR FILE AGAINST THE EXPECTED BASELINE.
    expected_factor_path: pathlib.Path = expected_directory_path / test_utilities.FACTOR_FILENAME
    expected_factor_file_exists: bool = expected_factor_path.exists()
    if expected_factor_file_exists:
        # CHECK IF TH FACTOR FILES MATCH.
        factor_files_match: bool = test_utilities.files_match_binary(factor_path, str(expected_factor_path))
        if not factor_files_match:
            # TRACK THE FAILURE.
            test_result["passed"] = False
            test_result["failures"].append("Factor file does not match expected output")

    # COMPARE THE PRINT FILE AGAINST THE EXPECTED BASELINE.
    expected_print_path: pathlib.Path = expected_directory_path / test_utilities.PRINT_FILENAME
    expected_print_file_exists: bool = expected_print_path.exists()
    if expected_print_file_exists:
        # CHECK IF THE PRINT FILES MATCH.
        print_files_match: bool = test_utilities.text_files_match(print_path, str(expected_print_path))
        if not print_files_match:
            # TRACK THE FAILURE.
            test_result["passed"] = False
            test_result["failures"].append("Print file does not match expected output (ignoring date)")

    # COMPARE STDOUT LINE-BY-LINE AGAINST THE EXPECTED BASELINE.
    expected_stdout_path: pathlib.Path = expected_directory_path / test_utilities.STDOUT_FILENAME
    expected_stdout_file_exists: bool = expected_stdout_path.exists()
    if expected_stdout_file_exists:
        # READ IN THE EXPECTED TEXT.
        expected_stdout_text: str = ""
        with open(expected_stdout_path, "r") as expected_stdout_file:
            expected_stdout_text = expected_stdout_file.read()

        # CHECK IF THE LINES ARE EQUIVALENT.
        actual_lines: list[str] = [line.strip() for line in process_result.stdout.splitlines()]
        expected_lines: list[str] = [line.strip() for line in expected_stdout_text.splitlines()]
        lines_are_equivalent: bool = actual_lines == expected_lines
        if not lines_are_equivalent:
            # TRACK THE FAILURE.
            test_result["passed"] = False
            test_result["failures"].append("Stdout does not match expected output")

    # COMPARE THE RETURN CODE AGAINST THE EXPECTED BASELINE.
    expected_return_code_path: pathlib.Path = expected_directory_path / test_utilities.RETURN_CODE_FILENAME
    expected_return_code_file_exists: bool = expected_return_code_path.exists()
    if expected_return_code_file_exists:
        # READ IN THE EXPECTED RETURN CODE.
        expected_return_code: int = 0
        with open(expected_return_code_path, "r") as expected_return_code_file:
            expected_return_code = int(expected_return_code_file.read().strip())

        # CHECK IF THE RETURN CODES ARE DIFFERENT.
        return_codes_differ: bool = process_result.returncode != expected_return_code
        if return_codes_differ:
            # TRACK THE FAILURE.
            test_result["passed"] = False
            test_result["failures"].append(f"Return code: expected {expected_return_code}, got {process_result.returncode}")

# ============================================================================
# BASELINE GENERATION
# ============================================================================

## Run all test cases via COBOL executable and save outputs as baselines.
##
## Creates or overwrites expected output files (factor.dat, print.dat,
## stdout.txt, returncode.txt) for each test case. These baselines are
## used by subsequent test runs for regression comparison.
##
## \param[in] verbose - If True, show detailed output for each test case.
def generate_expected_outputs(verbose: bool = False):
    # PRINT THE GENERATION BANNER.
    HEADER_SEPARATOR_CHARACTER: str = "="
    HEADER_SEPARATOR_CHARACTER_COUNT: int = 70
    HEADER_SEPARATOR_LINE: str = HEADER_SEPARATOR_CHARACTER * HEADER_SEPARATOR_CHARACTER_COUNT
    print(HEADER_SEPARATOR_LINE)
    print("GENERATING EXPECTED OUTPUT BASELINES")
    print(HEADER_SEPARATOR_LINE)
    print()

    # RUN EACH TEST CASE AND SAVE THE OUTPUTS AS EXPECTED BASELINES.
    for test_case in test_utilities.TEST_CASES:
        # PROVIDE VISIBILITY INTO THE TEST CASE BEING PROCESSED.
        test_case_name: str = test_case["name"]
        print(f"  Generating: {test_case_name}...")

        # SKIP IF THE INPUT CARD FILE DOES NOT EXIST.
        input_card_file_path: pathlib.Path = test_utilities.g_test_cases_directory_path / test_case_name / test_utilities.INPUT_FILENAME
        input_card_file_exists: bool = input_card_file_path.exists()
        if not input_card_file_exists:
            print(f"    SKIPPED - Input file not found: {input_card_file_path}")
            continue

        # EXECUTE THE COBOL PROGRAM IN A TEMPORARY DIRECTORY.
        # A temporary directory is used to hold some files that will be cleaned up after the test.
        temporary_directory_path: str = tempfile.mkdtemp(prefix = f"clreb020_gen_{test_case_name}_")
        try:
            # BUILD THE ENVIRONMENT FOR RUNNING THE COBOL PROGRAM.
            process_environment: dict = os.environ.copy()
            process_environment["PATH"] = str(g_build_directory_path) + os.pathsep + process_environment.get("PATH", "")
            process_environment[DD_INPUT_ENVIRONMENT_VARIABLE_NAME] = str(input_card_file_path)
            factor_path: str = os.path.join(temporary_directory_path, test_utilities.FACTOR_FILENAME)
            process_environment[DD_FACTOR_ENVIRONMENT_VARIABLE_NAME] = factor_path
            print_path: str = os.path.join(temporary_directory_path, test_utilities.PRINT_FILENAME)
            process_environment[DD_PRINT_ENVIRONMENT_VARIABLE_NAME] = print_path

            # RUN THE COBOL EXECUTABLE.
            process_result: subprocess.CompletedProcess = subprocess.run(
                [str(g_cobol_executable_path)],
                env = process_environment,
                capture_output = True,
                text = True,
                timeout = test_utilities.SUBPROCESS_TIMEOUT_IN_SECONDS,
                cwd = temporary_directory_path,)

            # ENSURE THE EXPECTED OUTPUT DIRECTORY EXISTS.
            expected_directory_path: pathlib.Path = test_utilities.g_expected_output_directory_path / test_case_name
            expected_directory_path.mkdir(parents = True, exist_ok = True)

            # SAVE ALL OUTPUT FILES AS EXPECTED BASELINES.
            _save_baseline_file(factor_path, expected_directory_path / test_utilities.FACTOR_FILENAME, "factor.dat")
            _save_baseline_file(print_path, expected_directory_path / test_utilities.PRINT_FILENAME, "print.dat")

            # SAVE THE STDOUT AND RETURN CODE AS BASELINES.
            with open(str(expected_directory_path / test_utilities.STDOUT_FILENAME), "w") as stdout_file:
                stdout_file.write(process_result.stdout)
            print(f"    Saved stdout.txt")

            with open(str(expected_directory_path / test_utilities.RETURN_CODE_FILENAME), "w") as return_code_file:
                return_code_file.write(str(process_result.returncode))
            print(f"    Saved returncode.txt (RC={process_result.returncode})")

            # WARN IF THE PROCESS PRODUCED STDERR OUTPUT.
            has_stderr_output: bool = bool(process_result.stderr.strip())
            if has_stderr_output:
                print(f"    WARNING: stderr: {process_result.stderr.strip()[:200]}")

            # PRINT VERBOSE DETAILS IF REQUESTED.
            if verbose:
                print(f"    Stdout: {repr(process_result.stdout[:200])}")
                record_counts: dict = test_utilities.parse_stdout_counts(process_result.stdout)
                print(f"    Counts: {record_counts}")

        finally:
            # CLEAN UP THE TEMPORARY DIRECTORY.
            try:
                shutil.rmtree(temporary_directory_path)
            except OSError:
                pass

        print()

    # PRINT THE COMPLETION BANNER.
    print("Expected output generation complete.")
    print()

## Copy an output file to the expected baselines directory.
##
## If the source file exists and is non-empty, copies it. Otherwise,
## creates an empty file to indicate that empty output is expected.
##
## \param[in] source_path - Path to the output file produced by the test run.
## \param[in] destination_path - Path where the baseline file should be saved.
## \param[in] display_name - Human-readable filename shown in progress messages (e.g., "factor.dat").
def _save_baseline_file(source_path: str, destination_path: pathlib.Path, display_name: str):
    # CHECK IF THE SOURCE FILE HAS CONTENT.
    source_file_has_content: bool = os.path.exists(source_path) and os.path.getsize(source_path) > 0
    if source_file_has_content:
        # COPY THE SOURCE FILE TO THE DESTINATION PATH.
        shutil.copy2(source_path, str(destination_path))

        # GET THE SIZE OF THE SOURCE FILE IN BYTES.
        file_size_in_bytes: int = os.path.getsize(source_path)

        # CHECK IF THIS IS THE FACTOR FILE OR ANOTHER FILE.
        is_factor_file: bool = display_name == test_utilities.FACTOR_FILENAME
        if is_factor_file:
            # CALCULATE THE RECORD COUNT.
            record_count: int = file_size_in_bytes // test_utilities.FACTOR_RECORD_SIZE_IN_BYTES
            print(f"    Saved {display_name} ({file_size_in_bytes} bytes, {record_count} records)")
        else:
            # JUST DISPLAY THE FILE SIZE.
            print(f"    Saved {display_name} ({file_size_in_bytes} bytes)")
    else:
        # CREATE AN EMPTY FILE TO INDICATE THAT THE FILE IS EMPTY.
        with open(str(destination_path), "wb"):
            pass
        print(f"    Saved {display_name} (0 bytes, empty)")

# ============================================================================
# TEST SUITE RUNNER
# ============================================================================

## Run all 50 test cases and print a summary report.
##
## \param[in] verbose - If True, show detailed output for each test case.
## \return 0 if all tests passed, 1 if any test failed.
def run_all_tests(verbose: bool = False) -> int:
    # PRINT THE HARNESS BANNER.
    HEADER_SEPARATOR_CHARACTER: str = "="
    HEADER_SEPARATOR_CHARACTER_COUNT: int = 70
    HEADER_SEPARATOR_LINE: str = HEADER_SEPARATOR_CHARACTER * HEADER_SEPARATOR_CHARACTER_COUNT
    print(HEADER_SEPARATOR_LINE)
    print("CLREB020 COBOL TEST HARNESS")
    print(HEADER_SEPARATOR_LINE)
    print()
    print(f"Executable:    {g_cobol_executable_path}")
    print(f"Test cases:    {test_utilities.g_test_cases_directory_path}")
    print(f"Expected dir:  {test_utilities.g_expected_output_directory_path}")
    print(f"Build DLLs:    {g_build_directory_path}")
    print()

    # VERIFY THAT THE COBOL EXECUTABLE EXISTS.
    ERROR_EXIT_CODE: int = 1
    cobol_executable_exists: bool = g_cobol_executable_path.exists()
    if not cobol_executable_exists:
        print(f"ERROR: Executable not found: {g_cobol_executable_path}")
        print("Please build CLREB020.exe first.")
        return ERROR_EXIT_CODE

    # CHECK WHETHER EXPECTED BASELINES EXIST.
    has_expected_baselines: bool = any(
        (test_utilities.g_expected_output_directory_path / test_case["name"] / test_utilities.FACTOR_FILENAME).exists()
        for test_case in test_utilities.TEST_CASES
    )
    if not has_expected_baselines:
        print("WARNING: No expected output files found.")
        print("Run with --generate-expected to create baselines first.")
        print()

    # RUN EACH TEST CASE AND ACCUMULATE RESULTS.
    total_test_count: int = len(test_utilities.TEST_CASES)
    passed_count: int = 0
    failed_count: int = 0
    failure_details: list[tuple[str, list[str]]] = []

    MINOR_SEPARATOR_CHARACTER: str = "-"
    MINOR_SEPARATOR_LINE: str = MINOR_SEPARATOR_CHARACTER * HEADER_SEPARATOR_CHARACTER_COUNT
    print(MINOR_SEPARATOR_LINE)

    for test_case in test_utilities.TEST_CASES:
        # RUN THE TEST CASE.
        test_case_name: str = test_case["name"]
        test_case_description: str = test_case["description"]
        test_case_result: dict = run_test(test_case, verbose = verbose)

        # CHECK IF THE TEST PASSED OR FAILED.
        if test_case_result["passed"]:
            status: str = "PASS"
            passed_count += 1
        else:
            status = "FAIL"
            failed_count += 1
            failure_details.append((test_case_name, test_case_result["failures"]))

        # FORMAT THE RETURN CODE AND COUNTS FOR DISPLAY.
        return_code_display: str = (
            f"RC={test_case_result['return_code']}"
            if test_case_result["return_code"] is not None
            else "RC=?"
        )
        record_counts: dict = test_case_result["counts"]
        counts_display: str = ""
        has_input_count: bool = record_counts.get("input_count") is not None
        if has_input_count:
            counts_display = (
                f"in={record_counts['input_count']} "
                f"out={record_counts.get('output_count', '?')} "
                f"err={record_counts.get('error_count', '?')}"
            )

        # PRINT THE OVERALL SUMMARY FOR THIS TEST.
        print(f"  [{status}] {test_case_name:<25} {return_code_display:<8} {counts_display}")

        # PRINT VERBOSE DETAILS IF REQUESTED.
        if verbose:
            print(f"         {test_case_description}")

            # Standard output should be printed if available.
            has_stdout: bool = bool(test_case_result["stdout"])
            if has_stdout:
                # PRINT EACH LINE OF THE STANDARD OUTPUT.
                standard_output_lines: list[str] = test_case_result["stdout"].strip().splitlines()
                for line in standard_output_lines:
                    print(f"         stdout: {line}")

            # Standard error should be printed if available.
            has_stderr: bool = bool(test_case_result["stderr"])
            if has_stderr:
                # PRINT EACH LINE OF THE STANDARD ERROR.
                standard_error_lines: list[str] = test_case_result["stderr"].strip().splitlines()
                for line in standard_error_lines:
                    print(f"         stderr: {line}")

            # Failures should be printed if any exist.
            has_failures: bool = bool(test_case_result["failures"])
            if has_failures:
                # PRINT EACH FAILURE.
                for failure in test_case_result["failures"]:
                    print(f"         FAILURE: {failure}")
            print()

    # PRINT THE SUMMARY REPORT.
    print(MINOR_SEPARATOR_LINE)
    print()
    print(f"Results: {passed_count} passed, {failed_count} failed, {total_test_count} total")
    print()

    # PRINT FAILURE DETAILS IF ANY TESTS FAILED.
    if failure_details:
        # PRINT THE FAILURE DETAILS.
        print("FAILURES:")
        print()
        for test_case_name, failures in failure_details:
            # PRINT FAILURES FOR THE CURRENT TEST CASE.
            print(f"  {test_case_name}:")
            for failure in failures:
                print(f"    - {failure}")
            print()

    # RETURN THE APPROPRIATE EXIT CODE.
    SUCCESS_EXIT_CODE: int = 0
    exit_code: int = SUCCESS_EXIT_CODE if failed_count == 0 else ERROR_EXIT_CODE
    return exit_code

# ============================================================================
# MAIN ENTRY POINT
# ============================================================================

## Parse command-line arguments and run the COBOL test harness.
##
## \return Exit code (0 if all tests passed, 1 if any test failed).
def main() -> int:
    # PARSE THE COMMAND LINE ARGUMENTS.
    command_line_argument_parser: argparse.ArgumentParser = argparse.ArgumentParser(
        description = "COBOL Test Harness for CLREB020 - Equalization Factor Editor")
    command_line_argument_parser.add_argument(
        "--generate-expected",
        action = "store_true",
        help = "Run all tests and save outputs as expected baselines")
    command_line_argument_parser.add_argument(
        "--verbose", "-v",
        action = "store_true",
        help = "Show detailed output for each test case")
    command_line_arguments: argparse.Namespace = command_line_argument_parser.parse_args()

    # GENERATE EXPECTED BASELINES IF REQUESTED.
    if command_line_arguments.generate_expected:
        generate_expected_outputs(verbose = command_line_arguments.verbose)
        print()

    # RUN ALL TESTS AND RETURN THE EXIT CODE.
    exit_code: int = run_all_tests(verbose = command_line_arguments.verbose)
    return exit_code

if __name__ == "__main__":
    sys.exit(main())
