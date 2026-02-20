#!/usr/bin/env python3
## \file java_test_harness.py
## Java Test Harness for CLREB020 - Equalization Factor Edit and List Program.
##
## Runs the Java conversion (tax-extension JAR) against the full test suite and
## validates outputs against expected baselines. This is the Java-side equivalent
## of cobol_test_harness.py, testing the Java implementation independently without
## needing the COBOL executable.
##
## Usage:
##     python Testing/CLREB020/java_test_harness.py [--generate-expected] [--verbose]
##
## Previously Known COBOL/Java Differences (now resolved):
##     - invalid_zero_factor and boundary_year_00 previously differed because Java
##       used numeric > 0 comparison while COBOL uses alphanumeric comparison.
##     - Fixed by changing Java validation to use >= 0 (COBOL-compatible default).
##     - The Java-specific baselines (expected_output_java/) are no longer needed.

import os
import sys
import subprocess
import tempfile
import shutil
import argparse
import pathlib

import test_utilities

# ============================================================================
# JAVA-SPECIFIC CONFIGURATION
# ============================================================================
## Root directory of the Java tax extension project.
g_java_project_directory_path: pathlib.Path = test_utilities.g_project_root_absolute_path / "tax_extension_java"
## Path to the compiled tax-extension JAR file.
g_java_jar_path: pathlib.Path = g_java_project_directory_path / "target" / "tax-extension-0.1.0-SNAPSHOT.jar"
## Alternate baseline directory for tests with known COBOL/Java differences.
## Currently unused — all differences have been resolved, so every test uses
## the shared expected_output/ baselines.
g_expected_output_java_directory_path: pathlib.Path = test_utilities.g_test_harness_directory_absolute_path / "expected_output_java"
## Tests with known COBOL/Java behavioral differences.
## These use expected_output_java/ instead of expected_output/.
## Currently empty: all behavioral differences have been resolved.
JAVA_SPECIFIC_TESTS: dict[str, str] = {}

# ============================================================================
# JAVA-SPECIFIC HELPERS
# ============================================================================

## Return the correct expected-output directory for a given test.
##
## Tests listed in JAVA_SPECIFIC_TESTS use Java-specific baselines from
## expected_output_java/. All others use the shared COBOL baselines from
## expected_output/. This allows the Java harness to share baselines with
## the COBOL harness for identical-behavior tests while maintaining separate
## baselines where the two implementations intentionally diverge.
##
## \param[in] test_name - Name of the test case.
## \return Path to the expected output directory.
def get_expected_directory(test_name: str) -> pathlib.Path:
    # CHECK WHETHER THIS TEST HAS JAVA-SPECIFIC BASELINES.
    is_java_specific_test: bool = test_name in JAVA_SPECIFIC_TESTS
    if is_java_specific_test:
        # CHECK IF THE JAVA-SPECIFIC DIRECTORY EXISTS.
        java_specific_directory_path: pathlib.Path = g_expected_output_java_directory_path / test_name
        java_specific_directory_exists: bool = java_specific_directory_path.exists()
        if java_specific_directory_exists:
            # RETURN THE JAVA-SPECIFIC DIRECTORY.
            return java_specific_directory_path
    
    # RETURN THE GENERIC EXPECTED OUTPUT DIRECTORY.
    generic_expected_directory_path: pathlib.Path = test_utilities.g_expected_output_directory_path / test_name
    return generic_expected_directory_path

# ============================================================================
# TEST RUNNER
# ============================================================================

## Run a single test case via the Java JAR and validate outputs.
##
## Executes the tax-extension JAR with input/output file paths as command-line
## arguments (unlike the COBOL harness which uses DD environment variables).
## Validates return code, stderr, factor record count, error count, and
## compares outputs against expected baselines.
##
## Stdout comparison is semantic rather than textual: COBOL formats counts as
## "+004" (COMP-3 DISPLAY) while Java writes plain "4". Both are parsed to
## integers for comparison.
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
##     - "java_specific" (bool): True if this test uses Java-specific baselines.
def run_test(test_case: dict, verbose: bool = False) -> dict:
    # INITIALIZE THE TEST RESULT.
    name: str = test_case["name"]
    is_java_specific: bool = name in JAVA_SPECIFIC_TESTS
    test_result: dict = {
        "passed": True,
        "failures": [],
        "return_code": None,
        "stdout": "",
        "stderr": "",
        "factor_records": 0,
        "counts": {},
        "java_specific": is_java_specific,
    }

    # CHECK THAT THE INPUT CARD FILE EXISTS.
    input_card_file_path: pathlib.Path = test_utilities.g_test_cases_directory_path / name / test_utilities.INPUT_FILENAME
    input_card_file_exists: bool = input_card_file_path.exists()
    if not input_card_file_exists:
        # INDICATE THAT THE TEST FAILED BECAUSE THE INPUT CARD FILE WAS NOT FOUND.
        test_result["passed"] = False
        test_result["failures"].append(f"Input file not found: {input_card_file_path}")
        return test_result

    # EXECUTE THE JAVA PROGRAM IN A TEMPORARY DIRECTORY.
    # A temporary directory is used to hold some files that will be cleaned up after the test.
    temporary_directory_path: str = tempfile.mkdtemp(prefix = f"clreb020_java_{name}_")
    try:
        # RUN THE JAVA JAR WITH INPUT AND OUTPUT FILE PATHS.
        try:
            factor_path: str = os.path.join(temporary_directory_path, test_utilities.FACTOR_FILENAME)
            print_path: str = os.path.join(temporary_directory_path, test_utilities.PRINT_FILENAME)
            process_result: subprocess.CompletedProcess = subprocess.run(
                [
                    "java", "-jar", str(g_java_jar_path),
                    str(input_card_file_path), print_path, factor_path,
                ],
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
            test_result["failures"].append("Java runtime (java) not found")
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
        expected_directory_path: pathlib.Path = get_expected_directory(name)
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
## \param[in] actual_return_code - The process exit code returned by the Java program.
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

## Check that stderr is empty (unexpected errors go to stderr).
##
## Any non-empty stderr output indicates an unexpected runtime error from the
## Java program and causes the test to fail.
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
## stdout content (semantic count comparison rather than textual — because
## COBOL uses COMP-3 sign format '+004' while Java writes '4'), and
## return code file. For sequence-error tests, also verifies that the
## expected diagnostic messages appear in stdout.
##
## \param[in,out] test_result - Test result dict to update with pass/fail status and failure messages.
## \param[in] expected_directory_path - Path to the directory containing expected baseline files.
## \param[in] factor_path - Path to the actual factor output file produced by the Java program.
## \param[in] print_path - Path to the actual print output file produced by the Java program.
## \param[in] process_result - The completed process result from running the Java JAR.
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
        # CHECK IF THE FACTOR FILES MATCH.
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

    # COMPARE STDOUT COUNTS SEMANTICALLY AGAINST THE EXPECTED BASELINE.
    # COBOL formats counts as "+004" while Java writes plain "4", so we parse
    # both sides to integers rather than comparing textually.
    expected_stdout_path: pathlib.Path = expected_directory_path / test_utilities.STDOUT_FILENAME
    expected_stdout_file_exists: bool = expected_stdout_path.exists()
    if expected_stdout_file_exists:
        # READ IN THE EXPECTED TEXT.
        with open(expected_stdout_path, "r") as expected_stdout_file:
            expected_stdout_text: str = expected_stdout_file.read()

        # COMPARE EACH COUNT FIELD.
        expected_counts: dict = test_utilities.parse_stdout_counts(expected_stdout_text)
        actual_counts: dict = test_result["counts"]
        for count_key in ["input_count", "output_count", "error_count"]:
            # CHECK IF BOTH EXPECTED AND ACTUAL VALUES ARE PRESENT.
            expected_value: object = expected_counts.get(count_key)
            actual_value: object = actual_counts.get(count_key)
            both_values_present: bool = expected_value is not None and actual_value is not None
            if both_values_present:
                # CHECK IF THE VALUES DIFFER.
                values_differ: bool = expected_value != actual_value
                if values_differ:
                    # TRACK THE FAILURE.
                    test_result["passed"] = False
                    test_result["failures"].append(f"Stdout {key}: expected {expected_value}, got {actual_value}")

            # CHECK IF THE EXPECTED VALUE IS PRESENT BUT THE ACTUAL VALUE IS MISSING.
            expected_present_but_actual_missing: bool = expected_value is not None and actual_value is None
            if expected_present_but_actual_missing:
                # TRACK THE FAILURE.
                test_result["passed"] = False
                test_result["failures"].append(f"Stdout missing {count_key}")

        # VALIDATE SEQUENCE ERROR DIAGNOSTIC MESSAGES.
        _validate_sequence_error_messages(test_result, expected_stdout_text, process_result.stdout)

    # COMPARE THE RETURN CODE AGAINST THE EXPECTED BASELINE.
    expected_return_code_path: pathlib.Path = expected_directory_path / test_utilities.RETURN_CODE_FILENAME
    expected_return_code_file_exists: bool = expected_return_code_path.exists()
    if expected_return_code_file_exists:
        # READ IN THE EXPECTED RETURN CODE.
        with open(expected_return_code_path, "r") as expected_return_code_file:
            expected_return_code: int = int(expected_return_code_file.read().strip())

        # CHECK IF THE RETURN CODES ARE DIFFERENT.
        return_codes_differ: bool = process_result.returncode != expected_return_code
        if return_codes_differ:
            # TRACK THE FAILURE.
            test_result["passed"] = False
            test_result["failures"].append(
                f"Return code file mismatch: expected {expected_return_code}, "
                f"got {process_result.returncode}")

## Verify that sequence-error diagnostic messages appear in actual stdout.
##
## When the expected baseline contains 'CARDS OUT OF SEQUENCE', the Java
## output must also contain it. Similarly for 'CURRENT CARD'. These messages
## are emitted by both COBOL (via DISPLAY) and Java when a sequence error
## triggers RETURN-CODE 16.
##
## \param[in,out] test_result - Test result dict to update with pass/fail status and failure messages.
## \param[in] expected_stdout - The expected stdout text from the baseline file.
## \param[in] actual_stdout - The actual stdout text captured from the Java program.
def _validate_sequence_error_messages(test_result: dict, expected_stdout: str, actual_stdout: str):
    # VERIFY THAT SEQUENCE-ERROR DIAGNOSTIC MESSAGES APPEAR IN ACTUAL STDOUT.
    expected_has_sequence_error: bool = "CARDS OUT OF SEQUENCE" in expected_stdout
    if expected_has_sequence_error:
        # CHECK IF THE ACTUAL OUTPUT INDICATES A SEQUENCE ERROR.
        actual_has_sequence_error: bool = "CARDS OUT OF SEQUENCE" in actual_stdout
        if not actual_has_sequence_error:
            # TRACK THE FAILURE.
            test_result["passed"] = False
            test_result["failures"].append("Stdout missing 'CARDS OUT OF SEQUENCE' message")

        # CHECK IF THE EXPECTED OUTPUT INDICATES A CURRENT CARD.
        expected_has_current_card: bool = "CURRENT CARD" in expected_stdout
        if expected_has_current_card:
            # CHECK IF THE ACTUAL OUTPUT INDICATES A CURRENT CARD.
            actual_has_current_card: bool = "CURRENT CARD" in actual_stdout
            if not actual_has_current_card:
                # TRACK THE FAILURE.
                test_result["passed"] = False
                test_result["failures"].append("Stdout missing 'CURRENT CARD' message")

# ============================================================================
# BASELINE GENERATION
# ============================================================================

## Run Java-specific tests and save Java-specific baselines.
##
## Only generates baselines for tests listed in JAVA_SPECIFIC_TESTS. All
## other tests use the shared COBOL baselines from expected_output/.
## Currently JAVA_SPECIFIC_TESTS is empty, so this is a no-op.
##
## \param[in] verbose - If True, show detailed output for each test case.
def generate_expected_outputs(verbose: bool = False):
    # PRINT THE GENERATION BANNER.
    HEADER_SEPARATOR_CHARACTER: str = "="
    HEADER_SEPARATOR_CHARACTER_COUNT: int = 70
    HEADER_SEPARATOR_LINE: str = HEADER_SEPARATOR_CHARACTER * HEADER_SEPARATOR_CHARACTER_COUNT
    print(HEADER_SEPARATOR_LINE)
    print("GENERATING JAVA-SPECIFIC EXPECTED OUTPUT BASELINES")
    print(HEADER_SEPARATOR_LINE)
    print()
    print("Only generating baselines for tests with known COBOL/Java differences.")
    print(f"Other tests use the COBOL baselines in expected_output/.")
    print()

    # GENERATE BASELINES ONLY FOR TESTS WITH KNOWN COBOL/JAVA DIFFERENCES.
    for test_case in test_utilities.TEST_CASES:
        # CHECK IF THE TEST CASE IS JAVA-SPECIFIC.
        test_case_name: str = test_case["name"]
        is_java_specific_test: bool = test_case_name in JAVA_SPECIFIC_TESTS
        if not is_java_specific_test:
            # SKIP THE TEST CASE IF IT IS NOT JAVA-SPECIFIC.
            continue

        # PROVIDE VISIBILITY INTO THE TEST CASE BEING PROCESSED.
        print(f"  Generating: {test_case_name}...")
        print(f"    Reason: {JAVA_SPECIFIC_TESTS[test_case_name]}")

        # SKIP IF THE INPUT CARD FILE DOES NOT EXIST.
        input_card_file_path: pathlib.Path = test_utilities.g_test_cases_directory_path / test_case_name / test_utilities.INPUT_FILENAME
        input_card_file_exists: bool = input_card_file_path.exists()
        if not input_card_file_exists:
            print(f"    SKIPPED - Input file not found: {input_card_file_path}")
            continue

        # ENSURE THE EXPECTED OUTPUT DIRECTORY EXISTS.
        expected_directory_path: pathlib.Path = g_expected_output_java_directory_path / test_case_name
        expected_directory_path.mkdir(parents = True, exist_ok = True)

        # EXECUTE THE JAVA PROGRAM IN A TEMPORARY DIRECTORY.
        # A temporary directory is used to hold some files that will be cleaned up after the test.
        temporary_directory_path: str = tempfile.mkdtemp(prefix = f"clreb020_jgen_{test_case_name}_")
        try:
            # RUN THE JAVA JAR AND SAVE ALL OUTPUTS AS BASELINES.
            factor_path: str = os.path.join(temporary_directory_path, test_utilities.FACTOR_FILENAME)
            print_path: str = os.path.join(temporary_directory_path, test_utilities.PRINT_FILENAME)
            process_result: subprocess.CompletedProcess = subprocess.run(
                [
                    "java", "-jar", str(g_java_jar_path),
                    str(input_card_file_path), print_path, factor_path,
                ],
                capture_output = True,
                text = True,
                timeout = test_utilities.SUBPROCESS_TIMEOUT_IN_SECONDS,
                cwd = temporary_directory_path)

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

            # PRINT VERBOSE DETAILS IF REQUESTED.
            if verbose:
                print(f"    Stdout: {repr(process_result.stdout[:200])}")

        finally:
            # CLEAN UP THE TEMPORARY DIRECTORY.
            try:
                shutil.rmtree(temporary_directory_path)
            except OSError:
                pass

        print()

    # PRINT THE COMPLETION BANNER.
    print("Java-specific baseline generation complete.")
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

## Run all 50 test cases via Java and print a summary report.
##
## \param[in] verbose - If True, show detailed output for each test case.
## \return 0 if all tests passed, 1 if any test failed.
def run_all_tests(verbose: bool = False) -> int:
    # PRINT THE HARNESS BANNER.
    HEADER_SEPARATOR_CHARACTER: str = "="
    HEADER_SEPARATOR_CHARACTER_COUNT: int = 70
    HEADER_SEPARATOR_LINE: str = HEADER_SEPARATOR_CHARACTER * HEADER_SEPARATOR_CHARACTER_COUNT
    print(HEADER_SEPARATOR_LINE)
    print("CLREB020 JAVA TEST HARNESS")
    print(HEADER_SEPARATOR_LINE)
    print()
    print(f"Java JAR:      {g_java_jar_path}")
    print(f"Test cases:    {test_utilities.g_test_cases_directory_path}")
    print(f"Expected:      {test_utilities.g_expected_output_directory_path}")
    print(f"Java-specific: {g_expected_output_java_directory_path}")
    print()

    # VERIFY THAT THE JAVA JAR EXISTS.
    ERROR_EXIT_CODE: int = 1
    java_jar_exists: bool = g_java_jar_path.exists()
    if not java_jar_exists:
        print(f"ERROR: JAR not found: {g_java_jar_path}")
        print("Build with: cd tax_extension_java && mvn clean package -q")
        return ERROR_EXIT_CODE

    # VERIFY THAT JAVA IS AVAILABLE ON THE PATH.
    try:
        # RUN THE JAVA VERSION CHECK.
        # A quick version check should complete almost instantly; 10 seconds
        # is a generous upper bound for a slow or cold JVM start.
        VERSION_CHECK_TIMEOUT_IN_SECONDS: int = 10
        subprocess.run(
            ["java", "-version"],
            capture_output = True,
            timeout = VERSION_CHECK_TIMEOUT_IN_SECONDS)
    except (FileNotFoundError, subprocess.TimeoutExpired):
        print("ERROR: Java runtime not found. Install JDK 17+.")
        return ERROR_EXIT_CODE

    # RUN EACH TEST CASE AND ACCUMULATE RESULTS.
    total_test_count: int = len(test_utilities.TEST_CASES)
    passed_count: int = 0
    failed_count: int = 0
    java_specific_count: int = 0
    failure_details: list[tuple[str, list[str]]] = []

    MINOR_SEPARATOR_CHARACTER: str = "-"
    MINOR_SEPARATOR_LINE: str = MINOR_SEPARATOR_CHARACTER * HEADER_SEPARATOR_CHARACTER_COUNT
    print(MINOR_SEPARATOR_LINE)

    for test_case in test_utilities.TEST_CASES:
        # RUN THE TEST CASE.
        test_case_name: str = test_case["name"]
        test_case_result: dict = run_test(test_case, verbose = verbose)

        # CHECK IF THE TEST PASSED OR FAILED.
        if test_case_result["passed"]:
            # CHECK IF THIS TEST USES JAVA-SPECIFIC BASELINES.
            is_java_specific: bool = test_case_result["java_specific"]
            if is_java_specific:
                status: str = "JSPC"
                java_specific_count += 1
            else:
                status = "PASS"
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
        # "JSPC" is 4 chars; "PASS"/"FAIL" are 4 chars — no padding needed.
        print(f"  [{status:4s}] {test_case_name:<25} {return_code_display:<8} {counts_display}")

        # PRINT VERBOSE FAILURE DETAILS IF REQUESTED.
        has_failures: bool = verbose and bool(test_case_result["failures"])
        if has_failures:
            for failure in test_case_result["failures"]:
                print(f"         FAILURE: {failure}")

    # PRINT THE SUMMARY REPORT.
    print(MINOR_SEPARATOR_LINE)
    print()
    print(
        f"Results: {passed_count} passed, {failed_count} failed, "
        f"{java_specific_count} java-specific baselines, {total_test_count} total"
    )
    print()

    # PRINT JAVA-SPECIFIC TEST DETAILS IF ANY EXIST.
    has_java_specific_tests: bool = java_specific_count > 0
    if has_java_specific_tests:
        print("JAVA-SPECIFIC TESTS (using Java baselines, not COBOL):")
        for test_case_name, test_case_description in JAVA_SPECIFIC_TESTS.items():
            print(f"  {test_case_name}: {test_case_description}")
        print()

    # PRINT FAILURE DETAILS IF ANY TESTS FAILED.
    if failure_details:
        # PRINT THE FAILURE DETAILS.
        print("FAILURES:")
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

## Parse command-line arguments and run the Java test harness.
##
## \return Exit code (0 if all tests passed, 1 if any test failed).
def main() -> int:
    # PARSE THE COMMAND LINE ARGUMENTS.
    command_line_argument_parser: argparse.ArgumentParser = argparse.ArgumentParser(
        description = "Java Test Harness for CLREB020 - Equalization Factor Editor")
    command_line_argument_parser.add_argument(
        "--generate-expected",
        action = "store_true",
        help = "Run Java-specific tests and save Java baselines")
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
