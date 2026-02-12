      *================================================================
      * PROGRAM: CLREB020 - Equalization Factor Edit and List
      *================================================================
      *
      * BUSINESS PURPOSE:
      *   This program is part of Cook County's property tax extension
      *   system. It reads equalization factor cards (input data
      *   representing tax equalization multipliers for assessment
      *   districts), validates them, writes valid factors to a disk
      *   file for downstream processing, and produces a printed
      *   listing/audit report.
      *
      *   Equalization factors are multipliers applied to assessed
      *   property values to ensure uniformity across different
      *   assessment districts ("quads") within Cook County. For
      *   example, a factor of 2.9744 means assessed values in that
      *   quad are multiplied by 2.9744 to reach the equalized value.
      *
      *   The output factor file (FACTOR-FILE) is consumed by
      *   downstream programs ASHMA850, ASHMA855, and ASHMA857 which
      *   use these factors in the actual tax extension calculations.
      *
      * INPUT:   CARD-FILE  - 80-byte records containing year, quad,
      *                       and equalization factor
      * OUTPUT:  FACTOR-FILE - 21-byte records with validated factors
      *          PRINT-FILE  - 133-byte report listing (audit trail)
      *
      * VALIDATION RULES:
      *   1. Year (CD-YR) must be numeric and greater than zero
      *   2. Factor (CD-FACTOR) must be numeric and greater than zero
      *   3. Quad (CD-QUAD) must be 1, 2, 3, or 4
      *   4. Cards must be in ascending sequence by year+quad
      *
      * ERROR HANDLING:
      *   - Validation failures: card is listed with "NOT NUMERIC"
      *     error message; no factor record is written
      *   - Sequence errors: program terminates immediately with
      *     RETURN-CODE = 16 (critical error in mainframe conventions)
      *
      * AUTHORS: Russ Dober, Ron Urbaniak
      * DATE:    September 25, 1978
      *================================================================
      *
      *================================================================
      * COBOL LANGUAGE NOTES (for non-COBOL programmers):
      *
      * COBOL programs have four required DIVISIONS, always in order:
      *   1. IDENTIFICATION DIVISION - metadata (program name, author)
      *   2. ENVIRONMENT DIVISION    - hardware/OS config, file paths
      *   3. DATA DIVISION           - all variable/data declarations
      *   4. PROCEDURE DIVISION      - executable logic (the "code")
      *
      * COBOL is a FIXED-FORMAT language. Column positions matter:
      *   Cols 1-6:  Sequence numbers (ignored by compiler)
      *   Col 7:     Indicator: * = comment, - = continuation,
      *              D = debug line, space = normal code
      *   Cols 8-11: Area A (division/section/paragraph headers,
      *              01-level data items must start here)
      *   Cols 12-72: Area B (all other statements and data items)
      *   Cols 73-80: Ignored (historically used for card ID)
      *
      * KEY DIFFERENCES FROM C/C++/C#/Python:
      *   - No pointers, no heap allocation, no dynamic memory
      *   - All data is statically declared with fixed sizes
      *   - Variables have formatted types (PIC clauses) not raw types
      *   - String handling is fixed-width, space-padded (no null term)
      *   - Numeric types can be zoned decimal or packed (COMP-3)
      *   - File I/O is record-oriented, not stream-oriented
      *   - "Paragraphs" are like labels/functions but with no params
      *     or return values; they share all data via global scope
      *   - PERFORM = function call; PERFORM THRU = call a range of
      *     consecutive paragraphs; PERFORM UNTIL = while loop
      *================================================================
00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID. CLREB020.
00003  AUTHOR. RUSS DOBER - RON URBANIAK.
00004  DATE-WRITTEN. SEP 25, 1978.
00005  DATE-COMPILED.
      *----------------------------------------------------------------
      * Original remarks preserved from the 1978 source:
      *----------------------------------------------------------------
00006 *REMARKS. THIS PROGRAM WILL EDIT AND LIST EQUALIZATION
00007 *         FACTORS AND PUT THEM OUT ON A DISK FILE.
00008 *    SKIP3
      *
      *================================================================
      * ENVIRONMENT DIVISION
      *   Specifies the computing environment and file assignments.
      *
      * SOURCE/OBJECT-COMPUTER: Originally IBM System/370 mainframe.
      *   Now compiled with GnuCOBOL targeting x86 Windows.
      *
      * FILE-CONTROL / SELECT statements:
      *   Each SELECT statement declares a "logical file" -- which is
      *   just COBOL's name for a file handle that the program uses
      *   internally. Think of it as declaring a FILE* variable in C.
      *
      *   The SELECT statement has two parts:
      *     SELECT <logical-name> ASSIGN TO <external-name>.
      *
      *   - <logical-name> (e.g., CARD-FILE) is the COBOL variable
      *     name. This is what you use in OPEN, READ, WRITE, and
      *     CLOSE statements throughout the program. It is NOT
      *     declared anywhere else -- the SELECT statement itself
      *     is the declaration. (Unlike C where you write
      *     "FILE* cardFile;", COBOL combines declaration and
      *     external binding in one SELECT statement.)
      *
      *   - <external-name> (e.g., UT-S-CARDS) tells the runtime
      *     WHERE the actual file lives on disk. The "UT-S-" prefix
      *     is an IBM convention meaning Utility (UT), Sequential (S).
      *     On the mainframe, this maps to a JCL DD statement.
      *     Under GnuCOBOL, the runtime strips the prefix and maps
      *     to an environment variable:
      *       UT-S-CARDS  -> env var DD_CARDS  (file path)
      *       UT-S-PRINT  -> env var DD_PRINT  (file path)
      *       UT-S-FACTOR -> env var DD_FACTOR (file path)
      *
      *   HOW TO RUN THIS PROGRAM with actual files:
      *     On Windows (GnuCOBOL):
      *       set DD_CARDS=C:\data\cards.dat
      *       set DD_PRINT=C:\data\report.txt
      *       set DD_FACTOR=C:\data\factors.dat
      *       CLREB020.exe
      *     On Linux:
      *       DD_CARDS=cards.dat DD_PRINT=report.txt \
      *         DD_FACTOR=factors.dat ./CLREB020
      *
      *   IMPORTANT: The SELECT only creates the file handle. The
      *   file's RECORD STRUCTURE (what the data looks like) is
      *   declared separately in the DATA DIVISION with an FD
      *   (File Description) entry. Each file MUST have its own FD
      *   with its own record layout -- COBOL has no concept of
      *   shared struct/type declarations for file records. Even if
      *   two files had identical layouts, each would need its own
      *   FD and 01-level record definition.
      *
      *   In C, this whole section is roughly equivalent to:
      *     FILE* cardFile  = fopen(getenv("DD_CARDS"), "r");
      *     FILE* printFile = fopen(getenv("DD_PRINT"), "w");
      *     FILE* factorFile = fopen(getenv("DD_FACTOR"), "w");
      *================================================================
00009  ENVIRONMENT DIVISION.
00010  CONFIGURATION SECTION.
00011  SOURCE-COMPUTER. IBM-370.
00012  OBJECT-COMPUTER. IBM-370.
00013  INPUT-OUTPUT SECTION.
00014  FILE-CONTROL.
00015      SELECT CARD-FILE ASSIGN TO UT-S-CARDS.
00016      SELECT PRINT-FILE ASSIGN TO UT-S-PRINT.
00017      SELECT FACTOR-FILE ASSIGN TO UT-S-FACTOR.
00018 *    SKIP3
      *
      *================================================================
      * DATA DIVISION - FILE SECTION
      *   Defines the structure of each file's records. This is the
      *   COBOL equivalent of defining a C struct for each file's
      *   record format, plus metadata about the file itself.
      *
      * KEY CONCEPTS:
      *
      * FD (File Description): Declares a file and its record format.
      *   RECORD CONTAINS n: Each record is exactly n bytes (fixed).
      *   BLOCK CONTAINS 0:  Let the OS determine blocking factor.
      *   LABEL RECORDS STANDARD: File has standard header/trailer
      *     labels (a mainframe tape/disk convention; effectively
      *     ignored on modern systems).
      *
      * Level Numbers (01, 05, 10): Define a data hierarchy.
      *   01 = top-level record (the whole record)
      *   05 = first-level subdivision (like struct fields)
      *   10 = second-level subdivision (fields within fields)
      *   Higher numbers = deeper nesting. Any number 02-49 works;
      *   05/10/15 by fives is a common convention.
      *
      * CHARACTERS vs BYTES:
      *   In standard COBOL with DISPLAY (default) storage, one
      *   "character" = one byte. PIC X(5) = 5 bytes. PIC 99 = 2
      *   bytes. RECORD CONTAINS 80 CHARACTERS = 80 bytes on disk.
      *   This is true for ASCII and EBCDIC (both are single-byte
      *   encodings). The only exception is COMP-3 (packed decimal)
      *   where digits are packed 2-per-byte (see below).
      *
      * PIC (PICTURE) Clause: COBOL's type+size declaration.
      *   PIC is short for PICTURE. It defines three things at once:
      *     1. The TYPE of data (alphabetic, numeric, alphanumeric)
      *     2. The SIZE (how many characters/bytes)
      *     3. The FORMAT (how data is displayed or stored)
      *   Every elementary data item MUST have a PIC clause.
      *   There is no equivalent of C's "int" or "char" as separate
      *   type keywords -- PIC IS the type system.
      *
      *   PIC X     = 1 alphanumeric character (like char in C)
      *   PIC X(n)  = n alphanumeric characters (like char[n])
      *   PIC XX    = shorthand for PIC X(2)
      *   PIC 9     = 1 numeric digit (stored as EBCDIC/ASCII char)
      *   PIC 99    = 2 numeric digits (like a 2-char numeric string)
      *   PIC 9V9999 = numeric with implied decimal: 1 integer digit
      *     + 4 decimal digits. The V is a virtual decimal point --
      *     it does NOT occupy a byte in storage. The actual storage
      *     is 5 bytes (characters '0'-'9'). Example: "29744" in
      *     memory represents the value 2.9744.
      *   PIC S999  = signed 3-digit number. The S means signed but
      *     in DISPLAY format the sign is encoded in the last digit's
      *     zone bits (an EBCDIC convention).
      *   PIC ZZ9   = 3-digit number with leading zero suppression.
      *     Zeroes are replaced with spaces. E.g., 5 displays as
      *     "  5", 42 as " 42", 100 as "100".
      *
      * 88-level (Condition Names):
      *   An 88-level is NOT a variable. It does NOT hold a value.
      *   It is a named boolean TEST (like a read-only property or
      *   a #define macro) that checks whether its PARENT field
      *   currently contains specific value(s).
      *
      *   Example: under CD-QUAD (PIC X):
      *     88 VALID-QUAD  VALUE '1' THRU '4'.
      *
      *   This means: VALID-QUAD is TRUE whenever CD-QUAD contains
      *   '1', '2', '3', or '4'. VALID-QUAD is FALSE otherwise.
      *   You CANNOT assign a value to VALID-QUAD -- it's a test,
      *   not a variable. You use it like:
      *     IF VALID-QUAD ...  (same as: IF CD-QUAD >= '1'
      *                                  AND CD-QUAD <= '4')
      *
      *   In C:  #define VALID_QUAD (cd_quad>='1' && cd_quad<='4')
      *   In C#: bool ValidQuad => cdQuad>='1' && cdQuad<='4';
      *   In Python: valid_quad = cd_quad in ('1','2','3','4')
      *
      *   The 88 is a special level number (not 01-49 or 77). It
      *   must appear immediately below the field it tests. Multiple
      *   88-levels can exist under one field.
      *
      * REDEFINES: Overlays one data definition on top of another,
      *   sharing the same memory. Like a C union or reinterpret_cast.
      *   CD-FACTOR-RD REDEFINES CD-FACTOR means both names refer
      *   to the same 5 bytes, but CD-FACTOR treats them as PIC X(5)
      *   (alphanumeric) while CD-FACTOR-RD treats them as PIC 9V9999
      *   (numeric with implied decimal).
      *
      * FILLER: An unnamed field that occupies space but is never
      *   referenced by name. Used for padding or spacers.
      *   Like: char _padding[72]; in C (unnamed struct padding).
      *================================================================
00019  DATA DIVISION.
00020  FILE SECTION.
      *----------------------------------------------------------------
      * CARD-FILE: Input file containing equalization factor cards.
      *   Each record is 80 bytes (standard IBM card image format,
      *   dating back to when data was entered on 80-column punch
      *   cards).
      *
      * Record layout (80 bytes):
      *   Bytes 1-2:  CD-YR      - Tax year (2-digit, e.g. "25")
      *   Byte  3:    CD-QUAD    - Assessment quadrant (1-4)
      *   Bytes 4-8:  CD-FACTOR  - Equalization factor, 5 chars
      *                            Interpreted as 9V9999 (implied
      *                            decimal): "29744" = 2.9744
      *   Bytes 9-80: CD-FILLER  - Unused (72 bytes of padding)
      *
      *   CARD (bytes 1-3) is the sequence key: year + quad.
      *   Records must be in ascending order by this 3-byte key.
      *----------------------------------------------------------------
00021  FD  CARD-FILE
00022      RECORD CONTAINS 80 CHARACTERS
00023      BLOCK CONTAINS 0 RECORDS
00024 *    RECORDING MODE IS F
00025      LABEL RECORDS ARE STANDARD
00026      DATA RECORD IS CARD-REC.
00027  01  CARD-REC.
00028      05  CARD.
      *        CARD is a group item (bytes 1-3) used as the sequence
      *        key. Group items in COBOL are always treated as
      *        alphanumeric (PIC X) regardless of their children's
      *        types. Comparisons on CARD use character/string
      *        comparison rules, not numeric rules.
00029          10 CD-YR          PIC XX.
      *           2-digit tax year, e.g. "25" for 2025.
      *           Stored as 2 ASCII/EBCDIC characters.
00030          10 CD-QUAD        PIC X.
      *           Assessment quadrant: '1', '2', '3', or '4'.
      *           Cook County is divided into 4 quadrants for
      *           property assessment purposes.
00031            88 VALID-QUAD         VALUE '1' THRU '4'.
      *           88-level condition (NOT a variable -- see above).
      *           VALID-QUAD is a boolean test: it is TRUE when
      *           CD-QUAD currently holds '1', '2', '3', or '4'.
      *           You use it in IF statements like a boolean:
      *             IF VALID-QUAD ...
      *           which is shorthand for:
      *             IF CD-QUAD >= '1' AND CD-QUAD <= '4' ...
00032      05 CD-FACTOR.
      *        The equalization factor as 5 alphanumeric characters.
      *        This is the "raw" view used for validation:
      *        IF CD-FACTOR NUMERIC checks all 5 chars are digits.
00033         10 CD-FT1      PIC X.
      *          First digit (integer part), e.g. '2' in factor 2.9744
00034         10 CD-FT4      PIC X(4).
      *          Last 4 digits (decimal part), e.g. "9744" in 2.9744
00035      05 CD-FACTOR-RD REDEFINES CD-FACTOR PIC 9V9999.
      *        Same 5 bytes as CD-FACTOR, but with numeric type and
      *        an implied decimal point (V). "29744" is treated as
      *        the numeric value 2.9744. The REDEFINES means this
      *        occupies the SAME memory as CD-FACTOR -- it's a
      *        different interpretation of the same bytes.
      *        IMPORTANT: When CD-FACTOR-RD is MOVEd to another
      *        PIC 9V9999 field, COBOL copies the raw characters
      *        (both are DISPLAY numeric), not the numeric value.
      *        This is a character-level copy, not an arithmetic
      *        conversion.
00036      05 CD-FILLER      PIC X(72).
      *        Unused padding. On real punch cards, these 72 columns
      *        would have been blank.
00037 *SKIP1
      *----------------------------------------------------------------
      * PRINT-FILE: Output report file for the audit listing.
      *   133 bytes per record. The first byte was traditionally the
      *   ASA carriage control character (space=single space, '0'=
      *   double space, '1'=new page, '+' = overprint). In this
      *   program, carriage control is handled by the AFTER ADVANCING
      *   clause on WRITE statements rather than by the first byte.
      *
      *   The 133-byte width is a mainframe convention: 1 byte for
      *   carriage control + 132 characters of printable content,
      *   fitting a standard 132-column line printer.
      *
      *   PRINT-REC is defined as a flat PIC X(133) because the
      *   actual content is built in WORKING-STORAGE (WORK-LINE,
      *   HDG-LINE, etc.) and written using WRITE ... FROM, which
      *   copies the source data into PRINT-REC before output.
      *----------------------------------------------------------------
00038  FD  PRINT-FILE
00039      LABEL RECORDS ARE STANDARD
00040 *    RECORDING MODE IS F
00041      RECORD CONTAINS 133 CHARACTERS
00042      BLOCK CONTAINS 0 RECORDS
00043      DATA RECORD IS PRINT-REC.
00044  01  PRINT-REC         PIC X(133).
00045 *    SKIP1
      *----------------------------------------------------------------
      * FACTOR-FILE: Output file containing validated equalization
      *   factors. 21 bytes per record. This file feeds downstream
      *   programs (ASHMA850, ASHMA855, ASHMA857) that apply these
      *   factors during the tax extension computation.
      *
      * Record layout (21 bytes, defined by copybook REBEQFRD01):
      *   Bytes 1-2:  FT-TAXYR   - Tax year (PIC 99, 2 numeric chars)
      *   Byte  3:    FT-QUAD    - Quadrant (PIC 9, 1 numeric char)
      *   Bytes 4-8:  FT-EQFACT  - Factor (PIC 9V9999, 5 numeric
      *                             chars with implied decimal)
      *   Bytes 9-21: FILLER     - 13 bytes of unused padding
      *
      *   All fields are DISPLAY numeric (human-readable digit chars),
      *   not binary or packed. The file can be read as plain text.
      *----------------------------------------------------------------
00046  FD  FACTOR-FILE
00047      RECORD CONTAINS 21 CHARACTERS
00048      BLOCK CONTAINS 0 RECORDS
00049 *    RECORDING MODE IS F
00050      LABEL RECORDS ARE STANDARD
00051      DATA RECORD IS FACTOR-REC.
00052  01  FACTOR-REC.
00053      05  FT-TAXYR      PIC 99.
00054      05  FT-QUAD       PIC 9.
00055      05  FT-EQFACT     PIC 9V9999.
00056      05  FILLER        PIC X(13).
00057 *    SKIP2
      *
      *================================================================
      * WORKING-STORAGE SECTION
      *   All "local" variables (though in COBOL they're really global
      *   -- all data is visible to all paragraphs). These persist
      *   for the life of the program. This is equivalent to declaring
      *   global variables in C:
      *
      *   // Counters and flags
      *   int line_cnt = 60;      // LINE-CNT (packed decimal)
      *   int in_cnt = 0;         // IN-CNT
      *   int out_cnt = 0;        // OUT-CNT
      *   int error_cnt = 0;      // ERROR-CNT
      *   int page_cnt = 0;       // PAGE-CNT
      *   bool end_of_file = false; // CARD-EOF / END-OF-CARD-FILE
      *   bool seq_error = false; // SEQ-CHECK / SEQ-ERROR
      *   char err_mesg[11] = "NOT NUMERIC";
      *
      *   // Work areas for building report lines
      *   struct { ... } work_area;
      *
      * 77-level items: Standalone variables (not part of a group).
      *   A legacy construct equivalent to 01-level with no children.
      *   Think of these as simple scalar globals.
      *
      * COMP-3 (Packed Decimal): A storage format where two decimal
      *   digits are packed into each byte, with the last half-byte
      *   holding the sign. PIC S999 COMP-3 uses 2 bytes:
      *     byte 0: two digits (0-9, 0-9)
      *     byte 1: one digit (0-9) + sign nibble (C=positive, D=neg)
      *   Arithmetic is done directly on packed decimal, avoiding
      *   binary floating-point issues. In Java/C#, this maps to
      *   simple int (these are internal counters only, never written
      *   to an output file in packed format).
      *
      * VALUE clause: Initial value at program start. Like
      *   initialization in a C declaration: int x = 0;
      *================================================================
00058 ****************  WORKING STORAGE STARTS HERE  *******************
00059 *    SKIP1
00060  WORKING-STORAGE SECTION.
00061  77  LINE-CNT          PIC S999       VALUE +60      COMP-3.
      *    Report line counter. Initialized to 60 (greater than 55)
      *    to force the heading routine to execute before the first
      *    detail line is printed. This is a common COBOL idiom:
      *    set the counter past the page-break threshold so the first
      *    iteration triggers a new page header.
      *    In C: int line_cnt = 60;
00062  77  ERR-MESG          PIC X(11)      VALUE 'NOT NUMERIC'.
      *    Error message constant. Moved into WK-MESG when a card
      *    fails validation. 11 characters exactly.
      *    In C: const char* err_mesg = "NOT NUMERIC";
00063  77  IN-CNT            PIC S999       VALUE +0       COMP-3.
      *    Count of valid input records read (incremented in 030).
00064  77  OUT-CNT           PIC S999       VALUE +0       COMP-3.
      *    Count of factor records written (incremented in 040).
00065  77  ERROR-CNT         PIC S999       VALUE +0       COMP-3.
      *    Count of records failing validation (incremented in 020).
00066  77  PAGE-CNT          PIC S999       VALUE +0       COMP-3.
      *    Page number counter for the report.
00067  77  CARD-EOF          PIC 9          VALUE 0.
      *    End-of-file flag. Set to 1 when CARD-FILE reaches EOF.
      *    In C: bool card_eof = false;
00068      88 END-OF-CARD-FILE              VALUE 1.
      *    88-level condition (NOT a variable -- it's a boolean
      *    test). END-OF-CARD-FILE is TRUE when its parent field
      *    CARD-EOF currently contains 1. It is FALSE otherwise.
      *    Usage: IF END-OF-CARD-FILE ...
      *    is shorthand for: IF CARD-EOF = 1 ...
      *    In C: if (card_eof == 1) ...
00069  77  SEQ-CHECK         PIC X.
      *    Sequence error flag. Set to 'E' on out-of-sequence cards.
      *    Note: Not initialized with a VALUE clause, so its initial
      *    value is SPACES (PIC X defaults to spaces in COBOL).
      *    In C: char seq_check = ' ';
00070      88  SEQ-ERROR                    VALUE 'E'.
      *    88-level condition (NOT a variable). SEQ-ERROR is TRUE
      *    when its parent field SEQ-CHECK contains 'E'.
      *    Usage: IF SEQ-ERROR ...
      *    is shorthand for: IF SEQ-CHECK = 'E' ...
      *
      *----------------------------------------------------------------
      * WORK-AREA: Group item (struct) containing work fields used to
      *   build report lines and track state. All sub-fields at the
      *   05 and 10 levels are members of this "struct."
      *
      *   In C, this would be:
      *   struct work_area {
      *     struct { char fact1; char dot; char fact4[4]; } work_factor;
      *     struct { char yr[2]; char quad; } prev_card;
      *     struct { char filler[11]; char date[8]; } date_line;
      *     struct { ... } hdg_line;
      *     struct { ... } ttl_line;
      *     struct { ... } ttl_line2;
      *     struct { ... } work_line;
      *   };
      *
      *   IMPORTANT: In COBOL, these sub-items (WORK-FACTOR, DATE-LINE,
      *   HDG-LINE, TTL-LINE, TTL-LINE2, WORK-LINE) are contiguous in
      *   memory as children of WORK-AREA. However, each is used
      *   independently -- they are NOT part of a single output record.
      *   Each is a template that gets written to PRINT-REC separately
      *   via WRITE PRINT-REC FROM <sub-item>.
      *----------------------------------------------------------------
00071  01  WORK-AREA.
00072      05 WORK-FACTOR.
      *       Formatted factor for display: "N.NNNN" (6 chars total).
      *       Built by copying CD-FT1 to WK-FACT1 and CD-FT4 to
      *       WK-FACT4. The embedded '.' literal provides the decimal
      *       point display character.
      *       Example: factor 29744 -> WK-FACT1='2', '.', WK-FACT4=
      *                "9744" -> WORK-FACTOR = "2.9744"
00073         10 WK-FACT1    PIC X.
      *          Integer part of factor (1 digit)
00074         10 FILLER      PIC X          VALUE '.'.
      *          Literal decimal point character (always '.')
00075         10 WK-FACT4    PIC X(4).
      *          Decimal part of factor (4 digits)
00076      05  PREV-CARD.
      *       Stores the previous card's sequence key (year + quad)
      *       for sequence checking. Initialized to LOW-VALUE (0x00)
      *       so the first card always passes the sequence check
      *       (any valid card key > 0x00).
      *
      *       LOW-VALUE is the lowest possible character value in the
      *       collating sequence: 0x00 in ASCII, 0x00 in EBCDIC.
      *       In C: memset(prev_card, '\0', 3);
00077          10 PREV-YR    PIC XX         VALUE LOW-VALUE.
00078          10 PREV-QUAD  PIC X          VALUE LOW-VALUE.
00079      05 DATE-LINE.
      *       First line of each page header: contains today's date.
      *       Total content width = 11 spaces + 8-char date = 19 chars.
      *       When written to PRINT-REC (133 chars), COBOL pads the
      *       remaining 114 characters with spaces automatically.
00080          10 FILLER     PIC X(11)      VALUE SPACES.
      *          11-space left margin
00081          10 DATE-DT    PIC X(8).
      *          Date in YYYYMMDD format (8 chars). Populated from
      *          FUNCTION CURRENT-DATE at program start.
      *          Example: "20260210" for February 10, 2026.
00082      05 HDG-LINE.
      *       Second line of each page header: program name, office
      *       name, and page number. Total = 11+40+70+6+3 = 130 chars.
00083          10 FILLER     PIC X(11)      VALUE SPACES.
00084          10 FILLER     PIC X(40)      VALUE 'CLREB020'.
      *          Program name "CLREB020" followed by 32 spaces to
      *          fill the 40-character field.
00085          10 FILLER     PIC X(70)      VALUE 'OFFICE  OF  THE COUNT
00086 -        'Y  CLERK'.
      *          Office name. Note: line 86 starts with '-' in col 7
      *          which is a CONTINUATION character. The string literal
      *          continues from the previous line. The full value is:
      *          'OFFICE  OF  THE COUNTY  CLERK' (29 chars) followed
      *          by 41 spaces to fill the 70-character field.
      *
      *          Continuation rules: The '-' in column 7 means "this
      *          line continues the previous line's statement." For
      *          string literals, the continued string starts at the
      *          quote character on the continuation line.
00087          10 FILLER     PIC X(6)       VALUE 'PAGE  '.
00088          10 HDG-PG     PIC ZZ9.
      *          Page number with leading zero suppression.
      *          ZZ9 means: first two digits show as spaces if zero,
      *          last digit always shows. So page 1 = "  1",
      *          page 12 = " 12", page 100 = "100".
00089      05  TTL-LINE.
      *       Third line of page header: "EQUALIZATION FACTORS" title.
00090          10 FILLER     PIC X(51)      VALUE SPACES.
00091          10 FILLER     PIC X(25)      VALUE 'EQUALIZATION      FAC
00092 -        'TORS'.
      *          Full value: 'EQUALIZATION      FACTORS' (25 chars)
00093      05  TTL-LINE2.
      *       Fourth line (column headers): "YEAR  QUAD  FACTOR"
00094          10 FILLER     PIC X(51)      VALUE SPACES.
00095          10 FILLER     PIC X(25)      VALUE 'YEAR      QUAD     FA
00096 -        'CTOR'.
      *          Full value: 'YEAR      QUAD     FACTOR' (25 chars)
00097      05  WORK-LINE.
      *       Template for detail lines in the report.
      *       Layout: 52 spaces + year(2) + 8 spaces + quad(1) +
      *               7 spaces + factor(6) + 5 spaces + message(11)
      *       Total = 52+2+8+1+7+6+5+11 = 92 chars.
      *       Written to PRINT-REC (133 chars); COBOL space-pads
      *       the remaining 41 chars to 133 automatically.
      *
      *       WK-MESG is normally spaces (blank). When a card fails
      *       validation, ERR-MESG ("NOT NUMERIC") is moved into
      *       WK-MESG, then cleared back to spaces after the line
      *       is written.
00098          10 FILLER     PIC X(52)      VALUE SPACES.
00099          10 WK-YR      PIC XX.
00100          10 FILLER     PIC X(8)       VALUE SPACES.
00101          10 WK-QUAD    PIC X.
00102          10 FILLER     PIC X(7)       VALUE SPACES.
00103          10 WK-FACT    PIC X(6).
      *          Factor display field (6 chars): "N.NNNN"
00104          10 FILLER     PIC X(5)       VALUE SPACES.
00105          10 WK-MESG    PIC X(11)      VALUE SPACES.
      *          Error message field. Blank for valid cards,
      *          "NOT NUMERIC" for invalid cards.
00106 *SKIP2
00107 *****************  WORKING STORAGE ENDS HERE  ********************
00108 *SKIP3
      *
      *================================================================
      * PROCEDURE DIVISION
      *   Contains all executable logic. Organized into "paragraphs"
      *   which function like subroutines but with no parameters or
      *   return values -- all data is shared globally.
      *
      * PARAGRAPH NAMING CONVENTION:
      *   NNN-NAME: NNN is a numeric prefix for ordering/grouping.
      *   NNN-EXIT: An empty paragraph that serves as the end marker
      *     for PERFORM ... THRU ranges.
      *
      * KEY STATEMENTS USED IN THIS PROGRAM:
      *
      * PERFORM 020-MAIN-LINE THRU 020-EXIT
      *   Calls paragraph 020-MAIN-LINE through 020-EXIT inclusive.
      *   Equivalent to calling a function. The THRU clause means
      *   "execute all paragraphs from 020-MAIN-LINE to 020-EXIT."
      *   The EXIT paragraph is a no-op endpoint.
      *   In C: mainLine();
      *
      * PERFORM ... UNTIL condition
      *   A pre-test loop: checks the condition BEFORE each iteration.
      *   Equivalent to: while (!condition) { body(); }
      *   Note: COBOL UNTIL = "loop until true" = C while(!condition).
      *
      * MOVE source TO target
      *   Copies data from source to target with automatic type
      *   conversion and padding/truncation. This is the COBOL
      *   assignment statement. Rules vary by type:
      *   - Alphanumeric to alphanumeric: left-justified, space-padded
      *   - Numeric to numeric: right-justified, zero-padded
      *   - MOVE SPACES TO field: fills with space characters (0x20)
      *   In C: strncpy(target, source, sizeof(target)); // roughly
      *
      * WRITE record FROM source AFTER ADVANCING n
      *   Writes to a file. FROM copies source into the record buffer
      *   first. AFTER ADVANCING n = advance n lines before printing
      *   (double-spacing for n=2). AFTER PAGE = form feed (new page).
      *   In C: fprintf(printFile, "\n%s", work_line); // roughly
      *
      * READ file AT END statement
      *   Reads the next sequential record. AT END executes the
      *   statement if there are no more records (EOF).
      *   In C: if (fread(buf, reclen, 1, f) != 1) { /* at end */ }
      *
      * IF condition ... ELSE ...
      *   Standard conditional. COBOL IF can be nested but periods
      *   terminate ALL active IF/ELSE blocks (a notorious source
      *   of bugs in COBOL). This program uses period-terminated IFs.
      *
      * ADD n TO counter
      *   Arithmetic: counter = counter + n;
      *
      * DISPLAY text
      *   Writes to stdout/console (SYSOUT in mainframe terms).
      *   In C: printf("%s\n", text);
      *
      * STOP RUN
      *   Terminates the program. In C: exit(0); or return 0;
      *
      * RETURN-CODE
      *   Special register (built-in variable) that sets the program's
      *   exit code. In C: sets the value returned by main() or
      *   passed to exit(). 0 = success, 16 = severe error.
      *
      * EXIT
      *   A no-op statement. Used as a target for PERFORM THRU to
      *   mark the end of a paragraph range. Like a label with no
      *   code after it.
      *================================================================
00109  PROCEDURE DIVISION.
      *----------------------------------------------------------------
      * 010-BEGIN: Main entry point (program initialization and
      *   main loop).
      *
      *   Business logic:
      *     1. Open all three files
      *     2. Capture today's date for report headers
      *     3. Process all cards in a loop until EOF or sequence error
      *     4. Display processing statistics to the console
      *     5. Close all files and terminate
      *
      *   The PERFORM UNTIL loop is equivalent to:
      *     while (!end_of_card_file && !seq_error) {
      *         mainLine();
      *     }
      *
      *   Note: STOP RUN terminates the program here (not at the end
      *   of the file). Paragraphs 020-060 are only reached via
      *   PERFORM, never by fall-through from 010-BEGIN.
      *----------------------------------------------------------------
00110  010-BEGIN.
      *    -- Open all three files for processing --
      *    OPEN INPUT = open for reading; OPEN OUTPUT = open for
      *    writing (creates new file or overwrites existing).
      *    In C: cardFile = fopen(..., "r");
      *          printFile = fopen(..., "w");
      *          factorFile = fopen(..., "w");
00111      OPEN    INPUT CARD-FILE
00112              OUTPUT PRINT-FILE
00113                     FACTOR-FILE
      *    -- Capture today's date for report page headers --
      *    FUNCTION CURRENT-DATE returns a 21-character string:
      *      Positions 1-8:   YYYYMMDD  (date)
      *      Positions 9-14:  HHMMSScc  (time, cc = hundredths)
      *      Positions 15-21: +/-HHMM   (UTC offset)
      *    Only the first 8 characters (YYYYMMDD) are captured
      *    because DATE-DT is PIC X(8). COBOL truncates on the
      *    right when moving to a shorter alphanumeric field.
      *    In C: strftime(date_dt, 9, "%Y%m%d", localtime(&now));
00114      MOVE FUNCTION CURRENT-DATE TO DATE-DT
      *    -- Main processing loop --
      *    Repeatedly call 020-MAIN-LINE (which reads one card
      *    and processes it) until either:
      *      - END-OF-CARD-FILE becomes true (88-level: CARD-EOF=1)
      *      - SEQ-ERROR becomes true (88-level: SEQ-CHECK='E')
      *    PERFORM UNTIL is a pre-test loop (checks BEFORE each
      *    iteration). In C:
      *      while (!end_of_card_file && !seq_error) {
      *          mainLine();
      *      }
00115      PERFORM 020-MAIN-LINE THRU 020-EXIT
00116              UNTIL END-OF-CARD-FILE
00117               OR   SEQ-ERROR
      *    -- Display processing statistics to console (stdout) --
      *    DISPLAY concatenates its arguments and writes to stdout.
      *    The COMP-3 counters are automatically converted to
      *    display format. In C: printf("...", in_cnt);
00118      DISPLAY 'NO. OF INPUT RECORDS  = ' IN-CNT
00119      DISPLAY 'NO. OF OUTPUT RECORDS = ' OUT-CNT
00120      DISPLAY 'NO. OF ERROR RECORDS  = ' ERROR-CNT
      *    -- Close all files and terminate --
      *    CLOSE flushes buffers and releases file handles.
      *    In C: fclose(cardFile); fclose(printFile); ...
00121      CLOSE   CARD-FILE
00122              PRINT-FILE
00123              FACTOR-FILE
      *    STOP RUN terminates the program and returns control
      *    to the OS. RETURN-CODE (default 0) becomes the exit
      *    code. In C: return 0; or exit(return_code);
00124      STOP RUN.
00125 *    SKIP3
      *----------------------------------------------------------------
      * 020-MAIN-LINE: Process one input card.
      *
      *   Business logic:
      *     1. Read the next card (with sequence checking)
      *     2. If not EOF and not sequence error:
      *        a. Validate the card fields:
      *           - CD-YR must be NUMERIC (all digits) AND > 0
      *           - CD-FACTOR must be NUMERIC AND > 0
      *           - CD-QUAD must be a VALID-QUAD ('1' thru '4')
      *        b. If valid: write factor record AND report line
      *        c. If invalid: increment error count, add error
      *           message to report line, and write the report line
      *           (no factor record is written for errors)
      *
      *   COBOL BEHAVIOR NOTE - "GREATER THAN 0" comparisons:
      *     CD-YR and CD-FACTOR are PIC X (alphanumeric) fields.
      *     When COBOL compares an alphanumeric field to a numeric
      *     literal (0), it converts the literal to a 1-character
      *     string "0" and pads it with SPACES to match the field
      *     length. So:
      *       CD-YR GREATER THAN 0
      *     actually compares the 2-char string in CD-YR against
      *     "0 " (character '0' followed by a space).
      *
      *     This means "00" > "0 " is TRUE (because at position 2,
      *     '0' (0x30) > ' ' (0x20)). So a year of "00" passes
      *     this check! Similarly, factor "00000" > "0    " is TRUE.
      *     This is arguably a bug in the original code -- a numeric
      *     comparison was likely intended, but the PIC X type forces
      *     alphanumeric comparison rules.
      *
      *   COBOL IF STRUCTURE NOTE:
      *     The period (.) at the end of line 140 terminates ALL
      *     nested IF statements. In COBOL, a period ends the entire
      *     sentence, closing all open IF/ELSE blocks. This is very
      *     different from C's brace-matching. The equivalent C code:
      *
      *     if (!end_of_card_file && !seq_error) {
      *         if (isNumeric(cd_yr) && strcmp(cd_yr, "0 ") > 0
      *             && isNumeric(cd_factor)
      *             && strcmp(cd_factor, "0    ") > 0
      *             && isValidQuad(cd_quad)) {
      *             createFactor();
      *             writeReportLine();
      *         } else {
      *             error_cnt++;
      *             strcpy(wk_mesg, err_mesg);
      *             writeReportLine();
      *         }
      *     }
      *----------------------------------------------------------------
00126  020-MAIN-LINE.
      *    -- Step 1: Read the next card from input file --
      *    This calls 030-READ-CARD which reads one 80-byte
      *    record and checks its sequence. If EOF, sets
      *    END-OF-CARD-FILE. If out of sequence, sets SEQ-ERROR.
00127      PERFORM 030-READ-CARD THRU 030-READ-EXIT
      *    -- Step 2: If we got a card, validate it --
      *    Outer IF: skip processing if EOF or sequence error.
      *    In C: if (!eof && !seqError) { ... }
00128      IF      NOT END-OF-CARD-FILE
00129        AND   NOT SEQ-ERROR
      *        Inner IF: validate all three card fields.
      *        NUMERIC = all characters are digits '0'-'9'.
      *        GREATER THAN 0 = alphanumeric compare (see note
      *        above about the "GREATER THAN 0" quirk).
      *        VALID-QUAD = 88-level test: CD-QUAD in '1'..'4'.
      *        ALL conditions must be true (AND logic).
00130              IF     CD-YR NUMERIC
00131                AND  CD-YR GREATER THAN 0
00132                AND  CD-FACTOR NUMERIC
00133                AND  CD-FACTOR GREATER THAN 0
00134                AND  VALID-QUAD
      *              -- Card is VALID: write factor + report line --
00135                     PERFORM 040-CREATE-FACTOR THRU 040-EXIT
00136                     PERFORM 050-WRITE THRU 050-EXIT
      *        ELSE = card failed one or more validations.
00137              ELSE
      *              -- Card is INVALID: count error, tag report --
      *              error_cnt++;
00138                     ADD +1 TO ERROR-CNT
      *              Copy "NOT NUMERIC" into the report line's
      *              message field so the error is visible.
      *              In C: strcpy(wk_mesg, "NOT NUMERIC");
00139                     MOVE ERR-MESG TO WK-MESG
      *              Write the report line (with error message).
      *              No factor record is written for invalid cards.
00140                     PERFORM 050-WRITE THRU 050-EXIT.
      *    The period on line 140 ends BOTH IF statements.
      *    This is the end of the 020-MAIN-LINE sentence.
00141 *    SKIP1
00142  020-EXIT.
00143      EXIT.
00144 *    SKIP3
      *----------------------------------------------------------------
      * 030-READ-CARD: Read the next card and check sequence.
      *
      *   Business logic:
      *     1. Read next record from CARD-FILE
      *     2. If EOF: set END-OF-CARD-FILE flag (loop terminates)
      *     3. If not EOF: compare CARD (current 3-byte key) against
      *        PREV-CARD (previous 3-byte key)
      *        a. If CARD < PREV-CARD (out of sequence):
      *           - Set RETURN-CODE = 16 (severe error)
      *           - Display diagnostic messages to console
      *           - Set SEQ-ERROR flag (loop terminates)
      *        b. If CARD >= PREV-CARD (in sequence):
      *           - Save current key as PREV-CARD
      *           - Increment input counter
      *
      *   SEQUENCE CHECKING:
      *     CARD (3 bytes: year + quad) is compared as a string.
      *     "251" < "252" < "253" < "254" < "261" etc.
      *     PREV-CARD starts as LOW-VALUE (0x000000), which is less
      *     than any valid card key, so the first card always passes.
      *
      *     IMPORTANT: The check is LESS THAN, not LESS THAN OR EQUAL.
      *     Duplicate cards (same year+quad) are allowed and will NOT
      *     trigger a sequence error. Only strictly descending keys
      *     will trigger an error.
      *
      *   RETURN-CODE = 16:
      *     On IBM mainframes, return code 16 means "severe error."
      *     Convention: 0=success, 4=warning, 8=error, 16=severe.
      *     JCL job steps check this code to decide whether to
      *     continue or abort the job.
      *
      *   COBOL IF NESTING NOTE:
      *     The period at the end of line 157 terminates BOTH nested
      *     IF statements. In C:
      *     if (!end_of_card_file) {
      *         if (strcmp(card, prev_card) < 0) {
      *             return_code = 16;
      *             printf("CARDS OUT OF SEQUENCE\n");
      *             printf("CURRENT CARD %s\n", card);
      *             printf("PREVIOUS CARD %s\n", prev_card);
      *             seq_check = 'E';
      *         } else {
      *             memcpy(prev_card, card, 3);
      *             in_cnt++;
      *         }
      *     }
      *----------------------------------------------------------------
00145  030-READ-CARD.
      *    -- Read the next 80-byte record from CARD-FILE --
      *    The record is placed into CARD-REC (the 01-level buffer
      *    declared under FD CARD-FILE in the DATA DIVISION).
      *    AT END fires if the file has no more records (EOF).
      *    In C: if (fread(&card_rec, 80, 1, cardFile) != 1)
      *              card_eof = 1;
00146      READ    CARD-FILE
00147              AT END MOVE 1 TO CARD-EOF.
      *    Note: the period above ends the READ statement.
      *    The IF below is a NEW statement (not inside the READ).
      *
      *    -- If not EOF, check sequence --
00148      IF      NOT END-OF-CARD-FILE
      *        Compare current card's 3-byte key (year+quad)
      *        against the previously-read card's key.
      *        This is a string comparison (CARD is PIC X(3),
      *        PREV-CARD is PIC X(3)). LESS THAN means the
      *        current card comes BEFORE the previous one in
      *        alphabetical/ASCII order -- i.e., out of sequence.
00149              IF   CARD LESS THAN PREV-CARD
      *              -- SEQUENCE ERROR: cards are out of order --
      *              Set program exit code to 16 (severe error).
      *              In C: return_code = 16;
00150                   MOVE 16 TO RETURN-CODE
      *              Print diagnostic messages to console.
00151                   DISPLAY 'CARDS OUT OF SEQUENCE'
00152                   DISPLAY 'CURRENT CARD ' CARD
00153                   DISPLAY 'PREVIOUS CARD ' PREV-CARD
      *              Set SEQ-ERROR flag. This makes the 88-level
      *              SEQ-ERROR true, which will terminate the
      *              main loop in 010-BEGIN.
      *              In C: seq_check = 'E';
00154                   MOVE 'E' TO SEQ-CHECK
      *         ELSE = card is in sequence (>= previous card)
00155              ELSE
      *              Save this card's key as the new "previous"
      *              for the next iteration's sequence check.
      *              MOVE copies 3 bytes (group-level move).
      *              In C: memcpy(prev_card, card, 3);
00156                   MOVE CARD TO PREV-CARD
      *              Count this as a valid input record.
      *              In C: in_cnt++;
00157                   ADD +1 TO IN-CNT.
      *    The period above ends BOTH nested IF statements.
00158 *    SKIP1
00159  030-READ-EXIT.
00160      EXIT.
00161 *    SKIP3
      *----------------------------------------------------------------
      * 040-CREATE-FACTOR: Build and write one factor record.
      *
      *   Business logic:
      *     Creates a 21-byte output record containing the validated
      *     equalization factor data, and writes it to FACTOR-FILE.
      *
      *   Data flow:
      *     1. Clear FACTOR-REC to spaces (initializes FILLER too)
      *     2. Copy year from input card to factor record
      *     3. Copy quad from input card to factor record
      *     4. Copy factor value from input card to factor record
      *     5. Write the completed record to FACTOR-FILE
      *     6. Increment output counter
      *
      *   CRITICAL TRANSLATION NOTE - LINE 166:
      *     MOVE CD-FACTOR-RD TO FT-EQFACT
      *     Both CD-FACTOR-RD (PIC 9V9999) and FT-EQFACT (PIC 9V9999)
      *     are DISPLAY numeric -- the digits are stored as readable
      *     character bytes ('0'-'9'), not as binary integers. The V
      *     (implied decimal) affects arithmetic operations but NOT
      *     the MOVE. This MOVE copies 5 raw character bytes from
      *     the card to the factor record. It is a character-level
      *     copy: "29744" in -> "29744" out.
      *
      *     If you translate this to Java or C#, do NOT use floating-
      *     point conversion. Use string/character copy to preserve
      *     the exact byte content. The factor value 2.9744 is stored
      *     as the string "29744" in both the input and output files.
      *
      *   MOVE SPACES TO FACTOR-REC:
      *     Fills all 21 bytes with space characters (0x20). This
      *     ensures the 13-byte FILLER area is properly initialized.
      *     In C: memset(factor_rec, ' ', 21);
      *----------------------------------------------------------------
00162  040-CREATE-FACTOR.
      *    -- Step 1: Clear the output record to all spaces --
      *    This initializes all 21 bytes, including the 13-byte
      *    FILLER at the end. In C: memset(factor_rec, ' ', 21);
00163      MOVE    SPACES TO FACTOR-REC
      *    -- Step 2: Copy year from card to factor record --
      *    CD-YR (PIC XX, 2 chars) -> FT-TAXYR (PIC 99, 2 chars).
      *    Both are 2-byte fields so it's a simple byte copy.
      *    In C: memcpy(ft_taxyr, cd_yr, 2);
00164      MOVE    CD-YR      TO  FT-TAXYR
      *    -- Step 3: Copy quad from card to factor record --
      *    CD-QUAD (PIC X, 1 char) -> FT-QUAD (PIC 9, 1 char).
      *    In C: ft_quad = cd_quad;
00165      MOVE    CD-QUAD    TO  FT-QUAD
      *    -- Step 4: Copy factor value (the critical move) --
      *    CD-FACTOR-RD (PIC 9V9999) -> FT-EQFACT (PIC 9V9999).
      *    Both are DISPLAY numeric with identical PIC, so COBOL
      *    copies the 5 raw character bytes directly ("29744").
      *    This is NOT arithmetic -- no decimal conversion occurs.
      *    In C: memcpy(ft_eqfact, cd_factor_rd, 5);
00166      MOVE    CD-FACTOR-RD  TO  FT-EQFACT
      *    -- Step 5: Write the 21-byte record to FACTOR-FILE --
      *    WRITE outputs the entire FACTOR-REC (21 bytes) to the
      *    file. No line terminator is added (binary sequential).
      *    In C: fwrite(factor_rec, 21, 1, factorFile);
00167      WRITE   FACTOR-REC
      *    -- Step 6: Increment output record counter --
      *    In C: out_cnt++;
00168                   ADD +1 TO OUT-CNT.
00169 *    SKIP1
00170  040-EXIT.
00171      EXIT.
00172 *    SKIP3
      *----------------------------------------------------------------
      * 050-WRITE: Write one detail line to the report.
      *
      *   Business logic:
      *     Formats and prints one line on the audit report, showing
      *     the year, quad, factor (formatted with decimal point),
      *     and optionally an error message for invalid cards.
      *
      *   Page break logic:
      *     If LINE-CNT > 55, call the heading routine first to start
      *     a new page. The page holds approximately 55 lines of
      *     output (the heading uses 8 line-equivalents, and each
      *     detail uses 2, so ~24 details per page).
      *
      *   Factor display formatting:
      *     The factor "29744" needs to display as "2.9744" (6 chars).
      *     This is done via character manipulation, NOT arithmetic:
      *       1. CD-FT1 (first char, e.g. '2') -> WK-FACT1
      *       2. CD-FT4 (last 4 chars, e.g. "9744") -> WK-FACT4
      *       3. WORK-FACTOR = WK-FACT1 + '.' + WK-FACT4 = "2.9744"
      *       4. WORK-FACTOR -> WK-FACT (copied into report line)
      *
      *     This avoids any floating-point conversion and guarantees
      *     the displayed value matches the raw input exactly.
      *
      *   WRITE PRINT-REC FROM WORK-LINE AFTER ADVANCING 2:
      *     - FROM WORK-LINE: copies WORK-LINE content into PRINT-REC
      *       (the file's record buffer) before writing. WORK-LINE is
      *       92 chars; PRINT-REC is 133 chars, so COBOL space-pads
      *       the remaining 41 characters.
      *     - AFTER ADVANCING 2: skip 2 lines before printing
      *       (double-spacing). This creates a blank line between
      *       detail lines in the report output.
      *
      *   WK-MESG cleanup:
      *     After writing, WK-MESG is reset to SPACES. This ensures
      *     the error message doesn't persist into the next card's
      *     report line. The ERR-MESG was moved in during 020 for
      *     error cases; valid cards never touch WK-MESG.
      *
      *   LINE-CNT tracking:
      *     ADD +2 mirrors the AFTER ADVANCING 2 spacing. The counter
      *     tracks approximate line position on the page to know when
      *     to trigger a new page header.
      *----------------------------------------------------------------
00173  050-WRITE.
      *    -- Page break check --
      *    If we've printed more than 55 lines on this page,
      *    print a new page header before this detail line.
      *    In C: if (line_cnt > 55) { writeHeading(); }
00174      IF      LINE-CNT GREATER THAN +55
00175              PERFORM 060-HDG-ROUTINE THRU 060-EXIT.
      *    CRITICAL: The period (.) on line 175 ENDS the IF.
      *    Everything below runs UNCONDITIONALLY -- it is NOT
      *    inside the IF block. This is a notorious COBOL gotcha:
      *    a period terminates the entire IF statement. The code
      *    below always executes, whether or not the heading was
      *    printed. In C:
      *      if (line_cnt > 55) { writeHeading(); }
      *      // --- unconditional from here on ---
      *
      *    -- Build the report line: copy card fields into it --
      *    Copy the 2-char year into the report template.
      *    In C: memcpy(wk_yr, cd_yr, 2);
00176      MOVE    CD-YR      TO  WK-YR
      *    Copy the 1-char quad into the report template.
      *    In C: wk_quad = cd_quad;
00177      MOVE    CD-QUAD    TO  WK-QUAD
      *    -- Format factor for display as "N.NNNN" --
      *    Copy the integer digit (e.g., '2') into WK-FACT1.
00178      MOVE    CD-FT1     TO  WK-FACT1
      *    Copy 4 decimal digits (e.g., "9744") into WK-FACT4.
      *    WORK-FACTOR now = "2.9744" (the '.' comes from the
      *    FILLER VALUE '.' declared between WK-FACT1 and WK-FACT4
      *    in WORKING-STORAGE).
00179      MOVE    CD-FT4     TO  WK-FACT4
      *    Copy the assembled 6-char "N.NNNN" string into the
      *    report line's factor display field.
00180      MOVE    WORK-FACTOR TO WK-FACT
      *    -- Write the report line to PRINT-FILE --
      *    FROM WORK-LINE: copies WORK-LINE (92 chars) into
      *    PRINT-REC (133 chars), padding with spaces.
      *    AFTER ADVANCING 2: double-space (skip a blank line
      *    before printing). In C: fprintf(f, "\n\n%s", line);
00181      WRITE   PRINT-REC FROM WORK-LINE
00182              AFTER ADVANCING 2
      *    -- Clean up: clear the error message field --
      *    Reset WK-MESG to spaces so the error message from
      *    an invalid card doesn't carry over to the next line.
      *    In C: memset(wk_mesg, ' ', 11);
00183      MOVE    SPACES TO WK-MESG
      *    -- Track line position on the page --
      *    Add 2 to match the AFTER ADVANCING 2 above. When
      *    this counter exceeds 55, the next call to 050-WRITE
      *    will trigger a new page header.
      *    In C: line_cnt += 2;
00184      ADD     +2 TO LINE-CNT.
00185 *    SKIP1
00186  050-EXIT.
00187      EXIT.
00188 *    SKIP3
      *----------------------------------------------------------------
      * 060-HDG-ROUTINE: Print page header (4 lines).
      *
      *   Business logic:
      *     Prints the top-of-page header block for the audit report.
      *     Called whenever LINE-CNT exceeds 55 (approximately every
      *     24 detail lines).
      *
      *   Page header content (4 lines):
      *     Line 1 (AFTER PAGE):        Date (YYYYMMDD)
      *     Line 2 (AFTER ADVANCING 2): Program name + office + page#
      *     Line 3 (AFTER ADVANCING 2): "EQUALIZATION FACTORS"
      *     Line 4 (AFTER ADVANCING 3): "YEAR  QUAD  FACTOR" (col hdrs)
      *
      *   AFTER PAGE:
      *     Ejects to a new page (form feed, 0x0C) before printing
      *     the date line. On a mainframe line printer, this would
      *     advance the paper to the top of the next page.
      *
      *   LINE-CNT = 8:
      *     After printing 4 header lines with spacing of 1+2+2+3 = 8
      *     line-equivalents, the counter is reset to 8. This means
      *     55 - 8 = 47 more line-equivalents are available for
      *     detail lines (at 2 lines each = ~23-24 details per page
      *     before the next page break).
      *
      *   HDG-PG (page number):
      *     PAGE-CNT is incremented, then moved to HDG-PG (PIC ZZ9)
      *     which formats it with leading zero suppression for display.
      *
      *   WRITE ... FROM ...:
      *     Each WRITE copies the named work area (DATE-LINE, HDG-LINE,
      *     TTL-LINE, TTL-LINE2) into PRINT-REC and writes to the
      *     file. The work areas are shorter than 133 bytes, so COBOL
      *     space-pads the remainder automatically.
      *----------------------------------------------------------------
00189  060-HDG-ROUTINE.
      *    -- Increment page counter --
      *    In C: page_cnt++;
00190      ADD     +1 TO PAGE-CNT
      *    -- Format page number for display --
      *    Copy the integer page count into HDG-PG (PIC ZZ9),
      *    which automatically suppresses leading zeros.
      *    E.g., page 1 displays as "  1", page 12 as " 12".
      *    In C: sprintf(hdg_pg, "%3d", page_cnt);
00191      MOVE    PAGE-CNT TO HDG-PG
      *    -- Print header line 1: date --
      *    AFTER PAGE = form feed (0x0C). Ejects to a new page
      *    on a printer, or inserts a FF character in the file.
      *    DATE-LINE contains 11 spaces + YYYYMMDD date.
      *    In C: fprintf(f, "\f%s", date_line);  // FF + content
00192      WRITE   PRINT-REC FROM DATE-LINE
00193              AFTER PAGE
      *    -- Print header line 2: program name + office + page --
      *    AFTER ADVANCING 2 = skip one blank line before this.
      *    HDG-LINE = "CLREB020" + "OFFICE OF THE COUNTY CLERK"
      *    + "PAGE  N".
00194      WRITE   PRINT-REC FROM HDG-LINE
00195              AFTER ADVANCING 2
      *    -- Print header line 3: report title --
      *    AFTER ADVANCING 2 = skip one blank line before this.
      *    TTL-LINE = "EQUALIZATION FACTORS"
00196      WRITE   PRINT-REC FROM TTL-LINE
00197              AFTER ADVANCING 2
      *    -- Print header line 4: column headers --
      *    AFTER ADVANCING 3 = skip two blank lines before this.
      *    TTL-LINE2 = "YEAR  QUAD  FACTOR"
00198      WRITE   PRINT-REC FROM TTL-LINE2
00199              AFTER ADVANCING 3
      *    -- Reset line counter --
      *    The 4 header lines with their spacing used 8 line-
      *    equivalents (1+2+2+3=8). Set counter to 8 so detail
      *    lines are counted from here. Next page break at 55.
      *    In C: line_cnt = 8;
00200      MOVE    +8 TO LINE-CNT.
00201 *    SKIP1
00202  060-EXIT.
00203      EXIT.


