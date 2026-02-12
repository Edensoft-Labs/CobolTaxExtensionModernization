# CLREB Programs -- Clerk Equalization

Domain prefix **CLREB** (Clerk's Real Estate Board / Equalization). This single program handles equalization factor editing and validation.

---

## Problem Domain: Equalization Factors

Equalization factors are state-mandated multipliers used in the Illinois property tax system to ensure uniformity of assessed property values across different taxing jurisdictions. The Illinois Department of Revenue (IDOR) calculates and publishes these factors annually.

**Why equalization is needed:** Different township assessors may assess properties at different percentages of market value. If one assessor assesses at 25% and another at 33%, taxpayers in the second area would pay more tax on equivalent properties. Equalization factors correct this disparity by multiplying assessed values so that all jurisdictions effectively assess at the same rate.

**Cook County quadrants:** Cook County divides its territory into assessment quadrants (numbered 1 through 4). Each quadrant represents a geographic area that may have a different assessment level, and therefore a different equalization factor. Historically, Cook County has had among the highest equalization factors in the state because of its assessment practices.

**Typical factor values:** Real-world Cook County equalization factors typically range from about 2.0 to 4.0 (stored as "20000" to "40000" in the 5-digit format). A factor of 1.0000 ("10000") means no equalization is needed. Factors less than 1.0 are theoretically possible but rare (they would mean the assessment level is already above the state standard).

**"Cards":** The term "card" is a legacy from the era of 80-column IBM punch cards. Each "card" is one 80-byte input record. In the original mainframe workflow, a data entry operator would physically keypunch equalization factor data onto cards which were then fed into a card reader. Today, the "cards" are simply fixed-width records in a sequential file, but the 80-byte format and terminology persist.

---

## CLREB020

| Attribute | Detail |
|-----------|--------|
| **Program ID** | CLREB020 |
| **Purpose** | Edits and lists equalization factors, writing validated factors to a disk file. |
| **Authors** | Russ Dober, Ron Urbaniak |
| **Date Written** | September 25, 1978 |
| **Lines of Code** | 203 (original), ~1155 (with annotations) |
| **Copybook Dependencies** | None |
| **Subprogram Calls** | None |

### Inputs

| Logical Name | DD Name | GnuCOBOL Env Var | Description |
|-------------|---------|-------------------|-------------|
| CARD-FILE | UT-S-CARDS | DD_CARDS | Equalization factor input cards (80 bytes/record) |

### Outputs

| Logical Name | DD Name | GnuCOBOL Env Var | Description |
|-------------|---------|-------------------|-------------|
| PRINT-FILE | UT-S-PRINT | DD_PRINT | Equalization factor listing/audit report (133 bytes/record) |
| FACTOR-FILE | UT-S-FACTOR | DD_FACTOR | Validated equalization factor disk file (21 bytes/record) |

### Record Layouts

#### Card Input Record (80 bytes)

| Byte Position | COBOL Field | PIC Clause | Size | Valid Values | Description |
|---------------|-------------|------------|------|--------------|-------------|
| 1-2 | CD-YR | XX | 2 | "01"-"99" digits; "00" passes due to alphanumeric comparison quirk | Tax year (2-digit) |
| 3 | CD-QUAD | X | 1 | '1', '2', '3', '4' (88-level VALID-QUAD) | Assessment quadrant |
| 4-8 | CD-FACTOR / CD-FACTOR-RD | X(5) / 9V9999 | 5 | "00001"-"99999" digits; "00000" passes COBOL validation | Equalization factor (implied decimal: "29744" = 2.9744) |
| 4 | CD-FT1 | X | 1 | '0'-'9' | Factor integer part (subdivision of CD-FACTOR) |
| 5-8 | CD-FT4 | X(4) | 4 | "0000"-"9999" | Factor decimal part (subdivision of CD-FACTOR) |
| 9-80 | CD-FILLER | X(72) | 72 | Spaces (ignored) | Unused padding |

**Sequence key:** Bytes 1-3 (CD-YR + CD-QUAD) = the CARD group item. Cards must be in ascending alphanumeric order on this 3-byte key. Duplicate keys are allowed (LESS THAN check, not LESS THAN OR EQUAL).

#### Factor Output Record (21 bytes)

| Byte Position | COBOL Field | PIC Clause | Size | Value Range | Description |
|---------------|-------------|------------|------|-------------|-------------|
| 1-2 | FT-TAXYR | 99 | 2 | "00"-"99" (whatever passed input validation) | Tax year copied from CD-YR |
| 3 | FT-QUAD | 9 | 1 | "1"-"4" (only valid quads reach output) | Quadrant copied from CD-QUAD |
| 4-8 | FT-EQFACT | 9V9999 | 5 | "00000"-"99999" (represents 0.0000-9.9999); realistic: "10000"-"45000" (1.0-4.5) | Factor copied character-by-character from CD-FACTOR-RD |
| 9-21 | FILLER | X(13) | 13 | Always 13 space characters (0x20) | Initialized by MOVE SPACES TO FACTOR-REC |

**Output invariants:** Every factor record is exactly 21 bytes. Bytes 1-8 are always digit characters ('0'-'9'). Bytes 9-21 are always spaces. Record count equals OUT-CNT displayed on stdout.

#### Print Detail Line (133 bytes)

| Byte Position | COBOL Field | Content | Value Range | Notes |
|---------------|-------------|---------|-------------|-------|
| 1-52 | FILLER | 52 spaces | Always spaces | Left margin |
| 53-54 | WK-YR | Year | "00"-"99" or non-numeric chars | From CD-YR (printed for both valid and invalid cards) |
| 55-62 | FILLER | 8 spaces | Always spaces | Spacer |
| 63 | WK-QUAD | Quad | Any single character | From CD-QUAD (printed even if invalid) |
| 64-70 | FILLER | 7 spaces | Always spaces | Spacer |
| 71-76 | WK-FACT | "N.NNNN" | "0.0000"-"9.9999" for valid; garbled for non-numeric | Factor with decimal point inserted |
| 77-81 | FILLER | 5 spaces | Always spaces | Spacer |
| 82-92 | WK-MESG | Error msg | "NOT NUMERIC" (11 chars) or 11 spaces | Error indicator for invalid cards |
| 93-133 | (auto-pad) | 41 spaces | Always spaces | COBOL auto-pads to 133 |

**Print output invariants:** Every detail line is preceded by AFTER ADVANCING 2 (double-spaced). The print file always starts with a page header (4 lines). A new page header is emitted whenever LINE-CNT exceeds 55 (~24 details per page).

#### Realistic Values for Cook County

| Scenario | Year | Quad | Factor | Display | Meaning |
|----------|------|------|--------|---------|---------|
| Typical current | "25" | "1" | "29744" | "2.9744" | 2025, North quad, assessed values multiplied by ~2.97 |
| Typical current | "25" | "2" | "31205" | "3.1205" | 2025, West quad, factor 3.1205 |
| Historical | "10" | "3" | "25000" | "2.5000" | 2010, South quad, factor 2.5 |
| No equalization needed | "25" | "4" | "10000" | "1.0000" | Assessment already at state level |
| Extreme reassessment | "25" | "1" | "45000" | "4.5000" | Very high factor (has occurred historically) |
| Minimum nonzero | "25" | "1" | "00001" | "0.0001" | Theoretically valid but unrealistic |
| Maximum possible | "25" | "1" | "99999" | "9.9999" | Extreme upper bound of 5-digit format |

### Copybooks

None. CLREB020 has zero copybook dependencies, making it the simplest program in the system for conversion purposes.

### Validation Rules

1. **CD-YR** must be NUMERIC (all digits) AND GREATER THAN 0 (alphanumeric comparison against "0 ")
2. **CD-FACTOR** must be NUMERIC (all digits) AND GREATER THAN 0 (alphanumeric comparison against "0    ")
3. **CD-QUAD** must satisfy 88-level VALID-QUAD: VALUE '1' THRU '4'
4. Cards must be in ascending sequence by CARD group item (bytes 1-3: year+quad)

### Error Handling

- **Validation failure:** Card is printed on the report with "NOT NUMERIC" in the message column. No factor record is written. ERROR-CNT is incremented.
- **Sequence error:** Program terminates immediately with RETURN-CODE = 16. Diagnostic messages ("CARDS OUT OF SEQUENCE", current and previous card keys) are displayed to stdout. Factor records written before the error are preserved.

### Key Business Logic

- Reads equalization factor input cards (one factor per year+quadrant combination).
- Validates each factor against the rules above.
- Writes validated factors to a sequential disk file for use by downstream programs.
- Produces a printed audit listing of all cards (valid and invalid).
- The output factor file is consumed by programs ASHMA850, ASHMA855, and ASHMA857, which use these factors to compute equalized assessed values (EAV). An incorrect equalization factor would ripple through every tax bill in the affected quadrant.

### Program Flow

1. **010-BEGIN:** Open files, capture date, loop until EOF or sequence error, display counts, close files, stop.
2. **020-MAIN-LINE:** Read next card, validate, write factor (if valid) or count error (if invalid), write report line.
3. **030-READ-CARD:** Read one 80-byte record, check sequence against previous card key.
4. **040-CREATE-FACTOR:** Build 21-byte factor record (character copy, not arithmetic), write to FACTOR-FILE.
5. **050-WRITE:** Check page break, format factor display as "N.NNNN", write report line, clear error message.
6. **060-HDG-ROUTINE:** Print 4-line page header (date, program/office/page, title, column headers).

---

## Downstream Impact

The validated factor file produced by CLREB020 feeds directly into:

| Program | Purpose | How Factor is Used |
|---------|---------|-------------------|
| ASHMA850 | Senior Freeze calculation | Multiplies assessed values by equalization factor to compute EAV |
| ASHMA855 | Senior Freeze calculation (variant) | Same as ASHMA850 |
| ASHMA857 | Senior Freeze calculation (variant) | Same as ASHMA850 |
| ASREA864 | Homestead EQ valuation updates | Updates equalized assessed values using factor |

An error in the factor file propagates to every property tax bill in the affected quadrant for the affected year.

---

## Domain Notes

- Equalization factors are state-mandated multipliers applied to assessed values to bring them in line with statewide assessment levels.
- This is the entry point for equalization factor data into the system.
- The output factor file is critical -- it feeds into Senior Freeze calculations (ASHMA850/855/857), homestead EQ valuation updates (ASREA864), and frozen valuation processing.
- The factor file record layout (21 bytes) is defined by copybook `REBEQFRD01.cpy`, though CLREB020 itself does not use COPY statements (the layout is defined inline in the FD).
