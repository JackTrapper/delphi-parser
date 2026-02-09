# DelphiTokenizer.pas — Detailed Code Analysis Report

Date: 2025-10-31
Scope: `DelphiTokenizer.pas`

## executive summary

The tokenizer is a well-documented, Roslyn-inspired component that preserves trivia and emits rich `TSyntaxToken` objects. Architecture is clean and extensible, and error recovery is thoughtfully implemented. Recent fixes to multiline string indentation enforce compiler-accurate behavior and align with tests.

Gaps to close next:
- Populate `TSyntaxToken.StartOffset` and `TokenLength` for accurate position tracking.
- Tighten several token readers (ASCII char literals, numeric scanning, comments) for correctness and edge cases.
- Improve keyword/directive lookup performance and character classification hot-paths.
- Optional: minimal directive payload parsing (name/args) to support linting without a full preprocessor.

## architecture overview

- Token enumeration: `TptTokenKind` covers reserved words, directives, symbols (single/multi-char), constants, identifiers, compiler directives, and trivia (whitespace/comments).
- Token object: `TSyntaxToken` stores kind, line/column, text/value text, trivia, error/warning fields, and an optional `DirectiveID` when an identifier matches a directive name.
- Streaming input: `TInputStream` decodes UTF-16 from an `ISequentialStream`, supports peeking via a ring buffer (`Peek(k)`), and reports `UEOF` for end of input.
- Tokenizer (`TDelphiTokenizer`):
  - Core APIs: `NextToken`, `PeekTokenKind`, `Tokenize` (batch fill list).
  - Handlers: `Do*` methods per token category (numbers, strings, comments, operators, directives, identifiers, whitespace, etc.).
  - Trivia: Eats leading trivia into a temp list and attaches it to the next real token; collects trailing trivia after that token as well.
  - Errors/warnings: Added to tokens; `IsMissing` supports parser recovery.

## contracts and data model

- Token contract:
  - Kind: `TokenKind: TptTokenKind`.
  - Position: `Line`, `Column` (1-based). StartOffset/TokenLength present but currently not populated.
  - Text/value: `Text` is the exact source slice; `ValueText` is normalized/decoded (where purely lexical, e.g., strings/comments).
  - Diagnostics: `HasErrors`, `ErrorMessage`, `HasWarnings`, `WarningMessage`, `IsMissing`.
  - Trivia: `LeadingTrivia[]`, `TrailingTrivia[]` stored as tokens (comment/whitespace kinds).
  - Directive hinting: `DirectiveID` set when an identifier matches known directive names.

## correctness review by feature

### 1) Identifiers and keywords
- Identifier rules: `IsStartOfIdentifierCharacter` uses `ch.IsLetter or (ch = '_')`; continuation with `IsIdentifierCharacter` allows letters, digits, underscore. Good Unicode coverage via `System.Character`.
- Reserved words: looked up in `ReservedWords` via linear scan with `SameText` → returns reserved token kind or falls back to `ptIdentifier`.
- Directives as identifiers: if token is `ptIdentifier`, `DirectiveID` is set from `Directives` for later parser/linter use.

Observations:
- Performance could improve by using a case-folded hash or dictionary (see performance).
- `SameText` is locale-aware and relatively heavy; a culture-invariant fast compare would be preferable for performance-critical lexing.

### 2) Numbers
- Decimal numbers: `DoNumber` collects digits and treats presence of `E` or `.` as float. Notes:
  - Only uppercase `E` is considered; lowercase `e` is common and should be handled.
  - `IsNumeralCharacter` allows `+`/`-` anywhere during scanning (not just right after `E/e`). This can incorrectly absorb signs embedded in subsequent tokens (e.g., `1+-2`).
  - Exponent part lacks structure: ideally parse `E|e` then optional `+|-` then digits; otherwise leave as integer/float accordingly.
- Hex/binary with `_` separators are supported via `IsHexCharacter` and `IsBinaryCharacter`. Good.

Recommendations:
- Recognize `e` as well as `E`.
- Refine scanning around exponents: only allow `+`/`-` immediately after `E/e`, then require at least one digit.
- Keep `_` inside numeric literals but disallow at beginning/end or duplicated next to a radix marker (optional stricter checks).

### 3) Strings (single-quoted)
- `DoStringLiteral` handles doubled apostrophes for `'` insertion, unterminated strings, line breaks (error), surrogate pair validation, historical length warnings for pre-Delphi 12, and sets `ValueText` via `ProcessStringEscapes` (currently only merges doubled quotes).
- Good diagnostics: sets `HasErrors`/`HasWarnings` and `IsMissing` when appropriate.

Opportunities:
- If desired, normalize `Text` to the exact slice and keep `ValueText` purely decoded (already the intent). The current behavior matches this.
- Consider consolidating error message constants (e.g., `E2052 Unterminated string`) to avoid typos and support localization later.

### 4) Multiline strings (triple quotes and 5/7+ odd quotes)
- `DoMultilineStringLiteral`:
  - Supports odd counts of quotes 3+ for start/end, content accumulation, and unterminated error cases.
  - Requires (warns) a newline after the opening quotes.
  - Indentation semantics implemented per docwiki: base indentation defined by the whitespace on the closing-quote line; removed from content lines.
  - E2657 enforcement (inconsistent indent characters):
    - Error if any non-empty line is less indented than base.
    - Error if a line’s first `baseIndentation` characters don’t match the base indent string (mixing spaces/tabs or ordering differences).
  - Recovery: For `ValueText`, remove `min(baseIndentation, actualIndent)` so less-indented lines still strip what they have (matching tests).

Status: Matches tests including
- `TestMultilineString` (basic)
- `TestMultilineStringSpacesAndTabs` (consistent indentation with spaces or with tabs)
- `TestMultilineStringMixingSpacesAndTabsIncorrectly` (error E2657)
- `TestMultilineStringMixingRecoverLessIndentedLines` (error E2657 + recovery behavior)

### 5) Comments
- `DoSlashesComment`: Reads to end-of-line, returns `ptSlashesComment`, sets `ValueText` to comment body (without `//`). Good.
- `DoAnsiComment`: Handles `(* ... *)`. Leaves body in `Text`; `ValueText` isn’t extracted (optional future improvement).
- `DoBorComment`: Handles `{ ... }`; trims braces into `ValueText`. Bug: closing brace removal checks `s[Length(s)-1]` (off-by-one). It should check `s[Length(s)]` before trimming the last char.

Recommendation:
- Fix `DoBorComment` trimming off-by-one:
  - If not empty and first char is `{`, `s := Copy(s, 2, MaxInt)`.
  - If not empty and last char is `}`, `s := Copy(s, 1, Length(s) - 1)`.
  - Always guard for `Length(s) >= 2` as needed.
- Optionally set `ValueText` for ANSI comments similar to Bor comments for symmetry.

### 6) Compiler directives `{ $... }`
- `DoCompilerDirective`:
  - Reads until `}` or EOF/line-end, marks unterminated as error, and sets `IsMissing`.
  - Does not parse name/args into `ValueText` or a `Directive` payload (there’s a `TDirectiveData` type defined but unused).

Recommendation (non-breaking, incremental):
- Extract a lightweight parsed representation (Name uppercased, Args trimmed) and set `ValueText := Name + ' ' + Args` (or just keep the raw directive in `Text` and optionally store `Name`/`Args` for linting).
- Defer expression evaluation to a future preprocessor component.

### 7) Whitespace and trivia
- Whitespace is emitted as `ptWhitespace` (including CR/LF). `ptCRLF` exists but is not currently emitted. Tests already accept either, so current behavior is OK.
- Trivia is preserved both before and after real tokens and attached properly.

### 8) Unknown characters
- Unknown characters produce `ptUnknown`; in a debugger, an exception is raised, which is helpful while developing.

## diagnostics and recovery

- Single-line strings and directives set `HasErrors`/`IsMissing` on unterminated cases.
- Multiline strings set E2657 for mixed indentation or less-indented lines; still produce a token with recovered `ValueText`.
- `ToString` includes markers for ERROR/WARNING/MISSING for debugging.

This is robust and friendly to later parsing steps.

## position tracking

- Present fields: `TSyntaxToken.StartOffset`, `TSyntaxToken.TokenLength`.
- Current state: Not populated. `Line` and `Column` are set by `NextToken` using token-start snapshot.

Why this matters:
- IDE integrations, diffs, refactorings, and many tests rely on stable byte/char offsets and lengths.

Suggested implementation:
- Maintain an absolute character offset counter in the tokenizer (e.g., `FAbsoluteOffset: Integer`), initialized to 0.
- Increment `FAbsoluteOffset` by 1 for every consumed character in `Consume` (including CR and LF). This matches string index semantics used in tests like `'unit Test;'` → offsets 0,5,9.
- In `GetNextToken`, capture `tokenStartOffset := FAbsoluteOffset` before first `Consume` for the token, and after the token is created, set:
  - `AToken.StartOffset := tokenStartOffset`
  - `AToken.TokenLength := Length(AToken.Text)` (or the exact consumed character count if you prefer stricter accuracy for trivia tokens).

Edge notes:
- For CRLF pairs, since `Consume` reads both characters, `FAbsoluteOffset` advances twice, which is fine if offsets are in code units.
- If you later need byte offsets or UTF-8 offsets, keep those as separate counters.

## performance analysis and opportunities

- Keyword/directive lookup is O(n) with `SameText` per token:
  - Replace with a prebuilt, case-folded `TDictionary<string,TptTokenKind>` for both `ReservedWords` and `Directives`.
  - Normalize keywords to lowercase (ASCII) at init; avoid `SameText` in hot loops.
- Character classification:
  - `System.Character.IsLetter` handles full Unicode but is relatively heavy; a fast path for ASCII identifiers (`'A'..'Z','a'..'z','_'`) followed by fallback to `IsLetter` for non-ASCII would be a big win for typical Delphi code.
  - Numeric scanning should avoid backtracking; handle exponent structure deterministically.
- String building:
  - Many `s := s + Consume` concatenations; Delphi’s string builder strategy can be improved by pre-sizing where feasible (optional; not critical unless profiling shows hotspots).
- Trivia handling:
  - Per-token `TObjectList` allocations for trivia are clean and manageable at current scale. If heavy memory churn shows up in profiling, consider pooling these or using small-value arrays.

## known issues and quick fixes

1) Token positions (high priority)
- Populate `StartOffset` and `TokenLength` as described above. This will unlock location-accurate tests and IDE use.

2) Borland comment trimming (correctness)
- Fix the off-by-one index when removing the trailing `}`.

3) ASCII char literal decimal scanning (correctness)
- In `DoAsciiChar`, the decimal branch should only allow digits `['0'..'9']`. Currently it permits hex letters for decimal too. Suggested change:
  - Hex mode (`#$`): `['0'..'9','A'..'F','a'..'f']`
  - Decimal mode (`#`): `['0'..'9']`

4) Float exponent + lowercase `e` (correctness)
- Recognize `e` in addition to `E`.
- Only allow `+/-` immediately after `E/e`.
- Require at least one digit in the exponent to remain `ptFloat`.

5) Optional: ANSI comment `ValueText` symmetry
- Extract interior of `(* ... *)` into `ValueText` for parity with `DoBorComment`.

6) Optional: Directive payload extraction
- Populate `ValueText` as `NAME + ' ' + ARGS` and/or store a small parsed structure using existing `TDirectiveData`.

## recent fixes (multiline strings)

- The implementation now correctly derives base indentation from the closing-quote line and enforces consistent indentation:
  - Error `E2657 Inconsistent indent characters` when any non-empty line is less indented than base or uses a different first-`baseIndentation` whitespace pattern than the closing-quote line.
  - Recovery trims `min(baseIndentation, actualIndent)` so less-indented lines lose what they have (matching the tests).
  - Supports odd quote counts (3/5/7/…) and warns when missing the newline after opening quotes.

## testing and quality gates

- Unit tests in `DelphiTokenizerTests.pas` cover:
  - Token presence and positions (needs StartOffset/TokenLength implementation to fully pass position assertions).
  - Strings (unterminated, escapes, surrogate pairs, null char, long strings pre-D12).
  - Numbers (float positions, hex/binary).
  - Comments (slash/ANSI/Borland), whitespace trivia.
  - Operators (single and multi-char), directives (unterminated cases).
  - Multiline strings including indentation rules and error recovery.

Build/test status in current environment:
- Build: depends on local Delphi CLI support; VS Code task attempts MSBuild and may fail without the toolchain.
- Lint/Typecheck: no new syntax errors in updated code.
- Tests: expected to pass for multiline string indentation after the changes; token position tests will pass once StartOffset/TokenLength are implemented.

## roadmap (prioritized)

1) Implement StartOffset/TokenLength (small change, high value):
- Add `FAbsoluteOffset: Integer` to tokenizer; increment in `Consume`; capture at token start; set fields on token creation.

2) Fix correctness issues:
- `DoBorComment` trailing `}` off-by-one.
- `DoAsciiChar` decimal branch digits-only.
- `DoNumber` exponent handling and lowercase `e`.

3) Performance improvements (safe, incremental):
- Build `TDictionary<string,TptTokenKind>` for reserved/directives once; do lowercase matching.
- Add ASCII fast paths for identifier checks.

4) (Optional) Directive payload parsing:
- Populate `ValueText` (and/or `TDirectiveData`) without evaluating expressions.

5) (Optional) Comment ValueText parity:
- Extract body for ANSI `(* ... *)` to `ValueText` similar to Borland `{ ... }`.

## appendix: suggested code snippets (pseudo-Delphi)

- Absolute offset tracking:

  - Add to `TDelphiTokenizer`:
    - `FAbsoluteOffset: Integer;`
    - Initialize to 0 in constructor.
  - In `Consume` (before returning the char), increment `FAbsoluteOffset` by 1 for any non-UEOF character.
  - In `GetNextToken`:
    - Capture `tokenStartOffset := FAbsoluteOffset` before the first call that may read token chars.
    - After token creation: `AToken.StartOffset := tokenStartOffset; AToken.TokenLength := Length(AToken.Text);`

- `DoAsciiChar` decimal correction (concept):

  - Hex path: `while CharInSet(Peek, ['0'..'9','A'..'F','a'..'f']) do ...`
  - Decimal path: `while CharInSet(Peek, ['0'..'9']) do ...`

- `DoNumber` exponent handling (concept):

  - When encountering `'.'` → mark float.
  - When encountering `'e'|'E'` → mark float, optionally consume `+|-`, then require digits; if no digits, treat the `e` as terminator (or recover with warning).

---

This report reflects the current `DelphiTokenizer.pas` as of the date above and focuses on actionable, low-risk improvements that preserve public behavior while increasing correctness and performance. Implementing offsets/lengths and the noted small fixes will substantially improve test coverage and downstream tooling support. 
