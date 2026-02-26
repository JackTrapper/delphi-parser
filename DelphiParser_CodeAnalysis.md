# DelphiParser.pas Knowledge Bootstrap

## Overview
`DelphiParser.pas` is the core syntax analysis engine for the DeLinter project. It takes a stream of lexical tokens (produced by `DelphiTokenizer.pas`) and constructs a full-fidelity syntax tree that represents Delphi/Pascal source code. 

The architecture is heavily inspired by Roslyn (C# compiler), where the resulting tree is a blend of internal syntax nodes (`TSyntaxNode2`) and leaf tokens (`TSyntaxToken`), ensuring that absolutely no source characters (including comments and whitespace) are lost in the translation.

## Core Components

### 1. `TSyntaxNode2` (The Syntax Node)
- **Why "2"?**: It is named `TSyntaxNode2` to avoid naming conflicts with the third-party `DelphiAST` library's `TSyntaxNode`.
- **Classification**: Each node has a `NodeType` of type `TSyntaxNodeType` (e.g., `ntCompilationUnit`, `ntIfStatement`, `ntProperty`).
- **Children**: Contains a list of `ChildNodes` wrapped in `TSyntaxNodeOrToken`, meaning a child can be either another `TSyntaxNode2` (a nested structure) or a `TSyntaxToken` (a leaf).
- **Attributes**: Semantic modifiers (like `anType`, `anVisibility`, `anClass`) are stored in an `Attributes: TDictionary<TAttributeName, string>` rather than structural nodes, making it easier for tools to query the node's properties.
- **Fidelity**: The tree intrinsically owns all trivia because the parser attaches tokens directly to the syntax nodes.

### 2. `TDelphiParser` (The Parser Engine)
- **Recursive Descent**: Implements a top-down, recursive descent parsing strategy.
- **Grammar Alignment**: Methods are named mapping directly to DGrok grammar productions (e.g., `ParseUnit`, `ParseClassType`, `ParseProperty`). Many of these methods have `IsPossibleXxxx` look-ahead counterparts.
- **Token Consumption**: 
  - `PeekToken()`, `PeekTokenKind()`: Look ahead without advancing.
  - `NextToken()`: Advances the cursor.
  - `EatToken(ExpectedKind)`: Asserts and consumes the expected token, advancing the cursor. Essential for building the tree.
  - `ConsumeKeyword()`: Helper to optionally grab a keyword while maintaining trivia alignment.
- **Error Recovery**: Handles malformed code by creating synthetic nodes or gracefully reporting issues using `SynError()` or exceptions like `ESyntaxError` / `EParserException`, without failing the entire parse.

## Design Rules for AI Agents

1. **Do not drop tokens**: The golden rule of this parser is full fidelity. Every token retrieved from the tokenizer MUST be attached to a `TSyntaxNode2`. Unattached tokens cause memory leaks and break round-trip capability.
2. **Follow DGrok Grammar**: When modifying or adding parsing methods, strictly adhere to the DGrok grammar rules (`Documentation/Grammar.yaml`). Match the method names (e.g., `ParseFieldDecl`).
3. **Use Expected Tokens**: Whenever a specific token structure is required by the grammar, use `EatToken(ptTokenKind)` so the tree retains the literal syntax elements (like `ptSemicolon`, `ptClass`).
4. **Build Upwards**: Create the `TSyntaxNode2` root for the production first, consume tokens and nested productions by adding them as children (`Result.AddChild()`), and then return the node. Set `Attributes` for semantic meaning rather than relying purely on branch hierarchy where appropriate.
