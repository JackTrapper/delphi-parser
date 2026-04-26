# delphi-parser

The **TDelphiParser** class turns Delphi source code into a syntax tree.

Sample Usage
------------

```delphi		
program DumpTree;

uses
	DelphiParser;

var
	syntaxTree: TSyntaxTree;
begin
	syntaxTree := TDelphiParser.ParseText(SourceCode);
end.	
```

The returned `TSyntaxTree` contains the root node of the syntax tree.

- `syntaxTree: TSyntaxTree`
	- `📁Root: TSyntaxNode2` (`ntCompilationUnit`)
		- ...child nodes...
    

**Note:** The returned tree is a **Syntax Tree**, rather than an **Abstract Syntax Tree** (AST), 
	meaning the tree preserves all tokens (`TSyntaxToken2`) and trivia (whitespace/comments) from the source code.

For the sample code above, the resulting **Root** node of the syntax tree looks like:

```
📁ntCompilationUnit
└─ 📁ntProgram
│  ├─ 🔤ptProgram("program")	
│  ├─ 📁ntIdentifier anName="DumpTree"
│  │  └─ 🔤ptIdentifier("DumpTree")
│  ├─ 🔤ptSemiColon(";")	
│  ├─ 📁ntUses
│  │  ├─ 🔤ptUses("uses")
│  │  ├─ 📁ntUsedUnit anName="DelphiParser"
│  │  │  └─ 📁ntQualifiedIdentifier anName="DelphiParser"
│  │  │     └─ 🔤ptIdentifier("DelphiParser")
│  │  └─ 🔤ptSemiColon(";")
│  ├─ 📁ntVarSection
│	│  ├─ 🔤ptVar("var")
│  │  └─ 📁ntVariable
│  │     ├─ 📁ntIdentifierList
│  │     │     📁ntIdentifier anName="syntaxTree"
│  │     ├─ 🔤ptIdentifier("syntaxTree")
│  │     ├─ 🔤ptColon(":")	
│  │     ├─ 📁ntType anName="TSyntaxTree"
│  │     │  └─ 🔤ptIdentifier("TSyntaxTree")
│	│     └─ 🔤ptSemiColon(";")
│  ├─ 🔤ptBegin("begin")
│  └─ 📁ntStatementList
│  │  └─ 📁ntStatement
│  │     └─ 📁ntAssign
│  │        ├─ 📁ntParticle
│  │        │  └─ 📁ntIdentifier anName="syntaxTree"
│  │        └─ 📁ntParticle
│  │           ├─ 📁ntIdentifier anName="TDelphiParser"
│  │           ├─ 📁ntIdentifier anName="ParseText"
│  │           └─ 📁ntParticle
│  │              └─ 📁ntIdentifier anName="SourceCode"
│  ├─ 🔤ptEnd("end")
│  └─ 🔤ptDot(".")
└─ 🔤ptEof
```

Legend: 📁Node 🔤Token


Tokenizer 
=========

`DelphiTokenizer.pas` implements the tokenizer, which accepts a stream of characters and produces a series of tokens (`TSyntaxToken2`):

<kbd>🔤unit</kbd> <kbd>🔤System</kbd> <kbd>🔤.</kbd> <kbd>🔤Generics</kbd> <kbd>🔤.</kbd> <kbd>🔤Collections</kbd> <kbd>🔤;</kbd> ⇐ `unit System.Generics.Collections;`

Each token has its own Leading/Trailing trivia tokens. For example:

Sample code:

```pascal
unit System.Generics.Collections;
```

Resulting tokens:

- 🔤**ptKeyword**(Text="`unit`")
     - TrailingTrivia=[🔤**ptWhitespace**(Text="`␣`")]
- 🔤**ptIdentifier**(Text="`System`") 
- 🔤**ptDot**(Text="`.`")  
- 🔤**ptIdentifier**(Text="`Generics`")
- 🔤**ptDot**(Text="`.`")
- 🔤**ptIdentifier**(Text="`Collections`")
- 🔤**ptSemiColon**(Text="`;`")
	- TrailingTrivia=[🔤**ptCRLF**(Text=`#13#10`)]

Syntax Tree Design
------------------

Every node in the tree (**TSyntaxNode**) can have child nodes:

- **ChildNodes**: TObjectList<TSyntaxNodeOrToken>;
	- 📁**TSyntaxNode** (nonterminal)
	- 🔤**TSyntaxToken2** (terminal token)

The property **ChildNodes** contains a list of **TSyntaxNodeOrToken** objects:

	TSyntaxNode.ChildNodes: TObjectList<TSyntaxNodeOrToken>;

**TSyntaxNodeOrToken** is a wrapper that can represent either a child node or a token:

- 🔤**TSyntaxNode**
	- **ChildNodes**: TObjectList<TSyntaxNodeOrToken>;
		- property **IsNode**: Boolean; *(is this thing a Node?)*
		- property **IsToken**: Boolean; *(is this thing a Token?)*
		- property **AsNode**: 📁TSyntaxNode; *(current thing as a Node)*
		- property **AsToken**: 🔤TSyntaxToken2; *(current thing as a Token)*


Parser Code Structure
---------------------

### Parser Methods

The parser is organized into methods corresponding to the names of grammar productions:

- **Grammer**: `Xxx`
- **Method**: `ParseXxx`

for example:

- **Grammer**: `ImeplementationDecl`
- **Method**: `ParseImplementationDecl`

This makes it easy to know what method is for what produciton. 

#### More example of parse methods


```
Program
	-> PROGRAM Ident ';'
			[UsesClause]
			(ImplementationDecl)*
			[BEGIN StatementList]
			END '.'
```

The parser will methods called:

- Parse**Ident**
- Parse**UsesClause**
- Parse**ImplementationDecl**
- Parse**StatementList**

### ParseXxx Methods Return a Node


The grammer production methods are functions that **return** a `TSyntaxNode`. E.g.:

- `function ParseUsesClause: TSyntaxNode2;`
- `function ParseImplementationDecl: TSyntaxNode2;`
- `function ParseStatementList: TSyntaxNode2;`

This makes it easier to build the tree in code, which a much easier to read idiom:

```delphi
Result.AddChild(ParseGotoStatement);
```

### Example Production Method

The **GotoStatement** production:

```
GotoStatement
	-> GOTO LabelId
```

has corresponding **ParseGotoStatement** method:

```delphi
function TDelphiParser.ParseGotoStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntGoto);
	Result.AddChild(EatToken(ptGoto));
	Result.AddChild(ParseLabelId);
end;
```


## Parsing Optional Grammar Parts

When a piece of grammer `Xxx` is optional, we usually have a method named `IsPossibleXxx` to peek ahead to see if it is present:

- **RequiresClause**
- Parse**RequiresClause**
- IsPossible**RequiresClause**

For example:

```
Package
	-> PACKAGE QualifiedIdent ';'
			[RequiresClause]
			[UsesClause]
			(AssemblyAttribute)*
			END '.'
```

In this case the **RequiresClause** production is optional. For this we use the method naming scheme:

- **Grammer**: `RequiresClause`
- **Parser Method**: `function ParseRequiresClause: TSytnaxNode2`
- **Check Existence**: `IsPossibleRequiresClause: Boolean`

The DelphiParser is a recursive descent parser, so the `IsPossibleXxx` method will peek if the next token(s) match the expected pattern for the `Xxx` grammer.

## Token Eating, Expecting, and Peeking

The parser has 3 ways to navigate tokens:

1. Consume (Eat) the current token, return it, and advance to the next token
	- `EatToken(ExpectedTokenKind): TSyntaxToken;` *(eat, create an IsMissing token if it's not)*
	- `EatTokenEx(ExpectedTokenContentualKind): TSyntaxToken;` *(eat, create an IsMissing token if it's not)*
	- `EatToken(): TSyntaxToken;` *(TODO: remove)*
2. Peeking ahead to future tokens:
	- `PeekToken: TSyntaxToken;`
	- `PeekToken(n: Integer): TSyntaxToken;` *(0:CurrentToken, 1:PeekToken, 2:...)*
	- `PeekTokenKind: TptTokenKind;` *(helper for PeekToken.TokenKind)*
	- `PeekTokenExID: TptTokenKind;` *(helper for PeekToken.ExID)*
3. Reading the current token
	- `CurrentToken: TSyntaxToken`
	- `CurrentTokenKind: TptTokenKind` *(helper for CurrentToken.TokenKind)*
	- `CurrentTokenContentualKind: TptTokenKind` *(helper for CurrentToken.TokenContextualKind)*

The main parser entry point is:

	function ParseCore: TSyntaxTree; virtual; //as a nice way to split plumbing from grammer

This is what the descendant parsers will override to implement the actual grammer parsing, while the base class will provide all the plumbing for loading files, handling encodings, tokenizing, and providing the token navigation methods (EatToken, PeekToken, etc).

---

**TODO**: I want to remove all the delphi grammer parsing to its own class, and have the base parser support all the file loading, encoding, loading tokens, providing navigation methods, etc in the base class. i want to get all the Delphi grammer parsing logic to it's only class. And it's **not** to split it into like Delphi5Parser, Delphi7Parser, DelphiXE7Parser, Dephi13Parser. No, that will be done with feature flags. Every new language feature as Delphi moved on will have to cataloged and given a code, and then the parser shold retroactively check for **IsFeatureAvailble(IDS_FeatureMultilineStringLiteral)**.  But i think it should be:
 
 	CheckFeatureAvailablity(node: TSyntaxNode, Feature: Integer);
 
 Is if a token or feature is being used, we call **CheckFeatureAvailability** to ensure it is supported. If not, an error node is added as a child to the specified node. So the parser doesn't pretend to be the older language. It tells you that what you're using isn't supported; it's an error. But the parser will happily understand it for you.

---

### Eating

When **eating** a token, if the current token is not the expected kind, the `EatToken`/`EatTokenEx` method will synthesize a **TSyntaxToken** of the expected type, but with the `IsMissing` flag set to `True`:

	missingToken := TSyntaxToken.Create(ExpectedTokenKind);
	missingToken := TSyntaxToken.Create(ExpectedTokenKind);

For example the following code is missing a semicolon:

```pascal
unit Test //should be a ptSemicolon token here
interface
```

The parser will synthesize a token in the place one should be, and place it in the parse tree as appropriate (just with the flag `IsMissing: Boolean = True`). This allows the parser to continue parsing and produce a syntax tree even in the presence of syntax errors.

You can use the presence of these `IsMissing` tokens to build a list of syntax errors.

-----------

WARNING: TODO: Need to remove 

	function EatToken(): TSyntaxToken;
	
and replace it with:

	function EatToken(const ExpectedTokenType: array of TptTokenKind): TSyntaxToken;

You really should be forced to specify what token kind you expected, so we can create the missing token for you. I am certain `EatToken()` existed because i was too lazy to figure out which token kind, or it is allowed to be more than one.

- TODO: is anyone creating the missing token outside EatToken?
- TODO: If they do specify more than 1, which one do we use when creating the missing token? Is any one ok? Is the first one preferable; does that match Delphi? Or is this important enough that we should definitely have the user choose. But then the only reason to choose is because one has a preference, and if that is the case, then use `EatToken(tokenKind)` as your error message.
- TODO: Is there some more complicated reason why `EatToken()` has to stay?

--------------

### Reading the current token

The **CurrentToken** is the token that will be consumed by the next call to **EatToken** or **EatTokenEx**. 

- `property CurrentToken: TSyntaxToken;`
- `property CurrentTokenKind: TptTokenKind;` *==>`CurrentToken.TokenKind`*
- `property CurrentTokenContentualKind: TptTokenKind;` *==>`CurrentToken.ContextualKind`*

### Peeking

Calling **PeekToken** returns the next token:

- `PeekToken: TSyntaxToken;` returns the next token
- `PeekToken(0): TSyntaxToken;` returns the current token (same as CurrentToken)
- `PeekToken(1): TSyntaxToken;` returns the next token (same as PeekToken)
- `PeekToken(n: Integer): TSyntaxToken;` returns the nth token ahead (0 for current, 1 for next, etc.)

---

**TODO:** I wonder if there is any performance benefit of having an actual `FCurrentToken` and `FPeekToken`. Right now it's just the current index (`FCurrent`) into a **TList** (`FTokens`). So an `FCurrentToken` and `FPeekToken` would allow a prefetch, or at the very least avoid a **TList** index lookup, and be a cache. Or, you know, it makes absolutely no difference. I dunno! Perhaps the savings is in the shuffle `FCurrentToken := FPeekToken`, and you save an indexed lookup. Long before we get there, we have to get string interning (sharing) working, token arena and dictionary lookup, node arena.

---

# Next Steps

- Implement the preprocessor, right now it is just a stub passthrough
- Test the case of a program fragment (i.e. just some code), that they parser went haywire over
- have the test harness look for `IsMissing` tokens and report them as syntax errors, with line/column info from the token
- tokens are interned based on `Kind`, `Text`, `ContextualKind`, `Text`, `ValueText`, `Width`, `FullWidth`. But how does Roslyn handle trivia? Surely comments don't get interred with the tokens...
- and look at all those TODOs...
