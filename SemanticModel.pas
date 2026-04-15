unit SemanticModel;

(*
TCompilation
│
├─ TSyntaxTree (one per source file)
│    ├─ FNodeArena
│    ├─ FTokenArena
│    └─ Root: TSyntaxNode2
│
├─ TSemanticModel (per SyntaxTree)
│    ├─ SourceLocations
│    ├─ Scopes
│    ├─ Symbols
│    ├─ Bindings
│    ├─ Types
│    ├─ Constants
│    └─ ConversionTable
│
└─ TFlowAnalysis (per method or region)
     └─ FlowTable


These are the tables:

1. Source locations table: 			nodeID ==> TLocationInfo (StartOffset, EndOffset, StartLine, EndLine, TokenCount, ParentNodeID)
2. Scope table:							nodeID ==> TScopeInfo    (
3. Symbol table
4. Binding table:						nodeID ==> (e.g. SymbolID, ScopeID, SymbolKind)
5. Type table:							nodeID ==> (e.g. ExpressionKind, ResultType, Nullable)
6. Constant table:						nodeID ==> (e.g. ValueKind, IntValue, FloatValue, BooleanValue)
7. Flow table:
8. Diagnostics
9. Conversion table (todo)


Model of the entire project
-----------------------------

	Tokenizer					Converts text into tokens (DelphiTokenizer.TSyntaxToken)
		v
	Parser						Builds a Syntax Tree out of a file (DelphiParser.TSyntaxNode2)
		v
	Semantic						Builds semantic meaning tables out of the tree
		- Binding analysis   symbol info
		- Type analysis
		- Constant folding
		- Flow analysis
		- Diagnostics



1. Source Locations Table
================================================================================

Maps a nodeID to it's location in the source file.

	SourceLocations[NodeID] ==> TSourceLocationInfo

For example:

	unit U;

	procedure P;
	begin
	  X := A + B;
	end;

will have a SourceLocations table:

	| NodeId | NodeKind           | StartOffset | EndOffset | StartLine | StartColumn | EndLine | EndColumn | TokenCount | ParentNode |
	| ------ | ------------------ | ----------- | --------- | --------- | ----------- | ------- | --------- | ---------- | ---------- |
	| 1      | ntCompilationUnit  | 0           | 54        | 1         | 1           | 6       | 4         | 20         | null       |
	| 2      | ntUnitDeclaration  | 0           | 7         | 1         | 1           | 1       | 7         | 3          | 1          |
	| 3      | ntProcedureDecl    | 10          | 30        | 3         | 1           | 3       | 13        | 4          | 1          |
	| 4      | ntAssign           | 36          | 48        | 5         | 3           | 5       | 13        | 5          | 3          |
	| 5      | ntBinaryExpression | 41          | 47        | 5         | 8           | 5       | 12        | 3          | 4          |

Remarks
-------

Provides precise mapping between syntax nodes and source text.

Used for:
	- error reporting
	- IDE navigation
	- diagnostics
	- formatting tools

Why This Table Exists:
	- tokenizer only tracks widths of each token in order to remain immutable
	- building this table adds up all the lenghts, and counts line lenghts and line numbers.
	- But you can also go backwards:
			- find the the nodes that cover offset 237
			- where should i draw this red squiggle


2. Scope table
================================================================================

Defines where names live and how they are visible.

Every declaration occurs inside a scope.
Every identifier lookup walks outward through scopes until a symbol is found.

Without this table you cannot answer:
	- what X refers to
	- whether a variable shadows another
	- whether a symbol is visible
	- what with blocks do
	- which unit declarations are in scope

Example
--------

   unit U;

   interface

   procedure Foo(A: Integer);

   implementation

   procedure Foo(A: Integer);
   var
     X: Integer;
   begin
     if A > 0 then
     begin
       var Y: Integer;
       X := Y;
     end;
   end;

   end.

Scope
-----

	Unit Scope
		- Procedure Foo Scope
			- Block Scope
				- IF Block Scope


| ScopeId	| ScopeKind	| ParentScope	| DeclNodeId	| StartNode	| EndNode	| SymbolCount	| Notes					|
|-----------|-----------|--------------|--------------|-----------|-----------|--------------|--------------------|
| 1			| Unit		| null			| 2				| 1			| 40			| 3				| global unit scope	|
| 2			| Procedure	| 1				| 10				| 10			| 38			| 1				| parameters			|
| 3			| Block		| 2				| 14				| 14			| 36			| 1				| local variables		|
| 4			| Block		| 3				| 20				| 20			| 30			| 1				| IF block				|


Scopes are created by constructs like:

	- unit							yes					ntUnitDeclaration
	- interface section			yes					ntInterfaceSection
	- implementation section	yes					ntImplementation
	- type declarations			yes
	- class/record definitions	yes
	- procedure/function			yes
	- begin..end blocks			yes
	- for/while/repeat bodies	yes
	- anonymous methods			yes
	- with statements				special case

3. Symbol Table
================================================================================

Scopes contain symbols.

	[symbolID] ==> (Name, ScopeID, TypeID)

Symbol table
-------------

| SymbolId	| Name	| SymbolKind		| DeclNodeId	| ScopeId	| TypeId    |
|-----------|--------|-----------------|--------------|-----------|-----------|
| 100			| Foo		| Procedure			| 10				| 1			| proc		|
| 101			| A		| Parameter			| 11				| 2			| Integer	|
| 102			| X		| LocalVariable	| 15				| 3			| Integer	|
| 103			| Y		| LocalVariable	| 21				| 4			| Integer	|


Binding (the next table) resolves identifiers like:

	X := Y;

Resolution algorithm:

	currentScope = node.scope

	while currentScope <> nil
		search symbols declared in currentScope
		if found
			bind
		else
			currentScope = parentScope



4. Binding Table
================================================================================

Resolves identifiers to symbols.

   Bindings[NodeID] ==> TBindingInfo;

For example:

	X := A + B

will have a Bindings table:

	| NodeId | Identifier | SymbolId | SymbolKind    | DeclNodeId | ScopeId | AccessKind | Ambiguous |
	| ------ | ---------- | -------- | ------------- | ---------- | ------- | ---------- | --------- |
	| 14     | X          | 101      | LocalVariable | 8          | 3       | Write      | false     |
	| 17     | A          | 102      | Parameter     | 6          | 3       | Read       | false     |
	| 19     | B          | 103      | Field         | 4          | 1       | Read       | false     |


What's it used for:
	- Rules like:
			- unused variable
			- unknown identifier
			- variable shadowing
			- property vs field confusion



5. Types table
================================================================================

Determines the type of every expression.

   Types[NodeID] ==> TTypeInfo;

For example:

	A + B

you want to know the type(A) and type(B):

	| NodeId | ExpressionKind | ResultType | ExpectedType | ConversionApplied | IsLValue | SizeBytes | Nullable |
	|--------|----------------|------------|--------------|-------------------|----------|-----------|----------|
	| 17     | Identifier     | Integer    | Integer      | none              | true     | 4         | false    |
	| 18     | Identifier     | Integer    | Integer      | none              | true     | 4         | false    |
	| 19     | BinaryAdd      | Integer    | Integer      | none              | false    | 4         | false    |

What it's used for:

- Rules like:
		- incompatible assignment
		- suspicious type conversions
		- redundant casts
		- implicit numeric widening



6. Constants
================================================================================

Track expressions that evaluate at compile time.

For example:

	if 1 + 1 = 2 then 		//the condition is constant


	| NodeId | IsConstant | ValueKind | IntValue | FloatValue | StringValue | BoolValue | EvalStatus |
	| ------ | ---------- | --------- | -------- | ---------- | ----------- | --------- | ---------- |
	| 21     | true       | Integer   | 2        | null       | null        | null      | success    |
	| 22     | true       | Boolean   | null     | null       | null        | true      | success    |

What it's used for:

- Rules like:
		- dead branches
		- duplicate case labels
		- impossible conditions
		- constant loop detection



7. Flow Table (Control Flow Graph)
================================================================================

Tracks execution paths and variable state.

Without this table, you cannot detect:
	- uninitialized variables
	- unreachable code
	- overwritten assignments
	- missing return values

Example Code
------------

	if A then
		X := 1
	else
		X := 2;

	Y := X;

Example Control Flow Graph
--------------------------

	Entry
		|
		v
	Condition
		- True---> Assign1
		- False--> Assign2
		|
		v
	Use X


Flow Node Table
----------------

| FlowNode	| SyntaxNode	| BlockId	| Reachable	| Predecessors	| Successors	|
|-----------|--------------|-----------|-----------|--------------|--------------|
| 1 			| Entry			| 0			| true		| none			| 2				|
| 2			| Condition		| 1			| true		| 1				| 3,4				|
| 3			| Assign1		| 2			| true		| 2				| 5				|
| 4			| Assign2		| 3			| true		| 2				| 5				|
| 5			| UseX			| 4			| true		| 3,4				| exit			|

Variable State Table
---------------------

| NodeId	|SymbolId	| Assigned	| Read	| LastWriteNode	| PossiblyNil |
|--------|-----------|-----------|--------|-----------------|-------------|
| 5		| X			| true		| true	| 3/4					| false			|




8. Diagnostics table
================================================================================

Stores all warnings and errors produced by the analysis.

| DiagnosticId | Severity | Message                          | NodeId | StartLine | StartColumn | EndLine | EndColumn | Rule           |
| ------------ | -------- | -------------------------------- | ------ | --------- | ----------- | ------- | --------- | -------------- |
| DL0001       | Warning  | Variable assigned but never used | 14     | 5         | 3           | 5       | 4         | UnusedVariable |
| DL0002       | Error    | Unknown identifier               | 21     | 7         | 5           | 7       | 6         | UnknownSymbol  |




*)

interface

uses
	System.Generics.Collections,
	DelphiParser;

type
	TNodeID		= type Cardinal;
	TScopeID		= type Cardinal;
	TSymbolID	= type Cardinal;

	TScopeKind = (
		skUnknown,
		skUnit,
		skInterfaceSection,
		skImplementationSection,
		skType,
		skClass,
		skRecord,
		skProcedure,
		skFunction,
		skMethod,
		skBlock,
		skFor,
		skWith,
		skAnonymousMethod
	);

	TSymbolKind = (
		symUnknown,
		symUnit,
		symType,
		symConst,
		symVar,
		symField,
		symParameter,
		symProcedure,
		symFunction,
		symMethod,
		symProperty,
		symEnumMember
	);


	TLocationInfo = class
		NodeId: TNodeID;								//e.g. 1
		NodeKind: TSyntaxNodeType;					//e.g. ntCompilationUnit
		StartOffset, EndOffset: Integer;			//e.g. 0, 54
		StartLine, StartColumn: Integer;			//e.g. 1, 1
		EndLine, EndColumn: Integer;				//e.g. 6, 4
		TokenCount: Integer;							//e.g. 20
		ParentNode: TNodeID;							// e.g. null
	end;

	TScopeInfo = class
		ScopeId: TScopeID;							//e.g. 1
		ScopeKind: TScopeKind;						//e.g. Unit, Procedure, Block
		ParentScope: TScopeID;						//e.g. null
		DeclNodeId: TNodeID;							//e.g. 2
		StartNode: TNodeID;							//e.g. 1
		EndNode: TNodeID;								//e.g. 40
		SymbolCount: Integer;						//e.g. 3
		      Name: string;
	end;

   TSymbolInfo = class
		SymbolId: TSymbolID;							// 100
   	Name: string;									// "Foo"
      SymbolKind: TSymbolKind; 					// Procedure, Parameter, LocalVariable
      DeclNodeId: TNodeID;							// 10
		ScopeId: TScopeID;							// 1
      TypeNodeId: TNodeID;							//
	end;

	TBindingInfo = class
		NodeId: TNodeID;
		Identfier: Integer;					// X, A, B
		SymbolId: TSymbolID;					// 101
		SymbolKind: Integer;					// LocalVariable, Parameter, Field
		DeclNodeId: TNodeID;					// 8
      ScopeId: TScopeID;					// 3
      AccessKind: Integer;					// Read, Write
      Ambigious: Boolean;
	end;

	TTypeInfo = class
      NodeId: TNodeID;						// 17
      ExpressionKind: Integer;			// Identifier, BinaryAdd
      ResultType: Integer;					// Integer
		ExpectedType: Integer;				// Integer
      ConverstionApplied: Integer;		// none
      IsLValue: Boolean;					//
      SizeBytes: Integer;					// 4
		Nullable: Boolean;					// False;
	end;

	TConstantInfo = class
		NodeId: TNodeID;
		IsConstant: Boolean;
		ValueKind: Integer;					// Integer, Float, String, Boolean
		IntValue: Int64;						// 2
		FloatValue: Real;						// 0
		StringValue: string;					// ''
		BoolValue: Boolean;					//False;
		EvalStatus: Integer;					// success
	end;

{
	The object that holds all the semantic meaning from a tree
	The tables will be populated when you construct the model.
}
	TSemanticModel = class
	private
		FRootNode: TSyntaxNode2;

		FSourceLocations:	TDictionary<TNodeID,		TLocationInfo>;
		FScopes:				TDictionary<TScopeID,	TScopeInfo>;
		FSymbols:			TDictionary<TSymbolID,	TSymbolInfo>;
		FBindings:			TDictionary<TNodeID,		TBindingInfo>;
		FTypes:				TDictionary<TNodeID,		TTypeInfo>;
		FConstants:			TDictionary<TNodeID,		TConstantInfo>;
//		FConversionTable

		function get_Bindings(NodeId: TNodeID): TBindingInfo;
		function get_Constants(NodeId: TNodeID): TConstantInfo;
		function get_Scopes(ScopeId: TScopeID): TScopeInfo;
		function get_SourceLocations(NodeId: TNodeID): TLocationInfo;
		function get_Symbols(SymbolId: TSymbolID): TSymbolInfo;
		function get_Types(NodeId: TNodeID): TTypeInfo;

	public
		constructor Create(RootNode: TSyntaxNode2);
		destructor Destroy; override;

		property SourceLocations[NodeId: TNodeID]: TLocationInfo read get_SourceLocations;
		property Scopes[ScopeId: TScopeID]: TScopeInfo read get_Scopes;
		property Symbols[SymbolId: TSymbolID]: TSymbolInfo read get_Symbols;
		property Bindings[NodeId: TNodeID]: TBindingInfo read get_Bindings;
		property Types[NodeId: TNodeID]: TTypeInfo read get_Types;
		property Constants[NodeId: TNodeID]: TConstantInfo read get_Constants;

	end;


implementation

{ TSemanticModel }

constructor TSemanticModel.Create(RootNode: TSyntaxNode2);
begin
	inherited Create;

	FRootNode := RootNode;

	FSourceLocations	:= TDictionary<TNodeID,		TLocationInfo>.Create;
	FScopes				:= TDictionary<TScopeID,	TScopeInfo>.Create;
	FSymbols				:= TDictionary<TSymbolID,	TSymbolInfo>.Create;
	FBindings			:= TDictionary<TNodeID,		TBindingInfo>.Create;
	FTypes				:= TDictionary<TNodeID,		TTypeInfo>.Create;
	FConstants			:= TDictionary<TNodeID,		TConstantInfo>.Create;

end;

destructor TSemanticModel.Destroy;
begin

  inherited;
end;

function TSemanticModel.get_Bindings(NodeId: TNodeID): TBindingInfo;
begin
	Result := FBindings.Items[NodeId];
end;

function TSemanticModel.get_Constants(NodeId: TNodeID): TConstantInfo;
begin
	Result := FConstants.Items[NodeId];
end;

function TSemanticModel.get_Scopes(ScopeId: TScopeID): TScopeInfo;
begin
	Result := FScopes.Items[ScopeId];
end;

function TSemanticModel.get_SourceLocations(NodeId: TNodeID): TLocationInfo;
begin
	Result := FSourceLocations.Items[NodeId];
end;

function TSemanticModel.get_Symbols(SymbolId: TSymbolID): TSymbolInfo;
begin
	Result := FSymbols.Items[SymbolId];
end;

function TSemanticModel.get_Types(NodeId: TNodeID): TTypeInfo;
begin
	Result := FTypes.Items[NodeId];
end;

end.
