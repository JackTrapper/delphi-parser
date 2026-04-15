ntCompilationUnit
	ntUnitDeclaration anName="Contoso.SpecialCharactersDemo"
		ntQualifiedIdentifier anName="Contoso.SpecialCharactersDemo"
		ntPortabilityDirective anLibrary="avTrue"
		ntPortabilityDirective anPlatform="avTrue"
		ntPortabilityDirective anDeprecated="use Fabrikam.SpecialCharactersDemo"
		ntPortabilityDirective anExperimental="avTrue"
		ntInterfaceSection
			ntUses
				ntUsedUnit anName="System.Classes"
					ntQualifiedIdentifier anName="System.Classes"
				ntUsedUnit anName="System.Collections.Generics"
					ntQualifiedIdentifier anName="System.Collections.Generics"
				ntUsedUnit anName="ContosoTools"
					ntQualifiedIdentifier anName="ContosoTools"
			ntConstants
				ntConstant anName="A" anValueText="1"
					ntIdentifier anName="A"
					ntExpression anValueText="1"
				ntConstant anName="B" anValueText="2"
					ntIdentifier anName="B"
					ntExpression anValueText="2"
				ntConstant anName="C" anValueText="3"
					ntIdentifier anName="C"
					ntExpression anValueText="3"
			ntTypeSection
				ntTypeDecl anName="TSpecial"
					ntTypeParams
						ntTypeParam
							ntType anName="T"
							ntType anName="U"
							ntConstraints
								ntClassConstraint
								ntConstructorConstraint
						ntTypeParam
							ntType anName="V"
							ntConstraints
								ntType
									ntQualifiedIdentifier anName="IComparable"
									ntTypeParams
										ntTypeParam
		`									ntType anName="T"
					ntType anType="avClass"
						ntQualifiedIdentifier anName="TShape"
						ntQualifiedIdentifier anName="ILogger"
						ntField
							ntName anName="FTime"
							ntType anName="TBestTime"
						ntVisibilitySection anVisibility="private"
							ntField
								ntName anName="FValue"
								ntType anName="T"
							ntField
								ntName anName="FId"
								ntType anName="TGUID"
						ntVisibilitySection anVisibility="public"
							ntMethod anKind="avConstructor"
								ntQualifiedIdentifier anName="Create"
								ntParameterList
									ntParameter
										ntIdentifierList
											ntIdentifier anName="Value"
										ntParameterType
											ntQualifiedIdentifier anName="T"
							ntMethod anKind="avProcedure"
								ntQualifiedIdentifier anName="DemoMethod"
							ntProperty
								ntIdentifier anName="Value"
								ntType anName="Integer"
								ntPropertyDirective anKind="read"
									ntParticle
										ntIdentifier anName="FValue"
								ntPropertyDirective anKind="write"
									ntParticle
										ntIdentifier anName="FValue"
		ntImplementation
			ntMethod anKind="avConstructor"
				ntQualifiedIdentifier anName="TSpecial.Create"
				ntParameterList
					ntParameter
						ntIdentifierList
							ntIdentifier anName="Value"
						ntParameterType
							ntQualifiedIdentifier anName="T"
				ntStatementList
					ntStatement
						ntAssign
							ntParticle
								ntIdentifier anName="FValue"
							ntParticle
								ntIdentifier anName="Value"
			ntMethod anKind="avProcedure"
				ntQualifiedIdentifier anName="TSpecial.DemoMethod"
				ntVarSection
					ntVariable
						ntIdentifierList
							ntIdentifier anName="Ptr"
						ntType anType="avPointer"
							ntType anName="Integer"
					ntVariable
						ntIdentifierList
							ntIdentifier anName="Arr"
						ntType anType="array"
							ntExpressionOrRange
								ntParticle
								ntParticle
							ntType anName="Integer"
					ntVariable
						ntIdentifierList
							ntIdentifier anName="Sum"
						ntType anName="Integer"
				ntStatementList
					ntStatement
						ntAssign
							ntParticle
								ntIdentifier anName="Arr"
								ntParticle
							ntParticle
								ntIdentifier anName="FValue"
					ntStatement
						ntAssign
							ntParticle
								ntIdentifier anName="Arr"
								ntParticle
							ntBinaryExpression
								ntParticle
									ntIdentifier anName="FValue"
								ntAdd
								ntParticle
					ntStatement
						ntAssign
							ntParticle
								ntIdentifier anName="Sum"
							ntBinaryExpression
								ntParticle
									ntIdentifier anName="Arr"
									ntParticle
								ntAdd
								ntParticle
									ntIdentifier anName="Arr"
									ntParticle
					ntStatement
						ntAssign
							ntParticle
								ntIdentifier anName="Ptr"
							ntUnaryOperator
								ntParticle
									ntIdentifier anName="Sum"
					ntStatement
						ntParticle
							ntIdentifier anName="Writeln"
							ntParticle
							ntParticle
								ntIdentifier anName="Ptr"
					ntStatement
						ntIf
							ntExpression
								ntParticle
									ntIdentifier anName="Sum"
								ntGreaterEqual
								ntParticle
							ntThen
								ntStatement
									ntParticle
										ntIdentifier anName="Writeln"
										ntParticle
							ntElse
								ntStatement
									ntIf
										ntExpression
											ntParticle
												ntIdentifier anName="Sum"
											ntLowerEqual
											ntParticle
										ntThen
											ntStatement
												ntParticle
													ntIdentifier anName="Writeln"
													ntParticle
										ntElse
											ntStatement
												ntParticle
													ntIdentifier anName="Writeln"
													ntParticle
					ntStatement
						ntIf
							ntExpression
								ntParticle
									ntIdentifier anName="Sum"
								ntNotEqual
								ntParticle
							ntThen
								ntStatement
									ntParticle
										ntIdentifier anName="Writeln"
										ntParticle
					ntStatement
						ntFor
							ntQualifiedIdentifier anName="Sum"
							ntFrom
								ntParticle
							ntTo
								ntParticle
									ntIdentifier anName="High"
									ntParticle
										ntIdentifier anName="Arr"
							ntStatement
								ntParticle
									ntIdentifier anName="Writeln"
									ntParticle
									ntParticle
										ntIdentifier anName="Sum"
									ntParticle
									ntParticle
										ntIdentifier anName="Arr"
										ntParticle
											ntIdentifier anName="Sum"
					ntStatement
						ntCaseStatement
							ntParticle
								ntIdentifier anName="FValue"
							ntCaseSelector
								ntCaseLabels
									ntCaseLabel
										ntExpression
											ntParticle
										ntExpression
											ntParticle
								ntStatement
									ntParticle
										ntIdentifier anName="Writeln"
										ntParticle
							ntCaseSelector
								ntCaseLabels
									ntCaseLabel
										ntExpression
											ntParticle
										ntExpression
											ntParticle
								ntStatement
									ntParticle
										ntIdentifier anName="Writeln"
										ntParticle
							ntStatementList
								ntStatement
									ntParticle
										ntIdentifier anName="Writeln"
										ntParticle
					ntStatement
						ntParticle
							ntIdentifier anName="Writeln"
							ntParticle