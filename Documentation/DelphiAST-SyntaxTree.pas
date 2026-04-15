ntUnit anName="Contoso.SpecialCharactersDemo"
	ntInterface
		ntUses
			ntUnit anName="System.Classes"
			ntUnit anName="System.Collections.Generics"
			ntUnit anName="ContosoTools"
		ntConstants
			ntConstant
				ntName
				ntValue
					ntExpression
						ntLiteral anType="numeric"
			ntConstant
				ntName
				ntValue
					ntExpression
						ntLiteral anType="numeric"
			ntConstant
				ntName
				ntValue
					ntExpression
						ntLiteral anType="numeric"
		ntTypeSection
			ntTypeDecl anName="TSpecial"
				ntTypeParams
					ntTypeParam
						ntType anName="T"
						ntConstraints
							ntClassConstraint
							ntConstructorConstraint
					ntTypeParam
						ntType anName="U"
						ntConstraints
							ntClassConstraint
							ntConstructorConstraint
					ntTypeParam
						ntType anName="V"
						ntConstraints
							ntType anName="IComparable"
								ntTypeArgs
									ntType anName="T"
				ntType anType="class"
					ntType anName="TShape"
					ntType anName="ILogger"
					ntField
						ntName
						ntType anName="TBestTime"
					ntPrivate anVisibility="true"
						ntField
							ntName
							ntType anName="T"
						ntField
							ntName
							ntType anName="TGUID"
					ntPublic anVisibility="true"
						ntMethod anKind="constructor" anName="Create"
							ntParameters
								ntParameter
									ntName
									ntType anName="T"
						ntMethod anKind="procedure" anName="DemoMethod"
						ntProperty anName="Value"
							ntType anName="Integer"
							ntRead
								ntIdentifier anName="FValue"
							ntWrite
								ntIdentifier anName="FValue"
	ntImplementation
		ntMethod anKind="constructor" anName="TSpecial.Create"
			ntParameters
				ntParameter
					ntName
					ntType anName="T"
			ntStatements
				ntAssign
					ntLHS
						ntIdentifier anName="FValue"
					ntRHS
						ntExpression
							ntIdentifier anName="Value"
		ntMethod anKind="procedure" anName="TSpecial.DemoMethod"
			ntVariables
				ntVariable
					ntName
					ntType anType="pointer"
						ntType anName="Integer"
				ntVariable
					ntName
					ntType anType="array"
						ntBounds
							ntDimension
								ntExpression
									ntLiteral anType="numeric"
								ntExpression
									ntLiteral anType="numeric"
						ntType anName="Integer"
				ntVariable
					ntName
					ntType anName="Integer"
			ntStatements
				ntAssign
					ntLHS
						ntIndexed
							ntExpression
								ntLiteral anType="numeric"
							ntIdentifier anName="Arr"
					ntRHS
						ntExpression
							ntIdentifier anName="FValue"
				ntAssign
					ntLHS
						ntIndexed
							ntExpression
								ntLiteral anType="numeric"
							ntIdentifier anName="Arr"
					ntRHS
						ntExpression
							ntAdd
								ntIdentifier anName="FValue"
								ntLiteral anType="numeric"
				ntAssign
					ntLHS
						ntIdentifier anName="Sum"
					ntRHS
						ntExpression
							ntAdd
								ntIndexed
									ntExpression
										ntLiteral anType="numeric"
									ntIdentifier anName="Arr"
								ntIndexed
									ntExpression
										ntLiteral anType="numeric"
									ntIdentifier anName="Arr"
				ntAssign
					ntLHS
						ntIdentifier anName="Ptr"
					ntRHS
						ntExpression
							ntAddr
								ntIdentifier anName="Sum"
				ntCall
					ntCall
						ntIdentifier anName="Writeln"
						ntExpressions
							ntExpression
								ntLiteral anType="string"
							ntExpression
								ntDeref
									ntIdentifier anName="Ptr"
				ntIf
					ntExpression
						ntGreaterEqual
							ntIdentifier anName="Sum"
							ntLiteral anType="numeric"
					ntThen
						ntCall
							ntCall
								ntIdentifier anName="Writeln"
								ntExpressions
									ntExpression
										ntLiteral anType="string"
					ntElse
						ntIf
							ntExpression
								ntLowerEqual
									ntIdentifier anName="Sum"
									ntLiteral anType="numeric"
							ntThen
								ntCall
									ntCall
										ntIdentifier anName="Writeln"
										ntExpressions
											ntExpression
												ntLiteral anType="string"
							ntElse
								ntCall
									ntCall
										ntIdentifier anName="Writeln"
										ntExpressions
											ntExpression
												ntLiteral anType="string"
				ntIf
					ntExpression
						ntNotEqual
							ntIdentifier anName="Sum"
							ntLiteral anType="numeric"
					ntThen
						ntCall
							ntCall
								ntIdentifier anName="Writeln"
								ntExpressions
									ntExpression
										ntLiteral anType="string"
				ntFor
					ntIdentifier anName="Sum"
					ntFrom
						ntExpression
							ntLiteral anType="numeric"
					ntTo
						ntExpression
							ntCall
								ntIdentifier anName="High"
								ntExpressions
									ntExpression
										ntIdentifier anName="Arr"
					ntCall
						ntCall
							ntIdentifier anName="Writeln"
							ntExpressions
								ntExpression
									ntLiteral anType="string"
								ntExpression
									ntIdentifier anName="Sum"
								ntExpression
									ntLiteral anType="string"
								ntExpression
									ntIndexed
										ntExpression
											ntIdentifier anName="Sum"
										ntIdentifier anName="Arr"
				ntCase
					ntExpression
						ntIdentifier anName="FValue"
					ntCaseSelector
						ntCaseLabels
							ntCaseLabel
								ntExpression
									ntLiteral anType="numeric"
								ntExpression
									ntLiteral anType="numeric"
						ntCall
							ntCall
								ntIdentifier anName="Writeln"
								ntExpressions
									ntExpression
										ntLiteral anType="string"
					ntCaseSelector
						ntCaseLabels
							ntCaseLabel
								ntExpression
									ntLiteral anType="numeric"
								ntExpression
									ntLiteral anType="numeric"
						ntCall
							ntCall
								ntIdentifier anName="Writeln"
								ntExpressions
									ntExpression
										ntLiteral anType="string"
					ntCaseElse
						ntStatements
							ntCall
								ntCall
									ntIdentifier anName="Writeln"
									ntExpressions
										ntExpression
											ntLiteral anType="string"
				ntCall
					ntCall
						ntIdentifier anName="Writeln"
						ntExpressions
							ntExpression
								ntLiteral anType="string"