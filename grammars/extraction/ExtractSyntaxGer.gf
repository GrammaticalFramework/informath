concrete ExtractSyntaxGer of ExtractSyntax =
  StructuralGer **
    ExtractSyntaxFunctor - [pmkV2, prefixCN] with
      (Syntax=SyntaxGer),
      (Symbolic=SymbolicGer),
      (Extend=ExtendGer) **
      open (P=ParadigmsGer) in {

   oper
     pmkV2 : V -> V2 = P.mkV2 ;

     prefixCN : Str -> CN -> CN = \s, cn ->
        cn ** {s = \\a, n, c => s ++ cn.s ! a ! n ! c} ;

     }
