concrete ExtractSyntaxSwe of ExtractSyntax =
  StructuralSwe **
    ExtractSyntaxFunctor - [pmkV2, prefixCN] with
      (Syntax=SyntaxSwe),
      (Symbolic=SymbolicSwe),
      (Extend=ExtendSwe) **
      open (P=ParadigmsSwe) in {

   oper
     pmkV2 : V -> V2 = P.mkV2 ;
     prefixCN : Str -> CN -> CN = \s, cn ->
        cn ** {s = \\n, c, d => s ++ cn.s ! n ! c ! d} ;

     }
