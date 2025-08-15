concrete ExtractSyntaxFin of ExtractSyntax =
  StructuralFin **
    ExtractSyntaxFunctor - [pmkV2, prefixCN] with
      (Syntax=SyntaxFin),
      (Symbolic=SymbolicFin),
      (Extend=ExtendFin) **
      open (P=ParadigmsFin) in {

   oper
     pmkV2 : V -> V2 = P.mkV2 ;
     prefixCN : Str -> CN -> CN = \s, cn ->
        cn ** {s = \\n => s ++ cn.s ! n} ;

     }
