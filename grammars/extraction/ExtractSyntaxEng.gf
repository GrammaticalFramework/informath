concrete ExtractSyntaxEng of ExtractSyntax =
  StructuralEng **
    ExtractSyntaxFunctor - [pmkV2, prefixCN] with
      (Syntax=SyntaxEng),
      (Symbolic=SymbolicEng),
      (Extend=ExtendEng) **
      open (P=ParadigmsEng) in {

   oper
     pmkV2 : V -> V2 = P.mkV2 ;
     prefixCN : Str -> CN -> CN = \s, cn ->
        cn ** {s = \\n, c => s ++ cn.s ! n ! c} ;

     }
