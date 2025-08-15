concrete ExtractSyntaxPor of ExtractSyntax =
  StructuralPor **
    ExtractSyntaxFunctor - [pmkV2, prefixCN, BareCN] with
      (Syntax=SyntaxPor),
      (Symbolic=SymbolicPor),
      (Extend=ExtendPor) **
    open
      (P=ParadigmsPor),
      (M=MakeStructuralPor)
    in {

   lin
     BareCN cn = mkNP (M.mkDet "") cn ;

   oper
     pmkV2 : V -> V2 = P.mkV2 ;
     prefixCN : Str -> CN -> CN = \s, cn ->
        cn ** {s = \\n => s ++ cn.s ! n} ;

     }
