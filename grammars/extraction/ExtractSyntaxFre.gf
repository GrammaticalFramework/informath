concrete ExtractSyntaxFre of ExtractSyntax =
  StructuralFre **
    ExtractSyntaxFunctor - [pmkV2, prefixCN, BareCN] with
      (Syntax=SyntaxFre),
      (Symbolic=SymbolicFre),
      (Extend=ExtendFre) **
    open
      (P=ParadigmsFre),
      (M=MakeStructuralFre)
    in {

   lin
     BareCN cn = mkNP (M.mkDet "") cn ;

   oper
     pmkV2 : V -> V2 = P.mkV2 ;
     prefixCN : Str -> CN -> CN = \s, cn ->
        cn ** {s = \\n => s ++ cn.s ! n} ;

     }
