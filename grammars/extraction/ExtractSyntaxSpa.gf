concrete ExtractSyntaxSpa of ExtractSyntax =
  StructuralSpa **
    ExtractSyntaxFunctor - [pmkV2, prefixCN, BareCN] with
      (Syntax=SyntaxSpa),
      (Symbolic=SymbolicSpa),
      (Extend=ExtendSpa) **
    open
      (P=ParadigmsSpa),
      (M=MakeStructuralSpa)
    in {

   lin
     BareCN cn = mkNP (M.mkDet "" P.singular) cn ;

   oper
     pmkV2 : V -> V2 = P.mkV2 ;
     prefixCN : Str -> CN -> CN = \s, cn ->
        cn ** {s = \\n => s ++ cn.s ! n} ;

     }
