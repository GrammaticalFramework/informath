concrete ExtractSyntaxIta of ExtractSyntax =
  StructuralIta **
    ExtractSyntaxFunctor - [pmkV2, prefixCN, BareCN] with
      (Syntax=SyntaxIta),
      (Symbolic=SymbolicIta),
      (Extend=ExtendIta) **
    open
      (P=ParadigmsIta),
      (M=MakeStructuralIta)
    in {

   lin
     BareCN cn = mkNP (M.mkDet "" P.singular) cn ;

   oper
     pmkV2 : V -> V2 = P.mkV2 ;
     prefixCN : Str -> CN -> CN = \s, cn ->
        cn ** {s = \\n => s ++ cn.s ! n} ;

     }
