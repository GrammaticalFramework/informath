concrete ExtractSyntaxit of ExtractSyntax =
  Structuralit **
    ExtractSyntaxFunctor - [pmkV2, prefixCN, BareCN] with
      (Syntax=Syntaxit),
      (Symbolic=Symbolicit),
      (Extend=Extendit) **
    open
      (P=Paradigmsit),
      (M=MakeStructuralit)
    in {

   lin
     BareCN cn = mkNP (M.mkDet "") cn ;

   oper
     pmkV2 : V -> V2 = P.mkV2 ;
     prefixCN : Str -> CN -> CN = \s, cn ->
        cn ** {s = \\n => s ++ cn.s ! n} ;

     }
