incomplete concrete ExtractSyntaxFunctor of ExtractSyntax = Structural

** open
  Syntax,
  Symbolic,
  Extend

in {

lincat
  MT = Utt ;

lin
  NPMT np = mkUtt np ;
  APMT ap = mkUtt ap ;
  AdvMT adv = mkUtt adv ;

  UseN n = mkCN n ;
  AdjCN ap cn = mkCN ap cn ;
  CompoundN x y = Extend.CompoundN x y ; 
  IntCompoundCN i x = prefixCN (i.s ++ hyphen) x ;
  NameCompoundCN n x = prefixCN ((mkUtt (mkNP n)).s ++ hyphen) x ;
  NounIntCN cn i = mkCN cn (symb i) ;
  NounPrepCN cn adv = mkCN cn adv ;
  NounGenCN cn np = prefixCN (mkUtt (mkNP (Extend.GenNP np))).s cn ;

  DefCN cn = mkNP the_Det cn ;
  DefPluralCN cn = mkNP thePl_Det cn ;
  IndefCN cn = mkNP a_Det cn ;
  IndefPluralCN cn = mkNP aPl_Det cn ;
  BareCN cn = mkNP cn ;

  PositA a = mkAP a ;
  AdAP ad ap = mkAP ad ap ;
  AAdAP a ap = mkAP (lin AdA (mkAdv a)) ap ;
  PastPartAP v = Extend.PastPartAP (mkVPSlash (pmkV2 v)) ;

  PrepNP prep np = mkAdv prep np ;

oper
  -- not used for parsing, but as replacements of Int-functions when used as lin terms,
  -- as a work-around to GF type checking
  nounStrCN : CN -> Str -> CN = \cn, s -> mkCN cn (symb s) ;
  strCompoundCN : Str -> CN -> CN = \s, cn -> prefixCN (s ++ hyphen) cn ;

  -- a back-up oper for unanalysed terms
  rawMT : Str -> MT = \s ->
    lin MT (lin Utt {s = s}) ; ---

  hyphen : Str = Predef.BIND ++ "-" ++ Predef.BIND ;

  -- you may want to change this in the functor instantiation
  prefixCN : Str -> CN -> CN = \s, cn ->
    cn ** {s = \\n => s ++ cn.s ! n} ;

  -- this you have to define
  pmkV2 : V -> V2 = variants {} ;

}