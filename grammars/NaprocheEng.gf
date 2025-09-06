concrete NaprocheEng of Naproche = MathCoreEng **

  open
    SyntaxEng,
    ParadigmsEng,
    SymbolicEng,
    (E = ExtendEng)
in {

lin
  SupposePropHypo prop =
    mkUtt (mkImp (E.ComplBareVS suppose_VS (topProp prop))) ;
  IffIffProp a b = simpleProp (lin S {s = a.s.s ++ "iff" ++ b.s.s}) ;
  WeHaveFormulaProp formula =
    simpleProp (mkS (mkCl we_NP have_V2 (latexNP (mkSymb formula.s)))) ;
  NoCommaAllProp argkinds prop =
    simpleProp (lin S {s = (SyntaxEng.mkAdv for_Prep (mkNP all_Predet argkinds.pl)).s ++ (partProp prop).s}) ;

  BareIdentsArgKind idents =
    {cn = mkCN emptyN idents.np ; adv = emptyAdv ; isPl = idents.isPl} ;


oper
  suppose_VS = mkVS (mkV "suppose") ;

  emptyN = mkN "" "" ;
  emptyAdv = ParadigmsEng.mkAdv "" ;
  
}