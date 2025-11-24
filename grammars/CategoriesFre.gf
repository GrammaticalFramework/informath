concrete CategoriesFre of Categories =
  IdentifiersLatex **
  CategoriesFunctor - [BaseArgKind, ConsArgKind]
  with
    (Utilities = UtilitiesFre),
    (Syntax = SyntaxFre),
    (Symbolic = SymbolicFre),
    (Markup = MarkupFre) **
  open
    ParadigmsFre
  in {

lin
  BaseArgKind kind = {
    sg = case kind.isPl of {
      True => mkNP aPl_Det (useKind kind) ;
      False => mkNP aSg_Det (useKind kind)
      } ;
    neg = case kind.isPl of {
      True => mkNP no_Quant (useKind kind) ;
      False => mkNP no_Quant (useKind kind)
      } ;
    pl = mkNP thePl_Det (useKind kind)
    } ;
    
  ConsArgKind kind kinds = {
    sg = case kind.isPl of {
      True => mkNP and_Conj (mkNP aPl_Det (useKind kind)) kinds.sg ;
      False => mkNP and_Conj (mkNP aSg_Det (useKind kind)) kinds.sg
      } ;
    neg = case kind.isPl of {
      True => mkNP and_Conj (mkNP no_Quant (useKind kind)) kinds.sg ;
      False => mkNP and_Conj (mkNP no_Quant (useKind kind)) kinds.sg
      } ;
    pl = mkNP and_Conj (mkNP thePl_Det (useKind kind)) kinds.pl 
    } ;


}
