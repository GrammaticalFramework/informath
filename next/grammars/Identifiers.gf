abstract Identifiers = {

cat
  Term ;
  Ident ;

fun
  IdentTerm : Ident -> Term ;
  NumberTerm : Int -> Term ; --- was float
  StrIdent : String -> Ident ;

}