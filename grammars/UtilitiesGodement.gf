-- words and helper opers for GodementSyntaxFunctor

interface UtilitiesGodement = open Syntax, Utilities in {

oper
  say_V2 : V2 ;
  call_V2 : V2 ;
  defineG_V2 : V2 ;
  denote_V2 : V2 ;
  endow_V2 : V2 ;
  by_definition_AdV : AdV ;
  resp_Str : Str ;
  or_Str : Str ;
  namely_Str : Str ;
  following_A : A ;
  unique_A : A ;
  necessary_A : A ;
  sufficient_A : A ;
  equivalent_A : A ;
  condition_N : N ;
  property_N : N ;
  form_N : N ;
  numberG_N : N ;
  whereG_Subj : Subj ;
  whenever_Subj : Subj ;
  given_Prep : Prep ;
  so_is_Str : Str ;
  exactly_AdN : AdN ;
  oneG_Card, twoG_Card, threeG_Card : Card ;
  any_Det : Det ;
  called_Str : Str ;
  denoted_by_Str : Str ;
  endowed_with_Str : Str ;
  termCard : Str -> Card ;
  symbS : Str -> S ;

}
