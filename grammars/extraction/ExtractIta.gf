--# -path=.:morphodict

-- make a symlink to morphodict

concrete ExtractIta of ExtractItaAbs =
  MorphoDictIta - [in_Prep],
  MathWordsIta,
  ExtractSyntaxIta
  ;

