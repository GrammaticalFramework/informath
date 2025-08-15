--# -path=.:morphodict

-- make a symlink to morphodict

concrete ExtractGer of ExtractGerAbs =
  MorphoDictGer,
  MathWordsGer,
  ExtractSyntaxGer
  ;

