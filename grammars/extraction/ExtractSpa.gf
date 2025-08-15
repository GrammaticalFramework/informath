--# -path=.:morphodict

-- make a symlink to morphodict

concrete ExtractSpa of ExtractSpaAbs =
  MorphoDictSpa,
  MathWordsSpa,
  ExtractSyntaxSpa
  ;

