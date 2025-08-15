--# -path=.:morphodict

-- make a symlink to morphodict

concrete ExtractFin of ExtractFinAbs =
  MorphoDictFin,
  MathWordsFin,
  ExtractSyntaxFin
  ;

