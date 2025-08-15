--# -path=.:morphodict

-- make a symlink to morphodict

concrete ExtractPor of ExtractPorAbs =
  MorphoDictPor,
  MathWordsPor,
  ExtractSyntaxPor
  ;

