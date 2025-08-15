--# -path=.:morphodict

-- make a symlink to morphodict

concrete ExtractFre of ExtractFreAbs =
  MorphoDictFre,
  MathWordsFre,
  ExtractSyntaxFre
  ;

