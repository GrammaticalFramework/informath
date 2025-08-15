--# -path=.:morphodict

-- make a symlink to morphodict

concrete ExtractSwe of ExtractSweAbs =
  MorphoDictSwe - [under_Prep],
  MathWordsSwe,
  ExtractSyntaxSwe
  ;

