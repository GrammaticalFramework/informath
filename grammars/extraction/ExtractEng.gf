--# -path=.:morphodict

-- make a symlink to morphodict

concrete ExtractEng of ExtractEngAbs =
  MorphoDictEng,
  MathWordsEng,
  ExtractSyntaxEng
  ;

