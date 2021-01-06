module Blueprint.AbstractSyntaxTree (SchemeExpression(..)) where

type Numerator = Integer
type Denominator = Integer

data ScmSign = Positive | Negative
  deriving (Show, Eq)

data SchemeExpression = ScmBool Bool
                      | ScmSymbol String
                      | ScmInteger Integer
--                      | ScmRational (Numerator,Denominator)
--                      | ScmReal Integer (Numerator,Denominator) Integer
--                      | ScmNaN ScmSign
--                      | ScmInf ScmSign
                      | ScmList [SchemeExpression]
                      | ScmUndefined
  deriving (Show, Eq)
