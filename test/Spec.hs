import Test.Hspec
import Text.Megaparsec as MP

import Blueprint.AbstractSyntaxTree as AST
import Blueprint.Parser.Boolean as PBool
import Blueprint.Parser.Number as PNumber
import Blueprint.Parser.Symbol as PSymbol

main :: IO ()
main = hspec $ do
  describe "ScmBool.read" $ do
    it "#t     → True"  $ bool "#t"     `shouldBe` (Right (AST.ScmBool True))
    it "#true  → True"  $ bool "#true"  `shouldBe` (Right (AST.ScmBool True))
    it "#f     → False" $ bool "#f"     `shouldBe` (Right (AST.ScmBool False))
    it "#false → False" $ bool "#false" `shouldBe` (Right (AST.ScmBool False))
  describe "ScmSymbol.read" $ do
    it "foo → foo" $ sym "foo" `shouldBe` (Right (AST.ScmSymbol "foo"))
    it "+   → +"   $ sym "+"   `shouldBe` (Right (AST.ScmSymbol "+"))
    it "-   → -"   $ sym "-"   `shouldBe` (Right (AST.ScmSymbol "-"))
    it "/   → /"   $ sym "/"   `shouldBe` (Right (AST.ScmSymbol "/"))
  describe "ScmNumber.readInteger" $ do
    it "123  → 123  :: Integer" $ int "123"  `shouldBe` consnum 123
    it "-123 → -123 :: Integer" $ int "-123" `shouldBe` consnum (-123)

  where consnum n = Right $ AST.ScmInteger n
        parse f = MP.parse f ""
        bool = parse PBool.read
        sym  = parse PSymbol.read
        int  = parse PNumber.readInteger
