{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Py2TsSpec where
import           Control.Exception           (evaluate)
import           Control.Monad               ((<=<), (=<<), forM, join)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Py2Ts.PyParse
import           Py2Ts.Types
import qualified Text.Megaparsec
import           Test.Hspec
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE



spec :: Spec
spec = do

  describe "Individual parser tests" $ do

    it "parse str" $ do
        (parse pStr "str ") `shouldBe` (Right PyStr)

    it "parse None" $ do
        (parse pNone "None ") `shouldBe` (Right PyNone)

    it "parse Union via bars" $ do
        (parse pUnionBars "str | None") `shouldBe` (Right $ PyUnion (PyStr :| [PyNone]))

    it "parse Union via bars (int)" $ do
        (parse pUnionBars "int | None") `shouldBe` (Right $ PyUnion (PyInt :| [PyNone]))


    it "parse NotRequired" $ do
        (parse pNotRequired "NotRequired[str | None]")
          `shouldBe` (Right $ PyNotRequired (PyUnion (PyStr :| [PyNone])))

    it "parse pNonEmptyListOf" $ do
        parse (pNonEmptyListOf pStr) "str, str"
          `shouldBe` (Right $ (PyStr :| [PyStr]))

    it "parse pParam" $ do
        parse (pParam "tuple" (pNonEmptyListOf pStr) id) "tuple[str, str]"
          `shouldBe` (Right $ (PyStr :| [PyStr]))

    it "parse pParam across newline in bracket expression" $ do
        parse (pParam "tuple" (pNonEmptyListOf pStr) id) "tuple[\n  str,\n   str\n]"
          `shouldBe` (Right $ (PyStr :| [PyStr]))


    it "parse tuple" $ do
        (parse pTuple "tuple[str, int]")
          `shouldBe` (Right $ PyTuple (PyStr :| [PyInt]))

    it "parse Dict" $ do
        (parse pDict "dict[str, int]")
          `shouldBe` (Right $ PyDict PyStr PyInt)

    it "parse Explicit Union" $ do
        (parse pUnionExplicit "Union[str, int]")
          `shouldBe` (Right $ PyUnion (PyStr :| [PyInt]))

    it "parse TypedDict field" $ do
        (parse pTypedDictField "title: str")
          `shouldBe` (Right $ ("title", PyStr))

    it "parse TypedDict field with union" $ do
        (parse pTypedDictField "year: int | None")
          `shouldBe` (Right $ ("year", PyUnion (PyInt :| [PyNone])))

    it "parse TypedDict" $ do
        let s = "class Title(TypedDict):\n  title: str\n  year: int\n"
            expected =  Right $
                  PyTypedDict "Title" "TypedDict" [("title", PyStr), ("year", PyInt)]
        (parse pTypedDict s) `shouldBe` expected

    it "parse TypedDict with union field" $ do
        let s = "class Title(TypedDict):\n  title: str\n  year: int | None\n"
            expected =  Right $
                  PyTypedDict "Title" "TypedDict" [("title", PyStr), ("year", PyUnion (PyInt :| [PyNone]))]
        (parse pTypedDict s) `shouldBe` expected


    it "parse literal strings" $ do
        let s = "Literal[\"foo\", \"bar\"]"
            expected =  Right $ PyLiteral ("foo" :| ["bar"])
        (parse pLiteralStrings s) `shouldBe` expected

    it "parse literal strings across lijnes" $ do
        let s = "Literal[\n  \"foo\",\n   \"bar\"\n]\n"
            expected =  Right $ PyLiteral ("foo" :| ["bar"])
        (parse pLiteralStrings s) `shouldBe` expected

    it "parse type alias (allow trailing comma)" $ do
        let s = "MyCustomType = Literal[\n  \"foo\",\n  \"bar\",\n]\n"
            expected =  Right $ PyTypeAlias "MyCustomType" (PyLiteral (NE.fromList ["foo", "bar"]))
        (parse pTypeAlias s) `shouldBe` expected

    it "parse custom type" $ do
        let s = "MyCustomType"
            expected =  Right $ PyCustomType "MyCustomType"
        (parse pCustomType s) `shouldBe` expected

    it "parse custom type quoted" $ do
        let s = "'MyCustomType'"
            expected =  Right $ PyCustomType "MyCustomType"
        (parse pCustomType s) `shouldBe` expected

    it "parse custom type double-quoted" $ do
        let s = "\"MyCustomType\""
            expected =  Right $ PyCustomType "MyCustomType"
        (parse pCustomType s) `shouldBe` expected

  describe "General parser tests" $ do

    it "parse NotRequired general" $ do
        (parse pType "NotRequired[str | None]")
          `shouldBe` (Right $ PyNotRequired (PyUnion (PyStr :| [PyNone])))

    it "parse TypedDict" $ do
        let s = "class Title(TypedDict):\n  title: str\n  year: int\n"
            expected =  Right $
                  PyTypedDict "Title" "TypedDict" [("title", PyStr), ("year", PyInt)]
        (parse pType s) `shouldBe` expected

    it "skips line comment" $ do
        let s = "class Title(TypedDict):\n  title: str # here is a comment \n  year: int\n  runtime: NotRequired[int | None] # comment\n"
            expected =

                  Right (PyTypedDict "Title" "TypedDict" [("title",PyStr),("year",PyInt),("runtime",PyNotRequired (PyUnion (PyInt :| [PyNone])))])

        (parse pType s) `shouldBe` expected


