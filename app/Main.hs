module Main where
import PyTypedDictTs.PyParse (parseTypes, pType)
import PyTypedDictTs.Conversion  (convertPy)
import PyTypedDictTs.Print (printTs)
import Data.Text (Text)
import qualified Data.Text.IO as T


main :: IO ()
main = do
  s <- T.getContents
  case parseTypes s of
    Left err -> error err
    Right xs -> do
      mapM_ (\x -> do
          T.putStrLn . printTs . convertPy $ x
          T.putStrLn ""
        ) xs



