module Main where
import Py2Ts.PyParse (parseTypes, pType)
import Py2Ts.Conversion  (convertPy)
import Py2Ts.Print (printTs)
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



