module PyTypedDictTs.Print where
import PyTypedDictTs.Types
import PyTypedDictTs.Conversion
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.List.NonEmpty (toList)


printTs :: TsType -> Text
printTs TsString = "string"
printTs TsNumber = "number"
printTs TsNull = "null"
printTs TsBool = "bool"
printTs (TsArray typ) = "Array<" <> printTs typ <> ">"
printTs (TsTuple typs) = "[" <> (T.intercalate ", " . toList . fmap printTs $ typs) <> "]"
printTs (TsRecord kTyp vTyp) = "Record<" <> printTs kTyp <> ", " <> printTs vTyp <> ">"
printTs (TsUnion typs) = T.intercalate " | " . toList . fmap printTs $ typs
printTs (TsObject fields) = "{\n  " <> (T.intercalate "\n  " . fmap printTsField $ fields) <> "\n}"
-- export by default?
printTs (TsTypeAlias name typ) = "export type " <> name <> " = " <> printTs typ <> ";"
printTs (TsInterface name super typ) = undefined -- "interface " <> name <> " {\n" <> printTs typ <> "\n}"
printTs (TsLiteralString litString) = pack.show.unpack $ litString
printTs (TsCustomType name) = name

printTsField :: (Text, (TsType, TsPropModifier)) -> Text
printTsField (name, (typ, TsPropOptional)) = name <> "?: " <> printTs typ <> ";"
printTsField (name, (typ, _)) = name <> ": " <> printTs typ <> ";"
