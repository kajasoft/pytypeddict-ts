module Py2Ts.Types where
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)

-- python typehint types


data PyType = PyStr
            | PyInt
            | PyFloat
            | PyBool
            | PyNone
            | PyList PyType
            | PyTuple (NonEmpty PyType)
            | PyDict PyType PyType
            | PyNotRequired PyType
            | PyUnion (NonEmpty PyType)
            | PyLiteral (NonEmpty Text)
            | PyTypedDict Text Text [(Text, PyType)] -- name, superclass, fields
            -- https://mypy.readthedocs.io/en/stable/literal_types.html
            -- just parse Literal[string literals] for now
            -- distinguish reference to a type vs a type alias definition
            | PyTypeAlias Text PyType
            | PyCustomType Text -- type alias?
          deriving (Eq, Show)


data TsType = TsString
            | TsNumber
            | TsNull
            | TsBool
            | TsArray TsType
            | TsTuple (NonEmpty TsType)
            | TsRecord TsType TsType
            | TsUnion (NonEmpty TsType)
            | TsObject [(Text, (TsType, TsPropModifier))]
            | TsTypeAlias Text TsType -- name, type, intersection? TODO
            | TsInterface Text Text TsType -- name, super, type (most like object)
            -- type alias vs interface
            -- literals? just strings for now
            | TsLiteralString Text
            | TsCustomType Text -- type alias?
          deriving (Eq, Show)

-- TODO these may not be disjunctive (mutually exclusive)
-- maybe enclose in list that may be empty or multiple
data TsPropModifier = TsPropOptional
                    | TsPropReadyOnly
                    | TsPropNoModify
          deriving (Eq, Show)

-- https://www.typescriptlang.org/docs/handbook/2/everyday-types.html

-- https://www.typescriptlang.org/docs/handbook/2/objects.html#tuple-types

