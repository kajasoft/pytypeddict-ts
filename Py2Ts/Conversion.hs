module Py2Ts.Conversion where
import Py2Ts.Types
import Control.Arrow (second)


convertPy :: PyType -> TsType
convertPy PyStr = TsString
convertPy PyInt = TsNumber
convertPy PyFloat = TsNumber
convertPy PyBool = TsBool
convertPy PyNone = TsNull
convertPy (PyList pyType) = TsArray (convertPy pyType)
convertPy (PyTuple pyTypes) = TsTuple (fmap convertPy pyTypes)
convertPy (PyDict pyKeyType pyValType) = TsRecord (convertPy pyKeyType) (convertPy pyValType)
convertPy x@(PyNotRequired _) = error $ "Unexpected at top-level: " <> (show x)
convertPy (PyUnion pyTypes) = TsUnion (fmap convertPy pyTypes)
convertPy (PyLiteral pyTypes) = TsUnion (fmap TsLiteralString pyTypes)
convertPy (PyTypedDict name _ pyFields) = -- TODO do something with super
        TsTypeAlias name (TsObject $ fmap (second convertPyField) pyFields)
convertPy (PyTypeAlias name typ) = TsTypeAlias name (convertPy typ)
convertPy (PyCustomType name) = TsCustomType name

convertPyField :: PyType -> (TsType, TsPropModifier)
convertPyField (PyNotRequired pyType) = (convertPy pyType, TsPropOptional)
convertPyField pyType = (convertPy pyType, TsPropNoModify)
