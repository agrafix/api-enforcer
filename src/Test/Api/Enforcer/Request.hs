{-# LANGUAGE StrictData #-}
module Test.Api.Enforcer.Request
    ( StringValue(..)
    , NumValue(..)
    , ListValue(..)
    , UnstructuredHashValue(..)
    , HashValue(..)
    , ExactValue(..)
    , RequestArgumentValue(..)
    , RequestArgument(..)
    , Request(..)
    )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

data StringValue
    = StringValue
    { sv_alphabet :: V.Vector Char
    , sv_minLength :: Maybe Int
    , sv_maxLength :: Maybe Int
    } deriving (Show, Eq)

data NumValue i
    = NumValue
    { iv_lowerBound :: Maybe i
    , iv_upperBound :: Maybe i
    } deriving (Show, Eq)

data ListValue
    = ListValue
    { lv_minLength :: Maybe Int
    , lv_maxLength :: Maybe Int
    , lv_values :: V.Vector RequestArgumentValue
    } deriving (Show, Eq)

data UnstructuredHashValue
    = UnstructuredHashValue
    { uhv_key :: StringValue
    , uhv_values :: V.Vector RequestArgumentValue
    , uhv_minLength :: Maybe Int
    , uhv_maxLength :: Maybe Int
    } deriving (Show, Eq)

data HashValue
    = HvStructured (HM.HashMap T.Text RequestArgumentValue)
    | HvUnstructured UnstructuredHashValue
    deriving (Show, Eq)

data ExactValue
    = EvString T.Text
    | EvBool Bool
    | EvInt Int
    | EvFloat Double
    | EvList (V.Vector ExactValue)
    | EvHash (HM.HashMap T.Text ExactValue)
    deriving (Show, Eq)

data RequestArgumentValue
    = RavString StringValue
    | RavEnum (V.Vector RequestArgumentValue)
    | RavInt (NumValue Int)
    | RavFloat (NumValue Float)
    | RavBool
    | RavList ListValue
    | RavHash HashValue
    | RavExactValue ExactValue
    deriving (Show, Eq)

data RequestArgument
    = RequestArgument
    { ra_optional :: Bool
    , ra_value :: RequestArgumentValue
    } deriving (Show, Eq)

data Request
    = Request
    { r_arguments :: HM.HashMap T.Text RequestArgument
    } deriving (Show, Eq)
