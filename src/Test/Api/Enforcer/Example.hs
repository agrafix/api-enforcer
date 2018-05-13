module Test.Api.Enforcer.Example where

import Test.Api.Enforcer.RequestGen
import Test.Api.Enforcer.Types

import Data.Aeson.Encode.Pretty
import Test.QuickCheck
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

dummyReq :: Request
dummyReq =
    Request $
    HM.fromList
    [ ( "id"
      , RequestArgument
        { ra_optional = False
        , ra_value =
                RavExactValue $ EvString "usr_asdljaslkdj"
        }
      )
    , ( "name"
      , RequestArgument
        { ra_optional = False
        , ra_value =
                RavString
                StringValue
                { sv_alphabet = ['A'..'Z'] ++ ['a'..'z']
                , sv_minLength = Just 4
                , sv_maxLength = Just 20
                }
        }
      )
    , ( "age"
      , RequestArgument
        { ra_optional = False
        , ra_value =
                RavInt
                NumValue
                { iv_lowerBound = Just 0
                , iv_upperBound = Just 100
                }
        }
      )
    , ( "sex"
      , RequestArgument
        { ra_optional = True
        , ra_value =
                RavEnum $
                V.fromList $
                map RavExactValue [EvString "male", EvString "female", EvString "other"]
        }
      )
    ]

runExample :: IO ()
runExample =
    do r <-
           T.decodeUtf8 . BSL.toStrict . encodePretty <$> generate (requestGen dummyReq)
       T.putStrLn r
