module Test.Api.Enforcer.RequestGen
    ( requestGen )
where

import Test.Api.Enforcer.Request
import qualified Data.HashMap.Strict as HM

import Data.Aeson
import Data.Maybe
import Data.Scientific
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Vector as V

doExactValue :: ExactValue -> Value
doExactValue ev =
    case ev of
      EvString x -> String x
      EvBool x -> Bool x
      EvInt i -> Number (fromIntegral i)
      EvFloat f -> Number (fromFloatDigits f)
      EvList evs -> Array (V.map doExactValue evs)
      EvHash x -> Object (HM.map doExactValue x)

boundedList :: (Maybe Int, Maybe Int) -> Gen a -> Gen [a]
boundedList x elemGen =
    case x of
      (Nothing, Nothing) -> listOf elemGen
      (Just ml, Nothing) ->
          sized $ \n ->
          do k <- choose (ml, ml + n)
             vectorOf k elemGen
      (Nothing, Just ub) ->
          sized $ \n ->
          do k <- choose (max 0 (ub - n), ub)
             vectorOf k elemGen
      (Just ml, Just ub) ->
          do k <- choose (ml, ub) -- size param relevant here?
             vectorOf k elemGen

doStringValue :: StringValue -> Gen T.Text
doStringValue sv =
    do let strGen = elements $ V.toList $ sv_alphabet sv
       T.pack <$> boundedList (sv_minLength sv, sv_maxLength sv) strGen

doNumValue :: (Arbitrary i, Num i, Ord i) => NumValue i -> Gen i
doNumValue nv =
    case (iv_lowerBound nv, iv_upperBound nv) of
      (Nothing, Nothing) -> arbitrary
      (Just x, Nothing) ->
          do (NonNegative q) <- arbitrary
             pure (x + q)
      (Nothing, Just x) ->
          do (NonNegative q) <- arbitrary
             pure (x - q)
      (Just x, Just y) ->
          suchThat arbitrary $ \i -> i >= x && i <= y

doListValue :: ListValue -> Gen (V.Vector Value)
doListValue lv =
    do let elemGen = oneof $ V.toList $ V.map doArgVal (lv_values lv)
       V.fromList <$> boundedList (lv_minLength lv, lv_maxLength lv) elemGen

doUnstructuredHashValue :: UnstructuredHashValue -> Gen [(T.Text, Value)]
doUnstructuredHashValue uhv =
    boundedList (uhv_minLength uhv, uhv_maxLength uhv) elementGen
    where
      elementGen =
          do key <- doStringValue (uhv_key uhv)
             value <- oneof $ V.toList $ V.map doArgVal (uhv_values uhv)
             pure (key, value)

doHashValue :: HashValue -> Gen [(T.Text, Value)]
doHashValue hv =
    case hv of
      HvStructured x ->
          mapM (\(k, v) -> (,) <$> pure k <*> doArgVal v) (HM.toList x)
      HvUnstructured uv ->
          doUnstructuredHashValue uv

doArgVal :: RequestArgumentValue -> Gen Value
doArgVal val =
    case val of
      RavExactValue ev -> pure (doExactValue ev)
      RavBool -> Bool <$> arbitrary
      RavString str -> String <$> doStringValue str
      RavInt nv -> Number . fromIntegral <$> doNumValue nv
      RavFloat nv -> Number . fromFloatDigits <$> doNumValue nv
      RavList lst -> Array <$> doListValue lst
      RavHash hv -> Object . HM.fromList <$> doHashValue hv
      RavEnum els -> oneof $ V.toList $ V.map doArgVal els

doPair :: T.Text -> RequestArgument -> Gen (Maybe (T.Text, Value))
doPair key arg =
    do shouldGen <-
           if not (ra_optional arg)
           then pure True
           else arbitrary
       if shouldGen
          then do val <- doArgVal (ra_value arg)
                  pure (Just (key, val))
          else pure Nothing

requestGen :: Request -> Gen Value
requestGen req =
    do let args = HM.toList (r_arguments req)
       res <- HM.fromList . catMaybes <$> mapM (uncurry doPair) args
       pure (Object res)
