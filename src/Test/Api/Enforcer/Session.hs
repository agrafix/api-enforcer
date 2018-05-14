module Test.Api.Enforcer.Session where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Hashable
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

newtype Var = Var { unVar :: T.Text }
    deriving (Show, Eq, Hashable)

data Literal
    = LString T.Text
    | LBool Bool
    | LInt Int
    | LFloat Double
    | LList (V.Vector SessionExpr)
    | LHash (HM.HashMap T.Text SessionExpr)
    deriving (Show, Eq)

data SessionExpr
    = SeLiteral Literal
    | SeVar Var
    | SeLet Var SessionExpr SessionExpr
    | SeLambda (V.Vector Var) SessionExpr
    | SeApp Var (V.Vector SessionExpr)
    deriving (Show, Eq)

type Env = HM.HashMap Var SessionExpr

reduceLiteral :: (MonadReader Env m, MonadError T.Text m) => Literal -> m SessionExpr
reduceLiteral l =
    fmap SeLiteral $
    case l of
      LString x -> pure $ LString x
      LBool b -> pure $ LBool b
      LInt i -> pure $ LInt i
      LFloat f -> pure $ LFloat f
      LList xs -> LList <$> V.mapM reduceExpr xs
      LHash hm ->
          LHash . HM.fromList <$>
          mapM (\(k, v) -> (,) <$> pure k <*> reduceExpr v) (HM.toList hm)

reduceApp ::
    (MonadReader Env m, MonadError T.Text m)
    => Var -> V.Vector SessionExpr -> m SessionExpr
reduceApp v args =
    do evaledArgs <- mapM reduceExpr args
       case (v, V.toList evaledArgs) of
         (Var "add", [SeLiteral (LInt lhs), SeLiteral (LInt rhs)]) ->
             pure (SeLiteral $ LInt $ lhs + rhs)
         (Var "add", [SeLiteral (LFloat lhs), SeLiteral (LFloat rhs)]) ->
             pure (SeLiteral $ LFloat $ lhs + rhs)
         (Var "int2float", [SeLiteral (LInt lhs)]) ->
             pure (SeLiteral $ LFloat $ fromIntegral lhs)
         (Var "float2int", [SeLiteral (LFloat lhs)]) ->
             pure (SeLiteral $ LInt $ truncate lhs)
         _ ->
             do fun <- reduceExpr (SeVar v)
                case fun of
                  SeLambda argVars bodyExpr
                    | length argVars /= length evaledArgs ->
                          throwError ("Wrong number of arguments passed to " <> unVar v)
                    | otherwise ->
                          let bindings =
                                  HM.fromList $ V.toList $ V.zip argVars evaledArgs
                          in local (HM.union bindings) (reduceExpr bodyExpr)
                  _ ->
                      throwError ("Can not call " <> unVar v <> " as function")


reduceExpr :: (MonadReader Env m, MonadError T.Text m) => SessionExpr -> m SessionExpr
reduceExpr se =
    case se of
      SeLiteral l -> reduceLiteral l
      SeVar v ->
          ask >>= \env ->
          case HM.lookup v env of
            Nothing ->
                throwError ("Undefined variable " <> unVar v)
            Just expr -> reduceExpr expr
      SeLet v expr expr2 -> local (HM.insert v expr) (reduceExpr expr2)
      SeApp v args -> reduceApp v args
      SeLambda args expr -> pure (SeLambda args expr)

data Action
    = Action
    { a_url :: SessionExpr
    , a_requestBody :: SessionExpr
    } deriving (Show, Eq)

data Session
    = Session
    { s_actions :: V.Vector Action
    } deriving (Show, Eq)
