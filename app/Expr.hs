module Expr (Expr (..), typeCheck, fromAst, toAst) where

import Ast (Ast)
import qualified Ast
import Data.List (elemIndex)
import Relude

data Expr
    = App Expr Expr
    | Lam (Maybe Text) Expr Expr
    | Let Text Expr Expr
    | Pi (Maybe Text) Expr Expr
    | Type
    | Var Int
    deriving (Show, Eq)

type NamingContext = [Text]

fromAst :: Ast -> ReaderT NamingContext (Either Text) Expr
fromAst (Ast.App f x) = App <$> fromAst f <*> fromAst x
fromAst (Ast.Lam m a b) = Lam m <$> fromAst a <*> maybe id (\n -> local (n :)) m (fromAst b)
fromAst (Ast.Let n a b) = Let n <$> fromAst a <*> local (n :) (fromAst b)
fromAst (Ast.Pi m a b) = Pi m <$> fromAst a <*> maybe id (\n -> local (n :)) m (fromAst b)
fromAst Ast.Type = pure Type
fromAst (Ast.Var n) = do
    ctx <- ask
    i <- lift . maybeToRight "TODO 0" $ elemIndex n ctx
    pure $ Var i

toAst :: Expr -> ReaderT NamingContext (Either Text) Ast
toAst (App f x) = Ast.App <$> toAst f <*> toAst x
toAst (Lam m a b) = Ast.Lam m <$> toAst a <*> maybe id (\n -> local (n :)) m (toAst b)
toAst (Let n a b) = Ast.Let n <$> toAst a <*> local (n :) (toAst b)
toAst (Pi m a b) = Ast.Pi m <$> toAst a <*> maybe id (\n -> local (n :)) m (toAst b)
toAst Type = pure Ast.Type
toAst (Var i) = do
    ctx <- ask
    n <- lift . maybeToRight "TODO 1" $ ctx !!? i
    pure $ Ast.Var n

type TypingContext = [(Text, Expr)]

typeCheck :: Expr -> ReaderT TypingContext (Either Text) Expr
typeCheck (App f x) =
    typeCheck f
        >>= ( \case
                Pi m a b -> do
                    xTy <- typeCheck x
                    when (a /= xTy) . lift $ Left "TODO 2"
                    case m of
                        Nothing -> pure $ substitute 0 x b
                        Just _ -> pure . shift 0 (-1) $ substitute 0 (shift 0 1 x) b
                _ -> lift $ Left "TODO 3"
            )
            . value
typeCheck (Lam m a b) =
    typeCheck a >>= \case
        Type -> do
            bTy <- maybe id (\n -> local ((n, a) :)) m $ typeCheck b
            pure $ Pi m a bTy
        _ -> lift $ Left "TODO 4"
typeCheck (Let _ a b) = typeCheck . shift 0 (-1) $ substitute 0 (shift 0 1 a) b
typeCheck (Pi m a b) =
    typeCheck a >>= \case
        Type ->
            maybe id (\n -> local ((n, a) :)) m (typeCheck b) >>= \case
                Type -> pure Type
                _ -> lift $ Left "TODO 5"
        _ -> lift $ Left "TODO 6"
typeCheck Type = pure Type
typeCheck (Var i) = do
    ctx <- ask
    (_, ty) <- lift . maybeToRight "Index out of bounds." $ ctx !!? i
    pure $ shift 0 (i + 1) ty

shift :: Int -> Int -> Expr -> Expr
shift c d (App f x) = App (shift c d f) $ shift c d x
shift c d (Lam Nothing a b) = Lam Nothing (shift c d a) $ shift c d b
shift c d (Lam m a b) = Lam m (shift c d a) $ shift (c + 1) d b
shift c d (Let n a b) = Let n (shift c d a) $ shift (c + 1) d b
shift c d (Pi Nothing a b) = Pi Nothing (shift c d a) $ shift c d b
shift c d (Pi m a b) = Pi m (shift c d a) $ shift (c + 1) d b
shift _ _ t@Type = t
shift c d t@(Var i) = if i < c then t else Var $ i + d

substitute :: Int -> Expr -> Expr -> Expr
substitute i x (App f y) = App (substitute i x f) $ substitute i x y
substitute i x (Lam Nothing a b) = Lam Nothing (substitute i x a) $ substitute i x b
substitute i x (Lam m a b) = Lam m (substitute i x a) $ substitute (i + 1) (shift 0 1 x) b
substitute i x (Let n a b) = Let n (substitute i x a) $ substitute (i + 1) (shift 0 1 x) b
substitute i x (Pi Nothing a b) = Pi Nothing (substitute i x a) $ substitute i x b
substitute i x (Pi m a b) = Pi m (substitute i x a) $ substitute (i + 1) (shift 0 1 x) b
substitute _ _ t@Type = t
substitute i x t@(Var i') = if i' == i then x else t

value :: Expr -> Expr
value t@(App f x) = case value f of
    Lam _ _ b -> value . shift 0 (-1) $ substitute 0 (shift 0 1 x) b
    _ -> t
value (Let _ a b) = value . shift 0 (-1) $ substitute 0 (shift 0 1 a) b
value t = t

-- def value(expr: Expr, ctx: Context = []) -> Expr:
--     match expr:
--         case App(fun, arg):
--             match value(fun, ctx):
--                 case Lam(_, _, body):
--                     return value(shift(substitute(body, shift(arg, 1)), -1), ctx)
--
--                 case _:
--                     raise Exception('Expecting a lambda')
--
--         case Let(var, term, body):
--             return value(body, [(var, Unknown(), term), *ctx])
--
--         case Var(index):
--             _, _, term = ctx[index]
--             return expr if term is None else shift(term, int(index) + 1)
--
--         case _:
--             return expr
