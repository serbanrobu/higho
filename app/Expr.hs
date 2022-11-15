module Expr (Expr (..), typeCheck, reduce) where

import GHC.Show (showChar, showSpace)
import Relude
import Text.Show (ShowS, showParen, showString, shows, showsPrec)

data Expr
    = App Expr Expr
    | Lam Bool (Maybe Text) Expr Expr
    | Let Text Expr Expr
    | Pi Bool (Maybe Text) Expr Expr
    | Type
    | Var Text Int

varToText :: Maybe Text -> Text
varToText = fromMaybe "_"

showText :: Text -> ShowS
showText = showString . toString

showBraces :: Bool -> ShowS -> ShowS
showBraces p s = if p then showChar '{' . s . showChar '}' else s

showAbsHead :: Text -> Expr -> ShowS
showAbsHead n t =
    showText n
        . showString " : "
        . shows t

showAbsBody :: Expr -> ShowS
showAbsBody b = showString ". " . shows b

showArr :: Int -> Bool -> Expr -> Expr -> ShowS
showArr p i a b =
    showParen (p > arrPrec) $
        showBraces i (showsPrec (arrPrec + 1) a)
            . showString " → "
            . showsPrec arrPrec b
  where
    arrPrec = 9

instance Show Expr where
    showsPrec p (App f a) =
        showParen (p > appPrec) $
            showsPrec appPrec f
                . showSpace
                . showsPrec (appPrec + 1) a
      where
        appPrec = 10
    showsPrec p (Lam infer m a b) =
        showParen (p > 0) $
            showString "λ "
                . showAbsHead (varToText m) a
                . showAbsBody b
    showsPrec p (Let n a b) =
        showParen (p > 0) $
            showString "let "
                . showText n
                . showString " ≡ "
                . shows a
                . showAbsBody b
    showsPrec p (Pi infer Nothing a b) = showArr p infer a b
    showsPrec p (Pi _ (Just n) a b) =
        showParen (p > 0) $
            showString "Π "
                . showAbsHead n a
                . showAbsBody b
    showsPrec _ Type = showString "Type"
    showsPrec _ (Var n _) = showText n

instance Eq Expr where
    App f x == App f' x' = f == f' && x == x'
    Lam _ _ a b == Lam _ _ a' b' = a == a' && b == b'
    Let _ a b == Let _ a' b' = a == a' && b == b'
    Pi _ _ a b == Pi _ _ a' b' = a == a' && b == b'
    Type == Type = True
    Var _ i == Var _ i' = i == i'
    _ == _ = False

type TypingContext = [(Text, Expr)]

typeCheck :: Expr -> ReaderT TypingContext (Either Text) Expr
typeCheck (App f x) =
    typeCheck f
        >>= ( \case
                Pi infer m a b -> do
                    xT <- typeCheck x
                    when (a /= xT) . lift $ Left "TODO 2"
                    case m of
                        Nothing -> pure $ substitute 0 x b
                        Just _ -> pure . shift 0 (-1) $ substitute 0 (shift 0 1 x) b
                _ -> lift $ Left "TODO 3"
            )
            . value
typeCheck (Lam infer m a b) =
    typeCheck a >>= \case
        Type -> do
            bT <- maybe id (local . (:) . (,a)) m $ typeCheck b
            pure $ Pi infer m a bT
        _ -> lift $ Left "TODO 4"
typeCheck (Let _ a b) = typeCheck . shift 0 (-1) $ substitute 0 (shift 0 1 a) b
typeCheck (Pi infer m a b) =
    typeCheck a >>= \case
        Type ->
            maybe id (local . (:) . (,a)) m (typeCheck b) >>= \case
                Type -> pure Type
                _ -> lift $ Left "TODO 5"
        _ -> lift $ Left "TODO 6"
typeCheck Type = pure Type
typeCheck (Var _ i) = do
    ctx <- ask
    (_, t) <- lift . maybeToRight "Index out of bounds." $ ctx !!? i
    pure $ shift 0 (i + 1) t

shift :: Int -> Int -> Expr -> Expr
shift c d (App f x) = App (shift c d f) $ shift c d x
shift c d (Lam infer Nothing a b) = Lam infer Nothing (shift c d a) $ shift c d b
shift c d (Lam infer m a b) = Lam infer m (shift c d a) $ shift (c + 1) d b
shift c d (Let n a b) = Let n (shift c d a) $ shift (c + 1) d b
shift c d (Pi infer Nothing a b) = Pi infer Nothing (shift c d a) $ shift c d b
shift c d (Pi infer m a b) = Pi infer m (shift c d a) $ shift (c + 1) d b
shift _ _ t@Type = t
shift c d t@(Var n i) = if i < c then t else Var n $ i + d

substitute :: Int -> Expr -> Expr -> Expr
substitute i x (App f y) = App (substitute i x f) $ substitute i x y
substitute i x (Lam infer Nothing a b) = Lam infer Nothing (substitute i x a) $ substitute i x b
substitute i x (Lam infer m a b) = Lam infer m (substitute i x a) $ substitute (i + 1) (shift 0 1 x) b
substitute i x (Let n a b) = Let n (substitute i x a) $ substitute (i + 1) (shift 0 1 x) b
substitute i x (Pi infer Nothing a b) = Pi infer Nothing (substitute i x a) $ substitute i x b
substitute i x (Pi infer m a b) = Pi infer m (substitute i x a) $ substitute (i + 1) (shift 0 1 x) b
substitute _ _ t@Type = t
substitute i x t@(Var _ i') = if i' == i then x else t

value :: Expr -> Expr
value t@(App f x) = case value f of
    Lam _ _ _ b -> value . shift 0 (-1) $ substitute 0 (shift 0 1 x) b
    _ -> t
value (Let _ a b) = value . shift 0 (-1) $ substitute 0 (shift 0 1 a) b
value t = t

reduce :: Expr -> Expr
reduce =
    ( \case
        App f x -> App (reduce f) $ reduce x
        Lam infer m a b -> Lam infer m (reduce a) $ reduce b
        Pi infer m a b -> Pi infer m (reduce a) $ reduce b
        s -> s
    )
        . value
