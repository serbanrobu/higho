module Ast (Ast (..)) where

import GHC.Show (showSpace)
import Relude
import Text.Show (ShowS, showChar, showParen, showString, shows, showsPrec)

data Ast
    = App Ast Ast
    | Lam (Maybe Text) Ast Ast
    | Let Text Ast Ast
    | Pi (Maybe Text) Ast Ast
    | Type
    | Var Text

varToText :: Maybe Text -> Text
varToText = fromMaybe "_"

showText :: Text -> ShowS
showText = showString . toString

showLetHead :: Text -> Ast -> ShowS
showLetHead n t =
    showChar '('
        . showText n
        . showString " ≡ "
        . shows t
        . showChar ')'

showLetBody :: Ast -> ShowS
showLetBody (Let n a b) = showSpace . showLetHead n a . showLetBody b
showLetBody b = showString ". " . shows b

showAbsHead :: Text -> Ast -> ShowS
showAbsHead n t =
    showChar '('
        . showText n
        . showString " : "
        . shows t
        . showChar ')'

showAbsBody :: Ast -> ShowS
showAbsBody b = showString ". " . shows b

showLamBody :: Ast -> ShowS
showLamBody (Lam m a b) = showSpace . showAbsHead (varToText m) a . showLamBody b
showLamBody b = showAbsBody b

showArr :: Int -> Ast -> Ast -> ShowS
showArr p a b =
    showParen (p > arrPrec) $
        showsPrec (arrPrec + 1) a
            . showString " → "
            . showsPrec arrPrec b
  where
    arrPrec = 9

showPiBody :: Ast -> ShowS
showPiBody (Pi Nothing a b) = showString ". " . showArr 0 a b
showPiBody (Pi (Just n) a b) = showSpace . showAbsHead n a . showPiBody b
showPiBody b = showAbsBody b

instance Show Ast where
    showsPrec p (App f a) =
        showParen (p > appPrec) $
            showsPrec appPrec f
                . showSpace
                . showsPrec (appPrec + 1) a
      where
        appPrec = 10
    showsPrec p (Lam m a b) =
        showParen (p > 0) $
            showString "λ " . showAbsHead (varToText m) a . showLamBody b
    showsPrec p (Let n a b) =
        showParen (p > 0) $
            showString "let " . showLetHead n a . showLetBody b
    showsPrec p (Pi Nothing a b) = showArr p a b
    showsPrec p (Pi (Just n) a b) =
        showParen (p > 0) $
            showString "Π " . showAbsHead n a . showPiBody b
    showsPrec _ Type = showString "Type"
    showsPrec _ (Var n) = showText n
