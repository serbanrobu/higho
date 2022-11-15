module Transpiler (Js (..)) where

import Data.Text (replace)
import Expr (Expr (..))
import GHC.Show (showChar)
import Relude
import Text.Show (ShowS, showParen, showString, shows, showsPrec)

newtype Js = Js Expr

instance Show Js where
    showsPrec p (Js (App f a)) =
        showParen (p > appPrec) $
            showsPrec appPrec (Js f)
                . showChar '('
                . shows (Js a)
                . showChar ')'
      where
        appPrec = 10
    showsPrec p (Js (Lam m _ b)) =
        showParen (p > 0) $
            showVar (fromMaybe "_" m)
                . showString " => "
                . shows (Js b)
    showsPrec _ (Js (Let n a b)) =
        showChar '('
            . showVar n
            . showString " => "
            . shows (Js b)
            . showString ")("
            . shows (Js a)
            . showChar ')'
    showsPrec _ (Js Pi{}) = showString "null"
    showsPrec _ (Js Type) = showString "null"
    showsPrec _ (Js (Var n _)) = showVar n

showText :: Text -> ShowS
showText = showString . toString

showVar :: Text -> ShowS
showVar = showText . replace "\'" "$"
