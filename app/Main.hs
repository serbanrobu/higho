module Main (main) where

import Control.Arrow (ArrowChoice (left))
import Expr (fromAst, toAst, typeCheck)
import qualified Parser (file)
import Relude
import Text.Parsec (parse)

source :: Text
source =
    unlines
        [ "-- Π (A : Type) (B : Type) (C : Type). (B → C) → (A → B) → A → C"
        , "-- λ (A : Type) (B : Type) (C : Type) (bc : B → C) (ab : A → B) (a : A). bc (ab a)"
        , ""
        , "-- Π (Pair : Type → Type → Type)"
        , "--   (pair : Π (A : Type) (B : Type). A → B → Pair A B)"
        , "--   (fst : Π (A : Type) (B : Type). Pair A B → A)"
        , "--   (snd : Π (A : Type) (B : Type). Pair A B → B)"
        , "--   (A : Type)"
        , "--   (B : Type)."
        , "--   Pair A B → Pair B A"
        , ""
        , "-- λ (Pair : Type → Type → Type)"
        , "--   (pair : Π (A : Type) (B : Type). A → B → Pair A B)"
        , "--   (fst : Π (A : Type) (B : Type). Pair A B → A)"
        , "--   (snd : Π (A : Type) (B : Type). Pair A B → B)"
        , "--   (A : Type)"
        , "--   (B : Type)"
        , "--   (p : Pair A B)."
        , "--   pair B A (snd A B p) (fst A B p)"
        , ""
        , "-- Π (Void : Type). let (Not ≡ λ (A : Type). A → Void). Π (P : Type) (Q : Type). (P → Q) → Not Q → Not P"
        , "-- λ (Void : Type). let (Not ≡ λ (A : Type). A → Void). λ (P : Type) (Q : Type) (pq : P → Q) (nq : Not Q) (p : P). nq (pq p)"
        , "-- Π (A : Type). A → A"
        , "λ (A : Type) (a : A). a"
        ]

main :: IO ()
main = print $ do
    ast <- left show $ parse Parser.file "" source
    term <- runReaderT (fromAst ast) []
    ty <- runReaderT (typeCheck term) []
    runReaderT (toAst ty) []
