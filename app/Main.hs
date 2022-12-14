module Main (main) where

import Control.Arrow (ArrowChoice (left))
import Expr (reduce, typeCheck)
import Parser (parse)
import Relude
import Transpiler (Js (Js))

source :: Text
source =
    unlines
        [ "-- Π A : Type. Π B : Type. Π C : Type. (B → C) → (A → B) → A → C"
        , "-- λ A : Type. λ B : Type. λ C : Type. λ bc : B → C. λ ab : A → B. λ a : A. bc (ab a)"
        , ""
        , "-- Π Pair : Type → Type → Type"
        , "-- . Π pair : Π A : Type. Π B : Type. A → B → Pair A B"
        , "-- . Π fst : Π A : Type. Π B : Type. Pair A B → A"
        , "-- . Π snd : Π A : Type. Π B : Type. Pair A B → B"
        , "-- . Π A : Type"
        , "-- . Π B : Type"
        , "-- . Pair A B → Pair B A"
        , ""
        , "λ Pair : Type → Type → Type"
        , ". λ pair : Π A : Type. Π B : Type. A → B → Pair A B"
        , ". λ fst : Π A : Type. Π B : Type. Pair A B → A"
        , ". λ snd : Π A : Type. Π B : Type. Pair A B → B"
        , ". λ A : Type"
        , ". λ B : Type"
        , ". λ p : Pair A B"
        , ". pair B A (snd A B p) (fst A B p)"
        , ""
        , "-- Π (Void : Type). let (Not ≡ λ (A : Type). A → Void). Π (P : Type) (Q : Type). (P → Q) → Not Q → Not P"
        , "-- λ Void : Type"
        , "-- . let Not ≡ λ A : Type. A → Void"
        , "-- . λ P : Type"
        , "-- . λ Q : Type"
        , "-- . λ pq : P → Q"
        , "-- . λ nq : Not Q"
        , "-- . λ p : P"
        , "-- . nq (pq p)"
        , ""
        , "-- Π A : Type. A → A"
        , "-- λ A : Type. λ a : A. a"
        ]

main :: IO ()
main = case run of
    Left err -> print err
    Right expr -> writeFile "output.mjs" $ "export default " <> show expr

run :: Either Text Js
run = do
    expr <- left show $ parse source
    _ <- reduce <$> runReaderT (typeCheck expr) []
    pure $ Js expr
