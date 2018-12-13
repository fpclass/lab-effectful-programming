--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 9: Effectful programming                                               --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (Either(..))

import Control.Monad (liftM)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Char

import qualified Lab9 as L

--------------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (L.Either a b) where
    arbitrary = oneof [liftM L.Left arbitrary, liftM L.Right arbitrary]

    shrink (L.Left x)  = [ L.Left  x' | x' <- shrink x ]
    shrink (L.Right y) = [ L.Right y' | y' <- shrink y ]

duckily :: [(String, String)]
duckily =
    [ ("Grandduck", "Grand duckster")
    , ("Baby Duck", "Parent Duck")
    , ("Duckling", "Older duckling")
    , ("Parent Duck", "Grandduck")
    ]

grandparents :: [(String, Maybe String)]
grandparents =
    [ ("Grand duckster", Nothing)
    , ("Grandduck", Nothing)
    , ("Baby Duck", Just "Grandduck")
    , ("Duckling", Nothing)
    , ("Parent Duck", Just "Grand duckster")
    ]

-- | The main entry point to the test suite.
main :: IO ()
main = hspec $ do
    describe "grandduck" $ do
        prop "finds grandducks for ducks" $
            forAll (elements grandparents) $ \(duck, grand) ->
            L.grandduck duckily duck == grand
    describe "zipWithM" $ do
        prop "returns the empty list when the first list is empty" $
            \(xs :: [Int]) -> L.zipWithM L.safediv [] xs == Just []
        prop "returns the empty list when the second list is empty" $
            \(xs :: [Int]) -> L.zipWithM L.safediv xs [] == Just []
        prop "returns a list of the same length as the shortest input list" $
            \(xs :: [Int]) ->
            forAll (arbitrary `suchThat` (0 `notElem`)) $ \(ys :: [Int]) ->
            (length <$> L.zipWithM L.safediv xs ys) ==
            Just (min (length xs) (length ys))
        prop "fails for safediv if the second list contains 0" $
            forAll (arbitrary `suchThat` (0 `elem`)) $ \(ys :: [Int]) ->
            forAll (arbitrary `suchThat` ((<=) (length ys) . length)) $
            \(xs :: [Int]) -> L.zipWithM L.safediv xs ys == Nothing
        prop "for safediv, zipWith divides elements of the two lists" $
            \(xs :: [Int]) ->
            forAll (arbitrary `suchThat` (0 `notElem`)) $ \(ys :: [Int]) ->
            let divProp (Just z,(x,y)) = z * y + (x `mod` y) == x
                divProp (Nothing, _)   = False
            in all divProp $ zip (sequence $ L.zipWithM L.safediv xs ys) (zip xs ys)
    describe "Either is a Functor" $ do
        prop "fmap id x = x" $ do
            \(x :: L.Either Int Int) -> fmap id x == x
        prop "fmap (f . g) x = fmap f (fmap g x)" $ do
            \(x :: L.Either Int Int) ->
                let f = (+) 4
                    g = (*) 2
                in fmap (f . g) x == fmap f (fmap g x)
    describe "Either is an Applicative" $ do
        prop "pure id <*> v = v" $ \(v :: L.Either Int Int) ->
            (pure id <*> v) == v
        prop "pure f <*> pure x = pure (f x)" $ \(x :: Int) ->
            (pure (+5) <*> pure x) == (pure ((+5) x) :: L.Either Int Int)
        prop "u <*> pure y = pure ($ y) <*> u" $
            \(y :: Int) x -> let u1 = pure (+5) :: L.Either Int (Int -> Int)
                                 u2 = L.Left x  :: L.Either Int (Int -> Int)
                             in (u1 <*> pure y) == (pure ($ y) <*> u1) &&
                                (u2 <*> pure y) == (pure ($ y) <*> u2)
        prop "pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $
            \(w :: L.Either Int Int) ->
                let u1 = pure (+5) :: L.Either Int (Int -> Int)
                    u2 = L.Left 0  :: L.Either Int (Int -> Int)
                    v1 = pure (*2) :: L.Either Int (Int -> Int)
                    v2 = L.Left 1  :: L.Either Int (Int -> Int)
                in (pure (.) <*> u1 <*> v1 <*> w) == (u1 <*> (v1 <*> w))
                && (pure (.) <*> u2 <*> v1 <*> w) == (u2 <*> (v1 <*> w))
                && (pure (.) <*> u1 <*> v2 <*> w) == (u1 <*> (v2 <*> w))
                && (pure (.) <*> u2 <*> v2 <*> w) == (u2 <*> (v2 <*> w))
    describe "Either is a Monad" $ do
        prop "return x >>= f = f x" $
            \(x :: Int) ->
                let f a = L.Right a :: L.Either Int Int
                    g b = L.Left 0  :: L.Either Int Int
                in (return x >>= f) == f x &&
                   (return x >>= g) == g x
        prop "m >>= return = m" $
            \(m :: L.Either Int Int) -> (m >>= return) == m
        prop "(m >>= f) >>= g = m >>= (\\x -> f x >>= g)" $
            \(m :: L.Either Int Int) ->
                let f a = L.Right a
                    g b = L.Right b
                in ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))

--------------------------------------------------------------------------------
