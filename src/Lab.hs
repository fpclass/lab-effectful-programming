--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Effectful programming                                                 --
--------------------------------------------------------------------------------

module Lab where

import Prelude hiding (Either(..), catch, mapM)

--------------------------------------------------------------------------------
-- safediv

-- | This is our usual function for safe division, which returns Nothing if
-- the second argument is 0, or the result of dividing the first argument by
-- the second wrapped into the Just constructor instead.
safediv :: Int -> Int -> Maybe Int
safediv x 0 = Nothing
safediv x y = Just (x `div` y)

--------------------------------------------------------------------------------
-- Using the Maybe monad

duckily :: [(String, String)]
duckily =
    [ ("Grandduck", "Grand duckster")
    , ("Baby Duck", "Parent Duck")
    , ("Duckling", "Older duckling")
    , ("Parent Duck", "Grandduck")
    ]

-- | A function to find the grandduck of a duck in the duck family.
grandduck :: [(String, String)] -> String -> Maybe String
-- grandduck rels name = lookup name rels >>= 
--     \parent -> lookup parent rels 
-- grandduck rels name = do
--     parent <- lookup name rels
--     lookup parent rels 
grandduck rels name = lookup name rels >>= flip lookup rels 

--------------------------------------------------------------------------------
-- Generic functions

-- | mapM, but without do-notation
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = pure []
-- mapM f (x:xs) = 
--     f x >>= \y -> 
--     mapM f xs >>= \ys ->
--     pure (y:ys)
mapM f (x:xs) = do
    y <- f x 
    ys <- mapM f xs 
    pure (y:ys)

-- | the same as above, but with Applicative instead of Monad
mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA _ [] = pure []
mapA f (x:xs) = (:) <$> f x <*> mapA f xs 

-- | Like zipWith, but the function returns a computation (has an effect).
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM _ [] _ = pure []
zipWithM _ _ [] = pure []
-- zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
-- zipWithM f (x:xs) (y:ys) = do
--     r <- f x y
--     rs <- zipWithM f xs ys
--     pure (r:rs)
zipWithM f (x:xs) (y:ys) =
    f x y >>= \r ->
    zipWithM f xs ys >>= \rs ->
    pure (r:rs)





-- instance Applicative [] where 
--    pure x = [x]
-- instance Monad [] where 
--    xs >>= f = concat (map f xs)

--    zipWithM (\x y -> [x,y]) [1,2] [3,4]
-- [1,2] >>= 

-- zipWithM f [] [] = [[]]
-- zipWithM f [2] [4] = [[2],[4]]

-- => [1,3] >>= \r ->
--    [[2],[4]] >>= \rs ->
--    pure (r:rs)

-- Case (r=1) (rs=[2]): [[1,2]]
-- Case (r=1) (rs=[4]): [[1,4]]
-- Case (r=3) (rs=[2]): [[3,2]]
-- Case (r=3) (rs=[4]): [[3,4]]

-- => (\x y -> [x,y]) 1 3 >>= \r ->
--    zipWithM (\x y -> [x,y]) [2] [4] >>= \rs ->
--    pure (r:rs)
-- => [1,3] >>= \r ->
--    zipWithM (\x y -> [x,y]) [2] [4] >>= \rs ->
--    pure (r:rs)
-- => let f = (\x y -> [x,y]) 
--        g = \r -> zipWithM f [2] [4] >>= 
--            \rs -> pure (r:rs)
--    in concat (map g [1,3])

--    zipWithM f [2] [4] >>= \rs -> pure (1:rs)
-- => (f 2 4 >>= \r -> 
--    zipWithM f [] [] >>= \rs ->
--    pure (r:rs)) >>= \rs ->
--    pure (1:rs)
-- => ([2,4] >>= \r -> 
--    zipWithM f [] [] >>= \rs ->
--    pure (r:rs)) >>= \rs ->
--    pure (1:rs)
-- => ([2,4] >>= \r -> 
--    pure [] >>= \rs ->
--    pure (r:rs)) >>= \rs ->
--    pure (1:rs)
-- => ([2,4] >>= \r -> 
--    [[]] >>= \rs ->
--    pure (r:rs)) >>= \rs ->
--    pure (1:rs)
-- => (concat (map (\r -> 
--    [[]] >>= \rs ->
--    pure (r:rs)) [2,4]) >>= \rs ->
--    pure (1:rs)
-- => (concat (map (\r -> 
--    concat (map (\rs -> pure (r:rs)) [])) [2,4]) >>= \rs ->
--    pure (1:rs) 
-- => (concat (map (\r -> 
--    concat ([(\rs -> pure (r:rs)) []])) [2,4])) >>= \rs ->
--    pure (1:rs)
-- => (concat (map (\r -> 
--    concat ([pure (r:[])])) [2,4])) >>= \rs ->
--    pure (1:rs)
-- => (concat (map (\r -> 
--    concat [[r]]) [2,4])) >>= \rs ->
--    pure (1:rs)
-- => (concat (map (\r -> [r]) [2,4])) >>= \rs ->
--    pure (1:rs)
-- => (concat [[2],[4]]) >>= \rs ->
--    pure (1:rs)
-- => [2,4] >>= \rs ->
--    pure (1:rs)

--------------------------------------------------------------------------------
-- Either is a Monad

-- | The Either type is defined in the Prelude and has Functor, Applicative,
-- and Monad instances there. For the purpose of this lab, we do not import it
-- and instead define it here ourselves.
data Either a b = Left a | Right b
    deriving (Eq, Show)

instance Functor (Either e) where
    -- fmap :: (a -> b) -> Either e a -> Either e b
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)

instance Applicative (Either e) where
    -- pure :: a -> Either e a
    pure = Right

    -- (<*>) :: Either e (a -> b) -> Either e a -> Either e b
    (Left x) <*> _ = Left x
    -- (Right f) <*> (Left x) = Left x 
    -- (Right f) <*> (Right y) = Right (f y)
    (Right f) <*> y = fmap f y -- f <$> y

instance Monad (Either e) where
    -- (>>=) :: Either e a -> (a -> Either e b) -> Either e b
    (Left x) >>= _ = Left x
    (Right x) >>= f = f x

--------------------------------------------------------------------------------
