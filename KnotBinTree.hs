{-# LANGUAGE OverloadedStrings #-}

module KnotBinTree where

import Control.Applicative ( (<$>) )
import Control.DeepSeq ( NFData(rnf), deepseq )
import Control.Monad.Random ( getRandomR )
import Criterion.Main ( defaultMain, bench, nf )
import Data.Maybe ( isJust )
import Text.PrettyPrint ( Doc, empty, text, hang, (<+>), parens, vcat )
import Test.QuickCheck -- ( quickCheck )

data KnotBinTree a = KnotBinTree { parent     :: Maybe (KnotBinTree a)
                                 , leftChild  :: Maybe (KnotBinTree a)
                                 , rightChild :: Maybe (KnotBinTree a)
                                 , value      :: a
                                 }

prettyKnotBinTree :: Show a => Maybe (KnotBinTree a) -> Doc
prettyKnotBinTree Nothing = "null"
prettyKnotBinTree (Just (KnotBinTree p l r v))
    = hang (text (show v) <+> parentVal) 4
    $ vcat ["{", prettyKnotBinTree l, prettyKnotBinTree r, "}"]
    where parentVal = case p of Nothing -> empty
                                Just i  -> parens $ text $ show $ value i

instance Show a => Show (KnotBinTree a) where
    show = show . prettyKnotBinTree . Just

instance NFData a => NFData (KnotBinTree a) where
    rnf t = rnf (leftChild t, rightChild t, value)

singleton :: a -> KnotBinTree a
singleton = KnotBinTree Nothing Nothing Nothing

setParent :: KnotBinTree a -> KnotBinTree a -> KnotBinTree a
setParent p t = t { parent = Just p }

(-:) :: KnotBinTree a -> (KnotBinTree a, KnotBinTree a) -> KnotBinTree a
x -: (a,b) = result
    where result = x { leftChild  = Just $ setParent result a
                     , rightChild = Just $ setParent result b
                     }

fromList :: Ord a => (a,[a]) -> KnotBinTree a
fromList (x,xs) = foldr insert (singleton x) (reverse xs)

insert :: Ord a => a -> KnotBinTree a -> KnotBinTree a
insert x t = if x <= value t
                then -- left
                    case leftChild t of
                        Nothing -> result where result = t { leftChild = Just $ setParent result (singleton x) }
                        Just ch -> result where result = t { leftChild = Just $ insert x ch }
                else -- right
                    case rightChild t of
                        Nothing -> result where result = t { rightChild = Just $ setParent result (singleton x) }
                        Just ch -> result where result = t { rightChild = Just $ insert x ch }
-- insert x t@(KnotBinTree _ Nothing _ v) | x <= v = result
--     where result = t { leftChild = Just $ setParent result (singleton x) }
-- insert x t@(KnotBinTree _ (Just l) _ v) | x <= v = result
--     where result = t { leftChild = Just $ insert x l }
-- insert x t@(KnotBinTree _ _ Nothing v) | x > v = result
--     where result = t { rightChild = Just $ setParent result (singleton x) }
-- insert x t@(KnotBinTree _ _ (Just l) v) | x > v = result
--     where result = t { rightChild = Just $ insert x l }

search :: Ord a => a -> KnotBinTree a -> Maybe (KnotBinTree a)
search x t = case x `compare` value t of
    EQ -> Just t
    LT -> case leftChild  t of Nothing -> Nothing
                               Just ch -> search x ch
    GT -> case rightChild t of Nothing -> Nothing
                               Just ch -> search x ch

member :: Ord a => a -> KnotBinTree a -> Bool
member x t = isJust (search x t)

pathTo :: Ord a => a -> KnotBinTree a -> [a]
pathTo px pt | not (member px pt) = []
             | otherwise = worker px pt
    where
        worker x t = case x `compare` value t of
            EQ -> [value t]
            LT -> case leftChild  t of Nothing -> []
                                       Just ch -> value t : worker x ch
            GT -> case rightChild t of Nothing -> []
                                       Just ch -> value t : worker x ch

pathTo' :: Ord a => a -> KnotBinTree a -> [a]
pathTo' x t = case search x t of Nothing -> []
                                 Just t' -> value t' : parentVals t'

parentVals :: KnotBinTree a -> [a]
parentVals KnotBinTree { parent = Nothing } = []
parentVals KnotBinTree { parent = Just p  } = value p : parentVals p

prop_insertMember :: (Eq a, Ord a) => a -> (a,[a]) -> Bool
prop_insertMember x (y,ys) = (x `elem` y:ys) == (member x (fromList (y,ys)))

prop_pathTos :: Ord a => a -> (a,[a]) -> Bool
prop_pathTos x ys = pathTo x (fromList ys) == reverse (pathTo' x (fromList ys))

qcAll :: IO ()
qcAll = do
    quickCheck ( prop_insertMember :: Int -> (Int,[Int]) -> Bool )
    quickCheck ( prop_pathTos      :: Int -> (Int,[Int]) -> Bool )

main :: IO ()
main = do
    let
        rand :: IO Int
        rand = getRandomR (0,10000)

        nb :: Int
        nb = 10000

    top     <- rand
    stuff   <- sequence $ replicate nb rand
    needles <- map (stuff!!) <$> sequence (replicate 5 $ getRandomR (0,nb-1))
    let tree = fromList (top,stuff)
    deepseq (needles,tree) $ defaultMain $ concat
        [ [ bench ("pathTo' " ++ show needle) $ nf (uncurry pathTo') (needle,tree)
          , bench ("pathTo  " ++ show needle) $ nf (uncurry pathTo ) (needle,tree)
          ]
        |  needle <- needles
        ]
