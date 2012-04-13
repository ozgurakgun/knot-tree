{-# LANGUAGE OverloadedStrings #-}

module KnotTree where

import Text.PrettyPrint

data KnotTree a = KnotTree { parent   :: Maybe (KnotTree a)
                           , children :: [KnotTree a]
                           , value    :: a
                           }

prettyKnotTree :: Show a => KnotTree a -> Doc
prettyKnotTree (KnotTree _ [] v) = text $ show v
prettyKnotTree (KnotTree _ ts v) = hang (text $ show v) 4 $ vcat $ ["{"] ++ map prettyKnotTree ts ++ ["}"]

instance Show a => Show (KnotTree a) where
    show = show . prettyKnotTree

singleton :: a -> KnotTree a
singleton = KnotTree Nothing []

setParent :: KnotTree a -> KnotTree a -> KnotTree a
setParent p t = t { parent = Just p }

(-:) :: KnotTree a -> [KnotTree a] -> KnotTree a
x -: xs = result
    where result = x { children = map (setParent result) xs }
