
-- | Build up shared prefix trees
module System.FilePattern.Tree(
    Tree(..), makeTree
    ) where

import Data.List.Extra
import Prelude


data Tree k v = Tree [v] [(k, Tree k v)]

makeTree :: Ord k => [(v, [k])] -> Tree k v
makeTree = f . sortOn snd
    where
        f xs = Tree (map fst empty) [(fst g1, f $ map snd gs) | gs@(g1:_) <- groups]
            where
                (empty, nonEmpty) = span (null . snd) xs
                groups = groupOn fst [(x, (a,xs)) | (a,x:xs) <- nonEmpty]
