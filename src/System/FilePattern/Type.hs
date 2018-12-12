
-- | The type of patterns and wildcards
module System.FilePattern.Type(
    Pats, fromPats, mkPats,
    Pat(..),
    toWildcard,
    lit, fromLit,
    star
    ) where

import System.FilePattern.Wildcard
import System.FilePattern.Core2
import Data.List.Extra


mkPats :: [Pat] -> Pats
mkPats = Pats

-- | Convert a Pat to a Wildcard structure
toWildcard :: Pats -> Pattern
toWildcard (Pats xs) = Pattern $ case map (map unstars) $ split (== Skip) xs of
    [] -> error "toWildcard: impossible - split never returns []"
    pre:xs -> case unsnoc xs of
        Nothing -> Literal pre
        Just (mid, post) -> Wildcard pre mid post
    where unstars (Stars x) = x
          unstars Skip = error "toWildcard: impossible - already split on Skip"


-- | Parsed 'FilePattern'.
newtype Pats = Pats {fromPats :: [Pat]}
    deriving (Eq,Show)


data Pat = Skip -- ^ /**/
         | Stars (Wildcard String) -- ^ *foo*, prefix (fixed), infix floaters, suffix
                                   -- e.g. *foo*bar = Stars "" ["foo"] "bar"
            deriving (Show,Eq,Ord)

-- | Create a mattern equivalent to *.
star :: Pat
star = Stars $ Wildcard "" [] ""

-- | Create a mattern equivalent to x as a literal.
lit :: String -> Pat
lit = Stars . Literal

fromLit :: Pat -> Maybe String
fromLit (Stars (Literal x)) = Just x
fromLit _ = Nothing
