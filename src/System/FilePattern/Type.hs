
-- | The type of patterns and wildcards
module System.FilePattern.Type(
    FilePattern,
    Pattern(..), parsePattern,
    Path(..), parsePath,
    Pats, fromPats, mkPats,
    Pat(..),
    toWildcard,
    lit, fromLit,
    star
    ) where

import System.FilePattern.Wildcard
import System.FilePath
import Data.List.Extra


mkPats :: [Pat] -> Pats
mkPats = Pats

-- | Convert a Pat to a Wildcard structure
toWildcard :: Pats -> Wildcard [Wildcard String]
toWildcard (Pats xs) = case map (map unstars) $ split (== Skip) xs of
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



-- | A type synonym for file patterns, containing @**@ and @*@. For the syntax
--   and semantics of 'FilePattern' see '?=='.
--
--   Most 'FilePath' values lacking literal @.@ and @..@ components are suitable as 'FilePattern' values which match
--   only that specific file. On (Windows @\\@ is treated as equivalent to @\/@.
--
--   You can write 'FilePattern' values as a literal string, or build them
--   up using the operators '<.>' and '</>'.
type FilePattern = String


newtype Path = Path [String]

newtype Pattern = Pattern (Wildcard [Wildcard String])


split0 :: (a -> Bool) -> [a] -> [[a]]
split0 f [] = []
split0 f xs = split f xs

parsePath :: FilePath -> Path
parsePath = Path . split0 isPathSeparator

parsePattern :: FilePattern -> Pattern
parsePattern = Pattern . fmap (map $ f '*') . f "**" . split0 isPathSeparator
    where
        f :: Eq a => a -> [a] -> Wildcard [a]
        f x xs = case split (== x) xs of
            pre:mid_post -> case unsnoc mid_post of
                Nothing -> Literal pre
                Just (mid, post) -> Wildcard pre mid post
