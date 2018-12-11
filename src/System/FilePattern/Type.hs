
-- | The type of patterns and wildcards
module System.FilePattern.Type(
    FilePattern,
    Pats(..),
    Pat(..),
    toWildcard,
    lit, fromLit,
    star
    ) where

import System.FilePattern.Wildcard
import Data.List.Extra


-- | Convert a Pat to a Wildcard structure
toWildcard :: Pats -> Wildcard [Wildcard String]
toWildcard (Pats xs) = case map (map unstars) $ split (== Skip) xs of
    [] -> error "toWildcard: impossible - split never returns []"
    pre:xs -> case unsnoc xs of
        Nothing -> Literal pre
        Just (mid, post) -> Wildcard pre mid post
    where unstars (Stars x) = x
          unstars Skip = error "toWildcard: impossible - already split on Skip"



-- | A type synonym for file patterns, containing @**@ and @*@. For the syntax
--   and semantics of 'FilePattern' see '?=='.
--
--   Most 'FilePath' values lacking literal @.@ and @..@ components are suitable as 'FilePattern' values which match
--   only that specific file. On (Windows @\\@ is treated as equivalent to @\/@.
--
--   You can write 'FilePattern' values as a literal string, or build them
--   up using the operators '<.>' and '</>'.
type FilePattern = String


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
