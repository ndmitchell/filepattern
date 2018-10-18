
-- | The types involved
module System.FilePattern.Type(
    FilePattern,
    Pats(..),
    Pat(..),
    Wildcard(..),
    wildcard,
    lit, fromLit,
    star
    ) where

import Data.Functor
import Data.List.Extra
import Prelude


-- | A type synonym for file patterns, containing @**@ and @*@. For the syntax
--   and semantics of 'FilePattern' see '?=='.
--
--   Most 'FilePath' values lacking @.@ and @..@ are suitable as 'FilePattern' values which match
--   only that specific file. On (Windows @\\@ is treated as equivalent to @\/@.
--
--   You can write 'FilePattern' values as a literal string, or build them
--   up using the operators '<.>' and '</>'.
type FilePattern = String


-- | Parsed 'FilePattern'.
newtype Pats = Pats {fromPats :: [Pat]}
    deriving (Eq,Show)

data Wildcard a = Wildcard a [a] a
                | Literal a
    deriving (Show,Eq,Ord)

-- Only return the first (all patterns left-most) valid star matching
wildcard :: Eq a => Wildcard [a] -> [a] -> Maybe [[a]]
wildcard (Literal mid) x = if mid == x then Just [] else Nothing
wildcard (Wildcard pre mid post) x = do
    y <- stripPrefix pre x
    z <- if null post then Just y else stripSuffix post y
    stripInfixes mid z
    where
        stripInfixes [] y = Just [y]
        stripInfixes (m:ms) y = do
            (a,z) <- stripInfix m y
            (a:) <$> stripInfixes ms z


data Pat = Skip -- ^ /**/
         | Stars (Wildcard String) -- ^ *foo*, prefix (fixed), infix floaters, suffix
                          -- e.g. *foo*bar = Stars "" ["foo"] "bar"
            deriving (Show,Eq,Ord)

star :: Pat
star = Stars $ Wildcard "" [] ""

lit :: String -> Pat
lit = Stars . Literal

fromLit :: Pat -> Maybe String
fromLit (Stars (Literal x)) = Just x
fromLit _ = Nothing
