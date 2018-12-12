
-- | The type of patterns and wildcards
module System.FilePattern.Core2(
    FilePattern,
    Pattern(..), parsePattern,
    Path(..), parsePath,
    Part(..), match
    ) where

import System.FilePattern.Wildcard
import System.FilePath (isPathSeparator)
import Data.Either.Extra
import Data.List.Extra



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


data Part = Part String | Parts [String]

match :: Pattern -> Path -> Maybe [Part]
match (Pattern w) (Path x) = f <$> wildcard (wildcard equals) w x
    where
        f :: [Either [[Either [()] String]] [String]] -> [Part]
        f (Left x:xs) = map Part (rights $ concat x) ++ f xs
        f (Right x:xs) = Parts x : f xs
        f [] = []
