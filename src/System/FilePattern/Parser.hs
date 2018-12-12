{-# LANGUAGE PatternGuards #-}

-- | Parsers for the flavours of 'FilePattern'.
module System.FilePattern.Parser(
    parse
    ) where

import Data.List.Extra
import Data.Maybe
import Prelude
import System.FilePattern.Type
import System.FilePattern.Wildcard
import System.FilePattern.Core2
import System.FilePath (isPathSeparator)


---------------------------------------------------------------------
-- PATTERNS

data Lexeme = Str String | Slash | SlashSlash


-- | Parse a FilePattern with a given lexer. All optimisations I can think of are invalid because they change the extracted expressions.
parseLexeme :: [Lexeme] -> [Pat]
parseLexeme = f False True
    where
        -- str = I have ever seen a Str go past (equivalent to "can I be satisfied by no paths")
        -- slash = I am either at the start, or my previous character was Slash
        f _ slash [] = [lit "" | slash]
        f _ _ (Str "**":xs) = Skip : f True False xs
        f str slash (Str ".":Slash:xs) = f str slash xs
        f str slash (Str ".":xs) = f str slash xs
        f _ _ (Str x:xs) = parseLit x : f True False xs
        f str _ (SlashSlash:Slash:xs) | not str = star : Skip : f str True xs
        f str _ (SlashSlash:xs) = Skip : f str False xs
        f str _ (Slash:xs) = [lit "" | not str] ++ f str True xs


parseLit :: String -> Pat
parseLit x = case split (== '*') x of
    [] -> error "parseLit: given empty string"
    [y] -> lit y
    pre:xs -> case unsnoc xs of
        Nothing -> error "parseLit: Stars check failed"
        Just (mid,post) -> Stars $ Wildcard pre mid post


parse :: FilePattern -> Pats
parse = mkPats . parseLexeme . f
    where
        f "" = []
        f (x1:x2:xs) | isPathSeparator x1, isPathSeparator x2 = f (x2:xs)
        f (x1:xs) | isPathSeparator x1 = Slash : f xs
        f xs = Str a : f b
            where (a,b) = break isPathSeparator xs
