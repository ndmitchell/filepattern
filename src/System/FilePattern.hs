
-- | A module for pattern matching on file names.
--
-- >>> "/**/*.png" ?== "/foo/bar/baz.png"
-- True

module System.FilePattern(
    -- * Primitive API
    FilePattern, (?==), match,
    -- * Optimisation opportunities
    simple, arity,
    -- * Multipattern file rules
    substitute,
    -- * Accelerated searching
    Step(..), step
    ) where

import Control.Exception.Extra
import Data.Maybe
import Data.List.Extra
import System.FilePattern.Core(FilePattern, parsePattern, parsePath, renderPath)
import qualified System.FilePattern.Core as Core
import System.FilePattern.Step
import Prelude


---------------------------------------------------------------------
-- PATTERNS

-- | Match a 'FilePattern' against a 'FilePath', There are three special forms:
--
-- * @*@ matches an entire path component, excluding any separators.
--
-- * @**@ as a path component matches an arbitrary number of path components, but not
--   absolute path prefixes.
--
--   Some examples:
--
-- * @test.c@ matches @test.c@ and nothing else.
--
-- * @*.c@ matches all @.c@ files in the current directory, so @file.c@ matches,
--   but @file.h@ and @dir\/file.c@ don't.
--
-- * @**/*.c@ matches all @.c@ files anywhere on the filesystem,
--   so @file.c@, @dir\/file.c@, @dir1\/dir2\/file.c@ and @\/path\/to\/file.c@ all match,
--   but @file.h@ and @dir\/file.h@ don't.
--
-- * @dir\/*\/*@ matches all files one level below @dir@, so @dir\/one\/file.c@ and
--   @dir\/two\/file.h@ match, but @file.c@, @one\/dir\/file.c@, @dir\/file.h@
--   and @dir\/one\/two\/file.c@ don't.
--
--   Patterns with constructs such as @foo\/..\/bar@ will never match
--   normalised 'FilePath' values, so are unlikely to be correct.
(?==) :: FilePattern -> FilePath -> Bool
(?==) w = isJust . Core.match (parsePattern w) . parsePath


-- | Like '?==', but returns 'Nothing' on if there is no match, otherwise 'Just' with the list
--   of fragments matching each wildcard. For example:
--
-- @
-- isJust ('match' p x) == (p '?==' x)
-- 'match' \"**\/*.c\" \"test.txt\" == Nothing
-- 'match' \"**\/*.c\" \"foo.c\" == Just [\"",\"foo\"]
-- 'match' \"**\/*.c\" \"bar\/baz\/foo.c\" == Just [\"bar\/baz/\",\"foo\"]
-- @
--
--   Note that the @**@ will often contain a trailing @\/@, and even on Windows any
--   @\\@ separators will be replaced by @\/@.
match :: FilePattern -> FilePath -> Maybe [String]
match w = Core.match (parsePattern w) . parsePath


---------------------------------------------------------------------
-- MULTIPATTERN COMPATIBLE SUBSTITUTIONS

-- | Is the pattern free from any @*@ and @**@.
simple :: FilePattern -> Bool
simple w = Core.arity (parsePattern w) == 0

-- | How many @*@ and @**@ elements are there.
arity :: FilePattern -> Int
arity = Core.arity . parsePattern


-- | Given a successful 'match', substitute it back in to a pattern with the same 'arity'.
--   Raises an error if there are not exactly the right number of substitutions,
--   indicating the patterns had different arity.
--
-- @
-- p '?==' x ==> 'substitute' (fromJust $ 'match' p x) p == x
-- @
substitute :: Partial => FilePattern -> [String] -> FilePath
substitute w xs = maybe (error msg) renderPath $ Core.substitute (parsePattern w) xs
    where
        msg = "Failed substitute, patterns of different arity. Pattern " ++ show w ++
              " expects " ++ show (arity w) ++ " elements, but got " ++ show xs ++
              " which has " ++ show (length xs) ++ "."
