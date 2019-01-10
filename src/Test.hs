
module Main(main) where

import Control.Exception.Extra
import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe
import System.FilePattern as FilePattern
import qualified System.FilePattern.Core as Core
import System.FilePath(isPathSeparator, (</>))
import System.IO.Unsafe
import System.Info.Extra
import Test.QuickCheck


---------------------------------------------------------------------
-- TEST UTILITIES

assertBool :: Bool -> String -> [String] -> IO ()
assertBool b msg fields = unless b $ error $ unlines $
    ("ASSERTION FAILED: " ++ msg) : fields

assertException :: IO () -> [String] -> String -> [String] -> IO ()
assertException a parts msg fields = do
    res <- try_ a
    case res of
        Left e -> assertBool (all (`isInfixOf` show e) parts) msg $ ["Expected" #= parts, "Got" #= e] ++ fields
        Right _ -> assertBool False msg $ ["Expected" #= parts, "Got" #= "<No exception>"] ++ fields

(#=) :: Show a => String -> a -> String
(#=) a b = a ++ ": " ++ show b


newtype ArbPattern = ArbPattern FilePattern deriving (Show,Eq)
newtype ArbPath    = ArbPath    FilePath    deriving (Show,Eq)

-- Since / and * are the only "interesting" elements, just add ab to round out the set

instance Arbitrary ArbPattern where
    arbitrary = fmap ArbPattern $ listOf $ elements "\\/*ab."
    shrink (ArbPattern x) = map ArbPattern $ shrinkList (\x -> ['/' | x == '\\']) x

instance Arbitrary ArbPath where
    arbitrary = fmap ArbPath $ listOf $ elements "\\/ab."
    shrink (ArbPath x) = map ArbPath $ shrinkList (\x -> ['/' | x == '\\']) x


{-
-- | Write 'matchBool' in terms of 'walker'.
walkerMatch :: Switch -> FilePattern -> FilePath -> Bool
walkerMatch Switch{..} a b = if null b2 then empty else maybe False (f b2) w
    where
        b2 = filter (/= ".") $ split isPathSeparator b
        (empty, w) = walk [a]

        f [x]    (Walk op) = fst $ op x
        f [x]    (WalkTo (file, dir)) = x `elem` file
        f (x:xs) (Walk op) = maybe False (f xs) $ snd $ op x
        f (x:xs) (WalkTo (file, dir)) | Just w <- lookup x dir = f xs w
        f _ _ = False

showWalk :: Walk -> String
showWalk (Walk _) = "Walk _"
showWalk (WalkTo (p,xs)) = "WalkTo " ++ pair (show p) (list [pair (show a) (showWalk b) | (a,b) <- xs])
    where
        pair a b = "(" ++ a ++ "," ++ b ++ ")"
        list xs = "[" ++ intercalate "," xs ++ "]"
-}

---------------------------------------------------------------------
-- DRIVER

main :: IO ()
main = do
    let dot x = putStr "." >> x
    putStr "Testing "
    dot testSimple
    dot testArity
    dot testSubstitute
    dot testMatch
    -- when False $ dot $ testWalk s
    putStr " "
    testProperties []


testSimple :: IO ()
testSimple = do
    let x # y = assertBool (res == y) "simple" ["Input" #= x, "Expected" #= y, "Got" #= res]
            where res = simple x
    "a*b" # False
    "a//b" # True
    "a/**/b" # False
    "/a/b/cccc_" # True
    "a///b" # True
    "a/**/b" # False


testArity :: IO ()
testArity = do
    let x # y = assertBool (res == y) "arity" ["Input" #= x, "Expected" #= y, "Got" #= res]
            where res = arity x
    "" # 0
    "foo/**/*" # 2
    "//*a.txt" # 1
    "foo//a*.txt" # 1
    "**/*a.txt" # 2
    "foo/**/a*.txt" # 2
    "//*a.txt" # 1
    "foo//a*.*txt" # 2
    "foo/**/a*.*txt" # 3


testSubstitute :: IO ()
testSubstitute = do
    let f a b c = assertBool (res == c) "substitute" ["Parts" #= a, "Pattern" #= b, "Expected" #= c, "Got" #= res]
            where res = substitute b a
    f ["","test","da"] "**/*a*.txt" "testada.txt"
    f ["foo/bar","test"] "**/*a.txt" "foo/bar/testa.txt"
    let deep = void . evaluate . length . show
    -- error if the number of replacements is wrong
    -- assertException (deep $ substitute ["test"] "nothing") ["substitute","wanted 0","got 1","test","nothing"] "substitute" []
    -- assertException (deep $ substitute ["test"] "*/*") ["substitute","wanted 2","got 1","test","*/*"] "substitute" []
    assertException (deep $ substitute "nothing" ["test"]) ["substitute"] "substitute" []


testMatch :: IO ()
testMatch = do
    let f a b c = assertBool (res == c) "match" ["Pattern" #= a, "File" #= b, "Expected" #= c, "Got" #= res]
            where res = Core.match (Core.parsePattern a) (Core.parsePath b)
    let yes a b c = f a b $ Just c
    let no a b = f a b Nothing

    no "//*.c" "foo/bar/baz.c"
    --yes "//*.c" "/baz.c" ["baz"]
    yes "**/*.c" "foo/bar/baz.c" ["foo/bar","baz"]
    yes ("**" </> "*.c") ("foo/bar" </> "baz.c") ["foo/bar","baz"]
    yes "*.c" "baz.c" ["baz"]
    no "//*.c" "baz.c"
    yes "**/*.c" "baz.c" ["","baz"]
    yes "**/*a.txt" "foo/bar/testa.txt" ["foo/bar","test"]
    no "**/*.c" "baz.txt"
    yes "**/*a.txt" "testa.txt" ["","test"]
    yes "**/a.txt" "a.txt" [""]
    yes "a/**/b" "a/b" [""]
    yes "a/**/b" "a/x/b" ["x"]
    yes "a/**/b" "a/x/y/b" ["x/y"]
    yes "a/**/**/b" "a/x/y/b" ["","x/y"]
    yes "**/*a*.txt" "testada.txt" ["","test","da"]
    yes "test.c" "test.c" []
    no "*.c" "foor/bar.c"
    no "*/*.c" "foo/bar/baz.c"
    no "foo//bar" "foobar"
    no "foo/**/bar" "foobar"
    no "foo//bar" "foobar/bar"
    no "foo/**/bar" "foobar/bar"
    no "foo//bar" "foo/foobar"
    no "foo/**/bar" "foo/foobar"
    -- yes "foo//bar" "foo/bar" []
    yes "foo/**/bar" "foo/bar" [""]
    yes "foo/bar" ("foo" </> "bar") []
    yes ("foo" </> "bar") "foo/bar" []
    yes ("foo" </> "bar") ("foo" </> "bar") []
    yes "**/*.c" ("bar" </> "baz" </> "foo.c") ["bar/baz","foo"]
    -- yes "//*" "/bar" ["bar"]
    yes "**/*" "/bar" ["/","bar"]
    no "/bob//foo" "/bob/this/test/foo"
    -- yes "/bob//foo" "/bob/foo" []
    yes "/bob/**/foo" "/bob/this/test/foo" ["this/test"]
    no "/bob//foo" "bob/this/test/foo"
    no "/bob/**/foo" "bob/this/test/foo"
    no "bob//foo/" "bob/this/test/foo/"
    -- yes "bob//foo/" "bob/foo/" []
    yes "bob/**/foo/" "bob/this/test/foo/" ["this/test"]
    yes "bob/**/foo/" "bob/foo/" [""]
    yes "bob/**/foo/" "bob//foo/" ["/"]
    no "bob//foo/" "bob/this/test/foo"
    no "bob/**/foo/" "bob/this/test/foo"
    yes ("**" </> "*a*.txt") "testada.txt" ["","test","da"]
    no "a//" "a"
    yes "a/**" "a" [""]
    -- yes "a//" "a/" []
    no "/a//" "/a"
    yes "a/**" "a" [""]
    -- yes "/a//" "/a/" []
    yes "/a/**" "/a" [""]
    no "///a//" "/a"
    -- yes "///a//" "/a/" []
    yes "**/a/**" "/a" ["/",""]
    no "///" ""
    -- yes "///" "/" []
    yes "/**" "/" ["/"]
    yes "**/" "a/" ["a"]
    -- yes "////" "/" []
    -- yes "**/**" "" ["","/"]
    yes "x/**/y" "x/y" [""]
    -- yes "x///" "x/" []
    yes "x/**/" "x/" [""]
    yes "x/**/" "x/foo/" ["foo"]
    no "x///" "x"
    no "x/**/" "x"
    yes "x/**/" "x/foo/bar/" ["foo/bar"]
    no "x///" "x/foo/bar"
    no "x/**/" "x/foo/bar"
    -- yes "x///y" "x/y" []
    yes "x/**/*/y" "x/z/y" ["","z"]
    yes "" "" []
    no "" "y"
    no "" "/"

    yes "*/*" "x/y" ["x","y"]
    no "*/*" "x"
    -- yes "//*" "/x" ["x"]
    yes "**/*" "x" ["","x"]
    -- yes "//*" "/" [""]
    -- yes "**/*" "" ["",""]
    -- yes "*//" "x/" ["x"]
    yes "*/**" "x" ["x",""]
    -- yes "*//" "/" [""]
    -- yes "*//*" "x/y" ["x","y"]
    yes "*/**/*" "x/y" ["x","","y"]
    no "*//*" ""
    no "*/**/*" ""
    no "*//*" "x"
    no "*/**/*" "x"
    no "*//*//*" "x/y"
    no "*/**/*/**/*" "x/y"
    -- yes "//*/" "//" [""]
    yes "**/*/" "/" ["",""]
    -- yes "*/////" "/" [""]
    yes "*/**/**/" "/" ["","",""]
    no "b*b*b*//" "bb"
    no "b*b*b*/**" "bb"

    yes "**" "/" ["//"] -- UGLY corner case
    yes "**/x" "/x" ["/"]
    yes "**" "x/" ["x/"]
    let s = if isWindows then '/' else '\\'
    yes "**" "\\\\drive" [s:s:"drive"]
    yes "**" "C:\\drive" ["C:"++s:"drive"]
    yes "**" "C:drive" ["C:drive"]

    -- We support ignoring '.' values in FilePath as they are inserted by @filepath@ a lot
    -- yes "./file" "file" []
    no "/file" "file"
    -- yes "foo/./bar" "foo/bar" []
    yes "foo/./bar" "foo/./bar" []
    -- yes "foo/./bar" "foo/bar" []

{-
testWalk :: Switch -> IO ()
testWalk Switch{..} = do
    let shw (a, b) = "(" ++ show a ++ "," ++ maybe "Nothing" ((++) "Just " . showWalk) b ++ ")"
    let both p w = assertBool (shw res == shw w) "walk" ["Pattern" #= p, "Expected" #= shw w, "Got" #= shw res]
            where res = walk p
    let walk_ = Walk undefined

    both ["*.xml"] (False, Just walk_)
    both ["//*.xml"] (False, Just $ WalkTo ([], [("",walk_)]))
    both ["**/*.xml"] (False, Just walk_)
    both ["foo//*.xml"] (False, Just $ WalkTo ([], [("foo",walk_)]))
    both ["foo/**/*.xml"] (False, Just $ WalkTo ([], [("foo",walk_)]))
    both ["foo/bar/*.xml"] (False, Just $ WalkTo ([], [("foo",WalkTo ([],[("bar",walk_)]))]))
    both ["a","b/c"] (False, Just $ WalkTo (["a"],[("b",WalkTo (["c"],[]))]))
    let (False, Just (Walk f)) = walk ["*/bar/*.xml"]
        shw2 (b, mw) = (b, maybe "" showWalk mw)
    assertBool (shw2 (f "foo") == shw2 (False, Just $ WalkTo ([],[("bar",walk_)]))) "walk inner" []
    both ["bar/*.xml","baz//*.c"] (False, Just $ WalkTo ([],[("bar",walk_),("baz",walk_)]))
    both ["bar/*.xml","baz/**/*.c"] (False, Just $ WalkTo ([],[("bar",walk_),("baz",walk_)]))
    both [] (False, Nothing)
    both [""] (True, Just $ WalkTo ([""], []))
    both ["//"] (False, Just $ WalkTo ([], [("",WalkTo ([""],[]))]))
    both ["**"] (True, Just walk_)
-}

testProperties :: [String] -> IO ()
testProperties xs = do
    forM_ xs $ \x -> forM_ xs $ \y -> prop x y
    Success{} <- quickCheckWithResult stdArgs{maxSuccess=10000} $ \(ArbPattern p) (ArbPath x) ->
        (if p ?== x then label "match" else property) $ unsafePerformIO $ prop p x >> return True
    return ()
    where
        prop :: FilePattern -> FilePath -> IO ()
        prop pat file = do
            let ppat = Core.parsePattern pat
            let pfile = Core.parsePath file
            whenJust (Core.match ppat pfile) $ \ps ->
                assertBool (Core.substitute ppat ps == Just pfile) "FAILED PROPERTY" ["Pattern" #= pat, "File" #= file]

            let b = pat ?== file
            let fields = ["Pattern" #= pat, "File" #= file, "?==" #= b]
            let res = FilePattern.match pat file in assertBool (b == isJust res) "match" $ fields ++ ["match" #= res]
            -- when False $ let res = walkerMatch switch pat file in assertBool (b == res) "walker" $ fields ++ ["walker" #= res]
            let norm = (\x -> if null x then [""] else x) . filter (/= ".") . split isPathSeparator
            when b $ let res = substitute pat (fromJust $ FilePattern.match pat file) in
                assertBool (norm res == norm file) "substitute" $ fields ++ ["Match" #= FilePattern.match pat file, "Got" #= res, "Input (norm)" #= norm file, "Got (norm)" #= norm res]
