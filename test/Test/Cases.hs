
module Test.Cases(testCases) where

import Test.Util
import System.FilePath((</>))
import System.Info.Extra


testCases :: IO ()
testCases = testMatch >> testSimple >> testArity >> testSubstitute


testSimple :: IO ()
testSimple = do
    simple "a*b" False
    simple "a//b" True
    simple "a/**/b" False
    simple "/a/b/cccc_" True
    simple "a///b" True
    simple "a/**/b" False


testArity :: IO ()
testArity = do
    arity "" 0
    arity "foo/**/*" 2
    arity "//*a.txt" 1
    arity "foo//a*.txt" 1
    arity "**/*a.txt" 2
    arity "foo/**/a*.txt" 2
    arity "//*a.txt" 1
    arity "foo//a*.*txt" 2
    arity "foo/**/a*.*txt" 3


testSubstitute :: IO ()
testSubstitute = do
    substitute "**/*a*.txt" ["","test","da"] "testada.txt"
    substitute "**/*a.txt" ["foo/bar","test"] "foo/bar/testa.txt"
    -- error if the number of replacements is wrong
    substituteErr "nothing" ["test"] ["substitute","nothing","expects 0","got 1","test"]
    substituteErr "*/*" ["test"] ["substitute","*/*","expects 2","got 1","test"]


testMatch :: IO ()
testMatch = do
    matchN "//*.c" "foo/bar/baz.c"
    matchY "**/*.c" "foo/bar/baz.c" ["foo/bar","baz"]
    matchY ("**" </> "*.c") ("foo/bar" </> "baz.c") ["foo/bar","baz"]
    matchY "*.c" "baz.c" ["baz"]
    matchN "//*.c" "baz.c"
    matchY "**/*.c" "baz.c" ["","baz"]
    matchY "**/*a.txt" "foo/bar/testa.txt" ["foo/bar","test"]
    matchN "**/*.c" "baz.txt"
    matchY "**/*a.txt" "testa.txt" ["","test"]
    matchY "**/a.txt" "a.txt" [""]
    matchY "a/**/b" "a/b" [""]
    matchY "a/**/b" "a/x/b" ["x"]
    matchY "a/**/b" "a/x/y/b" ["x/y"]
    matchY "a/**/**/b" "a/x/y/b" ["","x/y"]
    matchY "**/*a*.txt" "testada.txt" ["","test","da"]
    matchY "test.c" "test.c" []
    matchN "*.c" "foor/bar.c"
    matchN "*/*.c" "foo/bar/baz.c"
    matchN "foo//bar" "foobar"
    matchN "foo/**/bar" "foobar"
    matchN "foo//bar" "foobar/bar"
    matchN "foo/**/bar" "foobar/bar"
    matchN "foo//bar" "foo/foobar"
    matchN "foo/**/bar" "foo/foobar"
    matchN "foo//bar" "foo/bar"
    matchY "foo/**/bar" "foo/bar" [""]
    matchY "foo/bar" ("foo" </> "bar") []
    matchY ("foo" </> "bar") "foo/bar" []
    matchY ("foo" </> "bar") ("foo" </> "bar") []
    matchY "**/*.c" ("bar" </> "baz" </> "foo.c") ["bar/baz","foo"]
    matchY "**/*" "/bar" ["/","bar"]
    matchN "/bob//foo" "/bob/this/test/foo"
    matchY "/bob/**/foo" "/bob/this/test/foo" ["this/test"]
    matchN "/bob//foo" "bob/this/test/foo"
    matchN "/bob/**/foo" "bob/this/test/foo"
    matchN "bob//foo/" "bob/this/test/foo/"
    matchY "bob/**/foo/" "bob/this/test/foo/" ["this/test"]
    matchY "bob/**/foo/" "bob/foo/" [""]
    matchY "bob/**/foo/" "bob//foo/" ["/"]
    matchN "bob//foo/" "bob/this/test/foo"
    matchN "bob/**/foo/" "bob/this/test/foo"
    matchY ("**" </> "*a*.txt") "testada.txt" ["","test","da"]
    matchN "a//" "a"
    matchY "a/**" "a" [""]
    matchY "a/**" "a/" ["/"]
    matchN "/a//" "/a"
    matchY "a/**" "a" [""]
    matchY "/a/**" "/a" [""]
    matchN "///a//" "/a"
    matchY "**/a/**" "/a" ["/",""]
    matchN "///" ""
    matchY "/**" "/" ["/"]
    matchY "**/" "a/" ["a"]
    matchY "**/**" "" ["","/"]
    matchY "x/**/y" "x/y" [""]
    matchY "x/**/" "x/" [""]
    matchY "x/**/" "x/foo/" ["foo"]
    matchN "x///" "x"
    matchN "x/**/" "x"
    matchY "x/**/" "x/foo/bar/" ["foo/bar"]
    matchN "x///" "x/foo/bar"
    matchN "x/**/" "x/foo/bar"
    matchY "x/**/*/y" "x/z/y" ["","z"]
    matchY "" "" []
    matchN "" "y"
    matchN "" "/"

    matchY "*/*" "x/y" ["x","y"]
    matchN "*/*" "x"
    matchY "**/*" "x" ["","x"]
    matchY "**/*" "" ["",""]
    matchY "*/**" "x" ["x",""]
    matchY "*/**/*" "x/y" ["x","","y"]
    matchN "*//*" ""
    matchN "*/**/*" ""
    matchN "*//*" "x"
    matchN "*/**/*" "x"
    matchN "*//*//*" "x/y"
    matchN "*/**/*/**/*" "x/y"
    matchY "**/*/" "/" ["",""]
    matchY "*/**/**/" "/" ["","",""]
    matchN "b*b*b*//" "bb"
    matchN "b*b*b*/**" "bb"

    matchY "**" "/" ["//"] -- UGLY corner case
    matchY "**/x" "/x" ["/"]
    matchY "**" "x/" ["x/"]
    let s = if isWindows then '/' else '\\'
    matchY "**" "\\\\drive" [s:s:"drive"]
    matchY "**" "C:\\drive" ["C:"++s:"drive"]
    matchY "**" "C:drive" ["C:drive"]

    matchN "./file" "file"
    matchN "/file" "file"
    matchN "foo/./bar" "foo/bar"
    matchY "foo/./bar" "foo/./bar" []
    matchN "foo/./bar" "foo/bar"


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
