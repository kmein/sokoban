import Test.HUnit
import Test.QuickCheck

sokoban =
    TestList $
    map TestCase
        [ assertEqual 1 1
        ]

prop_trivial c = c == c

main =
    do runTestTT sokoban
       quickCheck prop_trivial
