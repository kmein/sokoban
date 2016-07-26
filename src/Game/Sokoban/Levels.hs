module Game.Sokoban.Levels where

testLevel :: String
testLevel =
    unlines
    [ "#####"
    , "#.o@#"
    , "#####"
    ]

level :: Int -> String
level 1 =
    unlines
    [ "    #####          "
    , "    #   #          "
    , "    #o  #          "
    , "  ###  o##         "
    , "  #  o o #         "
    , "### # ## #   ######"
    , "#   # ## #####  ..#"
    , "# o  o          ..#"
    , "##### ### #@##  ..#"
    , "    #     #########"
    , "    #######        "
    ]


