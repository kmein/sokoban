module Main where

import qualified Game.Sokoban.Console as Console (mainWith)
import qualified Game.Sokoban.Gtk as Gtk (mainWith)

import Options.Applicative

main :: IO ()
main =
    do level <- execParser $
           info (helper <*> sokobanOptions)
               (fullDesc <> progDesc "Play a GTK clone of Sokoban")
       Gtk.mainWith level
    where sokobanOptions = argument auto (metavar "N" <> help "Select level N")
