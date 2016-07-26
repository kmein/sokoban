module Main where

import qualified Game.Sokoban.Console as Console (main)
import qualified Game.Sokoban.Gtk as Gtk (main)

main :: IO ()
main = Gtk.main
