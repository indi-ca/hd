module Configuration (Configuration(..)) where

import UI.NCurses
import Palette (Palette)


data Configuration = Configuration {
    palette :: Palette
} deriving Show
