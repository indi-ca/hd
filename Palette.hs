module Palette (
    getPalette, Palette(..),
    acColor, normalColor
    ) where

import UI.NCurses


--ColorBlack
--ColorRed
--ColorGreen
--ColorYellow
--ColorBlue
--ColorMagenta
--ColorCyan
--ColorWhite
--ColorDefault


data Palette = Palette {
    colors :: [ColorID]
} deriving Show


-- TODO: Change this so that it returns a recipie to make the type Palette
getPalette :: Curses [ColorID]
getPalette = sequence [newColorID ColorDefault ColorDefault 1, newColorID ColorBlue ColorDefault 2]


normalColor :: Palette -> ColorID
normalColor p = (colors p) !! 0

acColor :: Palette -> ColorID
acColor p = (colors p) !! 1
