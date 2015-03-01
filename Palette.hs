module Palette (getPalette, Palette(..) ) where

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
getPalette = sequence [newColorID ColorBlue ColorDefault 1, newColorID ColorBlue ColorDefault 2]

