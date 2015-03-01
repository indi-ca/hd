module Buffer (
      Buffer(..),
      insert, delete, clearBuffer,
    ) where

import UI.NCurses
import Configuration


base_position = (0, 2)
moveCursor' x = moveCursor (fst base_position) (snd base_position + x)


data Buffer = Buffer {
    context :: (Integer, Integer),
    config :: Configuration,
    contents :: String,
    cursorPos :: Integer
} deriving Show


--TODO: How do I create a new Buffer, but just modify one item?
--TODO: Ofcourse, both insert and delete fail the cursor is not at the end
--I would have to pass in the contextual position of the buffer

insert :: Buffer -> Char -> (Buffer, Update ())
insert b c = (b', (moveCursor' newPos) >> (drawString [c]))
    where b' = b { contents = newContents, cursorPos = newPos}
          newPos = cursorPos b + 1
          newContents = contents b ++ [c]

emptyBuffer :: Buffer -> Bool
emptyBuffer b = contents b == ""

delete :: Buffer -> (Buffer, Update ())
delete b = if emptyBuffer b then (b, return ())
                            else (b', (moveCursor' (cursorPos b)) >> (drawString " "))
    where b' = b { contents = newContents, cursorPos = newPos}
          newPos = cursorPos b - 1
          newContents = (init $ contents b)

clearBuffer :: Buffer -> (Buffer, Update ())
clearBuffer b = if emptyBuffer b then (b, return ())
                                 else (b', (moveCursor' 0) >> (drawString empties))
    where b' = b { contents = "", cursorPos = 0}
          --TODO: Note the hack, because I'm not counting right
          empties = (replicate . length . contents) b ' '



--setOne :: Buffer -> Update ()
--setOne b = setColor ( (head . colours . config) b )

--setTwo :: Buffer -> Update ()
--setTwo b = setColor ( (last . colours . config) b )

--complete :: Buffer -> String -> (Buffer, Update ())
--complete b str = (b, (moveCursor' 0) >> (setTwo b >> (drawString str) >> setOne b  )  )


