module Buffer (
      Buffer(..),
      insert, delete, clearBuffer,
    ) where

import UI.NCurses


base_position = (0, 2)
moveCursor' x = moveCursor (fst base_position) (snd base_position + x)


-- How do I make config common?
--data Configuration = Configuration {
--    colours :: [ColorID]
--} deriving Show


data Buffer = Buffer {
    contents :: String,
    cursorPos :: Integer
} deriving Show


--TODO: How do I create a new Buffer, but just modify one item?
--TODO: Ofcourse, both insert and delete fail the cursor is not at the end
--I would have to pass in the contextual position of the buffer

insert :: Buffer -> Char -> (Buffer, Update ())
insert b c = (b', (moveCursor' newPos) >> (drawString [c]))
    where b' = Buffer (contents b ++ [c]) newPos
          newPos = cursorPos b + 1

emptyBuffer :: Buffer -> Bool
emptyBuffer b = contents b == ""

delete :: Buffer -> (Buffer, Update ())
delete b = if emptyBuffer b then (b, return ())
                            else (b', (moveCursor' (cursorPos b)) >> (drawString " "))
    where b' = Buffer (init $ contents b) (newPos)
          newPos = cursorPos b - 1

clearBuffer :: Buffer -> (Buffer, Update ())
clearBuffer b = if emptyBuffer b then (b, return ())
                                 else (b', (moveCursor' 0) >> (drawString empties))
    where b' = Buffer "" 0
          --TODO: Note the hack, because I'm not counting right
          empties = (replicate . length . contents) b ' '



--setOne :: Buffer -> Update ()
--setOne b = setColor ( (head . colours . config) b )

--setTwo :: Buffer -> Update ()
--setTwo b = setColor ( (last . colours . config) b )

--complete :: Buffer -> String -> (Buffer, Update ())
--complete b str = (b, (moveCursor' 0) >> (setTwo b >> (drawString str) >> setOne b  )  )


