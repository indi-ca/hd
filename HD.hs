#!/usr/bin/env runhaskell

module Main where

import System.IO (Handle)
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter, close)
import System.Log.Formatter
import UI.NCurses

import Buffer
import Proc
import Palette
import Configuration

-- The State
data State = State {
    buffer :: Buffer
} deriving Show


logPath = "/Users/indika/logs/hd/hd.log"


main :: IO ()
main = do
    f <- fileHandler logPath DEBUG
    let myFileHandler' = withFormatter f
    updateGlobalLogger "HD" (addHandler f)
    updateGlobalLogger "HD" (setLevel DEBUG)
    --debugM "HD" "This is Haskell Development"

    c <- runCurses getPalette
    let palette = Palette c
        config = Configuration palette
        initial_buffer = Buffer (0, 2) config "" 0
        initialState = State initial_buffer

    mainLoop initialState initialRender

    close f

withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
    -- http://hackage.haskell.org/packages/archive/hslogger/1.1.4/doc/html/System-Log-Formatter.html
    where formatter = simpleLogFormatter "[$time $loggername $prio] $msg"




exitFilter :: Event -> Bool
exitFilter (EventCharacter 'q') = True
exitFilter _ = False

mainLoop :: State -> Update () -> IO ()
mainLoop s u = do
    e <- runCurses $ renderState u
    let (s', u, y) = updateState s e
    y -- any IO that is necessary
    if exitFilter e then return () else mainLoop s' u



-- I can specify a timeout
getInput :: Window -> Curses Event
getInput w = do
    ev <- getEvent w Nothing
    case ev of Nothing -> getInput w
               Just ev' -> return ev'

renderState :: Update () -> Curses Event
renderState u = do
    setEcho False
    w <- defaultWindow
    updateWindow w $ u
    render
    e <- getInput w
    return e



initialRender :: Update ()
initialRender = do
    moveCursor 0 0
    drawString "» "


-- executing the contents of the buffer
execute :: State -> (State, Update (), IO ())
execute s = do
    let buffer_contents = contents . buffer $ s
        (b', u') = clearBuffer (buffer s)
        s' = State b'
        io = do
            (diff, result) <- performTask motor releaseCommand
            case result of Nothing -> debugM "HD" "Failed"
                           Just (code, str1, str2) -> debugM "HD" str1
    (s', u', io)


updateState :: State -> Event -> (State, Update (), IO ())
updateState s (EventCharacter c)
    | c == '\n'     = execute s
    | c == '\DEL'   = (\x -> (State (fst x), (snd x), return ())) (delete (buffer s))
    | otherwise     = (\x -> (State (fst x), (snd x), return ())) (insert (buffer s) c)




