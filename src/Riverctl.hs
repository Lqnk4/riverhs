{-# LANGUAGE GADTs #-}

-- | Keymap setup
module Riverctl where

import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import System.Process (createProcess, proc)
import Prelude hiding (Left, Right)

-- from left to right,
-- Mode, mods, lhs, rhs
type Keymap = ([Mode], String, String, [String])

data Mode = N | L
data Modkey = M | S | A | C | None deriving (Read)
data View = Next | Prev
data Direction = Left | Down | Up | Right
data Split = Horizontal | Vertical

instance Show Mode where
    show N = "normal"
    show L = "locked"

instance Show Modkey where
    show M = "Super"
    show S = "Shift"
    show A = "Alt"
    show C = "Control"
    show None = "None"

instance Show View where
    show Next = "next"
    show Prev = "previous"

instance Show Direction where
    show Left = "left"
    show Down = "down"
    show Up = "up"
    show Right = "right"

instance Show Split where
    show Horizontal = "horizontal"
    show Vertical = "vertical"

-- | start a program
spawn :: String -> [String]
spawn program = ["spawn", program]

-- | exit river
exit :: [String]
exit = ["exit"]

-- close a window
close :: [String]
close = ["close"]

-- | focus the next/previous view in the layout stack
focusView :: View -> [String]
focusView view = ["focus-view", show view]

-- | swap the focused view with the next/previous view in the layout stack
swap :: View -> [String]
swap view = ["swap", show view]

-- | focus the next/previous output
focusOutput :: View -> [String]
focusOutput view = ["focus-output", show view]

-- | send the focused view to the next/previous output
sendtoOutput :: View -> [String]
sendtoOutput view = ["send-to-output", show view]

-- | bump the focused view to the top of the layout stack
zoom :: [String]
zoom = ["zoom"]

-- | send a layout command to rivertile(1)
sendLayoutCmd :: String -> [String]
sendLayoutCmd cmd = ["send-layout-cmd", "rivertile", cmd]

{- | set the main ratio of rivertile(1)
prefix with +/- to increment/decrement
-}
mainRatio :: String -> [String]
mainRatio s = sendLayoutCmd $ "main-ratio " ++ s

-- | increment/decrement the main count of rivertile(1)
mainCount :: String -> [String]
mainCount s = sendLayoutCmd $ "main-count " ++ s

-- | move views
move :: Direction -> Int -> [String]
move dir n = ["move", show dir, show n]

-- | snap views to screen edges
snap :: Direction -> [String]
snap dir = ["snap", show dir]

-- | resize views
resize :: Split -> Int -> [String]
resize sp n = ["resize", show sp, show n]

-- | move views with pointer
ptrMoveView :: [String]
ptrMoveView = ["move-view"]

-- | resize views with pointer
ptrResizeView :: [String]
ptrResizeView = ["resize-view"]

-- | toggle float with pointer
ptrToggleFloat :: [String]
ptrToggleFloat = ["toggle-float"]

-- | call riverctl with the provided args
callctl :: [String] -> IO ()
callctl args = void $ createProcess (proc "riverctl" args)

-- | call rivertile with the provided args
callRiverTile :: [String] -> IO ()
callRiverTile args = void $ createProcess (proc "rivertile" args)

-- | call an external process
callExternal :: String -> [String] -> IO ()
callExternal p args = void $ createProcess (proc p args)

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : splitWhen p s''
      where
        (w, s'') = break p s'

parseMods :: String -> String
parseMods = intercalate "+" . map show . modlist
  where
    modlist :: String -> [Modkey]
    modlist = map read . splitWhen (== '-')

-- | calls riverctl to set a keymap
setKeymap :: Keymap -> IO ()
setKeymap ([], _, _, _) = return ()
setKeymap (mode : modes, modkeys, lhs, rhs) = do
    callctl $ "map" : show mode : parseMods modkeys : lhs : rhs
    setKeymap (modes, modkeys, lhs, rhs)

-- | calls riverctl to set a keymap pointer
setPtrMap :: Keymap -> IO ()
setPtrMap ([], _, _, _) = return ()
setPtrMap (mode : modes, modkeys, lhs, rhs) = do
    callctl $ "map-pointer" : show mode : parseMods modkeys : lhs : rhs
    setKeymap (modes, modkeys, lhs, rhs)

setKeymaps :: [Keymap] -> IO ()
setKeymaps = traverse_ setKeymap

setPtrMaps :: [Keymap] -> IO ()
setPtrMaps = traverse_ setPtrMap
