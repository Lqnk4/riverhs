{-# LANGUAGE GADTs #-}

-- | module containing riverctl configuration functions
module Riverctl where

import Control.Monad (void)
import Data.Bits (shiftL)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import System.Process (createProcess, proc, shell)
import Prelude hiding (Left, Right)

{- | from left to right,
Mode, mods, lhs, rhs
-}
type Keymap = ([Mode], String, String, [String])

type Rule = (RuleType, String, RuleAction)

data Mode = N | L | P
data Modkey = M | S | A | C | None deriving (Read)
data View = Next | Prev
data Direction = Left | Down | Up | Right
data Split = Horizontal | Vertical

data RuleType = AppID | Title
data RuleAction = Float | NoFloat | SSD | CSD | Tags Int | Output String

instance Show Mode where
    show N = "normal"
    show L = "locked"
    show P = "passthrough"

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

instance Show RuleType where
    show AppID = "-app-id"
    show Title = "-title"

instance Show RuleAction where
    show Float = "float"
    show NoFloat = "no-float"
    show SSD = "ssd"
    show CSD = "csd"
    show (Tags t) = "tags " ++ show t
    show (Output s) = "output " ++ s

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

-- | change layout orientation
mainLocation :: Direction -> [String]
mainLocation dir =
    sendLayoutCmd $
        (++) "main-location " $ case dir of
            Left -> "left"
            Right -> "right"
            Down -> "bottom"
            Up -> "top"

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

-- | toggle floating view
toggleFloat :: [String]
toggleFloat = ["toggle-float"]

toggleFullScreen :: [String]
toggleFullScreen = ["toggle-fullscreen"]

enterMode :: Mode -> [String]
enterMode mode = ["enter-mode", show mode]

-- | generates mappings for tags [0..8] bound to [1..9]
genTagMaps :: [Keymap]
genTagMaps =
    concat
        [ [ ([N], "M", show x, ["set-focused-tags", tags x])
          , ([N], "M-S", show x, ["set-view-tags", tags x])
          , ([N], "M-C", show x, ["toggle-focused-tags", tags x])
          , ([N], "M-S-C", show x, ["toggle-view-tags", tags x])
          ]
        | x <- [1 .. 9]
        ]
        ++ [([N], "M", "0", ["set-focused-tags", alltags]), ([N], "M-S", "0", ["set-view-tags", alltags])]
  where
    tags x = show $ (1 :: Int) `shiftL` (x - 1)
    alltags = show $ ((1 :: Int) `shiftL` 32) - 1

-- | call riverctl with the provided args
callctl :: [String] -> IO ()
callctl args = void $ createProcess (proc "riverctl" args)

-- | call an external process with args
callExternal :: String -> [String] -> IO ()
callExternal p args = void $ createProcess (proc p args)

-- | call an external shell command
callExternalShell :: String -> IO ()
callExternalShell cmd = void $ createProcess (shell cmd)

{- | declare modes in river ctl
To add a custom mode, first make a constructor
and then add an instance of show.
-}
declareCustomModes :: [Mode] -> IO ()
declareCustomModes = traverse_ declareMode
  where
    declareMode :: Mode -> IO ()
    declareMode mode = callctl ["declare-mode", show mode]

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
    setPtrMap (modes, modkeys, lhs, rhs)

-- | set keyboard repeat rate
setKeyRepeat :: Int -> Int -> IO ()
setKeyRepeat x y = callctl ["set-repeat", show x, show y]

{- | set background color in the form of a hex string
example: '0x002b36'
-}
setBackgroundColor :: String -> IO ()
setBackgroundColor c = callctl ["background-color", c]

-- | set border color in the form of a hex string
setBorderColorFocused :: String -> IO ()
setBorderColorFocused c = callctl ["border-color-focused", c]

-- | set border color in the form of a hex string
setBorderColorUnfocused :: String -> IO ()
setBorderColorUnfocused c = callctl ["border-color-unfocused", c]

-- | set a window rule
addRule :: Rule -> IO ()
addRule (ruleType, glob, ruleAction) = callctl $ ["rule-add", show ruleType, glob] ++ actionToArgs ruleAction
  where
    actionToArgs = words . show
