module Main where

import Riverctl
import Prelude hiding (Left, Right)

term :: String
term = "alacritty"

browser :: String
browser = "zen-browser"

launcher :: String
launcher = "fuzzel"

fcitxToggle :: String
fcitxToggle = "fcitx5-remote -t ; pkill -SIGRTMIN+1 waybar"

keymaps :: [Keymap]
keymaps =
    [ ([N], "M-S", "E", exit)
    , ([N], "M-S", "Q", close)
    , -- spawn programs
      ([N], "M-S", "Return", spawn term)
    , ([N], "M", "G", spawn browser)
    , ([N], "M", "D", spawn launcher)
    , ([N], "C", "Space", spawn fcitxToggle)
    , -- navigate views
      ([N], "M", "Return", zoom)
    , ([N], "M", "J", focusView Next)
    , ([N], "M", "K", focusView Prev)
    , ([N], "M-S", "J", swap Next)
    , ([N], "M-S", "K", swap Prev)
    , ([N], "M", "Period", focusOutput Next)
    , ([N], "M", "Comma", focusOutput Prev)
    , ([N], "M-S", "Period", sendtoOutput Next)
    , ([N], "M-S", "Comma", sendtoOutput Prev)
    , -- change layout
      ([N], "M", "H", mainRatio "-0.05")
    , ([N], "M", "L", mainRatio "+0.05")
    , ([N], "M-S", "H", mainCount "+1")
    , ([N], "M-S", "L", mainCount "-1")
    , ([N], "M-A", "H", move Left 100)
    , ([N], "M-A", "J", move Down 100)
    , ([N], "M-A", "K", move Up 100)
    , ([N], "M-A", "L", move Right 100)
    , ([N], "M-A-C", "H", snap Left)
    , ([N], "M-A-C", "J", snap Down)
    , ([N], "M-A-C", "K", snap Up)
    , ([N], "M-A-C", "L", snap Right)
    , -- resize views
      ([N], "M-A-S", "H", resize Horizontal (-100))
    , ([N], "M-A-S", "J", resize Vertical 100)
    , ([N], "M-A-S", "K", resize Vertical (-100))
    , ([N], "M-A-S", "L", resize Horizontal 100)
    ]

ptrKeymaps :: [Keymap]
ptrKeymaps =
    [ -- pointer keymaps
      ([N], "M", "BTN_LEFT", ptrMoveView)
    , ([N], "M", "BTN_RIGHT", ptrResizeView)
    , ([N], "M", "BTN_MIDDLE", ptrToggleFloat)
    ]

main :: IO ()
main = do
    setKeymaps keymaps
    setPtrMaps ptrKeymaps
    callctl ["default-layout", "rivertile"]
    callRiverTile ["-view-padding", "0", "-outer-padding", "0"]
