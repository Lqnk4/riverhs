module Main where

import Riverctl
import Prelude hiding (Left, Right)
import Data.Foldable (traverse_)

display :: String
display = "eDP-1"

term :: String
term = "foot"

browser :: String
browser = "zen-browser"

launcher :: String
launcher = "fuzzel"

fcitxToggle :: String
fcitxToggle = "fcitx5-remote -t ; pkill -SIGRTMIN+1 waybar"

customModes :: [Mode]
customModes = [P]

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
    , ([N], "M", "Left", mainLocation Left)
    , ([N], "M", "Right", mainLocation Right)
    , ([N], "M", "Up", mainLocation Up)
    , ([N], "M", "Down", mainLocation Down)
    , -- resize views
      ([N], "M-A-S", "H", resize Horizontal (-100))
    , ([N], "M-A-S", "J", resize Vertical 100)
    , ([N], "M-A-S", "K", resize Vertical (-100))
    , ([N], "M-A-S", "L", resize Horizontal 100)
    , -- floating/fullscreen
      ([N], "M", "Space", toggleFloat)
    , ([N], "M", "F", toggleFullScreen)
    , -- passthrough
      ([N], "M", "F11", enterMode P)
    , ([P], "M", "F11", enterMode N)
    , -- XF86 maps
      ([N, L], "None", "XF86AudioMute", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ([N, L], "None", "XF86AudioRaiseVolume", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ([N, L], "None", "XF86AudioLowerVolume", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ([N, L], "None", "XF86AudioMedia", spawn "playerctl play-pause")
    , ([N, L], "None", "XF86AudioPlay", spawn "playerctl play-pause")
    , ([N, L], "None", "XF86AudioPrev", spawn "playerctl previous")
    , ([N, L], "None", "XF86AudioNext", spawn "playerctl next")
    , ([N, L], "None", "XF86MonBrightnessUp", spawn "light -A 5")
    , ([N, L], "None", "XF86MonBrightnessDown", spawn "light -U 5")
    , -- screenshots
      ([N], "M", "P", spawn "grim -g \"$(slurp -d)\" - | swappy -f -")
    , ([N], "M-S", "P", spawn "grim - | swappy -f -")
    , -- waybar
      ([N], "M", "B", spawn "killall waybar || waybar")
    ]

ptrKeymaps :: [Keymap]
ptrKeymaps =
    [ -- pointer keymaps
      ([N], "M", "BTN_LEFT", ptrMoveView)
    , ([N], "M", "BTN_RIGHT", ptrResizeView)
    , ([N], "M", "BTN_MIDDLE", toggleFloat)
    ]

windowRules :: [Rule]
windowRules =
    [ (AppID, "bar", CSD)
    , (AppID, "zen-alpha", SSD)
    , (AppID, "org.pwmt.zathura", SSD)
    , (AppID, "emacs", SSD)
    , (AppID, "com.mitchellh.ghostty", SSD)
    ]

main :: IO ()
main = do
    declareCustomModes customModes
    traverse_ setKeymap keymaps
    traverse_ setKeymap genTagMaps
    traverse_ setPtrMap ptrKeymaps
    traverse_ addRule windowRules
    setKeyRepeat 50 300

    callExternalShell "swaybg -i $HOME/Wallpapers/toradora_minimal_solarized.png"
    callExternal "waybar" []
    callExternalShell
        "swayidle -w \
        \ timeout 500 'waylock -fork-on-lock' \
        \ timeout 1000 'systemctl suspend'"
    callExternal "wayland-pipewire-idle-inhibit" []

    callctl ["default-layout", "rivertile"]
    callExternal "rivertile" ["-view-padding", "0", "-outer-padding", "0"]
