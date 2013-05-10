import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops

main :: IO ()
main = xmonad =<< xmobar defaultConfig
        {
          modMask = mod4Mask
        , workspaces = ["1:main"] ++ map show [2..9]
        , handleEventHook = fullscreenEventHook
        }
