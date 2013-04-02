import XMonad
import XMonad.Hooks.DynamicLog

main = xmonad =<< xmobar defaultConfig
        {
          modMask = mod4Mask
        , workspaces = ["1:main"] ++ map show [2..9]
        }
