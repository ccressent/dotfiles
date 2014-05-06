import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers

myWorkspaces = ["1:main"] ++ map show [2..8] ++ ["9:system"]

myManageHook = composeAll
    [ className =? "Vlc" --> doCenterFloat ]

main :: IO ()
main = xmonad =<< xmobar defaultConfig
        {
          modMask = mod4Mask
        , terminal = "urxvt"
        , workspaces = myWorkspaces
        , manageHook = myManageHook
        , handleEventHook = fullscreenEventHook
        }
