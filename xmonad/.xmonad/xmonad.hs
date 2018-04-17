import XMonad
import XMonad.StackSet (RationalRect(..))

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run
import XMonad.Util.Scratchpad

import Graphics.X11.ExtraTypes.XF86

myWorkspaces :: [String]
myWorkspaces = ["1:main"] ++ map show [2..8 :: Int] ++ ["9:system"]

myManageHook = manageScratchpad <+> composeAll
    [ className =? "Vlc" --> doCenterFloat ]

manageScratchpad :: ManageHook
manageScratchpad = scratchpadManageHook (RationalRect x y w h)
  where h = 0.25 -- terminal heigh: 25%
        w = 1    -- terminal width: 100%
        x = 0    -- distance from left (%)
        y = 0    -- distance from top (%)

myKeys = [
  ((modm, xK_p), spawn "rofi -show run"),
  ((modm, xK_grave), scratchpadSpawnAction myConfig),

  -- Multimedia Keys
  ((0, xF86XK_AudioMute),        spawn "pactl set-sink-mute   @DEFAULT_SINK@   toggle"),
  ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@   -5%"),
  ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@   +5%"),
  ((0, xF86XK_AudioMicMute),     spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle"),

  ((0, xF86XK_MonBrightnessDown), spawn "light -U 5"),
  ((0, xF86XK_MonBrightnessUp),   spawn "light -A 5")
  ] where modm = mod4Mask

myConfig = def
        {
          modMask = mod4Mask
        , terminal = "urxvt"
        , workspaces = myWorkspaces
        , logHook = dynamicLogWithPP $ def { ppCurrent = \w -> "<" ++ w ++ ">" }
        , manageHook = myManageHook
        , handleEventHook = fullscreenEventHook
        } `additionalKeys` myKeys

main :: IO ()
main = xmonad =<< xmobar myConfig
