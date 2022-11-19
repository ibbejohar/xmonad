import XMonad
--import XMonad.ManageHook
import Data.Monoid
import System.Exit

import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeys, mkKeymap, additionalKeysP)
import XMonad.Actions.Navigation2D
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End))
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.StatusBar
--import XMonad.Hooks.StatusBar.PP
import Data.Maybe
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ToggleLayouts

main = do 
  xmproc <- spawnPipe "pkill xmobar || xmobar -x 0"
  xmonad $ ewmhFullscreen . ewmh $ docks defaults {
	logHook = dynamicLogWithPP $ def { 
	  ppOutput = hPutStrLn xmproc 
	, ppCurrent = xmobarColor "#314f57" ""
	, ppVisible = clickable
	, ppHidden = xmobarColor "#4863db" "" . clickable 
	, ppUrgent = xmobarColor "Red" ""
	, ppHiddenNoWindows = xmobarColor "white" "" . clickable
        , ppOrder  = \(ws:_:_) -> [ws]

	}
  }

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y

myTerminal      = "alacritty"
myLauncher 	= "rofi -show drun"
myRestart	= "xmonad --recompile && xmonad --restart"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myModMask       = mod4Mask

--wsIconFull = "\60340"
--wsIconHidden = "\60341"
--wsIconEmpty = "\60017"

myWorkspaces    = ["I", "II","III","IV","V"]

myBorderWidth   = 4
myNormalBorderColor  = "#0f0f17"
myFocusedBorderColor = "#314f57"
------------------------------------------------------------------------
myKeys = \c -> mkKeymap c $
    [ -- Terminal
      ("M-<Return>", spawn (myTerminal)) 

      -- Launcher
    , ("M-s", spawn (myLauncher)) 
     
      -- Bar
    , ("M-b", sendMessage ToggleStruts)

      -- Layout
    , ("M-<Space>", sendMessage NextLayout)

    , ("M-C-S-h", sendMessage $ pullGroup L)
    , ("M-C-S-l", sendMessage $ pullGroup R)
    , ("M-C-S-k", sendMessage $ pullGroup U)
    , ("M-C-S-j", sendMessage $ pullGroup D)

    , ("M-C-S-m", withFocused (sendMessage . MergeAll))
    , ("M-C-S-u", withFocused (sendMessage . UnMerge))

    , ("M-S-<Tab>", onGroup W.focusUp')
    , ("M-<Tab>", onGroup W.focusDown')
    , ("M-e", sendMessage (Toggle "Full"))

      -- Window
    , ("M-h", windowGo R True)
    , ("M-l", windowGo L True)
    , ("M-j", windowGo D True)
    , ("M-k", windowGo U True)
    , ("M-C-h", windowSwap R True)
    , ("M-C-l", windowSwap L True)
    , ("M-C-j", windowSwap D True)
    , ("M-C-k", windowSwap U True)
    , ("M-S-h", sendMessage Shrink)
    , ("M-S-l", sendMessage Expand)

    , ("M-f", withFocused $ windows . W.sink)

    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))

      -- Quit
    , ("M-q", kill)
    , ("M-S-q", io (exitWith ExitSuccess))
    , ("M-S-r", spawn (myRestart))

    -- Switch workspace
    , ("M-1", (windows $ W.greedyView $ myWorkspaces !! 0))
    , ("M-2", (windows $ W.greedyView $ myWorkspaces !! 1))
    , ("M-3", (windows $ W.greedyView $ myWorkspaces !! 2))
    , ("M-4", (windows $ W.greedyView $ myWorkspaces !! 3))
    , ("M-5", (windows $ W.greedyView $ myWorkspaces !! 4))

    -- Move window to workspace
    , ("M-S-1", (windows $ W.shift $ myWorkspaces !! 0))
    , ("M-S-2", (windows $ W.shift $ myWorkspaces !! 1))
    , ("M-S-3", (windows $ W.shift $ myWorkspaces !! 2))
    , ("m-s-4", (windows $ W.shift $ myWorkspaces !! 3))
    , ("M-S-5", (windows $ W.shift $ myWorkspaces !! 4))

    ]
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

--confWinNav = configurableNavigation noNavigateBorders
ful = toggleLayouts Full 

myLayout = smartBorders $ ful $ avoidStruts (windowNavigation $ subTabbed tiled)
    where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- > xprop | grep WM_CLASS
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.

myManageHook = composeAll
    [insertPosition End Newer
    , className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    ]
------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty
------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--myLogHook = return ()
------------------------------------------------------------------------
-- Startup hook

myStartupHook = do
	spawnOnce "nitrogen --restore"
	spawnOnce "setxkbmap -option caps:escape_shifted_capslock"
	spawnOnce "unclutter --timeout 3 &"
	spawnOnce "xsetroot -cursor_name left_ptr"
------------------------------------------------------------------------

defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook
    }
