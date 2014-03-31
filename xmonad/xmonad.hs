--
-- An XMonad configuration file with:
--   SubLayouts
--   Proper floating window support for KDE
--   GridSelect
--   Little title box overlays for non-focus windows.

import XMonad
import Control.Exception (bracket)
import Data.Monoid
import Graphics.X11.Xinerama
import System.Exit
import System.IO
import System.Process (readProcessWithExitCode, system)

import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn (spawnAndDo, manageSpawn)
import XMonad.Actions.Submap

import XMonad.Config.Kde
import XMonad.Config.Gnome
import XMonad.Config.Desktop

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.Themes

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.NoBorders
import XMonad.Layout.DwmStyle
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "konsole"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
--
myBorderWidth   = 4

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["emacs","web","term","firefox","nx/misc",
                   "emacs2","web2","term2","misc", "misc2"]

-- Border colors for unfocused and focused windows, respectively.
--
-- myNormalBorderColor  = "#0c141f"
-- myFocusedBorderColor = "#df740c"

-- Palette
-- "#df740c" - orange
-- "#ffe64d" -- blue
-- E6FFFF - Pane
-- 6FC3DF - Cyan
-- 0C141F -- background
-- Green: 7ce31a
-- Blue: 4cc0e1
-- Orange: f3ce3e

greenColorizer = colorRangeFromClassName
                     (0x04,0x0c,0x0e) -- lowest inactive bg
                     (0x4c,0xc0,0xe1) -- highest inactive bg
                     (0xdf,0x74,0x0c)            -- active bg
                     (0xe0,0xe0,0xe0)            -- inactive fg
                     (0xff,0xff,0xff)            -- active fg
--  where black = minBound
--        white = maxBound

-- defaultGSConfig
gsconfig = (buildDefaultGSConfig greenColorizer)  {
             gs_font = "xft:Anka/Coder Condensed:pixelsize=12",
             gs_cellheight = 30,
             gs_cellwidth = 300,
             gs_cellpadding = 15
}

myNormalBorderColor  = "#000000" -- Black
--myFocusedBorderColor = "#4CC0E1" -- Torqoise (blue)
myFocusedBorderColor = "#4cc0e1" -- Blue #"#F8CE3E" -- Orange

------------------------------------------------------------------------
-- Custom actions
--

-- XXX actually getting process exit status within xmonad is painful; we need to
--     claim our SIGPIPE back:
noSignalHandlers =
    (bracket uninstallSignalHandlers $ const installSignalHandlers) . const
xsystem = noSignalHandlers . system

-- (Google specific)
getProdAccess :: X ()
getProdAccess = do
    -- determine if prodaccess is already available
    status <- io $ xsystem "prodcertstatus --quiet --check_ssh --check_remaining_hours=10"
    -- if not, run prodaccess in a little floating xterm
    case status of
        ExitSuccess -> return ()
        otherwise ->
            spawnAndDo doCenterFloat "xterm -title prodaccess -geometry 60x5 -e prodaccess -s"


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch gmrun
    [ ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

      -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Group-based window management
    , ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
    , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)
    , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U)
    , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D)

    , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
    , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))

    , ((modm .|. controlMask, xK_period), onGroup W.focusDown')
    , ((modm .|. controlMask, xK_comma), onGroup W.focusUp')
    , ((modm,               xK_j), focusDown)
    , ((modm,               xK_k), focusUp)
    , ((modm,               xK_s), submap $ defaultSublMap conf)

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Toggle GridSelect program menu
    , ((modm              , xK_g     ), goToSelected gsconfig)

   -- Note: I can also have a separate grid with different datasets;
   -- but I think I'd rather bind that to a larger "task" framework
   -- later.  Either way, see
   -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-GridSelect.html

    -- Logout
    -- , ((modm .|. shiftMask, xK_q     ),
    --                  spawn "gnome-session-save --gui --logout-dialog")

    -- Fetch OTP
    -- , ((modm .|. shiftMask, xK_i     ), spawn "/usr/bin/fetchotp -c")

    -- lock
    , ((modm .|. shiftMask, xK_l     ),
                  spawn "qdbus org.freedesktop.ScreenSaver /ScreenSaver Lock")

    -- prodaccess
    , ((modm .|. controlMask, xK_p    ), getProdAccess)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
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
-- Theme
--
-- For the title box on the right.
-- myTheme = defaultTheme

--myTheme = theme donaldTheme
-- see http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-Themes.html#t%3AThemeInfo
newTheme :: ThemeInfo
newTheme = TI "" "" "" defaultTheme
-- Green: 7ce31a
-- Blue: 4cc0e1
-- Orange: f3ce3e

myTheme = newTheme { themeName = "tronTheme"
                   , themeAuthor = "Lally Singh"
                   , themeDescription = "from the movie."
                   , theme = defaultTheme {
                               activeColor = "#F3CE3E"
                             , activeBorderColor = "#f3ce3e"
                             , activeTextColor = "#ffffff"
                             , inactiveColor = "#000000"
                             , inactiveBorderColor = "#4cc0e1"
                             , inactiveTextColor = "#4cc0e1"
                             , decoHeight = 24
                             , decoWidth = 300
--                             , fontName = "xft:LMSansDemiCond10:pixelsize=12"
                             , fontName = "xft:Anka/Coder Condensed:pixelsize=12"
                             }
                   }


------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
-- windowNavigation $ subTabbed $ boringWindows $
myLayout = dwmStyle shrinkText (theme myTheme) (
  windowNavigation $ subTabbed $ boringWindows $ (tiled ||| Mirror tiled
                                                  ||| noBorders Full
                                                  ||| ThreeCol nmaster delta ratio))
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

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- TODO(lally): Look at scratchpad workspaces (Util.Scratchpad?
-- scratchpadFilterOutWorkspace) to put the mount-info popup on.  Those should
-- never get shown.
myManageHook = composeAll
    [ manageHook kde4Config
    , className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , title     =? "plasma-desktop" --> doFloat
    , resource  =? "desktop_window" --> doFloat
    , resource  =? "kdesktop"       --> doFloat
    , className =? "InputOutput"    --> doFloat
    , title     =? "Eclipse"        --> doFloat
    , className =? "Plasma-desktop" --> doFloat -- (doShift "misc2")
    , isDialog                      --> doCenterFloat
    , isFullscreen --> doFullFloat ]


managementHooks :: [ManageHook]
managementHooks = [
    resource  =? "Do"        --> doIgnore
  , className =? "rdesktop"  --> doFloat
  ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mappend fullscreenEventHook ewmhDesktopsEventHook -- fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--myLogHook = dynamicLogWithPP (myPrettyPrinter dbus)

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

{-
<p>xmproc &lt;- spawnPipe "/usr/bin/xmobar /home/blackgod/.xmobarrc"</p>
<p>xmonad $ defaultConfig</p>
<p>{ manageHook = manageDocks &lt;+&gt; myManageHook -- make sure to include myManageHook definition from above</p>
<p>&lt;+&gt; manageHook defaultConfig</p>
<p>, layoutHook = avoidStruts  $  layoutHook defaultConfig</p>
<p>, logHook = dynamicLogWithPP xmobarPP</p>
<p>{ ppOutput = hPutStrLn xmproc</p>
<p>, ppTitle = xmobarColor "grey" "" . shorten 50</p>
<p>}</p>

-}

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do if not Graphics.X11.Xinerama.compiledWithXinerama 
            then putStrLn "WARNING: Xinerama was not compiled in."
            else do  xmproc <- spawnPipe "/usr/bin/xmobar /ulg/home/lally/config/xmobarrc"
                     xmonad kde4Config {
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
	                layoutHook         = desktopLayoutModifiers (myLayout), 
			logHook = dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn xmproc
			                                    , ppTitle = xmobarColor "grey" "" . shorten 50},
	                manageHook         = myManageHook,
	                handleEventHook    = myEventHook,
	                startupHook        = myStartupHook
	             }  

