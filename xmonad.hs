{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           Control.Monad                  ( replicateM_ )
import           Data.Map                       ( fromList
                                                , (!)
                                                )
import           Data.Monoid                    ( All
                                                , (<>)
                                                )
import           Network.HostName               ( getHostName )
import           XMonad                  hiding ( (|||) )
import           XMonad.Actions.CopyWindow      ( copy
                                                , kill1
                                                )
import           XMonad.Actions.CycleWS         ( Direction1D(..)
                                                , WSType(..)
                                                , moveTo
                                                )
import           XMonad.Actions.DynamicWorkspaces
                                                ( addWorkspacePrompt
                                                , removeEmptyWorkspace
                                                , renameWorkspace
                                                , withWorkspace
                                                )
import           XMonad.Actions.FloatKeys       ( keysMoveWindow
                                                , keysResizeWindow
                                                )
import           XMonad.Actions.RotSlaves       ( rotSlavesDown
                                                , rotSlavesUp
                                                )
import           XMonad.Actions.Submap          ( submap )
import           XMonad.Actions.UpdatePointer   ( updatePointer )
import           XMonad.Hooks.EwmhDesktops      ( ewmh )
import           XMonad.Hooks.ManageDocks       ( ToggleStruts(ToggleStruts)
                                                , avoidStruts
                                                , docks
                                                , manageDocks
                                                )
import           XMonad.Hooks.SetWMName         ( setWMName )
import           XMonad.Layout.LayoutCombinators
                                                ( (|||) )
import           XMonad.Layout.LimitWindows     ( limitSelect
                                                , setLimit
                                                )
import           XMonad.Layout.MosaicAlt        ( tallWindowAlt
                                                , wideWindowAlt
                                                )
import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Layout.Reflect          ( reflectHoriz )
import           XMonad.Layout.ResizableTile    ( MirrorResize
                                                  ( MirrorExpand
                                                  , MirrorShrink
                                                  )
                                                , ResizableTall(..)
                                                )
import           XMonad.Layout.ThreeColumns     ( ThreeCol(..) )
import           XMonad.Layout.ToggleLayouts    ( ToggleLayout(..)
                                                , toggleLayouts
                                                )
import           XMonad.Prompt                  ( XPConfig(..)
                                                , XPPosition(..)
                                                , font
                                                , height
                                                , position
                                                )
import qualified XMonad.StackSet               as W
import           XMonad.Util.EZConfig           ( additionalKeysP
                                                , checkKeymap
                                                , mkKeymap
                                                )
import           XMonad.Util.NamedScratchpad    ( NamedScratchpad(..)
                                                , customFloating
                                                , namedScratchpadAction
                                                , namedScratchpadManageHook
                                                )
import           XMonad.Layout.Fullscreen       ( fullscreenEventHook
                                                , fullscreenFull
                                                , fullscreenManageHook
                                                )
import           XMonad.Util.Paste              ( sendKey )
import           System.Taffybar.Support.PagerHints
                                                ( pagerHints )


-- Everything begins at main
main = getHostName >>= xmonad . mkConfig

-- Make a config with an xmobar handle and the hostname. Note the
-- config depends on the keys and vice versa - all is good thanks to
-- Haskell's lazyness!
mkConfig hostname = docks . ewmh . pagerHints $ myConfig
 where
  keyconfig = myKeys myConfig hostname
  myConfig =
    def { terminal           = "termite"
        , normalBorderColor  = solarized "base1"
        , focusedBorderColor = solarized "red"
        , borderWidth        = 3
        , modMask            = mod4Mask
        , workspaces         = show <$> [1 :: Int .. 3]
        , logHook            = mkLogHook
        , manageHook         = myManageHook
        , layoutHook         = myLayoutHook
        , startupHook        = mkStartupHook myConfig keyconfig
        , handleEventHook    = myHandleEventHook
        }
      `additionalKeysP` keyconfig

-- Layout hook - all quite standard, note the use of limitSelect (I
-- have a key-binding to limit the windows defined below, which is
-- pretty handy)
myLayoutHook =
  smartBorders
    . avoidStruts
    . fullscreenFull
    . toggleLayouts Full
    . limitSelect 1 5
    $ layouts

-- The type of my layouts - not sure there is an easier way to express this

-- Layouts
layouts = tall ||| reflectedTall ||| mirrorTall ||| threecolmid
 where
  tall          = ResizableTall 1 0.03 (φ / (1 + φ)) []
  reflectedTall = reflectHoriz tall
  mirrorTall    = Mirror (ResizableTall 1 0.03 (φ / (1 + φ)) [])
  threecolmid   = ThreeColMid 1 (3 / 100) (1 / 2)
  φ             = realToFrac ((1.0 + sqrt 5) / 2.0 :: Double)

-- LG3D is needed for Java applications. TODO: The checkKeymap is according
-- to docs but doesn't seem to have an effect.
mkStartupHook c k =
  setWMName "LG3D" <+> setFullscreenSupported >> return () >> checkKeymap c k

-- all pretty standard
myManageHook :: ManageHook
myManageHook =
  def
    <+> manageDocks
    <+> manageGimp
    <+> fullscreenManageHook
    <+> manageScratchPad
 where
  manageGimp       = composeAll [className =? "Gimp" --> doFloat]
  manageScratchPad = namedScratchpadManageHook myScratchpads

-- scratchpads, see the docs of NamedScratchpad
myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "termite"
       "termite --name scratchpad"
       (resource =? "scratchpad")
       (customFloating $ center 0.6 0.6)
  , NS "pavucontrol"
       "pavucontrol"
       (className =? "Pavucontrol")
       (customFloating $ center 0.6 0.6)
  , NS "spotify"
       "spotify"
       (className =? "Spotify")
       (customFloating $ center 0.8 0.8)
  ]
  where center w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h

-- Log hook which hides the workspace named "NSP" (scratchpad) and
-- which sets the mouse pointer in the middle of a window on focus
-- change
mkLogHook :: X ()
mkLogHook = updatePointer (0.5, 0.5) (0, 0)

myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def <+> fullscreenEventHook

-- key configuration with modal submaps: once you type the prefix, you
-- remain in that "mode" for as long as you press keys defined in that
-- model.
myKeys :: XConfig a -> String -> [(String, X ())]
myKeys cfg hostname =
  let modal' = modal cfg
                                    -- manage workspaces
  in
    [ ("M-u"  , moveTo Prev (WSIs $ return ((/= "NSP") . W.tag)))
    , ("M-i"  , moveTo Next (WSIs $ return ((/= "NSP") . W.tag)))
    , ("M-; ;", spawn "rofi -show window")
    , ("M-; a", addWorkspacePrompt myXPConfig)
    , ("M-; k", removeEmptyWorkspace)
    , ("M-; m", withWorkspace myXPConfig (windows . W.shift))
    , ("M-; c", withWorkspace myXPConfig (windows . copy))
    , ("M-; r", renameWorkspace myXPConfig)
    , ("M-f"  , sendMessage $ Toggle "Full")
    ]
    -- set number of windows in workspace
    ++ map (\n -> ("M-; " <> show n, setLimit $ n + 1)) [0 .. 9]
    -- set backlight brightness
    ++ map (\n -> ("M-b " <> show n, setBacklight n))   [0 .. 9]
    ++
    -- managing applications
       [ ("M-d d", spawn "rofi -show run")
       , ("M-d e", spawn "emacsclient -c")
       , ("M-d t", spawn "termite")
       , ("M-d f", spawn "firefox")
       , ( "M-d k"
         , kill1
         )
       -- change tab in an underlying window, very convenient in
       -- Firefox where Vimium often does not work when a search
       -- dialogue is open and where the standard shortcut "C-<Tab>"
       -- is awkward
       , ( "M-<Tab>"
         , modal'
           [ ("k", sendKey controlMask xK_Tab)
           , ("j", sendKey (controlMask .|. shiftMask) xK_Tab)
           ]
         )
         -- manage passwords & otp keys
       , ( "M-p p"
         , spawn
           "gopass ls --flat | rofi -dmenu -matching fuzzy -sort -sort-levenshtein | xargs --no-run-if-empty gopass show -c"
         )
       , ( "M-p l"
         , spawn
           "gopass ls --flat | rofi -dmenu -matching fuzzy -sort -sort-levenshtein | xargs --no-run-if-empty gopass show login -c"
         )
       , ( "M-p o"
         , spawn
           "gopass ls --flat | rofi -dmenu -matching fuzzy -sort -sort-levenshtein | xargs --no-run-if-empty gopass otp -c"
         )
       , ("M-<Delete>", spawn "i3lock")
       , ("M-m"       , windows focusMaster)
       , ("M-S-m"     , windows W.swapMaster)
       , ("M-S-k"     , windows W.swapUp)
       , ("M-S-j"     , windows W.swapDown)
       , ( "M-<Return>"
         , windows shiftMaster
         )
       -- rotate slave modal mode - very convenient!
       , ("M-'", modal' [("k", rotSlavesUp), ("j", rotSlavesDown)])
       , ("M-h", rotSlavesUp)
       , ( "M-l"
         , rotSlavesDown
         )
--       , ("M-j", focusDown)
--       , ("M-k", focusUp)
       , ("M-s <Return>", namedScratchpadAction myScratchpads "termite")
       , ("M-s v", namedScratchpadAction myScratchpads "pavucontrol")
       , ("M-s s", namedScratchpadAction myScratchpads "spotify")
       , ( "M-s n"
         , spawn
           "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"
         )
       , ( "M-s p"
         , spawn
           "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
         )
       , ("M-s w", spawn "wpa_gui")
       , ("M-b b", sendMessage ToggleStruts)
       , ( "M-b f"
         , withFocused float
         )
       -- modal mode to move floating windows around
       , ( "M-n"
         , let n = fromIntegral (40 :: Int)
           in  modal'
                 [ ("h"  , withFocused (keysMoveWindow (-n, 0)))
                 , ("l"  , withFocused (keysMoveWindow (n, 0)))
                 , ("k"  , withFocused (keysMoveWindow (0, -n)))
                 , ("j"  , withFocused (keysMoveWindow (0, n)))
                 , ("S-h", withFocused (keysResizeWindow (-n, 0) (0, 0)))
                 , ("S-l", withFocused (keysResizeWindow (n, 0) (0, 0)))
                 , ("S-j", withFocused (keysResizeWindow (0, n) (0, 0)))
                 , ("S-k", withFocused (keysResizeWindow (0, -n) (0, 0)))
                 ]
         )
         -- Resize window mode, similar to what i3 does. Vertical
         -- resizing works in the ResizableTile layout, the standard
         -- step size is extremely small so I just dispatch the
         -- message multiple times per keypress
       , ( "M-r"
         , modal'
           [ ("j", replicateM_ 5 $ sendMessage MirrorShrink)
           , ("k", replicateM_ 5 $ sendMessage MirrorExpand)
           , ("h", sendMessage Shrink)
           , ("l", sendMessage Expand)
           ]
         )
       , ("M-S-s", withFocused (sendMessage . tallWindowAlt))
       , ("M-S-d", withFocused (sendMessage . wideWindowAlt))
       , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 2")
       , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 2")
       , ("M-c", modal' [("j", windows W.swapDown), ("k", windows W.swapUp)])
       ]
    -- the reason why we need the hostname here: different PA devices
    ++ case hostname of
         "worky-mcworkface" ->
           [ ( "M-<F9>"
             , spawn
               "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"
             )
           , ("M-<F10>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
           , ("M-<F11>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -1.5%")
           , ("M-<F12>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%")
           ]
         _ ->
           [ ("<XF86AudioMute>"       , spawn "pactl set-sink-mute 0 toggle")
           , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -1.5%")
           , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +1.5%")
           , ( "<XF86AudioPlay>"
             , spawn
               "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"
             )
           , ( "<XF86AudioNext>"
             , spawn
               "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"
             )
           , ( "<XF86AudioPrev>"
             , spawn
               "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
             )
           ]

-- set the backlight - I like 0 to yield 100%
setBacklight :: Int -> X ()
setBacklight n = spawn $ "xbacklight -set " <> show (f n)
 where
  f 0 = 100
  f i = 1 + 11 * (i - 1)

-- Function to generate a modal keymap
modal :: XConfig a -> [(String, X ())] -> X ()
modal cfg bindings = modalMap
 where
  exitKeys = ["<Space>", "<Return>", "<Escape>"]
  modalMap = submap . mkKeymap cfg $ map f bindings ++ map g exitKeys
  f (k, a) = (k, a >> modalMap)
  g k = (k, pure ())

-- Config for the XMonad prompt
myXPConfig :: XPConfig
myXPConfig =
  def { position = Top, font = "xft:DejaVu Sans:size=9", height = 40 }

-- The solarized colors
solarized :: String -> String
solarized key =
  fromList
      [ ("base03" , "#002B36")
      , ("base02" , "#073642")
      , ("base01" , "#586E75")
      , ("base00" , "#657B83")
      , ("base0"  , "#839496")
      , ("base1"  , "#93A1A1")
      , ("base2"  , "#EEE8D5")
      , ("base3"  , "#FDF6E3")
      , ("yellow" , "#B58900")
      , ("orange" , "#CB4B16")
      , ("red"    , "#DC322F")
      , ("magenta", "#D33682")
      , ("violet" , "#6C71C4")
      , ("blue"   , "#268BD2")
      , ("cyan"   , "#2AA198")
      , ("green"  , "#859900")
      ]
    ! key

-- Customized focusMaster function: if master is already focused, the
-- original W.focusMaster is a no-op. I find it convenient however to
-- swap the topmost windows in that case
focusMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
focusMaster = W.modify' $ \c -> case c of
  W.Stack t [] (x : xs) -> W.Stack x [] (t : xs)
  W.Stack _ [] _ -> c
  W.Stack t ls rs -> W.Stack x [] (xs ++ t : rs) where (x : xs) = reverse ls

-- Customized shiftMaster with the same behavior as focusMaster if the
-- master is already in focus
shiftMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
shiftMaster = W.modify' $ \c -> case c of
  W.Stack t [] (x : xs) -> W.Stack x [] (t : xs)
  W.Stack _ [] _        -> c -- already master.
  W.Stack t ls rs       -> W.Stack t [] (reverse ls ++ rs)

-- Some hack from
-- https://www.reddit.com/r/xmonad/comments/77szad/cant_go_fullscreen_in_firefox_even_with_ewmh/
-- to fix Firefox in full screen
setFullscreenSupported :: X ()
setFullscreenSupported = withDisplay $ \dpy -> do
  r    <- asks theRoot
  a    <- getAtom "_NET_SUPPORTED"
  c    <- getAtom "ATOM"
  supp <- mapM
    getAtom
    [ "_NET_WM_STATE_HIDDEN"
    , "_NET_WM_STATE_FULLSCREEN" -- XXX Copy-pasted to add this line
    , "_NET_NUMBER_OF_DESKTOPS"
    , "_NET_CLIENT_LIST"
    , "_NET_CLIENT_LIST_STACKING"
    , "_NET_CURRENT_DESKTOP"
    , "_NET_DESKTOP_NAMES"
    , "_NET_ACTIVE_WINDOW"
    , "_NET_WM_DESKTOP"
    , "_NET_WM_STRUT"
    ]
  io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)
