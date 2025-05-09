import qualified XMonad as X
import XMonad (X, (.|.), (=?), (-->), (<+>), (|||))
import qualified XMonad.Prompt as Prompt
import qualified XMonad.Prompt.Window as PromptW
import qualified XMonad.Layout.NoBorders as Layout
import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Actions.WindowGo as WindowGo
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Util.EZConfig as EZConfig
import qualified XMonad.Actions.CycleWS as CycleWS
import qualified XMonad.Layout.TwoPane as TwoPane
import qualified XMonad.Actions.Volume as Vol
import qualified XMonad.Util.Dzen as Dzen -- display volume
import qualified XMonad.Hooks.EwmhDesktops as Ewmh
import qualified XMonad.Layout.Reflect as Reflect
import qualified XMonad.Layout.MultiToggle as Toggle
import qualified XMonad.Layout.LayoutScreens as LayoutScreens
import qualified XMonad.Layout.ThreeColumns as ThreeColumns
import qualified XMonad.Actions.EasyMotion as EasyMotion
import qualified XMonad.Actions.GridSelect as GridSelect
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Bool as B
import qualified Data.Foldable as Fold
import Control.Monad ((>=>), void, forM)

main :: IO ()
main = do
  xmobar <- DynamicLog.xmobar $ Ewmh.ewmh $ X.def
    { X.modMask = X.mod4Mask
    , X.layoutHook = myLayout
    , X.borderWidth = 8
    , X.focusedBorderColor = "red"
    , X.normalBorderColor = "black"
    , X.workspaces = workspaces
    , X.manageHook = myManageHook <+> X.manageHook X.def
    , X.clickJustFocuses = False
    }
    `EZConfig.removeKeysP`
    [ "M-S-/"
    ]
    `EZConfig.additionalKeysP`
    ( [ ("M-x", X.spawn "lock-screen")
      -- Launcher
      , ("M-p", X.spawn "rofi-custom")
      -- Change to full screen layout
      , ("M-f", X.sendMessage $ X.JumpToLayout "Full")
      -- Go to window by name search
      , ("M-s", PromptW.windowPrompt promptDef PromptW.Goto PromptW.allWindows)
      -- Bring window by name search
      , ("M-S-s", PromptW.windowPrompt promptDef PromptW.Bring PromptW.allWindows)
      -- Spawn emacs
      , ("M-e" , X.spawn "emacsclient -c -n -e '(switch-to-buffer nil)'") -- This replaces a default physical screen keybinding
      -- Go to first emacs frame
      , ("M-<Home>", WindowGo.runOrRaise "emacs" (WindowGo.className =? "Emacs"))
      -- Movement bindings like emacs
      , ("M-o", X.windows W.focusDown)
      , ("M-S-o", X.windows W.focusUp)
      -- Add or select workspace
      , ("M-=", DW.selectWorkspace promptDef)
      -- Move current window to workspace
      , ("M-S-=", DW.withWorkspace promptDef (X.windows . W.shift))
      -- Remove workspace (difficult sequence to avoid accidents)
      , ("M-C-M1--", DW.removeWorkspace)
      -- Rename workspace
      , ("M-S--", DW.renameWorkspace X.def)
      -- Go back to most recent workspace
      , ("M-/", CycleWS.toggleWS)
      , ("M-z", CycleWS.toggleWS)
      -- Change directly to specific screen layouts
      , ("M-<Up>", X.sendMessage $ X.JumpToLayout "Full")
      , ("M-<Right>", X.sendMessage $ X.JumpToLayout "TwoPane")
      , ("M-<Down>", X.sendMessage $ X.JumpToLayout "Tall")
      , ("M-<Left>", X.sendMessage $ X.JumpToLayout "ThreeCol")
      -- Audio buttons
      , ("<XF86AudioMute>", Vol.toggleMute >>= (B.bool (showVolume "mute") (showVolume "unmute")))
      , ("<XF86AudioLowerVolume>", Vol.lowerVolume 5 >>= showVolume . show . round)
      , ("<XF86AudioRaiseVolume>", Vol.raiseVolume 5 >>= showVolume . show . round)
      -- Screenshot
      , ("<Print>", X.spawn "screenshot")
      -- Reflect layout horizontally
      , ("M-\\", X.sendMessage $ Toggle.Toggle Reflect.REFLECTX)
      -- Split into two screens
      , ("M-S-<Space>", LayoutScreens.layoutScreens 2 (TwoPane.TwoPane 0.5 0.5))
      -- Reset the screen configuration
      , ("M-<Esc>", X.rescreen)
      -- Focus a window by visible selection
      , ("M-.", EasyMotion.selectWindow X.def >>= (`X.whenJust` X.windows . W.focusWindow))
      -- Bind comma too, just so it doesn't do its default binding
      , ("M-,", EasyMotion.selectWindow X.def >>= (`X.whenJust` X.windows . W.focusWindow))
      -- Bring up a 2D grid of the non-visible windows in the current workspace for selection
      , ("M-g", showNonVisibleWindows)
      -- Increase or decrease number of windows in the master area. 
      , ("M-[", X.sendMessage $ X.IncMasterN 1) -- %! Increment the number of windows in the master area
      , ("M-]", X.sendMessage $ X.IncMasterN (-1)) -- %! Decrement the number of windows in the master area
      ]
      -- mod-[1..9]       %! Switch to workspace N in the list of workspaces
      -- mod-shift-[1..9] %! Move client to workspace N in the list of workspaces
      ++
      map (\n -> ("M-" ++ show n, DW.withNthWorkspace W.greedyView (n-1))) [1 .. 9]
      ++
      map (\n -> ("M-S-" ++ show n, DW.withNthWorkspace W.shift (n-1))) [1 .. 9]
      )
  X.xmonad xmobar
  where
    promptDef = X.def
      { Prompt.searchPredicate = myFuzzyFinder
      , Prompt.alwaysHighlight = True
      }
    workspaces = ["1:main", "2:web", "3:media", "4:meeting"] ++ map show [5 .. 9]

showNonVisibleWindows :: X ()
showNonVisibleWindows = do
  windows <- getNonVisibleWindows
  windowPairs :: [(String, X.Window)] <- forM windows $ \w -> do
    title <- X.runQuery X.title w
    pure (title, w)
  GridSelect.gridselect X.def windowPairs >>= (`X.whenJust` X.windows . W.focusWindow)

getNonVisibleWindows :: X [X.Window]
getNonVisibleWindows = do
  X.XState { X.mapped = mappedWins, X.windowset = ws } <- X.get
  let visibleWindows :: [X.Window] = Fold.toList mappedWins
  currentScreen <- W.current <$> X.gets X.windowset
  filter (not . (`elem` visibleWindows))
    . W.integrate'
    . W.stack
    . W.workspace
    . W.current
    <$> X.gets X.windowset

showVolume :: String -> X ()
showVolume = Dzen.dzenConfig (Dzen.timeout 1 >=> centered)
  where
    centered =
      Dzen.onCurr (Dzen.center 300 100)
      >=> Dzen.font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
      >=> Dzen.addArgs ["-fg", "#80c0ff"]
      >=> Dzen.addArgs ["-bg", "#000040"]

myFuzzyFinder :: String -> String -> Bool
myFuzzyFinder a b = map C.toLower a `L.isInfixOf` map C.toLower b

myManageHook = X.composeAll
  [ X.className =? "Thunderbird" --> X.doShift "2:web"
  , X.className =? "Slack" --> X.doShift "2:web"
  , X.className =? "zoom" --> X.doShift "4:meeting"
  , X.title =? "zoom"  --> X.doFloat
  ]

myLayout =
  Toggle.mkToggle (Toggle.single Reflect.REFLECTX)
  $ Layout.smartBorders
  $ tiled ||| threeCol ||| twoPane ||| X.Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = X.Tall nmaster delta ratio

    -- A layout that splits the screen horizontally and shows two windows.
    -- The left window is always the master window,
    -- and the right is either the currently focused window or the second window in layout order.
    twoPane = TwoPane.TwoPane delta ratio

    -- Three column layout with master window in the middle.
    threeCol = ThreeColumns.ThreeColMid nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 1/2

    -- Percent of screen to increment by when resizing panes
    delta = 3/100

{-
DEFAULT KEY BINDINGS
FROM: https://hackage.haskell.org/package/xmonad-0.17.0/docs/src/XMonad.Config.html

keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask,               xK_p     ), spawn "dmenu_run") -- %! Launch dmenu
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad

    , ((modMask .|. shiftMask, xK_slash ), helpCommand) -- %! Run xmessage with a summary of the default keybindings (useful for beginners)
    -- repeat the binding for non-American layout keyboards
    , ((modMask              , xK_question), helpCommand) -- %! Run xmessage with a summary of the default keybindings (useful for beginners)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


FROM SOMEWHERE ELSE?
,((modm, xK_b     ), sendMessage ToggleStruts)
-}
