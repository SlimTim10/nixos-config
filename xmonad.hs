import qualified XMonad as X
import XMonad ((.|.))
import qualified XMonad.Prompt as Prompt
import qualified XMonad.Prompt.Window as PromptW
import qualified XMonad.Layout.NoBorders as Layout
import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Actions.WindowGo as WindowGo
import XMonad.Actions.WindowGo ((=?))
import qualified XMonad.StackSet as W
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Map as M

main :: IO ()
main = X.xmonad =<< DynamicLog.xmobar X.def
  { X.modMask = X.mod4Mask
  , X.keys = \c -> mykeys c `M.union` X.keys X.defaultConfig c
  , X.layoutHook = Layout.smartBorders $ X.layoutHook X.def
  , X.borderWidth = 2
  , X.focusedBorderColor = "#00FF00"
  }
  where
    mykeys (X.XConfig {X.modMask = modm}) = M.fromList $
      [ ((modm, X.xK_x), X.spawn "lock-screen")
      -- Go to window by name search
      , ((modm, X.xK_s), PromptW.windowPrompt
          X.def
          { Prompt.searchPredicate = myFuzzyFinderFunction
          , Prompt.alwaysHighlight = True
          }
          PromptW.Goto
          PromptW.allWindows)
      -- Bring window by name search
      , ((modm .|. X.shiftMask, X.xK_s), PromptW.windowPrompt
          X.def { Prompt.searchPredicate = myFuzzyFinderFunction }
          PromptW.Bring
          PromptW.allWindows)
      -- Go to emacs
      , ((modm, X.xK_e), WindowGo.runOrRaise "emacs" (WindowGo.className =? "Emacs"))
      -- Movement bindings like emacs
      , ((modm, X.xK_o), X.windows W.focusDown)
      , ((modm .|. X.shiftMask, X.xK_o), X.windows W.focusUp)
      ]

myFuzzyFinderFunction :: String -> String -> Bool
myFuzzyFinderFunction a b = map C.toLower a `L.isInfixOf` map C.toLower b
