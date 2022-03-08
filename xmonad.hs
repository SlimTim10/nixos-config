import XMonad
import XMonad.Actions.GridSelect
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Actions.WindowGo
import XMonad.Prompt
import XMonad.Prompt.Window
import Data.List (isInfixOf)
import Data.Char (toLower)
import qualified Data.Map as M

main :: IO ()
main = xmonad =<< xmobar def
  { modMask = mod4Mask
  , keys = \c -> mykeys c `M.union` keys defaultConfig c
  , layoutHook = smartBorders $ layoutHook def
  , borderWidth = 2
  , focusedBorderColor = "#00FF00"
  }
  where
    mykeys (XConfig {modMask = modm}) = M.fromList $
      [ ((modm, xK_x), spawn "lock-screen")
      -- Go to window by name search
      , ((modm, xK_g), windowPrompt
          def
          { searchPredicate = myFuzzyFinderFunction
          , alwaysHighlight = True
          }
          Goto
          allWindows)
      -- Bring window by name search
      , ((modm .|. shiftMask, xK_g), windowPrompt
          def { searchPredicate = myFuzzyFinderFunction }
          Bring
          allWindows)
      -- Go to emacs
      , ((modm, xK_e), runOrRaise "emacs" (className =? "Emacs"))
      ]

myFuzzyFinderFunction :: String -> String -> Bool
myFuzzyFinderFunction a b = map toLower a `isInfixOf` map toLower b
