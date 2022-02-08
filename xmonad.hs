import XMonad
import XMonad.Actions.GridSelect
import XMonad.Layout.NoBorders
import qualified Data.Map as M

main :: IO ()
main = xmonad $ def
  { modMask = mod4Mask
  , keys = \c -> mykeys c `M.union` keys defaultConfig c
  , layoutHook = smartBorders $ layoutHook def
  }
  where
    mykeys (XConfig {modMask = modm}) = M.fromList $
      [ ((modm, xK_x), spawn "lock-screen")
      , ((modm, xK_g), goToSelected def)
      ]
