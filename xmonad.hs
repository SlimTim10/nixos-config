import XMonad
import XMonad.Actions.GridSelect
import qualified Data.Map as M

main :: IO ()
main = xmonad $ def
  { modMask = mod4Mask
  , keys = \c -> mykeys c `M.union` keys defaultConfig c
  }
  where
    mykeys (XConfig {modMask = modm}) = M.fromList $
      [ ((modm, xK_x), spawn "lock-screen")
      , ((modm, xK_g), goToSelected def)
      ]
