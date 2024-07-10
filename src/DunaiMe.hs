{-# LANGUAGE LambdaCase #-}

module DunaiMe where

import Bluefin.Algae
import Bluefin.Algae.State
import Bluefin.Eff
import Bluefin.IO
import Control.Applicative (Alternative (..))
import Data.Time.Clock
import FRP.BearRiver
import FRP.BearRiver qualified as BR
import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Image.Internal (Image (attr))
import YampaMe (Pos, defAttr, initBG, me)

data Sig = ChangePos Pos | Resize Pos | SwitchMode AppMode | Adjust RGB Int | Quit
  deriving (Show)

data AppMode = Move | AdjColor RGB deriving (Show)

data RGB = R | G | B deriving (Show, Eq)

data AppState = AppState
  { background :: Picture,
    backgroundImg :: [(Pos, Image)],
    dpWH :: Pos,
    mePosition :: Pos,
    meColor :: (Int, Int, Int),
    appMode :: AppMode
  }
  deriving (Show)

mkPic :: AppState -> Picture
mkPic a =
  ((background a) {picLayers = map (uncurry (calcPos (dpWH a))) (backgroundImg a)})
    `addToBottom` translateY (h - imageHeight col) col
    `addToTop` calcPos
      (w, h)
      (mePosition a)
      (me {attr = attr me `withForeColor` linearColor r g b})
  where
    (w, h) = dpWH a
    (r, g, b) = meColor a
    f st ac c c' n = st' Graphics.Vty.<|> charFill (YampaMe.defAttr `withBackColor` c) ' ' ((n * (w - imageWidth st')) `div` 256) 1
      where
        def = YampaMe.defAttr `withForeColor` c
        defMod = YampaMe.defAttr `withBackColor` c `withForeColor` c'
        st'
          | AdjColor x <- appMode a, x == ac = string defMod st
          | otherwise = string def st
    rCol = f "[r]ed   " R (linearColor 255 0 0) (linearColor 0 255 255) r
    gCol = f "[g]reen " G (linearColor 0 255 0) (linearColor 255 0 255) g
    bCol = f "[b]lue  " B (linearColor 0 0 255) (linearColor 255 255 0) b
    col = rCol <-> gCol <-> bCol

calcPos :: Pos -> Pos -> Image -> Image
calcPos (w, h) (rx, ry) img = translate x y img
  where
    (iw, ih) = (imageWidth img, imageHeight img)
    f a b c = (a `div` 2) - (b `div` 2) + c
    x = f w iw rx
    y = f h ih ry

appLoop :: (e :> es, z1 :> es, z2 :> es) => Vty -> IOE e -> Handler (State AppState) z2 -> Handler (State UTCTime) z1 -> Eff es ()
appLoop vty io s t = do
  reactimate (initialize vty io s t) (sense vty io s t) (actuate vty io s) sf

center = char (YampaMe.defAttr `withStyle` bold `withForeColor` red) '。'

initialize :: (e :> es, z1 :> es, z2 :> es) => Vty -> IOE e -> Handler (State AppState) z2 -> Handler (State UTCTime) z1 -> Eff es (BR.Event a)
initialize vty io s t = do
  (dw, dh) <- effIO io $ displayBounds $ outputIface vty
  effIO io getCurrentTime >>= put t
  let initAppState =
        AppState
          initBG
          [((0, 0), center)]
          (dw, dh)
          (0, 0)
          (128, 128, 128)
          Move
  put s initAppState
  effIO io $ update vty $ mkPic initAppState
  return NoEvent

sense vty io s t _ = do
  e <- effIO io $ nextEventNonblocking vty
  appMode <- appMode <$> get s
  t0 <- get t
  t1 <- effIO io getCurrentTime
  let dt = realToFrac (t1 `diffUTCTime` t0)
  put t t1
  pure (dt, fmap (eventToSig appMode) e Control.Applicative.<|> Just NoEvent)

eventToSig :: AppMode -> Graphics.Vty.Event -> BR.Event Sig
eventToSig _ (EvKey (KChar 'q') []) = BR.Event Quit
eventToSig _ (EvKey (KChar 'm') []) = BR.Event (SwitchMode Move)
eventToSig _ (EvKey (KChar 'r') []) = BR.Event (SwitchMode (AdjColor R))
eventToSig _ (EvKey (KChar 'g') []) = BR.Event (SwitchMode (AdjColor G))
eventToSig _ (EvKey (KChar 'b') []) = BR.Event (SwitchMode (AdjColor B))
eventToSig _ (EvResize x y) = BR.Event (Resize (x, y))
eventToSig Move (EvKey KUp []) = BR.Event (ChangePos (0, -1))
eventToSig Move (EvKey KDown []) = BR.Event (ChangePos (0, 1))
eventToSig Move (EvKey KRight []) = BR.Event (ChangePos (2, 0))
eventToSig Move (EvKey KLeft []) = BR.Event (ChangePos (-2, 0))
eventToSig (AdjColor c) (EvKey d []) = Adjust c <$> d'
  where
    d' = case d of
      KRight -> BR.Event 2
      KLeft -> BR.Event (-2)
      _ -> BR.NoEvent
eventToSig _ _ = BR.NoEvent

-- actuate :: (z :> zz, e :> zz) => Vty -> IOE e -> Handler (State AppState) z -> p -> (BR.Event Sig, BR.Event a) -> Eff zz Bool
actuate vty io s _ e = do
  case e of
    BR.Event Quit -> pure True
    BR.Event (ChangePos (i, j)) -> do
      modify s (\a -> let (x, y) = mePosition a in a {mePosition = (x + i, y + j)})
      redraw
    BR.Event (Resize (i, j)) -> do
      modify s (\a -> a {dpWH = (i, j)})
      redraw
    BR.Event (SwitchMode m) -> do
      modify s (\a -> a {appMode = m})
      redraw
    BR.Event (Adjust c n) -> do
      let f (r, g, b) =
            case c of
              R -> (max 0 $ min 255 $ r + n, g, b)
              G -> (r, max 0 $ min 255 $ g + n, b)
              B -> (r, g, max 0 $ min 255 $ b + n)
      modify s (\a -> a {meColor = f (meColor a)})
      redraw
    _ -> pure False
  where
    redraw = do
      a <- get s
      effIO io $ update vty $ mkPic a
      pure False

sf = identity

main :: IO ()
main = runEff $ \io -> evalState undefined $ \t -> evalState undefined $ \s -> do
  vty <- effIO io $ mkVty defaultConfig
  appLoop vty io s t
  effIO io $ shutdown vty
  get s >>= effIO io . print

