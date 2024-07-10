{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

module YampaMe where

import Control.Applicative (Alternative (..))
import Control.Category
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.State (evalStateT)
import Data.Time.Clock
import Graphics.Vty (withForeColor)
import Graphics.Vty hiding (defAttr, (<|>))
import Graphics.Vty qualified as Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Image.Internal (Image (attr))
import "Yampa" FRP.Yampa hiding (Event)
import "Yampa" FRP.Yampa qualified as Yampa
import Prelude hiding (id, (.))

integration :: (Show a, RealFrac a) => (a -> a) -> a -> a -> a -> a
integration func x0 x1 precision
  | x1 > x0 = s0
  | otherwise = negate s0
  where
    x = x1 - x0
    y0 = x * func x0
    s0 = go y0 1
    go y n
      | precision1 <= precision = y
      | otherwise = next
      where
        dt = x / fromIntegral n
        dt2 = dt / 2
        y1 = (y / 2) + sum (map func $ take n $ iterate (+ dt) (x0 + dt2)) * dt2
        precision1 = abs (y - y1)
        next = go y1 (n * 2)
{-# INLINE integration #-}

type Pos = (Int, Int)

data AppState = AppState
  { background :: Picture,
    dpWH :: Pos,
    mePosition :: Pos,
    meColor :: [Color],
    timer :: UTCTime
  }
  deriving (Show)

data Sig = ChangePos Pos | Quit
  deriving (Show)

-- mkPic :: AppState -> Picture
mkPic a =
  background a
    `addToTop` uncurry
      translate
      (mePosition a)
      (me {attr = attr me `withForeColor` head (meColor a)})

initialize :: (MonadIO m, MonadState AppState m) => Vty -> m (Yampa.Event Sig)
initialize vty = do
  (dw, dh) <- liftIO $ displayBounds $ outputIface vty
  t0 <- liftIO getCurrentTime
  let (mw, mh) = (dw `div` 2 - imageWidth center `div` 2, dh `div` 2 - imageHeight center `div` 2)
      center = char (defAttr `withBackColor` red `withStyle` bold `withForeColor` yellow) '。'
      initAppState =
        AppState
          ( initBG
              `addToTop` translate
                mw
                mh
                center
          )
          (dw, dh)
          (mw, mh)
          [black, red, blue]
          t0
  put initAppState
  liftIO $ update vty $ mkPic initAppState
  return NoEvent

defAttr :: Attr
defAttr = Vty.defAttr `withBackColor` white

initBG :: Picture
initBG = emptyPicture {picBackground = Background ' ' (defAttr `withBackColor` white)}

me :: Image
me = char (defAttr `withForeColor` black) '我'

sense :: (MonadIO m, MonadState AppState m) => Vty -> Bool -> m (DTime, Maybe (Yampa.Event Sig))
sense vty _ = do
  e <- liftIO $ nextEventNonblocking vty
  t0 <- timer <$> get
  t1 <- liftIO getCurrentTime
  let dt = realToFrac (t1 `diffUTCTime` t0)
  modify (\a -> a {timer = t1})
  pure (dt, fmap eventToSig e <|> Just NoEvent)

actuate :: (MonadIO m, MonadState AppState m) => Vty -> Bool -> (Yampa.Event Sig, Yampa.Event ()) -> m Bool
actuate vty _ e = do
  when (isEvent (snd e)) $ modify (\a -> let l = meColor a in a {meColor = tail l ++ [head l]})
  t <- case fst e of
    Yampa.Event Quit -> liftIO (shutdown vty) >> pure True
    Yampa.Event (ChangePos (i, j)) -> do
      modify (\a -> let (x, y) = mePosition a in a {mePosition = (x + i, y + j)})
      pure False
    _ -> pure False
  a <- get
  liftIO $ update vty $ mkPic a `addToTop` string (defAttr `withForeColor` black) (show (mePosition a))
  pure t

eventToSig :: Event -> Yampa.Event Sig
eventToSig = \case
  EvKey (KChar 'q') [] -> Yampa.Event Quit
  EvKey KUp [] -> Yampa.Event (ChangePos (0, -1))
  EvKey KDown [] -> Yampa.Event (ChangePos (0, 1))
  EvKey KRight [] -> Yampa.Event (ChangePos (1, 0))
  EvKey KLeft [] -> Yampa.Event (ChangePos (-1, 0))
  _ -> Yampa.NoEvent

main :: IO ()
main = flip evalStateT undefined $ do
  vty <- liftIO $ mkVty defaultConfig
  reactimate (initialize vty) (sense vty) (actuate vty) (identity &&& repeatedly 1 ())
  get >>= liftIO . print
