{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens ((?~))
import           Control.Monad (replicateM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom
import           Reflex.Dom.Plotly
import           System.Random (randomIO)

tshow :: Show a => a -> Text
tshow = T.pack . show

main :: IO ()
main = mainWidget app

app :: MonadWidget t m => m ()
app = do
  randomE <- button "Randomize"
  trace0 <- liftIO randomTrace
  traceE <- performEvent $ liftIO randomTrace <$ randomE
  traces <- holdDyn trace0 traceE
  el "div" $ do
    let layout = pure $ def & layout_margin ?~ noMargin
                            & layout_width ?~ 600
                            & layout_height ?~ 400
    plotlyPlotDef traces layout

  where
    genR n = replicateM n (randomIO :: IO Int)

    randomTrace = do
      let xs = [1..10] :: [Int]
          xsLen = length xs
          labels = fmap (("label-" <>) . tshow) xs
      tnum <- fmap ((`mod` 4) . abs) randomIO
      case tnum :: Int of
        0 -> do
          ys <- genR xsLen
          return $ [scatter xs ys]
        1 -> do
          ys <- genR xsLen
          return $ [bar xs ys]
        2 -> do
          ys0 <- genR xsLen
          ys1 <- genR xsLen
          return $ [boxY ys0, boxY ys1]
        3 -> do
          vs <- fmap (fmap abs) $ genR xsLen
          return $ [pie labels vs]
