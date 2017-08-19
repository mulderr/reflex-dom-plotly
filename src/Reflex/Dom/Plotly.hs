{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.Plotly
  ( module Reflex.Dom.Plotly
  , module Reflex.Dom.Plotly.Types
  ) where

import Control.Lens ((?~))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON, toJSON)
import Data.Text (Text)
import qualified GHCJS.DOM.Types as DOM
import Reflex.Dom

import Reflex.Dom.Plotly.Internal.JavaScript
import Reflex.Dom.Plotly.Types


-----------------------------------------------------------------------------
scatter :: (ToJSON a, ToJSON b) => [a] -> [b] -> Trace
scatter x y = def & trace_type .~ Scatter
                  & trace_x ?~ fmap toJSON x
                  & trace_y ?~ fmap toJSON y

bar :: (ToJSON a, ToJSON b) => [a] -> [b] -> Trace
bar x y = def & trace_type .~ Bar
              & trace_x ?~ fmap toJSON x
              & trace_y ?~ fmap toJSON y

boxX :: (ToJSON a) => [a] -> Trace
boxX x = def & trace_type .~ Box
             & trace_x ?~ fmap toJSON x

boxY ::  (ToJSON a) => [a] -> Trace
boxY y = def & trace_type .~ Box
             & trace_y ?~ fmap toJSON y

pie :: (ToJSON a) => [Text] -> [a] -> Trace
pie labels values = def & trace_type .~ Pie
                        & trace_labels ?~ labels
                        & trace_values ?~ fmap toJSON values


-----------------------------------------------------------------------------
plotlyPlotStatic :: forall t m . (DomBuilder t m, MonadIO (Performable m)
  , PerformEvent t m, PostBuild t m, Reflex t, TriggerEvent t m
  , DOM.IsGObject (RawElement (DomBuilderSpace m)))
  => [Trace] -> Layout -> m ()
plotlyPlotStatic traces layout = plotlyPlotDef (pure traces) (pure layout)


plotlyPlotDef :: forall t m . (DomBuilder t m, MonadIO (Performable m)
  , PerformEvent t m, PostBuild t m, Reflex t, TriggerEvent t m
  , DOM.IsGObject (RawElement (DomBuilderSpace m)))
  => Dynamic t [Trace] -> Dynamic t Layout -> m ()
plotlyPlotDef = plotlyPlot Nothing


plotlyPlot :: forall t m . (DomBuilder t m, MonadIO (Performable m)
  , PerformEvent t m, PostBuild t m, Reflex t, TriggerEvent t m
  , DOM.IsGObject (RawElement (DomBuilderSpace m)))
  => Maybe Config -> Dynamic t [Trace] -> Dynamic t Layout -> m ()
plotlyPlot config tracesDyn layoutDyn = do
  -- TODO: cleanup if/when reflex-dom gets mount state reporting
  (plotDiv, pb :: Event t ()) <- el' "div" $ getPostBuild >>= delay 0
  let state = zipDynWith (,) tracesDyn layoutDyn
      initE = tag (current state) pb
      plotElem = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw plotDiv
  performEvent_ $ ffor initE $ \(traces, layout) -> plot plotElem traces layout config
  performEvent_ $ ffor (updated state) $ \(traces, layout) -> do
    purge plotElem
    plot plotElem traces layout config
