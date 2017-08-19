{-# LANGUAGE JavaScriptFFI #-}

module Reflex.Dom.Plotly.Internal.JavaScript where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON, toJSON)
import GHCJS.DOM.Types (JSVal, JSString, HTMLElement, toJSString, toJSVal, pToJSVal)
import Data.Text (Text)

import Reflex.Dom.Plotly.Types


-----------------------------------------------------------------------------
foreign import javascript unsafe "Plotly[\"plot\"]($1, $2, $3)"
  js_plot3 :: HTMLElement -- {string id or DOM element} gd
           -> JSVal -- {array of objects} data
           -> JSVal -- {object} layout
           -> IO ()

foreign import javascript unsafe "Plotly[\"plot\"]($1, $2, $3, $4)"
  js_plot4 :: HTMLElement -- {string id or DOM element} gd
           -> JSVal -- {array of objects} data
           -> JSVal -- {object} layout
           -> JSVal -- {object} config
           -> IO ()


plot :: (MonadIO m) => HTMLElement -> [Trace] -> Layout -> Maybe Config -> m ()
plot e traces layout mconf = liftIO $ do
  jsTraces <- toJSVal $ toJSON traces
  jsLayout <- toJSVal $ toJSON layout
  case mconf of
    Nothing -> js_plot3 e jsTraces jsLayout
    Just conf -> do
      jsConf <- toJSVal $ toJSON conf
      js_plot4 e jsTraces jsLayout jsConf


-----------------------------------------------------------------------------
foreign import javascript unsafe "Plotly[\"purge\"]($1)"
  js_purge :: HTMLElement -> IO ()

purge :: (MonadIO m) => HTMLElement -> m ()
purge = liftIO . js_purge
