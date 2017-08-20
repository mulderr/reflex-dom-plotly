{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}

module Reflex.Dom.Plotly.Internal.JavaScript where

import Control.Lens ((^.))
import Data.Aeson (ToJSON, toJSON)
import Data.Text (Text)
import GHCJS.DOM.Types (JSM, MonadJSM, JSVal, HTMLElement, liftJSM, toJSVal)

#ifndef ghcjs_HOST_OS
import Control.Monad (void)
import Language.Javascript.JSaddle.Object
#endif

import Reflex.Dom.Plotly.Types


-----------------------------------------------------------------------------
plot :: (MonadJSM m) => HTMLElement -> [Trace] -> Layout -> Maybe Config -> m ()
#ifdef ghcjs_HOST_OS
plot e traces layout mconf = liftJSM $ do
  jsTraces <- toJSVal $ toJSON traces
  jsLayout <- toJSVal $ toJSON layout
  case mconf of
    Nothing -> js_plot3 e jsTraces jsLayout
    Just conf -> do
      jsConf <- toJSVal $ toJSON conf
      js_plot4 e jsTraces jsLayout jsConf

foreign import javascript unsafe "Plotly[\"plot\"]($1, $2, $3)"
  js_plot3 :: HTMLElement -- {string id or DOM element} gd
           -> JSVal -- {array of objects} data
           -> JSVal -- {object} layout
           -> JSM ()

foreign import javascript unsafe "Plotly[\"plot\"]($1, $2, $3, $4)"
  js_plot4 :: HTMLElement -- {string id or DOM element} gd
           -> JSVal -- {array of objects} data
           -> JSVal -- {object} layout
           -> JSVal -- {object} config
           -> JSM ()
#else
plot e traces layout mconf = liftJSM $ void $ do
  jsTraces <- toJSVal $ toJSON traces
  jsLayout <- toJSVal $ toJSON traces
  plotly <- jsg "Plotly"
  case mconf of
    Nothing -> plotly ^. js3 "plot" e jsTraces jsLayout
    Just conf -> do
      jsConf <- toJSVal $ toJSON conf
      plotly ^. js4 "plot" e jsTraces jsLayout jsConf
#endif

-----------------------------------------------------------------------------
purge :: (MonadJSM m) => HTMLElement -> m ()
#ifdef ghcjs_HOST_OS
purge = liftJSM . js_purge

foreign import javascript unsafe "Plotly[\"purge\"]($1)"
  js_purge :: HTMLElement -> JSM ()
#else
purge e = liftJSM $ void $ do
  plotly <- jsg "Plotly"
  plotly ^. js1 "purge" e
#endif
