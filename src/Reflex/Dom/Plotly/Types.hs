{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.Plotly.Types where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types (Options (..))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import           Data.Char (toLower)
import           Data.Default
import           Data.List (intercalate, stripPrefix)
import           Data.Semigroup ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           GHC.Generics
import           Reflex.Dom


------------------------------------------------------------------------------
-- customToJSON :: (Generic a, GToJSON AE.Value Zero (Rep a)) => a -> AE.Value
customToJSON
  = genericToJSON $ defaultOptions { omitNothingFields = True
                                   , fieldLabelModifier = dropPrefix
                                   }
  where
    dropPrefix = drop 1 . dropWhile (/= '_') . drop 1


------------------------------------------------------------------------------
data Color = ColorRGBA Int Int Int Int
           | ColorRGB Int Int Int

instance ToJSON Color where
  toJSON (ColorRGB r g b) = toJSON $ "rgb(" <> intercalate "," (fmap show [r, g, b]) <> ")"
  toJSON (ColorRGBA r g b a) = toJSON $ "rgba(" <> intercalate "," (fmap show [r, g, b, a]) <> ")"


------------------------------------------------------------------------------
data TraceType = Scatter | Bar | Box | Pie deriving (Eq, Ord, Show)

instance ToJSON TraceType where
  toJSON x = toJSON $ fmap toLower $ show x

instance Default TraceType where
  def = Scatter


------------------------------------------------------------------------------
data TraceMode = Lines | Markers | Text deriving (Eq, Ord, Show)
data HoverOn = Points | Fills deriving (Eq, Ord, Show)

data FlagList a = FlagListNone | FlagListSet (Set a)

instance (Show a) => ToJSON (FlagList a) where
  toJSON FlagListNone = toJSON ("none" :: Text)
  toJSON (FlagListSet xs) = toJSON $ intercalate "+" $ fmap (fmap toLower . show) $ Set.toList xs


------------------------------------------------------------------------------
data LineShape = Linear | Spline | Hv | Vh | Hvh | Vhv deriving (Eq, Ord, Show)

instance ToJSON LineShape where
  toJSON x = toJSON $ fmap toLower $ show x

data LineDash = Solid | Dot | Dash | Longdash | Dotdash | Longdashdot deriving (Eq, Ord, Show)

instance ToJSON LineDash where
  toJSON x = toJSON $ fmap toLower $ show x

data Line
  = Line { _line_color :: Maybe Color
         , _line_width :: Maybe Int
         , _line_shape :: Maybe LineShape
         , _line_smoothing :: Maybe Float
         , _line_dash :: Maybe LineDash
         , _line_simplify :: Maybe Bool
         } deriving Generic

instance ToJSON Line where
  toJSON = customToJSON


------------------------------------------------------------------------------
data Trace
  = Trace { _trace_type :: TraceType
          , _trace_x :: Maybe [AE.Value]
          , _trace_y :: Maybe [AE.Value]
          , _trace_z :: Maybe [AE.Value]
          , _trace_labels :: Maybe [Text]
          , _trace_values :: Maybe [AE.Value]
          , _trace_visible :: Maybe Bool
          , _trace_showlegend :: Maybe Bool
          , _trace_legendgroup :: Maybe Text
          , _trace_opacity :: Maybe Float
          , _trace_name :: Maybe Text
          , _trace_text :: Maybe Text
          , _trace_mode :: Maybe (FlagList TraceMode)
          , _trace_hoveron :: Maybe (FlagList HoverOn)
          } deriving Generic

makeLenses ''Trace

instance ToJSON (Trace) where
  toJSON = customToJSON

instance Default (Trace) where
  def = Trace def Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


------------------------------------------------------------------------------
data Axis
  = Axis { _axis_title :: Maybe Text
         , _axis_tickformat :: Maybe Text
         } deriving Generic

makeLenses ''Axis

instance ToJSON Axis where
  toJSON = customToJSON

instance Default Axis where
  def = Axis Nothing Nothing


------------------------------------------------------------------------------
data Font
  = Font { _font_family :: Maybe Text
         , _font_size :: Maybe Int
         , _font_color :: Maybe Color
         } deriving Generic

instance ToJSON Font where
  toJSON = customToJSON

instance Default Font where
  def = Font Nothing Nothing Nothing

------------------------------------------------------------------------------
data Margin
  = Margin { _margin_l :: Int
           , _margin_r :: Int
           , _margin_t :: Int
           , _margin_b :: Int
           , _margin_pad :: Int
           , _margin_autoexpand :: Bool
           } deriving Generic

makeLenses ''Margin

instance ToJSON Margin where
  toJSON = customToJSON

instance Default Margin where
  def = Margin { _margin_l = 80
               , _margin_r = 80
               , _margin_t = 100
               , _margin_b = 80
               , _margin_pad = 0
               , _margin_autoexpand = True
               }

uniformMargin :: Int -> Margin
uniformMargin x = def { _margin_l = x
                      , _margin_r = x
                      , _margin_t = x
                      , _margin_b = x
                      }

noMargin :: Margin
noMargin = uniformMargin 0


------------------------------------------------------------------------------
data Layout
  = Layout { _layout_xaxis :: Maybe Axis
           , _layout_yaxis :: Maybe Axis
           , _layout_zaxis :: Maybe Axis
           , _layout_font :: Maybe Font
           , _layout_title :: Maybe Text
           --, _layout_title_font :: Maybe Font
           , _layout_autosize :: Maybe Bool
           , _layout_width :: Maybe Int
           , _layout_height :: Maybe Int
           , _layout_margin :: Maybe Margin
           --, _layout_paperBgColor :: Maybe Text
           --, _layout_plotBgColor :: Maybe Text
           --, _layout_separators :: Maybe Text
           --, _layout_hideSources :: Maybe Bool
           , _layout_showlegend :: Maybe Bool
           } deriving Generic

makeLenses ''Layout

instance ToJSON Layout where
  toJSON = customToJSON

instance Default Layout where
  def = Layout Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


-----------------------------------------------------------------------------
data ModebarButton
  = ToImage
  | SendDataToCloud
  | Zoom2d
  | Pan2d
  | Select2d
  | Lasso2d
  | ZoomIn2d
  | ZoomOut2d
  | AutoScale2d
  | ResetScale2d
  | HoverClosestCartesian
  | HoverCompareCartesian
  | Zoom3d
  | Pan3d
  | OrbitRotation
  | TableRotation
  | ResetCameraDefault3d
  | ResetCameraLastSave3d
  | HoverClosest3d
  | ZoomInGeo
  | ZoomOutGeo
  | ResetGeo
  | HoverClosestGeo
  | HoverClosestGl2d
  | HoverClosestPie
  | ToggleHover
  | ResetViews
  | ToggleSpikelines
  | ResetViewMapbox
  deriving (Eq, Ord, Show)

instance ToJSON ModebarButton where
  toJSON = toJSON . lowerFirst . show

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x:xs) = (toLower x):xs

-----------------------------------------------------------------------------
data ConfigEdits
  = ConfigEdits { _configEdits_annotationPosition :: Maybe Bool
                , _configEdits_annotationTail :: Maybe Bool
                , _configEdits_annotationText :: Maybe Bool
                , _configEdits_axisTitleText :: Maybe Bool
                , _configEdits_colorbarPosition :: Maybe Bool
                , _configEdits_colorbarTitleText :: Maybe Bool
                , _configEdits_legendPosition :: Maybe Bool
                , _configEdits_legendText :: Maybe Bool
                , _configEdits_shapePosition :: Maybe Bool
                , _configEdits_titleText :: Maybe Bool
                } deriving (Generic)

makeLenses ''ConfigEdits

instance ToJSON ConfigEdits where
  toJSON = customToJSON

instance Default ConfigEdits where
  def = ConfigEdits { _configEdits_annotationPosition = Nothing
                    , _configEdits_annotationTail = Nothing
                    , _configEdits_annotationText = Nothing
                    , _configEdits_axisTitleText = Nothing
                    , _configEdits_colorbarPosition = Nothing
                    , _configEdits_colorbarTitleText = Nothing
                    , _configEdits_legendPosition = Nothing
                    , _configEdits_legendText = Nothing
                    , _configEdits_shapePosition = Nothing
                    , _configEdits_titleText = Nothing
                    }

data ModebarMode = ModebarOn | ModebarOff | ModebarHover

instance ToJSON ModebarMode where
  toJSON ModebarOn = toJSON True
  toJSON ModebarOff = toJSON False
  toJSON ModebarHover = toJSON ("hover" :: Text)

data Config
  = Config { _config_staticPlot :: Maybe Bool
           , _config_editable :: Maybe Bool
           , _config_edits :: Maybe ConfigEdits
           , _config_autosizable :: Maybe Bool
           , _config_queueLength :: Maybe Int
           , _config_fillFrame :: Maybe Bool
           , _config_frameMargins :: Maybe Int
           , _config_scrollZoom :: Maybe Bool
           , _config_doubleClick :: Maybe Text
           , _config_showTips :: Maybe Bool
           , _config_showAxisDragHandles :: Maybe Bool
           , _config_showAxisRangeEntryBoxes :: Maybe Bool
           , _config_showLink :: Maybe Bool
           , _config_sendData :: Maybe Bool
           , _config_linkText :: Maybe Text
           , _config_showSources :: Maybe Bool
           , _config_displayModeBar :: Maybe ModebarMode
           , _config_modeBarButtonsToRemove :: Maybe [ModebarButton]
           , _config_modeBarButtonsToAdd :: Maybe [AE.Value]
           , _config_modeBarButtons :: Maybe Bool
           , _config_displaylogo :: Maybe Bool
           , _config_plotGlPixelRatio :: Maybe Int
           -- , _config_setBackground :: ?
           , _config_topojsonURL :: Maybe Text
           , _config_mapboxAccessToken :: Maybe Text
           , _config_logging :: Maybe Bool
           -- , _config_globalTransforms :: ?
           } deriving Generic

makeLenses ''Config

instance ToJSON Config where
  toJSON = customToJSON

instance Default Config where
  def = Config { _config_staticPlot = Nothing
               , _config_editable = Nothing
               , _config_edits = Nothing
               , _config_autosizable = Nothing
               , _config_queueLength = Nothing
               , _config_fillFrame = Nothing
               , _config_frameMargins = Nothing
               , _config_scrollZoom = Nothing
               , _config_doubleClick = Nothing
               , _config_showTips = Nothing
               , _config_showAxisDragHandles = Nothing
               , _config_showAxisRangeEntryBoxes = Nothing
               , _config_showLink = Nothing
               , _config_sendData = Nothing
               , _config_linkText = Nothing
               , _config_showSources = Nothing
               , _config_displayModeBar = Nothing
               , _config_modeBarButtonsToRemove = Nothing
               , _config_modeBarButtonsToAdd = Nothing
               , _config_modeBarButtons = Nothing
               , _config_displaylogo = Nothing
               , _config_plotGlPixelRatio = Nothing
               -- , _config_setBackground = Nothing
               , _config_topojsonURL = Nothing
               , _config_mapboxAccessToken = Nothing
               , _config_logging = Nothing
               -- , _config_globalTransforms = Nothing
               }
