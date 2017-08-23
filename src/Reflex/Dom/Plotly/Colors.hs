module Reflex.Dom.Plotly.Colors where

import Reflex.Dom.Plotly.Types (Color (..))


------------------------------------------------------------------------------
mutedBlue :: Color
mutedBlue = ColorRGB 0x1f 0x77 0xb4

safetyOrange :: Color
safetyOrange = ColorRGB 0xff 0x7f 0x0e

cookedAsparagusGreen :: Color
cookedAsparagusGreen = ColorRGB 0x2c 0xa0 0x2c

brickRed :: Color
brickRed = ColorRGB 0xd6 0x27 0x28

mutatedPurple :: Color
mutatedPurple = ColorRGB 0x94 0x67 0xbd

chestnutBrown :: Color
chestnutBrown = ColorRGB 0x8c 0x56 0x4b

raspberryYogurtPink :: Color
raspberryYogurtPink = ColorRGB 0xe3 0x77 0xc2

middleGray :: Color
middleGray = ColorRGB 0x7f 0x7f 0x7f

curryYellowGreen :: Color
curryYellowGreen = ColorRGB 0xbc 0xbd 0x22

blueTeal :: Color
blueTeal = ColorRGB 0x17 0xbe 0xcf


defaultColors :: [Color]
defaultColors =
  [ mutedBlue
  , safetyOrange
  , cookedAsparagusGreen
  , brickRed
  , mutatedPurple
  , chestnutBrown
  , raspberryYogurtPink
  , middleGray
  , curryYellowGreen
  , blueTeal
  ]
