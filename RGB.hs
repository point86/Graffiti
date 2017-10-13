module RGB
	(
		isAlphaSet,
		toRGB,
		fromRGB,
		RGB (..),
		correctRgbBounds
	) where

import Data.Bits
import Graphics.GD

data RGB = RGB {
				red 	:: Int,
				green 	:: Int,
				blue	:: Int
			}	deriving (Eq, Read, Show, Ord)

instance Num RGB where
	(RGB a b c) + (RGB x y z) = RGB (a+x) (b+y) (c+z)
	(RGB a b c) - (RGB x y z) = RGB (a-x) (b-y) (c-z)
	(RGB a b c) * (RGB x y z) = RGB (a*x) (b*y) (c*z)
	abs (RGB a b c) = RGB (a*signum a) (b*signum b) (c*signum c)
	signum (RGB a b c) = 1

isAlphaSet x	| toA x > 100 = True
				| otherwise = False
				where
					toA x = fromIntegral $ shiftR ((.&.) (16711680*256) x) 24

correctRgbBounds (RGB a b c) = RGB (corr a) (corr b) (corr c)
corr x | x >= 255 = 255
	   | x <= 0 = 0
	   | otherwise = x


toRGB :: Color -> RGB
toRGB x = RGB r g b
			where
				x' = fromIntegral x
				r = shiftR ((.&.) 16711680 x') 16
				g = shiftR ((.&.) 65280 x') 8
				b = (.&.) 255 x'

fromRGB :: RGB -> Color
fromRGB (RGB x y z) = (fromIntegral $ x'  .|.  y'  .|.  z) :: Color
					where
						x' = shiftL y 8
						y' = shiftL x 16											
