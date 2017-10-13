-- Incolla una foto all'interno di un'altra... se usato tra un'immagine di un muro ed il 
-- contorno di un viso (o scritta.. chiamata MASK) emula l'effetto di un murales.
-- Prima calcola il colore medio dell'immagine di base, poi colora i punti determinati 
-- da mask con il colore del pixel + (RGB NERO - coloreMedio).

import RGB
import Graphics.GD
import Control.Monad.Trans
import Control.Monad

faceColor = RGB 0 0 0 

main = do
	-- si assume che immagine img sia piu estesa di mask!
	mask <- loadPngFile "/home/paolo/haskell/graffiti/logo4.png"
	img <- loadJpegFile "/home/paolo/haskell/graffiti/wall3good.jpg"
	size <- imageSize img
	allpoints <- return $ allPixels size
	resultPix <- newImage size

	midColor <- getMidColor img
		
	mapM_ (drawPixel resultPix img mask (faceColor - midColor)) allpoints 
	
	saveJpegFile 95 "/home/paolo/haskell/graffiti/result" resultPix



drawPixel :: Image -> Image -> Image -> RGB -> Point -> IO ()
drawPixel res img mask differ p = do
											c <- getPixel p img >>= return . toRGB
											maskBit <- (getPixel p mask) >>= return . isAlphaSet
											setPixel p (colorCalc c maskBit differ)  res

colorCalc :: RGB -> Bool -> RGB -> Color
colorCalc col True  differ = fromRGB col
colorCalc col False differ = fromRGB $ correctRgbBounds $ col + differ
									


getMidColor :: Image -> IO RGB
getMidColor img = do
					size <- imageSize img
					list <- (mapM (\p -> getPixel p img) (tuttaRigaVert size))
					return $ medianRGB (map toRGB list)

medianRGB :: [RGB] -> RGB
medianRGB list = RGB r g b
			where			
				len = fromIntegral $ length list
				r = sum (map red list) `div` len
				g = sum (map green list) `div` len
				b = sum (map blue list) `div` len			


allPixels :: Size -> [Point]
allPixels (w,h) = [(x,y) | x <- [0..(w-1)], y <- [0..(h-1)]]


tuttaRigaVert :: (Int,Int) -> [(Int,Int)]
tuttaRigaVert (w,h) = [(x,y) | y <- [0..h]]
					where
						x = w `div` 2


