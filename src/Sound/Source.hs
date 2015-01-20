module Sound.Source where
import Data.Monoid

-- A source takes a time 't' and returns the amplitude of the source
-- at that time. 't' is a time in seconds, representing the current
-- time where 0.0 is the start of the audio data
newtype Source = Source { sample :: Double -> Double }

instance Monoid (Source) where
    mempty = Source (const 0.0)
    mappend (Source f) (Source g) = Source (\t -> f t + g t)
    mconcat srcs = Source (\t -> foldr (\(Source f) x -> f t + x) 0.0 srcs)


type Synth = (Double -> Source)

sineSynth :: Double -> Source
sineSynth = Source . sineWave

sawSynth :: Double -> Source
sawSynth = Source . sawWave

triangleSynth :: Double -> Source
triangleSynth = Source . triangleWave

squareSynth :: Double -> Source
squareSynth = Source . squareWave

sineWave :: Double -> Double -> Double
sineWave freq t = sin (freq * t * 2 * pi)

sawWave :: Double -> Double -> Double
sawWave freq t = saw (freq * t)
 where saw x = 2 * (x - fromInteger (floor (0.5 + x)))

triangleWave :: Double -> Double -> Double
triangleWave freq t = 2 * abs (sawWave freq t) - 1

squareWave :: Double -> Double -> Double
squareWave freq t
  | s < 0 = -1
  | otherwise = 1
 where s = sineWave freq t
