module Sound.Source where
import Data.Monoid
import Data.List
import System.Random

-- A source takes a time 't' and returns the amplitude of the source
-- at that time. 't' is a time in seconds, representing the current
-- time where 0.0 is the start of the audio data
newtype Source = Source { sample :: [Double] -> [Double] }

instance Monoid (Source) where
    mempty = Source (const (repeat 0.0))
    mappend (Source f) (Source g) = Source (\ts -> zipWith (+) (f ts) (g ts))
    mconcat srcs = Source (\ts -> map sum . transpose . map (`sample` ts) $ srcs)

type Oscillator = (Double -> Source)

randomNoise :: StdGen -> Source
randomNoise rnd = Source (\_ -> randomRs (-1.0,1.0) rnd)

source :: Double -> Source
source = Source . const . repeat

oscFor :: (Double -> Double -> Double) -> Oscillator
oscFor f = Source . map . f

sourceFor :: (Double -> Double) -> Source
sourceFor f = Source (map f)

sineOsc :: Oscillator
sineOsc = oscFor sineWave

cosineOsc :: Oscillator
cosineOsc = oscFor cosineWave

sawOsc :: Oscillator
sawOsc = oscFor sawWave

triangleOsc :: Oscillator
triangleOsc = oscFor triangleWave

squareOsc :: Oscillator
squareOsc = oscFor squareWave

withHarmonics :: [Double] -> Oscillator -> Oscillator
withHarmonics harmonics o hz = mconcat . map (o . (*hz)) $ harmonics

mult :: Source -> Source -> Source
mult (Source f) (Source g) = Source (\t -> zipWith (*) (f t) (g t))

quantizeOsc :: Double -> Oscillator -> Oscillator
quantizeOsc step o hz = quantize step (o hz)

quantize :: Double -> Source -> Source
quantize step (Source src) = Source $ map impl . src
    where impl t = fromIntegral (floor (t * fact) :: Int) * step
          fact = 1 / step

range0to1 :: Source -> Source
range0to1 (Source src) = Source $ map (\x -> x * 0.5 + 0.5) . src

sineWave :: Double -> Double -> Double
sineWave freq t = sin (freq * t * 2 * pi)

cosineWave :: Double -> Double -> Double
cosineWave freq t = cos (freq * t * 2 * pi)

sawWave :: Double -> Double -> Double
sawWave freq t = saw (freq * t)
 where saw x = 2 * (x - fromIntegral (floor (0.5 + x) :: Int))

triangleWave :: Double -> Double -> Double
triangleWave freq t = 2 * abs (sawWave freq t) - 1

squareWave :: Double -> Double -> Double
squareWave freq t = 1 - 2 * fromIntegral (2 * floor ft - floor (2 * ft) + 1 :: Int)
    where ft = freq * t
