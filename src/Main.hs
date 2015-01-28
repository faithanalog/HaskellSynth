import           Data.Binary.Builder
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid
import           Sound.Filter
import           Sound.Instrument
import           Sound.Notes
import           Sound.Source
import System.Random

bpm :: Double
bpm = 148

bps :: Double
bps = bpm / 60

delay :: Double
delay = 1 / bps / 2

numSamples :: Double
numSamples = (4 * 4) / bps * 2

main :: IO ()
main = do
    let o   = withNotes noteFor (withHarmonics [1,2,4] triangleOsc) <> withNotes noteFor (withHarmonics [2,4] squareOsc)
        o2  = sineOsc 82.4
        fm  = modulate (amp 0.015 $ sineOsc 5) . modulate (amp 0.05 $ sineOsc 1 `mult` sineOsc 3)
        fm2 = modulate (amp 0.25 $ sineOsc (1.8 / 8))
        vol = amp 0.5
        lp  = lowpass outSampleRate $ (translate 50 . amp 450 $ range0to1 (sourceFor (negate . cosineWave 0.15)))
        lp2 = lowpass outSampleRate $ (translate 50 . amp 450 $ sourceFor lpOsc)
        rnd = randomNoise (mkStdGen 10)
        -- lp3 = lowpass outSampleRate $ Source (const (repeat 1000))
    BL.putStr (toLazyByteString $ sourceData (vol . (ampSrc $ sourceFor lpOsc) $ o2))

dMod :: (RealFrac a) => a -> a -> a
dMod a b = (a / b - whole) * b
    where whole = fromIntegral (floor (a / b) :: Int)

noteFor :: Double -> Double
noteFor t = noteFreq $ round (ctl t * 2) + 40
    where ctl t = sineWave 0.9 t * triangleWave 1.2 t

withNotes :: (Double -> Double) -> Oscillator -> Source
withNotes f o = Source impl
    where impl ts = [head $ sample (o n) [t] | (n,t) <- zip notes ts]
           where notes = map noteFor ts


lpOsc :: Double -> Double
lpOsc tIn
    | t < 0.125 / 6 = sin (3 * 8 * t * 2 * pi)
    | otherwise = 0.0
    where t = tIn `dMod` 0.25

-- Implementation details
outSampleRate :: Double
outSampleRate = 44100

sourceData :: Source -> Builder
sourceData src = do
    let xs = [0,1/outSampleRate..60] :: [Double]
        sndData = map (fromIntegral . ampToInt . max (-1) . min 1) $ sample src xs

    mconcat (map putWord16le sndData)
 where ampToInt x = truncate (x * 32767) :: Int
