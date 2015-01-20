import qualified Data.ByteString.Lazy as BL
import Data.Binary.Builder
import Data.Monoid
import Sound.Source
import Sound.Notes
import Sound.Filter

bpm :: Double
bpm = 148

bps :: Double
bps = bpm / 60

delay :: Double
delay = 1 / bps / 2

-- One measure per line
-- These are eigth notes
nightmare :: [Int]
nightmare = cycle $ map (+12) [
        d1,   d1, g1, a1,  g1,  g1,  d1,  d1,
        f1,   f1, d1, d1, cs1, cs1, cs1, cs1,
        d1,   d1, g1, a1,  g1,  g1,  d1,  d1,
        bf1, bf1, a1, a1,  g1,  f1,  e1,  e1
    ]

nightmare2 :: [Int]
nightmare2 = map (subtract 12) nightmare

-- Unused, will sound better with ADSR envelope
nightmare3 :: [Int]
nightmare3 = cycle $ map (+12) [
        rt,   rt, rt, rt,  rt,  rt,  rt,  rt,
        bf0, bf0, rt, rt,  g0,  g0,  g0,  g0,
        rt,   rt, rt, rt,  rt,  rt,  rt,  rt,
        bf0, bf0, a0, a0,  g0,  rt,  c1,  c1
    ]

rfact :: [Int]
rfact = cycle [
        fs1,  rt,  rt, fs1,  rt, fs1,  a1, gs1,
        fs1,  rt,  rt, fs1,  rt,  a1, gs1, fs1,
         f1,  rt,  rt,  f1,  rt,  f1,  f1,  e1,
         d1,  rt,  rt,  d1,  rt,  d1,  e1,  f1
    ]

rfact2 :: [Int]
rfact2 = cycle [
        fs0, fs0, fs0, fs0, fs0, fs0,  a0, gs0,
        fs0, fs0, fs0, fs0, fs0,  a0, gs0, fs0,
         f0,  f0,  f0,  f0,  f0,  f0,  f0,  e0,
         d0,  d0,  d0,  d0,  d0,  d0,  e0,  f0
    ]

-- As above, saving for ADSR envelope
rfact3 :: [Int]
rfact3 = cycle [
        fs0,  rt,  rt, fs0,  rt, fs0,  a0, gs0,
        fs0,  rt,  rt, fs0,  rt,  a0, gs0, fs0,
         f0,  rt,  rt,  f0,  rt,  f0,  f0,  e0,
         d0,  rt,  rt,  d0,  rt,  d0,  e0,  f0
    ]

numSamples :: Double
numSamples = (4 * 4) / bps * 2

songNote :: [Int] -> Int -> Synth -> Source
songNote notes shift synth = Source output
 where output t
         | curNote > 700 = 0.0
         | otherwise = sample (mconcat (map (synth . (*curFreq)) [1..1])) t -- Adjust the second number to add harmonic overtones... I think
        where curNote = notes !! floor (t / delay)
              curFreq = nfreq (curNote + shift)

song :: [Int] -> [Int] -> Source
song notes1 notes2 = mconcat (map noteFor instruments)
  where noteFor (s,f) = songNote notes1 s f <> songNote notes2 s f
        instruments = [
                (0  , squareSynth),
                (-12, triangleSynth),
                (-24, triangleSynth),
                (-24, sawSynth)
            ]

rfactNotes :: Source
rfactNotes = song rfact rfact2

nightmareNotes :: Source
nightmareNotes = song nightmare nightmare2

main :: IO ()
main = do
    let source = rfactNotes
    BL.putStr (toLazyByteString $ sourceData (volume 0.035 source))


-- Implementation details
sampleRate :: Double
sampleRate = 44100

sourceData :: Source -> Builder
sourceData src = do
    let xs = [0,1/sampleRate..numSamples] :: [Double]
        sndData = map (fromInteger . ampToInt . sample src) xs
    mconcat (map putWord16le sndData)
 where ampToInt x = floor (x * 32767)
