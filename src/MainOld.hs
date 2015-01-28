import           Data.Binary.Builder
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid
import           Sound.Filter
import           Sound.Instrument
import           Sound.Notes
import           Sound.Source

bpm :: Double
bpm = 148

bps :: Double
bps = bpm / 60

delay :: Double
delay = 1 / bps / 2

-- One measure per line
-- These are eigth notes
nightmare :: [Note]
nightmare = noteStates' $ cycle [
        d1,   d1, g1, a1,  g1,  g1,  d1,  d1,
        f1,   f1, d1, d1, cs1, cs1, cs1, cs1,
        d1,   d1, g1, a1,  g1,  g1,  d1,  d1,
        bf1, bf1, a1, a1,  g1,  f1,  e1,  e1
    ]

-- nightmare2 :: [Int]
-- nightmare2 = map (subtract 12) nightmare

-- Unused, will sound better with ADSR envelope
nightmare2 :: [Note]
nightmare2 = noteStates' $ cycle [
        rt,   rt, rt, rt,  rt,  rt,  rt,  rt,
        bf0, bf0, rt, rt,  g0,  g0,  g0,  g0,
        rt,   rt, rt, rt,  rt,  rt,  rt,  rt,
        bf0, bf0, a0, a0,  g0,  f0,  c1,  c1
    ]

rfact :: [Note]
rfact = noteStates $ cycle [
        fs1,  rt,  rt, fs1,  rt, fs1,  a1, gs1,
        fs1,  rt,  rt, fs1,  rt,  a1, gs1, fs1,
         f1,  rt,  rt,  f1,  rt,  f1,  f1,  e1,
         d1,  rt,  rt,  d1,  rt,  d1,  e1,  f1
    ]

rfact5 :: [Note]
rfact5 = noteStates $ cycle [
         rt, fs0, fs0,  rt, fs0,  rt,  rt,  rt,
         rt, fs0, fs0,  rt, fs0,  rt,  rt,  rt,
         rt,  f0,  f0,  rt,  f0,  rt,  rt,  rt,
         rt,  d0,  d0,  rt,  d0,  rt,  rt,  rt
    ]

rfact2 :: [Note]
rfact2 = noteStates $ cycle [
        fs0, fs0, fs0, fs0, fs0, fs0,  a0, gs0,
        fs0, fs0, fs0, fs0, fs0,  a0, gs0, fs0,
         f0,  f0,  f0,  f0,  f0,  f0,  f0,  e0,
         d0,  d0,  d0,  d0,  d0,  d0,  e0,  f0
    ]


rfact4 :: [Note]
rfact4 = noteStates $ cycle [
        fs1, fs0, fs0, fs1, fs0, fs1,  a1, gs1,
        fs1, fs0, fs0, fs1, fs0,  a1, gs1, fs1,
        f1,   f0,  f0,  f1,  f0,  f1,  f1,  e1,
        d1,   d0,  d0,  d1,  d0,  d1,  e1,  f1
    ]



-- As above, saving for ADSR envelope
rfact3 :: [Int]
rfact3 = cycle [
        fs0,  rt,  rt, fs0,  rt, fs0,  a0, gs0,
        fs0,  rt,  rt, fs0,  rt,  a0, gs0, fs0,
         f0,  rt,  rt,  f0,  rt,  f0,  f0,  e0,
         d0,  rt,  rt,  d0,  rt,  d0,  e0,  f0
    ]


noteStates x = tail (scanl states (Note (-1) False (-999)) (zip x times))
    where states old (n,t)
        --    | lNote == n && noteOn old = old
           | n == rt = if noteOn old then old { noteTime = t, noteOn = False } else old
           | otherwise = Note n True t
           where lNote = noteNum old
          times = [0,delay..]

noteStates' x = tail (scanl states (Note (-1) False (-999)) (zip x times))
    where states old (n,t)
           | lNote == n && noteOn old = old
           | n == rt = if noteOn old then old { noteTime = t, noteOn = False } else old
           | otherwise = Note n True t
           where lNote = noteNum old
          times = [0,delay..]

tmpNotes :: [Int]
tmpNotes = cycle $ map (subtract 24) [
        c1, c1, c1, c1, c1, c1,  c1, c1, c1, c1, c1, c1,
        c1, c1, c1, c1, c1, c1,  c1, c1, c1, c1, c1, c1,
        b0, b0, b0, b0, b0, b0,  b0, b0, b0, b0, b0, b0,
        a0, a0, a0, a0, a0, a0,  g0, g0, g0, g0, g0, g0,

        c1, c1, c1, c1, c1, c1,  c1, c1, c1, c1, c1, c1,
        c1, c1, c1, c1, c1, c1,  c1, c1, c1, c1, c1, c1,
        b0, b0, b0, b0, b0, b0,  b0, b0, b0, b0, b0, b0,
        a0, a0, a0, a0, a0, a0,  c1, b0, a0, g0, a0, b0
    ]

tmpNotes2 :: [Int]
tmpNotes2 = cycle $ map (subtract 0) [
        d1, d1, d1, d1, d1, d1, d1, d1,
        d1, d1, d1, d1, d1, d1, d1, d1,
        c1, c1, c1, c1, c1, c1, c1, c1,
        b0, b0, b0, b0, a0, a0, a0, a0
    ]

numSamples :: Double
numSamples = (4 * 4) / bps * 2
-- numSamples = (6 * 4) / bps * 4
-- numSamples = 8

adsrEnv = ADSREnvelope 0.03 0.02 0.8

songNote :: [Note] -> Int -> Instrument -> Source
songNote notes shift synth = Source output
 where output t = sample (synth curState) t
--         | curNote > 700 = 0.0
--         | otherwise = sample (mconcat (map (synth . (*curFreq)) [1,1])) t -- Adjust the second number to add harmonic overtones... I think
        where curNote = notes !! floor (t / delay)
              curState = curNote { noteNum = noteNum curNote + shift }

song :: [Note] -> [Note] -> Source
song notes1 notes2 = mconcat (concatMap noteFor [(s,synthInstrument $ i) | (s,i) <- instruments])
  where noteFor (s,f) = [songNote notes1 s f1, songNote notes1 (s - 12) f1, volume 1.2 (songNote notes2 s f2)]
         where f1 = withADSR env1 f
               f2 = withADSR env2 f
        env1 = adsrEnv 0.05
        env2 = adsrEnv 0.01
        instruments = [
                -- (0, squareSynth),
                -- (0, sawSynth),
                -- (-12, sineSynth),
                -- (-24, sineSynth)
                (0  , squareSynth),
                (-12, triangleSynth),
                (-24, sineSynth)
                -- (-24, sawSynth)
            ]

rfactNotes :: Source
rfactNotes = song rfact rfact5

nightmareNotes :: Source
nightmareNotes = song nightmare nightmare2
--
-- testNotes :: Source
-- testNotes = song tmpNotes (repeat rt)

-- adrsEnv = ADSREnvelope

main :: IO ()
main = do
    let source = rfactNotes
        src2 = Source (\t -> sin (700 * t + (10 * abs (t - numSamples * 0.5) * sin (350 * t))))
        freq = nfreq (c0 - 8)
        synth = mconcat [squareSynth (freq)]
        amp   = (*0.5) . (+1) . sineWave 12
        src   = Source (\t -> amp t * sample synth t)
    BL.putStr (toLazyByteString $ sourceData (volume 0.04 synth))


-- Implementation details
sampleRate :: Double
sampleRate = 44100

sourceData :: Source -> Builder
sourceData src = do
    let xs = [0,1/sampleRate..numSamples] :: [Double]
        samples = lowpass ((*400) . (* 0.5) . (+1) . (\t -> sineWave 4 t * triangleWave 3 t)) sampleRate (map (sample src) xs)
        samples' = (map (sample src) xs)
        sndData = map (fromIntegral . ampToInt) samples
    mconcat (map putWord16le sndData)
 where ampToInt x = floor (x * 32767) :: Int
