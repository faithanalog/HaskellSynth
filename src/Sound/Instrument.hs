module Sound.Instrument where
import           Sound.Source
import Data.Monoid


data Note = Note { noteNum  :: Int
                 , noteOn   :: Bool
                 , noteTime :: Double
                 } deriving (Eq,Show)
type Instrument = Note -> Source


-- Memoized since it will often be called a whole lot
noteFreq :: Int -> Double
noteFreq = (map impl [0..] !!)
    where note x = fromIntegral (x :: Int)
          impl x = 440 * ((2 ** (1 / 12)) ** note (x - 69))


oscInstrument :: Oscillator -> Instrument
oscInstrument osc (Note n on _)
    | on = osc (noteFreq n)
    | otherwise = mempty

-- Attack Decay Sustain Release envelope.
-- http://en.wikipedia.org/wiki/Synthesizer#ADSR_envelope
withADSR :: ADSREnvelope -> Instrument -> Instrument
withADSR (ADSREnvelope atk dcy stn rls) instr nt@(Note _ on time) = Source impl
    where impl ts = zipWith (*) (map (clamp . envelope) ts) (sample (instr nt{noteOn = True}) ts)
          clamp = max 0.0 . min 1.0
          envelope t
           | not on = r
           | to <= atk = a
           | to <= atk + dcy = d
           | otherwise = stn
           where to = t - time
                 a  = 1.0 - ((atk - to) / atk)
                 d  = (1.0 - ((atk + dcy - to) / dcy)) * (1.0 - stn) + stn
                 r  = ((rls - to) / rls) * stn

data ADSREnvelope = ADSREnvelope { attackTime   :: Double
                                 , decayTime    :: Double
                                 , sustainLevel :: Double
                                 , releaseTime  :: Double
                                 } deriving (Eq,Show)
