module Sound.Filter where
import Sound.Source


type Filter = Source -> Source

sampleRate :: [Double] -> Int
sampleRate (a:b:_) = floor (1 / (b - a))
sampleRate _ = undefined

delta :: Num a => [a] -> a
delta (a:b:_) = b - a
delta _ = undefined

amp :: Double -> Filter
amp vol (Source src) = Source $ map (*vol) . src

ampSrc :: Source -> Filter
ampSrc (Source vol) (Source src) = Source (\ts -> zipWith (*) (vol ts) (src ts))

phaseshift :: Double -> Filter
phaseshift t (Source src) = Source $ src . map (+t)

translate :: Double -> Filter
translate y (Source src) = Source $ map (+y) . src

modulate :: Source -> Filter
modulate (Source fmod) (Source src) = Source (\ts -> src $ zipWith (+) ts (fmod ts))

-- lowpass :: (Double -> Double) -> SampleFilter
-- lowpass cutoff sr samples = scanl lpass (head samples) (tail $ zip samples [0,dt..])
--     where dt = 1 / sr
--           lpass y (x,t) = alpha * x + (1 - alpha) * y
--             where rc = 1 / (cutoff t * 2 * pi)
--                   alpha = dt / (rc + dt)

lowpass :: Double -> Source -> Filter
lowpass sr (Source cutoff) (Source src) = Source impl
 where impl ts = tail $ scanl lpass 0.0 (zip (cutoff ts) (src ts))
        where dt = 1 / sr
              lpass y (cut,x) = alpha * x + (1 - alpha) * y -- y + alpha * (x - y)
               where rc = 1 / cut
                     alpha = dt / (rc + dt)
