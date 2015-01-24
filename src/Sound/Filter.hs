module Sound.Filter where
import Sound.Source


type Filter = Source -> Source

timeshift :: Double -> Filter
timeshift t (Source src) = Source $ src . (+t)

volume :: Double -> Filter
volume vol (Source src) = Source $ (* vol) . src

modulate :: (Double -> Double) -> Filter
modulate fmod (Source src) = Source (\t -> src (t + fmod t))

type SampleFilter = Double -> [Double] -> [Double]

lowpass :: (Double -> Double) -> SampleFilter
lowpass cutoff sr samples = scanl lpass (head samples) (tail $ zip samples [0,dt..])
    where dt = 1 / sr
          lpass y (x,t) = alpha * x + (1 - alpha) * y
            where rc = 1 / (cutoff t * 2 * pi)
                  alpha = dt / (rc + dt)
