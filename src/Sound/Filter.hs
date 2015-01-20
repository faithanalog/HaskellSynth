module Sound.Filter where
import Sound.Source


type Filter = Source -> Source

timeshift :: Double -> Filter
timeshift t (Source src) = Source $ src . (+t)

volume :: Double -> Filter
volume vol (Source src) = Source $ (* vol) . src

modulate :: (Double -> Double) -> Filter
modulate fmod (Source src) = Source (\t -> src (t + fmod t))
