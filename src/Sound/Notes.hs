module Sound.Notes where

a4 :: Int
a4 = 440

nfreq :: Int -> Double
nfreq x = fromIntegral a4 * ((2 ** (1 / 12)) ** fromIntegral x)


-- Notes n' crap
rt :: Int
rt = 999

c0 :: Int
c0 = -9 - 12

cs0 :: Int
cs0 = c0 + 1

d0 :: Int
d0  = c0 + 2

e0 :: Int
e0  = c0 + 4

f0 :: Int
f0  = c0 + 5

fs0 :: Int
fs0 = c0 + 6

g0 :: Int
g0  = c0 + 7

gs0 :: Int
gs0 = c0 + 8

a0 :: Int
a0  = c0 + 9

as0 :: Int
as0 = c0 + 10

bf0 :: Int
bf0 = c0 + 10

b0 :: Int
b0  = c0 + 11

c1 :: Int
c1 = c0 + 12

cs1 :: Int
cs1 = c1 + 1

d1 :: Int
d1  = c1 + 2

e1 :: Int
e1  = c1 + 4

f1 :: Int
f1  = c1 + 5

fs1 :: Int
fs1 = c1 + 6

g1 :: Int
g1  = c1 + 7

gs1 :: Int
gs1 = c1 + 8

a1 :: Int
a1  = c1 + 9

as1 :: Int
as1 = c1 + 10

bf1 :: Int
bf1 = c1 + 10

b1 :: Int
b1  = c1 + 11
