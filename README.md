#Haskell Synth
An effort to learn about software audio synthesis

---

Audio data is output raw to STDOUT, so to play it pipe it to a program which can
handle that data. I prefer to use the program 'aplay' like so:

`dist/build/gensound/gensound | aplay -r 44100 -f S16_LE`
