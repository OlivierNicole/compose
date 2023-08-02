This program takes two musical works in input and outputs a new musical work in
invention form (a baroque keyboard form with the melody rocking back and forth
between the upper and lower voice and some degree of imitation between the
voices), trying to mimic the style of the input works. It uses the techniques
described by David Cope in his book *Computers and Musical Style*, 1991, chapter 4.
The general idea is to look for motives that appear in both works (called
"signatures") and to recombine signatures to compose a new piece of music while
adhering to some constraints.

Disclaimer: this is an extremely simple program. Used correctly, it will produce
music that is "correct" tonal Western music in a very loose sense of the term
(e.g., no dissonance, cadential ending), but certainly not successful from a
compositor's point of view. This simple program does not give an accurate image
of David Cope's work on automated composition, which is far deeper and more
complex.

An example output can be found in the `examples/` subdirectory. It is the result
of running the program on the two (manually simplified) Bach inventions nos. 1
and 5 present in `examples/` as MIDI files.

## Build, run and/or install

```
# Installing
opam pin add llama_midi https://github.com/gridbugs/llama.git#0.1.0
opam install .

# ... or simply building and running
opam pin add llama_midi https://github.com/gridbugs/llama#0.1.0
opam install . --deps-only
dune exec -- compose <args...>
```

## Usage

```
compose <input work 1> <input work 2> -o <output file>
  -o Output file name
  --seed Random seed
  --debug Comma-separated list of passes to enable debug output for
  --motive-size Size of signatures to look for in the input works (default 4)
  --min-length Minimum length of the output piece in beats. The true length may be slightly  greater because of the requirement to end on a cadence. (default 29)
  --track-index-1 Index of the track to analyze in first input work (default 0)
  --track-index-2 Index of the track to analyze in second input work (default 0)
  -help  Display this list of options
  --help  Display this list of options
```

The input works should be in MIDI format, and so will be the output. For the
output to be even remotely satisfactory, the input works should verify a number
of conditions:

- They must be in C major
- Ornamentations and accidentals should be removed (you can try not to, but
  pattern matching may yield an very small number of common motives, which will
  show in the result)
