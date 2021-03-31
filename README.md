# Repro for Unboxed vector map crashing with `ARR_WORDS object entered!`

On GHC 8.6.5, `stack build && stack exec repro` crashes with:

```
Unboxed.Vector (Vec World) roundtrip (does not crash in repro)
True
Unboxed.Vector (NormalVec World) roundtrip (CRASHES in repro)
repro: internal error: ARR_WORDS object entered!
    (GHC version 8.6.5 for x86_64_unknown_linux)
    Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
```

when performing a `VU.map (v VG.!) (VU.fromList [0,1,2])`.

This does not happen with GHC 8.8.2 (`lts-15.0`), or GHC 8.8.1 (checked with nix).

The `vector` version is the same among all of the above.


## What to find out

As of writing, it is unclear to me which GHC fix made the crash go away.
