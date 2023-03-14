# StableBuffer

Stable Buffers in Motoko.

## Note - Stable Buffer v1.0.0 has just been released. 
This is a breaking change , as it now follows the `[var ?X]` array convention adapted by the [Buffer class in the motoko-base library](https://github.com/dfinity/motoko-base/blob/98c5717b283a8ec233907c3a3e92c79e3bbab1b3/src/Buffer.mo#L69) in late October '22 (previously the internal elements were `[var X]`).


## Motivation
Inspiration taken from [this back and forth in the Dfinity developer forums](https://forum.dfinity.org/t/clarification-on-stable-types-with-examples/11075).

## API Documentation

API documentation for this library can be found at https://canscale.github.io/StableBuffer

## About
  This module is a direct deconstruction of the object oriented [Buffer.mo class in motoko-base]
  (https://github.com/dfinity/motoko-base/blob/master/src/Buffer.mo)
  into a series of functions and is meant to be persistent across updates, with the tradeoff 
  being larger function signatures.

## Usage
Install vessel and ensure this is included in your package-set.dhall and vessel.dhall
```
import B "mo:stablebuffer/StableBuffer";
...

// initialize and add to a Buffer
stable let b = B.init<Nat>();
B.add(b, 5);

// initialize from an existing Array
stable let b = B.fromArray<Nat>([1,2,3,4,5]);

// for more, check out the API Documentation -> https://canscale.github.io/StableBuffer
```

## License
StableBuffer is distributed under the terms of the Apache License (Version 2.0).

See LICENSE for details.
