/// Generic, extensible buffers
///
/// `StableBuffer<X>` is adapted directly from https://github.com/dfinity/motoko-base/blob/master/src/Buffer.mo,
/// ripping all functions and instance variables out of the `Buffer` class in order to make a stable, persistent
/// buffer.
///
/// Generic, mutable sequences that grow to accommodate arbitrary numbers of elements.
///
/// `StableBuffer<X>` provides extensible, mutable sequences of elements of type `X`.
/// that can be efficiently produced and consumed with imperative code.
/// A buffer object can be extended by a single element or the contents of another buffer object.
///
/// When required, the current state of a buffer object can be converted to a fixed-size array of its elements.
///
/// Buffers complement Motoko's non-extensible array types
/// (arrays do not support efficient extension, because the size of an array is
/// determined at construction and cannot be changed).

import Prim "mo:⛔";
import Result "mo:base/Result";
import Order "mo:base/Order";
import Array "mo:base/Array";

module {
  type Order = Order.Order;

  // The following constants are used to manage the capacity.
  // The length of `elements` is increased by `INCREASE_FACTOR` when capacity is reached.
  // The length of `elements` is decreased by `DECREASE_FACTOR` when capacity is strictly less than
  // `DECREASE_THRESHOLD`.

  // INCREASE_FACTOR = INCREASE_FACTOR_NUME / INCREASE_FACTOR_DENOM (with floating point division)
  // Keep INCREASE_FACTOR low to minimize cycle limit problem
  private let INCREASE_FACTOR_NUME = 3;
  private let INCREASE_FACTOR_DENOM = 2;
  private let DECREASE_THRESHOLD = 4; // Don't decrease capacity too early to avoid thrashing
  private let DECREASE_FACTOR = 2;
  private let DEFAULT_CAPACITY = 8;

  private func newCapacity(oldCapacity : Nat) : Nat {
    if (oldCapacity == 0) {
      1;
    } else {
      // calculates ceil(oldCapacity * INCREASE_FACTOR) without floats
      ((oldCapacity * INCREASE_FACTOR_NUME) + INCREASE_FACTOR_DENOM - 1) / INCREASE_FACTOR_DENOM;
    };
  };

  public type StableBuffer<X> = {
    initCapacity : Nat;
    var count : Nat;
    var elems : [var ?X];
  };

  /// Initializes a buffer of given initial capacity. Note that this capacity is not realized until an element
  /// is added to the buffer.
  public func initPresized<X>(initCapacity : Nat) : StableBuffer<X> = {
    initCapacity = initCapacity;
    var count = 0;
    var elems = Prim.Array_init(initCapacity, null);
  };

  /// Initializes a buffer of initial capacity 0. When the first element is added the size will grow to one
  public func init<X>() : StableBuffer<X> = {
    initCapacity = 0;
    var count = 0;
    var elems = [var];
  };

  /// Adds a single element to the end of the buffer, doubling
  /// the size of the array if capacity is exceeded.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// StableBuffer.add(buffer, 0); // add 0 to buffer
  /// StableBuffer.add(buffer, 1);
  /// StableBuffer.add(buffer, 2);
  /// StableBuffer.add(buffer, 3); // causes underlying array to increase in capacity
  /// StableBuffer.toArray(buffer) // => [0, 1, 2, 3]
  /// ```
  ///
  /// Amortized Runtime: O(1), Worst Case Runtime: O(size)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size)
  public func add<X>(buffer : StableBuffer<X>, element : X) {
    if (buffer.count == buffer.elems.size()) {
      reserve(buffer, newCapacity(buffer.elems.size()));
    };
    buffer.elems[buffer.count] := ?element;
    buffer.count += 1;
  };

  /// Removes and returns the element at `index` from the buffer.
  /// All elements with index > `index` are shifted one position to the left.
  /// This may cause a downsizing of the array.
  ///
  /// Traps if index >= size.
  ///
  /// WARNING: Repeated removal of elements using this method is ineffecient
  /// and might be a sign that you should consider a different data-structure
  /// for your use case.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// StableBuffer.add(buffer, 10);
  /// StableBuffer.add(buffer, 11);
  /// StableBuffer.add(buffer, 12);
  /// let x = StableBuffer.remove(buffer, 1); // evaluates to 11. 11 no longer in list.
  /// StableBuffer.toArray(buffer) // => [10, 12]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size)
  public func remove<X>(buffer : StableBuffer<X>, index : Nat) : X {
    if (index >= buffer.count) {
      Prim.trap "Buffer index out of bounds in remove";
    };

    let element = buffer.elems[index];

    // copy elements to new array and shift over in one pass
    if ((buffer.count - 1) : Nat < buffer.elems.size() / DECREASE_THRESHOLD) {
      let elements2 = Prim.Array_init<?X>(buffer.elems.size() / DECREASE_FACTOR, null);

      var i = 0;
      var j = 0;
      label l while (i < buffer.count) {
        if (i == index) {
          i += 1;
          continue l;
        };

        elements2[j] := buffer.elems[i];
        i += 1;
        j += 1;
      };
      buffer.elems := elements2;
    } else {
      // just shift over elements
      var i = index;
      while (i < (buffer.count - 1 : Nat)) {
        buffer.elems[i] := buffer.elems[i + 1];
        i += 1;
      };
      buffer.elems[buffer.count - 1] := null;
    };

    buffer.count -= 1;

    switch (element) {
      case (?element) {
        element;
      };
      case null {
        Prim.trap "Malformed buffer in remove";
      };
    };
  };

  /// Removes and returns the last item in the buffer or `null` if
  /// the buffer is empty.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// StableBuffer.add(buffer, 10);
  /// StableBuffer.add(buffer, 11);
  /// StableBuffer.removeLast(buffer); // => ?11
  /// ```
  ///
  /// Amortized Runtime: O(1), Worst Case Runtime: O(size)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size)
  public func removeLast<X>(buffer : StableBuffer<X>) : ?X {
    if (buffer.count == 0) {
      return null;
    };

    buffer.count -= 1;
    let lastElement = buffer.elems[buffer.count];
    buffer.elems[buffer.count] := null;

    if (buffer.count < buffer.elems.size() / DECREASE_THRESHOLD) {
      // FIXME should this new capacity be a function of count
      // instead of the current capacity? E.g. count * INCREASE_FACTOR
      reserve(buffer, buffer.elems.size() / DECREASE_FACTOR);
    };

    lastElement;
  };

  /// Adds all elements in buffer `b` to this buffer.
  ///
  /// ```motoko include=initialize
  /// let buffer1 = StableBuffer.initPresized<Nat>(2);
  /// let buffer2 = StableBuffer.initPresized<Nat>(2);
  /// StableBuffer.add(buffer1, 10);
  /// StableBuffer.add(buffer1, 11);
  /// StableBuffer.add(buffer2, 12);
  /// StableBuffer.add(buffer2, 13);
  /// StableBuffer.append(buffer1, buffer2); // adds elements from buffer2 to buffer1
  /// StableBuffer.toArray(buffer1) // => [10, 11, 12, 13]
  /// ```
  ///
  /// Amortized Runtime: O(size2), Worst Case Runtime: O(size1 + size2)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size1 + size2)
  public func append<X>(buffer : StableBuffer<X>, buffer2 : StableBuffer<X>) {
    let size2 = size(buffer2);

    // Make sure you only allocate a new array at most once
    if (buffer.count + size2 > buffer.elems.size()) {
      // FIXME would be nice to have a tabulate for var arrays here
      reserve(buffer, newCapacity(buffer.count + size2));
    };
    var i = 0;
    while (i < size2) {
      buffer.elems[buffer.count + i] := getOpt(buffer2, i);
      i += 1;
    };

    buffer.count += size2;
  };

  /// Returns the current number of elements in the buffer.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// size(buffer) // => 0
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func size<X>(buffer : StableBuffer<X>) : Nat = buffer.count;

  /// Returns the capacity of the buffer (the length of the underlying array).
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// let buffer = StableBuffer.initPresized<Nat>(2); // underlying array has capacity 2
  /// StableBuffer.add(buffer, 10);
  /// let c1 = StableBuffer.capacity(buffer); // => 2
  /// StableBuffer.add(buffer, 11);
  /// StableBuffer.add(buffer, 12); // causes capacity to increase by factor of 1.5
  /// let c2 = StableBuffer.capacity(buffer); // => 3
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func capacity<X>(buffer : StableBuffer<X>) : Nat = buffer.elems.size();

  /// Changes the capacity to `capacity`. Traps if `capacity` < `size`.
  ///
  /// ```motoko include=initialize
  ///
  /// StableBuffer.reserve(buffer, 4);
  /// StableBuffer.add(buffer, 10);
  /// StableBuffer.add(buffer, 11);
  /// StableBuffer.capacity(buffer); // => 4
  /// ```
  ///
  /// Runtime: O(capacity)
  ///
  /// Space: O(capacity)
  public func reserve<X>(buffer : StableBuffer<X>, capacity : Nat) {
    if (capacity < buffer.count) {
      Prim.trap "capacity must be >= size in reserve";
    };

    let elements2 = Prim.Array_init<?X>(capacity, null);

    var i = 0;
    while (i < buffer.count) {
      elements2[i] := buffer.elems[i];
      i += 1;
    };
    buffer.elems := elements2;
  };

  /// Resets the buffer. Capacity is set to 8.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// StableBuffer.add(buffer, 10);
  /// StableBuffer.add(buffer, 11);
  /// StableBuffer.add(buffer, 12);
  /// StableBuffer.clear(buffer, ); // buffer is now empty
  /// StableBuffer.toArray(buffer) // => []
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func clear<X>(buffer : StableBuffer<X>) {
    buffer.count := 0;
    reserve(buffer, DEFAULT_CAPACITY);
  };

  /// Returns a copy of `buffer`, with the same capacity.
  ///
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// StableBuffer.add(buffer, 1);
  ///
  /// let clone = StableBuffer.clone(buffer);
  /// StableBuffer.toArray(clone); // => [1]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func clone<X>(buffer : StableBuffer<X>) : StableBuffer<X> {
    let newBuffer = initPresized<X>(capacity(buffer));
    for (element in vals(buffer)) {
      add(newBuffer, element);
    };
    newBuffer;
  };

  /// Returns an Iterator (`Iter`) over the elements of this buffer.
  /// Iterator provides a single method `next()`, which returns
  /// elements in order, or `null` when out of elements to iterate over.
  ///
  /// ```motoko include=initialize
  ///
  /// StableBuffer.add(buffer, 10);
  /// StableBuffer.add(buffer, 11);
  /// StableBuffer.add(buffer, 12);
  ///
  /// var sum = 0;
  /// for (element in StableBuffer.vals(buffer, )) {
  ///   sum += element;
  /// };
  /// sum // => 33
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func vals<X>(buffer : StableBuffer<X>) : { next : () -> ?X } = object {
    // FIXME either handle modification to underlying list
    // or explicitly warn users in documentation
    var nextIndex = 0;
    public func next() : ?X {
      if (nextIndex >= buffer.count) {
        return null;
      };
      let nextElement = buffer.elems[nextIndex];
      nextIndex += 1;
      nextElement;
    };
  };

  /// Creates a buffer containing elements from `array`.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// let array = [2, 3];
  ///
  /// let buf = StableBuffer.fromArray<Nat>(array); // => [2, 3]
  /// StableBuffer.toText(buf, Nat.toText);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func fromArray<X>(array : [X]) : StableBuffer<X> {
    // When returning new buffer, if possible, set the capacity
    // to the capacity of the old buffer. Otherwise, return them
    // at 2/3 capacity (like in this case). Alternative is to
    // calculate what the size would be if the elements were
    // sequentially added using `add`. This current strategy (2/3)
    // is the upper bound of that calculation (if the last element
    // added caused a capacity increase).
    let newBuffer = initPresized<X>(newCapacity(array.size()));

    for (element in array.vals()) {
      add(newBuffer, element);
    };

    newBuffer;
  };

  /// Creates an array containing elements from `buffer`.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// StableBuffer.add(buffer, 1);
  /// StableBuffer.add(buffer, 2);
  /// StableBuffer.add(buffer, 3);
  ///
  /// StableBuffer.toArray<Nat>(buffer); // => [1, 2, 3]
  ///
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func toArray<X>(buffer : StableBuffer<X>) : [X] =
  // immutable clone of array
  Prim.Array_tabulate<X>(
    size(buffer),
    func(i : Nat) : X { get(buffer, i) },
  );

  /// Creates a mutable array containing elements from `buffer`.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// StableBuffer.add(buffer, 1);
  /// StableBuffer.add(buffer, 2);
  /// StableBuffer.add(buffer, 3);
  ///
  /// StableBuffer.toVarArray<Nat>(buffer); // => [1, 2, 3]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func toVarArray<X>(buffer : StableBuffer<X>) : [var X] {
    let count = size(buffer);
    if (count == 0) { [var] } else {
      let newArray = Prim.Array_init<X>(count, get(buffer, 0));
      var i = 1;
      while (i < count) {
        newArray[i] := get(buffer, i);
        i += 1;
      };
      newArray;
    };
  };

  /// Returns the element at index `index`. Traps if  `index >= size`. Indexing is zero-based.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// StableBuffer.add(buffer,10);
  /// StableBuffer.add(buffer,11);
  /// StableBuffer.get(buffer,0); // => 10
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func get<X>(buffer : StableBuffer<X>, index : Nat) : X {
    switch (buffer.elems[index]) {
      case (?element) element;
      case null Prim.trap("Buffer index out of bounds in get");
    };
  };

  /// Returns the element at index `index` as an option.
  /// Returns `null` when `index >= size`. Indexing is zero-based.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// StableBuffer.add(buffer, 10);
  /// StableBuffer.add(buffer, 11);
  /// let x = StableBuffer.getOpt(buffer, 0); // => ?10
  /// let y = StableBuffer.getOpt(buffer, 2); // => null
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func getOpt<X>(buffer : StableBuffer<X>, index : Nat) : ?X {
    if (index < buffer.count) {
      buffer.elems[index];
    } else {
      null;
    };
  };

  /// Overwrites the current element at `index` with `element`. Traps if
  /// `index` >= size. Indexing is zero-based.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// StableBuffer.add(buffer, 10);
  /// StableBuffer.put(buffer, 0, 20); // overwrites 10 at index 0 with 20
  /// StableBuffer.toArray(Buffer, buffer) // => [20]
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func put<X>(buffer : StableBuffer<X>, index : Nat, element : X) {
    if (index >= buffer.count) {
      Prim.trap "Buffer index out of bounds in put";
    };
    buffer.elems[index] := ?element;
  };

  /// Returns true iff `buffer` contains `element` with respect to equality
  /// defined by `equal`.
  ///
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// StableBuffer.add(buffer, 2);
  /// StableBuffer.add(buffer, 0);
  /// StableBuffer.add(buffer, 3);
  /// StableBuffer.contains<Nat>(buffer, 2, Nat.equal); // => true
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func contains<X>(buffer : StableBuffer<X>, element : X, equal : (X, X) -> Bool) : Bool {
    for (current in vals(buffer)) {
      if (equal(current, element)) {
        return true;
      };
    };

    false;
  };

  /// Finds the first index of `element` in `buffer` using equality of elements defined
  /// by `equal`. Returns `null` if `element` is not found.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// StableBuffer.add(buffer, 1);
  /// StableBuffer.add(buffer, 2);
  /// StableBuffer.add(buffer, 3);
  /// StableBuffer.add(buffer, 4);
  ///
  /// StableBuffer.indexOf<Nat>(3, buffer, Nat.equal); // => ?2
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func indexOf<X>(element : X, buffer : StableBuffer<X>, equal : (X, X) -> Bool) : ?Nat {
    let count = size(buffer);
    var i = 0;
    while (i < count) {
      if (equal(get(buffer, i), element)) {
        return ?i;
      };
      i += 1;
    };

    null;
  };
};
