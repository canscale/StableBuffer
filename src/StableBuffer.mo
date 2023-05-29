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

  /// Removes all elements from the buffer for which the predicate returns false.
  /// The predicate is given both the index of the element and the element itself.
  /// This may cause a downsizing of the array.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// StableBuffer.add(buffer, 10);
  /// StableBuffer.add(buffer, 11);
  /// StableBuffer.add(buffer, 12);
  /// StableBuffer.filterEntries(buffer, func(_, x) = x % 2 == 0); // only keep even elements
  /// StableBuffer.toArray(buffer) // => [10, 12]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size)
  public func filterEntries<X>(buffer : StableBuffer<X>, predicate : (Nat, X) -> Bool) {
    var numRemoved = 0;
    let keep = Prim.Array_tabulate<Bool>(
      buffer.count,
      func i {
        switch (buffer.elems[i]) {
          case (?element) {
            if (predicate(i, element)) {
              true;
            } else {
              numRemoved += 1;
              false;
            };
          };
          case null {
            Prim.trap "Malformed buffer in filter()";
          };
        };
      },
    );

    let capacity = buffer.elems.size();

    if ((buffer.count - numRemoved : Nat) < capacity / DECREASE_THRESHOLD) {
      let elements2 = Prim.Array_init<?X>(capacity / DECREASE_FACTOR, null);

      var i = 0;
      var j = 0;
      while (i < buffer.count) {
        if (keep[i]) {
          elements2[j] := buffer.elems[i];
          i += 1;
          j += 1;
        } else {
          i += 1;
        };
      };

      buffer.elems := elements2;
    } else {
      var i = 0;
      var j = 0;
      while (i < buffer.count) {
        if (keep[i]) {
          buffer.elems[j] := buffer.elems[i];
          i += 1;
          j += 1;
        } else {
          i += 1;
        };
      };

      while (j < buffer.count) {
        buffer.elems[j] := null;
        j += 1;
      };
    };

    buffer.count -= numRemoved;
  };

  /// Inserts `element` at `index`, shifts all elements to the right of
  /// `index` over by one index. Traps if `index` is greater than size.
  ///
  /// ```motoko include=initialize
  /// let buffer1 = StableBuffer.initPresized<Nat>(2);
  /// let buffer2 = StableBuffer.initPresized<Nat>(2);
  /// StableBuffer.add(buffer, 10);
  /// StableBuffer.add(buffer, 11);
  /// StableBuffer.insert(buffer, 1, 9);
  /// StableBuffer.toArray(buffer) // => [10, 9, 11]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size)
  public func insert<X>(buffer : StableBuffer<X>, index : Nat, element : X) {
    if (index > buffer.count) {
      Prim.trap "Buffer index out of bounds in insert";
    };
    let capacity = buffer.elems.size();

    if (buffer.count + 1 > capacity) {
      let capacity = buffer.elems.size();
      let elements2 = Prim.Array_init<?X>(newCapacity capacity, null);
      var i = 0;
      while (i < buffer.count + 1) {
        if (i < index) {
          elements2[i] := buffer.elems[i];
        } else if (i == index) {
          elements2[i] := ?element;
        } else {
          elements2[i] := buffer.elems[i - 1];
        };

        i += 1;
      };
      buffer.elems := elements2;
    } else {
      var i : Nat = buffer.count;
      while (i > index) {
        buffer.elems[i] := buffer.elems[i - 1];
        i -= 1;
      };
      buffer.elems[index] := ?element;
    };

    buffer.count += 1;
  };

  /// Inserts `buffer2` at `index`, and shifts all elements to the right of
  /// `index` over by size2. Traps if `index` is greater than size.
  ///
  /// ```motoko include=initialize
  /// let buffer1 = StableBuffer.initPresized<Nat>(2);
  /// let buffer2 = StableBuffer.initPresized<Nat>(2);
  /// StableBuffer.add(buffer1, 10);
  /// StableBuffer.add(buffer1, 11);
  /// StableBuffer.add(buffer2, 12);
  /// StableBuffer.add(buffer2, 13);
  /// StableBuffer.insertBuffer(buffer1, 1, buffer2);
  /// StableBuffer.toArray(buffer1) // => [10, 12, 13, 11]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size1 + size2)
  public func insertBuffer<X>(buffer : StableBuffer<X>, index : Nat, buffer2 : StableBuffer<X>) {
    if (index > buffer.count) {
      Prim.trap "Buffer index out of bounds in insertBuffer";
    };

    let size2 = size(buffer2);
    let capacity = buffer.elems.size();

    // copy elements to new array and shift over in one pass
    if (buffer.count + size2 > capacity) {
      let elements2 = Prim.Array_init<?X>(newCapacity(buffer.count + size2), null);
      var i = 0;
      for (element in buffer.elems.vals()) {
        if (i == index) {
          i += size2;
        };
        elements2[i] := element;
        i += 1;
      };

      i := 0;
      while (i < size2) {
        elements2[i + index] := getOpt(buffer2, i);
        i += 1;
      };
      buffer.elems := elements2;
    } // just insert
    else {
      var i = index;
      while (i < index + size2) {
        if (i < buffer.count) {
          buffer.elems[i + size2] := buffer.elems[i];
        };
        buffer.elems[i] := getOpt(buffer2, (i - index : Nat));

        i += 1;
      };
    };

    buffer.count += size2;
  };

  /// Sorts the elements in the buffer according to `compare`.
  /// Sort is deterministic, stable, and in-place.
  ///
  /// ```motoko include=initialize
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// StableBuffer.add(buffer, 11);
  /// StableBuffer.add(buffer, 12);
  /// StableBuffer.add(buffer, 10);
  /// StableBuffer.sort(buffer, Nat.compare);
  /// StableBuffer.toArray(buffer) // => [10, 11, 12]
  /// ```
  ///
  /// Runtime: O(size * log(size))
  ///
  /// Space: O(size)
  public func sort<X>(buffer : StableBuffer<X>, compare : (X, X) -> Order.Order) {
    // Stable merge sort in a bottom-up iterative style
    if (buffer.count == 0) {
      return;
    };
    let scratchSpace = Prim.Array_init<?X>(buffer.count, null);

    let sizeDec = buffer.count - 1 : Nat;
    var currSize = 1; // current size of the subarrays being merged
    // when the current size == size, the array has been merged into a single sorted array
    while (currSize < buffer.count) {
      var leftStart = 0; // selects the current left subarray being merged
      while (leftStart < sizeDec) {
        let mid : Nat = if (leftStart + currSize - 1 : Nat < sizeDec) {
          leftStart + currSize - 1;
        } else { sizeDec };
        let rightEnd : Nat = if (leftStart + (2 * currSize) - 1 : Nat < sizeDec) {
          leftStart + (2 * currSize) - 1;
        } else { sizeDec };

        // Merge subarrays elements[leftStart...mid] and elements[mid+1...rightEnd]
        var left = leftStart;
        var right = mid + 1;
        var nextSorted = leftStart;
        while (left < mid + 1 and right < rightEnd + 1) {
          let leftOpt = buffer.elems[left];
          let rightOpt = buffer.elems[right];
          switch (leftOpt, rightOpt) {
            case (?leftElement, ?rightElement) {
              switch (compare(leftElement, rightElement)) {
                case (#less or #equal) {
                  scratchSpace[nextSorted] := leftOpt;
                  left += 1;
                };
                case (#greater) {
                  scratchSpace[nextSorted] := rightOpt;
                  right += 1;
                };
              };
            };
            case (_, _) {
              // only sorting non-null items
              Prim.trap "Malformed buffer in sort";
            };
          };
          nextSorted += 1;
        };
        while (left < mid + 1) {
          scratchSpace[nextSorted] := buffer.elems[left];
          nextSorted += 1;
          left += 1;
        };
        while (right < rightEnd + 1) {
          scratchSpace[nextSorted] := buffer.elems[right];
          nextSorted += 1;
          right += 1;
        };

        // Copy over merged elements
        var i = leftStart;
        while (i < rightEnd + 1) {
          buffer.elems[i] := scratchSpace[i];
          i += 1;
        };

        leftStart += 2 * currSize;
      };
      currSize *= 2;
    };
  };

  /// Returns true if and only if the buffer is empty.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// StableBuffer.add(buffer, 2);
  /// StableBuffer.add(buffer, 0);
  /// StableBuffer.add(buffer, 3);
  /// StableBuffer.isEmpty(buffer); // => false
  /// ```
  ///
  /// ```motoko include=initialize
  /// Buffer.isEmpty(buffer); // => true
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func isEmpty<X>(buffer : StableBuffer<X>) : Bool = size(buffer) == 0;

  /// Finds the greatest element in `buffer` defined by `compare`.
  /// Returns `null` if `buffer` is empty.
  ///
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// StableBuffer.add(buffer, 1);
  /// StableBuffer.add(buffer, 2);
  ///
  /// StableBuffer.max(buffer, Nat.compare); // => ?2
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func max<X>(buffer : StableBuffer<X>, compare : (X, X) -> Order) : ?X {
    if (size(buffer) == 0) {
      return null;
    };

    var maxSoFar = get(buffer, 0);
    for (current in vals(buffer)) {
      switch (compare(current, maxSoFar)) {
        case (#greater) {
          maxSoFar := current;
        };
        case _ {};
      };
    };

    ?maxSoFar;
  };

  /// Finds the least element in `buffer` defined by `compare`.
  /// Returns `null` if `buffer` is empty.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// StableBuffer.add(buffer, 1);
  /// StableBuffer.add(buffer, 2);
  ///
  /// StableBuffer.min(buffer, Nat.compare); // => ?1
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func min<X>(buffer : StableBuffer<X>, compare : (X, X) -> Order) : ?X {
    if (size(buffer) == 0) {
      return null;
    };

    var minSoFar = get(buffer, 0);
    for (current in vals(buffer)) {
      switch (compare(current, minSoFar)) {
        case (#less) {
          minSoFar := current;
        };
        case _ {};
      };
    };

    ?minSoFar;
  };

  /// Defines equality for two buffers, using `equal` to recursively compare elements in the
  /// buffers. Returns true iff the two buffers are of the same size, and `equal`
  /// evaluates to true for every pair of elements in the two buffers of the same
  /// index.
  ///
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// let buffer1 = StableBuffer.initPresized<Nat>(2);
  /// StableBuffer.add(buffer1, 1);
  /// StableBuffer.add(buffer1, 2);
  ///
  /// let buffer2 = StableBuffer.initPresized<Nat>(5);
  /// StableBuffer.add(buffer2, 1);
  /// StableBuffer.add(buffer2, 2);
  ///
  /// StableBuffer.equal(buffer1, buffer2, Nat.equal); // => true
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func equal<X>(buffer1 : StableBuffer<X>, buffer2 : StableBuffer<X>, equal : (X, X) -> Bool) : Bool {
    let size1 = size(buffer1);

    if (size1 != size(buffer2)) {
      return false;
    };

    var i = 0;
    while (i < size1) {
      if (not equal(get(buffer1, i), get(buffer2, i))) {
        return false;
      };
      i += 1;
    };

    true;
  };

  /// Defines comparison for two buffers, using `compare` to recursively compare elements in the
  /// buffers. Comparison is defined lexicographically.
  ///
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// let buffer1 = StableBuffer.initPresized<Nat>(2);
  /// StableBuffer.add(buffer1, 1);
  /// StableBuffer.add(buffer1, 2);
  ///
  /// let buffer2 = StableBuffer.initPresized<Nat>(3);
  /// StableBuffer.add(buffer2, 3);
  /// StableBuffer.add(buffer2, 4);
  ///
  /// StableBuffer.compare<Nat>(buffer1, buffer2, Nat.compare); // => #less
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func compare<X>(buffer1 : StableBuffer<X>, buffer2 : StableBuffer<X>, compare : (X, X) -> Order.Order) : Order.Order {
    let size1 = size(buffer1);
    let size2 = size(buffer2);
    let minSize = if (size1 < size2) { size1 } else { size2 };

    var i = 0;
    while (i < minSize) {
      switch (compare(get(buffer1, i), get(buffer2, i))) {
        case (#less) {
          return #less;
        };
        case (#greater) {
          return #greater;
        };
        case _ {};
      };
      i += 1;
    };

    if (size1 < size2) {
      #less;
    } else if (size1 == size2) {
      #equal;
    } else {
      #greater;
    };
  };

  /// Creates a textual representation of `buffer`, using `toText` to recursively
  /// convert the elements into Text.
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
  /// StableBuffer.toText(buffer, Nat.toText); // => "[1, 2, 3, 4]"
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `toText` runs in O(1) time and space.
  public func toText<X>(buffer : StableBuffer<X>, toText : X -> Text) : Text {
    let count : Int = size(buffer);
    var i = 0;
    var text = "";
    while (i < count - 1) {
      text := text # toText(get(buffer, i)) # ", "; // Text implemented as rope
      i += 1;
    };
    if (count > 0) {
      // avoid the trailing comma
      text := text # toText(get(buffer, i));
    };

    "[" # text # "]";
  };

  /// Hashes `buffer` using `hash` to hash the underlying elements.
  /// The deterministic hash function is a function of the elements in the Buffer, as well
  /// as their ordering.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Hash "mo:base/Hash";
  ///
  /// StableBuffer.add(buffer, 1);
  /// StableBuffer.add(buffer, 2);
  /// StableBuffer.add(buffer, 3);
  /// StableBuffer.add(buffer, 1000);
  ///
  /// StableBuffer.hash<Nat>(buffer, Hash.hash); // => 2_872_640_342
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `hash` runs in O(1) time and space.
  public func hash<X>(buffer : StableBuffer<X>, hash : X -> Nat32) : Nat32 {
    let count = size(buffer);
    var i = 0;
    var accHash : Nat32 = 0;

    while (i < count) {
      accHash := Prim.intToNat32Wrap(i) ^ accHash ^ hash(get(buffer, i));
      i += 1;
    };

    accHash;
  };
};
