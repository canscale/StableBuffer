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
    var _size : Nat; // avoid name clash with `size()` method
    var elements : [var ?X];
  };

  /// Initializes a buffer of given initial capacity. Note that this capacity is not realized until an element
  /// is added to the buffer.
  public func initPresized<X>(initCapacity : Nat) : StableBuffer<X> = {
    var _size = 0;
    var elements = Prim.Array_init(initCapacity, null);
  };

  /// Initializes a buffer of initial capacity 0. When the first element is added the size will grow to one
  public func init<X>() : StableBuffer<X> = {
    var _size = 0;
    var elements = [var];
  };

  /// Returns the current number of elements in the buffer.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// buffer.size() // => 0
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func size<X>(buffer : StableBuffer<X>) : Nat = buffer._size;

  /// Adds a single element to the end of the buffer, doubling
  /// the size of the array if capacity is exceeded.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// buffer.add(0); // add 0 to buffer
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3); // causes underlying array to increase in capacity
  /// Buffer.toArray(buffer) // => [0, 1, 2, 3]
  /// ```
  ///
  /// Amortized Runtime: O(1), Worst Case Runtime: O(size)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size)
  public func add<X>(buffer : StableBuffer<X>, element : X) {
    if (buffer._size == buffer.elements.size()) {
      reserve(buffer, newCapacity(buffer.elements.size()));
    };
    buffer.elements[buffer._size] := ?element;
    buffer._size += 1;
  };

  /// Returns the element at index `index`. Traps if  `index >= size`. Indexing is zero-based.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// buffer.add(10);
  /// buffer.add(11);
  /// buffer.get(0); // => 10
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func get<X>(buffer : StableBuffer<X>, index : Nat) : X {
    switch (buffer.elements[index]) {
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
  /// buffer.add(10);
  /// buffer.add(11);
  /// let x = buffer.getOpt(0); // => ?10
  /// let y = buffer.getOpt(2); // => null
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func getOpt<X>(buffer : StableBuffer<X>, index : Nat) : ?X {
    if (index < buffer._size) {
      buffer.elements[index];
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
  /// buffer.add(10);
  /// buffer.put(0, 20); // overwrites 10 at index 0 with 20
  /// Buffer.toArray(buffer) // => [20]
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func put<X>(buffer : StableBuffer<X>, index : Nat, element : X) {
    if (index >= buffer._size) {
      Prim.trap "Buffer index out of bounds in put";
    };
    buffer.elements[index] := ?element;
  };

  /// Removes and returns the last item in the buffer or `null` if
  /// the buffer is empty.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// buffer.add(10);
  /// buffer.add(11);
  /// buffer.removeLast(); // => ?11
  /// ```
  ///
  /// Amortized Runtime: O(1), Worst Case Runtime: O(size)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size)
  public func removeLast<X>(buffer : StableBuffer<X>) : ?X {
    if (buffer._size == 0) {
      return null;
    };

    buffer._size -= 1;
    let lastElement = buffer.elements[buffer._size];
    buffer.elements[buffer._size] := null;

    if (buffer._size < buffer.elements.size() / DECREASE_THRESHOLD) {
      // FIXME should this new capacity be a function of _size
      // instead of the current capacity? E.g. _size * INCREASE_FACTOR
      reserve(buffer, buffer.elements.size() / DECREASE_FACTOR);
    };

    lastElement;
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
  /// buffer.add(10);
  /// buffer.add(11);
  /// buffer.add(12);
  /// let x = buffer.remove(1); // evaluates to 11. 11 no longer in list.
  /// Buffer.toArray(buffer) // => [10, 12]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size)
  public func remove<X>(buffer : StableBuffer<X>, index : Nat) : X {
    if (index >= buffer._size) {
      Prim.trap "Buffer index out of bounds in remove";
    };

    let element = buffer.elements[index];

    // copy elements to new array and shift over in one pass
    if ((buffer._size - 1) : Nat < buffer.elements.size() / DECREASE_THRESHOLD) {
      let elements2 = Prim.Array_init<?X>(buffer.elements.size() / DECREASE_FACTOR, null);

      var i = 0;
      var j = 0;
      label l while (i < buffer._size) {
        if (i == index) {
          i += 1;
          continue l;
        };

        elements2[j] := buffer.elements[i];
        i += 1;
        j += 1;
      };
      buffer.elements := elements2;
    } else {
      // just shift over elements
      var i = index;
      while (i < (buffer._size - 1 : Nat)) {
        buffer.elements[i] := buffer.elements[i + 1];
        i += 1;
      };
      buffer.elements[buffer._size - 1] := null;
    };

    buffer._size -= 1;

    switch (element) {
      case (?element) {
        element;
      };
      case null {
        Prim.trap "Malformed buffer in remove";
      };
    };
  };

  /// Resets the buffer. Capacity is set to 8.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// buffer.add(10);
  /// buffer.add(11);
  /// buffer.add(12);
  /// buffer.clear(); // buffer is now empty
  /// Buffer.toArray(buffer) // => []
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func clear<X>(buffer : StableBuffer<X>) {
    buffer._size := 0;
    reserve(buffer, DEFAULT_CAPACITY);
  };

  /// Removes all elements from the buffer for which the predicate returns false.
  /// The predicate is given both the index of the element and the element itself.
  /// This may cause a downsizing of the array.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// buffer.add(10);
  /// buffer.add(11);
  /// buffer.add(12);
  /// buffer.filterEntries(func(_, x) = x % 2 == 0); // only keep even elements
  /// Buffer.toArray(buffer) // => [10, 12]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size)
  public func filterEntries<X>(buffer : StableBuffer<X>, predicate : (Nat, X) -> Bool) {
    var numRemoved = 0;
    let keep = Prim.Array_tabulate<Bool>(
      buffer._size,
      func i {
        switch (buffer.elements[i]) {
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

    let capacity = buffer.elements.size();

    if ((buffer._size - numRemoved : Nat) < capacity / DECREASE_THRESHOLD) {
      let elements2 = Prim.Array_init<?X>(capacity / DECREASE_FACTOR, null);

      var i = 0;
      var j = 0;
      while (i < buffer._size) {
        if (keep[i]) {
          elements2[j] := buffer.elements[i];
          i += 1;
          j += 1;
        } else {
          i += 1;
        };
      };

      buffer.elements := elements2;
    } else {
      var i = 0;
      var j = 0;
      while (i < buffer._size) {
        if (keep[i]) {
          buffer.elements[j] := buffer.elements[i];
          i += 1;
          j += 1;
        } else {
          i += 1;
        };
      };

      while (j < buffer._size) {
        buffer.elements[j] := null;
        j += 1;
      };
    };

    buffer._size -= numRemoved;
  };

  /// Returns the capacity of the buffer (the length of the underlying array).
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// let buffer = Buffer.Buffer<Nat>(2); // underlying array has capacity 2
  /// buffer.add(10);
  /// let c1 = buffer.capacity(); // => 2
  /// buffer.add(11);
  /// buffer.add(12); // causes capacity to increase by factor of 1.5
  /// let c2 = buffer.capacity(); // => 3
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func capacity<X>(buffer : StableBuffer<X>) : Nat = buffer.elements.size();

  /// Changes the capacity to `capacity`. Traps if `capacity` < `size`.
  ///
  /// ```motoko include=initialize
  ///
  /// buffer.reserve(4);
  /// buffer.add(10);
  /// buffer.add(11);
  /// buffer.capacity(); // => 4
  /// ```
  ///
  /// Runtime: O(capacity)
  ///
  /// Space: O(capacity)
  public func reserve<X>(buffer : StableBuffer<X>, capacity : Nat) {
    if (capacity < buffer._size) {
      Prim.trap "capacity must be >= size in reserve";
    };

    let elements2 = Prim.Array_init<?X>(capacity, null);

    var i = 0;
    while (i < buffer._size) {
      elements2[i] := buffer.elements[i];
      i += 1;
    };
    buffer.elements := elements2;
  };

  /// Adds all elements in buffer `b` to this buffer.
  ///
  /// ```motoko include=initialize
  /// let buffer1 = Buffer.Buffer<Nat>(2);
  /// let buffer2 = Buffer.Buffer<Nat>(2);
  /// buffer1.add(10);
  /// buffer1.add(11);
  /// buffer2.add(12);
  /// buffer2.add(13);
  /// buffer1.append(buffer2); // adds elements from buffer2 to buffer1
  /// Buffer.toArray(buffer1) // => [10, 11, 12, 13]
  /// ```
  ///
  /// Amortized Runtime: O(size2), Worst Case Runtime: O(size1 + size2)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size1 + size2)
  public func append<X>(buffer : StableBuffer<X>, buffer2 : StableBuffer<X>) {
    let size2 = size(buffer2);

    // Make sure you only allocate a new array at most once
    if (buffer._size + size2 > buffer.elements.size()) {
      // FIXME would be nice to have a tabulate for var arrays here
      reserve(buffer, newCapacity(buffer._size + size2));
    };
    var i = 0;
    while (i < size2) {
      buffer.elements[buffer._size + i] := getOpt(buffer2, i);
      i += 1;
    };

    buffer._size += size2;
  };

  /// Inserts `element` at `index`, shifts all elements to the right of
  /// `index` over by one index. Traps if `index` is greater than size.
  ///
  /// ```motoko include=initialize
  /// let buffer1 = Buffer.Buffer<Nat>(2);
  /// let buffer2 = Buffer.Buffer<Nat>(2);
  /// buffer.add(10);
  /// buffer.add(11);
  /// buffer.insert(1, 9);
  /// Buffer.toArray(buffer) // => [10, 9, 11]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size)
  public func insert<X>(buffer : StableBuffer<X>, index : Nat, element : X) {
    if (index > buffer._size) {
      Prim.trap "Buffer index out of bounds in insert";
    };
    let capacity = buffer.elements.size();

    if (buffer._size + 1 > capacity) {
      let capacity = buffer.elements.size();
      let elements2 = Prim.Array_init<?X>(newCapacity capacity, null);
      var i = 0;
      while (i < buffer._size + 1) {
        if (i < index) {
          elements2[i] := buffer.elements[i];
        } else if (i == index) {
          elements2[i] := ?element;
        } else {
          elements2[i] := buffer.elements[i - 1];
        };

        i += 1;
      };
      buffer.elements := elements2;
    } else {
      var i : Nat = buffer._size;
      while (i > index) {
        buffer.elements[i] := buffer.elements[i - 1];
        i -= 1;
      };
      buffer.elements[index] := ?element;
    };

    buffer._size += 1;
  };

  /// Inserts `buffer2` at `index`, and shifts all elements to the right of
  /// `index` over by size2. Traps if `index` is greater than size.
  ///
  /// ```motoko include=initialize
  /// let buffer1 = Buffer.Buffer<Nat>(2);
  /// let buffer2 = Buffer.Buffer<Nat>(2);
  /// buffer1.add(10);
  /// buffer1.add(11);
  /// buffer2.add(12);
  /// buffer2.add(13);
  /// buffer1.insertBuffer(1, buffer2);
  /// Buffer.toArray(buffer1) // => [10, 12, 13, 11]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size1 + size2)
  public func insertBuffer<X>(buffer : StableBuffer<X>, index : Nat, buffer2 : StableBuffer<X>) {
    if (index > buffer._size) {
      Prim.trap "Buffer index out of bounds in insertBuffer";
    };

    let size2 = size(buffer2);
    let capacity = buffer.elements.size();

    // copy elements to new array and shift over in one pass
    if (buffer._size + size2 > capacity) {
      let elements2 = Prim.Array_init<?X>(newCapacity(buffer._size + size2), null);
      var i = 0;
      for (element in buffer.elements.vals()) {
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
      buffer.elements := elements2;
    } // just insert
    else {
      var i = index;
      while (i < index + size2) {
        if (i < buffer._size) {
          buffer.elements[i + size2] := buffer.elements[i];
        };
        buffer.elements[i] := getOpt(buffer2, (i - index));

        i += 1;
      };
    };

    buffer._size += size2;
  };

  /// Sorts the elements in the buffer according to `compare`.
  /// Sort is deterministic, stable, and in-place.
  ///
  /// ```motoko include=initialize
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(11);
  /// buffer.add(12);
  /// buffer.add(10);
  /// buffer.sort(Nat.compare);
  /// Buffer.toArray(buffer) // => [10, 11, 12]
  /// ```
  ///
  /// Runtime: O(size * log(size))
  ///
  /// Space: O(size)
  public func sort<X>(buffer : StableBuffer<X>, compare : (X, X) -> Order.Order) {
    // Stable merge sort in a bottom-up iterative style
    if (buffer._size == 0) {
      return;
    };
    let scratchSpace = Prim.Array_init<?X>(buffer._size, null);

    let sizeDec = buffer._size - 1 : Nat;
    var currSize = 1; // current size of the subarrays being merged
    // when the current size == size, the array has been merged into a single sorted array
    while (currSize < buffer._size) {
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
          let leftOpt = buffer.elements[left];
          let rightOpt = buffer.elements[right];
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
          scratchSpace[nextSorted] := buffer.elements[left];
          nextSorted += 1;
          left += 1;
        };
        while (right < rightEnd + 1) {
          scratchSpace[nextSorted] := buffer.elements[right];
          nextSorted += 1;
          right += 1;
        };

        // Copy over merged elements
        var i = leftStart;
        while (i < rightEnd + 1) {
          buffer.elements[i] := scratchSpace[i];
          i += 1;
        };

        leftStart += 2 * currSize;
      };
      currSize *= 2;
    };
  };

  /// Returns an Iterator (`Iter`) over the elements of this buffer.
  /// Iterator provides a single method `next()`, which returns
  /// elements in order, or `null` when out of elements to iterate over.
  ///
  /// ```motoko include=initialize
  ///
  /// buffer.add(10);
  /// buffer.add(11);
  /// buffer.add(12);
  ///
  /// var sum = 0;
  /// for (element in buffer.vals()) {
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
      if (nextIndex >= buffer._size) {
        return null;
      };
      let nextElement = buffer.elements[nextIndex];
      nextIndex += 1;
      nextElement;
    };
  };

  /// Returns true if and only if the buffer is empty.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// buffer.add(2);
  /// buffer.add(0);
  /// buffer.add(3);
  /// Buffer.isEmpty(buffer); // => false
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

  /// Returns true iff `buffer` contains `element` with respect to equality
  /// defined by `equal`.
  ///
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(2);
  /// buffer.add(0);
  /// buffer.add(3);
  /// Buffer.contains<Nat>(buffer, 2, Nat.equal); // => true
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

  /// Returns a copy of `buffer`, with the same capacity.
  ///
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// buffer.add(1);
  ///
  /// let clone = Buffer.clone(buffer);
  /// Buffer.toArray(clone); // => [1]
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

  /// Finds the greatest element in `buffer` defined by `compare`.
  /// Returns `null` if `buffer` is empty.
  ///
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  ///
  /// Buffer.max(buffer, Nat.compare); // => ?2
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
  /// buffer.add(1);
  /// buffer.add(2);
  ///
  /// Buffer.min(buffer, Nat.compare); // => ?1
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
  /// let buffer1 = Buffer.Buffer<Nat>(2);
  /// buffer1.add(1);
  /// buffer1.add(2);
  ///
  /// let buffer2 = Buffer.Buffer<Nat>(5);
  /// buffer2.add(1);
  /// buffer2.add(2);
  ///
  /// Buffer.equal(buffer1, buffer2, Nat.equal); // => true
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
  /// let buffer1 = Buffer.Buffer<Nat>(2);
  /// buffer1.add(1);
  /// buffer1.add(2);
  ///
  /// let buffer2 = Buffer.Buffer<Nat>(3);
  /// buffer2.add(3);
  /// buffer2.add(4);
  ///
  /// Buffer.compare<Nat>(buffer1, buffer2, Nat.compare); // => #less
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
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  ///
  /// Buffer.toText(buffer, Nat.toText); // => "[1, 2, 3, 4]"
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `toText` runs in O(1) time and space.
  public func toText<X>(buffer : StableBuffer<X>, toText : X -> Text) : Text {
    let _size : Int = size(buffer);
    var i = 0;
    var text = "";
    while (i < _size - 1) {
      text := text # toText(get(buffer, i)) # ", "; // Text implemented as rope
      i += 1;
    };
    if (_size > 0) {
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
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(1000);
  ///
  /// Buffer.hash<Nat>(buffer, Hash.hash); // => 2_872_640_342
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `hash` runs in O(1) time and space.
  public func hash<X>(buffer : StableBuffer<X>, hash : X -> Nat32) : Nat32 {
    let _size = size(buffer);
    var i = 0;
    var accHash : Nat32 = 0;

    while (i < _size) {
      accHash := Prim.intToNat32Wrap(i) ^ accHash ^ hash(get(buffer, i));
      i += 1;
    };

    accHash;
  };

  /// Finds the first index of `element` in `buffer` using equality of elements defined
  /// by `equal`. Returns `null` if `element` is not found.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  ///
  /// Buffer.indexOf<Nat>(3, buffer, Nat.equal); // => ?2
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func indexOf<X>(element : X, buffer : StableBuffer<X>, equal : (X, X) -> Bool) : ?Nat {
    let _size = size(buffer);
    var i = 0;
    while (i < _size) {
      if (equal(get(buffer, i), element)) {
        return ?i;
      };
      i += 1;
    };

    null;
  };

  /// Finds the last index of `element` in `buffer` using equality of elements defined
  /// by `equal`. Returns `null` if `element` is not found.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  /// buffer.add(2);
  /// buffer.add(2);
  ///
  /// Buffer.lastIndexOf<Nat>(2, buffer, Nat.equal); // => ?5
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func lastIndexOf<X>(element : X, buffer : StableBuffer<X>, equal : (X, X) -> Bool) : ?Nat {
    let _size = size(buffer);
    if (_size == 0) {
      return null;
    };
    var i = _size;
    while (i >= 1) {
      i -= 1;
      if (equal(get(buffer, i), element)) {
        return ?i;
      };
    };

    null;
  };

  /// Searches for `subBuffer` in `buffer`, and returns the starting index if it is found.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  /// buffer.add(5);
  /// buffer.add(6);
  ///
  /// let sub = Buffer.Buffer<Nat>(2);
  /// sub.add(4);
  /// sub.add(5);
  /// sub.add(6);
  ///
  /// Buffer.indexOfBuffer<Nat>(sub, buffer, Nat.equal); // => ?3
  /// ```
  ///
  /// Runtime: O(size of buffer + size of subBuffer)
  ///
  /// Space: O(size of subBuffer)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func indexOfBuffer<X>(subBuffer : StableBuffer<X>, buffer : StableBuffer<X>, equal : (X, X) -> Bool) : ?Nat {
    // Uses the KMP substring search algorithm
    // Implementation from: https://www.educative.io/answers/what-is-the-knuth-morris-pratt-algorithm
    let _size = size(buffer);
    let subSize = size(subBuffer);
    if (subSize > _size or subSize == 0) {
      return null;
    };

    // precompute lps
    let lps = Prim.Array_init<Nat>(subSize, 0);
    var i = 0;
    var j = 1;

    while (j < subSize) {
      if (equal(get(subBuffer, i), get(subBuffer, j))) {
        i += 1;
        lps[j] := i;
        j += 1;
      } else if (i == 0) {
        lps[j] := 0;
        j += 1;
      } else {
        i := lps[i - 1];
      };
    };

    // start search
    i := 0;
    j := 0;
    let subSizeDec = subSize - 1 : Nat; // hoisting loop invariant
    while (i < subSize and j < _size) {
      if (equal(get(subBuffer, i), get(buffer, j)) and i == subSizeDec) {
        return ?(j - i);
      } else if (equal(get(subBuffer, i), get(buffer, j))) {
        i += 1;
        j += 1;
      } else {
        if (i != 0) {
          i := lps[i - 1];
        } else {
          j += 1;
        };
      };
    };

    null;
  };

  /// Similar to indexOf, but runs in logarithmic time. Assumes that `buffer` is sorted.
  /// Behavior is undefined if `buffer` is not sorted. Uses `compare` to
  /// perform the search. Returns an index of `element` if it is found.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(4);
  /// buffer.add(5);
  /// buffer.add(6);
  ///
  /// Buffer.binarySearch<Nat>(5, buffer, Nat.compare); // => ?2
  /// ```
  ///
  /// Runtime: O(log(size))
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func binarySearch<X>(element : X, buffer : StableBuffer<X>, compare : (X, X) -> Order.Order) : ?Nat {
    var low = 0;
    var high = size(buffer);

    while (low < high) {
      let mid = (low + high) / 2;
      let current = get(buffer, mid);
      switch (compare(element, current)) {
        case (#equal) {
          return ?mid;
        };
        case (#less) {
          high := mid;
        };
        case (#greater) {
          low := mid + 1;
        };
      };
    };

    null;
  };

  /// Returns the sub-buffer of `buffer` starting at index `start`
  /// of length `length`. Traps if `start` is out of bounds, or `start + length`
  /// is greater than the size of `buffer`.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  /// buffer.add(5);
  /// buffer.add(6);
  ///
  /// let sub = Buffer.subBuffer(buffer, 3, 2);
  /// Buffer.toText(sub, Nat.toText); // => [4, 5]
  /// ```
  ///
  /// Runtime: O(length)
  ///
  /// Space: O(length)
  public func subBuffer<X>(buffer : StableBuffer<X>, start : Nat, length : Nat) : StableBuffer<X> {
    let _size = size(buffer);
    let end = start + length; // exclusive
    if (start >= _size or end > _size) {
      Prim.trap "Buffer index out of bounds in subBuffer";
    };

    let newBuffer = initPresized<X>(newCapacity length);

    var i = start;
    while (i < end) {
      add(newBuffer, get(buffer, i));

      i += 1;
    };

    newBuffer;
  };

  /// Checks if `subBuffer` is a sub-Buffer of `buffer`. Uses `equal` to
  /// compare elements.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  /// buffer.add(5);
  /// buffer.add(6);
  ///
  /// let sub = Buffer.Buffer<Nat>(2);
  /// sub.add(2);
  /// sub.add(3);
  /// Buffer.isSubBufferOf(sub, buffer, Nat.equal); // => true
  /// ```
  ///
  /// Runtime: O(size of subBuffer + size of buffer)
  ///
  /// Space: O(size of subBuffer)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func isSubBufferOf<X>(subBuffer : StableBuffer<X>, buffer : StableBuffer<X>, equal : (X, X) -> Bool) : Bool {
    switch (indexOfBuffer(subBuffer, buffer, equal)) {
      case null size(subBuffer) == 0;
      case _ true;
    };
  };

  /// Checks if `subBuffer` is a strict subBuffer of `buffer`, i.e. `subBuffer` must be
  /// strictly contained inside both the first and last indices of `buffer`.
  /// Uses `equal` to compare elements.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  ///
  /// let sub = Buffer.Buffer<Nat>(2);
  /// sub.add(2);
  /// sub.add(3);
  /// Buffer.isStrictSubBufferOf(sub, buffer, Nat.equal); // => true
  /// ```
  ///
  /// Runtime: O(size of subBuffer + size of buffer)
  ///
  /// Space: O(size of subBuffer)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func isStrictSubBufferOf<X>(subBuffer : StableBuffer<X>, buffer : StableBuffer<X>, equal : (X, X) -> Bool) : Bool {
    let subBufferSize = size(subBuffer);

    switch (indexOfBuffer(subBuffer, buffer, equal)) {
      case (?index) {
        index != 0 and index != (size(buffer) - subBufferSize : Nat) // enforce strictness
      };
      case null {
        subBufferSize == 0 and subBufferSize != size(buffer)
      };
    };
  };

  /// Returns the prefix of `buffer` of length `length`. Traps if `length`
  /// is greater than the size of `buffer`.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  ///
  /// let pre = Buffer.prefix(buffer, 3); // => [1, 2, 3]
  /// Buffer.toText(pre, Nat.toText);
  /// ```
  ///
  /// Runtime: O(length)
  ///
  /// Space: O(length)
  public func prefix<X>(buffer : StableBuffer<X>, length : Nat) : StableBuffer<X> {
    let _size = size(buffer);
    if (length > _size) {
      Prim.trap "Buffer index out of bounds in prefix";
    };

    let newBuffer = initPresized<X>(newCapacity length);

    var i = 0;
    while (i < length) {
      add(newBuffer, get(buffer, i));
      i += 1;
    };

    newBuffer;
  };

  /// Checks if `prefix` is a prefix of `buffer`. Uses `equal` to
  /// compare elements.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  ///
  /// let pre = Buffer.Buffer<Nat>(2);
  /// pre.add(1);
  /// pre.add(2);
  /// Buffer.isPrefixOf(pre, buffer, Nat.equal); // => true
  /// ```
  ///
  /// Runtime: O(size of prefix)
  ///
  /// Space: O(size of prefix)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func isPrefixOf<X>(prefix : StableBuffer<X>, buffer : StableBuffer<X>, equal : (X, X) -> Bool) : Bool {
    let sizePrefix = size(prefix);
    if (size(buffer) < sizePrefix) {
      return false;
    };

    var i = 0;
    while (i < sizePrefix) {
      if (not equal(get(buffer, i), get(prefix, i))) {
        return false;
      };

      i += 1;
    };

    return true;
  };

  /// Checks if `prefix` is a strict prefix of `buffer`. Uses `equal` to
  /// compare elements.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  ///
  /// let pre = Buffer.Buffer<Nat>(3);
  /// pre.add(1);
  /// pre.add(2);
  /// pre.add(3);
  /// Buffer.isStrictPrefixOf(pre, buffer, Nat.equal); // => true
  /// ```
  ///
  /// Runtime: O(size of prefix)
  ///
  /// Space: O(size of prefix)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func isStrictPrefixOf<X>(prefix : StableBuffer<X>, buffer : StableBuffer<X>, equal : (X, X) -> Bool) : Bool {
    if (size(buffer) <= size(prefix)) {
      return false;
    };
    isPrefixOf(prefix, buffer, equal);
  };

  /// Returns the suffix of `buffer` of length `length`.
  /// Traps if `length`is greater than the size of `buffer`.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  ///
  /// let suf = Buffer.suffix(buffer, 3); // => [2, 3, 4]
  /// Buffer.toText(suf, Nat.toText);
  /// ```
  ///
  /// Runtime: O(length)
  ///
  /// Space: O(length)
  public func suffix<X>(buffer : StableBuffer<X>, length : Nat) : StableBuffer<X> {
    let _size = size(buffer);

    if (length > _size) {
      Prim.trap "Buffer index out of bounds in suffix";
    };

    let newBuffer = initPresized<X>(newCapacity length);

    var i = _size - length : Nat;
    while (i < _size) {
      add(newBuffer, get(buffer, i));

      i += 1;
    };

    newBuffer;
  };

  /// Checks if `suffix` is a suffix of `buffer`. Uses `equal` to compare
  /// elements.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  ///
  /// let suf = Buffer.Buffer<Nat>(3);
  /// suf.add(2);
  /// suf.add(3);
  /// suf.add(4);
  /// Buffer.isSuffixOf(suf, buffer, Nat.equal); // => true
  /// ```
  ///
  /// Runtime: O(length of suffix)
  ///
  /// Space: O(length of suffix)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func isSuffixOf<X>(suffix : StableBuffer<X>, buffer : StableBuffer<X>, equal : (X, X) -> Bool) : Bool {
    let suffixSize = size(suffix);
    let bufferSize = size(buffer);
    if (bufferSize < suffixSize) {
      return false;
    };

    var i = bufferSize;
    var j = suffixSize;
    while (i >= 1 and j >= 1) {
      i -= 1;
      j -= 1;
      if (not equal(get(buffer, i), get(suffix, j))) {
        return false;
      };
    };

    return true;
  };

  /// Checks if `suffix` is a strict suffix of `buffer`. Uses `equal` to compare
  /// elements.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  ///
  /// let suf = Buffer.Buffer<Nat>(3);
  /// suf.add(2);
  /// suf.add(3);
  /// suf.add(4);
  /// Buffer.isStrictSuffixOf(suf, buffer, Nat.equal); // => true
  /// ```
  ///
  /// Runtime: O(length of suffix)
  ///
  /// Space: O(length of suffix)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func isStrictSuffixOf<X>(suffix : StableBuffer<X>, buffer : StableBuffer<X>, equal : (X, X) -> Bool) : Bool {
    if (size(buffer) <= size(suffix)) {
      return false;
    };
    isSuffixOf(suffix, buffer, equal);
  };

  /// Returns true iff every element in `buffer` satisfies `predicate`.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  ///
  /// Buffer.forAll<Nat>(buffer, func x { x > 1 }); // => true
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func forAll<X>(buffer : StableBuffer<X>, predicate : X -> Bool) : Bool {
    for (element in vals(buffer)) {
      if (not predicate element) {
        return false;
      };
    };

    true;
  };

  /// Returns true iff some element in `buffer` satisfies `predicate`.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  ///
  /// Buffer.forSome<Nat>(buffer, func x { x > 3 }); // => true
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func forSome<X>(buffer : StableBuffer<X>, predicate : X -> Bool) : Bool {
    for (element in vals(buffer)) {
      if (predicate element) {
        return true;
      };
    };

    false;
  };

  /// Returns true iff no element in `buffer` satisfies `predicate`.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  ///
  /// Buffer.forNone<Nat>(buffer, func x { x == 0 }); // => true
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func forNone<X>(buffer : StableBuffer<X>, predicate : X -> Bool) : Bool {
    for (element in vals(buffer)) {
      if (predicate element) {
        return false;
      };
    };

    true;
  };

  /// Creates an array containing elements from `buffer`.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// Buffer.toArray<Nat>(buffer); // => [1, 2, 3]
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
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// Buffer.toVarArray<Nat>(buffer); // => [1, 2, 3]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func toVarArray<X>(buffer : StableBuffer<X>) : [var X] {
    let _size = size(buffer);
    if (_size == 0) { [var] } else {
      let newArray = Prim.Array_init<X>(_size, get(buffer, 0));
      var i = 1;
      while (i < _size) {
        newArray[i] := get(buffer, i);
        i += 1;
      };
      newArray;
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
  /// let buf = Buffer.fromArray<Nat>(array); // => [2, 3]
  /// Buffer.toText(buf, Nat.toText);
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

  /// Creates a buffer containing elements from `array`.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// let array = [var 1, 2, 3];
  ///
  /// let buf = Buffer.fromVarArray<Nat>(array); // => [1, 2, 3]
  /// Buffer.toText(buf, Nat.toText);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func fromVarArray<X>(array : [var X]) : StableBuffer<X> {
    let newBuffer = initPresized<X>(newCapacity(array.size()));

    for (element in array.vals()) {
      add(newBuffer, element);
    };

    newBuffer;
  };

  /// Creates a buffer containing elements from `iter`.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// let array = [1, 1, 1];
  /// let iter = array.vals();
  ///
  /// let buf = Buffer.fromIter<Nat>(iter); // => [1, 1, 1]
  /// Buffer.toText(buf, Nat.toText);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func fromIter<X>(iter : { next : () -> ?X }) : StableBuffer<X> {
    let newBuffer = initPresized<X>(DEFAULT_CAPACITY); // can't get size from `iter`

    for (element in iter) {
      add(newBuffer, element);
    };

    newBuffer;
  };

  /// Reallocates the array underlying `buffer` such that capacity == size.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// let buffer = Buffer.Buffer<Nat>(10);
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// Buffer.trimToSize<Nat>(buffer);
  /// buffer.capacity(); // => 3
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func trimToSize<X>(buffer : StableBuffer<X>) {
    let _size = size<X>(buffer);
    if (_size < capacity(buffer)) {
      reserve(buffer, _size);
    };
  };

  /// Creates a new buffer by applying `f` to each element in `buffer`.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// let newBuf = Buffer.map<Nat, Nat>(buffer, func (x) { x + 1 });
  /// Buffer.toText(newBuf, Nat.toText); // => [2, 3, 4]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func map<X, Y>(buffer : StableBuffer<X>, f : X -> Y) : StableBuffer<Y> {
    let newBuffer = initPresized<Y>(capacity(buffer));

    for (element in vals(buffer)) {
      add(newBuffer, f element);
    };

    newBuffer;
  };

  /// Applies `f` to each element in `buffer`.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// Buffer.iterate<Nat>(buffer, func (x) {
  ///   Debug.print(Nat.toText(x)); // prints each element in buffer
  /// });
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func iterate<X>(buffer : StableBuffer<X>, f : X -> ()) {
    for (element in vals(buffer)) {
      f element;
    };
  };

  /// Applies `f` to each element in `buffer` and its index.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// let newBuf = Buffer.mapEntries<Nat, Nat>(buffer, func (x, i) { x + i + 1 });
  /// Buffer.toText(newBuf, Nat.toText); // => [2, 4, 6]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func mapEntries<X, Y>(buffer : StableBuffer<X>, f : (Nat, X) -> Y) : StableBuffer<Y> {
    let newBuffer = initPresized<Y>(capacity(buffer));

    var i = 0;
    let _size = size(buffer);
    while (i < _size) {
      add(newBuffer, f(i, get(buffer, i)));
      i += 1;
    };

    newBuffer;
  };

  /// Creates a new buffer by applying `f` to each element in `buffer`,
  /// and keeping all non-null elements.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// let newBuf = Buffer.mapFilter<Nat, Nat>(buffer, func (x) {
  ///   if (x > 1) {
  ///     ?(x * 2);
  ///   } else {
  ///     null;
  ///   }
  /// });
  /// Buffer.toText(newBuf, Nat.toText); // => [4, 6]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func mapFilter<X, Y>(buffer : StableBuffer<X>, f : X -> ?Y) : StableBuffer<Y> {
    let newBuffer = initPresized<Y>(capacity(buffer));

    for (element in vals(buffer)) {
      switch (f element) {
        case (?element) {
          add(newBuffer, element);
        };
        case _ {};
      };
    };

    newBuffer;
  };

  /// Creates a new buffer by applying `f` to each element in `buffer`.
  /// If any invocation of `f` produces an `#err`, returns an `#err`. Otherwise
  /// Returns an `#ok` containing the new buffer.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Result "mo:base/Result";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// let result = Buffer.mapResult<Nat, Nat, Text>(buffer, func (k) {
  ///   if (k > 0) {
  ///     #ok(k);
  ///   } else {
  ///     #err("One or more elements are zero.");
  ///   }
  /// });
  ///
  /// Result.mapOk<Buffer.Buffer<Nat>, [Nat], Text>(result, func buffer = Buffer.toArray(buffer)) // => #ok([1, 2, 3])
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func mapResult<X, Y, E>(buffer : StableBuffer<X>, f : X -> Result.Result<Y, E>) : Result.Result<StableBuffer<Y>, E> {
    let newBuffer = initPresized<Y>(capacity(buffer));

    for (element in vals(buffer)) {
      switch (f element) {
        case (#ok result) {
          add(newBuffer, result);
        };
        case (#err e) {
          return #err e;
        };
      };
    };

    #ok newBuffer;
  };

  /// Creates a new buffer by applying `k` to each element in `buffer`,
  /// and concatenating the resulting buffers in order. This operation
  /// is similar to what in other functional languages is known as monadic bind.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// let chain = Buffer.chain<Nat, Nat>(buffer, func (x) {
  ///   let b = Buffer.Buffer<Nat>(2);
  ///   b.add(x);
  ///   b.add(x * 2);
  ///   return b;
  /// });
  /// Buffer.toText(chain, Nat.toText); // => [1, 2, 2, 4, 3, 6]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `k` runs in O(1) time and space.
  public func chain<X, Y>(buffer : StableBuffer<X>, k : X -> StableBuffer<Y>) : StableBuffer<Y> {
    let newBuffer = initPresized<Y>(size(buffer) * 4);

    for (element in vals(buffer)) {
      append(newBuffer, k element);
    };

    newBuffer;
  };

  /// Collapses the elements in `buffer` into a single value by starting with `base`
  /// and progessively combining elements into `base` with `combine`. Iteration runs
  /// left to right.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// Buffer.foldLeft<Text, Nat>(buffer, "", func (acc, x) { acc # Nat.toText(x)}); // => "123"
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `combine` runs in O(1) time and space.
  public func foldLeft<A, X>(buffer : StableBuffer<X>, base : A, combine : (A, X) -> A) : A {
    var accumulation = base;

    for (element in vals(buffer)) {
      accumulation := combine(accumulation, element);
    };

    accumulation;
  };

  /// Collapses the elements in `buffer` into a single value by starting with `base`
  /// and progessively combining elements into `base` with `combine`. Iteration runs
  /// right to left.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// Buffer.foldRight<Nat, Text>(buffer, "", func (x, acc) { Nat.toText(x) # acc }); // => "123"
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `combine` runs in O(1) time and space.
  public func foldRight<X, A>(buffer : StableBuffer<X>, base : A, combine : (X, A) -> A) : A {
    let _size = size(buffer);
    if (_size == 0) {
      return base;
    };
    var accumulation = base;

    var i = _size;
    while (i >= 1) {
      i -= 1; // to avoid Nat underflow, subtract first and stop iteration at 1
      accumulation := combine(get(buffer, i), accumulation);
    };

    accumulation;
  };

  /// Returns the first element of `buffer`. Traps if `buffer` is empty.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// Buffer.first(buffer); // => 1
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func first<X>(buffer : StableBuffer<X>) : X = get(buffer, 0);

  /// Returns the last element of `buffer`. Traps if `buffer` is empty.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// Buffer.last(buffer); // => 3
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func last<X>(buffer : StableBuffer<X>) : X = get(buffer, size(buffer) - 1);

  /// Returns a new buffer with capacity and size 1, containing `element`.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// let buffer = Buffer.make<Nat>(1);
  /// Buffer.toText(buffer, Nat.toText); // => [1]
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func make<X>(element : X) : StableBuffer<X> {
    let newBuffer = initPresized<X>(1);
    add(newBuffer, element);
    newBuffer;
  };

  /// Reverses the order of elements in `buffer`.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// Buffer.reverse(buffer);
  /// Buffer.toText(buffer, Nat.toText); // => [3, 2, 1]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  public func reverse<X>(buffer : StableBuffer<X>) {
    let _size = size(buffer);
    if (_size == 0) {
      return;
    };

    var i = 0;
    var j = _size - 1 : Nat;
    var temp = get(buffer, 0);
    while (i < _size / 2) {
      temp := get(buffer, j);
      put(buffer, j, get(buffer, i));
      put(buffer, i, temp);
      i += 1;
      j -= 1;
    };
  };

  /// Merges two sorted buffers into a single sorted buffer, using `compare` to define
  /// the ordering. The final ordering is stable. Behavior is undefined if either
  /// `buffer1` or `buffer2` is not sorted.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// let buffer1 = Buffer.Buffer<Nat>(2);
  /// buffer1.add(1);
  /// buffer1.add(2);
  /// buffer1.add(4);
  ///
  /// let buffer2 = Buffer.Buffer<Nat>(2);
  /// buffer2.add(2);
  /// buffer2.add(4);
  /// buffer2.add(6);
  ///
  /// let merged = Buffer.merge<Nat>(buffer1, buffer2, Nat.compare);
  /// Buffer.toText(merged, Nat.toText); // => [1, 2, 2, 4, 4, 6]
  /// ```
  ///
  /// Runtime: O(size1 + size2)
  ///
  /// Space: O(size1 + size2)
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func merge<X>(buffer1 : StableBuffer<X>, buffer2 : StableBuffer<X>, compare : (X, X) -> Order) : StableBuffer<X> {
    let size1 = size(buffer1);
    let size2 = size(buffer2);

    let newBuffer = initPresized<X>(newCapacity(size1 + size2));

    var pointer1 = 0;
    var pointer2 = 0;

    while (pointer1 < size1 and pointer2 < size2) {
      let current1 = get(buffer1, pointer1);
      let current2 = get(buffer2, pointer2);

      switch (compare(current1, current2)) {
        case (#less) {
          add(newBuffer, current1);
          pointer1 += 1;
        };
        case _ {
          add(newBuffer, current2);
          pointer2 += 1;
        };
      };
    };

    while (pointer1 < size1) {
      add(newBuffer, get(buffer1, pointer1));
      pointer1 += 1;
    };

    while (pointer2 < size2) {
      add(newBuffer, get(buffer2, pointer2));
      pointer2 += 1;
    };

    newBuffer;
  };

  /// Eliminates all duplicate elements in `buffer` as defined by `compare`.
  /// Elimination is stable with respect to the original ordering of the elements.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// Buffer.removeDuplicates<Nat>(buffer, Nat.compare);
  /// Buffer.toText(buffer, Nat.toText); // => [1, 2, 3]
  /// ```
  ///
  /// Runtime: O(size * log(size))
  ///
  /// Space: O(size)
  public func removeDuplicates<X>(buffer : StableBuffer<X>, compare : (X, X) -> Order) {
    let _size = size(buffer);
    let indices = Prim.Array_tabulate<(Nat, X)>(_size, func i = (i, get(buffer, i)));
    // Sort based on element, while carrying original index information
    // This groups together the duplicate elements
    let sorted = Array.sort<(Nat, X)>(indices, func(pair1, pair2) = compare(pair1.1, pair2.1));
    let uniques = initPresized<(Nat, X)>(_size);

    // Iterate over elements
    var i = 0;
    while (i < _size) {
      var j = i;
      // Iterate over duplicate elements, and find the smallest index among them (for stability)
      var minIndex = sorted[j];
      label duplicates while (j < (_size - 1 : Nat)) {
        let pair1 = sorted[j];
        let pair2 = sorted[j + 1];
        switch (compare(pair1.1, pair2.1)) {
          case (#equal) {
            if (pair2.0 < pair1.0) {
              minIndex := pair2;
            };
            j += 1;
          };
          case _ {
            break duplicates;
          };
        };
      };

      add(uniques, minIndex);
      i := j + 1;
    };

    // resort based on original ordering and place back in buffer
    sort<(Nat, X)>(
      uniques,
      func(pair1, pair2) {
        if (pair1.0 < pair2.0) {
          #less;
        } else if (pair1.0 == pair2.0) {
          #equal;
        } else {
          #greater;
        };
      },
    );

    clear(buffer);
    reserve(buffer, size(uniques));
    for (element in vals(uniques)) {
      add(buffer, element.1);
    };
  };

  /// Splits `buffer` into a pair of buffers where all elements in the left
  /// buffer satisfy `predicate` and all elements in the right buffer do not.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  /// buffer.add(5);
  /// buffer.add(6);
  ///
  /// let partitions = Buffer.partition<Nat>(buffer, func (x) { x % 2 == 0 });
  /// (Buffer.toArray(partitions.0), Buffer.toArray(partitions.1)) // => ([2, 4, 6], [1, 3, 5])
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func partition<X>(buffer : StableBuffer<X>, predicate : X -> Bool) : (StableBuffer<X>, StableBuffer<X>) {
    let _size = size(buffer);
    let trueBuffer = initPresized<X>(_size);
    let falseBuffer = initPresized<X>(_size);

    for (element in vals(buffer)) {
      if (predicate element) {
        add(trueBuffer, element);
      } else {
        add(falseBuffer, element);
      };
    };

    (trueBuffer, falseBuffer);
  };

  /// Splits the buffer into two buffers at `index`, where the left buffer contains
  /// all elements with indices less than `index`, and the right buffer contains all
  /// elements with indices greater than or equal to `index`. Traps if `index` is out
  /// of bounds.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  /// buffer.add(5);
  /// buffer.add(6);
  ///
  /// let split = Buffer.split<Nat>(buffer, 3);
  /// (Buffer.toArray(split.0), Buffer.toArray(split.1)) // => ([1, 2, 3], [4, 5, 6])
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func split<X>(buffer : StableBuffer<X>, index : Nat) : (StableBuffer<X>, StableBuffer<X>) {
    let _size = size(buffer);

    if (index < 0 or index > _size) {
      Prim.trap "Index out of bounds in split";
    };

    let buffer1 = initPresized<X>(newCapacity index);
    let buffer2 = initPresized<X>(newCapacity(_size - index));

    var i = 0;
    while (i < index) {
      add(buffer1, get(buffer, i));
      i += 1;
    };
    while (i < _size) {
      add(buffer2, get(buffer, i));
      i += 1;
    };

    (buffer1, buffer2);
  };

  /// Breaks up `buffer` into buffers of size `size`. The last chunk may
  /// have less than `size` elements if the number of elements is not divisible
  /// by the chunk size.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  /// buffer.add(4);
  /// buffer.add(5);
  /// buffer.add(6);
  ///
  /// let chunks = Buffer.chunk<Nat>(buffer, 3);
  /// Buffer.toText<Buffer.Buffer<Nat>>(chunks, func buf = Buffer.toText(buf, Nat.toText)); // => [[1, 2, 3], [4, 5, 6]]
  /// ```
  ///
  /// Runtime: O(number of elements in buffer)
  ///
  /// Space: O(number of elements in buffer)
  public func chunk<X>(buffer : StableBuffer<X>, _size : Nat) : StableBuffer<StableBuffer<X>> {
    if (_size == 0) {
      Prim.trap "Chunk size must be non-zero in chunk";
    };

    // ceil(buffer.size() / size)
    let newBuffer = initPresized<StableBuffer<X>>((size(buffer) + _size - 1) / _size);

    var newInnerBuffer = initPresized<X>(newCapacity _size);
    var innerSize = 0;
    for (element in vals(buffer)) {
      if (innerSize == _size) {
        add(newBuffer, newInnerBuffer);
        newInnerBuffer := initPresized<X>(newCapacity _size);
        innerSize := 0;
      };
      add(newInnerBuffer, element);
      innerSize += 1;
    };
    if (innerSize > 0) {
      add(newBuffer, newInnerBuffer);
    };

    newBuffer;
  };

  /// Groups equal and adjacent elements in the list into sub lists.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(2);
  /// buffer.add(4);
  /// buffer.add(5);
  /// buffer.add(5);
  ///
  /// let grouped = Buffer.groupBy<Nat>(buffer, func (x, y) { x == y });
  /// Buffer.toText<Buffer.Buffer<Nat>>(grouped, func buf = Buffer.toText(buf, Nat.toText)); // => [[1], [2, 2], [4], [5, 5]]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func groupBy<X>(buffer : StableBuffer<X>, equal : (X, X) -> Bool) : StableBuffer<StableBuffer<X>> {
    let _size = size(buffer);
    let newBuffer = initPresized<StableBuffer<X>>(_size);
    if (_size == 0) {
      return newBuffer;
    };

    var i = 0;
    var baseElement = get(buffer, 0);
    var newInnerBuffer = initPresized<X>(_size);
    while (i < _size) {
      let element = get(buffer, i);

      if (equal(baseElement, element)) {
        add(newInnerBuffer, element);
      } else {
        add(newBuffer, newInnerBuffer);
        baseElement := element;
        newInnerBuffer := initPresized<X>(_size - i);
        add(newInnerBuffer, element);
      };
      i += 1;
    };
    if (size(newInnerBuffer) > 0) {
      add(newBuffer, newInnerBuffer);
    };

    newBuffer;
  };

  /// Flattens the buffer of buffers into a single buffer.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// let buffer = Buffer.Buffer<Buffer.Buffer<Nat>>(1);
  ///
  /// let inner1 = Buffer.Buffer<Nat>(2);
  /// inner1.add(1);
  /// inner1.add(2);
  ///
  /// let inner2 = Buffer.Buffer<Nat>(2);
  /// inner2.add(3);
  /// inner2.add(4);
  ///
  /// buffer.add(inner1);
  /// buffer.add(inner2);
  /// // buffer = [[1, 2], [3, 4]]
  ///
  /// let flat = Buffer.flatten<Nat>(buffer);
  /// Buffer.toText<Nat>(flat, Nat.toText); // => [1, 2, 3, 4]
  /// ```
  ///
  /// Runtime: O(number of elements in buffer)
  ///
  /// Space: O(number of elements in buffer)
  public func flatten<X>(buffer : StableBuffer<StableBuffer<X>>) : StableBuffer<X> {
    let _size = size(buffer);
    if (_size == 0) {
      return initPresized<X>(0);
    };

    let newBuffer = initPresized<X>(
      if (size(get(buffer, 0)) != 0) {
        newCapacity(size(get(buffer, 0)) * _size);
      } else {
        newCapacity(_size);
      }
    );

    for (innerBuffer in vals(buffer)) {
      for (innerElement in vals(innerBuffer)) {
        add(newBuffer, innerElement);
      };
    };

    newBuffer;
  };

  /// Combines the two buffers into a single buffer of pairs, pairing together
  /// elements with the same index. If one buffer is longer than the other, the
  /// remaining elements from the longer buffer are not included.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// let buffer1 = Buffer.Buffer<Nat>(2);
  /// buffer1.add(1);
  /// buffer1.add(2);
  /// buffer1.add(3);
  ///
  /// let buffer2 = Buffer.Buffer<Nat>(2);
  /// buffer2.add(4);
  /// buffer2.add(5);
  ///
  /// let zipped = Buffer.zip(buffer1, buffer2);
  /// Buffer.toArray(zipped); // => [(1, 4), (2, 5)]
  /// ```
  ///
  /// Runtime: O(min(size1, size2))
  ///
  /// Space: O(min(size1, size2))
  public func zip<X, Y>(buffer1 : StableBuffer<X>, buffer2 : StableBuffer<Y>) : StableBuffer<(X, Y)> {
    // compiler should pull lamda out as a static function since it is fully closed
    zipWith<X, Y, (X, Y)>(buffer1, buffer2, func(x, y) = (x, y));
  };

  /// Combines the two buffers into a single buffer, pairing together
  /// elements with the same index and combining them using `zip`. If
  /// one buffer is longer than the other, the remaining elements from
  /// the longer buffer are not included.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// let buffer1 = Buffer.Buffer<Nat>(2);
  /// buffer1.add(1);
  /// buffer1.add(2);
  /// buffer1.add(3);
  ///
  /// let buffer2 = Buffer.Buffer<Nat>(2);
  /// buffer2.add(4);
  /// buffer2.add(5);
  /// buffer2.add(6);
  ///
  /// let zipped = Buffer.zipWith<Nat, Nat, Nat>(buffer1, buffer2, func (x, y) { x + y });
  /// Buffer.toArray(zipped) // => [5, 7, 9]
  /// ```
  ///
  /// Runtime: O(min(size1, size2))
  ///
  /// Space: O(min(size1, size2))
  ///
  /// *Runtime and space assumes that `zip` runs in O(1) time and space.
  public func zipWith<X, Y, Z>(buffer1 : StableBuffer<X>, buffer2 : StableBuffer<Y>, zip : (X, Y) -> Z) : StableBuffer<Z> {
    let size1 = size(buffer1);
    let size2 = size(buffer2);
    let minSize = if (size1 < size2) { size1 } else { size2 };

    var i = 0;
    let newBuffer = initPresized<Z>(newCapacity minSize);
    while (i < minSize) {
      add(newBuffer, zip(get(buffer1, i), get(buffer2, i)));
      i += 1;
    };
    newBuffer;
  };

  /// Creates a new buffer taking elements in order from `buffer` until predicate
  /// returns false.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// let newBuf = Buffer.takeWhile<Nat>(buffer, func (x) { x < 3 });
  /// Buffer.toText(newBuf, Nat.toText); // => [1, 2]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func takeWhile<X>(buffer : StableBuffer<X>, predicate : X -> Bool) : StableBuffer<X> {
    let newBuffer = initPresized<X>(size(buffer));

    for (element in vals(buffer)) {
      if (not predicate element) {
        return newBuffer;
      };
      add(newBuffer, element);
    };

    newBuffer;
  };

  /// Creates a new buffer excluding elements in order from `buffer` until predicate
  /// returns false.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// buffer.add(1);
  /// buffer.add(2);
  /// buffer.add(3);
  ///
  /// let newBuf = Buffer.dropWhile<Nat>(buffer, func x { x < 3 }); // => [3]
  /// Buffer.toText(newBuf, Nat.toText);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func dropWhile<X>(buffer : StableBuffer<X>, predicate : X -> Bool) : StableBuffer<X> {
    let newBuffer = initPresized<X>(buffer._size);

    var i = 0;
    var take = false;
    label iter for (element in vals(buffer)) {
      if (not (take or predicate element)) {
        take := true;
      };
      if (take) {
        add(newBuffer, element);
      };
    };
    newBuffer;
  };
};
