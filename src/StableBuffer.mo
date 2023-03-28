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

module {
  private let DECREASE_THRESHOLD = 4; // Don't decrease capacity too early to avoid thrashing
  private let DECREASE_FACTOR = 2;

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

  /// Adds a single element to the buffer.
  public func add<X>(buffer : StableBuffer<X>, elem : X) : () {
    if (buffer.count == buffer.elems.size()) {
      let size = if (buffer.count == 0) {
        if (buffer.initCapacity > 0) { buffer.initCapacity } else { 1 };
      } else {
        2 * buffer.elems.size();
      };

      let elems2 = Prim.Array_init<?X>(size, null);

      var i = 0;
      label l loop {
        if (i >= buffer.count) break l;
        elems2[i] := buffer.elems[i];
        i += 1;
      };

      buffer.elems := elems2;
    };

    buffer.elems[buffer.count] := ?elem;
    buffer.count += 1;
  };

  /// Removes and returns the element at `index` from the buffer.
  /// All elements with index > `index` are shifted one position to the left.
  /// This may cause a downsizing of the array.
  ///
  /// Traps if index >= Buffer.size(buffer).
  ///
  /// WARNING: Repeated removal of elements using this method is ineffecient
  /// and might be a sign that you should consider a different data-structure
  /// for your use case.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// Buffer.add(buffer, 10);
  /// Buffer.add(buffer, 11);
  /// Buffer.add(buffer, 12);
  /// let x = Buffer.remove(buffer, 1); // evaluates to 11. 11 no longer in list.
  /// Buffer.toArray(buffer) // => [10, 12]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Amortized Space: O(1), Worst Case Space: O(size)
  public func remove<X>(buffer : StableBuffer<X>, index : Nat) : X {
    if (index >= buffer.count) {
      Prim.trap "Buffer index out of bounds in remove"
    };

    let element = get(buffer, index);

    // copy elements to new array and shift over in one pass
    if ((buffer.count - 1) : Nat < buffer.elems.size() / DECREASE_THRESHOLD) {
      let elements2 = Prim.Array_init<?X>(buffer.elems.size() / DECREASE_FACTOR, null);

      var i = 0;
      var j = 0;
      label l while (i < buffer.count) {
        if (i == index) {
          i += 1;
          continue l
        };

        elements2[j] := buffer.elems[i];
        i += 1;
        j += 1
      };
      buffer.elems := elements2;
    }
    // just shift over elements
    else {
      var i = index;
      while (i < (buffer.count - 1 : Nat)) {
        buffer.elems[i] := buffer.elems[i + 1];
        i += 1
      };
      buffer.elems[buffer.count - 1] := null;
    };

    buffer.count -= 1;

    element; 
  };

  /// Removes and returns the last item in the buffer or `null` if
  /// the buffer is empty.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// Buffer.add(buffer, 10);
  /// Buffer.add(buffer, 11);
  /// Buffer.removeLast(buffer); // => ?11
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
      // FIXME should this new capacity be a function of _size
      // instead of the current capacity? E.g. _size * INCREASE_FACTOR
      reserve(buffer, buffer.elems.size() / DECREASE_FACTOR)
    };

    lastElement;
  };

  /// Adds all elements in buffer `b` to buffer `a`.
  public func append<X>(a : StableBuffer<X>, b : StableBuffer<X>) : () {
    let i = vals(b);
    loop {
      switch (i.next()) {
        case null return;
        case (?x) { add(a, x) };
      };
    };
  };

  /// Returns the count of elements in the buffer
  public func size<X>(buffer : StableBuffer<X>) : Nat { buffer.count };

  /// Returns the capacity of the buffer (the length of the underlying array).
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// let buffer = Buffer.initPresized<Nat>(2); // underlying array has capacity 2
  /// Buffer.add(buffer, 10);
  /// let size = Buffer.size(buffer);   // => size of 1
  /// let c1 = Buffer.capacity(buffer); // => capacity of 2
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func capacity<X>(buffer: StableBuffer<X>) : Nat = buffer.elems.size();

  /// Changes the capacity to `capacity`. Traps if `capacity` < `Buffer.size(buffer)`.
  ///
  /// ```motoko include=initialize
  ///
  /// Buffer.reserve(buffer, 4);
  /// Buffer.add(buffer, 10);
  /// Buffer.add(buffer, 11);
  /// Buffer.capacity(buffer); // => 4
  /// ```
  ///
  /// Runtime: O(capacity)
  ///
  /// Space: O(capacity)
  public func reserve<X>(buffer: StableBuffer<X>, capacity : Nat) {
    if (capacity < buffer.count) {
      Prim.trap "capacity must be >= size in reserve"
    };

    let elements2 = Prim.Array_init<?X>(capacity, null);

    var i = 0;
    while (i < buffer.count) {
      elements2[i] := buffer.elems[i];
      i += 1
    };
    buffer.elems:= elements2
  };

  /// Resets the buffer.
  public func clear<X>(buffer : StableBuffer<X>) : () {
    buffer.count := 0;
    buffer.elems := [var];
  };

  /// Returns a copy of this buffer.
  public func clone<X>(buffer : StableBuffer<X>) : StableBuffer<X> {
    let c = initPresized<X>(buffer.elems.size());
    var i = vals(buffer);

    label l loop {
      switch (i.next()) {
        case null break l;
        case (?x) { add(c, x) };
      };
    };

    c;
  };

  /// Returns an `Iter` over the elements of this buffer.
  public func vals<X>(buffer : StableBuffer<X>) : { next : () -> ?X } = object {
    var pos = 0;
    public func next() : ?X {
      if (pos == buffer.count) { null } else {
        let elem = buffer.elems[pos];
        pos += 1;
        elem;
      };
    };
  };

  /// Creates a Buffer from an Array
  public func fromArray<X>(xs : [X]) : StableBuffer<X> {
    let ys : StableBuffer<X> = initPresized(xs.size());
    for (x in xs.vals()) {
      add(ys, x);
    };

    ys;
  };

  /// Creates a new array containing this buffer's elements.
  public func toArray<X>(buffer : StableBuffer<X>) : [X] =
  // immutable clone of array
  Prim.Array_tabulate<X>(
    buffer.count,
    func(x : Nat) : X {
      get(buffer, x);
    },
  );

  /// Creates a mutable array containing this buffer's elements.
  public func toVarArray<X>(buffer : StableBuffer<X>) : [var X] {
    if (buffer.count == 0) { [var] } else {
      let a = Prim.Array_init<X>(buffer.count, get(buffer, 0));
      var i = 0;
      label l loop {
        if (i >= buffer.count) break l;
        a[i] := get(buffer, i);
        i += 1;
      };

      a;
    };
  };

  /// Gets the `i`-th element of this buffer. Traps if  `i >= count`. Indexing is zero-based.
  public func get<X>(buffer : StableBuffer<X>, i : Nat) : X {
    switch (buffer.elems[i]) {
      case (?elem) elem;
      case (null) Prim.trap("Buffer index out of bounds in get");
    };
  };

  /// Gets the `i`-th element of the buffer as an option. Returns `null` when `i >= count`. Indexing is zero-based.
  public func getOpt<X>(buffer : StableBuffer<X>, i : Nat) : ?X {
    if (i < buffer.count) {
      buffer.elems[i];
    } else {
      null;
    };
  };

  /// Overwrites the current element at `index` with `element`. Traps if
  /// `index` >= Buffer.size(buffer). Indexing is zero-based.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// Buffer.add(buffer, 10);
  /// Buffer.put(buffer, 0, 20); // overwrites 10 at index 0 with 20
  /// Buffer.toArray(buffer) // => [20]
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func put<X>(buffer : StableBuffer<X>, index : Nat, element : X) {
    if (index >= buffer.count) {
      Prim.trap "Buffer index out of bounds in put"
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
  /// Buffer.add(buffer, 2);
  /// Buffer.add(buffer, 0);
  /// Buffer.add(buffer, 3);
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
        return true
      }
    };

    false
  };

  /// Finds the first index of `element` in `buffer` using equality of elements defined
  /// by `equal`. Returns `null` if `element` is not found.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// Buffer.add(buffer, 1);
  /// Buffer.add(buffer, 2);
  /// Buffer.add(buffer, 3);
  /// Buffer.add(buffer, 4);
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
    let bufferSize = size(buffer);
    var i = 0;
    while (i < bufferSize) {
      if (equal(get(buffer, i), element)) {
        return ?i
      };
      i += 1
    };

    null
  };
};
