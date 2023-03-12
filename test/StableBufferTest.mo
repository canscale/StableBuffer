import Prim "mo:⛔";
import B "../src/StableBuffer";
import I "mo:base/Iter";
import O "mo:base/Option";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import Suite "mo:matchers/Suite";
import M "mo:matchers/Matchers";
import T "mo:matchers/Testable";

let { run; test; suite } = Suite;

// test repeated growing
let a = B.initPresized<Nat>(3);
for (i in I.range(0, 123)) {
  B.add(a, i);
};
for (i in I.range(0, 123)) {
  assert (B.get(a, i) == i);
};


// test repeated appending
let b = B.initPresized<Nat>(3);
for (i in I.range(0, 123)) {
  B.append(b, a);
};

// test repeated removing
for (i in I.revRange(123, 0)) {
  switch(B.removeLast(a)) {
    case null { assert false };
    case (?el) { assert el == i };
  }
};
assert O.isNull(B.removeLast(a));

func natArrayIter(elems:[Nat]) : I.Iter<Nat> = object {
  var pos = 0;
  let count = elems.size();
  public func next() : ?Nat {
    if (pos == count) { null } else {
      let elem = ?elems[pos];
      pos += 1;
      elem
    }
  }
};

func natVarArrayIter(elems:[var Nat]) : I.Iter<Nat> = object {
  var pos = 0;
  let count = elems.size();
  public func next() : ?Nat {
    if (pos == count) { null } else {
      let elem = ?elems[pos];
      pos += 1;
      elem
    }
  }
};

func natIterEq(a:I.Iter<Nat>, b:I.Iter<Nat>) : Bool {
   switch (a.next(), b.next()) {
     case (null, null) { true };
     case (?x, ?y) {
       if (x == y) { natIterEq(a, b) }
       else { false }
     };
     case (_, _) { false };
   }
};

// regression test: buffers with extra space are converted to arrays of the correct length
do {
  let bigLen = 100;
  let len = 3;
  let c = B.initPresized<Nat>(bigLen);
  assert (len < bigLen);
  for (i in I.range(0, len - 1)) {
    B.add(c, i);
  };
  assert (B.size<Nat>(c) == len);
  assert (B.toArray(c).size() == len);
  assert (natIterEq(B.vals(c), natArrayIter(B.toArray(B.clone<Nat>(c)))));
  assert (B.toVarArray(c).size() == len);
  assert (natIterEq(B.vals(c), natVarArrayIter(B.toVarArray(B.clone(c)))));
};

// regression test: initially-empty buffers grow, element-by-element
do {
  let c = B.initPresized<Nat>(0);
  assert (B.toArray(c).size() == 0);
  assert (B.toVarArray(c).size() == 0);
  B.add(c, 0);
  assert (B.toArray(c).size() == 1);
  assert (B.toVarArray(c).size() == 1);
  B.add(c, 0);
  assert (B.toArray(c).size() == 2);
  assert (B.toVarArray(c).size() == 2);
};

// test fromArray
do {
  let arr = [1,2,3,4,5];
  let d = B.fromArray<Nat>(arr);
  assert (natIterEq(B.vals<Nat>(d), arr.vals())); 
  assert (B.size<Nat>(d) == arr.size()); 
};

// test init
do {
  let e = B.init<Nat>();
  assert (B.toArray(e).size() == 0);
  assert (B.toVarArray(e).size() == 0);
  B.add(e, 0);
  assert (B.toArray(e).size() == 1);
  assert (B.toVarArray(e).size() == 1);
  B.add(e, 1);
  B.add(e, 2);
  assert (B.toArray(e).size() == 3);
  assert (B.toVarArray(e).size() == 3);
  assert (e.elems.size() == 3);
};

// test capacity
do {
  let buffer = B.initPresized<Nat>(2); // underlying array has capacity 2
  B.add(buffer, 10);
  assert(B.capacity(buffer) == 2);
  B.add(buffer, 11);
  B.add(buffer, 12); // causes capacity to increase by factor of 1.5
  assert(B.capacity(buffer) == 3);
};

// test insert
do {
  let buffer = B.initPresized<Nat>(2);
  B.add(buffer, 10);
  B.add(buffer, 11);
  B.insert(buffer, 1, 9);
  assert(B.capacity(buffer) == 3); // ceil (2 * 1.5)
  B.insert(buffer, 1, 15);
  assert(B.capacity(buffer) == 5); // ceil(3 * 1.5)
  
  let expected = [10, 15, 9, 11];
  assert (natIterEq(B.vals<Nat>(buffer), expected.vals())); 
};

// test insertBuffer
do {
  let buffer = B.initPresized<Nat>(15);
  for (i in Iter.range(0, 5)) {
    B.add(buffer, i)
  };

  let buffer2 = B.initPresized<Nat>(10);
  for (i in Iter.range(10, 15)) {
    B.add(buffer2, i)
  };

  B.insertBuffer(buffer, 3, buffer2);

  run(
    suite(
      "insertBuffer",
      [
        test(
          "size",
          B.size(buffer),
          M.equals(T.nat(12))
        ),
        test(
          "capacity",
          B.capacity(buffer),
          M.equals(T.nat(15))
        ),
        test(
          "elements",
          B.toArray(buffer),
          M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 10, 11, 12, 13, 14, 15, 3, 4, 5]))
        )
      ]
    )
  );
};

// test insertBuffer at start
do {
  let buffer = B.initPresized<Nat>(15);
  for (i in Iter.range(0, 5)) {
    B.add(buffer, i)
  };

  let buffer2 = B.initPresized<Nat>(10);
  for (i in Iter.range(10, 15)) {
    B.add(buffer2, i)
  };

  B.insertBuffer(buffer, 0, buffer2);

  run(
    suite(
      "insertBuffer at start",
      [
        test(
          "size",
          B.size(buffer),
          M.equals(T.nat(12))
        ),
        test(
          "capacity",
          B.capacity(buffer),
          M.equals(T.nat(15))
        ),
        test(
          "elements",
          B.toArray(buffer),
          M.equals(T.array<Nat>(T.natTestable, [10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5]))
        )
      ]
    )
  );
};

// test insertBuffer at end
do {
  let buffer = B.initPresized<Nat>(15);
  for (i in Iter.range(0, 5)) {
    B.add(buffer, i)
  };

  let buffer2 = B.initPresized<Nat>(10);
  for (i in Iter.range(10, 15)) {
    B.add(buffer2, i)
  };

  B.insertBuffer(buffer, 6, buffer2);

  run(
    suite(
      "insertBuffer at end",
      [
        test(
          "size",
          B.size(buffer),
          M.equals(T.nat(12))
        ),
        test(
          "capacity",
          B.capacity(buffer),
          M.equals(T.nat(15))
        ),
        test(
          "elements",
          B.toArray(buffer),
          M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5, 10, 11, 12, 13, 14, 15]))
        )
      ]
    )
  );
};

// test insertBuffer with capacity change
do {
  let buffer = B.initPresized<Nat>(8);
  for (i in Iter.range(0, 5)) {
    B.add(buffer, i)
  };

  let buffer2 = B.initPresized<Nat>(10);
  for (i in Iter.range(10, 15)) {
    B.add(buffer2, i)
  };

  B.insertBuffer(buffer, 3, buffer2);

  run(
    suite(
      "insertBuffer with capacity change",
      [
        test(
          "size",
          B.size(buffer),
          M.equals(T.nat(12))
        ),
        test(
          "capacity",
          B.capacity(buffer),
          M.equals(T.nat(18))
        ),
        test(
          "elements",
          B.toArray(buffer),
          M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 10, 11, 12, 13, 14, 15, 3, 4, 5]))
        )
      ]
    )
  );
};

// test insertBuffer at start with capacity change
do {
  let buffer = B.initPresized<Nat>(8);
  for (i in Iter.range(0, 5)) {
    B.add(buffer, i)
  };

  let buffer2 = B.initPresized<Nat>(10);
  for (i in Iter.range(10, 15)) {
    B.add(buffer2, i)
  };

  B.insertBuffer(buffer, 0, buffer2);

  run(
    suite(
      "insertBuffer at start with capacity change",
      [
        test(
          "size",
          B.size(buffer),
          M.equals(T.nat(12))
        ),
        test(
          "capacity",
          B.capacity(buffer),
          M.equals(T.nat(18))
        ),
        test(
          "elements",
          B.toArray(buffer),
          M.equals(T.array<Nat>(T.natTestable, [10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5]))
        )
      ]
    )
  );
};

// test insertBuffer at end with capacity change
do {
  let buffer = B.initPresized<Nat>(8);
  for (i in Iter.range(0, 5)) {
    B.add(buffer, i)
  };

  let buffer2 = B.initPresized<Nat>(10);
  for (i in Iter.range(10, 15)) {
    B.add(buffer2, i)
  };

  B.insertBuffer(buffer, 6, buffer2);

  run(
    suite(
      "insertBuffer at end with capacity change",
      [
        test(
          "size",
          B.size(buffer),
          M.equals(T.nat(12))
        ),
        test(
          "capacity",
          B.capacity(buffer),
          M.equals(T.nat(18))
        ),
        test(
          "elements",
          B.toArray(buffer),
          M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5, 10, 11, 12, 13, 14, 15]))
        )
      ]
    )
  );
};

// test insertBuffer to empty buffer
do {
  let buffer = B.init<Nat>();

  let buffer2 = B.initPresized<Nat>(10);
  for (i in Iter.range(10, 15)) {
    B.add(buffer2, i)
  };

  B.insertBuffer(buffer, 0, buffer2);

  run(
    suite(
      "insertBuffer to empty buffer",
      [
        test(
          "size",
          B.size(buffer),
          M.equals(T.nat(6))
        ),
        test(
          "capacity",
          B.capacity(buffer),
          M.equals(T.nat(9))
        ),
        test(
          "elements",
          B.toArray(buffer),
          M.equals(T.array<Nat>(T.natTestable, [10, 11, 12, 13, 14, 15]))
        )
      ]
    )
  );
};
