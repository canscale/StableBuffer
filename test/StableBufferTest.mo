import Prim "mo:⛔";
import B "../src/StableBuffer";
import I "mo:base/Iter";
import O "mo:base/Option";
import Nat "mo:base/Nat";
import Array "mo:base/Array";

import Suite "mo:matchers/Suite";
import T "mo:matchers/Testable";
import M "mo:matchers/Matchers";

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
assert a.elems[0] == null;
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
  assert (e.elems.size() == 4);
};


var buffer = B.initPresized<Nat>(3);

for (i in I.range(0, 5)) {
  B.add(buffer, i);
};

let containsSuite = suite("contains", [
  suite("with an empty buffer",
    [
      test("returns false for any element",
        B.contains<Nat>(B.initPresized<Nat>(3), 2, Nat.equal),
        M.equals(T.bool(false))
      ),
    ]
  ),
  suite("with a non-empty buffer", [
    test("returns true if the element exists",
      B.contains<Nat>(buffer, 2, Nat.equal),
      M.equals(T.bool(true))
    ),
    test("returns false if the element does not exist",
      B.contains<Nat>(buffer, 9, Nat.equal),
      M.equals(T.bool(false))
    )
  ])
]);

B.clear(buffer);

for (i in I.range(0, 6)) {
  B.add(buffer, i)
};

let indeOfSuite = suite("indexOf", [
  test(
    "find in middle",
    B.indexOf<Nat>(2, buffer, Nat.equal),
    M.equals(T.optional(T.natTestable, ?2))
  ),
  test(
    "find first",
    B.indexOf<Nat>(0, buffer, Nat.equal),
    M.equals(T.optional(T.natTestable, ?0))
  ),
  test(
    "find last",
    B.indexOf<Nat>(6, buffer, Nat.equal),
    M.equals(T.optional(T.natTestable, ?6))
  ),
  test(
    "not found",
    B.indexOf<Nat>(10, buffer, Nat.equal),
    M.equals(T.optional(T.natTestable, null : ?Nat))
  ),
  test(
    "empty",
    B.indexOf<Nat>(100, B.initPresized<Nat>(3), Nat.equal),
    M.equals(T.optional(T.natTestable, null : ?Nat))
  )
]);


for (i in I.range(0, 6)) {
  B.add(buffer, i)
};

let clearSuite = suite("clear", [
  test("before clearing, buffer has at least one element",
    B.capacity(buffer) > 0 and B.size(buffer) > 0,
    M.equals(T.bool(true))
  ),
  test("clears a buffer with elements",
    do {
      B.clear(buffer);
      B.capacity(buffer) == 0 and B.size(buffer) == 0;
    },
    M.equals(T.bool(true))
  ),
  test("clearing an empty buffer makes no difference",
    do {
      B.clear(B.initPresized<Nat>(3));
      B.capacity(buffer) == 0 and B.size(buffer) == 0;
    },
    M.equals(T.bool(true))
  ),
]);

buffer := B.init<Nat>();
for (i in I.range(0, 5)) {
  B.add(buffer, i);
};

let removeSuite = suite("remove", [
  suite("middle element",
    [
      test(
        "return value",
        B.remove(buffer, 2),
        M.equals(T.nat(2))
      ),
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(5))
      ),
      test(
        "underlying array size",
        buffer.elems.size(),
        M.equals(T.nat(8))
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 3, 4, 5]))
      ),
      test(
        "then remove first element",
        B.remove(buffer, 0),
        M.equals(T.nat(0))
      ),
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(4))
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8))
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [1, 3, 4, 5]))
      )
    ]
  ),
  suite(
    "remove last element at capacity",
    [
      test(
        "return value",
        do {
          buffer := B.initPresized<Nat>(3);
          for (i in I.range(0, 2)) {
            B.add(buffer,i)
          };
          B.remove(buffer, 2)
        },
        M.equals(T.nat(2))
      ),
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(2))
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(3))
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1]))
      )
    ]
  ),
    suite(
    "remove until empty",
    [
      test(
        "size",
        do {
          buffer := B.init<Nat>();
          for (i in I.range(0, 5)) {
            B.add(buffer, i)
          };
          for (i in I.range(0, 5)) {
            ignore B.remove(buffer, 5 - i : Nat);
          };
          B.size(buffer)
        },
        M.equals(T.nat(0))
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(2))
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, []))
      )
    ]
  )
]);

buffer := B.initPresized<Nat>(2);

let removeLastSuite = suite("removeLast", [
  suite("on empty buffer", [
    test(
      "return value",
      do {
        buffer := B.initPresized<Nat>(2);
        B.removeLast(buffer);
      },
      M.equals(T.optional(T.natTestable, null : ?Nat))
    ),
    test(
      "size",
      B.size(buffer),
      M.equals(T.nat(0))
    ),
    test(
      "capacity",
      B.capacity(buffer),
      M.equals(T.nat(2))
    ),
    test(
      "elements",
      B.toArray(buffer),
      M.equals(T.array<Nat>(T.natTestable, []))
    )
  ]),
  suite("once from filled buffer", [
    test(
      "return value",
      do {
        buffer := B.initPresized<Nat>(2);
        for (i in I.range(0, 5)) {
          B.add(buffer, i)
        };
        B.removeLast(buffer);
      },
      M.equals(T.optional<Nat>(T.natTestable, ?5))
    ),
    test(
      "size",
      B.size(buffer),
      M.equals(T.nat(5))
    ),
    test(
      "capacity",
      B.capacity(buffer),
      M.equals(T.nat(8))
    ),
    test(
      "elements",
      B.toArray(buffer),
      M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4]))
    )
  ]),
  suite("removeLast until empty", [
    test(
      "return value",
      do {
        buffer := B.initPresized<Nat>(3);
        for (i in I.range(0, 5)) {
          B.add(buffer, i)
        };
        for (i in I.range(0, 5)) {
          let x = B.removeLast(buffer);
        };
        B.removeLast(buffer);
      },
      M.equals(T.optional(T.natTestable, null : ?Nat))
    ),
    test(
      "size",
      B.size(buffer),
      M.equals(T.nat(0))
    ),
    test(
      "capacity",
      B.capacity(buffer),
      M.equals(T.nat(3))
    ),
    test(
      "elements",
      B.toArray(buffer),
      M.equals(T.array<Nat>(T.natTestable, []))
    )
  ])
]);

let reserveSuite = suite("reserve", [
  suite("decrease capacity", [
    test(
      "size",
      do {
        buffer := B.initPresized<Nat>(10);
        for (i in I.range(0, 5)) {
          B.add(buffer, i)
        };
        B.reserve(buffer, 6);
        B.size(buffer);
      },
      M.equals(T.nat(6))
    ),
    test(
      "capacity",
      B.capacity(buffer),
      M.equals(T.nat(6))
    ),
    test(
      "elements",
      B.toArray(buffer),
      M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5]))
    )
  ]),
  suite("increase capacity", [
    test(
      "size",
      do {
        buffer := B.initPresized<Nat>(10);
        for (i in I.range(0, 5)) {
          B.add(buffer, i)
        };

        B.reserve(buffer, 20);
        B.size(buffer);
      },
      M.equals(T.nat(6))
    ),
    test(
      "capacity",
      B.capacity(buffer),
      M.equals(T.nat(20))
    ),
    test(
      "elements",
      B.toArray(buffer),
      M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5]))
    )
  ])
]);

run(suite("buffer", [
  containsSuite,
  indeOfSuite,
  clearSuite,
  removeSuite,
  removeLastSuite,
  reserveSuite
]))
