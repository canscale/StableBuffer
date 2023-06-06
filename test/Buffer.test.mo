import Prim "mo:⛔";
import B "../src/StableBuffer";
import Iter "mo:base/Iter";
import Option "mo:base/Option";
import Nat "mo:base/Nat";
import Hash "mo:base/Hash";
import Nat32 "mo:base/Nat32";
import Order "mo:base/Order";

import Suite "mo:matchers/Suite";
import T "mo:matchers/Testable";
import M "mo:matchers/Matchers";

let { run; test; suite } = Suite;

let NatBufferTestable : T.Testable<B.StableBuffer<Nat>> = object {
  public func display(buffer : B.StableBuffer<Nat>) : Text {
    B.toText(buffer, Nat.toText);
  };
  public func equals(buffer1 : B.StableBuffer<Nat>, buffer2 : B.StableBuffer<Nat>) : Bool {
    B.equal(buffer1, buffer2, Nat.equal);
  };
};

class OrderTestable(initItem : Order.Order) : T.TestableItem<Order.Order> {
  public let item = initItem;
  public func display(order : Order.Order) : Text {
    switch (order) {
      case (#less) {
        "#less";
      };
      case (#greater) {
        "#greater";
      };
      case (#equal) {
        "#equal";
      };
    };
  };
  public let equals = Order.equal;
};

/* --------------------------------------- */
run(
  suite(
    "construct",
    [
      test(
        "initial size",
        B.size(B.initPresized<Nat>(10)),
        M.equals(T.nat(0)),
      ),
      test(
        "initial capacity",
        B.capacity(B.initPresized<Nat>(10)),
        M.equals(T.nat(10)),
      ),
    ],
  )
);

/* --------------------------------------- */

var buffer = B.initPresized<Nat>(10);
for (i in Iter.range(0, 3)) {
  B.add(buffer, i);
};

run(
  suite(
    "add",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(4)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(10)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(2);
for (i in Iter.range(0, 3)) {
  B.add(buffer, i);
};

run(
  suite(
    "add with capacity change",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(4)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(5)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(0);
for (i in Iter.range(0, 3)) {
  B.add(buffer, i);
};

run(
  suite(
    "add with capacity change, initial capacity 0",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(4)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(5)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(2);

run(
  suite(
    "removeLast on empty buffer",
    [
      test(
        "return value",
        B.removeLast(buffer),
        M.equals(T.optional(T.natTestable, null : ?Nat)),
      ),
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(0)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(2)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(2);
for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

run(
  suite(
    "removeLast",
    [
      test(
        "return value",
        B.removeLast(buffer),
        M.equals(T.optional<Nat>(T.natTestable, ?5)),
      ),
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(5)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);
for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

for (i in Iter.range(0, 5)) {
  ignore B.removeLast(buffer);
};

run(
  suite(
    "removeLast until empty",
    [
      test(
        "return value",
        B.removeLast(buffer),
        M.equals(T.optional(T.natTestable, null : ?Nat)),
      ),
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(0)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(2)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(0);
for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

run(
  suite(
    "remove",
    [
      test(
        "return value",
        B.remove(buffer, 2),
        M.equals(T.nat(2)),
      ),
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(5)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 3, 4, 5])),
      ),
      test(
        "return value",
        B.remove(buffer, 0),
        M.equals(T.nat(0)),
      ),
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(4)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [1, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);
for (i in Iter.range(0, 2)) {
  B.add(buffer, i);
};

run(
  suite(
    "remove last element at capacity",
    [
      test(
        "return value",
        B.remove(buffer, 2),
        M.equals(T.nat(2)),
      ),
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(2)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(3)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(0);
for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

for (i in Iter.range(0, 5)) {
  ignore B.remove(buffer, 5 - i : Nat);
};

run(
  suite(
    "remove until empty",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(0)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(2)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(1);
for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

B.filterEntries<Nat>(buffer, func(_, x) = x % 2 == 0);

run(
  suite(
    "filterEntries",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(3)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 2, 4])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(1);
B.filterEntries<Nat>(buffer, func(_, x) = x % 2 == 0);

run(
  suite(
    "filterEntries on empty",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(0)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(1)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(12);
for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};
B.filterEntries<Nat>(buffer, func(i, x) = i + x == 2);

run(
  suite(
    "filterEntries size down",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(1)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(6)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [1])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(5);
for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};
B.filterEntries<Nat>(buffer, func(_, _) = false);

run(
  suite(
    "filterEntries size down to empty",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(0)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(2)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [] : [Nat])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(10);
for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

run(
  suite(
    "get and getOpt",
    [
      test(
        "get",
        B.get(buffer, 2),
        M.equals(T.nat(2)),
      ),
      test(
        "getOpt success",
        B.getOpt(buffer, 0),
        M.equals(T.optional(T.natTestable, ?0)),
      ),
      test(
        "getOpt out of bounds",
        B.getOpt(buffer, 10),
        M.equals(T.optional(T.natTestable, null : ?Nat)),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(10);
for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

B.put(buffer, 2, 20);

run(
  suite(
    "put",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(6)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 20, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(10);
for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

B.reserve(buffer, 6);

run(
  suite(
    "decrease capacity",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(6)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(6)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(10);
for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

B.reserve(buffer, 20);

run(
  suite(
    "increase capacity",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(6)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(20)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(10);
for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

var buffer2 = B.initPresized<Nat>(20);

for (i in Iter.range(10, 15)) {
  B.add(buffer2, i);
};

B.append(buffer, buffer2);

run(
  suite(
    "append",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(12)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(18)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5, 10, 11, 12, 13, 14, 15])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(10);
for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

buffer2 := B.initPresized<Nat>(0);

B.append(buffer, buffer2);

run(
  suite(
    "append empty buffer",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(6)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(10)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(10);

buffer2 := B.initPresized<Nat>(10);

for (i in Iter.range(0, 5)) {
  B.add(buffer2, i);
};

B.append(buffer, buffer2);

run(
  suite(
    "append to empty buffer",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(6)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(10)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(8);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

B.insert(buffer, 3, 30);

run(
  suite(
    "insert",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(7)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 30, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(8);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

B.insert(buffer, 6, 60);

run(
  suite(
    "insert at back",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(7)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5, 60])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(8);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

B.insert(buffer, 0, 10);

run(
  suite(
    "insert at front",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(7)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [10, 0, 1, 2, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(6);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

B.insert(buffer, 3, 30);

run(
  suite(
    "insert with capacity change",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(7)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(9)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 30, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(5);

B.insert(buffer, 0, 0);

run(
  suite(
    "insert into empty buffer",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(1)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(5)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(0);

B.insert(buffer, 0, 0);

run(
  suite(
    "insert into empty buffer with capacity change",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(1)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(1)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(15);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

buffer2 := B.initPresized<Nat>(10);

for (i in Iter.range(10, 15)) {
  B.add(buffer2, i);
};

B.insertBuffer(buffer, 3, buffer2);

run(
  suite(
    "insertBuffer",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(12)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(15)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 10, 11, 12, 13, 14, 15, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(15);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

buffer2 := B.initPresized<Nat>(10);

for (i in Iter.range(10, 15)) {
  B.add(buffer2, i);
};

B.insertBuffer(buffer, 0, buffer2);

run(
  suite(
    "insertBuffer at start",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(12)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(15)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(15);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

buffer2 := B.initPresized<Nat>(10);

for (i in Iter.range(10, 15)) {
  B.add(buffer2, i);
};

B.insertBuffer(buffer, 6, buffer2);

run(
  suite(
    "insertBuffer at end",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(12)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(15)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5, 10, 11, 12, 13, 14, 15])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(8);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

buffer2 := B.initPresized<Nat>(10);

for (i in Iter.range(10, 15)) {
  B.add(buffer2, i);
};

B.insertBuffer(buffer, 3, buffer2);

run(
  suite(
    "insertBuffer with capacity change",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(12)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(18)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 10, 11, 12, 13, 14, 15, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(8);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

buffer2 := B.initPresized<Nat>(10);

for (i in Iter.range(10, 15)) {
  B.add(buffer2, i);
};

B.insertBuffer(buffer, 0, buffer2);

run(
  suite(
    "insertBuffer at start with capacity change",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(12)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(18)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(8);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

buffer2 := B.initPresized<Nat>(10);

for (i in Iter.range(10, 15)) {
  B.add(buffer2, i);
};

B.insertBuffer(buffer, 6, buffer2);

run(
  suite(
    "insertBuffer at end with capacity change",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(12)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(18)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5, 10, 11, 12, 13, 14, 15])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(7);

buffer2 := B.initPresized<Nat>(10);

for (i in Iter.range(10, 15)) {
  B.add(buffer2, i);
};

B.insertBuffer(buffer, 0, buffer2);

run(
  suite(
    "insertBuffer to empty buffer",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(6)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(7)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [10, 11, 12, 13, 14, 15])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

buffer2 := B.initPresized<Nat>(10);

for (i in Iter.range(10, 15)) {
  B.add(buffer2, i);
};

B.insertBuffer(buffer, 0, buffer2);

run(
  suite(
    "insertBuffer to empty buffer with capacity change",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(6)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(9)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [10, 11, 12, 13, 14, 15])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

B.clear(buffer);

run(
  suite(
    "clear",
    [
      test(
        "size",
        B.size(buffer),
        M.equals(T.nat(0)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

buffer2 := B.clone(buffer);

run(
  suite(
    "clone",
    [
      test(
        "size",
        B.size(buffer2),
        M.equals(T.nat(B.size(buffer))),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(B.capacity(buffer2))),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, B.toArray(buffer2))),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

var size = 0;

for (element in B.vals(buffer)) {
  M.assertThat(element, M.equals(T.nat(size)));
  size += 1;
};

run(
  suite(
    "vals",
    [
      test(
        "size",
        size,
        M.equals(T.nat(7)),
      )
    ],
  )
);

/* --------------------------------------- */
run(
  suite(
    "array round trips",
    [
      test(
        "fromArray and toArray",
        B.toArray<Nat>(B.fromArray<Nat>([0, 1, 2, 3])),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3])),
      ),
      test(
        "fromVarArray",
        B.toArray<Nat>(B.fromVarArray<Nat>([var 0, 1, 2, 3])),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3])),
      ),
    ],
  )
);

/* --------------------------------------- */
run(
  suite(
    "empty array round trips",
    [
      test(
        "fromArray and toArray",
        B.toArray<Nat>(B.fromArray<Nat>([])),
        M.equals(T.array<Nat>(T.natTestable, [])),
      ),
      test(
        "fromVarArray",
        B.toArray<Nat>(B.fromVarArray<Nat>([var])),
        M.equals(T.array<Nat>(T.natTestable, [])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

run(
  suite(
    "iter round trips",
    [
      test(
        "fromIter and vals",
        B.toArray(B.fromIter<Nat>(B.vals(buffer))),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5, 6])),
      ),
      test(
        "empty",
        B.toArray(B.fromIter<Nat>(B.vals(B.initPresized<Nat>(2)))),
        M.equals(T.array<Nat>(T.natTestable, [] : [Nat])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

B.trimToSize(buffer);

run(
  suite(
    "trimToSize",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(7)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5, 6])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

B.trimToSize(buffer);

run(
  suite(
    "trimToSize on empty",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(0)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

buffer2 := B.map<Nat, Nat>(buffer, func x = x * 2);

run(
  suite(
    "map",
    [
      test(
        "capacity",
        B.capacity(buffer2),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer2),
        M.equals(T.array<Nat>(T.natTestable, [0, 2, 4, 6, 8, 10, 12])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(0);

buffer2 := B.map<Nat, Nat>(buffer, func x = x * 2);

run(
  suite(
    "map empty",
    [
      test(
        "capacity",
        B.capacity(buffer2),
        M.equals(T.nat(0)),
      ),
      test(
        "elements",
        B.toArray(buffer2),
        M.equals(T.array<Nat>(T.natTestable, [])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

var sum = 0;

B.iterate<Nat>(buffer, func x = sum += x);

run(
  suite(
    "iterate",
    [
      test(
        "sum",
        sum,
        M.equals(T.nat(21)),
      ),
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 2, 3, 4, 5, 6])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

buffer2 := B.chain<Nat, Nat>(buffer, func x = B.make<Nat> x);

run(
  suite(
    "chain",
    [
      test(
        "elements",
        B.toArray(buffer2),
        M.equals(T.array<Nat>(T.natTestable, B.toArray(buffer))),
      )
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

buffer2 := B.mapFilter<Nat, Nat>(buffer, func x = if (x % 2 == 0) { ?x } else { null });

run(
  suite(
    "mapFilter",
    [
      test(
        "capacity",
        B.capacity(buffer2),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer2),
        M.equals(T.array<Nat>(T.natTestable, [0, 2, 4, 6])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

buffer2 := B.mapEntries<Nat, Nat>(buffer, func(i, x) = i * x);

run(
  suite(
    "mapEntries",
    [
      test(
        "capacity",
        B.capacity(buffer2),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer2),
        M.equals(T.array<Nat>(T.natTestable, [0, 1, 4, 9, 16, 25, 36])),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

var bufferResult = B.mapResult<Nat, Nat, Text>(buffer, func x = #ok x);

run(
  suite(
    "mapResult success",
    [
      test(
        "return value",
        #ok buffer,
        M.equals(T.result<B.StableBuffer<Nat>, Text>(NatBufferTestable, T.textTestable, bufferResult)),
      )
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

bufferResult := B.mapResult<Nat, Nat, Text>(
  buffer,
  func x = if (x == 4) { #err "error" } else { #ok x },
);

run(
  suite(
    "mapResult failure",
    [
      test(
        "return value",
        #err "error",
        M.equals(T.result<B.StableBuffer<Nat>, Text>(NatBufferTestable, T.textTestable, bufferResult)),
      )
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

run(
  suite(
    "foldLeft",
    [
      test(
        "return value",
        B.foldLeft<Text, Nat>(buffer, "", func(acc, x) = acc # Nat.toText(x)),
        M.equals(T.text("0123456")),
      ),
      test(
        "return value empty",
        B.foldLeft<Text, Nat>(B.initPresized<Nat>(4), "", func(acc, x) = acc # Nat.toText(x)),
        M.equals(T.text("")),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

run(
  suite(
    "foldRight",
    [
      test(
        "return value",
        B.foldRight<Nat, Text>(buffer, "", func(x, acc) = acc # Nat.toText(x)),
        M.equals(T.text("6543210")),
      ),
      test(
        "return value empty",
        B.foldRight<Nat, Text>(B.initPresized<Nat>(4), "", func(x, acc) = acc # Nat.toText(x)),
        M.equals(T.text("")),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

run(
  suite(
    "forAll",
    [
      test(
        "true",
        B.forAll<Nat>(buffer, func x = x >= 0),
        M.equals(T.bool(true)),
      ),
      test(
        "false",
        B.forAll<Nat>(buffer, func x = x % 2 == 0),
        M.equals(T.bool(false)),
      ),
      test(
        "default",
        B.forAll<Nat>(B.initPresized<Nat>(2), func _ = false),
        M.equals(T.bool(true)),
      ),
    ],
  )
);

/* --------------------------------------- */
run(
  suite(
    "forSome",
    [
      test(
        "true",
        B.forSome<Nat>(buffer, func x = x % 2 == 0),
        M.equals(T.bool(true)),
      ),
      test(
        "false",
        B.forSome<Nat>(buffer, func x = x < 0),
        M.equals(T.bool(false)),
      ),
      test(
        "default",
        B.forSome<Nat>(B.initPresized<Nat>(2), func _ = false),
        M.equals(T.bool(false)),
      ),
    ],
  )
);

/* --------------------------------------- */
run(
  suite(
    "forNone",
    [
      test(
        "true",
        B.forNone<Nat>(buffer, func x = x < 0),
        M.equals(T.bool(true)),
      ),
      test(
        "false",
        B.forNone<Nat>(buffer, func x = x % 2 != 0),
        M.equals(T.bool(false)),
      ),
      test(
        "default",
        B.forNone<Nat>(B.initPresized<Nat>(2), func _ = true),
        M.equals(T.bool(true)),
      ),
    ],
  )
);

/* --------------------------------------- */

buffer := B.make<Nat>(1);

run(
  suite(
    "make",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(1)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [1])),
      ),
    ],
  )
);

/* --------------------------------------- */

buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

run(
  suite(
    "contains",
    [
      test(
        "true",
        B.contains<Nat>(buffer, 2, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "true",
        B.contains<Nat>(buffer, 9, Nat.equal),
        M.equals(T.bool(false)),
      ),
    ],
  )
);

/* --------------------------------------- */

buffer := B.initPresized<Nat>(3);

run(
  suite(
    "contains empty",
    [
      test(
        "true",
        B.contains<Nat>(buffer, 2, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "true",
        B.contains<Nat>(buffer, 9, Nat.equal),
        M.equals(T.bool(false)),
      ),
    ],
  )
);

/* --------------------------------------- */

buffer := B.initPresized<Nat>(3);

B.add(buffer, 2);
B.add(buffer, 1);
B.add(buffer, 10);
B.add(buffer, 1);
B.add(buffer, 0);
B.add(buffer, 3);

run(
  suite(
    "max",
    [
      test(
        "return value",
        B.max<Nat>(buffer, Nat.compare),
        M.equals(T.optional(T.natTestable, ?10)),
      )
    ],
  )
);

/* --------------------------------------- */

buffer := B.initPresized<Nat>(3);

B.add(buffer, 2);
B.add(buffer, 1);
B.add(buffer, 10);
B.add(buffer, 1);
B.add(buffer, 0);
B.add(buffer, 3);
B.add(buffer, 0);

run(
  suite(
    "min",
    [
      test(
        "return value",
        B.min<Nat>(buffer, Nat.compare),
        M.equals(T.optional(T.natTestable, ?0)),
      )
    ],
  )
);

/* --------------------------------------- */

buffer := B.initPresized<Nat>(3);
B.add(buffer, 2);

run(
  suite(
    "isEmpty",
    [
      test(
        "true",
        B.isEmpty(B.initPresized<Nat>(2)),
        M.equals(T.bool(true)),
      ),
      test(
        "false",
        B.isEmpty(buffer),
        M.equals(T.bool(false)),
      ),
    ],
  )
);

/* --------------------------------------- */

buffer := B.initPresized<Nat>(3);

B.add(buffer, 2);
B.add(buffer, 1);
B.add(buffer, 10);
B.add(buffer, 1);
B.add(buffer, 0);
B.add(buffer, 3);
B.add(buffer, 0);

B.removeDuplicates<Nat>(buffer, Nat.compare);

run(
  suite(
    "removeDuplicates",
    [
      test(
        "elements (stable ordering)",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [2, 1, 10, 0, 3])),
      )
    ],
  )
);

/* --------------------------------------- */

buffer := B.initPresized<Nat>(3);

B.removeDuplicates<Nat>(buffer, Nat.compare);

run(
  suite(
    "removeDuplicates empty",
    [
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      )
    ],
  )
);

/* --------------------------------------- */

buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 4)) {
  B.add(buffer, 2);
};

B.removeDuplicates<Nat>(buffer, Nat.compare);

run(
  suite(
    "removeDuplicates repeat singleton",
    [
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [2])),
      )
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

run(
  suite(
    "hash",
    [
      test(
        "empty buffer",
        Nat32.toNat(B.hash<Nat>(B.initPresized<Nat>(8), Hash.hash)),
        M.equals(T.nat(0)),
      ),
      test(
        "non-empty buffer",
        Nat32.toNat(B.hash<Nat>(buffer, Hash.hash)),
        M.equals(T.nat(3365238326)),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

run(
  suite(
    "toText",
    [
      test(
        "empty buffer",
        B.toText<Nat>(B.initPresized<Nat>(3), Nat.toText),
        M.equals(T.text("[]")),
      ),
      test(
        "singleton buffer",
        B.toText<Nat>(B.make<Nat>(3), Nat.toText),
        M.equals(T.text("[3]")),
      ),
      test(
        "non-empty buffer",
        B.toText<Nat>(buffer, Nat.toText),
        M.equals(T.text("[0, 1, 2, 3, 4, 5]")),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

buffer2 := B.initPresized<Nat>(3);

for (i in Iter.range(0, 2)) {
  B.add(buffer, i);
};

run(
  suite(
    "equal",
    [
      test(
        "empty buffers",
        B.equal<Nat>(B.initPresized<Nat>(3), B.initPresized<Nat>(2), Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "non-empty buffers",
        B.equal<Nat>(buffer, B.clone(buffer), Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "non-empty and empty buffers",
        B.equal<Nat>(buffer, B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "non-empty buffers mismatching lengths",
        B.equal<Nat>(buffer, buffer2, Nat.equal),
        M.equals(T.bool(false)),
      ),
    ],
  )
);

/* --------------------------------------- */
buffer := B.initPresized<Nat>(3);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

buffer2 := B.initPresized<Nat>(3);

for (i in Iter.range(0, 2)) {
  B.add(buffer, i);
};

var buffer3 = B.initPresized<Nat>(3);

for (i in Iter.range(2, 5)) {
  B.add(buffer3, i);
};

run(
  suite(
    "compare",
    [
      test(
        "empty buffers",
        B.compare<Nat>(B.initPresized<Nat>(3), B.initPresized<Nat>(2), Nat.compare),
        M.equals(OrderTestable(#equal)),
      ),
      test(
        "non-empty buffers equal",
        B.compare<Nat>(buffer, B.clone(buffer), Nat.compare),
        M.equals(OrderTestable(#equal)),
      ),
      test(
        "non-empty and empty buffers",
        B.compare<Nat>(buffer, B.initPresized<Nat>(3), Nat.compare),
        M.equals(OrderTestable(#greater)),
      ),
      test(
        "non-empty buffers mismatching lengths",
        B.compare<Nat>(buffer, buffer2, Nat.compare),
        M.equals(OrderTestable(#greater)),
      ),
      test(
        "non-empty buffers lexicographic difference",
        B.compare<Nat>(buffer, buffer3, Nat.compare),
        M.equals(OrderTestable(#less)),
      ),
    ],
  )
);

/* --------------------------------------- */

var nestedBuffer = B.initPresized<B.StableBuffer<Nat>>(3);
for (i in Iter.range(0, 4)) {
  let innerBuffer = B.initPresized<Nat>(2);
  for (j in if (i % 2 == 0) { Iter.range(0, 4) } else { Iter.range(0, 3) }) {
    B.add(innerBuffer, j);
  };
  B.add(nestedBuffer, innerBuffer);
};
B.add(nestedBuffer, B.initPresized<Nat>(2));

buffer := B.flatten<Nat>(nestedBuffer);

run(
  suite(
    "flatten",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(45)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [0, 1, 2, 3, 4, 0, 1, 2, 3, 0, 1, 2, 3, 4, 0, 1, 2, 3, 0, 1, 2, 3, 4])),
      ),
    ],
  )
);

/* --------------------------------------- */

nestedBuffer := B.initPresized<B.StableBuffer<Nat>>(3);
for (i in Iter.range(0, 4)) {
  B.add(nestedBuffer, B.initPresized<Nat>(2));
};

buffer := B.flatten<Nat>(nestedBuffer);

run(
  suite(
    "flatten all empty inner buffers",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
    ],
  )
);

/* --------------------------------------- */

nestedBuffer := B.initPresized<B.StableBuffer<Nat>>(3);
buffer := B.flatten<Nat>(nestedBuffer);

run(
  suite(
    "flatten empty outer buffer",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(0)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 7)) {
  B.add(buffer, i);
};

B.clear(buffer2);

for (i in Iter.range(0, 6)) {
  B.add(buffer2, i);
};

B.clear(buffer3);

var buffer4 = B.make<Nat>(3);

B.reverse<Nat>(buffer);
B.reverse<Nat>(buffer2);
B.reverse<Nat>(buffer3);
B.reverse<Nat>(buffer4);

run(
  suite(
    "reverse",
    [
      test(
        "even elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [7, 6, 5, 4, 3, 2, 1, 0])),
      ),
      test(
        "odd elements",
        B.toArray(buffer2),
        M.equals(T.array(T.natTestable, [6, 5, 4, 3, 2, 1, 0])),
      ),
      test(
        "empty",
        B.toArray(buffer3),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
      test(
        "singleton",
        B.toArray(buffer4),
        M.equals(T.array(T.natTestable, [3])),
      ),
    ],
  )
);

/* --------------------------------------- */

B.clear(buffer);
for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

var partition = B.partition<Nat>(buffer, func x = x % 2 == 0);
buffer2 := partition.0;
buffer3 := partition.1;

run(
  suite(
    "partition",
    [
      test(
        "capacity of true buffer",
        B.capacity(buffer2),
        M.equals(T.nat(6)),
      ),
      test(
        "elements of true buffer",
        B.toArray(buffer2),
        M.equals(T.array(T.natTestable, [0, 2, 4])),
      ),
      test(
        "capacity of false buffer",
        B.capacity(buffer3),
        M.equals(T.nat(6)),
      ),
      test(
        "elements of false buffer",
        B.toArray(buffer3),
        M.equals(T.array(T.natTestable, [1, 3, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */

B.clear(buffer);
for (i in Iter.range(0, 3)) {
  B.add(buffer, i);
};

for (i in Iter.range(10, 13)) {
  B.add(buffer, i);
};

B.clear(buffer2);
for (i in Iter.range(2, 5)) {
  B.add(buffer2, i);
};
for (i in Iter.range(13, 15)) {
  B.add(buffer2, i);
};

buffer := B.merge<Nat>(buffer, buffer2, Nat.compare);

run(
  suite(
    "merge",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(23)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [0, 1, 2, 2, 3, 3, 4, 5, 10, 11, 12, 13, 13, 14, 15])),
      ),
    ],
  )
);

/* --------------------------------------- */

B.clear(buffer);
for (i in Iter.range(0, 3)) {
  B.add(buffer, i);
};

B.clear(buffer2);

buffer := B.merge<Nat>(buffer, buffer2, Nat.compare);

run(
  suite(
    "merge with empty",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(6)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [0, 1, 2, 3])),
      ),
    ],
  )
);

/* --------------------------------------- */

B.clear(buffer);
B.clear(buffer2);

buffer := B.merge<Nat>(buffer, buffer2, Nat.compare);

run(
  suite(
    "merge two empty",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(1)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

B.add(buffer, 0);
B.add(buffer, 2);
B.add(buffer, 1);
B.add(buffer, 1);
B.add(buffer, 5);
B.add(buffer, 4);

B.sort(buffer, Nat.compare);

run(
  suite(
    "sort even",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [0, 1, 1, 2, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

B.add(buffer, 0);
B.add(buffer, 2);
B.add(buffer, 1);
B.add(buffer, 1);
B.add(buffer, 5);

B.sort(buffer, Nat.compare);

run(
  suite(
    "sort odd",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [0, 1, 1, 2, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

B.sort(buffer, Nat.compare);

run(
  suite(
    "sort empty",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);
B.add(buffer, 2);

B.sort(buffer, Nat.compare);

run(
  suite(
    "sort singleton",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [2] : [Nat])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

partition := B.split<Nat>(buffer, 2);
buffer2 := partition.0;
buffer3 := partition.1;

run(
  suite(
    "split",
    [
      test(
        "capacity prefix",
        B.capacity(buffer2),
        M.equals(T.nat(3)),
      ),
      test(
        "elements prefix",
        B.toArray(buffer2),
        M.equals(T.array(T.natTestable, [0, 1])),
      ),
      test(
        "capacity suffix",
        B.capacity(buffer3),
        M.equals(T.nat(6)),
      ),
      test(
        "elements suffix",
        B.toArray(buffer3),
        M.equals(T.array(T.natTestable, [2, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

partition := B.split<Nat>(buffer, 0);
buffer2 := partition.0;
buffer3 := partition.1;

run(
  suite(
    "split at index 0",
    [
      test(
        "capacity prefix",
        B.capacity(buffer2),
        M.equals(T.nat(1)),
      ),
      test(
        "elements prefix",
        B.toArray(buffer2),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
      test(
        "capacity suffix",
        B.capacity(buffer3),
        M.equals(T.nat(9)),
      ),
      test(
        "elements suffix",
        B.toArray(buffer3),
        M.equals(T.array(T.natTestable, [0, 1, 2, 3, 4, 5])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

partition := B.split<Nat>(buffer, 6);
buffer2 := partition.0;
buffer3 := partition.1;

run(
  suite(
    "split at last index",
    [
      test(
        "capacity prefix",
        B.capacity(buffer2),
        M.equals(T.nat(9)),
      ),
      test(
        "elements prefix",
        B.toArray(buffer2),
        M.equals(T.array(T.natTestable, [0, 1, 2, 3, 4, 5])),
      ),
      test(
        "capacity suffix",
        B.capacity(buffer3),
        M.equals(T.nat(1)),
      ),
      test(
        "elements suffix",
        B.toArray(buffer3),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);
B.clear(buffer2);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};
for (i in Iter.range(0, 3)) {
  B.add(buffer2, i);
};

var bufferPairs = B.zip<Nat, Nat>(buffer, buffer2);

run(
  suite(
    "zip",
    [
      test(
        "capacity",
        B.capacity(bufferPairs),
        M.equals(T.nat(6)),
      ),
      test(
        "elements",
        B.toArray(bufferPairs),
        M.equals(T.array(T.tuple2Testable(T.natTestable, T.natTestable), [(0, 0), (1, 1), (2, 2), (3, 3)])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);
B.clear(buffer2);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

bufferPairs := B.zip<Nat, Nat>(buffer, buffer2);

run(
  suite(
    "zip empty",
    [
      test(
        "capacity",
        B.capacity(bufferPairs),
        M.equals(T.nat(1)),
      ),
      test(
        "elements",
        B.toArray(bufferPairs),
        M.equals(T.array(T.tuple2Testable(T.natTestable, T.natTestable), [] : [(Nat, Nat)])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);
B.clear(buffer2);

bufferPairs := B.zip<Nat, Nat>(buffer, buffer2);

run(
  suite(
    "zip both empty",
    [
      test(
        "capacity",
        B.capacity(bufferPairs),
        M.equals(T.nat(1)),
      ),
      test(
        "elements",
        B.toArray(bufferPairs),
        M.equals(T.array(T.tuple2Testable(T.natTestable, T.natTestable), [] : [(Nat, Nat)])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);
B.clear(buffer2);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};
for (i in Iter.range(0, 3)) {
  B.add(buffer2, i);
};

buffer3 := B.zipWith<Nat, Nat, Nat>(buffer, buffer2, Nat.add);

run(
  suite(
    "zipWith",
    [
      test(
        "capacity",
        B.capacity(buffer3),
        M.equals(T.nat(6)),
      ),
      test(
        "elements",
        B.toArray(buffer3),
        M.equals(T.array(T.natTestable, [0, 2, 4, 6])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);
B.clear(buffer2);

for (i in Iter.range(0, 5)) {
  B.add(buffer, i);
};

buffer3 := B.zipWith<Nat, Nat, Nat>(buffer, buffer2, Nat.add);

run(
  suite(
    "zipWithEmpty",
    [
      test(
        "capacity",
        B.capacity(buffer3),
        M.equals(T.nat(1)),
      ),
      test(
        "elements",
        B.toArray(buffer3),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 8)) {
  B.add(buffer, i);
};

var chunks = B.chunk<Nat>(buffer, 2);

run(
  suite(
    "chunk",
    [
      test(
        "num chunks",
        B.size(chunks),
        M.equals(T.nat(5)),
      ),
      test(
        "chunk 0 capacity",
        B.capacity(B.get(chunks, 0)),
        M.equals(T.nat(3)),
      ),
      test(
        "chunk 0 elements",
        B.toArray(B.get(chunks, 0)),
        M.equals(T.array(T.natTestable, [0, 1])),
      ),
      test(
        "chunk 2 capacity",
        B.capacity(B.get(chunks, 2)),
        M.equals(T.nat(3)),
      ),
      test(
        "chunk 2 elements",
        B.toArray(B.get(chunks, 2)),
        M.equals(T.array(T.natTestable, [4, 5])),
      ),
      test(
        "chunk 4 capacity",
        B.capacity(B.get(chunks, 4)),
        M.equals(T.nat(3)),
      ),
      test(
        "chunk 4 elements",
        B.toArray(B.get(chunks, 4)),
        M.equals(T.array(T.natTestable, [8])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

chunks := B.chunk<Nat>(buffer, 3);

run(
  suite(
    "chunk empty",
    [
      test(
        "num chunks",
        B.size(chunks),
        M.equals(T.nat(0)),
      )
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

chunks := B.chunk<Nat>(buffer, 10);

run(
  suite(
    "chunk larger than buffer",
    [
      test(
        "num chunks",
        B.size(chunks),
        M.equals(T.nat(1)),
      ),
      test(
        "chunk 0 elements",
        B.toArray(B.get(chunks, 0)),
        M.equals(T.array(T.natTestable, [0, 1, 2, 3, 4])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

B.add(buffer, 2);
B.add(buffer, 2);
B.add(buffer, 2);
B.add(buffer, 1);
B.add(buffer, 0);
B.add(buffer, 0);
B.add(buffer, 2);
B.add(buffer, 1);
B.add(buffer, 1);

var groups = B.groupBy<Nat>(buffer, Nat.equal);

run(
  suite(
    "groupBy",
    [
      test(
        "num groups",
        B.size(groups),
        M.equals(T.nat(5)),
      ),
      test(
        "group 0 capacity",
        B.capacity(B.get(groups, 0)),
        M.equals(T.nat(9)),
      ),
      test(
        "group 0 elements",
        B.toArray(B.get(groups, 0)),
        M.equals(T.array(T.natTestable, [2, 2, 2])),
      ),
      test(
        "group 1 capacity",
        B.capacity(B.get(groups, 1)),
        M.equals(T.nat(6)),
      ),
      test(
        "group 1 elements",
        B.toArray(B.get(groups, 1)),
        M.equals(T.array(T.natTestable, [1])),
      ),
      test(
        "group 4 capacity",
        B.capacity(B.get(groups, 4)),
        M.equals(T.nat(2)),
      ),
      test(
        "group 4 elements",
        B.toArray(B.get(groups, 4)),
        M.equals(T.array(T.natTestable, [1, 1])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

groups := B.groupBy<Nat>(buffer, Nat.equal);

run(
  suite(
    "groupBy clear",
    [
      test(
        "num groups",
        B.size(groups),
        M.equals(T.nat(0)),
      )
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, 0);
};

groups := B.groupBy<Nat>(buffer, Nat.equal);

run(
  suite(
    "groupBy clear",
    [
      test(
        "num groups",
        B.size(groups),
        M.equals(T.nat(1)),
      ),
      test(
        "group 0 elements",
        B.toArray(B.get(groups, 0)),
        M.equals(T.array(T.natTestable, [0, 0, 0, 0, 0])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

buffer := B.prefix<Nat>(buffer, 3);

run(
  suite(
    "prefix",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(5)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [0, 1, 2])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

buffer := B.prefix<Nat>(buffer, 0);

run(
  suite(
    "prefix of empty",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(1)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

buffer := B.prefix<Nat>(buffer, 5);

run(
  suite(
    "trivial prefix",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(8)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [0, 1, 2, 3, 4])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

B.clear(buffer2);

for (i in Iter.range(0, 2)) {
  B.add(buffer2, i);
};

B.clear(buffer3);

B.add(buffer3, 2);
B.add(buffer3, 1);
B.add(buffer3, 0);

run(
  suite(
    "isPrefixOf",
    [
      test(
        "normal prefix",
        B.isPrefixOf<Nat>(buffer2, buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "identical buffers",
        B.isPrefixOf<Nat>(buffer, buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "one empty buffer",
        B.isPrefixOf<Nat>(B.initPresized<Nat>(3), buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "not prefix",
        B.isPrefixOf<Nat>(buffer3, buffer, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "not prefix from length",
        B.isPrefixOf<Nat>(buffer, buffer2, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "not prefix of empty",
        B.isPrefixOf<Nat>(buffer, B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "empty prefix of empty",
        B.isPrefixOf<Nat>(B.initPresized<Nat>(4), B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.bool(true)),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

B.clear(buffer2);

for (i in Iter.range(0, 2)) {
  B.add(buffer2, i);
};

B.clear(buffer3);

B.add(buffer3, 2);
B.add(buffer3, 1);
B.add(buffer3, 0);

run(
  suite(
    "isStrictPrefixOf",
    [
      test(
        "normal prefix",
        B.isStrictPrefixOf<Nat>(buffer2, buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "identical buffers",
        B.isStrictPrefixOf<Nat>(buffer, buffer, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "one empty buffer",
        B.isStrictPrefixOf<Nat>(B.initPresized<Nat>(3), buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "not prefix",
        B.isStrictPrefixOf<Nat>(buffer3, buffer, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "not prefix from length",
        B.isStrictPrefixOf<Nat>(buffer, buffer2, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "not prefix of empty",
        B.isStrictPrefixOf<Nat>(buffer, B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "empty prefix of empty",
        B.isStrictPrefixOf<Nat>(B.initPresized<Nat>(4), B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.bool(false)),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

buffer := B.subBuffer<Nat>(buffer, 1, 3);

run(
  suite(
    "subBuffer",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(5)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [1, 2, 3])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

run(
  suite(
    "subBuffer edge cases",
    [
      test(
        "prefix",
        B.prefix(buffer, 3),
        M.equals({ { item = B.subBuffer(buffer, 0, 3) } and NatBufferTestable }),
      ),
      test(
        "suffix",
        B.suffix(buffer, 3),
        M.equals({ { item = B.subBuffer(buffer, 2, 3) } and NatBufferTestable }),
      ),
      test(
        "empty",
        B.toArray(B.subBuffer(buffer, 2, 0)),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
      test(
        "trivial",
        B.subBuffer(buffer, 0, B.size(buffer)),
        M.equals({ { item = buffer } and NatBufferTestable }),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

B.clear(buffer2);

for (i in Iter.range(0, 2)) {
  B.add(buffer2, i);
};

B.clear(buffer3);

for (i in Iter.range(1, 3)) {
  B.add(buffer3, i);
};

run(
  suite(
    "isSubBufferOf",
    [
      test(
        "normal subBuffer",
        B.isSubBufferOf<Nat>(buffer3, buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "prefix",
        B.isSubBufferOf<Nat>(buffer2, buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "identical buffers",
        B.isSubBufferOf<Nat>(buffer, buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "one empty buffer",
        B.isSubBufferOf<Nat>(B.initPresized<Nat>(3), buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "not subBuffer",
        B.isSubBufferOf<Nat>(buffer3, buffer2, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "not subBuffer from length",
        B.isSubBufferOf<Nat>(buffer, buffer2, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "not subBuffer of empty",
        B.isSubBufferOf<Nat>(buffer, B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "empty subBuffer of empty",
        B.isSubBufferOf<Nat>(B.initPresized<Nat>(4), B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.bool(true)),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

B.clear(buffer2);

for (i in Iter.range(0, 2)) {
  B.add(buffer2, i);
};

B.clear(buffer3);

for (i in Iter.range(1, 3)) {
  B.add(buffer3, i);
};

buffer4 := B.initPresized<Nat>(4);

for (i in Iter.range(3, 4)) {
  B.add(buffer4, i);
};

run(
  suite(
    "isStrictSubBufferOf",
    [
      test(
        "normal strict subBuffer",
        B.isStrictSubBufferOf<Nat>(buffer3, buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "prefix",
        B.isStrictSubBufferOf<Nat>(buffer2, buffer, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "suffix",
        B.isStrictSubBufferOf<Nat>(buffer4, buffer, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "identical buffers",
        B.isStrictSubBufferOf<Nat>(buffer, buffer, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "one empty buffer",
        B.isStrictSubBufferOf<Nat>(B.initPresized<Nat>(3), buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "not subBuffer",
        B.isStrictSubBufferOf<Nat>(buffer3, buffer2, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "not subBuffer from length",
        B.isStrictSubBufferOf<Nat>(buffer, buffer2, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "not subBuffer of empty",
        B.isStrictSubBufferOf<Nat>(buffer, B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "empty not strict subBuffer of empty",
        B.isStrictSubBufferOf<Nat>(B.initPresized<Nat>(4), B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.bool(false)),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

buffer := B.suffix<Nat>(buffer, 3);

run(
  suite(
    "suffix",
    [
      test(
        "capacity",
        B.capacity(buffer),
        M.equals(T.nat(5)),
      ),
      test(
        "elements",
        B.toArray(buffer),
        M.equals(T.array(T.natTestable, [2, 3, 4])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

run(
  suite(
    "suffix edge cases",
    [
      test(
        "empty",
        B.toArray(B.prefix(buffer, 0)),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
      test(
        "trivial",
        B.prefix(buffer, B.size(buffer)),
        M.equals({ { item = buffer } and NatBufferTestable }),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

B.clear(buffer2);
for (i in Iter.range(3, 4)) {
  B.add(buffer2, i);
};

B.clear(buffer3);

B.add(buffer3, 2);
B.add(buffer3, 1);
B.add(buffer3, 0);

run(
  suite(
    "isSuffixOf",
    [
      test(
        "normal suffix",
        B.isSuffixOf<Nat>(buffer2, buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "identical buffers",
        B.isSuffixOf<Nat>(buffer, buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "one empty buffer",
        B.isSuffixOf<Nat>(B.initPresized<Nat>(3), buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "not suffix",
        B.isSuffixOf<Nat>(buffer3, buffer, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "not suffix from length",
        B.isSuffixOf<Nat>(buffer, buffer2, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "not suffix of empty",
        B.isSuffixOf<Nat>(buffer, B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "empty suffix of empty",
        B.isSuffixOf<Nat>(B.initPresized<Nat>(4), B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.bool(true)),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

B.clear(buffer2);
for (i in Iter.range(3, 4)) {
  B.add(buffer2, i);
};

B.clear(buffer3);

B.add(buffer3, 2);
B.add(buffer3, 1);
B.add(buffer3, 0);

run(
  suite(
    "isStrictSuffixOf",
    [
      test(
        "normal suffix",
        B.isStrictSuffixOf<Nat>(buffer2, buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "identical buffers",
        B.isStrictSuffixOf<Nat>(buffer, buffer, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "one empty buffer",
        B.isStrictSuffixOf<Nat>(B.initPresized<Nat>(3), buffer, Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "not suffix",
        B.isStrictSuffixOf<Nat>(buffer3, buffer, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "not suffix from length",
        B.isStrictSuffixOf<Nat>(buffer, buffer2, Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "not suffix of empty",
        B.isStrictSuffixOf<Nat>(buffer, B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "empty suffix of empty",
        B.isStrictSuffixOf<Nat>(B.initPresized<Nat>(4), B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.bool(false)),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

run(
  suite(
    "takeWhile",
    [
      test(
        "normal case",
        B.toArray(B.takeWhile<Nat>(buffer, func x = x < 3)),
        M.equals(T.array(T.natTestable, [0, 1, 2])),
      ),
      test(
        "empty",
        B.toArray(B.takeWhile<Nat>(B.initPresized<Nat>(3), func x = x < 3)),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 4)) {
  B.add(buffer, i);
};

run(
  suite(
    "dropWhile",
    [
      test(
        "normal case",
        B.toArray(B.dropWhile<Nat>(buffer, func x = x < 3)),
        M.equals(T.array(T.natTestable, [3, 4])),
      ),
      test(
        "empty",
        B.toArray(B.dropWhile<Nat>(B.initPresized<Nat>(3), func x = x < 3)),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
      test(
        "drop all",
        B.toArray(B.dropWhile<Nat>(buffer, func _ = true)),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
      test(
        "drop none",
        B.toArray(B.dropWhile<Nat>(buffer, func _ = false)),
        M.equals(T.array(T.natTestable, [0, 1, 2, 3, 4])),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(1, 6)) {
  B.add(buffer, i);
};

run(
  suite(
    "binarySearch",
    [
      test(
        "find in middle",
        B.binarySearch<Nat>(2, buffer, Nat.compare),
        M.equals(T.optional(T.natTestable, ?1)),
      ),
      test(
        "find first",
        B.binarySearch<Nat>(1, buffer, Nat.compare),
        M.equals(T.optional(T.natTestable, ?0)),
      ),
      test(
        "find last",
        B.binarySearch<Nat>(6, buffer, Nat.compare),
        M.equals(T.optional(T.natTestable, ?5)),
      ),
      test(
        "not found to the right",
        B.binarySearch<Nat>(10, buffer, Nat.compare),
        M.equals(T.optional(T.natTestable, null : ?Nat)),
      ),
      test(
        "not found to the left",
        B.binarySearch<Nat>(0, buffer, Nat.compare),
        M.equals(T.optional(T.natTestable, null : ?Nat)),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

for (i in Iter.range(0, 6)) {
  B.add(buffer, i);
};

run(
  suite(
    "indexOf",
    [
      test(
        "find in middle",
        B.indexOf<Nat>(2, buffer, Nat.equal),
        M.equals(T.optional(T.natTestable, ?2)),
      ),
      test(
        "find first",
        B.indexOf<Nat>(0, buffer, Nat.equal),
        M.equals(T.optional(T.natTestable, ?0)),
      ),
      test(
        "find last",
        B.indexOf<Nat>(6, buffer, Nat.equal),
        M.equals(T.optional(T.natTestable, ?6)),
      ),
      test(
        "not found",
        B.indexOf<Nat>(10, buffer, Nat.equal),
        M.equals(T.optional(T.natTestable, null : ?Nat)),
      ),
      test(
        "empty",
        B.indexOf<Nat>(100, B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.optional(T.natTestable, null : ?Nat)),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

B.add(buffer, 2); // 0
B.add(buffer, 2); // 1
B.add(buffer, 1); // 2
B.add(buffer, 10); // 3
B.add(buffer, 1); // 4
B.add(buffer, 0); // 5
B.add(buffer, 10); // 6
B.add(buffer, 3); // 7
B.add(buffer, 0); // 8

run(
  suite(
    "lastIndexOf",
    [
      test(
        "find in middle",
        B.lastIndexOf<Nat>(10, buffer, Nat.equal),
        M.equals(T.optional(T.natTestable, ?6)),
      ),
      test(
        "find only",
        B.lastIndexOf<Nat>(3, buffer, Nat.equal),
        M.equals(T.optional(T.natTestable, ?7)),
      ),
      test(
        "find last",
        B.lastIndexOf<Nat>(0, buffer, Nat.equal),
        M.equals(T.optional(T.natTestable, ?8)),
      ),
      test(
        "not found",
        B.lastIndexOf<Nat>(100, buffer, Nat.equal),
        M.equals(T.optional(T.natTestable, null : ?Nat)),
      ),
      test(
        "empty",
        B.lastIndexOf<Nat>(100, B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.optional(T.natTestable, null : ?Nat)),
      ),
    ],
  )
);

/* --------------------------------------- */
B.clear(buffer);

B.add(buffer, 2); // 0
B.add(buffer, 2); // 1
B.add(buffer, 1); // 2
B.add(buffer, 10); // 3
B.add(buffer, 1); // 4
B.add(buffer, 10); // 5
B.add(buffer, 3); // 6
B.add(buffer, 0); // 7

run(
  suite(
    "indexOfBuffer",
    [
      test(
        "find in middle",
        B.indexOfBuffer<Nat>(B.fromArray<Nat>([1, 10, 1]), buffer, Nat.equal),
        M.equals(T.optional(T.natTestable, ?2)),
      ),
      test(
        "find first",
        B.indexOfBuffer<Nat>(B.fromArray<Nat>([2, 2, 1, 10]), buffer, Nat.equal),
        M.equals(T.optional(T.natTestable, ?0)),
      ),
      test(
        "find last",
        B.indexOfBuffer<Nat>(B.fromArray<Nat>([0]), buffer, Nat.equal),
        M.equals(T.optional(T.natTestable, ?7)),
      ),
      test(
        "not found",
        B.indexOfBuffer<Nat>(B.fromArray<Nat>([99, 100, 1]), buffer, Nat.equal),
        M.equals(T.optional(T.natTestable, null : ?Nat)),
      ),
      test(
        "search for empty buffer",
        B.indexOfBuffer<Nat>(B.fromArray<Nat>([]), buffer, Nat.equal),
        M.equals(T.optional(T.natTestable, null : ?Nat)),
      ),
      test(
        "search through empty buffer",
        B.indexOfBuffer<Nat>(B.fromArray<Nat>([1, 2, 3]), B.initPresized<Nat>(2), Nat.equal),
        M.equals(T.optional(T.natTestable, null : ?Nat)),
      ),
      test(
        "search for empty in empty",
        B.indexOfBuffer<Nat>(B.initPresized<Nat>(2), B.initPresized<Nat>(3), Nat.equal),
        M.equals(T.optional(T.natTestable, null : ?Nat)),
      ),
    ],
  )
);
