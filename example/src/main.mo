import Buffer "../../src/StableBuffer";
import Array "mo:base/Array";

actor {
  stable let myBuffer = Buffer.init<Nat>();

  public func addNatToBuffer(nat: Nat) {
    Buffer.add<Nat>(myBuffer, nat);
  };

  public query func printBuffer(): async [Nat] {
    Array.tabulate<Nat>(myBuffer.count, func(i) { myBuffer.elems[i] });
  };

  public query func getElementAtIndex(index: Nat): async Nat {
    Buffer.get<Nat>(myBuffer, index);
  }
}


// After the breaking change, if I want to keep the same API interface becomes 

/*
actor {
  stable let myBuffer = Buffer.init<Nat>();

  public func addNatToBuffer(nat: Nat) {
    Buffer.add<Nat>(myBuffer, nat);
  };

  public query func printBuffer(): async [Nat] {
    Array.tabulate<Nat>(myBuffer.count, func(i) { 
      switch(myBuffer.elems[i]) {
        case (?el) { el };
        case null { assert false; 0 };
      }
    });
  };

  public query func getElementAtIndex(index: Nat): async Nat {
    Buffer.get<Nat>(myBuffer, index)
  }
}
*/