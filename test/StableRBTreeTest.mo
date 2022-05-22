import Debug "mo:base/Debug";
import Nat "mo:base/Nat";
import I "mo:base/Iter";
import List "mo:base/List";
import RBT "../src/StableRBTree";
import Text "mo:base/Text";
import Option "mo:base/Option";

import M "mo:matchers/Matchers";
import S "mo:matchers/Suite";
import T "mo:matchers/Testable";

let sorted =
  [
    (1, "reformer"),
    (2, "helper"),
    (3, "achiever"),
    (4, "individualist"),
    (5, "investigator"),
    (6, "loyalist"),
    (7, "enthusiast"),
    (8, "challenger"),
    (9, "peacemaker"),
  ];

let unsort =
  [
    (6, "loyalist"),
    (3, "achiever"),
    (9, "peacemaker"),
    (1, "reformer"),
    (4, "individualist"),
    (2, "helper"),
    (8, "challenger"),
    (5, "investigator"),
    (7, "enthusiast"),
  ];

// var t = RBT.init<Nat, Text>(Nat.compare);
var t = RBT.init<Nat, Text>();

assert RBT.size(RBT.share(t)) == 0;

for ((num, lab) in unsort.vals()) {
  Debug.print (Nat.toText num);
  Debug.print lab;
  t := RBT.put<Nat, Text>(t, Nat.compare, num, lab);
};

do { var i = 1;
for ((num, lab) in RBT.entries(t)) {
  assert(num == i);
 i += 1;
}};

assert RBT.size(RBT.share(t)) == 9;

do { var i = 9;
for ((num, lab) in RBT.entriesRev(t)) {
  assert(num == i);
  i -= 1;
}};

assert RBT.size(RBT.share(t)) == 9;

t := RBT.delete<Nat, Text>(t, Nat.compare, 5);

assert RBT.size(RBT.share(t)) == 8;

// Test equalIncludeDeleted and equalIgnoreDeleted
var t1 = RBT.init<Nat, Text>();
var t2 = RBT.init<Nat, Text>();
assert RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);
assert RBT.equalIgnoreDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);

t1 := RBT.put<Nat, Text>(t1, Nat.compare, 5, "hello");
assert not RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);
assert not RBT.equalIgnoreDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);

t2 := RBT.put<Nat, Text>(t2, Nat.compare, 5, "hello");
assert RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);
assert RBT.equalIgnoreDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);

for ((num, lab) in unsort.vals()) {
  Debug.print (Nat.toText num);
  Debug.print lab;
  t1 := RBT.put<Nat, Text>(t1, Nat.compare, num, lab);
  t2 := RBT.put<Nat, Text>(t2, Nat.compare, num, lab);
};
assert RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);
assert RBT.equalIgnoreDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);

t2 := RBT.put<Nat, Text>(t2, Nat.compare, 1, "reformer*s*");
assert not RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);
assert not RBT.equalIgnoreDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);

// Test equalIgnoreDeleted versus equalIncludeDeleted, specifically the case where a 
// value has been deleted from one tree, making it equal to the other tree
t1 := RBT.init<Nat, Text>();
t2 := RBT.init<Nat, Text>();
t1 := RBT.put<Nat, Text>(t1, Nat.compare, 35, "john");
t2 := RBT.put<Nat, Text>(t1, Nat.compare, 35, "john");
assert RBT.equalIgnoreDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);
assert RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);

t1 := RBT.put<Nat, Text>(t1, Nat.compare, 31, "alice");
t1 := RBT.delete<Nat, Text>(t1, Nat.compare, 31);
assert RBT.equalIgnoreDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);
assert not RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal);

// Test split
var result = RBT.split<Nat, Text>(#leaf, Nat.compare);
switch(result) {
  case null { assert false };
  case (?(l, r)) {
    assert RBT.size(l) == 0;
    assert RBT.size(r) == 0;
  }
};
// test single node at root
var rootT = RBT.init<Nat, Text>();
rootT := RBT.put<Nat, Text>(rootT, Nat.compare, 10, "a");
result := RBT.split<Nat, Text>(rootT, Nat.compare);
switch(result) {
  case null { assert false };
  case (?(l, r)) {
    assert RBT.size(l) == 1;
    assert RBT.size(r) == 0;
    let lVal = Option.get(RBT.get<Nat, Text>(l, Nat.compare, 10), "null");
    assert lVal == "a";
  }
};
// test if delete root (now empty tree)
rootT := RBT.delete<Nat, Text>(rootT, Nat.compare, 10);
result := RBT.split<Nat, Text>(rootT, Nat.compare);
switch(result) {
  case null { assert false };
  case (?(l, r)) {
    assert l == #leaf;
    assert r == #leaf;
  }
};

// test root node with left child, but no right child
rootT := RBT.init<Nat, Text>();
rootT := RBT.put<Nat, Text>(rootT, Nat.compare, 10, "a");
rootT := RBT.put<Nat, Text>(rootT, Nat.compare, 5, "b");
result := RBT.split<Nat, Text>(rootT, Nat.compare);
switch(result) {
  case null { assert false };
  case (?(l, r)) {
    assert RBT.size(l) == 1;
    assert RBT.size(r) == 1;
    let lVal = Option.get(RBT.get<Nat, Text>(l, Nat.compare, 5), "null");
    assert lVal == "b";
    let rVal = Option.get(RBT.get<Nat, Text>(r, Nat.compare, 10), "null");
    assert rVal == "a";
  }
};
// delete root, so test with just left child
rootT := RBT.delete<Nat, Text>(rootT, Nat.compare, 10);
result := RBT.split<Nat, Text>(rootT, Nat.compare);
switch(result) {
  case null { assert false };
  case (?(l, r)) {
    // check 10 does not exist in left or right after delete
    var lVal = Option.get(RBT.get<Nat, Text>(l, Nat.compare, 10), "null");
    var rVal = Option.get(RBT.get<Nat, Text>(r, Nat.compare, 10), "null");
    assert lVal == "null";
    assert rVal == "null";
    // assert left tree is 5, and right is a leaf
    assert RBT.size(l) == 1;
    lVal := Option.get(RBT.get<Nat, Text>(l, Nat.compare, 5), "null");
    assert lVal == "b";
    assert r == #leaf;
  }
};
// test root node with right child, but no left child
rootT := RBT.init<Nat, Text>();
rootT := RBT.put<Nat, Text>(rootT, Nat.compare, 10, "a");
rootT := RBT.put<Nat, Text>(rootT, Nat.compare, 15, "c");
result := RBT.split<Nat, Text>(rootT, Nat.compare);
switch(result) {
  case null { assert false };
  case (?(l, r)) {
    assert RBT.size(l) == 1;
    assert RBT.size(r) == 1;
    let lVal = Option.get(RBT.get<Nat, Text>(l, Nat.compare, 10), "null");
    assert lVal == "a";
    let rVal = Option.get(RBT.get<Nat, Text>(r, Nat.compare, 15), "null");
    assert rVal == "c";
  }
};
// delete root, so test with just right child
rootT := RBT.delete<Nat, Text>(rootT, Nat.compare, 10);
result := RBT.split<Nat, Text>(rootT, Nat.compare);
switch(result) {
  case null { assert false };
  case (?(l, r)) {
    // check 10 does not exist in left or right after delete
    var lVal = Option.get(RBT.get<Nat, Text>(l, Nat.compare, 10), "null");
    var rVal = Option.get(RBT.get<Nat, Text>(r, Nat.compare, 10), "null");
    assert lVal == "null";
    assert rVal == "null";
    // assert left tree returned is 15, and right is a leaf
    assert RBT.size(l) == 1;
    lVal := Option.get(RBT.get<Nat, Text>(l, Nat.compare, 15), "null");
    assert lVal == "c";
    assert r == #leaf;
  }
};
// test root with both right and left nodes
rootT := RBT.init<Nat, Text>();
rootT := RBT.put<Nat, Text>(rootT, Nat.compare, 10, "a");
rootT := RBT.put<Nat, Text>(rootT, Nat.compare, 5, "b");
rootT := RBT.put<Nat, Text>(rootT, Nat.compare, 15, "c");
result := RBT.split<Nat, Text>(rootT, Nat.compare);
switch(result) {
  case null { assert false };
  case (?(l, r)) {
    // check 10 & 5 are in the left tree and 15 is in the right tree
    assert RBT.size(l) == 2;
    assert RBT.size(r) == 1;
    var lVal = Option.get(RBT.get<Nat, Text>(l, Nat.compare, 10), "null");
    assert lVal == "a";
    lVal := Option.get(RBT.get<Nat, Text>(l, Nat.compare, 5), "null");
    assert lVal == "b";
    let rVal = Option.get(RBT.get<Nat, Text>(r, Nat.compare, 15), "null");
    assert rVal == "c";
    // check that the root of the left tree is 5 (10 was inserted into it)
    switch(l) {
      case (#leaf) { assert false };
      case (#node(_, _, (k, v), _)) {
        assert k == 5;
        assert Option.get(v, "null") == "b";
      }
    }
  }
};
// delete root, test with left and right child
rootT := RBT.delete<Nat, Text>(rootT, Nat.compare, 10);
result := RBT.split<Nat, Text>(rootT, Nat.compare);
switch(result) {
  case null { assert false };
  case (?(l, r)) {
    // check 10 does not exist in left or right after delete
    var lVal = Option.get(RBT.get<Nat, Text>(l, Nat.compare, 10), "null");
    var rVal = Option.get(RBT.get<Nat, Text>(r, Nat.compare, 10), "null");
    assert lVal == "null";
    assert rVal == "null";
    // assert left tree returned is 5, and right is 15
    assert RBT.size(l) == 1;
    assert RBT.size(r) == 1;
    lVal := Option.get(RBT.get<Nat, Text>(l, Nat.compare, 5), "null");
    assert lVal == "b";
    rVal := Option.get(RBT.get<Nat, Text>(r, Nat.compare, 15), "null");
    assert rVal == "c";
  }
};

/* Should trap and throw an error if uncommented - leave commented for passing CI
// If passed an invalid RBTree, will throw an error and trap
let invalidRBTree = #node(
  #B,
  #node(
    #R, 
    #leaf, 
    (5, ?"b"), 
    #node(#B, #leaf, (10, ?"a"), #leaf)
  ),
  (15, ?"c"),
  #leaf
);
result := RBT.split<Nat, Text>(invalidRBTree, Nat.compare);
*/


let { run;test;suite; } = S;

func incrementFunc(v: ?Nat): Nat {
  switch(v) {
    case null { 1 };
    case (?v) { v + 1 };
  }
};

func testableUpdateRBTreeResult<K, V>(
  res: (?V, RBT.Tree<K, V>),
  keyEquals: (K, K) -> Bool,
  valueEquals: (V, V) -> Bool,
): T.TestableItem<(?V, RBT.Tree<K, V>)> = {
  display = func((ov: ?V, t: RBT.Tree<K, V>)): Text { "placeholder" }; // { "ov=" # debug_show(ov) # ", tree=" # debug_show(t) };
  equals = func(
    (ov1: ?V, t1: RBT.Tree<K, V>),
    (ov2: ?V, t2: RBT.Tree<K, V>)
  ): Bool {
    switch(ov1, ov2) {
      case (null, null) { RBT.equalIgnoreDeleted<K, V>(t1, t2, keyEquals, valueEquals) };
      case (?ov1, ?ov2) {
        valueEquals(ov1, ov2) and RBT.equalIgnoreDeleted<K, V>(t1, t2, keyEquals, valueEquals)
      };
      case _ { false }
    }
  };
  item = res;
};

let updateSuite = suite("update",
  [
    test("applies the increment function correctly to create the expected value (1) for the key if the tree is empty, returning null and the new tree",
      RBT.update<Text, Nat>(RBT.init<Text, Nat>(), Text.compare, "apples", incrementFunc),
      M.equals(
        testableUpdateRBTreeResult<Text, Nat>(
          (null, RBT.put<Text, Nat>(RBT.init<Text, Nat>(), Text.compare, "apples", 1)), 
          Text.equal, 
          Nat.equal
        )
      )
    ),
    test("applies the increment function correctly to create the expected value (1) for the key if the tree is not empty but does not contain the key, returning null and the new tree",
      do {
        let tree = RBT.put<Text, Nat>(RBT.init<Text, Nat>(), Text.compare, "oranges", 5);
        RBT.update<Text, Nat>(tree, Text.compare, "apples", incrementFunc);
      },
      M.equals(
        testableUpdateRBTreeResult<Text, Nat>(
          (null, 
            do {
              let expected = RBT.put<Text, Nat>(RBT.init<Text, Nat>(), Text.compare, "oranges", 5);
              RBT.put<Text, Nat>(expected, Text.compare, "apples", 1)
            }
          ), 
          Text.equal, 
          Nat.equal
        )
      )
    ),
    test("applies the increment function correctly to create the expected value (4) for the key if the tree contains the key, returning the original value and the new tree",
      do {
        var tree = RBT.put<Text, Nat>(RBT.init<Text, Nat>(), Text.compare, "oranges", 5);
        tree := RBT.put<Text, Nat>(tree, Text.compare, "apples", 9);
        RBT.update<Text, Nat>(tree, Text.compare, "apples", incrementFunc);
      },
      M.equals(
        testableUpdateRBTreeResult<Text, Nat>(
          (?9, 
            do {
              let expected = RBT.put<Text, Nat>(RBT.init<Text, Nat>(), Text.compare, "oranges", 5);
              RBT.put<Text, Nat>(expected, Text.compare, "apples", 10)
            }
          ), 
          Text.equal, 
          Nat.equal
        )
      )
    )
  ],
);

run(suite("StableRBTree", 
  [
    updateSuite
  ]
));