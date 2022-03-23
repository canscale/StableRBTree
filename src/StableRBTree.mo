/// Red-Black Trees

import Debug "mo:base/Debug";
import I "mo:base/Iter";
import List "mo:base/List";
import Nat "mo:base/Nat";
import O "mo:base/Order";

module {

  /// Node color: red or black.
  public type Color = { #R; #B };

  /// Ordered, (red-black) tree of entries.
  public type Tree<K, V> = {
    #node : (Color, Tree<K, V>, (K, ?V), Tree<K, V>);
    #leaf;
  };

  /// Initializes an empty Red-Black Tree of type <K, V>
  /// Returns this empty Red-Black Tree
  public func init<K, V>(): Tree<K, V> {
    (#leaf : Tree<K, V>);
  };

  /// Tree as sharable data.
  ///
  /// Get non-OO, purely-functional representation:
  /// for drawing, pretty-printing and non-OO contexts
  /// (e.g., async args and results):
  public func share<K,V>(tree: Tree<K, V>) : Tree<K, V> {
    tree
  };

  /// Returns the value associated with a given key.
  public func get<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order, k : K) : ?V {
    getRec(k, compareTo, tree);
  };

  /// Replace the value associated with a given key.
  /// Returns the replaced value (if exists) and the new tree
  public func replace<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order, k : K, v : V) : (?V, Tree<K, V>) {
    insertRoot(k, compareTo, v, tree);
  };

  /// Put an entry: A value associated with a given key.
  /// Returns the new tree
  public func put<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order, k : K, v : V): Tree<K,V> {
    let (res, t) = insertRoot(k, compareTo, v, tree);
    t
  };

  /// Delete the entry associated with a given key.
  /// Returns the new tree
  public func delete<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order, k : K): Tree<K, V> {
    let (res, t) = removeRec(k, compareTo, tree);
    t
  };

  /// Remove the entry associated with a given key.
  /// Returns the removed entry (if exists) and the new tree
  public func remove<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order, k : K) : (?V, Tree<K,V>) {
    removeRec(k, compareTo, tree);
  };

  /// An iterator for the key-value entries of the map, in ascending key order.
  ///
  /// iterator is persistent, like the tree itself
  public func entries<K, V>(tree: Tree<K, V>) : I.Iter<(K, V)> { iter(tree, #fwd) };

  /// An iterator for the key-value entries of the map, in descending key order.
  ///
  /// iterator is persistent, like the tree itself
  public func entriesRev<K, V>(tree: Tree<K, V>) : I.Iter<(K, V)> { iter(tree, #bwd) };



  type IterRep<K, V> = List.List<{ #tr:Tree<K, V>; #kv:(K, ?V) }>;

  /// An iterator for the entries of the map, in ascending (`#fwd`) or descending (`#bwd`) order.
  public func iter<K, V>(t : Tree<K, V>, dir : { #fwd; #bwd }) : I.Iter<(K, V)> {
    object {
      var trees : IterRep<K, V> = ?(#tr(t), null);
      public func next() : ?(K, V) {
        switch (dir, trees) {
          case (_, null) { null };
          case (_, ?(#tr(#leaf), ts)){
            trees := ts;
            next()
          };
          case (_, ?(#kv(kv), ts)) {
            trees := ts;
            switch (kv.1) {
              case null { next() };
              case (?v) { ?(kv.0, v) }
            }
          };
          case (#fwd, ?(#tr(#node(_, l, kv, r)), ts)) {
            trees := ?(#tr(l), ?(#kv(kv), ?(#tr(r), ts)));
            next()
          };
          case (#bwd, ?(#tr(#node(_, l, kv, r)), ts)) {
            trees := ?(#tr(r), ?(#kv(kv), ?(#tr(l), ts)));
            next()
          };
        }
      };
    }
  };

  /// Remove the value associated with a given key.
  func removeRec<K, V>(k : K, compareTo : (K, K) -> O.Order, t : Tree<K, V>)
    : (?V, Tree<K, V>) {
    switch t {
      case (#leaf) { (null, #leaf) };
      case (#node(c, l, kv, r)) {
        switch (compareTo(k, kv.0)) {
          case (#less) {
            let (vo, l2) = removeRec(k, compareTo, l);
            (vo, #node(c, l2, kv, r))
          };
          case (#equal) {
            (kv.1, #node(c, l, (k, null), r))
          };
          case (#greater) {
            let (vo, r2) = removeRec(k, compareTo, r);
            (vo, #node(c, l, kv, r2))
          };
        }
      }
    }
  };



  func bal<K, V>(color : Color, lt : Tree<K, V>, kv : (K, ?V), rt : Tree<K, V>) : Tree<K, V> {
    // thank you, algebraic pattern matching!
    // following notes from [Ravi Chugh](https://www.classes.cs.uchicago.edu/archive/2019/spring/22300-1/lectures/RedBlackTrees/index.html)
    switch (color, lt, kv, rt) {
      case (#B, #node(#R, #node(#R, a, k, b), v, c), z, d) {
        #node(#R, #node(#B, a, k, b), v, #node(#B, c, z, d))
      };
      case (#B, #node(#R, a, k, #node(#R, b, v, c)), z, d) {
        #node(#R, #node(#B, a, k, b), v, #node(#B, c, z, d))
      };
      case (#B, a, k, #node(#R, #node(#R, b, v, c), z, d)) {
        #node(#R, #node(#B, a, k, b), v, #node(#B, c, z, d))
      };
      case (#B, a, k, #node(#R, b, v, #node(#R, c, z, d))) {
        #node(#R, #node(#B, a, k, b), v, #node(#B, c, z, d))
      };
      case _ { #node(color, lt, kv, rt) };
    }
  };

  func insertRoot<K, V>(k : K, compareTo : (K, K) -> O.Order, v : V, t : Tree<K, V>)
    : (?V, Tree<K, V>) {
    switch (insertRec(k, compareTo, v, t)) {
      case (_, #leaf) { assert false; loop { } };
      case (vo, #node(_, l, kv, r)) { (vo, #node(#B, l, kv, r)) };
    }
  };

  func insertRec<K, V>(k : K, compareTo : (K, K) -> O.Order, v : V, t : Tree<K, V>)
    : (?V, Tree<K, V>) {
    switch t {
      case (#leaf) { (null, #node(#R, #leaf, (k, ?v), #leaf)) };
      case (#node(c, l, kv, r)) {
        switch (compareTo(k, kv.0)) {
          case (#less) {
            let (vo, l2) = insertRec(k, compareTo, v, l);
            (vo, bal(c, l2, kv, r))
          };
          case (#equal) {
            (kv.1, #node(c, l, (k, ?v), r))
          };
          case (#greater) {
            let (vo, r2) = insertRec(k, compareTo, v, r);
            (vo, bal(c, l, kv, r2))
          };
        }
      }
    }
  };

  func getRec<K, V>(k : K, compareTo : (K, K) -> O.Order, t : Tree<K, V>) : ?V {
    switch t {
      case (#leaf) { null };
      case (#node(c, l, kv, r)) {
        switch (compareTo(k, kv.0)) {
          case (#less) { getRec(k, compareTo, l) };
          case (#equal) { kv.1 };
          case (#greater) { getRec(k, compareTo, r) };
        }
      };
    }
  };

  func height<K, V>(t : Tree<K, V>) : Nat {
    switch t {
      case (#leaf) { 0 };
      case (#node(_, l, _, r)) {
        Nat.max(height(l), height(r)) + 1
      }
    }
  };

  /// The size of the tree as the number of key-value entries.
  public func size<K, V>(t : Tree<K, V>) : Nat {
    switch t {
      case (#leaf) { 0 };
      case (#node(_, l, kv, r)) {
        size(l) + size(r) + (switch (kv.1) { case null 0; case _ 1 });
      };
    }
  };

  func optionValueEquals<V>(valueEquals: (V, V) -> Bool, v1: ?V, v2: ?V): Bool {
    switch (v1, v2) {
      case (null, null) { true };
      case (?v1, ?v2) { valueEquals(v1, v2) };
      case _ { false }
    }
  };

  
  /// For most purposes, one should prefer this equalIgnoreDeleted function as opposed to equalIncludeDeleted.
  ///
  /// Functional Red-Black trees do not have efficient operations for deleting a red black tree. For reference, see 
  /// https://matt.might.net/papers/germane2014deletion.pdf.
  ///
  /// Therefore, "deleting" a node is represented as setting the value to null for a specific key.
  ///
  /// The equalIgnoreDeleted function returns a boolean value indicating if two Red-Black Trees are equivalent, ignoring node coloring
  /// and focusing solely on node location and key value equality as per the keyEquals and valueEquals methods supplied.
  ///
  /// Note the difference betweenn equalIgnoreDeleted and equalIncludeDeleted in the result of the last line in the following example.
  ///
  /// Example:
  ///
  /// ```motoko
  /// var t1 = RBT.init<Nat, Text>();
  /// var t2 = RBT.init<Nat, Text>();
  /// t1 := RBT.put<Nat, Text>(t1, Nat.compare, 35, "john");
  /// t2 := RBT.put<Nat, Text>(t1, Nat.compare, 35, "john");
  /// RBT.equalIgnoreDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal); // true
  /// RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal); // true
  ///
  /// t1 := RBT.put<Nat, Text>(t1, Nat.compare, 31, "alice");
  /// t1 := RBT.delete<Nat, Text>(t1, Nat.compare, 31);
  /// RBT.equalIgnoreDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal); // true
  /// RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal); // false 
  /// ```
  public func equalIgnoreDeleted<K, V>(t1: Tree<K, V>, t2: Tree<K, V>, keyEquals: (K, K) -> Bool, valueEquals: (V, V) -> Bool): Bool {
    let e1 = entries(t1);
    let e2 = entries(t2);
    return loop {
      switch(e1.next(), e2.next()) {
        case (null, null) { return true };
        case (?(k1, v1), ?(k2, v2)) {
          if ( not( 
            keyEquals(k1, k2) and optionValueEquals<V>(valueEquals, ?v1, ?v2)
          )) { return false }
        };
        case _ { return false }
      }
    }
  };

  /// Functional Red-Black trees do not have efficient operations for deleting a red black tree. Therefore, "deleting" a node is
  /// represented as setting the value to null for a specific key.
  ///
  /// Returns a boolean value indicating if two Red-Black Trees are equivalent, including node coloring and deleted "null" nodes,
  /// as well as node location and key value equality as per the keyEquals and valueEquals methods supplied
  ///
  /// Example:
  ///
  /// ```motoko
  /// var t1 = RBT.init<Nat, Text>();
  /// var t2 = RBT.init<Nat, Text>();
  /// t1 := RBT.put<Nat, Text>(t1, Nat.compare, 35, "john");
  /// t2 := RBT.put<Nat, Text>(t1, Nat.compare, 35, "john");
  /// RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal); // true
  ///
  /// t1 := RBT.put<Nat, Text>(t1, Nat.compare, 31, "alice");
  /// t1 := RBT.delete<Nat, Text>(t1, Nat.compare, 31);
  /// RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal); // false 
  /// ```
  ///
  public func equalIncludeDeleted<K, V>(t1: Tree<K, V>, t2: Tree<K, V>, keyEquals: (K, K) -> Bool, valueEquals: (V, V) -> Bool): Bool {
    switch(t1, t2) {
      case (#leaf, #leaf) { true };
      case (#node(c1, l1, (k1, ?v1), r1), #node(c2, l2, (k2, ?v2), r2)) {
        if (keyEquals(k1, k2) and optionValueEquals<V>(valueEquals, ?v1, ?v2)) {
          equalIncludeDeleted(l1, l2, keyEquals, valueEquals) and equalIncludeDeleted(r1, r2, keyEquals, valueEquals);
        } else {
          false
        }
      };
      case _ { false };
    }
  };
}
