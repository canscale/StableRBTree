import Iter "mo:base/Iter";
import RBTree "mo:base/RBTree";
import RBTreeStable "mo:stable-rbtree/StableRBTree";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Trie "mo:base/Trie";
import TrieMap "mo:base/TrieMap";

actor Echo {  
  var tree = RBTree.RBTree<Nat,Text>(Nat.compare);
  stable var stabletree = RBTreeStable.init<Nat,Text>();
  var trie: Trie.Trie<Nat,Text> = #empty;
  var trieMap = TrieMap.TrieMap<Nat, Text>(Nat.equal, func(n: Nat) { Text.hash(Nat.toText(n)) });

  var itemCount = 0;
  var stableItemCount = 0;
  var trieItemCount = 0;
  var trieMapItemCount = 0;

  public shared func testRBTree(items : Nat) : async Text {
    let newItemCount = itemCount + items;
    let tree = RBTree.RBTree<Nat,Text>(Nat.compare);
    for(thisItem in Iter.range(itemCount,newItemCount)){
        tree.put(thisItem, Nat.toText(thisItem));
    };
    itemCount := newItemCount;

    return "passed w/count: " # Nat.toText(itemCount);
  };

  public shared func testRBTreeStable(items : Nat) : async Text {
    let newItemCount = stableItemCount + items;
    
    for(thisItem in Iter.range(stableItemCount,newItemCount)){
        stabletree := RBTreeStable.put<Nat,Text>(stabletree, Nat.compare, thisItem, Nat.toText(thisItem));
    };
    stableItemCount := newItemCount;

    return "passed w/count: " # Nat.toText(stableItemCount);
  };

  func key(n: Nat): Trie.Key<Nat> = { key = n; hash = Text.hash(Nat.toText(n)); };

  public shared func testTrieStable(items : Nat) : async Text {
    let newItemCount = trieItemCount + items;
    
    for(thisItem in Iter.range(trieItemCount,newItemCount)){
        let (t,  _) = Trie.put<Nat,Text>(trie, key(thisItem), Nat.equal, Nat.toText(thisItem));
        trie := t;
    };
    trieItemCount := newItemCount;

    return "passed w/count: " # Nat.toText(trieItemCount);
  };

    public shared func testTrieMap(items : Nat) : async Text {
    let newItemCount = trieMapItemCount + items;
    
    for(thisItem in Iter.range(trieMapItemCount,newItemCount)){
        trieMap.put(thisItem, Nat.toText(thisItem));
    };
    trieMapItemCount := newItemCount;

    return "passed w/count: " # Nat.toText(trieMapItemCount);
  };

  public shared func clearTest() : async Text {
    stabletree := RBTreeStable.init<Nat,Text>();
    tree := RBTree.RBTree<Nat,Text>(Nat.compare);
    trie := #empty;
    trieMap := TrieMap.TrieMap<Nat, Text>(Nat.equal, func(n: Nat) { Text.hash(Nat.toText(n)) });
    itemCount := 0;
    stableItemCount := 0;
    trieItemCount := 0;
    trieMapItemCount := 0;

    return "cleared";
  };
};