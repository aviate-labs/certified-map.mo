import Blob "mo:base/Blob";
import Order "mo:base/Order";
import P "mo:base/Prelude";

import HashTree "HashTree";

module {
    public type Color = {
        #Red;
        #Black;
    };

    private func flip(c : Color) : Color {
        switch (c) {
            case (#Red)   { #Black; };
            case (#Black) { #Red;   };
        };
    };

    public type RBTree = {
        root : ?Node;
    };

    public func get(t : RBTree, k : Blob) : ?Blob {
        var root = t.root;
        label l loop {
            let (key, v, l, r, _, _) = switch (root) {
                case (null) { break l; };
                case (? v)  { v;       };
            };
            switch (Blob.compare(k, key)) {
                case (#less) {
                    root := l;
                };
                case (#equal) {
                    return ?v;
                };
                case (#greater) {
                    root := r;
                };
            };
        };
        null;
    };

    public type Node = (
        key   : Blob,          // 0 : key
        value : Blob,          // 1 : value
        left  : ?Node,         // 2 : left
        right : ?Node,         // 3 : right
        color : Color,         // 4 : color
        hash  : HashTree.Hash, // 5 : hash
    );

    private func isRed(n : ?Node) : Bool {
        switch (n) {
            case (? (_, _, _, _, #Red, _)) { true;  };
            case (_)                     { false; };
        };
    };

    private func balance(n : Node) : Node {
        switch (n) {
            case (k, v, ?l, ?r, c, h) {
                if (not isRed(?l) and isRed(?r)) return rotateLeft(n);
                if (isRed(?l) and isRed(l.2))    return rotateRight(n);
                if (isRed(?l) and isRed(?r))     return (k, v, ?flipColor(l), ?flipColor(r), flip(c), h);
            };
            case (_) {};
        };
        n;
    };

    private func rotateRight(n : Node) : Node {
        assert(isRed(n.2));
        var l = unwrap(n.2);
        // n.l = n.l.r;
        let h = update((n.0, n.1, l.3, n.3, n.4, n.5));
        // r.r = h;
        // r.c = h.c;
        // r.r.c = #Red;
        update((l.0, l.1, l.2, ?(h.0, h.1, h.2, h.3, #Red, h.5), h.4, l.5));
    };

    private func rotateLeft(n : Node) : Node {
        assert(isRed(n.3));
        var r = unwrap(n.3);
        // n.r = n.r.l;
        let h = update((n.0, n.1, n.2, r.2, n.4, n.5));
        // r.l = h;
        // r.c = h.c;
        // r.l.c = #Red;
        update((r.0, r.1, ?(h.0, h.1, h.2, h.3, #Red, h.5), r.3, h.4, r.5));
    };

    private func flipColor((k, v, l, r, c, h) : Node) : Node {
        (k, v, l, r, flip(c), h);
    };

    // NOTE: do use with caution!
    private func unwrap<T>(x : ?T) : T {
        switch x {
            case (null) { P.unreachable(); };
            case (? x_) { x_; };
        };
    };

    // Returns a new node based on the given key and value.
    public func newNode(key : Blob, value : Blob) : Node {
        let hash = HashTree.labeledHash(
            key,
            HashTree.leafHash(value),
        );
        (key, value, null, null, #Red, hash);
    };

    // Updates the hashes of the given node.
    private func update(n : Node) : Node {
        let (k, v, l, r, c, _) = n;
        (k, v, l, r, c, subHashTree(n));
    };

    private func subHashTree(n : Node) : HashTree.Hash {
        let h = dataHash(n);
        let (_, _, l, r, _, _) = n;
        switch (l, r) {
            case (null, null) { h; };
            case (?  l, null) { HashTree.forkHash(l.5, h); };
            case (null, ?  r) { HashTree.forkHash(h, r.5); };
            case (?  l, ?  r) {
                HashTree.forkHash(
                    l.5, HashTree.forkHash(h, r.5),
                );
            };
        };
    };

    // Returns the HashTree corresponding to the node.
    // 1. #Empty if null.
    // 2. #Pruned(hash) otherwise.
    public func getHashTree(n : ?Node) : HashTree.HashTree {
        switch (n) {
            case (null) { #Empty;       };
            case (? n)  { #Pruned(n.5); };
        };
    };

    // Returns #Labeled(key, #Leaf(value)).
    public func getDataTree((k, v, _, _, _, _) : Node) : HashTree.HashTree {
        #Labeled(k, #Leaf(v));
    };

    // Hashes the data contained within the node.
    public func dataHash((k, v, _, _, _, _) : Node) : HashTree.Hash {
        HashTree.labeledHash(k, HashTree.leafHash(v));
    };
};
