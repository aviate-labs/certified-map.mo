import Blob "mo:base/Blob";
import Iter "mo:base/Iter";
import Nat8 "mo:base/Nat8";

import RBTree "../src/RBTree";

func isRed(n : ?RBTree.Node) : Bool {
    switch (n) {
        case (? (_, _, _, _, #Red, _)) { true;  };
        case (_)                     { false; };
    };
};

func isBalanced(t : ?RBTree.Node) : Bool {
    func _isBalanced(n : ?RBTree.Node, nrBlack : Nat) : Bool {
        var _nrBlack = nrBlack;
        switch (n) {
            case (null) {
                _nrBlack == 0;
            };
            case (? n) {
                if (not isRed(?n)) {
                    _nrBlack -= 1;
                } else {
                    assert(not isRed(n.2));
                    assert(not isRed(n.3));
                };
                _isBalanced(n.2, _nrBlack) and _isBalanced(n.3, _nrBlack)
            };
        };
    };

    // Calculate number of black nodes by following left.
    var nrBlack = 0;
    var current = t;
    label l loop {
        switch (current) {
            case (null) { break l; };
            case (? n) {
                if (not isRed(?n)) nrBlack += 1;
                current := n.2;
            };
        };
    };
    _isBalanced(t, nrBlack);
};

func toBlob(n : Nat8) : Blob {
    Blob.fromArray([n]);
};

var tree : ?RBTree.Node = null;

func insert(n : Nat8) {
    let kv = toBlob(n);
    let (nt, ov) = RBTree.insertRoot(tree, kv, kv);
    assert(ov == null);
    assert(isBalanced(?nt));
    tree := ?nt;
};

insert(10);
insert(8);
insert(12);
insert(9);
insert(11);
