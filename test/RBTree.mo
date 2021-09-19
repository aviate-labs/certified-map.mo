import RBTree "../src/RBTree";

func isRed(n : ?RBTree.Node) : Bool {
    switch (n) {
        case (? (_, _, _, _, #Red, _)) { true;  };
        case (_)                     { false; };
    };
};

func isBalanced(t : RBTree.RBTree) : Bool {
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
    var current = t.root;
    label l loop {
        switch (current) {
            case (null) { break l; };
            case (? n) {
                if (not isRed(?n)) nrBlack += 1;
                current := n.2;
            };
        };
    };
    _isBalanced(t.root, nrBlack);
};