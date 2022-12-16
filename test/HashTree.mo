import Blob "mo:base-0.7.3/Blob";
import Hex "mo:encoding/Hex";
import Nat8 "mo:base-0.7.3/Nat8";
import SHA256 "mo:crypto/SHA/SHA256";
import Text "mo:base-0.7.3/Text";

import HashTree "../src/HashTree";

func b(t : Text) : [Nat8] {
    Blob.toArray(Text.encodeUtf8(t));
};

// Source: https://sdk.dfinity.org/docs/interface-spec/index.html#_example

// ─┬─┬╴"a" ─┬─┬╴"x" ─╴"hello"
//  │ │      │ └╴Empty
//  │ │      └╴  "y" ─╴"world"
//  │ └╴"b" ──╴"good"
//  └─┬╴"c" ──╴Empty
//    └╴"d" ──╴"morning"
let x : HashTree.HashTree = #Fork(
    #Labeled(b("x"), #Leaf(b("hello"))),
    #Empty,
);
assert(
    Hex.encode(HashTree.reconstruct(x))
    == "1b4feff9bef8131788b0c9dc6dbad6e81e524249c879e9f10f71ce3749f5a638",
);

let bt : HashTree.HashTree = #Leaf(b("good"));
assert(
    Hex.encode(HashTree.reconstruct(bt))
    == "7b32ac0c6ba8ce35ac82c255fc7906f7fc130dab2a090f80fe12f9c2cae83ba6",
);

let c : HashTree.HashTree = #Labeled(b("c"), #Empty);
assert(
    Hex.encode(HashTree.reconstruct(c))
    == "ec8324b8a1f1ac16bd2e806edba78006479c9877fed4eb464a25485465af601d",
);

let tree : HashTree.HashTree = #Fork(
    #Fork(
        #Labeled(b("a"), #Fork(
            x,
            #Labeled(b("y"), #Leaf(b("world"))),
        )),
        #Labeled(b("b"), bt),
    ),
    #Fork(
        c,
        #Labeled(b("d"), #Leaf(b("morning"))),
    ),
);

assert(
    Hex.encode(HashTree.reconstruct(tree))
    == "eb5c5b2195e62d996b84c9bcc8259d19a83786a2f59e0878cec84c811f669aa0"
);
