import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Iter "mo:base/Iter";
import Nat8 "mo:base/Nat8";
import SHA256 "mo:sha/SHA256";
import Text "mo:base/Text";

module {
    type Hash  = Blob;
    type Label = Blob;

    type HashTree<T> = {
        #Empty;
        #Fork    : (HashTree<T>, HashTree<T>);
        #Labeled : (Label,       HashTree<T>);
        #Leaf    : Blob;
        #Pruned  : Hash;
    };

    public func reconstruct<T>(t : HashTree<T>) : Hash {
        switch (t) {
            case (#Empty) {
                let ds = domainSeperator("ic-hashtree-empty");
                Blob.fromArray(SHA256.sum256(
                    Blob.toArray(ds),
                ));
            };
            case (#Fork(t1, t2)) {
                let ds = domainSeperator("ic-hashtree-fork");
                Blob.fromArray(SHA256.sum256(
                    append([ds, reconstruct(t1), reconstruct(t2)]),
                ));
            };
            case (#Labeled(l, t)) {
                let ds = domainSeperator("ic-hashtree-labeled");
                Blob.fromArray(SHA256.sum256(
                    append([ds, l, reconstruct(t)]),
                ));
            };
            case (#Leaf(v)) {
                let ds = domainSeperator("ic-hashtree-leaf");
                Blob.fromArray(SHA256.sum256(
                    append([ds, v]),
                ));
            };
            case (#Pruned(h)) {
                h;
            };
        };
    };

    private func append(xs : [Blob]) : [Nat8] {
        var ys = Blob.toArray(xs[0]);
        for (i in Iter.range(0, xs.size()-1)) {
            ys := Array.append(ys, Blob.toArray(xs[i]));
        };
        ys;
    };

    private func domainSeperator(t : Text) : Blob {
        Blob.fromArray(Array.append<Nat8>(
            [Nat8.fromNat(t.size())],
            Blob.toArray(Text.encodeUtf8(t)),
        ));
    };
};
