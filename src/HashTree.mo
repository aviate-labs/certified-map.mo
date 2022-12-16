import { Array_tabulate } = "mo:⛔";
import Blob "mo:base-0.7.3/Blob";
import Iter "mo:base-0.7.3/Iter";
import Nat8 "mo:base-0.7.3/Nat8";
import SHA256 "mo:crypto/SHA/SHA256";
import Text "mo:base-0.7.3/Text";

module {
    // TODO: replace [Nat8] with Blob, once it support indices.
    public type Hash  = [Nat8];
    public type Label = [Nat8];

    public type HashTree = {
        #Empty;
        #Fork    : (HashTree, HashTree);
        #Labeled : (Label,    HashTree);
        #Leaf    : [Nat8];
        #Pruned  : Hash;
    };

    public func reconstruct(t : HashTree) : Hash {
        switch (t) {
            case (#Empty) {
                let h = domainSeparator("ic-hashtree-empty");
                SHA256.sum(h);
            };
            case (#Fork(t1, t2)) {
                forkHash(reconstruct(t1), reconstruct(t2));
            };
            case (#Labeled(l, t)) {
                labeledHash(l, reconstruct(t));
            };
            case (#Leaf(v)) {
                leafHash(v);
            };
            case (#Pruned(h)) {
                h;
            };
        };
    };

    public func forkHash(l : Hash, r : Hash) : Hash {
        let h = domainSeparator("ic-hashtree-fork");
        SHA256.sum(append([h, l, r]));
    };

    public func labeledHash(l : Label, content : Hash) : Hash {
        SHA256.sum(append([domainSeparator("ic-hashtree-labeled"), l, content]));
    };

    public func leafHash(content : [Nat8]) : Hash {
        SHA256.sum(append([domainSeparator("ic-hashtree-leaf"), content]));
    };

    private func append(xs : [[Nat8]]) : [Nat8] {
        var size = 0;
        let sizes = Array_tabulate<(Nat, Nat)>(
            xs.size(),
            func (i : Nat) : (Nat, Nat) {
                let s = size;
                size += xs[i].size();
                (s, size);
            }
        );
        var index = 0;
        Array_tabulate(
            size,
            func (i : Nat) : Nat8 {
                let (f0, t0) = sizes[index];
                if (i < t0) return xs[index][i - f0];
                index += 1;
                let (f1, _) = sizes[index];
                return xs[index][i - f1];
            }
        );
    };

    private func domainSeparator(t : Text) : [Nat8] {
        let text = Blob.toArray(Text.encodeUtf8(t));
        Array_tabulate(
            t.size() + 1,
            func (i : Nat) : Nat8 {
                if (i == 0) return Nat8.fromNat(t.size());
                text[i - 1];
            }
        );
    };
};
