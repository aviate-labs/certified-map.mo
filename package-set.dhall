let base = https://github.com/internet-computer/base-package-set/releases/download/moc-0.7.4/package-set.dhall sha256:3a20693fc597b96a8c7cf8645fda7a3534d13e5fbda28c00d01f0b7641efe494
let Package = { name : Text, version : Text, repo : Text, dependencies : List Text }

let additions = [
  { name = "array"
  , version = "v0.2.1"
  , repo = "https://github.com/aviate-labs/array.mo"
  , dependencies = [ "base-0.7.3" ] : List Text
  },
  { name = "crypto"
  , repo = "https://github.com/aviate-labs/crypto.mo"
  , version = "v0.3.1"
  , dependencies = [ "base-0.7.3", "encoding" ]
  },
  { name = "encoding"
  , repo = "https://github.com/aviate-labs/encoding.mo"
  , version = "v0.4.1"
  , dependencies = [ "array", "base-0.7.3" ]
  }
] : List Package

in  base # additions
