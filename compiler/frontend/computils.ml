let keccakofstring str = (Cryptokit.hash_string (Cryptokit.Hash.keccak 256) (Bytes.to_string str))
