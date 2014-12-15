This is a silly experiment based on http://stackoverflow.com/questions/27463669/haskell-performance-implementing-unixs-cat-program-with-data-bytestring 

    timing 'cat'

    real	0m0.926s
    user	0m0.005s
    sys	0m0.231s


    timing lazy bytestring (standard)

    real	0m0.978s
    user	0m0.077s
    sys	0m0.290s


    timing fancy buffering from SO

    real	0m0.988s
    user	0m0.288s
    sys	0m0.440s


    timing fancier use of GHC Buf from SO

    real	0m0.822s
    user	0m0.033s
    sys	0m0.257s


    timing Pipes.ByteString

    real	0m0.978s
    user	0m0.021s
    sys	0m0.255s


    timing Pipes.MMap with SafeT stuff

    real	0m0.146s
    user	0m0.011s
    sys	0m0.015s


    timing Pipes.MMap

    real	0m0.143s
    user	0m0.007s
    sys	0m0.016s


    timing lazy mmap

    real	0m0.139s
    user	0m0.006s
    sys	0m0.014s
