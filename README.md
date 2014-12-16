This just replaces the lazy bytestrings from `bytestring-mmap`
with an intuitively correct type, here `Producer ByteString IO ()` .
It is about seven times as fast as the usual `Pipes.ByteString.fromHandle`.
Of course `fromHandle` is extremely fast, so the difference tends to
disappear with a few additional calculations or IO operations,
but is perhaps desirable in suitable cases.

The benchmarks below compare various implementations of `cat <file>` following
[this stack overflow discussion](http://stackoverflow.com/questions/27463669/haskell-performance-implementing-unixs-cat-program-with-data-bytestring) using contributions from `statusfailed`, `bmk`, `dons`, `tommd`, and `sibi`.

(The files used here are in `/bench` using `sibi`s caching-avoidance manoeuvre; the material from `/test` is copied over from `bytestring-mmap`)

    filesize
    4200000           42000000         420000000         4200000000

    timing 'cat'
                                    
    real  0m0.006s    real  0m0.013s    real  0m0.919s    real  0m8.154s
    user  0m0.002s    user  0m0.002s    user  0m0.005s    user  0m0.028s
    sys   0m0.003s    sys   0m0.009s    sys   0m0.223s    sys   0m2.179s
                                    
                                    
    timing lazy bytestring (idiomatic Haskell)
                                    
    real  0m0.009s    real  0m0.025s    real  0m0.894s    real  0m9.146s
    user  0m0.002s    user  0m0.006s    user  0m0.078s    user  0m0.787s
    sys   0m0.005s    sys   0m0.016s    sys   0m0.288s    sys   0m3.001s
                                    
                                    
    timing fancy buffering following statusfailed
                                    
    real  0m0.014s    real  0m0.066s    real  0m0.876s    real  0m8.686s
    user  0m0.005s    user  0m0.028s    user  0m0.278s    user  0m2.724s
    sys   0m0.007s    sys   0m0.035s    sys   0m0.424s    sys   0m4.232s
                                    
                                    
    timing fancier use of GHC.Buf following bmk
                                    
    real  0m0.011s    real  0m0.018s    real  0m0.831s    real  0m8.218s
    user  0m0.002s    user  0m0.003s    user  0m0.034s    user  0m0.289s
    sys   0m0.006s    sys   0m0.013s    sys   0m0.236s    sys   0m2.447s
                                    
                                    
    timing Pipes.ByteString following sibi
                                    
    real  0m0.012s    real  0m0.020s    real  0m0.845s    real  0m8.241s
    user  0m0.003s    user  0m0.004s    user  0m0.020s    user  0m0.175s
    sys   0m0.007s    sys   0m0.014s    sys   0m0.239s    sys   0m2.509s


    timing Lazy.MMap following dons and tommd
                                    
    real  0m0.006s    real  0m0.006s    real  0m0.037s    real  0m0.133s
    user  0m0.002s    user  0m0.002s    user  0m0.006s    user  0m0.051s
    sys   0m0.003s    sys   0m0.003s    sys   0m0.013s    sys   0m0.061s
                                    
                                    
    timing Pipes.ByteString.MMap with SafeT machinery
                                    
    real  0m0.006s    real  0m0.010s    real  0m0.051s    real  0m0.196s
    user  0m0.002s    user  0m0.004s    user  0m0.012s    user  0m0.099s
    sys   0m0.003s    sys   0m0.005s    sys   0m0.016s    sys   0m0.072s
                                    
                                    
    timing Pipes.ByteString.MMap 'withFile' style
                                    
    real  0m0.008s    real  0m0.008s    real  0m0.142s    real  0m0.134s
    user  0m0.002s    user  0m0.002s    user  0m0.007s    user  0m0.046s
    sys   0m0.004s    sys   0m0.004s    sys   0m0.016s    sys   0m0.066s
