#!/usr/bin/env bash
 

# Compile with optimisation
ghc -O2 Main.hs
ghc -O2 LazyMain.hs
ghc -O2 GHCBufMain.hs
ghc -O2 PipesMain.hs
ghc -O2 PipesMMap.hs
ghc -O2 PipesMMapSafish.hs
ghc -O2 LazyMMapMain.hs

 
sleep 5
# Give some time for freeing up memory

# Run cat
echo "timing 'cat'"
time cat huge > /dev/null


echo ""
echo ""
sleep 5


echo "timing lazy bytestring (idiomatic Haskell)"
time ./LazyMain huge1 > /dev/null


echo ""
echo ""
sleep 5

echo "timing fancy buffering following statusfailed"
time ./Main huge2 > /dev/null


echo ""
echo ""
sleep 5

echo "timing fancier use of GHC.Buf following bmk"
time ./GHCBufMain huge3 > /dev/null


echo ""
echo ""
sleep 5

echo "timing Pipes.ByteString following sibi"
time ./PipesMain huge4 > /dev/null




echo "timing Lazy.MMap following dons and tommd"
time ./LazyMMapMain huge5 > /dev/null

echo ""
echo ""
sleep 5

echo "timing Pipes.MMap with SafeT stuff"
time ./PipesMMapSafish huge6 > /dev/null

echo ""
echo ""
sleep 5

echo "timing Pipes.MMap"
time ./PipesMMap huge6 > /dev/null

echo ""
echo ""
sleep 5


