define
full ok
modify
fitsread ok
pseudo
1.0
exit
modify
circreg ok
s0 x=0.0 y=0.0 r=150.0
s1 x=0.0 y=0.0 r=150.0
g2 x=0.0 y=0.0 r=16.0
g3 x=0.0 y=0.0 r=16.0
-1
book
full ok
analyze
read
../testdata/dummysis.fits
-1 10000
read
../testdata/dummygis.fits
-1 10000
exit
n
@! @$ [ -f test.hbk ] && rm test.hbk
@! test.hbk
