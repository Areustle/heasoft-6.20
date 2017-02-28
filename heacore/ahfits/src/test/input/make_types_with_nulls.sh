cp -fv types.fits types_with_nulls.fits
fthedit types_with_nulls.fits TNULL1 add 255 insert=TFORM1
fthedit types_with_nulls.fits TNULL4 add -1024 insert=TFORM4
fthedit types_with_nulls.fits TNULL5 add -32768 insert=TFORM5
fthedit types_with_nulls.fits TNULL6 add -1000000000 insert=TFORM6
fthedit types_with_nulls.fits TNULL10 add -32768 insert=TFORM10
fthedit types_with_nulls.fits TNULL11 add 255 insert=TFORM11
fthedit types_with_nulls.fits TNULL13 add -1024 insert=TFORM13
fthedit types_with_nulls.fits TNULL14 add -1000000000 insert=TFORM14
fthedit types_with_nulls.fits TNULL19 add 255 insert=TFORM19
cat > __tmp_make_types_with_nulls.txt <<EOF
a_byte       2      255
a_short      2    -1024
a_long       2   -32768
a_longlong   2 -1000000000
a_float      2      NaN
a_double     2      NaN
v_longs      2   3       -32768
v_longs      2   7       -32768
v_bytes      2   3          255
v_bytes      2   7          255
v_shorts     2   3        -1024
v_shorts     2   7        -1024
v_longlongs  2   3     -1000000000
v_longlongs  2   7     -1000000000
v_floats     2   3          NaN
v_floats     2   7          NaN
v_doubles    2   3          NaN
v_doubles    2   7          NaN
EOF
ftedit types_with_nulls.fits @__tmp_make_types_with_nulls.txt
rm __tmp_make_types_with_nulls.txt

