@!.goto END

@.WHILE 1
@.IF 1

@.set break on
@/*
@i=0
@:loop
@//$(i)
@i=`awk 'BEGIN{ print $(i)+1; }'`
@.sleep 0.5
@.sleep 0.1
@.goto loop
@*/

@i=3
@.while $(i) gt 0
@i=`awk 'BEGIN{ print $(i)-1; }'`
echo "i=$(i)"
echo 1
@.repeat
echo 2
@.while 0
echo 3
@.end
echo 4
@.until 1
echo 5
@.end

@i=3
@.repeat
@i=`awk 'BEGIN{ print $(i)-1; }'`
echo "i=$(i)"
@.until $(i) eq 0

@i=0
@.while 1
@i=`awk 'BEGIN{ print $(i)+1; }'`
@.repeat
@.if $(i) eq 5
echo "i=$(i)"
@.goto END
echo "i=$(i)"
@.endif
@.until 1
@.end

@.ENDIF
@.END

@:END
echo END

@<

@.status

@.repeat 11
@.repeat 10
@.repeat  9
@.repeat  8
@.repeat  7
@.repeat  6
@.repeat  5
@!.repeat  4
@!.repeat  3
@!.repeat  2
@!.repeat  1
echo 1
@:
@.status
@.goto skip
@.zzz
@!:skip
@!.until   1
echo 2
@!.until   2
echo 3
@!.until   3
echo 4
@!.until   4
echo 5
@.until   5
echo 6
@.until   6
echo 7
@.until   7
@:skip
echo 8
@.until   8
echo 9
@.until   9
echo 10
@.until  10
echo 11
@.until  11

@!:skip
echo 12

@.goto loop
echo END
