@.if 0
echo true
@.elif 0
echo elif 0
@.elif 1
echo elif 1
@.elif 2
echo elif 2
@.else
echo false
@.endif

@.if	0 EQ 0
echo true
@.endif

@.if	NOT 1 EQ 0
echo true
@.endif

@.if	abc eqs abc
echo true
@.endif

@$ [ 2 = 2 ]
@.if $? eq 0
echo true
@.else
echo false
@.endif

@i=3
@:loop
@i=`awk 'BEGIN{ print $(i)-1; }'`
echo i=$(i)
@.if $i ne 0
@.	goto loop
@.endif

@.if 1
@.  if 1
@.    if 1
@.      if 0
echo true
@.      elif 1
echo elif 1
@.      else
echo else
@.      endif
@.    endif
@.  endif
@.else
@.  if 1
@.    if 1
@/*
@/*
@.      if 0
echo true
@.      elif 1
echo elif 1
@.      endif
@*/
@.    endif
@.  endif
@.endif

@.if 1
echo true
@.elif 2
@.elif 3
@.elif 4
@.else 5
@.elif 6
@.else 7
@.endif

@.if 0
echo true
@.elif 0
@.elif 0
@.elif 0
@.else 0
echo false
@.status
@.elif 0
@.else 0
@.endif

@.if 1
@.if 2
@.if 3
@.if 4
@.if 5
@.if 6
@.if 7
@.if 0
@!.if 9
echo ok
@.status
@.else
echo false
@.status
@!.endif 9
@.endif 8
@.endif 7
@.endif 6
@.endif 5
@.endif 4
@.endif 3
@.endif 2
@.endif 1
