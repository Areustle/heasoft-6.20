@symbol=zzz	! is this really comment ?
@//$symbol
echo $symbol

@aaa=a
@a=abc
@test=$$aaa aaa
@//$test

@aaa=a
@a=abc
@test=$$$aaa aaa
@//$test

@aaa=
@aaa=aaa $aaa
@//$aaa

@aaaa=
@bbbb=
@aaaa=aaaa$bbbb
@bbbb=bbbb$aaaa
@//$aaaa

1234567890
@char-after-space=a           
@//$char-after-space='$(char-after-space)'
