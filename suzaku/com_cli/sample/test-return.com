@:loop
@answer=?return(yes/no) ?
@.if $(answer) EQS yes
@return_value=
@return_value=?value ?
@.if $(?return_value) EQ 1
@.	return $(return_value)
@.else
@.	return
@.endif
@.endif
@.goto loop
