@.goto END
@:LABEL1:
@:LABEL2:
@:	LABEL3:
@:		LABEL4:
@:LABEL5:
@:
@:LABEL2
@:
@.sleep 0.5
@!.goto LABEL1:
@!.goto LABEL10:
@!.goto LABEL2:
@!.goto LABEL2
@//Done
@.exit 0
@:END
@.goto LABEL4:
