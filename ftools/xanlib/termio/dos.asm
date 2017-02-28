.MODEL	LARGE
	INCLUDE dos.inc
.CODE
	PUBLIC RDCHR, WRCHR

RDCHR	PROC
;---
; Read a single character from the keyboard.
; From Fortran use:
;	CHARACTER CHR
;	CALL RDCHR(CHR)
;---
; CHR       O  The character read.
;---
; 1990-Mar-21 - [AFT]
;---
	PUSH	BP
	MOV	BP,SP
	PUSH	AX
	PUSH	BX
	PUSH	ES
	JMP	OVER

IESC	DB	0,0,0,0		; Information about escape sequences
;
; Table gives the translation for scan codes 71 and above
TABLE	DB	2,'~','1',0	; Find
	DB	1,'A',0,0	; Cursor up
	DB	2,'~','5',0	; Pg Up (Prev)
	DB	0,0,0,0		; KP- (returns ASCII '-')
	DB	1,'D',0,0	; Cursor left
	DB	0,0,0,0		; KP5 (returns ASCII '5')
	DB	1,'C',0,0	; Cursor right
	DB	0,0,0,0		; KP+ (returns ASCII '+')
	DB	2,'~','4',0	; End (Select)
	DB	1,'B',0,0	; Cursor down
	DB	2,'~','6',0	; Pg Dn (Next)
	DB	2,'~','2',0	; Ins (Insert)
	DB	2,'~','3',0	; Del (Remove)
	DB	3,'~','3','2'	; F11
	DB	3,'~','4','2'	; F12
	DB	3,'~','5','2'	; F13
	DB	3,'~','6','2'	; F14
	DB	3,'~','8','2'	; Help (F15)
	DB	3,'~','9','2'	; Do   (F16)
;
; Check to see if we are in the process of sending an escape sequence.
OVER:	CMP	IESC,0
	JZ	KREAD
	MOV	BL,IESC
	XOR	BH,BH
	MOV	AL,IESC[BX]
	DEC	IESC
	JMP	EXITRD
;
; Call BIOS to get a keystroke
KREAD:	MOV	AH,00H
	INT	16H
	CMP	AH,0EH		; Is it the delete key ?
	JNE	NOTDEL
	MOV	AL,127		; If so, make it a delete character
;
NOTDEL:	CMP	AL,00H		; Not an ASCII character
	JNE	EXITRD
	SUB	AH,47H		; See if this scan code has a translation
	JL	EXITRD
	CMP	AH,12H
	JG	EXITRD
	MOV	AL,AH		; Yes, load translation into IESC area
	XOR	AH,AH
	SHL	AX,1
	SHL	AX,1
	MOV	BX,AX
	MOV	AL,TABLE[BX]
	MOV	IESC,AL
	MOV	AL,TABLE+1[BX]
	MOV	IESC+1,AL
	MOV	AL,TABLE+2[BX]
	MOV	IESC+2,AL
	MOV	AL,TABLE+3[BX]
	MOV	IESC+3,AL
	MOV	AL,9BH
;
EXITRD:	LES	BX,[BP+6]
	MOV	ES:[BX],AL

	POP	ES
	POP	BX
	POP	AX
	POP	BP
	RET	4
RDCHR	ENDP
;*********
WRCHR	PROC
;---
; Write a single character to the display.
; From Fortran use:
;	CHARACTER CHR
;	CALL WRCHR(CHR)
;---
; CHR     I    The character to be written
;---
; 1990-Mar-21 - [AFT]
;---
	PUSH	BP
	MOV	BP,SP
	PUSH	AX
	PUSH	BX
	PUSH	ES

	LES	BX,[BP+6]
	MOV	AX,ES:[BX]
	@DISPCH AL

	POP	ES
	POP	BX
	POP	AX
	POP	BP
	RET	4
WRCHR	ENDP
	END
