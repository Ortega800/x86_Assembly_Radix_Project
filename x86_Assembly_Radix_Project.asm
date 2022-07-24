; Radix Project - Estevan Ortega & Dalton Ranallo
; Summary: This program takes 2 numbers of any radix and performs arthimetic on them
;						  and outputs the results in decimal and a user chosen radix.

INCLUDE Irvine32.inc

ExitProcess PROTO,dwExitCode:dword

.data
; Output Stuff

bonusRadices   DB "Radices from 2-62 are available. Enter 'X' to exit program.",0AH,0DH
			   DB "a-z represent digits of the decimal values 10 to 35 and",0AH,0DH
			   DB "A-Z represent digits of the decimal values 36 to 61.",0AH,0DH,0

; prompts (used to request info from user)
radixInPrompt  DB "Enter the radix of the input: ",0
radixOutPrompt DB "Enter the radix of the output: ",0
numberPrompt   DB "Enter input number A: ",0

; error messages (tells user something went wrong and/or next actions to take)
radixError      DB "INVALID INPUT. Valid radices include 2-62",0AH,0DH,0
variableError   DB "INVALID INPUT. Valid values for numbers must be a 16-bit value",0AH,0DH
				DB "represented using the digits of the input radix, and",0AH,0DH
				DB "falls within the specified character range of 0-9,a-z,A-Z",0AH,0DH,0
B_is_zero_error DB "(B=0 ERROR)",0
overflow_Error  DB "OVERFLOW ERROR",0
undef_Error     DB "Value is undefined when A=0 & B=0",0

; defines the format of the outputted arithmetic
formatLine     DB "Expression |  Decimal   | Specified Radix",0AH,0DH,0
addOut         DB "     A + B = ",0
subOut   	   DB "     A - B = ",0
multOut		   DB "     A * B = ",0
divOut1		   DB " A / B quo.= ",0
divOut2		   DB " A / B rem.= ",0
expOut		   DB "     A ^ B = ",0

; Input Stuff
; the strings that hold input radix & output radix
radixInString  DB 4  DUP (?)	; will be letter or unsigned 2 char number, 
radixOutString DB 4  DUP (?)	; so 2 bytes + 1 for terminating null char

; the int values of the input radix & output radix strings
radixIn		   DB ?
radixOut	   DB ?

; the strings that hold A & B
varA_String    DB 17 DUP (?)	; Arbitrary size limit of variable to 16 bytes + 1 for terminating null char
varB_String    DB 17 DUP (?)	

; the values of the A & B strings (16-bit=2-byte)
varA           DW ?
varB           DW ?

radix          DB 10

max            DW ?

.code
main PROC
	LEA    EDX,bonusRadices			; tell user allowable radices (2-62)
	CALL   WriteString
beginning:
	LEA    EDX,radixInPrompt		; get input radix
	CALL   WriteString
	LEA    EDX,radixInString		; prepare call to ReadStr
	MOV    ECX,SIZEOF radixInString
	CALL   ReadStr
	CMP    EAX,2					; after a call to ReadStr, EAX contains the size of the input string
	JLE    radixIsCorrectLength
	LEA    EDX,radixError
	CALL   WriteString
	JMP    beginning
radixIsCorrectLength:
	LEA    ECX,radixInString
	CALL   radixToInt				; converts radixInString to its value in EAX, and checks if value is 2-62 inclusive
	CMP    SI,1						; SI=1 : notValid / SI=0 : valid
	JNE    validInRadix
	LEA    EDX,radixError
	CALL   WriteString
	JMP    beginning
validInRadix:						; input radix is valid by this point
	MOV    radixIn,AL
startGetRadixOut:
	LEA    EDX,radixOutPrompt		; get output radix
	CALL   WriteString
	LEA    EDX,radixOutString
	MOV    ECX,SIZEOF radixOutString
	CALL   ReadStr
	CMP    EAX,2
	JLE    radixIsCorrectLengthAgain
	LEA    EDX,radixError
	CALL   WriteString
	JMP    startGetRadixOut
radixIsCorrectLengthAgain:
	LEA    ECX,radixOutString
	CALL   radixToInt	
	CMP    SI,1
	JNE    validOutRadix
	LEA    EDX,radixError
	CALL   WriteString
	JMP    beginning
validOutRadix:			
	MOV    radixOut,AL
getVarA:
	LEA    EDX,numberPrompt			; get 16 bit variables A & B
	MOV    AL,"A"
	MOV    [EDX+19],AL				; changes 'B' to 'A' in numberPrompt before call to WriteString
	CALL   WriteString
	LEA    EDX,varA_String
	MOV    ECX,SIZEOF varA_String
	CALL   ReadStr
	JMP    chackVar_A_InputError
var_A_error:
	LEA    EDX,variableError
	CALL   WriteString
	JMP    getVarA
chackVar_A_InputError:
	LEA    ECX,varA_String
	CALL   InputErrorCheck			; Returns DI = 1 if invalid input, and DI = 0 for valid input
	CMP    DI,1
	JE     var_A_error
	LEA    ECX,varA_String			; pre-condition for stringToInt call
	MOV    AL,radixIn
	MOV    radix,AL					; radix variable is used within stringToInt proc. initialize it to the value of radixIn
	CALL   stringToInt				; returns int value of 'varA_String' in EAX
	MOV	   EBX, 0FFFFH				; Checks if input is a 16-bit number
	CMP	   EAX, EBX
	JGE    var_A_error
	MOV    varA,AX					; lower 2 bytes of EAX
getVarB:
	LEA    EDX,numberPrompt			; repeat for B
	MOV    AL,"B"
	MOV    [EDX+19],AL				; changes 'A' to 'B' in numberPrompt before call to WriteString
	CALL   WriteString
	LEA    EDX,varB_String
	MOV    ECX,SIZEOF varB_String
	CALL   ReadStr
	JMP    chackVar_B_InputError
var_B_error:
	LEA    EDX,variableError
	CALL   WriteString
	JMP    getVarB
chackVar_B_InputError:
	LEA    ECX,varB_String
	CALL   InputErrorCheck
	CMP    DI,1
	JE     var_B_error
	LEA    ECX,varB_String			
	MOV    AL,radixIn
	MOV    radix,AL					
	CALL   stringToInt	
	MOV	   EBX, 0FFFFH
	CMP	   EAX, EBX
	JGE    var_B_error
	MOV    varB,AX					
; At this point all input gathering/processing should be finished
; so do the arithmetic and output stuff next
	LEA    EDX,formatLine			; writes the header line for output
	CALL   WriteString
; ADDITION
	LEA    EDX,addOut				; writes next output format
	CALL   WriteString
	MOVSX  EAX,varA
	MOVSX  EBX,varB
	ADD    EAX,EBX					; A+B
	MOV    radix,10					; for default base10 output
	CALL   intToString
	CALL   printTheSpaces
	MOV    BL,radixOut				; sets the variable radix = radixOut
	MOV    radix,BL
	CALL   intToString				; for user-selected radix output
	CALL   CRLF
; SUBTRACTION
	LEA    EDX,subOut				; writes next output format
	CALL   WriteString
	MOVSX  EAX,varA
	MOVSX  EBX,varB
	SUB    EAX,EBX					; first A-B
	MOV    radix,10					; for default base10 output
	CALL   intToString
	CALL   printTheSpaces
	MOV    BL,radixOut				; sets the variable radix = radixOut
	MOV    radix,BL
	CALL   intToString				; for user-selected radix output
	CALL   CRLF
; MULTIPLICATION
	LEA    EDX,multOut				; writes next output format
	CALL   WriteString
	MOV    AX,varA
	MOV    BX,varB
	IMUL   BX					    ; first A*B
	ROR    EDX,16
	MOV    DX,AX
	MOV    EAX,EDX
	MOV    radix,10					; for default base10 output
	CALL   intToString
	CALL   printTheSpaces
	MOV    BL,radixOut				; sets the variable radix = radixOut
	MOV    radix,BL
	CALL   intToString				; for user-selected radix output
	CALL   CRLF
; DIVISION (quotient)
	LEA    EDX,divOut1				; writes next output format
	CALL   WriteString
	MOV    AX,varA
	MOV    BX,varB
	CMP    BX,0
	JE     B_is_zero
	MOVSX  EDX,AX
	ROR    EDX,16
	IDIV   BX					    ; first A/B
	PUSH   EDX
	MOV    radix,10					; for default base10 output
	CWDE
	CALL   intToString
	CALL   printTheSpaces
	MOV    BL,radixOut				; sets the variable radix = radixOut
	MOV    radix,BL
	CALL   intToString				; for user-selected radix output
	CALL   CRLF
	JMP	 B_is_not_zero
B_is_zero:
	LEA    EDX,B_is_zero_error
	CALL   WriteString
	CALL   CRLF
B_is_not_zero:
; DIVISION (remainder)
	LEA    EDX,divOut2				; writes next output format
	CALL   WriteString
	MOV    BX,varB
	CMP    BX,0
	JE     continue_after_div
	POP    EDX
	MOV    radix,10					; for default base10 output
	MOVSX  EAX,DX
	CALL   intToString
	CALL   printTheSpaces
	MOV    BL,radixOut				; sets the variable radix = radixOut
	MOV    radix,BL
	CALL   intToString				; for user-selected radix output
	CALL   CRLF
	JMP    continue_after_rem
continue_after_div:
	LEA    EDX,B_is_zero_error
	CALL   WriteString
	CALL   CRLF
continue_after_rem:
; EXPONENTIATION
; If B<0 -> Negate B [aka abs(B)]
; If B=0 -> IF A=/=0 -> Return 1
;        -> IF A=0   -> undefined (error)
; If B>0 -> run as usual
; If (A^B)>32-bit -> overflow error
	LEA    EDX,expOut				; writes next output format
	CALL   WriteString
	XOR    EDX,EDX
	MOVSX  EAX,varA					; sets initial value of multiplier (this one changes)
	MOVSX  EBX,varA					; keeps initial value as constant to multiply by
	MOV    CX,varB					; make sure B is positive
	CMP    CX,0
	JG     BisPositive
	JE     BisZero
	NEG    CX						; if this executes that means B<0
BisPositive:						; power loop
	DEC    CX						; loop is 0-indexed so start value of B is decremented
	JCXZ   outOfTheLoop
	IMUL   EBX						; EAX=EAX*EBX
	CMP    EDX,0
	JNZ    is_overflow
	JMP    BisPositive
outOfTheLoop:
	MOV    radix,10					; for default base10 output
	CALL   intToString
	CALL   printTheSpaces
	MOV    BL,radixOut				; sets the variable radix = radixOut
	MOV    radix,BL
	CALL   intToString				; for user-selected radix output
	JMP    after_overflow_error
is_overflow:						; this label executes if the (A^B)> 32 bit
	CMP    EDX,0FFFFFFFFH
	JE     BisPositive
	LEA    EDX,overflow_Error
	CALL   WriteString
	JMP    after_overflow_error
BisZero:							; special case if B=0
	CMP    varA,0					; different special case if A & B = 0
	JE     undef
	MOV    AL,'1'
	CALL   WriteChar
	JMP    after_overflow_error
undef:								; executes here when A=0 & B=0
	LEA    EDX,undef_Error
	CALL   WriteString
after_overflow_error:
	CALL   CRLF						; all paths lead to this instruction
; JUMP BACK TO BEGINNING
	JMP    beginning
endProgram::
	INVOKE ExitProcess,0
main ENDP



printTheSpaces PROC USES EAX
	MOV    CX,10					; calculates how many spaces to add for consistant format bc no printf :(
	SUB    CX,max
inLoop:
	MOV    AL,' '
	CALL   WriteChar
	JCXZ   outLoop
	DEC    CX
	JMP	   inLoop
outLoop:
	MOV    AL,'='					; more formatting chars
	CALL   WriteChar
	MOV    AL,' '
	CALL   WriteChar
	RET
printTheSpaces ENDP



intToString PROC USES EAX
; initial condition: Input signed int in EAX
;					 Input radix of int into radix variable
;                    Prints int to console
	MOV    max,0
	XOR    CX,CX
	MOVSX  EBX,radix;<- radix specified here
	CMP    EBX,16
	JE     twos_comp
	CMP    EBX,2
	JNE	   after_twos_comp
twos_comp:
	JMP    loopStart
after_twos_comp:
	CMP    EAX,0
	JGE    loopStart	; checks if int is negative. If so, output negative sign
	PUSH   EAX
	MOV    AL,'-'
	CALL   WriteChar
	INC    max
	POP    EAX
	NEG    EAX			; convert int to positive number
loopStart:
	XOR    EDX,EDX
	IDIV   EBX			; divide EAX with BX, remainder is in EDX
	PUSH   EDX
	INC    CX
	INC    max
	CMP    EAX,0
	JNZ    loopStart
popDigits:
	POP    EAX
	CMP    EAX,9
	JG     notNumerical
	ADD    EAX,30H		; if reaches here SI=0
	JMP    next
notNumerical:
	MOV	   BL,AL
	CALL   numberToLetter
	MOV    AL,BL
next:
	CALL   WriteChar
	DEC    CX
	JNZ    popDigits
	RET
intToString ENDP



radixToInt  PROC
; initial condition: LEA ECX, *byte array variable here*
;                    Returns int value in EAX
;					 SI = 0: valid radix
;					 SI = 1: invalid radix, and EAX returns as 0
	XOR    EAX,EAX
	XOR    DI,DI
	XOR    SI,SI
	MOV    BL,byte ptr [ECX]
	CMP    BL,0					; check if the end of the string
	JZ     notValid
	CALL   checkIfNumerical		; SI=1 : notNumerical / SI=0 : numerical
	CMP    SI,1
	JE     notNumerical
loopStart:						; is numerical
	MOV    BL,byte ptr [ECX]
	CMP    BL,0					; check if the end of the string
	JZ     loopEnd
	SUB    BL,30H
	MOVSX  EBX,BL
	MOV    EDX,10
	IMUL   EDX
	JO     notValid
	ADD    EAX,EBX
	JO     notValid
	INC    ECX
	JMP    loopStart
loopEnd:
	JMP    checkIfValid
notNumerical:
	; can either be valid letter (H/h,X/x) or invalid (everything else)
	XOR	   SI, SI
	CMP    BL,'x'
	JE     endProgram
	CMP    BL,'X'
	JE     endProgram
	CMP    BL,'h'
	JNE    continue
	MOV    EAX,16
	JMP    valid
continue:
	CMP    BL,'H'
	JNE    notValid		
	MOV    EAX,16
	JMP    valid
checkIfValid:
; Checks for valid radices which must be between 2-62
	XOR    SI,SI
	CMP    EAX,2
	JL     notValid
	CMP    EAX,62
	JG     notValid
	JMP    valid
notValid:
	MOV	   EAX,0
	MOV    SI,1
valid:
	RET
radixToInt ENDP



stringToInt PROC
; initial condition: LEA ECX, *byte array variable here*
;                    Returns int value in EAX
;					 SI = 1: overflow
;					 SI = 0: no overflow
;					 Returns int value in EAX
	XOR    EAX,EAX
	XOR    DI,DI
	XOR    SI,SI
loopStart:
	MOV    BL,byte ptr [ECX]
	CMP    BL,0
	JZ     loopEnd
	CMP    BL,'-'
	JNZ    positive
	MOV    DI,1
	INC    ECX
	JMP    loopStart
positive:
	CALL   checkIfNumerical
	CMP    SI,1
	JE     notNumerical
	SUB    BL,30H
notNumerical:
	CALL   letterToNumber
	MOVSX  EBX,BL
	MOVSX  EDX,radix
	IMUL   EDX
	JO     tooMany
	ADD    EAX,EBX
	JO     tooMany
	INC    ECX
	JMP    loopStart
loopEnd:
	XOR	   SI, SI
	CMP    DI,1
	JNE    theEnd
	NEG    EAX
	JMP    theEnd
tooMany:
	MOV    SI,1
theEnd:
	RET
stringToInt ENDP



checkIfNumerical PROC
; checks if char in BL is a number
; SI=1 : notNumerical
; SI=0 : numerical
	XOR    SI,SI
	CMP    BL,30H
	JL     notNumerical
	CMP    BL,39H
	JG     notNumerical
	JMP    numerical
notNumerical:
	MOV    SI,1
numerical:
	RET
checkIfNumerical ENDP



letterToNumber PROC
;Converts letter in BL register to a decimal in BL register
;Converts letters a-z and A-Z
;If character is not a letter, the procedure does nothing
;If character is not a-z or A-Z, then SI is set to 1

	XOR    SI,SI
	CMP    BL,65
	JL	   Invalid
	CMP    BL,90
	JG     aTOz
	SUB    BL,29		;Converts A-Z to decimal equivalent
	JMP	   Done
aTOz: 
	CMP    BL,97
	JL	   Invalid
	CMP    BL,122
	JG	   Invalid
	SUB	   BL,87		;Converts a-z to decimal equivalent
	JMP	   Done
Invalid:	
	MOV	   SI, 1
Done:
	RET
letterToNumber ENDP



numberToLetter PROC
;Converts number in BL register to a ascii code in BL register
;Converts numbers 10-61
;If character is not a number, the procedure does nothing
	CMP    BL,10
	JL	   done
	CMP    BL,35
	JG     aTOz
	ADD    BL,87		;Converts A-Z to decimal equivalent
aTOz: 
	CMP    BL,36
	JL	   done
	CMP    BL,61
	JG	   done
	ADD	BL,29			;Converts a-z to decimal equivalent
done:	
	RET
numberToLetter ENDP



InputErrorCheck PROC
; read in char in string
; convert char to number
; check if number is smaller than radix
; -if number is smaller check next number
; -if number is not smaller fail the check and set DI = 1
; If no input error is found, then DI = 0
	XOR    DI,DI
	MOV    BL,byte ptr [ECX]
	CMP    BL,0
	JZ     loopEnd
	CMP    BL,'-'
	JNE    loopStart
	INC    ECX					; if this executes than char at pos. 0 = '-'
	MOV    BL,byte ptr [ECX]
	CMP    BL,0
	JZ     notLetter			; Throws error if only minus character was inputted to either Var A or B
	JMP	   positive
loopStart:
	MOV    BL,byte ptr [ECX]
	CMP    BL,0
	JZ     loopEnd
positive:
	CALL   checkIfNumerical
	CMP    SI,1					; SI=1 : notValid / SI=0 : valid
	JE     notNumerical
	SUB    BL,30H
	CMP    BL,radixIn
	JNL    notLetter
	INC    ECX					; if this executes char in BL is a number therfore valid
	JMP	   loopStart
notNumerical:
	CALL   letterToNumber
	CMP    SI,1					; SI=1 : notValid / SI=0 : valid
	JE     notLetter
	CMP    BL,radixIn
	JNL    notLetter
	INC    ECX					; if this executes char in BL is a letter therfore valid
	JMP	   loopStart		
notLetter:
	MOV    DI,1
loopEnd:
	RET
InputErrorCheck ENDP



ReadStr PROC
; This procedure reads input from keyboard and writes to a byte array.
; Requires the maximum length of the string in ECX and
; the string OFFSET in EDX (input buffer of the string).
; Returns the length of the string in EAX.

	MOV	   EBX,ECX		; moves max length of string to unused register for later
startLoop:
	JCXZ   endLoop		; ECX contains the length of the string and behaves as the counter
	CALL   ReadChar		; Reads a character from keyboard
	CALL   WriteChar	; Writes the character into console
	CMP    AL,10		; Checks if character is an <enter> char
	JE	   endLoop
	CMP    AL,13		; Checks if character is an <enter> char
	JE	   endLoop
	CMP	   AL, 8
	JE	   delete
	MOV    [EDX],AL		; Moves output of ReadChar to address in EDX
	INC    EDX
	DEC    ECX
	JMP	   startLoop
delete:
	DEC    EDX
	INC    ECX
	MOV	   AL, 32		; Writes blank space to console
	CALL   WriteChar
	MOV	   AL, 8
	CALL   WriteChar
	MOV    AL, 0		; Writes zero to previous char in byte array
	MOV    [EDX],AL
	JMP	   startLoop
endLoop:
	XOR    AL,AL
	MOV    [EDX],AL		; Set the next byte to a zero
	SUB    EBX,ECX		; Subtract number of chars read from max value
	MOV    EAX,EBX		; Move result into EAX
	CALL   CRLF
	RET
ReadStr ENDP

END main