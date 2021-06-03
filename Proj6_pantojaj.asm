TITLE Project 6     (Proj6_pantojaj.asm)

; Author: Jesse Pantoja 
; Last Modified: 
; OSU email address: pantojaj@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number:  6               Due Date: 6/6/2021
; Description: This file is provided as a template from which you may work
;              when developing assembly projects in CS271.

INCLUDE Irvine32.inc

; (insert macro definitions here)
mDisplayString MACRO strAddr
    PUSH    EDX
    MOV     EDX, strAddr
    CALL    WriteString
    POP     EDX
ENDM

mGetString MACRO promptAddr, inputStringAddr, maxStrLength, charsReadAddr
    PUSH    EDX
    PUSH    ECX
    PUSH    EDI
    
    ; Prompt user (string)
    mDisplayString promptAddr
    MOV     ECX, maxStrLength
    MOV     EDX, inputStringAddr
    CALL    ReadString
    ; Copy number of characters read to charsReadAddr variable
    MOV     EDI, charsReadAddr
    MOV     [EDI], EAX

    ; Restore used registers
    POP     EDI
    POP     ECX
    POP     EDX


ENDM

; (insert constant definitions here)
MAX_STRING_LENGTH = 13   ; Max # of digits in a 32 bit SWORD integer is 10. 11 accounts for sign, 12 for null terminator. 13 is used for length validation.
MAX_CHARS_ALLOWED = 11
PLUS_SIGN_ASCII = 43
MINUS_SIGN_ASCII = 45
ZERO_ASCII = 48
NINE_ASCII = 57

.data
    ; Strings
    header1                 BYTE    "PROGRAMMING ASSIGNMENT 6: Designing low-level I/O procedures", 13, 10, 0
    header2                 BYTE    "Written by: Jesse Pantoja", 13, 10, 13, 10, 0
    header3                 BYTE    "Please enter 10 signed decimal integers between -2,147,483,648 and 2,147,483,647.", 13, 10, 0
    header4                 BYTE    "I will then display a list of the entered integers, their sum, and their average value.", 13, 10,
                                    "You may only enter 11 max characters per input.", 13, 10, 13, 10, 0
    promptUser              BYTE    "Please enter a signed number: ", 0
    invalidCharAmount       BYTE    "ERROR: You entered too many characters! Try again.", 13, 10, 0
    invalidCharAmount2      BYTE    "ERROR: You did not enter any characters! Try again.", 13, 10, 0
    invalidIntStr           BYTE    "ERROR: You did not enter a signed decimal number. Try again.", 13, 10, 0 
    validFirstChar          BYTE    "Valid first char", 13, 10, 0
    

    ; Data
    numCharsInputted        DWORD   ?
    userInputString         BYTE    MAX_STRING_LENGTH DUP(?)
    userInputNumericVal     SDWORD  ?
    isNegativeNum           DWORD   0

.code
main PROC

; (insert executable instructions here)
    PUSH    OFFSET header1
    PUSH    OFFSET header2
    PUSH    OFFSET header3
    PUSH    OFFSET header4
    CALL    introduction

    ; Call Read Val x10 
    PUSH    OFFSET isNegativeNum
    PUSH    OFFSET userInputNumericVal
    PUSH    OFFSET invalidIntStr
    PUSH    OFFSET invalidCharAmount2
    PUSH    OFFSET invalidCharAmount
    PUSH    OFFSET numCharsInputted
    PUSH    MAX_STRING_LENGTH
    PUSH    OFFSET userInputString
    PUSH    OFFSET promptUser
    CALL    ReadVal

    ; IT IS NECESSARY TO RESET isNegativeNum back to 0 after each call to ReadVal


    Invoke ExitProcess,0	; exit to operating system
main ENDP

; (insert additional procedures here)

introduction PROC
    ; Create stack frame
    PUSH    EBP
    MOV     EBP, ESP

    mDisplayString [EBP + 20]
    mDisplayString [EBP + 16]
    mDisplayString [EBP + 12]
    mDisplayString [EBP + 8]
    
    POP     EBP
    RET     16
introduction ENDP

ReadVal PROC
    PUSH    EBP
    MOV     EBP, ESP

    ; Save Used Registers
    PUSH    EAX
    PUSH    EBX
    PUSH    ECX
    PUSH    EDX
    PUSH    ESI
    
_getString:
    ; Args: promptUser addr, userInputStringAddr addr, MAX_STRING_LENGTH val, numCharsInputted addr
    mGetString [EBP + 8], [EBP + 12], [EBP + 16], [EBP + 20]
    
    ; Debugging
    ;MOV     EDX, OFFSET userInputString
    ;CALL    WriteString
    ;CALL    CrLf
    ;MOV     EAX, numCharsInputted
    ;CALL    WriteDec
    ;CALL    CrLf

    ; First check if user input string exceeds MAX_CHARS_ALLOWED
    ; [EBP + 20] = numCharsInputted address
    MOV     ESI, [EBP + 20]
    MOV     EBX, [ESI]
    CMP     EBX, MAX_CHARS_ALLOWED
    ; If numCharsInputted < 11, jump to next check, else display error and jump back to mGetString
    JLE     _checkForNullString
    MOV     EDX, [EBP + 24]
    CALL    WriteString
    JMP     _getString

    ; Check if user input is empty, i.e. bytes read = 0. Display error if so. Else, go to next check
_checkForNullString:
    CMP     EBX, 0
    JG      _checkForValidFirstChar
    MOV     EDX, [EBP + 28]
    CALL    WriteString
    JMP     _getString
 
    ; Check that first character is a +, -, or digit character
_checkForValidFirstChar:
    CLD
    ; Change ESI from num of chars inputted to inputStringAddress 
    MOV ESI, [EBP + 12]
    ; Copy first char of userInput into AL
    LODSB

    ; AL must be 43 OR 45 OR (48 <= AL <= 57)
    CMP AL, PLUS_SIGN_ASCII
    ; Check that theres more than 1 char before confirming valid input
    JE  _checkMoreThanOneChar

    CMP AL, MINUS_SIGN_ASCII
    ; Check that theres more than 1 char before confirming valid input
    JE  _checkMoreThanOneChar

    ; If AL val < 48 (0), first char must be invalid, else check if val > 57 (9)
    CMP AL, ZERO_ASCII
    JL  _stringInvalid

    ; If AL val <= NINE_ASCII, then first char at this point is valid
    CMP AL, NINE_ASCII
    JLE _firstCharValid

    ; Output error string and ask for input again
_stringInvalid:
    MOV     EDX, [EBP + 32]
    CALL    WriteString
    JMP     _getString

    ; If first char is a + or - char, we must also check that there is more than one char
_checkMoreThanOneChar:
    ; [EBP + 20] = numCharsInputted Address
    MOV     ECX, [EBP + 20]
    MOV     EBX, [ECX]
    ; If numCharsInputted value = 1, first char is valid but no valid number was inputted. Else, input is valid.
    CMP     EBX, 1
    JE      _stringInvalid
   
_firstCharValid:
    ; Check current val in AL (first val). 
    CMP     AL, PLUS_SIGN_ASCII
    JE      _loopPreconditions

    CMP     AL, MINUS_SIGN_ASCII
    JNE      _firstCharIsDigit
    ; First char is a minus sign, set isNegativeNum to 1
    PUSH    ESI
    PUSH    EDI
    ; [EBP + 40] = isNegativeNum variable address
    MOV     EDI, [EBP + 40]
    MOV     ESI, 1
    ; Set isNegativeNum = 1 because first character is a negative sign
    MOV     [EDI], ESI
    POP     EDI
    POP     ESI
    JMP     _loopPreconditions

  ; Else, first char is a digit. Need to take it into account outside of loop for conversion to an int
  ; Since userInputNumericVal starts up building from 0, the variable will always be set = first digit val (10* 0 + (ascii code - 48))
_firstCharIsDigit:
    PUSH    ESI
    ;[EBP + 36] = userInputNumericVal address, save address in ESI
    MOV     ESI, [EBP + 36]
    SUB     AL, 48
    MOV     [ESI], EAX
    POP     ESI

_loopPreconditions:
    PUSH    ESI
    MOV     ESI, [EBP + 20]
    MOV     ECX, [ESI]
    ; ECX should equal numCharsInputted - 1 as first character already accounted for
    DEC     ECX
    MOV     EBX, [ESI]
    ; If numCharsInputted >= 10, we will need to stop at second to last digit to determine if final number fits in 32 bit reg
    POP     ESI
    CMP     EBX, 10
    JL      _startLoop
    DEC     ECX
    
_startLoop:
    LODSB
    ; precondition to first check that current char is a digit. Break out if not. 
    ; If AL val < 48 (0), first char must be invalid, else check if val > 57 (9)
    CMP AL, ZERO_ASCII
    JL  _stringInvalid

    ; If AL val <= NINE_ASCII, then first char at this point is valid
    CMP AL, NINE_ASCII
    JG _stringInvalid

    ; User formula userNumericVal = 10 * userNumericVal + (ASCII CODE - 48)
    PUSH    ESI
    PUSH    EDI
    PUSH    EBX

    ; [EBP + 36] = userNumericVal address, save address in ESI
    MOV     ESI, [EBP + 36]
    ; Move current userNumericVal immediate val into EDI
    MOV     EDI, [ESI]
    ; Move 10 into EAX to prep 10*userNumericVal (this move replaces old ASCII value in EAX, must restore before using ASCII val again)
    PUSH    EAX
    MOV     EAX, 10
    ; Result in EAX
    MUL     EDI
    ; Save 10*userNumericVal from EAX to EBX
    MOV     EBX, EAX
    ; Current character's ASCII code - 48
    POP     EAX
    SUB     AL, 48
    ; Final result in EBX, save this new value in userNumericVal
    ADD     EBX, EAX
    MOV     [ESI], EBX
    POP     EBX
    POP     EDI
    POP     ESI
    
    LOOP    _startLoop



        ; a condition needs to exist after loop to check the sign of the number so that if it was negative input, final num should be negative. 




ReadVal ENDP


END main
