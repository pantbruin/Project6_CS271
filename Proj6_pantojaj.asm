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
NEGATIVE = 1
POSITIVE = 0

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
    numbersInputtedStr      BYTE    "You entered the following numbers:", 13, 10, 0
    sumOfNumbersStr         BYTE    "The sum of these numbers is: ", 0
    roundedAverageStr       BYTE    "The rounded average is: ", 0
    commaSpacing            BYTE    ", ", 0
    farewellStr             BYTE    "Thanks for playing!", 0
    overflowString          BYTE    "The number you entered is too large for a 32 bit register.", 13, 10, 0

    

    ; Data
    numCharsInputted        DWORD   ?
    userInputString         BYTE    MAX_STRING_LENGTH DUP(?)
    userInputNumericVal     SDWORD  ?
    isNegativeNum           DWORD   0
    totalSum                SDWORD  0
    writevalOutputString    BYTE    11 DUP(0)
    commaCounter            BYTE    0
    roundedAverageInt       SDWORD  ?
    

    ; Array
    userIntegersArray       SDWORD  11 DUP(?)

.code
main PROC

; (insert executable instructions here)
    PUSH    OFFSET header1
    PUSH    OFFSET header2
    PUSH    OFFSET header3
    PUSH    OFFSET header4
    CALL    introduction

    MOV     ECX, 10
    MOV     ESI, OFFSET userIntegersArray

_getIntegers:
    ; Call Read Val x10 
    PUSH    OFFSET overflowString
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

    ; ReadVal returns value in userInputNumericVal, move it into current array address
    MOV     EBX, userInputNumericVal
    MOV     [ESI], EBX
    ADD     ESI, TYPE userIntegersArray
    
    LOOP    _getIntegers

    ; Calculate sum of the obtained integers in array
    PUSH    OFFSET totalSum
    PUSH    OFFSET userIntegersArray
    CALL    calculateSum

    ; Display "You entered following numbers:" string
    CALL    CrLf
    CALL    CrLf
    mDisplayString OFFSET numbersInputtedStr

    ; Call WriteVal 10 times, each time pushing a value from userIntegersArray. For debugging, only do first val for now
    ; Reset isNegativeNum to false
    MOV     ESI, OFFSET userIntegersArray
    MOV     ECX, 10

_displayIntsAsStrings:
    PUSH    SIZEOF writevalOutputString
    PUSH    OFFSET isNegativeNum
    PUSH    OFFSET writevalOutputString
    ; Push VALUE at current address of userIntegersArray
    PUSH    [ESI]
    CALL    WriteVal
    ADD     ESI, 4

    CMP     commaCounter, 9
    JE      _writevalLoopInstruction
    mDisplayString OFFSET commaSpacing
    INC     commaCounter
_writevalLoopInstruction:
    LOOP    _displayIntsAsStrings


    ; Output sum strings
    CALL    CrLf
    CALL    CrLf
    mDisplayString OFFSET sumOfNumbersStr

    PUSH    SIZEOF writevalOutputString
    PUSH    OFFSET isNegativeNum
    PUSH    OFFSET writevalOutputString
    PUSH    totalSum
    CALL    WriteVal

    ; Calculate rounded average
    PUSH    OFFSET totalSum
    PUSH    OFFSET roundedAverageInt
    CALL    calculateRoundedAverage

    ; Display rounded average
    CALL    CrLf
    CALL    CrLf
    mDisplayString OFFSET roundedAverageStr
    
    PUSH    SIZEOF writevalOutputString
    PUSH    OFFSET isNegativeNum
    PUSH    OFFSET writevalOutputString
    PUSH    roundedAverageInt
    CALL    WriteVal

    ; Display farewell
    CALL    CrLf
    CALL    CrLf
    mDisplayString OFFSET farewellStr



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
    ; Reset isNegativeNum variable to positive
    MOV     EDI, [EBP + 40]
    MOV     EBX, 0
    MOV     [EDI], EBX
    ; Reset userNumericVal to 0
    MOV     EDI, [EBP + 36]
    MOV     EBX, 0
    MOV     [EDI], EBX

    mGetString [EBP + 8], [EBP + 12], [EBP + 16], [EBP + 20]

    ; First check if user input string exceeds MAX_CHARS_ALLOWED
    ; [EBP + 20] = numCharsInputted address
    MOV     ESI, [EBP + 20]
    MOV     EBX, [ESI]
    CMP     EBX, MAX_CHARS_ALLOWED
    ; If numCharsInputted < 11, jump to next check, else display error and jump back to mGetString
    JLE     _checkForNullString
    mDisplayString [EBP + 24]
    JMP     _getString

    ; Check if user input is empty, i.e. bytes read = 0. Display error if so. Else, go to next check
_checkForNullString:
    CMP     EBX, 0
    JG      _checkForValidFirstChar
    mDisplayString [EBP + 28]
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
    mDisplayString [EBP + 32]
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
    ; [EBP + 20] = numCharsInputted Data address
    MOV     ESI, [EBP + 20]
    MOV     ECX, [ESI]
    ; ECX should equal numCharsInputted - 1 as first character already accounted for
    DEC     ECX
    MOV     EBX, [ESI]
    ; If numCharsInputted >= 10, we will need to stop at second to last digit to determine if final number fits in 32 bit reg
    ; First restore ESI (pointing to the next character to evaluate in user string) as we no longer need numCharsInputted
    POP     ESI
    CMP     EBX, 1
    JE      _skipLoop
    CMP     EBX, 10
    JL      _startLoop
    ; Loop up to second to last digit 
    DEC     ECX
    
_startLoop:
    LODSB
    ; precondition to first check that current char is a digit. Break out and display error if not. 
    ; If AL val < 48 (0), current char is invalid, else check if val > 57 (9)
    CMP AL, ZERO_ASCII
    JL  _stringInvalid

    ; If AL val > NINE_ASCII, then current char is invalid
    CMP AL, NINE_ASCII
    JG _stringInvalid

    ;  Formula: userNumericVal = 10 * userNumericVal + (ASCII CODE - 48)
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
    ; Current character's ASCII code - 48. Restore EAX first
    POP     EAX
    SUB     AL, 48
    ; Final result in EBX, save this new value in userNumericVal
    ADD     EBX, EAX
    MOV     [ESI], EBX
    POP     EBX
    POP     EDI
    POP     ESI

    LOOP    _startLoop

_skipLoop:
    ; DETERMINE IF RUNNING VALUE NEEDS TO BE NEGATED
    ; PUSH    EBX unecessary because old ebx value is numCharsinputted, useless
    PUSH    ESI
    ; [EBP + 40] = isNegativeNum address
    MOV     ESI, [EBP + 40]
    MOV     EBX, [ESI]
    ; Restore ESI, as we've obtained isNegativeNum value already
    POP     ESI
    CMP     EBX, POSITIVE
    JE      _skipNegating
    ; Negate running value
    PUSH    EDI
    ; Move userNumericVal address into EDI
    MOV     EDI, [EBP + 36]
    ; Move [EDI] value into EBX, negate EBX, then store negated EBX value in EDI
    MOV     EBX, [EDI]
    NEG     EBX
    MOV     [EDI], EBX
    ; EDI only gets pushed when we need to negate, so only pop in negation block
    POP     EDI

_skipNegating:
    ; Restore EBX here from its push after loop end
    ; POP     EBX from line 352 push

    ; Next compare numOfCharsInputted by user >= 10 to check if final value fits in 32 bit reg.
    ; Skip and exit procedure if not. 
    ; PUSH    EBX unecessary push
    PUSH    ESI
    ; [EBP + 20] = numOfCharsInputted
    MOV     ESI, [EBP + 20]
    MOV     EBX, [ESI]
    ; ESI address value extracted and no longer needed
    POP     ESI
    ; Exit procedure if numCharsInputted < 10 as these numbers will always fit in 32 bit reg
    CMP     EBX, 10
    JL      _exitProcedure
    ; Else compute last digit on its own. 
    LODSB
    ; precondition to first check that current char is a digit. Break out and display error if not. 
    ; If AL val < 48 (0), current char is invalid, else check if val > 57 (9)
    CMP AL, ZERO_ASCII
    JL  _stringInvalid

    ; If AL val > NINE_ASCII, then current char is invalid
    CMP AL, NINE_ASCII
    JG _stringInvalid

    ;  Formula: userNumericVal = 10 * userNumericVal + (ASCII CODE - 48)
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
    IMUL     EDI
    ; Jump to stringInvalid if 10*userNumericVal triggers overflow
    JO      _overflowAtMultiplication

    ; Else Save 10*userNumericVal from EAX to EBX
    MOV     EBX, EAX
    ; Current character's ASCII code - 48. Restore EAX first
    POP     EAX
    SUB     AL, 48

    ; Determine if last digit (currently in AL) needs to be added or subtracted (is userNumericVal negative or positive?)
    PUSH    ESI
    PUSH    EDI
    ; [EBP + 40] = isNegativeNum boolean
    MOV     ESI, [EBP + 40]
    MOV     EDI, [ESI]
    CMP     EDI, POSITIVE
    JE      _skipNegatingLastDigit
    NEG     EAX

    ; Final result in EBX, save this new value in userNumericVal
_skipNegatingLastDigit:
    ; Restore registers from block starting line 351
    POP     EDI
    POP     ESI

    ADD     EBX, EAX
    ; Jump to stringInvalid if (10*userNumericVal) + integer triggers overflow
    JO      _overflowAtAddition
    MOV     [ESI], EBX
    POP     EBX
    POP     EDI
    POP     ESI

_exitProcedure: 
    ; Reset isNegativeNum to 0, it's original parameter value
    MOV     ESI, [EBP + 40]
    MOV     EBX, 0
    MOV     [ESI], EBX

    ; Clean up stack
    POP     ESI
    POP     EDX
    POP     ECX
    POP     EBX
    POP     EAX
    POP     EBP
    RET     36

_overflowAtMultiplication:
    ; Realign stack
    POP     EAX
    POP     EBX
    POP     EDI
    POP     ESI
    mDisplayString  [EBP + 44]
    JMP     _getString

_overflowAtAddition:
    ; Realign stack
    POP     EBX
    POP     EDI
    POP     ESI
    mDisplayString [EBP + 44]
    JMP     _getString

ReadVal ENDP


calculateSum    PROC
    PUSH    EBP
    MOV     EBP, ESP

    PUSH    ESI
    PUSH    EDI
    PUSH    ECX
    PUSH    EBX
    PUSH    EAX

    ; Loop through every integer (x10) in array
    MOV     ECX, 10

    ; LOOP PRECONDITIONS
    ; Move array address into ESI (array address at [EBP + 8]
    MOV     ESI, [EBP + 8]
    ; Move totalSum data variable address into EDI
    MOV     EDI, [EBP + 12]
    ; Loop 10 times for 10 integers in array
    MOV     ECX, 10

_startLoop:
    ; Move current array immediate val into EBX
    MOV     EBX, [ESI]
    ; Extract current running sum into EDX
    MOV     EAX, [EDI]
    ; Add array immediate to current running sum. Must be current running sum + array value as array value could be negative
    ADD     EAX, EBX
    ; Restore new sum into [EDI], which points to running sum address
    MOV     [EDI], EAX
    ; Increment array pointer by SDWORD type = 4
    ADD     ESI, 4
       
    LOOP _startLoop

    POP     EAX
    POP     EBX
    POP     ECX
    POP     EDI
    POP     ESI
    POP     EBP

    RET     8

calculateSum    ENDP

WriteVal    PROC
    PUSH    EBP
    MOV     EBP, ESP

    PUSH    ESI
    PUSH    EAX
    PUSH    EBX
    PUSH    EDX
    PUSH    EDI

    ; Determine if passed integer to convert [EBP + 8] is negative
    ; EAX becomes dividend at this line
    MOV     EAX, [EBP + 8]
    CMP     EAX, 0
    JNS     _loopPreconditions
    ; Convert to positive number and set isNegativeNum to true (1)
    NEG     EAX
    MOV     ESI, [EBP + 16]
    MOV     EBX, 1
    MOV     [ESI], EBX
    ; ESI and EBX register current values not needed after here. 
    
_loopPreconditions:
    ; Point EDI to second to last element in userInputStringVal. Last BYTE will always be numm term 0. Loop will insert bytes in reverse starting at 10th element
    MOV     EDI, [EBP + 12]
    ADD     EDI, 9
    ; Use post-test loop to sequentially divide integer. Append ASCII codes to output string in REVERSE. 
_startLoop:
    ; isNegativeNum stays = 0, positive dividend in EAX

    ; Clear EDX
    CDQ
    ; Divisor always 10
    MOV     EBX, 10
    IDIV    EBX

    ; Remainder in EDX, = the furthest right digit from dividend
    ; Convert value in remainder to its corresponding ASCII code.
    ADD     EDX, 48

    PUSH    EAX
    MOV     EAX, EDX
    STD
    STOSB
    POP     EAX

     ; Post test will CMP EAX, 0 after dividing. If quotient (EAX) = 0, then we've reached the last digit
    CMP     EAX, 0
    JNZ     _startLoop

    ; At end of loop, need to check isNegativeNum variable. If negative, we need to add final MINUS SIGN ascii to beginning
    MOV     ESI, [EBP + 16]
    MOV     EBX, [ESI]
    CMP     EBX, POSITIVE
    JE      _exitProcedure
    ; Add minus sign ascii
    MOV     EAX, MINUS_SIGN_ASCII
    STOSB

_exitProcedure:
    ; Reset isNegativeNum Variable to 0 as it originally was
    MOV     EBX, 0
    MOV     [ESI], EBX

    ; Increment EDI pointer by 1 byte to point to first ASCII character
    ADD     EDI, 1

    ; EDI holds address of first address 
    mDisplayString EDI

    POP    EDI
    POP    EDX
    POP    EBX
    POP    EAX
    POP    ESI
    POP    EBP
    RET     16


WriteVal    ENDP

calculateRoundedAverage     PROC
    PUSH    EBP
    MOV     EBP, ESP

    PUSH    EAX
    PUSH    EDX
    PUSH    EBX
    PUSH    ESI
    PUSH    EDI

    ; [EBP + 8] = roundedAverage SDWORD address
    MOV     EDI, [EBP + 8]

    ; [EBP + 12] = totalSum Address, the dividend
    MOV     ESI, [EBP + 12]
    MOV     EAX, [ESI]

    ; Dividend = 10 for 10 total integers
    MOV     EBX, 10

    ; Clear EDX
    CDQ
    IDIV    EBX
    
    ; Quotient in EAX
    MOV     [EDI], EAX

    POP     EDI
    POP     ESI
    POP     EBX
    POP     EDX
    POP     EAX
    POP     EBP
    RET     8


calculateRoundedAverage     ENDP

END main
