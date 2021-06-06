TITLE Project 6     (Proj6_pantojaj.asm)

; Author: Jesse Pantoja 
; Last Modified: 6/6/2021
; OSU email address: pantojaj@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number:  6               Due Date: 6/6/2021
; Description: Includes a program that asks the user for 10 signed numbers that can fit inside a 32 bit register as strings. 
; The program then converts the strings to their integer SDWORD data type form, stores them, calculates the sum, and rounded
; average. The program then converts the SDWORD data type integers, sum, and rounded average to strings. Finally,
; a list of the numbers the user entered, the sum of those numbers, and their rounded average are outputted as strings.

INCLUDE Irvine32.inc

; --------------------------------------------------------------------------------------------------
; Name: mDisplayString
;
; Writes a passed string (from address) to terminal.
;
; Preconditions: Do not use EDX as arguments
;
; Receives:
;       strAddr = the starting address of the string to output
; 
; --------------------------------------------------------------------------------------------------
mDisplayString MACRO strAddr
    PUSH    EDX
    MOV     EDX, strAddr
    CALL    WriteString
    POP     EDX
ENDM


; --------------------------------------------------------------------------------------------------
; Name: mGetString
;
; Prompts the user to enter an integer as a string, then reads and saves the input string to memory. Uses Irvine
; library ReadString 
;
; Preconditions: Do not use EDX, ECX, or EDI as arguments
;
; Receives:
;       promptAddr = The address of the string giving instructions to the user to enter a number
;       inputStringAddr = The address to save the user's input to. 
;       maxStrLength = an immediate value denoting the max number of bytes to read from the user
;       charsReadAddr = the address of a data variable to save the number of bytes read from the user's input.
;
; --------------------------------------------------------------------------------------------------
mGetString MACRO promptAddr, inputStringAddr, maxStrLength, charsReadAddr
    ; Save used registers
    PUSH    EDX
    PUSH    ECX
    PUSH    EDI
    
    ; Prompt user (string)
    mDisplayString promptAddr
    
    ; ReadString preconditions
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

MAX_STRING_LENGTH = 13   ; Max # of digits in a 32 bit SWORD integer is 10. 11 accounts for sign, 12 for null terminator. 13 is used for length validation.
MAX_CHARS_ALLOWED = 11
PLUS_SIGN_ASCII = 43
MINUS_SIGN_ASCII = 45
ZERO_ASCII = 48
NINE_ASCII = 57
NEGATIVE = 1
POSITIVE = 0

.data
    ; Output strings
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

    PUSH    OFFSET header1
    PUSH    OFFSET header2
    PUSH    OFFSET header3
    PUSH    OFFSET header4
    CALL    introduction

; -----------------------------------------------------------
; Calls ReadVal ten times. Each call to ReadVal asks for string input
; from the user and returns an integer in userNumericVal. Once procedure returns,
; the value returned is appended to userIntegersArray.
;
; -----------------------------------------------------------
    ; _getIntegers loop preconditions
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

; -----------------------------------------------------------
;  Calculate the sum of the 10 saved values in userIntegersArray
; (saved in totalSum)
; -----------------------------------------------------------

    ; Calculate sum of the obtained integers in array
    PUSH    OFFSET totalSum
    PUSH    OFFSET userIntegersArray
    CALL    calculateSum

; -----------------------------------------------------------
; Output to the terminal each SDWORD integer in userIntegersArray
; to its string form using WriteVal. Because there are 10 integers in the array,
; call WriteVal 10 times.
;
; -----------------------------------------------------------

    ; Display "You entered following numbers:" string
    CALL    CrLf
    CALL    CrLf
    mDisplayString OFFSET numbersInputtedStr

    ; Call WriteVal 10 times, each time pushing a value from userIntegersArray. 
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

; -----------------------------------------------------------
; Using WriteVal, output the totalSum SDWORD integer value in its string
; representation obtained; by calling calculateSum above. Then calculate 
; the rouned average and output that SDWORD integer value in its string representation
; using WriteVal as well.
;
; -----------------------------------------------------------

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

; -----------------------------------------------------------
; Display a farewell message.
;
; -----------------------------------------------------------

    ; Display farewell
    CALL    CrLf
    CALL    CrLf
    mDisplayString OFFSET farewellStr

    Invoke ExitProcess,0	; exit to operating system
main ENDP

; --------------------------------------------------------------------------------------------------
; Name: introduction
;
; Outputs program header and instructions for user strings to the terminal. Uses mDisplayString macro
; to output strings.
;
; Receives:
;    [EBP + 20] = header1 address. Program title string.
;    [EBP + 16] = header2 address. "Programmed by.." string 
;    [EBP + 12] = header3 address. String describing what the user should be inputting
;    [EBP + 8] = header4 address. String describing what the program will output
;
; --------------------------------------------------------------------------------------------------
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

; --------------------------------------------------------------------------------------------------
; Name: ReadVal
;
; Uses mGetString macro to get a single, valid signed number (as a string) from the user. The procedure
; will continuously ask for input until valid input is entered. Valid input are strings that start with
; a digit, +, or -; strings that fit inside a 32 bit register; strings with no other characters but
; digits after the first character; non-empty strings. ReadVal will takes a valid input (as a string) and
; convert it to an SDWORD integer and outputs that integer into SDWORD userNumericVal.
; 
; Postconditions: None. All registers are restored before and after procedure call.
;
; Receives:
;    [EBP + 44] = overflowString. Address of string to output to terminal when user entered a number to big for a 32bit reg.
;    [EBP + 40] = isNegativeNum boolean address. input/output param to denote if the user's input is a negative number. 
;    [EBP + 36] = userInputNumericVal data output parameter. When ReadVal converts a valid input to a SDWORD integer,
;                 it outputs the value at this address. 
;    [EBP + 32] = invalidIntStr. Address of string to output to terminal when user did not enter a signed number.
;    [EBP + 28] = invalidCharAmount2. Address of string to output to terminal when inputted string is empty.
;    [EBP + 24] = invalidCharAmount. Address of string to output to terminal when inputted string has too many characters.
;    [EBP + 20] = numCharsInputted data address as output param. Used as arg for mGetString and later for string validation. 
;                 numCharsInputted value is the integer number of characters inputted by the user.
;    [EBP + 16] = MAX_STRING_LENGTH immediate value. Used as mGetString argument and for string validation.
;    [EBP + 12] = userInputString data output address. mGetString receives this address to write the user's string input at said address.
;    [EBP + 8] = promptUser string address. String outputted to prompt user to enter a signed number.
;    MAX_CHARS_ALLOWED, ZERO_ASCII, NINE_ASCII, PLUS_SIGN_ASCII, MINUS_SIGN_ASCII, POSITIVE are global constants.
;
;   Returns:
;       userInputString: Data variable always modified for every call to ReadVal. Used as an input/output parameter
;                       to save the user's inputted string as an array of bytes. Every call to ReadVal overwrites previous strings in the variable, if any. 
;       numCharsInputted: Data variable always modified for every call to ReadVal. Used as an input/output parameter
;                         to save the number of characters the user inputted when mGetString macro is invoked within procedure.
;                         Every call to ReadVal overwrites previous value, if any. 
;       userInputNumericVal: ReadVal's primary output. This variable will hold the integer SDWORD type after ReadVal
;                           converts the user's string to integer. 
;       isNegativeNum: May be modified in a ReadVal call, but is always restored before procedure returns. 
;
;
; --------------------------------------------------------------------------------------------------
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
    ; Reset isNegativeNum variable to positive
    MOV     EDI, [EBP + 40]
    MOV     EBX, 0
    MOV     [EDI], EBX
    ; Reset userNumericVal to 0
    MOV     EDI, [EBP + 36]
    MOV     EBX, 0
    MOV     [EDI], EBX

    ; Args: promptUser addr, userInputStringAddr addr, MAX_STRING_LENGTH val, numCharsInputted addr
    mGetString [EBP + 8], [EBP + 12], [EBP + 16], [EBP + 20]

    ; First check if user input string exceeds MAX_CHARS_ALLOWED
    ; [EBP + 20] = numCharsInputted address
    MOV     ESI, [EBP + 20]
    MOV     EBX, [ESI]
    CMP     EBX, MAX_CHARS_ALLOWED
    ; If numCharsInputted < 11, jump to next check, else display error and ask for another input.
    JLE     _checkForNullString
    mDisplayString [EBP + 24]
    JMP     _getString

    ; Check if user input is empty, i.e. bytes read = 0. Display error if so. Else, go to next check
_checkForNullString:
    CMP     EBX, 0
    JG      _checkForValidFirstChar
    mDisplayString [EBP + 28]
    JMP     _getString
 
    ; Check that first character is valid. I.e. a +, -, or digit character. Use LODSB from beginning of string
_checkForValidFirstChar:
    CLD
    ; Change ESI from num of chars inputted to inputStringAddress 
    MOV     ESI, [EBP + 12]
    ; Copy first char of userInput into AL
    LODSB

    ; AL byte value must be ASCIIs 43 OR 45 OR (48 <= AL <= 57)
    CMP     AL, PLUS_SIGN_ASCII
    ; If first char is plus sign, check that theres more than 1 char before confirming valid input
    JE      _checkMoreThanOneChar

    CMP     AL, MINUS_SIGN_ASCII
    ; If first char is minus sign, check that theres more than 1 char before confirming valid input
    JE      _checkMoreThanOneChar

    ; If AL val < 48 (ASCII for 0), first char must be invalid, else check if val > 57 (ASCII for 9)
    CMP     AL, ZERO_ASCII
    JL      _stringInvalid

    ; If AL val <= NINE_ASCII, then first char must be valid at this point
    CMP     AL, NINE_ASCII
    JLE     _firstCharValid

    ; Output error string and ask for input again
_stringInvalid:
    mDisplayString [EBP + 32]
    JMP     _getString

    ; If first char is a + or - char, we must also check that there is more than one char
_checkMoreThanOneChar:
    ; [EBP + 20] = numCharsInputted Address
    MOV     ECX, [EBP + 20]
    MOV     EBX, [ECX]
    ; If numCharsInputted value = 1 and char is + or -, first char is valid but no valid number was inputted. Else, input is valid.
    CMP     EBX, 1
    JE      _stringInvalid
   
   ; First character is valid and/or there is more than one character.
_firstCharValid:
    ; Assess first character outside of loop. 
    ; If plus sign or minus sign, we don't have to convert to its SDWORD value. Go to next character
    CMP     AL, PLUS_SIGN_ASCII
    JE      _loopPreconditions

    ; If first char is a minus sign, then set isNegativeNum. 
    CMP     AL, MINUS_SIGN_ASCII
    ; Else, first character is a digit, process this number outside of loop.
    JNE      _firstCharIsDigit

    PUSH    ESI
    PUSH    EDI
    ; [EBP + 40] = isNegativeNum variable address
    MOV     EDI, [EBP + 40]
    MOV     ESI, 1
    ; Set isNegativeNum = 1
    MOV     [EDI], ESI
    POP     EDI
    POP     ESI
    JMP     _loopPreconditions

  ; Else, first char is a digit. Need to take it into account outside of loop for conversion to an int
  ; Since userInputNumericVal starts up building from 0, the variable will always be set equal to whatever the first digit is. (10* 0 + (ascii code - 48))
_firstCharIsDigit:
    PUSH    ESI
    ;[EBP + 36] = userInputNumericVal address, save address in ESI
    MOV     ESI, [EBP + 36]
    ; Convert from ASCII to integer
    SUB     AL, 48
    MOV     [ESI], EAX
    POP     ESI

    ; Set loop counters based on the number of characters inputted.
_loopPreconditions:
    PUSH    ESI
    ; [EBP + 20] = numCharsInputted Data address
    MOV     ESI, [EBP + 20]
    MOV     ECX, [ESI]
    ; ECX should always start off at numCharsInputted - 1 as first character already accounted for above.
    DEC     ECX
    MOV     EBX, [ESI]
    ; Restore ESI (to point to the next character to evaluate in user string). No longer need numCharsInputted
    POP     ESI

    ; If EBX holds the numCharsInputted value. If numCharsInputted = 1, then skip loop as integer is single digit.
    CMP     EBX, 1
    JE      _skipLoop
    
    ; If numCharsInputted >= 10, loop to second to last character digit to determine if final digit will fit in a 32 bit reg
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
    ; Final result in EBX, output this value in userNumericVal
    ADD     EBX, EAX
    MOV     [ESI], EBX
    POP     EBX
    POP     EDI
    POP     ESI

    LOOP    _startLoop

    ; Jumped to here when numCharsInputted is 1.
_skipLoop:
    ; userNumericVal always first computed as positive value. Convert to negative integer if user inputted negative value here
    PUSH    ESI
    ; [EBP + 40] = isNegativeNum address
    MOV     ESI, [EBP + 40]
    MOV     EBX, [ESI]
    ; Restore ESI, as we've obtained isNegativeNum value already
    POP     ESI
    
    ; If isNegativeNum is negative, negate userNumericVal. Else skip negating
    CMP     EBX, POSITIVE
    JE      _skipNegating
    ; Negate running value in userNumericVal
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
    ; Next compare numOfCharsInputted by user >= 10 to check if final value fits in 32 bit reg.
    ; Skip and exit procedure if < 10
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
    ; Precondition to first check that last char is a digit. Break out and display error if not. 
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
    ; If overflow flag is set, then number is too big for 32 bit. Ask for another number.
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

_skipNegatingLastDigit:
    ; Restore registers from line 527
    POP     EDI
    POP     ESI

    ; Last multiplication operation result was saved in EBX. 
    ADD     EBX, EAX
    ; If addition step triggers overflow flag, number is too big to fit in 32 bit reg. Ask for new input.
    JO      _overflowAtAddition
    ; Else save final value in userNumericVal
    MOV     [ESI], EBX
    POP     EBX
    POP     EDI
    POP     ESI

_exitProcedure: 
    ; Reset isNegativeNum to 0, it's original parameter value before procedure exit.
    MOV     ESI, [EBP + 40]
    MOV     EBX, 0
    MOV     [ESI], EBX

    ; Clean up stack from inital registers saved at start of procedure.
    POP     ESI
    POP     EDX
    POP     ECX
    POP     EBX
    POP     EAX
    POP     EBP
    RET     36

_overflowAtMultiplication:
    ; Realign stack before asking for a new input.
    POP     EAX
    POP     EBX
    POP     EDI
    POP     ESI
    mDisplayString  [EBP + 44]
    JMP     _getString

_overflowAtAddition:
    ; Realign stack before asking for a new input.
    POP     EBX
    POP     EDI
    POP     ESI
    mDisplayString [EBP + 44]
    JMP     _getString

ReadVal ENDP

; --------------------------------------------------------------------------------------------------
; Name: calculateSum
; 
; Calculates the total sum of the values in userIntegersArray. 
;
; Preconditions: Requires that userIntegersArray is filled with 10 integers by calling ReadVal multiple times.
;
; Receives:
;       [EBP + 12] = totalSum data adress. Used as an input/output parameter to store the total sum of the integers in userIntegersArray
;       [EBP + 8] = userIntegersArray address. Used as an input parameter to extract the values in the array to calculate the sum.
;
; Returns:
;       totalSum: upon procedure return, the total sum of the values in the userIntegersArray is stored in this variable.
;
; --------------------------------------------------------------------------------------------------
calculateSum    PROC
    PUSH    EBP
    MOV     EBP, ESP

    ; Save used registers
    PUSH    ESI
    PUSH    EDI
    PUSH    ECX
    PUSH    EBX
    PUSH    EAX

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

    ; Restore registers before procedure return
    POP     EAX
    POP     EBX
    POP     ECX
    POP     EDI
    POP     ESI
    POP     EBP
    RET     8

calculateSum    ENDP

; --------------------------------------------------------------------------------------------------
; Name: WriteVal
;
; WriteVal takes an immediate integer value as a parameter, converts that value into its string representation,
; and outputs that string representation to the terminal. The procedure breaks down the integer value starting
; from the ones place and moves left to higher powers. Thus, the string representation is written in reverse into
; the output (writevalOutputString). 
;
; Preconditions: writevalOutputString should be initially declared in the data section as an 11 element byte array filled with 0's
;
;
; Receives:
;       [EBP + 20] =  SIZEOF writevalOutputString. 
;       [EBP + 16] = isNegativeNum address. Used as a boolean to track if the passed value to convert is negative
;       [EBP + 12] = writevalOutputString address. Output parameter where the final string representation
;                   of the integer to be converted will be stored.
;       [EBP + 8] = an immediate SDWORD value to convert to its string representation
;       MINUS_SIGN_ASCII and POSITIVE, global constants
;
; Returns:
;       writevalOutputString: modified and not restored. Towards end of the procedure, mDisplayString is used
;       to display the string that WriteVal outputted to writevalOutputString. Upon procedure return, writevalOutputString is not restored.
; --------------------------------------------------------------------------------------------------
WriteVal    PROC
    PUSH    EBP
    MOV     EBP, ESP

    ; Save used registers
    PUSH    ESI
    PUSH    EAX
    PUSH    EBX
    PUSH    EDX
    PUSH    EDI

    ; Determine if passed integer (immediate) [EBP + 8] to convert to string is negative. If negative, convert to positive for easier processing
    MOV     EAX, [EBP + 8]      ; EAX contains starting dividend for loop at this line
    CMP     EAX, 0
    JNS     _loopPreconditions
    ; Convert to positive number and set isNegativeNum to true (1)
    NEG     EAX
    ; Set isNegativeNum to 1
    MOV     ESI, [EBP + 16]
    MOV     EBX, 1
    MOV     [ESI], EBX
    ; ESI and EBX register current values not needed after here. 

    ; Also check if passed integer is minimum value in 32 bit register (-2,147,483,648) edge case (negating will cause overflow only for this input)
    JNO     _loopPreconditions
    ; Address edge case by processing last digit individually, outside of loop. 
    ; INC EAX to -2147483647 so we can negate without an overflow
    INC     EAX
    NEG     EAX

    ; Point EDI to second to last element in userInputStringVal. Last BYTE will always be null term 0. Loop will insert bytes in reverse starting at 10th element
    MOV     EDI, [EBP + 12]
    ADD     EDI, 9

    ; Clear EDX
    CDQ
    ; Divisor always 10
    MOV     EBX, 10
    IDIV    EBX

    ; Remainder in EDX, = the furthest right digit from dividend
    ; Convert value in remainder to its corresponding ASCII code. +49 for this case because we want original number 8 not 7.
    ADD     EDX, 49

    PUSH    EAX
    MOV     EAX, EDX
    STD
    STOSB
    POP     EAX
    ; Skip non-edge case loopPreconditions, as preconditions already established at beginning of edge case block above
    JMP     _startLoop
    
_loopPreconditions:
    ; Point EDI to second to last element in userInputStringVal. Last BYTE will always be null term 0. Loop will insert bytes in reverse starting at 10th element
    MOV     EDI, [EBP + 12]
    ADD     EDI, 9

    ; Use post-test loop to sequentially divide integer. Append ASCII codes to output string in REVERSE. 
_startLoop:
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

     ; Post test will CMP EAX, 0 after dividing. If quotient (EAX) = 0, then we've processed the last digit and done
    CMP     EAX, 0
    JNZ     _startLoop

    ; At end of loop, need to check isNegativeNum variable. If negative, we need to add final MINUS SIGN ascii to beginning of string
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

    ; Increment EDI pointer by 1 byte to point to first ASCII character, as last STOSB decremented EDI. 
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

; --------------------------------------------------------------------------------------------------
; Name: calculateRoundedAverage
;
; Calculates the rounded average of the integers obtained from ReadVal being called 10 times.
;
; Preconditions: Requires that totalSum be calculated already. 
; 
; Receives:
;       [EBP + 12] = totalSum data address. Used as input to calculate rounded average.
;       [EBP + 8] = roundedAverageInt output parameter address. 
;
; Returns:
;       roundedAverageInt: the calculated rounded average is stored in this variable.
;
; --------------------------------------------------------------------------------------------------
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
    
    ; Quotient in EAX. Save result in roundedAverage variable
    MOV     [EDI], EAX

    ; Restore registers
    POP     EDI
    POP     ESI
    POP     EBX
    POP     EDX
    POP     EAX
    POP     EBP
    RET     8

calculateRoundedAverage     ENDP

END main
