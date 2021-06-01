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

; (insert constant definitions here)

.data

    ; Strings
    header1     BYTE    "PROGRAMMING ASSIGNMENT 6: Designing low-level I/O procedures", 13, 10, 0
    header2     BYTE    "Written by: Jesse Pantoja", 13, 10, 13, 10, 0
    header3     BYTE    "Please provide 10 signed decimal integers.", 13, 10, 0
    header4     BYTE    "Each number needs to be small enough to fit inside a 32 bit register.",
                        " After you have finished inputting the raw numbers, I will display a list",
                        " of the integers, their sum, and their average value.", 13, 10, 13, 10, 0

; (insert variable definitions here)

.code
main PROC

; (insert executable instructions here)
    PUSH    OFFSET header1
    PUSH    OFFSET header2
    PUSH    OFFSET header3
    PUSH    OFFSET header4
    CALL    introduction


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


END main
