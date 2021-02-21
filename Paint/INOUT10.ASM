TITLE   INOUT10  CSCI 225
NAME INOUT10
;DC Pankratz (All Rights Reserved)


COMMENT ! These procedures input and output chars, strings,
	  hex numbers from and to the keyboard and monitor
	  respectively.  BIOS interrupt 10h is used for the IO.

To use these routines you must name your code segment as follows
CSEG  SEGMENT PUBLIC

Also, define all procedures called from this file as EXTRN NEAR
within your code segment.
!


;************************************************************
;                    I N O U T 1 0
;************************************************************

CSEG          SEGMENT   PUBLIC
Assume  CS:CSEG

PUBLIC putDec10, putCstring10

; these procs are in BIOS.asm
EXTRN where:near, write:near, locate:near
EXTRN writeattr:near


putCstring10   PROC   NEAR
COMMENT  !   This procedure outputs a character at a time from
             consecutive memory locations until NULL = 0.
	      IN:  SI contains the starting address for the string.
		       BL contains the color attribute.
	     OUT:  none
	 !
	   PUSH    SI            ; Value parameter
	   PUSH    AX            ; Local variable
	   PUSH    DX
	   PUSH    CX

	   Call    Where         ; get cursor position
	   MOV    CX, 1          ; number of chars to write
SOagain:
	   MOV     AL,[SI]
	   CMP     AL,0          ; If end of string detected,
	   JE      SOdone        ; exit loop and return.
	   
	   call    locate
	   call    writeattr           
	   inc     DL            ; column

	   INC     SI            ; Get ready for next char.
	   JMP     SOagain
SOdone:    
	   POP     CX
	   POP     DX
	   POP     AX            ; Local variables.
	   POP     SI            ; Value parameter.
	   RET                   ; Return to calling program.
putCstring10  ENDP

;*************************************************************** 
;*****************************************************************


putDec10        PROC near
Comment!      Displays the number contained in DX in signed decimal 
	          representation.
	      IN:  DX = the number to be displayed with current attribute
	      OUT: none
!
	      Push       AX
	      Push       BX
	      Push       CX
	      Push       DX
	      Push       SI
	      mov   ax, 0
	      push  ax       ; sentinel
	      Mov   BX, 10 

	      mov   SI, 0
	      Cmp   DX, 0     ; check for negative
	      JGE   DOsign
		inc  SI       ; negative flag
		Neg DX
DOsign:       Mov  AX, DX
DONextDigit:
	      xor  DX, DX
	      DIV  BX
	      add  dl, 30h
	      Push DX
	      Cmp  AX, 0
	      JA  DONextDigit

	      Call where
	      mov   CX, 1     ; number of chars to write
	      cmp SI, 0
	      jz  DODisplay
	      mov AL, '-'
	      call write
	      inc DL 
DODisplay:
	      Pop  AX
	      cmp ax, 0
	      jz  DOdone
	      call locate
	      Call write
	      inc DL      ; next column
	      jmp DODisplay
DOdone:              
	      Pop        SI
	      Pop        DX
	      Pop        CX
	      Pop        BX
	      Pop        AX
	      Ret
putDec10      ENDP




CSEG         ENDS
	     END
