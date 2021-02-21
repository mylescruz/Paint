;   EXAM4 2019 - MOUSE DROPPINGS 
;   Myles Cruz
;	Modified: February 20, 2021
;		- File renamed "paint.asm"

Comment!
     This program uses int33h to attach to the system mouse 
     driver to control mouse movements.
	 
     Actions for mouse events are supplied by the student as 
     per EXAM4.DOC requirements. Refer to the external
     documentation in EXAM4DOC.DOCX for exceptions and
     enhancements. Non implemented components still under
     construction are identified in the external documentation
     EXAM4DOC.DOC and marked with comments in UPPERCASE within 
	 the modules below. 
     A separate file, EXAM4A.asm may contain an alternate 
     version of your program with code that "almost" works.

	 Assistance received:  (be very specific)

	 I am signing this document to verify that I 
	 have followed the Academic Honor Code without exception.


         HONOR CODE SIGNATURE: Myles Cruz DATE:12/2/2019
	 
       !

; Set up the stack
SSEG    SEGMENT   PARA  STACK  'STACK'
    DB  551  DUP ('MY_STACK')   ; can find the stack under debug
SSEG    ENDS


CSEG    SEGMENT  PARA  PUBLIC
	; Tell TASM about our segment definitions.
	ASSUME  CS:CSEG, DS:CSEG, SS:SSEG

	; We are linking BIOS + INOUT10
	;place the EXTRN statements here
    EXTRN clrscr:near
	EXTRN getch:near, kbhit:near
    EXTRN putCstring10:near
	EXTRN putDec10:near
	EXTRN writeattr:near
	EXTRN READATTR:near
	EXTRN SETCURSOR:near
	EXTRN GETCURSOR:near

; Data placed with the code so that the mouse driver can easily find it.

;Characters and Strings
error  				db 'Cannot initialize mouse -- shutting down'
					db 0
					
CrapChar 			db ' '     			; character used for droppings
										;changed in the main
CrapColor 			db 0F0h    			; color used for droppings
										; you will change this
colorBox 			db '     ',0 		; string to print out ColorBar
cover 				db '  ',0			; used to cover previous mouseX/mouseY and previous CrapChar
backgroundBox		db ' ',0			; used to print out 
hi     				db 'hi',0     		; used by dcp for debugging
CrapLabel 			db 'CrapChar: ', 0	; label for CrapChar
xyLabel				db 'mouseX:   mouseY:  ',0	; print out labels for x and y coordinates
MenuLabel 			db 'Exit( ) Erase( ) Clear( ) Foreground( ) Background( )',0 ; print out menu line

;Variables
mouseX  			db  0	; horizontal value for mouse pointer
mouseY  			db  0   ; vertical value for mouse pointer
Xcoordinate			db  7	; location of mouseX label
Ycoordinate 		db  17	; location of mouseY label
XYRow				db  24	; location of xyLabel
exitButton 			db  5	; location of exit button
eraseButton 		db  14	; location of erase button
clearButton			db  23	; location of clear button
foregroundButton 	db  37	; location of foreground button
backgroundButton 	db  51	; location of background button
toggleButton 		db  254	; value of filled in square to identify a highlighted button
EraseColor 			db  00h ; color black for erasing screen
BackgroundColor 	db  4Fh	; background color (red background with white foreground)
charFlag			db 	0	; used to check if a character was entered
moveFlag 			db  0	; used to check if mouse moved
leftClickFlag 		db  0	; used to check if left button was pressed
rightClickFlag		db 	0	; used to check if right button was pressed	
eraserFlag 			db  0	; used to check if "Eraser" button was pressed
foregroundFlag 		db  0	; used to check if "Foreground" button was pressed
backgroundFlag 		db  1	; used to check if "Background" button was pressed
							; set to 1 because, background is originally highlighted
exitFlag    		dw  0        ; bool used to exit program - set by getch
								 ; or mouse
; DATA ENDS HERE



MAIN: ; Life starts here.
	MOV   AX,CSEG   ; Make DS point to OUR code segment.
    MOV   DS,AX     ; Code and data live in one segment.
	
    call clrscr  	
    mov  ax, 00h    ; initialize mouse
	int  33h        ; call mouse driver
	cmp  ax, 00h    ; mouse avail?
	jne   MAINinstalled 
	
	mov  SI, offset error  ; problems capturing our rodent
	call putCstring10
	jmp  MAINquit


MAINinstalled:      ; OK the mouse is allocated to our window.
    mov  ax, 01h    ; Let's make the little rat visible.
	int  33h

	; Good place to get your menu and status bars on the screen
	CALL DisplayMenu
	CALL DisplayColors
	CALL DisplayCrapCharacter
	
    ; Now we install our mouse event handler.
	; When the mouse does anything as described in CX below, 
	; the driver will automatically call our MouseEvent callback function
     
        ;YOU NEED TO CHANGE CX IN ORDER TO HANDLE CLICKS
        mov  CX, 000Fh  ; call mouse event if moved
			; Look at the mask in the documentation
            ; for AX in MouseEvent.
	    push CS
        pop  ES                     ; ES must point to our CSEG
        mov  DX, offset MouseEvent  ; DX points to MouseEvent  
        mov  AX, 0Ch                ; Install our interrupt handler
	    int  33h                    ; for mouse events.
        ; From this point on, the function MouseEvent will be called
		; based on the CX mask.
	
       ; We change the CrapChar through a busy-wait loop.
       ; This is called polling. We keep asking about a key pressed.
       ; Compare this to the fcn MouseEvent which is called by the mouse interrupt.
	
MAINagain:        	
	cmp [exitFlag], 0   
	jne  MAINexit    ; exit program
	
	CMP [charFlag], 0
	JG CrapCharacter	; if Flag is 1, display new character
	CMP [moveFlag], 0
	JG	printCoordinate ;if Flag is 1, display new coordinate
	CMP [leftClickFlag],0
	JG 	leftButton ;if Flag is 1, handle click within function
	CMP [rightClickFlag],0
	JG 	rightButton	; if Flag is 1, switch the color attributes
	JMP MAINcheck

printCoordinate:
	CALL DisplayXYCoordinates
	JMP MAINagain
	
leftButton:
	CALL LeftClick
	JMP MAINagain
	
rightButton:
	CALL RightClick
	JMP MAINagain
	
CrapCharacter:
	MOV [CrapChar], AL
	CALL DisplayCrapCharacter	; display the updated CrapChar
	MOV [charFlag], 0

MAINcheck:
	call kbhit       ; check to see if we have a key
	jz  MAINagain
	MOV [charFlag], 1 ; mark flag if character was entered
	call getch       ; if so, remove the char from the buffer
	cmp  al, 27      ; if ESC, we want to exit as well
    jne   CrapCharacter ; if not ESC, display character
	mov   [exitFlag], 1  ; if ESC set the flag to leave
	jmp	MAINagain

MAINexit:           ; shutting down
	MOV DH, 0		; put toggle on exit button
	MOV DL, 5
	CALL SETCURSOR
	MOV AL, toggleButton
	MOV CX, 1
	MOV BL, BackgroundColor
	CALL writeattr
	mov  ax, 02h    ; hide mouse pointer
	int  33h
    mov  ax, 00h    ; disconnect our mouse handler
	int  33h        

MAINquit:
	MOV   AX,4C00h              ; Return control to DOS.
	INT   21h                   ; End of MAIN program.


; ****************************************************************
;*****************************************************************

MouseEvent  Proc  FAR        
Comment  !   
This function is called by the mouse Interrupt Service Routine (driver).
Make sure that you don't take up too much CPU time in here.

Input parameters:

Note that the actual mouse driver preserves the following registers for us,
so they will not be changed back in the main program.

	AX = events that occurred, depending on mask:   
	   bit 0 = mouse pointer moved
	   bit 1 = left button pressed
	   bit 2 = left button released
	   bit 3 = right button pressed
	   bit 4 = right button released
	   bit 5 = center button pressed
	   bit 6 = center button released

                                      
	BX = Current button state:
	   bit 0 = Left button (0 = up, 1 = pressed down)
	   bit 1 = Right button (0 = up, 1 = pressed down)
	   bit 2 = Center button (0 = up, 1 = pressed down)

	CX = Horizontal  coordinate of mouse
	DX = Vertical  coordinate

        These are used to check how far mouse moved since we were last
        in MouseEvent.
	SI = Last vertical mickey count
	DI = Last horizontal mickey count

	DS = Data seg of the mouse driver. I will reset it to your data seg.

	USE only BIOS interrupts.  NO NOT use any DOS interrupts like getChar and putChar.
  !

	push ds
	push ax
	push dx
	push cx
	push bx

       ; data for this driver will be in our code segment.
	push cs
    pop  ds   ; ds now points to our code segment       
	   
       ; test for events in the order you want priority 
       ; this is basically like a switch-case statement

       ; I'll test for a mouse move for you
MEtestmove:                  
    cmp  ax, 1        ; the mouse moved
    JL  MEret
	shr  cx, 1       ; 8 pixels per char position 
	shr  cx, 1
	shr  cx, 1
	shr  dx, 1
	shr  dx, 1
	shr  dx, 1
    mov  [mouseX], cl ; save the new position
	mov  [mouseY], dl
	MOV [moveFlag], 1 ; indicates the mouse moved

MEtestright:
	CMP AX, 0002h	; compare bit 1 to see if left button was pressed
	JNE MErightClick
	CMP BX, 0001h	; compare bit 0 to see if left button is pressed or released
	JNE MErightClick
	MOV [leftClickFlag], 1
	
MErightClick:
	CMP AX, 0008h	; compare bit 3 to see if right button was pressed
	JNE MEret
	CMP BX, 0002h	; compare bit 1 to see if right button is pressed or released
	JNE MEret
	MOV [rightClickFlag], 1
	
MEret:
	pop bx
	pop cx
	pop dx
	pop ax
	pop ds
	ret          ; back to the mouse driver (ISR)
MouseEvent endP
;**************************************************************
;**************************************************************
; My Functions

; Display Functions
DisplayMenu proc	
	PUSH SI
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX

	MOV DH, 0
	MOV DL, 0
	MOV BL, BackgroundColor
DMmenuLines:	; loop to display menu lines
	CALL SETCURSOR
	MOV AL, backgroundBox
	MOV CX, 80
	CALL writeattr
	INC DH
	CMP DH, 5
	JL DMmenuLines

	CALL SETCURSOR
	MOV AL, 196 ; character is a long dash to show end of menu lines
	MOV CX, 80
	CALL writeattr
	
	MOV DH, 24
DMstatusLines: ; loop to display status lines
	CALL SETCURSOR
	MOV AL, backgroundBox
	MOV CX, 80
	CALL writeattr
	DEC DH
	CMP DH,22
	JG DMstatusLines
	
	CALL SETCURSOR
	MOV AL, 196	; character is a long dash to show end of status lines
	MOV CX, 80
	CALL writeattr

DMmenuLabels:
	MOV DH, 24
	MOV DL, 0
	CALL SETCURSOR
	MOV SI, offset xyLabel ; prints out mouseX and mouseY labels
	CALL putCstring10

	MOV DH, 0
	CALL SETCURSOR
	MOV SI, offset MenuLabel ; prints out menu labels
	CALL putCstring10
	MOV DL, backgroundButton
	CALL SETCURSOR
	MOV AL, toggleButton
	MOV CX, 1
	CALL writeattr
	
	MOV DH, 23
	MOV DL, 0
	CALL SETCURSOR
	MOV SI, offset CrapLabel ; prints out label for current CrapChar
	CALL putCstring10
	
DisplayMenuReturn:
	POP DX
	POP CX
	POP BX
	POP AX
	POP SI
	RET
DisplayMenu endp

DisplayColors proc
Comment !
xbbb ifff (background foreground)
	000 = black
	001 = blue
    010 = green
	011 = green/blue (cyan)
    100 = red
    101 = red/blue (magenta)
    110 = red/green (brown)
    111 = white
!
	PUSH SI
	PUSH BX
	PUSH CX
	PUSH DX

	MOV BL, 00h	; start with black
	MOV CX, 1
	MOV DH, 2
	MOV DL, 1

DCnextColor:
	CALL SETCURSOR
	MOV SI, offset colorBox
	CALL putCstring10
	; explained how this works in EXAM4DOC
	ADD BL, 1
	PUSH CX
	MOV CX, 4
DCshiftLeft:	
	SHL BL, 1
	LOOP DCshiftLeft
	POP CX
	MOV BH, BL
	PUSH CX
	MOV CX, 4
DCshiftRight:
	SHR BH, 1
	LOOP DCshiftRight
	POP CX
	OR BL, BH
	INC CX
	ADD DL, 5
	CMP CX, 8
	JLE DCcontinueRow
	CMP CX, 9
	JG DCcontinueRow
	INC DH		; move the different colors' intensities into second row
	MOV DL, 1
	
DCcontinueRow:
	CMP CX, 16 ; check if loop ran for all 16 colors
	JLE DCnextColor

DisplayColorsReturn:
	POP DX
	POP CX
	POP BX
	POP SI
	RET
DisplayColors endp

DisplayXYCoordinates proc
	PUSH SI
	PUSH BX
	PUSH DX
	
	;use a block with background color to cover up previous coordinates
	MOV DH, XYRow
	MOV DL, Xcoordinate
	CALL SETCURSOR
	MOV BL, BackgroundColor
	MOV SI, offset cover ;cover up previous X coordinate
	CALL putCstring10
	MOV DH, XYRow
	MOV DL, Ycoordinate
	CALL SETCURSOR
	MOV BL, BackgroundColor
	MOV SI, offset cover ;cover up previous Y coordinate
	CALL putCstring10
	;print new coordinate
	MOV DH, XYRow
	MOV DL, Xcoordinate
	CALL SETCURSOR
	MOV DH, 0
	MOV DL, [mouseX]
	CALL putDec10
	MOV DH, XYRow
	MOV DL, Ycoordinate
	CALL SETCURSOR
	MOV DH, 0
	MOV DL, [mouseY]
	CALL putDec10
	MOV [moveFlag],0	;set back to 0 which means mouse is no longer moving

DisplayXYCoordinatesReturn:
	POP DX
	POP BX
	POP SI
	RET
DisplayXYCoordinates endp

DisplayCrapCharacter proc
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	
	; display current CrapChar value
	MOV DH, 23
	MOV DL, 9
	CALL SETCURSOR
	MOV AL, CrapChar
	MOV CX, 1
	MOV BL, CrapColor
	CALL writeattr

DisplayCrapCharacterReturn:
	POP DX
	POP CX
	POP BX
	POP AX
	RET
DisplayCrapCharacter endp

; Interrupt Functions
LeftClick proc
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX

LCexit:
	; checking if exit button was pressed
	CALL Exit	
	CMP [exitFlag], 1 
	JNE LCclearScreen
	JMP LeftClickReturn
	
LCclearScreen:
	; check if clear button was pressed
	CALL ClearScreen 

LCeraser:
	; checking if erase button was pressed
	CALL Eraser 
	CMP [eraserFlag], 1
	JNE LCforegroundBackground
	JMP LCcrap

LCforegroundBackground:
	; used to toggle between foreground and background buttons
	CALL ForegroundBackground
	
LCcolorBar:
	; checking to see if color was chosen
	CALL ColorBar
	
LCcrap:
	; clicking on screen to print out CrapChar
	CALL PrintCrap

LeftClickReturn:
	MOV [leftClickFlag], 0
	; set flag back to 0 to indicate left button is no longer clicked

	POP DX
	POP CX
	POP BX
	POP AX
	RET
LeftClick endp

RightClick proc
	PUSH AX
	PUSH BX
	PUSH CX
	
	; switches the foreground and background
	MOV BL, CrapColor
	MOV BH, BL
	
	MOV CX, 4
RCshiftLeft:
	SHL BL, 1
	LOOP RCshiftLeft
	
	MOV CX, 4
RCshiftRight:
	SHR BH, 1
	LOOP RCshiftRight
	
	OR BL, BH
	MOV CrapColor, BL
	MOV AL, CrapChar
	CALL DisplayCrapCharacter
	MOV [rightClickFlag], 0

RightClickReturn:
	POP CX
	POP BX
	POP AX
	RET
RightClick endp

; Functions
PrintCrap proc
	;prints out mouse droppings
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	
	CMP [mouseY], 5	; checking if clicked within menu lines
	JLE PrintCrapReturn
	CMP [mouseY], 22 ; checking if clicked within status lines
	JGE PrintCrapReturn
	MOV DH, [mouseY]
	MOV DL, [mouseX]
	CALL SETCURSOR
	
	MOV AL, CrapChar
	MOV CX, 1
	CMP [eraserFlag], 1 ; if eraser button was pressed
						; makes mouse droppings black, to "erase" previous droppings
	JE  PCprintErase
	MOV BL, CrapColor
	JMP PCprint
PCprintErase:
	MOV BL, EraseColor
PCprint:
	CALL writeattr

PrintCrapReturn:
	POP DX
	POP CX
	POP BX
	POP AX
	RET
PrintCrap endp

Exit proc
	; checks if "Exit" button was pressed
	CMP [mouseY], 0	
	JNE ExitReturn
	CMP [mouseX], 5
	JNE ExitReturn
	MOV [exitFlag], 1
ExitReturn:
	RET
Exit endp

Eraser proc
	; checks if "Erase" button was pressed
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX

EcheckEraserCoordinate:
	CMP [mouseY], 0
	JE	EcheckX
	JMP EraserReturn
EcheckX:
	CMP [mouseX], 14
	JE  EcheckEraserFlag
	JMP EraserReturn
EcheckEraserFlag:
	CMP [eraserFlag], 1
	JNE EsetEraserFlag
	MOV [eraserFlag], 0		; turning off eraser button
	MOV DH, 0
	MOV DL, 14
	CALL SETCURSOR
	MOV AL, cover
	MOV CX, 1
	MOV BL, BackgroundColor
	CALL writeattr			; covers toggle button if unclicked
	JMP EraserReturn
EsetEraserFlag:
	MOV [eraserFlag], 1		; turning on eraser button
	MOV DH, 0
	MOV DL, 14
	CALL SETCURSOR
	MOV AL, toggleButton
	MOV CX, 1
	MOV BL, BackgroundColor	; places toggle button if clicked
	CALL writeattr

EraserReturn:
	POP DX
	POP CX
	POP BX
	POP AX
	RET
Eraser endp

ClearScreen proc
	; clears the "screen" in between menu and status
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	
	; checks if "Clear" button was pressed 
	CMP [mouseY], 0
	JE  CScheckX
	JMP ClearScreenReturn
CScheckX:
	CMP [mouseX], 23
	JE CSclearScreen
	JMP ClearScreenReturn
	
	; places a black dropping over each location
CSclearScreen:
	MOV DH, 6
	MOV DL, 0
CSclearing:
	CALL SETCURSOR
	MOV AL, ' '
	MOV CX, 80
	MOV BL, EraseColor
	CALL writeattr
	INC DH
	CMP DH, 21
	JL  CSclearing
	
ClearScreenReturn:
	POP DX
	POP CX
	POP BX
	POP AX
	RET
ClearScreen endp

ForegroundBackground proc
	; checks the toggling between foreground and background buttons
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	
	; check if foreground button is clicked and on/off
	CMP [mouseY], 0
	JNE FBbackground
	CMP [mouseX], 37
	JNE FBbackground
	CMP [foregroundFlag], 1
	JNE FBsetForegroundFlag
	JMP ForegroundBackgroundReturn
FBsetForegroundFlag:
	; turn background button off and foreground button on
	MOV [foregroundFlag], 1
	MOV [backgroundFlag], 0
	MOV DH, [mouseY]
	MOV DL, [mouseX]
	CALL SETCURSOR
	MOV AL, toggleButton
	MOV BL, BackgroundColor
	MOV CX, 1
	CALL writeattr	; highlight foreground button
	MOV DH, 0
	MOV DL, backgroundButton
	CALL SETCURSOR
	MOV AL, cover
	CALL writeattr	; cover background button
	JMP ForegroundBackgroundReturn

FBbackground:
	;check if background button is clicked and on/off
	CMP [mouseY],0
	JNE ForegroundBackgroundReturn
	CMP [mouseX], 51
	JNE ForegroundBackgroundReturn
	CMP [backgroundFlag], 1
	JNE FBsetBackgroundFlag
	JMP ForegroundBackgroundReturn
FBsetBackgroundFlag:
	; turn background button on and foreground button off
	MOV [backgroundFlag], 1
	MOV [foregroundFlag], 0
	MOV DH, [mouseY]
	MOV DL, [mouseX]
	CALL SETCURSOR
	MOV AL, toggleButton
	MOV BL, BackgroundColor
	MOV CX, 1
	CALL writeattr	; highlight background button
	MOV DH, 0
	MOV DL, foregroundButton
	CALL SETCURSOR
	MOV AL, cover
	CALL writeattr	; cover foreground button

ForegroundBackgroundReturn:
	POP DX
	POP CX
	POP BX
	POP AX
	RET
ForegroundBackground endp

ColorBar proc
	; check which color was clicked
	PUSH AX
	PUSH BX
	PUSH DX
	
	; check if clicked within the Color Bar range
CBcheckYlow:
	CMP [mouseY],2
	JGE CBcheckYhigh
	JMP ColorBarReturn
CBcheckYhigh:
	CMP [mouseY],3
	JLE CBcheckXlow
	JMP ColorBarReturn
CBcheckXlow:
	CMP [mouseX], 1
	JGE CBcheckXhigh
	JMP ColorBarReturn
CBcheckXhigh:
	CMP [mouseX], 41
	JL CBchooseColor
	JMP ColorBarReturn
CBchooseColor:
	MOV DH, [mouseY]
	MOV DL, [mouseX]
	CALL SETCURSOR
	CALL READATTR
	; checks which button is highlighted
	CMP [backgroundFlag], 1
	JE CBbackgroundChange
	CMP [foregroundFlag], 1
	JE CBforegroundChange
CBbackgroundChange:
	MOV BH, CrapColor
	AND AH, 0F0h
	AND BH, 0Fh
	OR  AH, BH
	JMP CBsetChange
CBforegroundChange:
	MOV BH, CrapColor
	AND AH, 0Fh
	AND BH, 0F0h
	OR  AH, BH
CBsetChange:
	MOV CrapColor, AH
	MOV BL, CrapColor
	MOV AL, CrapChar
	CALL DisplayCrapCharacter

ColorBarReturn:
	POP DX
	POP BX
	POP AX
	RET
ColorBar endp

CSEG    ENDS            ; End of code segment.

END     MAIN            ; End of program. Start execution at MAIN
