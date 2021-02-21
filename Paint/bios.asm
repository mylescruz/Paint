TITLE BIOS - I/O ROUTINES
NAME BIOS
;DC Pankratz (All Rights reserved)

COMMENT !
        REMARKS:
                - Active page BH is always set to 0.

                - All routines are written assuming SCREEN TYPE
                  is type 3 = 25x80 color text. Set this by calling VIDEOTYPE.

                - To use this module, you must define your code segment
                  as  CSEG   SEGMENT  PUBLIC

                  and declare as EXTRN NEAR all procedures you want from
                  this library within CSEG
!
;**************************************************************************
;                        V I D E O   I N T   1 0
;**************************************************************************
CSEG    SEGMENT  BYTE  PUBLIC
        Assume   CS:CSEG

        PUBLIC  LOCATE, WHERE, WRITEATTR, WRITE, READATTR
        PUBLIC  SCROLLATTR, CLRSCR, CLRSCRATTR
        PUBLIC  CURSORON, CURSOROFF  ; 2001
        PUBLIC  GETCURSOR, SETCURSOR ; 2011

Comment!
                - The ATTRIBUTE byte for color settings is defined as:
                  BIT PATTERN  xbbb ifff
                  where  ifff = foreground (i = intensity)
				         xbbb = background (x = intensity in Win mode)
						                   (x = blinking in DOS mode)
	              000 = black
                  001 = blue
                  010 = green
                  100 = red
                  011 = green/blue (cyan)
                  101 = red/blue (magenta)
                  110 = red/green (brown)
                  111 = white

                - Active page BH is always set to 0.
        !

;**************************************************************************
LOCATE          PROC
;        Positions the cursor on the screen
;        IN:   DH    row number (0-25 decimal)
;              DL    col number (0-79 decimal)
;        OUT:  none
 
        push    bx
        push    ax
        mov     bh,0    ; page 0
        mov     ah,2    ; service
        int     10h
        pop     ax
        pop     bx
        ret
LOCATE          ENDP

;****************************************************************************
SETCURSOR       PROC
; Same as LOCATE
        call LOCATE
        ret
SETCURSOR       ENDP

;****************************************************************************
WHERE           PROC
COMMENT !
                Reads the cursor position on the screen.
                IN:     none
                OUT:    DH   row number (0-18h)
                        DL   col number (0-4Fh)
                ******  Remove the PUSH and POP CX if you want to read the 
                        cursor mode (CH start line - CL end line)
        !
        push    cx      ; remove for cursor mode
        push    bx      ; page 0
        push    ax
        mov     bh,0
        mov     ah,3
        int     10h
        pop     ax
        pop     bx
        pop     cx      ; remove for cursor mode
        ret
WHERE           ENDP

;****************************************************************************
GETCURSOR       PROC
          ; same as WHERE
          call WHERE
          ret
GETCURSOR       ENDP

;****************************************************************************
CURSOROFF        PROC
    ; turns the cursor off by resizing it
    push ax
    mov AH, 1
    mov CH, 20h
    mov CL, 0
    int 10h
    pop ax
    Ret
CURSOROFF       ENDP

;****************************************************************************
CURSORON      PROC
     ; turns cursor on by resizing it
     push ax
     mov  ah,1
     mov  CH, 0Dh
     mov  CL, 0Eh
     int 10h
     pop ax
     ret
CURSORON      ENDP

;****************************************************************************
WRITEATTR              PROC
COMMENT !
                Displays a character and an attribute w/o advance.
                IN:  AL    character
                     CX    number of times to repeat character
                     BL    attribute

                OUT: none
        !
        push    ax
        push    bx
        mov     bh,0   ; page
        mov     ah,9
        int     10h
        pop     bx
        pop     ax
        ret
WRITEATTR              ENDP

;****************************************************************************
WRITE                   PROC
COMMENT !
                Displays a character using the current attribute w/o advance.
                IN:  AL    character
                     CX    number of times to repeat character

                OUT: none
        !
        push    ax
        push    bx
        mov     bh,0
        mov     ah,0ah
        int     10h
        pop     bx
        pop     ax
        ret
WRITE                   ENDP

;****************************************************************************
READATTR               PROC
COMMENT   !
                Returns the character at the current position of the cursor on 
                the screen along with the ATTRIBUTE
                IN:   none 
                OUT:  AL    character
                      AH    attribute
          !
        push    bx
        mov     bh,0
        mov     ah,8
        int     10h
        pop     bx
        ret
READATTR               ENDP

;****************************************************************************
SCROLLATTR              PROC

COMMENT !
                Scrolls the window up.  Input lines inserted at bottom of 
                window.   Set CX = 0000 and DX = 184FH for the entire screen.
                IN:  AL   number of lines (AL = 0 blanks the entire window)
                     CH,CL row,column of upper left hand corner of window
                     DH,DL row,column of lower right hand corner of window
                     BH    ATTRIBUTE for blank lines
                OUT:    none
        !
        push    ax
        mov     ah,6
        int     10h
        pop     ax
        ret

SCROLLATTR              ENDP

;****************************************************************************
CLRSCR                  PROC
COMMENT !
                Scrolls the screen up so that it is blank
                Uses normal, non-blinking, low-intensity as the attribute.
                Positions the cursor in upper left hand corner.
                IN:  none
                OUT: none
        !
        push    ax
        push    bx
        push    cx
        push    dx
        mov     cx,0000 ; upper left hand corner
        mov     dx,184fh   ; lower right hand corner
        mov     bh,07h     ; normal,non-b,non-i
        mov     al,0       ; blank the entire window
        mov     ah,6       ; scroll up
        int     10h
        mov     dx,0
        mov     bh,0       ; page 0,
        mov     ah,2       ; home cursor
        int     10h
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        ret
CLRSCR             ENDP

;****************************************************************************
CLRSCRATTR                  PROC
COMMENT !
                Scrolls the screen up so that it is blank
                and sets a new attribute for the screen.
                Positions the cursor in upper left hand corner.
                IN:  BH  new attribute
                OUT: none
        !
        push    ax
        push    bx
        push    dx
        mov     cx,0000 ; upper left hand corner
        mov     dx,184fh   ; lower right hand corner
        mov     al, 0      ; 1997 fixed
        mov     ah,6       ; scroll up
        int     10h
        mov     dx,0
        mov     bh,0       ; page 0,
        mov     ah,2       ; home cursor
        int     10h
        pop     dx
        pop     bx
        pop     ax
        ret
CLRSCRATTR                ENDP

;****************************************************************************
CSEG         ENDS


CSEG         SEGMENT  BYTE   PUBLIC
             ASSUME   CS:CSEG
;**************************************************************************
;                        K E Y B D   1 6
;**************************************************************************

               PUBLIC  getch, kbhit
COMMENT !
        The hardware int 9 inserts data to the memory keyboard buffer every 
        time a key is pressed. If an int 9 occured, each of these 
        procedures will return the data in AX.
        AL = ascii char or zero if there is no ascii for the key
        AH = scan code    !

;**************************************************************************
getch           Proc  
;        Extracts data from the buffer. Waits if buffer is empty. 
;		Does not echo the key on screen   
        Mov Ah,0
        Int 16h
        Ret
getch   Endp

;**************************************************************************
kbhit         Proc
Comment !
        Examines the buffer to see if a key was entered. AX has a copy of the
		key, but it still remains in the buffer.
        If buffer non empty, then  ZF is set off = 0.
        If buffer empty,  then  ZF is set on = 1.
        For example, JZ NOKEY  will branch to the label NOKEY if no key was pressed.
        !
          Mov Ah, 1
          Int 16h
          Ret
kbhit   Endp

CSEG      ENDS
                END
