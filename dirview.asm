.8086
MODEL  TINY
.STACK  100H
CODE    SEGMENT PARA
ASSUME  CS:CODE,SS:STACK

AT              EQU     SET_CURSOR
WRITE_WORD      EQU     WW
WRITE_SYMBOL    EQU     WS

HILEFT  EQU     0C9h
HIRIGHT EQU     0BBH
LOLEFT  EQU     0C8H
LORIGHT EQU     0BCH
HORIZ   EQU     0CDH
VERT    EQU     0BAH

HEADLENY        EQU     24
HEADLENX        EQU     80
DONTLENY        EQU     5
DONTLENX        EQU     28
DONTATY         EQU     (25-DONTLENY)/2-1
DONTATX         EQU     (80-DONTLENX)/2
ERRORLENY       EQU     5
ERRORLENX       EQU     47
ERRORATY        EQU     (25-ERRORLENY)/2-1
ERRORATX        EQU     (80-ERRORLENX)/2
HELPLENY        EQU     11
HELPLENX        EQU     37
HELPATY         EQU     (25-HELPLENY)/2-1
HELPATX         EQU     (80-HELPLENX)/2
CREDITSLENY     EQU     9
CREDITSLENX     EQU     28
CREDITSATY      EQU     (25-CREDITSLENY)/2-1
CREDITSATX      EQU     (80-CREDITSLENX)/2
NOTFOUNDLENY    EQU     5
NOTFOUNDLENX    EQU     24
NOTFOUNDATY     EQU     (25-NOTFOUNDLENY)/2-1
NOTFOUNDATX     EQU     (80-NOTFOUNDLENX)/2
DRIVESLENY      EQU     5
DRIVESATY       EQU     (25-DRIVESLENY)/2-1

DONTWB          EQU     HEADLENY*HEADLENX*2
ERRORWB         EQU     DONTWB+DONTLENY*DONTLENX*2
HELPWB          EQU     ERRORWB+ERRORLENY*ERRORLENX*2
CREDITSWB       EQU     HELPWB+HELPLENY*HELPLENX*2
NOTFOUNDWB      EQU     CREDITSWB+CREDITSLENY*CREDITSLENX*2
DRIVESWB        EQU     NOTFOUNDWB+NOTFOUNDLENY*NOTFOUNDLENX*2

PATH    EQU     TABLES+16
DIR     EQU     PATH+5
OLD     EQU     PATH+4101
WIN     EQU     PATH+4352


DATS    EQU     CS:[DATA_SEG]
TABS    EQU     CS:[TABLES_SEG]
PATHS   EQU     CS:[PATH_SEG]
DIRS    EQU     CS:[DIR_SEG]
OLDS    EQU     CS:[OLDSCR_SEG]
WINS    EQU     CS:[WINDS_SEG]

ENTER_KEY       EQU     1CH
ESCAPE_KEY      EQU     01H
UP_KEY          EQU     48H
DOWN_KEY        EQU     50H
PGUP_KEY        EQU     49H
PGDOWN_KEY      EQU     51H
F9_KEY          EQU     43H
F10_KEY         EQU     44H
F1_KEY          EQU     3BH
LEFT_KEY        EQU     4BH
RIGHT_KEY       EQU     4DH

START:
NOW    PROC     NEAR
        MOV     AX,CS
        SUB     AX,10H
        CLI
        MOV     SS,AX
        MOV     SP,100H
        STI
        CALL    LOADWIN
        JNC     A0
        MOV     DS,DATS
        LEA     SI,DATA:PRINT_READ_ERROR
        CALL    WW
        JMP     SHORT   EXIT1
A0:
        CALL    SAVEALL
        CALL    SETUP
A1:
        ASSUME  DS:DATA
        MOV     DS,DATS
        CALL    DIRREADER
        CALL    PRINT_CURRENT_PATH
        CALL    PRINT_FREE_SPACE
A2:
        CALL    PRINT_CURRENT_DIR_CONTENTS
        JC      A1
A3:
        CALL    INKEY
        CMP     AL,ENTER_KEY
        JE      ENTERF
        CMP     AL,ESCAPE_KEY
        JE      F10
        CMP     AL,F10_KEY
        JE      F10
        CMP     AL,DOWN_KEY
        JE      DOWN
        CMP     AL,UP_KEY
        JE      UP
        CMP     AL,LEFT_KEY
        JE      PGUP
        CMP     AL,PGDOWN_KEY
        JE      PGDOWN
        CMP     AL,RIGHT_KEY
        JE      PGDOWN
        CMP     AL,PGUP_KEY
        JE      PGUP
        CMP     AL,F9_KEY
        JE      F9
        CMP     AL,F1_KEY
        JNE     A3
        CALL    PRINT_HELP_WIN
        JMP     SHORT   A3
ENTERF:
        CALL    ENTER_FUNCTION
        JNC     A3
        JMP     SHORT   A1
F10:
        JMP     SHORT   EXIT
DOWN:
        CALL    DOWN_FUNCTION
        JMP     SHORT   A3
UP:
        CALL    UP_FUNCTION
        JMP     SHORT   A3
PGDOWN:
        CALL    PGDOWN_FUNCTION
        JNC     A3
        JMP     SHORT   A2
PGUP:
        CALL    PGUP_FUNCTION
        JNC     A3
        JMP     SHORT   A2
F9:
        CALL    F9_FUNCTION
        JNC     A3
        CALL    OVER1
        JMP     SHORT   A1
EXIT:
        CALL    POPALL
        CALL    POPINT
EXIT1:
        MOV     AX,4C00H
        INT     21H
ENDP
SAVEALL PROC
        ASSUME  DS:DATA
        MOV     DS,DATS
        LEA     SI,CREDITS_STRING
        CALL    WW
        ASSUME  ES:OLD
        MOV     ES,OLDS
        XOR     DI,DI
        CALL    GET_CURSOR
        STOSW
        CALL    GET_CUR_SIZE
        STOSW
        CALL  GET_DRIVE
        STOSB
        LDS   SI,DWORD PTR CS:[VTBL]
ENDP
RECOPY_SCR    PROC
        MOV   AX,CS:[VTBL+4]
        MUL   CS:[VTBL+6]
        MOV   CX,AX
        REP   MOVSW
        RET
ENDP
GET_CUR_SIZE    PROC
        PUSH    BX
        PUSH    CX
        PUSH    DX
        MOV     AX,300H
        MOV     BH,0
        INT     10H
        MOV     AX,CX
        POP     DX
        POP     CX
        POP     BX
        RET
ENDP
SET_CUR_SIZE    PROC
        PUSH    AX
        PUSH    CX
        MOV     CX,AX
        MOV     AX,100H
        INT     10H
        POP     CX
        POP     AX
        RET
ENDP
POPALL  PROC
        ASSUME  DS:OLD
        MOV     DS,OLDS
        XOR     SI,SI
        LODSW
        CALL    SET_CURSOR
        LODSW
        CALL    SET_CUR_SIZE
        LODSB
        MOV   DL,AL
        CALL  SET_DRIVE
        LES   DI,DWORD PTR CS:[VTBL]
        JMP   SHORT     RECOPY_SCR
ENDP
SETUP   PROC
        ASSUME  DS:DATA
        MOV     DS,DATS
        CALL    DETECT_DRIVES
        CALL    SETINT
        CALL    GET_DTA_ADRESS
        CALL    SET_COORDX
        CALL    SET_C_SIZE
        CALL    CREATE_DRIVES_WIN
        CALL    CLS
        CALL    PRINT_CREDITS_WIN
        JMP     PRINT_HEAD_WIN
ENDP
SET_C_SIZE      PROC
        MOV     AX,200FH
        JMP     SHORT   SET_CUR_SIZE
ENDP
CLS     PROC
        MOV     AX,500H
        INT     10H
        MOV     AX,600H
        MOV     CX,0
        MOV     BH,07
        MOV     DX,24*256+79
        INT     10H
        RET
ENDP
SETINT  PROC
        PUSH    DS
        PUSH    ES
        ASSUME  DS:DATA
        MOV     DS,DATS
        MOV     AX,3524H
        INT     21H
        MOV     DS:[OLD_24H_SEG],ES
        MOV     DS:[OLD_24H_OFF],BX
        MOV     AX,2524H
        LEA     DX,CODE:NEW_24H
        PUSH    CS
        POP     DS
        INT     21H
        POP     ES
        POP     DS
        RET
ENDP
POPINT  PROC
        PUSH    DS
        ASSUME  ES:DATA
        MOV     ES,DATS
        MOV     DX,ES:[OLD_24H_OFF]
        MOV     DS,ES:[OLD_24H_SEG]
        MOV     AX,2524H
        INT     21H
        POP     DS
        RET
ENDP
NEW_24H PROC
        CALL    GETERROR
        ADD     SP,8
        POP     BX
        POP     CX
        POP     DX
        POP     SI
        POP     DI
        MOV     BP,SP
        OR      WORD PTR SS:[BP+10],1
        POP     BP
        POP     DS
        POP     ES
        IRET
ENDP
GETERROR        PROC
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DI
        PUSH    BP
        PUSH    DS
        PUSH    ES
        MOV     AX,5900H
        MOV     BX,0000H
        INT     21H
        POP     ES
        POP     DS
        POP     BP
        POP     DI
        POP     SI
        POP     DX
        POP     CX
        POP     BX
        RET
ENDP
SET_COORDX      PROC
        PUSH    ES
        ASSUME  ES:TABLES
        MOV     ES,TABS
        XOR     AX,AX
        MOV     AL,DS:[QUANT_LDISKS]
        MOV     DX,3
        MUL     DX
        MOV     DX,6
        ADD     AX,DX
        MOV     WORD PTR ES:[DRIVESWIN+6],AX
        MOV     DX,CS:[VTBL+6]
        SUB     DX,AX
        SHR     DX,1
        MOV     WORD PTR ES:[DRIVESWIN+0AH],DX
        ADD     DX,3
        MOV     WORD PTR DS:[FIRST_DISK_COORDX],DX
        POP     ES
        RET
ENDP
PRINT_HEAD_WIN  PROC
        ASSUME  DS:TABLES,ES:TABLES
        PUSH    SI
        PUSH    DI
        PUSH    DS
        PUSH    ES
        MOV     DS,TABS
        PUSH    DS
        POP     ES
        LEA     SI,HEADWIN
        LEA     DI,ES:HEADSCR
        JMP     PRINT_WIN_AT1
ENDP
LOADWIN PROC
        ASSUME  ES:DATA,DS:WIN
        PUSH    DS
        PUSH    ES
        MOV     ES,DATS
        LEA     SI,WINFNAME
        MOV     DS,WINS
        XOR     DX,DX
        XOR     AX,AX
        DEC     AX
        MOV     BX,AX
        MOV     CX,AX
        CALL    LOAD_FILE
        POP     ES
        POP     DS
        RET
ENDP
KEYPAUSE        PROC
        PUSH    AX
        MOV     AH,10H
        INT     16H
        POP     AX
        RET
ENDP
DETECT_DRIVES   PROC
        ASSUME  DS:DATA
        PUSH    AX
        PUSH    BX
        PUSH    DX
        CALL    GET_DRIVE
        MOV     DS:[CURRENT_DRIVE],AL
        CALL    DETECT_FLOPPY
        MOV     DS:[QUANT_FDISKS],AL
        PUSH    AX
        CALL    DETECT_HARD
        MOV     DS:[QUANT_HDISKS],AL
        POP     BX
        ADD     AL,BL
        MOV     DS:[QUANT_LDISKS],AL
        MOV     DL,DS:[CURRENT_DRIVE]
        CALL    SET_DRIVE
        POP     DX
        POP     BX
        POP     AX
        RET
ENDP
DETECT_FLOPPY   PROC
        MOV     CX,2
        XOR     AX,AX
        XOR     DL,DL
DETECT_FLOPPY_2:
        CALL    DETECT_DISK
        JNZ     DETECT_FLOPPY_1
        INC     AX
DETECT_FLOPPY_1:
        INC     DL
        LOOP    DETECT_FLOPPY_2
        RET
ENDP
DETECT_HARD     PROC
        XOR     AX,AX
        MOV     DL,2
        MOV     CX,0FFH-2
DETECT_HARD_1:
        CALL    DETECT_DISK
        JNZ     DETECT_HARD_2
        INC     AX
DETECT_HARD_2:
        INC     DL
        LOOP    DETECT_HARD_1
        RET
RET
ENDP
DETECT_DISK     PROC
        PUSH    AX
        MOV     AH,0EH
        INT     21H
        MOV     AH,19H
        INT     21H
        CMP     DL,AL
        POP     AX
        RET
ENDP
DIRREADER       PROC;(IN-DS=DATA)
        CALL    DIRREAD
        JNC     DIRREADER1
DIRREADER2:
        CALL    F9_FUNCTION
        JNC     DIRREADER2
        JMP     SHORT   DIRREADER
DIRREADER1:
        MOV     DS:[MAX_RECORDS],AX
        MOV     DS:[CURRENT_SELECTOR],0
        RET
ENDP
DIRREAD PROC
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DI
        PUSH    DS
        PUSH    ES
        ASSUME  ES:DIR,DS:DATA
DIRREAD_A:
        MOV     ES,DIRS
        MOV     DS,DATS
        XOR     DI,DI
        LDS     SI,DWORD PTR DS:[DTA_ADRESS]
        XOR     CX,CX
        CALL    READ_FRECORD_FIRST
        JNC     DIRREAD_1
        OR      AX,AX
        JZ      DIRREAD_2
        JMP     SHORT   DIRREAD_3
DIRREAD_1:
        CALL    DTA2DIR
        INC     CX
DIRREAD_2:
        CALL    READ_FRECORD_NEXT
        JNC     DIRREAD_1
        OR      AX,AX
        JZ      DIRREAD_2
DIRREAD_3:
        CALL    CHECKERROR
        MOV     AX,CX
        POP     ES
        POP     DS
        POP     DI
        POP     SI
        POP     DX
        POP     CX
        POP     BX
        RET
ENDP
DTA2DIR PROC
        PUSH    CX
        PUSH    SI
        PUSH    DI
        ADD     SI,15H
        MOV     CX,11
        REP     MOVSW
        POP     DI
        ADD     DI,32
        POP     SI
        POP     CX
        RET
ENDP
READ_FRECORD_FIRST      PROC
        PUSH    CX
        PUSH    DX
        PUSH    DS
        ASSUME  DS:DATA
        MOV     DS,DATS
        LEA     DX,DS:FILE_MASK
        MOV     CX,0FFFFH
        MOV     AH,4EH
        INT     21H
        POP     DS
        JC      R_F_F_E
        CMP     DS:[SI+1EH],' .'
        JNE     R_F_F_E
        XOR     AX,AX
        STC
R_F_F_E:
        POP     DX
        POP     CX
        RET
ENDP
READ_FRECORD_NEXT       PROC
        MOV     AH,4FH
        INT     21H
        RET
ENDP
CHECKERROR      PROC
        OR      AX,AX
        JZ      CHECKERROR_EXIT
        CMP     AX,17H
        JAE     CHECKERROR_READ
        CMP     AX,12H
        JE      CHECKERROR_EXIT
        JA      CHECKERROR_DONT
        CMP     AX,11H
        JNE     CHECKERROR_READ
CHECKERROR_DONT:
        CALL    PRINT_DONTREADY_WIN
        STC
CHECKERROR_EXIT:
        RET
CHECKERROR_READ:
        JMP     PRINT_READERROR_WIN
ENDP
PRINT_CURRENT_PATH      PROC
        PUSH    AX
        PUSH    DX
        PUSH    SI
        PUSH    DS
        PUSH    ES
        ASSUME  DS:PATH,ES:DATA
        MOV     DS,PATHS
        MOV     ES,DATS
        MOV     AH,47H
        MOV     DL,ES:[CURRENT_DRIVE]
        MOV     SI,3
        MOV     AL,DL
        ADD     AL,'A'
        MOV     DS:[SI-3],AL
        MOV     AX,'\:'
        MOV     DS:[SI-2],AX
        MOV     AH,47H
        INC     DL
        INT     21H
        MOV     AX,106H
        CALL    AT
        CALL    CLEAR_PATH_SCR
        XOR     SI,SI
        CALL    WRITE_WORD
        POP     ES
        POP     DS
        POP     SI
        POP     DX
        POP     AX
        RET
ENDP
CLEAR_PATH_SCR  PROC
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    BP
        MOV     AX,600H
        MOV     CX,106H
        MOV     DX,100H+78
        MOV     BH,07
        INT     10H
        POP     BP
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET
ENDP
GET_DTA_ADRESS  PROC
        PUSH    BX
        PUSH    DS
        PUSH    ES
        ASSUME  DS:DATA
        MOV     DS,DATS
        MOV     AH,2FH
        INT     21H
        MOV     DS:[DTA_ADRESS],BX
        MOV     DS:[DTA_ADRESS+2],ES
        POP     ES
        POP     DS
        POP     BX
        RET
ENDP
PRINT_FREE_SPACE        PROC
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DI
        PUSH    DS
        PUSH    ES
        CALL    CLEAR_FREE_SCR
        MOV     AH,36H
        MOV     DS,DATS
        PUSH    DS
        POP     ES
        ASSUME  DS:DATA,ES:DATA
        LEA     DI,ES:FREE_DIGITAL+2
        MOV     DL,DS:[CURRENT_DRIVE]
        INC     DL
        INT     21H
        MUL     CX
        MUL     BX
        CMP     DH,26H
        JA      P_F_SPACE_1
        PUSH    DI
        CALL    CONV_BYTES
        XOR     AL,AL
        STOSB
        POP     DI
        MOV     SI,DI
        SUB     DI,2
        PUSH    DI
        CALL    SETPOINT
        JMP     SHORT     P_F_SPACE_111
P_F_SPACE_1:
        PUSH    DI
        CALL    CONV_KILO
        XOR     AL,AL
        STOSB
        POP     DI
        MOV     SI,DI
        SUB     DI,2
        PUSH    DI
        CALL    SETPOINT
        DEC     DI
        MOV     AX,'K'
        STOSW
P_F_SPACE_111:
        POP     SI
        CALL    CONFIRM_LEN
        MOV     AX,23*256+0BH
        CALL    AT
        CALL    WRITE_WORD
P_F_SPACE_EXIT:
        POP     ES
        POP     DS
        POP     DI
        POP     SI
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET
ENDP
CLEAR_FREE_SCR  PROC
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    BP
        MOV     AX,600H
        MOV     CX,23*256+0BH
        MOV     DX,23*256+0BH+9
        MOV     BH,07
        INT     10H
        POP     BP
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET
ENDP
INKEY   PROC
        MOV     AH,10H
        INT     16H
        XCHG    AL,AH
        RET
ENDP
ENTER_FUNCTION  PROC
        PUSH    AX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DS
        PUSH    ES
        ASSUME  DS:DIR,ES:DATA
        MOV     DS,DIRS
        MOV     ES,DATS
        MOV     AX,ES:[CURRENT_CURSOR]
        MOV     CL,5
        SHL     AX,CL
        MOV     SI,AX
        LODSB
        TEST    AL,10000B
        JZ      ENTER_FUNCTION_EXIT
        TEST    AL,1000B
        JNZ     ENTER_FUNCTION_EXIT
        ADD     SI,(1EH-15H)-1
        MOV     DX,SI
        MOV     AH,3BH
        INT     21H
        MOV     ES:[CURRENT_SELECTOR],0
        CALL    OVER1
        STC
ENTER_FUNCTION_EXIT:
        POP     ES
        POP     DS
        POP     SI
        POP     DX
        POP     CX
        POP     AX
        RET
ENDP
DOWN_FUNCTION   PROC;(IN:DS=DATA)
        PUSH    AX
        PUSH    CX
        ASSUME  DS:DATA
        MOV     AX,DS:[CURRENT_CURSOR]
        INC     AX
        CMP     AX,DS:[MAX_RECORDS]
        JE      DOWN_FUNCTION_EXIT_1
        INC     DS:[CURRENT_CURSOR]
        CALL    OVER1
        CMP     DS:[CURRENT_CURSOR_SCR],17
        JE      DOWN_FUNCTION_SHIFT_DOWN_1
        INC     DS:[CURRENT_CURSOR_SCR]
        JMP     SHORT   DOWN_FUNCTION_EXIT
DOWN_FUNCTION_SHIFT_DOWN_1:
        INC     DS:[CURRENT_SELECTOR]
        MOV     CX,1
        STC
        CALL    SHIFT
        MOV     AX,DS:[CURRENT_CURSOR]
        CALL    DIRBUF2CONVBUF
        MOV     DS:[CURY],22
        CALL    PRINT_FILE_STRING
DOWN_FUNCTION_EXIT:
        CALL    OVER1
DOWN_FUNCTION_EXIT_1:
        POP     CX
        POP     AX
        RET
ENDP
UP_FUNCTION     PROC;(IN:DS=DATA)
        PUSH    AX
        PUSH    CX
        ASSUME  DS:DATA
        CMP     DS:[CURRENT_CURSOR],0
        JE      UP_FUNCTION_EXIT_1
        DEC     DS:[CURRENT_CURSOR]
        CALL    OVER1
        CMP     DS:[CURRENT_CURSOR_SCR],0
        JE      UP_FUNCTION_SHIFT_UP_1
        DEC     DS:[CURRENT_CURSOR_SCR]
        JMP     SHORT   UP_FUNCTION_EXIT
UP_FUNCTION_SHIFT_UP_1:
        DEC     DS:[CURRENT_SELECTOR]
        MOV     CX,1
        CLC
        CALL    SHIFT
        MOV     AX,DS:[CURRENT_CURSOR]
        CALL    DIRBUF2CONVBUF
        MOV     DS:[CURY],5
        CALL    PRINT_FILE_STRING
UP_FUNCTION_EXIT:
        CALL    OVER1
UP_FUNCTION_EXIT_1:
        POP     CX
        POP     AX
        RET
ENDP
PGUP_FUNCTION   PROC;(IN DS:DATA)
        PUSH    AX
        MOV     AX,DS:[CURRENT_SELECTOR]
        OR      AX,AX
        JZ      PGUP_FUNCTION_EXIT
        SUB     AX,18
        MOV     DS:[CURRENT_SELECTOR],AX
        JNS     PGUP_FUNCTION_EXIT_1
        MOV     DS:[CURRENT_SELECTOR],0
PGUP_FUNCTION_EXIT_1:
        CALL    OVER1
        CALL    PRINT_CURRENT_DIR_CONTENTS
PGUP_FUNCTION_EXIT:
        POP     AX
        RET
ENDP
PGDOWN_FUNCTION PROC;(IN DS:DATA)
        PUSH    AX
        MOV     AX,DS:[CURRENT_SELECTOR]
        ADD     AX,18
        CMP     AX,DS:[MAX_RECORDS]
        JAE     PGDOWN_FUNCTION_EXIT
        MOV     DS:[CURRENT_SELECTOR],AX
        CALL    OVER1
        CALL    PRINT_CURRENT_DIR_CONTENTS
PGDOWN_FUNCTION_EXIT:
        POP     AX
        RET
ENDP
F9_FUNCTION     PROC;(IN-DS=DATA)
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    BP
        PUSH    DS
        CALL    PRINT_DRIVES_WIN
F9_F_A:
        XOR     AX,AX
        MOV     AL,DS:[CURRENT_DRIVE]
        MOV     BX,AX
        MOV     DX,3
        MUL     DX
        MOV     DX,DS: WORD PTR [QUANT_FDISKS]
        ADD     AX,DS:[FIRST_DISK_COORDX]
        MOV     BP,AX
F9_F_NEXT:
        MOV     AX,BP
        CALL    OVER2
F9_F_AGAIN:
        CALL    INKEY
        CMP     AL,ESCAPE_KEY
        JE      F9_F_EXIT
        CMP     AL,LEFT_KEY
        JE      F9_F_LEFT
        CMP     AL,RIGHT_KEY
        JE      F9_F_RIGHT
        CMP     AL,ENTER_KEY
        JNE     F9_F_AGAIN
        MOV     DS:[CURRENT_DRIVE],BL
        MOV     DL,BL
        CALL    SET_DRIVE
        MOV     DS:[CURRENT_SELECTOR],0
        STC
F9_F_EXIT:
        MOV     AX,BP
        PUSHF
        CALL    OVER2
        CALL    PRINT_DRIVES_WIN
        POPF
        POP     DS
        POP     BP
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET
F9_F_LEFT:
        CALL    F9_LEFT
        JNC     F9_F_AGAIN
        JMP     SHORT   F9_F_NEXT
F9_F_RIGHT:
        CALL    F9_RIGHT
        JNC     F9_F_AGAIN
        JMP     SHORT   F9_F_NEXT
F9_LEFT PROC
        MOV     CL,BL
        MOV     CH,DL
        CMP     CL,2
        JA      F9_LEFT_1
        OR      CH,CH
        JZ      F9_LEFT_EXIT
        OR      CL,CL
        JZ      F9_LEFT_EXIT
        CMP     CH,CL
        JZ      F9_LEFT_1
        XOR     CL,CL
        JMP     SHORT   F9_LEFT_11
F9_LEFT_1:
        DEC     CL
F9_LEFT_11:
        MOV     BL,CL
F9_LEFT_2:
        MOV     AX,BP
        CALL    OVER2
        SUB     BP,3
        STC
F9_LEFT_EXIT:
        RET
ENDP
F9_RIGHT        PROC
        MOV     CH,DH
        MOV     CL,BL
        INC     CL
        SUB     CH,DL
        ADD     CH,2
        CMP     CL,CH
        JAE     F9_RIGHT_EXIT_2
        INC     CL
        CMP     CL,2
        JNZ     F9_RIGHT_EXIT_1
        ADD     CL,2
        SUB     CL,DL
F9_RIGHT_EXIT_1:
        DEC     CL
        MOV     BL,CL
        MOV     AX,BP
        CALL    OVER2
        ADD     BP,3
        STC
F9_RIGHT_EXIT_2:
        RET
ENDP
ENDP
PRINT_CURRENT_DIR_CONTENTS      PROC
        PUSH    AX
        PUSH    CX
        PUSH    DS
        MOV     DS,DATS
        MOV     CX,18
        STC
        CALL    SHIFT
        CMP     DS:[MAX_RECORDS],0
        JNZ     P_C_D_C_4
        CALL    PRINT_NOTFOUND_WIN
P_C_D_C_5:
        CALL    F9_FUNCTION
        JNC     P_C_D_C_5
        STC
        JMP     SHORT   P_C_D_C_6
P_C_D_C_4:
        MOV     DS:[CURRENT_CURSOR_SCR],0
        MOV     DS:[CURY],5
        MOV     AX,DS:[CURRENT_SELECTOR]
        MOV     DS:[CURRENT_CURSOR],AX
P_C_D_C_1:
        CALL    DIRBUF2CONVBUF
        CALL    PRINT_FILE_STRING
        INC     DS:[CURY]
        INC     AX
        CMP     AX,DS:[MAX_RECORDS]
        JAE     P_C_D_C_2
        LOOP    P_C_D_C_1
P_C_D_C_2:
        CALL    OVER1
P_C_D_C_6:
        POP     DS
        POP     CX
        POP     AX
        RET
ENDP
PRINT_FILE_STRING       PROC;(IN:DS=DATA)
        PUSH    AX
        PUSH    SI
        PUSH    DS
        ASSUME  DS:DATA
        MOV     DS,DATS
        MOV     AH,BYTE PTR DS:[CURY]
        MOV     AL,1
        CALL    AT
        LEA     SI,NAME_
        CALL    WRITE_WORD
        MOV     AL,14
        CALL    AT
        LEA     SI,DS:LEN
        CALL    WRITE_WORD
        MOV     AL,25
        CALL    AT
        LEA     SI,DATE
        CALL    WRITE_WORD
        MOV     AL,36
        CALL    AT
        LEA     SI,TIME
        CALL    WRITE_WORD
        MOV     AL,45
        CALL    AT
        TEST    DS:[FLAGS],1
        JZ      P_F_S_HID
        LEA     SI,RONLY
        CALL    WRITE_WORD
        TEST    DS:[FLAGS],110B
        JZ      P_F_S_EXIT
        MOV     AL,','
        CALL    WRITE_SYMBOL
P_F_S_HID:
        TEST    DS:[FLAGS],10B
        JZ      P_F_S_SYS
        LEA     SI,HIDDENF
        CALL    WRITE_WORD
        TEST    DS:[FLAGS],100B
        JZ      P_F_S_EXIT
        MOV     AL,','
        CALL    WRITE_SYMBOL
P_F_S_SYS:
        TEST    DS:[FLAGS],100B
        JZ      P_F_S_EXIT
        LEA     SI,SYSTEMF
        CALL    WRITE_WORD
P_F_S_EXIT:
        POP     DS
        POP     SI
        POP     AX
        RET
ENDP
OVER1   PROC
        PUSH    AX
        PUSH    SI
        PUSH    DS
        ASSUME  DS:TABLES,ES:DATA
        MOV     ES,DATS
        MOV     DS,TABS
        LEA     SI,DS:CURSOR1_TBL
        MOV     AX,ES:[CURRENT_CURSOR_SCR]
        ADD     AX,5
        MOV     DS:[SI+4],AX
        CALL    INVERT_SCR
        POP     DS
        POP     SI
        POP     AX
        RET
ENDP
OVER2   PROC;(IN-AX=AT_X)
        PUSH    SI
        PUSH    DS
        ASSUME  DS:TABLES
        MOV     DS,TABS
        LEA     SI,DS:CURSOR2_TBL
        MOV     DS:[SI+6],AX
        CALL    INVERT_SCR
        POP     DS
        POP     SI
        RET
ENDP
INVERT_SCR      PROC;(IN-DS:SI=TABLE OF SCREEN)
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DI
        PUSH    DS
        PUSH    ES
        MOV     AX,DS:[SI+4]
        MOV     BX,CS:[VTBL+6]
        MUL     BX
        ADD     AX,DS:[SI+6]
        SHL     AX,1
        SHL     BX,1
        MOV     DX,DS:[SI+2]
        MOV     CX,DS:[SI]
        LES     DI,DWORD PTR CS:[VTBL]
        ADD     DI,AX
        MOV     SI,DI
        PUSH    ES
        POP     DS
INVERT_SCR_2:
        PUSH    CX
        PUSH    DI
        MOV     CX,DX
INVERT_SCR_1:
        LODSW
        ROR     AH,1
        ROR     AH,1
        ROR     AH,1
        ROR     AH,1
        STOSW
        LOOP    INVERT_SCR_1
        POP     DI
        POP     CX
        ADD     DI,BX
        MOV     SI,DI
        LOOP    INVERT_SCR_2
        POP     ES
        POP     DS
        POP     DI
        POP     SI
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET
ENDP
DIRBUF2CONVBUF  PROC;(IN-AX=RECORD)
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DI
        PUSH    DS
        PUSH    ES
        ASSUME  DS:DIR,ES:DATA
        MOV     DS,DIRS
        MOV     ES,DATS
        MOV     CL,5
        SHL     AX,CL
        MOV     SI,AX
        LEA     DI,DATA:FLAGS
        LODSB
        STOSB
        MOV     AH,AL
        PUSH    AX
        CALL    COPY_TIME
        CALL    COPY_DATE
        CALL    COPY_LEN
        LEA     DI,DATA:NAME_
        POP     AX
        MOV     CX,13
DIRBUF2CONVBUF11:
        LODSB
        TEST    AH,10000B
        JNZ     DIRBUF2CONVBUF12
        CALL    TO_LOW_REG
DIRBUF2CONVBUF12:
        STOSB
        LOOP    DIRBUF2CONVBUF11
        POP     ES
        POP     DS
        POP     DI
        POP     SI
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET
ENDP
COPY_TIME       PROC
        PUSH    CX
        LEA     DI,DATA:TIME
        LODSW
        XOR     CX,CX
        PUSH    AX
        MOV     CL,5
        SHR     AX,CL
        PUSH    AX
        MOV     CL,6
        SHR     AX,CL
        MOV     CL,2
        CALL    HW2DA
        MOV     AL,':'
        STOSB
        POP     AX
        AND     AX,3FH
        MOV     CL,2
        CALL    HW2DA
        MOV     AL,':'
        STOSB
        POP    AX
        AND    AX,1FH
        SHL    AX,1
        MOV    CL,2
        CALL   HW2DA
        XOR    AL,AL
        STOSB
        POP     CX
        RET
ENDP
COPY_DATE       PROC
        PUSH    CX
        LEA     DI,DATA:DATE
        LODSW
        XOR     CX,CX
        MOV     CL,7
        ROL     AX,CL
        PUSH    AX
        MOV     CL,4
        ROL     AX,CL
        PUSH    AX
        MOV     CL,12
        SHR     AX,CL
        MOV     CL,2
        CALL    HW2DA
        MOV     AL,'-'
        STOSB
        POP     AX
        AND     AX,0FH
        MOV     CL,2
        CALL    HW2DA
        MOV     AL,'-'
        STOSB
        POP     AX
        AND     AX,7FH
        ADD     AX,1980
        MOV     CL,4
        CALL    HW2DA
        XOR     AL,AL
        STOSB
        POP     CX
        RET
ENDP
COPY_LEN        PROC
        PUSH    SI
        PUSH    DS
        LEA     DI,DATA:LEN
        TEST    [FLAGS],1000B
        JNZ     COPY_LEN_VOL
        TEST    [FLAGS],10000B
        JNZ     COPY_LEN_DIR
        ADD     DI,2
        PUSH    DI
        LODSW
        MOV     DX,AX
        LODSW
        XCHG    AX,DX
        CMP     BYTE PTR DS:[SI-1],26H
        JBE     COPY_LEN_SMALL
        ADD     SI,2
        CALL    CONV_KILO
        XOR     AL,AL
        STOSB
        POP     SI
        MOV     DI,SI
        SUB     DI,2
        PUSH    DI
        PUSH    ES
        POP     DS
        CALL    SETPOINT
        DEC     DI
        MOV     AX,'K'
        STOSW
        POP     SI
        JMP   SHORT     COPY_LEN_11
COPY_LEN_SMALL:
        CALL    CONV_BYTES
        XOR     AL,AL
        STOSB
        POP     SI
        MOV     DI,SI
        SUB     DI,2
        PUSH    DI
        PUSH    ES
        POP     DS
        CALL    SETPOINT
        POP     SI
COPY_LEN_11:
        CALL    CONFIRM_LEN
COPY_LEN_12:
        POP     DS
        POP     SI
        ADD     SI,4
        RET
COPY_LEN_VOL:
        ASSUME  DS:DATA
        MOV     DS,DATS
        LEA     SI,VOLUME
        JMP     SHORT   COPY_LEN_DIR1
COPY_LEN_DIR:
        ASSUME  DS:DATA
        MOV     DS,DATS
        LEA     SI,DIRECTORY
COPY_LEN_DIR1:
        MOV     CX,11
        REP     MOVSB
        JMP     SHORT   COPY_LEN_12
ENDP
CONFIRM_LEN     PROC
        PUSHF
        PUSH    BX
        PUSH    CX
        PUSH    SI
        PUSH    DI

        PUSH    DI
        SUB     DI,SI
        DEC     DI
        MOV     CX,DI
        SUB     DI,10
        NEG     DI
        MOV     BX,DI
        POP     DI

        PUSH    BX
        PUSH    CX
        DEC     DI
        DEC     DI
        ADD     SI,9
        XCHG    SI,DI
        STD
        POP     CX
        REP     MOVSB
        POP     CX
        MOV     AL,' '
        REP     STOSB
        POP     DI
        POP     SI
        POP     CX
        POP     BX
        POPF
        RET
ENDP
CONV_KILO       PROC
        PUSH    BX
        PUSH    CX
        MOV     CL,10
        ROR     DX,CL
        SHR     AX,CL
        MOV     BX,DX
        AND     BX,1111111111000000B
        AND     DX,111111B
        OR      AX,BX
        POP     CX
        POP     BX
ENDP
CONV_BYTES      PROC
        MOV     BX,10000
        DIV     BX
        PUSH    DX
        OR      AX,AX
        JZ      CONV_BYTES_1
        CLC
        CALL    HW2D
        STC
CONV_BYTES_1:
        POP     AX
        JMP     SHORT   HW2D
ENDP
INCLUDE HW2D.LIB
SET_CURSOR      PROC;(IN AX-CUR)
        PUSH    AX
        PUSH    BX
        PUSH    DX
        MOV     DX,AX
        MOV     AH,2
        XOR     BH,BH
        INT     10H
        POP     DX
        POP     BX
        POP     AX
        RET
ENDP
GET_CURSOR      PROC;(OUT AX-CUR)
        PUSH    BX
        PUSH    CX
        PUSH    DX
        XOR     BH,BH
        MOV     AH,3
        INT     10H
        MOV     AX,DX
        POP     DX
        POP     CX
        POP     BX
        RET
ENDP
SET_DRIVE       PROC;(IN DL-DRIVE)
        PUSH    AX
        MOV     AH,0EH
        INT     21H
        POP     AX
        RET
ENDP
GET_DRIVE       PROC;(OUT AL-DRIVE)
        MOV     AH,19H
        INT     21H
        RET
ENDP
TO_LOW_REG      PROC
        CMP     AL,'A'
        JB      TO_LOW_REG_EXIT
        CMP     AL,'Z'
        JBE     TO_LOW_REG_SET
        CMP     AL,80H
        JB      TO_LOW_REG_EXIT
        CMP     AL,8FH
        JBE     TO_LOW_REG_SET
        CMP     AL,9FH
        JA      TO_LOW_REG_EXIT
        ADD     AL,30H
TO_LOW_REG_SET:
        ADD     AL,20H
TO_LOW_REG_EXIT:
        RET
ENDP
SETPOINT        PROC
        PUSH    AX
        PUSH    CX
        PUSH    DX
        PUSH    SI

        PUSH    SI
        XOR     CX,CX
        MOV     DX,CX
        DEC     CX
SETPOINT_1:
        INC     CX
        LODSB
        OR      AL,AL
        JNZ     SETPOINT_1
        POP     SI
        MOV     AX,3
        XCHG    AX,CX
        DIV     CX
        OR      DX,DX
        JNZ     SETPOINT_2
        MOV     DX,CX
        DEC     AX
SETPOINT_2:
        MOV     CX,DX
        REP     MOVSB
        OR      AX,AX
        JZ      SETPOINT_4
        MOV     CX,AX
SETPOINT_3:
        MOV     AL,'.'
        STOSB
        PUSH    CX
        MOV     CX,3
        REP     MOVSB
        POP     CX
        LOOP    SETPOINT_3
SETPOINT_4:
        XOR     AL,AL
        STOSB

        POP     SI
        POP     DX
        POP     CX
        POP     AX
        RET
ENDP
PRINT_HELP_WIN  PROC
        PUSH    SI
        PUSH    DI
        PUSH    DS
        PUSH    ES
        ASSUME  DS:TABLES
        MOV     DS,TABS
        PUSH    DS
        POP     ES
        LEA     SI,HELPWIN
        LEA     DI,HELPSCR
        JMP     SHORT     PRINT_WIN_AT
ENDP
PRINT_DONTREADY_WIN     PROC
        PUSH    SI
        PUSH    DI
        PUSH    DS
        PUSH    ES
        ASSUME  DS:TABLES
        MOV     DS,TABS
        PUSH    DS
        POP     ES
        LEA     SI,DONTREADYWIN
        LEA     DI,DONTREADYSCR
PRINT_WIN_AT:
        CALL    SPR2SCR
        CALL    KEYPAUSE
PRINT_WIN_AT1:
        CALL    SPR2SCR
        POP     ES
        POP     DS
        POP     DI
        POP     SI
        RET
ENDP
PRINT_READERROR_WIN     PROC
        PUSH    SI
        PUSH    DI
        PUSH    DS
        PUSH    ES
        ASSUME  DS:TABLES
        MOV     DS,TABS
        PUSH    DS
        POP     ES
        LEA     SI,READERRORWIN
        LEA     DI,READERRORSCR
        JMP     SHORT   PRINT_WIN_AT
ENDP
PRINT_CREDITS_WIN       PROC
        PUSH    SI
        PUSH    DI
        PUSH    DS
        PUSH    ES
        ASSUME  DS:TABLES
        MOV     DS,TABS
        PUSH    DS
        POP     ES
        LEA     SI,CREDITSWIN
        LEA     DI,CREDITSSCR
        JMP     PRINT_WIN_AT
ENDP
PRINT_NOTFOUND_WIN      PROC
        PUSH    SI
        PUSH    DI
        PUSH    DS
        PUSH    ES
        ASSUME  DS:TABLES
        MOV     DS,TABS
        PUSH    DS
        POP     ES
        LEA     SI,NOTFOUNDWIN
        LEA     DI,NOTFOUNDSCR
        JMP     PRINT_WIN_AT
ENDP
PRINT_DRIVES_WIN        PROC
        PUSH    SI
        PUSH    DI
        PUSH    DS
        PUSH    ES
        ASSUME  DS:TABLES
        MOV     DS,TABS
        PUSH    DS
        POP     ES
        LEA     SI,DRIVESWIN
        LEA     DI,DRIVESSCR
        JMP     PRINT_WIN_AT1
ENDP
SHIFT   PROC;(C=1 SHIFT UP)
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    BP
        MOV     AH,07H
        JNC     SHIFT_1
        MOV     AH,06H
SHIFT_1:
        MOV     BX,700H
        MOV     AL,CL
        MOV     CX,501H
        MOV     DX,160CH
        INT     10H
        MOV     CL,14
        MOV     DL,23
        INT     10H
        MOV     CL,25
        MOV     DL,34
        INT     10H
        MOV     CL,36
        MOV     DL,43
        INT     10H
        MOV     CL,45
        MOV     DL,78
        INT     10H
        POP     BP
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET
ENDP
CREATE_DRIVES_WIN        PROC;(IN DS=DATA)
        PUSH    AX
        PUSH    CX
        PUSH    DX
        PUSH    DI
        PUSH    BP
        PUSH    DS
        PUSH    ES
        ASSUME  DS:DATA,ES:TABLES
        MOV     DS,DATS
        MOV     ES,TABS
        MOV     AX,WORD PTR ES:[DRIVESWIN+6]
        MOV     ES,WINS
        MOV     DI,DRIVESWB

        MOV     CX,AX
        MOV     BP,CX
        ADD     CX,2
        MOV     AH,DS:[COLOR]
        MOV     AL,' '
        REP     STOSW
        MOV     AL,HILEFT
        STOSW
        MOV     CX,BP
        SUB     CX,6
        MOV     AL,HORIZ
        REP     STOSW
        MOV     AL,HIRIGHT
        STOSW
        MOV     CX,4
        MOV     AL,' '
        REP     STOSW
        MOV     AL,VERT
        STOSW
        CALL    CREATE_FLOPPY_DISKS
        CALL    CREATE_HARD_DISKS
        MOV     AL,VERT
        STOSW
        MOV     CX,4
        MOV     AL,' '
        REP     STOSW
        MOV     AL,LOLEFT
        STOSW
        MOV     CX,BP
        SUB     CX,6
        MOV     AL,HORIZ
        REP     STOSW
        MOV     AL,LORIGHT
        STOSW
        MOV     CX,BP
        ADD     CX,2
        MOV     AL,' '
        REP     STOSW
        POP     ES
        POP     DS
        POP     BP
        POP     DI
        POP     DX
        POP     CX
        POP     AX
        RET
ENDP
CREATE_FLOPPY_DISKS     PROC
        MOV     AL,DS:[QUANT_FDISKS]
        OR      AL,AL
        JZ      CREATE_FLOPPY_DISKS_EXIT
        MOV     DL,'A'
        XOR     CH,CH
        MOV     CL,AL
CREATE_FLOPPY_DISKS_1:
        MOV     AL,' '
        STOSW
        MOV     AL,DL
        STOSW
        MOV     AL,' '
        STOSW
        INC     DL
        LOOP    CREATE_FLOPPY_DISKS_1
CREATE_FLOPPY_DISKS_EXIT:
        RET
ENDP
CREATE_HARD_DISKS       PROC
        MOV     AL,DS:[QUANT_HDISKS]
        OR      AL,AL
        JZ      CREATE_HARD_DISKS_EXIT
        MOV     DL,'C'
        XOR     CH,CH
        MOV     CL,AL
CREATE_HARD_DISKS_1:
        MOV     AL,' '
        STOSW
        MOV     AL,DL
        STOSW
        MOV     AL,' '
        STOSW
        INC     DL
        LOOP    CREATE_HARD_DISKS_1
CREATE_HARD_DISKS_EXIT:
        RET
ENDP
INCLUDE SPR2SCR.LIB
INCLUDE WW.LIB
INCLUDE IO.LIB
INCLUDE HW2DA.LIB

DATA_SEG        DW      DATA
TABLES_SEG      DW      TABLES
PATH_SEG        DW      PATH
DIR_SEG         DW      DIR
OLDSCR_SEG      DW      OLD
WINDS_SEG       DW      WIN

INCLUDE VTABLE.TXT

ENDS
DATA    SEGMENT PARA    PUBLIC  'DATA'

CREDITS_STRING  DB      'DIR Viewer 1.00 by Pavel A. Skrylev (C)1996',0dh,0ah,0
PRINT_READ_ERROR        DB      'File DIRVIEW.WIN is not read',0dh,0ah,0
WINFNAME        DB      'DIRVIEW.WIN',0
FILE_MASK       DB      '*.*',0
RONLY           DB      'Только Чтение',0
HIDDENF         DB      'Скрытый',0
SYSTEMF         DB      'Системный',0
VOLUME          DB      '     Метка',0
DIRECTORY       DB      'Директория',0

OLD_23H_OFF     DW      0
OLD_23H_SEG     DW      0
OLD_24H_OFF     DW      0
OLD_24H_SEG     DW      0

COLOR           DB      078H
CURRENT_DRIVE   DB      0
QUANT_HDISKS    DB      0
QUANT_FDISKS    DB      0
QUANT_LDISKS    DB      0
FLAGS   DB      0
TIME    DB      9       DUP     (0)
DATE    DB      11      DUP     (0)
LEN     DB      11      DUP     (0)
NAME_   DB      13      DUP     (0)
CURY    DW      0
FIRST_DISK_COORDX      DW      0
CURRENT_CURSOR_SCR      DW      0
CURRENT_CURSOR          DW      0
CURRENT_SELECTOR        DW      0
MAX_RECORDS             DW      0
DTA_ADRESS              DW      0,0
FREE_DIGITAL            DB      12      DUP     (0)
ENDS

TABLES  SEGMENT PARA    PUBLIC  'TABLES'
HEADWIN         DD      WIN:0
                DW      HEADLENY,HEADLENX,0,0
                DB      0,0
                DW      0;?
DONTREADYWIN    DD      WIN:DONTWB
                DW      DONTLENY,DONTLENX,DONTATY,DONTATX
                DB      0,0
                DW      0
READERRORWIN    DD      WIN:ERRORWB
                DW      ERRORLENY,ERRORLENX,ERRORATY,ERRORATX
                DB      0,0
                DW      0
HELPWIN         DD      WIN:HELPWB
                DW      HELPLENY,HELPLENX,HELPATY,HELPATX
                DB      0,0
                DW      0
CREDITSWIN      DD      WIN:CREDITSWB
                DW      CREDITSLENY,CREDITSLENX,CREDITSATY,CREDITSATX
                DB      0,0
                DW      0
NOTFOUNDWIN     DD      WIN:NOTFOUNDWB
                DW      NOTFOUNDLENY,NOTFOUNDLENX,NOTFOUNDATY,NOTFOUNDATX
                DB      0,0
                DW      0
DRIVESWIN       DD      WIN:DRIVESWB
                DW      DRIVESLENY,?,DRIVESATY,?
                DB      0,0
                DW      0
HEADSCR         DW      0
                DW      0,0,0
                DB      0
                DB      7       DUP     (0);?
DONTREADYSCR    DW      0
                DW      0,0,0
                DB      0
                DB      7       DUP     (0)
READERRORSCR    DW      0
                DW      0,0,0
                DB      0
                DB      7       DUP     (0)
HELPSCR         DW      0
                DW      0,0,0
                DB      0
                DB      7       DUP     (0)
CREDITSSCR      DW      0
                DW      0,0,0
                DB      0
                DB      7       DUP     (0)
NOTFOUNDSCR     DW      0
                DW      0,0,0
                DB      0
                DB      7       DUP     (0)
DRIVESSCR       DW      0
                DW      0,0,0
                DB      0
                DB      7       DUP     (0)
CURSOR1_TBL     DW      1
                DW      78,?,1
                DB      0
                DB      7       DUP     (0)
CURSOR2_TBL     DW      1
                DW      3,DRIVESATY+2,?
                DB      0
                DB      7       DUP     (0)
ENDS
END  START

