* $$ JOB JNM=VC01
* $$ LST LST=SYSLST,CLASS=A,RBM=999999
// JOB VC01
*
* Now assemble the main routine
*
ASSGN SYS001,X'362'
ASSGN SYS002,X'362'
ASSGN SYS003,X'362'
ASSGN SYS004,X'362'
ASSGN SYSLNK,X'365'
ASSGN SYSIPT,X'00C'
// DLBL IJSYSLN,'DOS/VS.SYSLNK.FILE',0,SD                               
// EXTENT SYSLNK,WORK03,1,0,1,15449      
// OPTION LINK
 PHASE VC01
// EXEC ASSEMBLY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* PROGRAM: VCON                                                     *
*                                                                   *
* (C) 1994 PF1:HELP   1600 WARWICK CT., WHEELING IL  60090-7116     *
*          (708) 459-5294   FAX/BBS: (708) 465-9520                 *
*                                                                   *
*      THIS ROUTINE IS A SAMPLE OF WHAT WAS/WILL BE AVAILABLE IN    *
*      THE "VSE INTERNALS" NEWSLETTER, PUBLISHED BI-MONTHLY.        *
*      MACROS AND SUBROUTINES INDICATED IN THIS ROUTINE ARE         *
*      AVAILABLE TO SUBSCRIBERS AT NO CHARGE.                       *
*                                                                   *
* PURPOSE: PROVIDE A SIMPLE CONSOLE PROGRAM AS A VTAM APPLICATION.  *
*                                                                   *
* INPUT: AN OPTIONAL PASSWORD (1-8 CHARACTERS) CAN BE PASSED IN     *
*      THE EXECUTION PARAMETER.  IF NONE, IS GIVE, THEN THE         *
*      PASSWORD "VCON" WILL BE ASSUMED.                             *
*                                                                   *
* SUBROUTINES: PRINT, WTO, PREPCMD, DSPLHEX, DUMP, VALIDADR         *
*                                                                   *
* NON-IBM MACROS:  BEGIN, $SYSDATE, $SYSTIME, SUBRT, DISPLAY, SNAP  *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* REGEQU IS NOT AVAILABLE ON DOS/VS
*         YREGS
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
SUBPOOL  EQU   0
VCON     BEGIN BASE2=R11,VRM=1.1
*
* GET THE PASSWORD FROM EXEC PARM.  IF NONE, THEN DEFAULT TO "VCON"
*
         CLI   1(R4),0                 SECRET WORD PASSED ?
         BZ    VC0010                  NO...USE THE DEFAULT
         MVC   PREPPARM(82),0(R4)      GET THE PASSED PARM
         CALL  PREPCMD,(PREPPARM)      CONVERT TO UPPER CASE, ETC.
         LH    R2,PREPPARM             GET THE LENGTH
         BCTR  R2,0                    RELATIVE TO ZERO
         MVC   PWD(8),=CL8' '          INIT THE PASSWORD
         EX    R2,VC0030               GET THE PASSWORD
*
* OPEN THE ACB
*
VC0010   SR    R15,R15                 REQUIRED FOR VSE/ESA
         OPEN  VACB                    OPEN THE ACB
         CLI   VACB+23,0               DID IT WORK ?
         BZ    VC0040                  YES...PROCEED
         DISPLAY 'ERROR OPENING ACB'   TELL THE OPERATOR
VC0020   XC    PARM1,PARM1             INIT THE WORK AREA
         MVC   PARM1+3(1),VACB+23      SAVE RETURN CODE
         CALL  DSPLHEX,(PARM1)         MAKE IT DISPLAYABLE
         MVC   MSGRC(4),PARM1+8        GET THE RETURN CODE
         DISPLAY ERMSG1                DISPLAY THE ERROR
         B     VC0060                  AND EXIT
VC0030   MVC   PWD(0),PREPPARM+2       GET THE EXECUTABLE VALUE
*
* ACTIVATE THE USE OF THE LOGON EXIT
*
VC0040   LA    R5,PRPL                 POINT TO THE RPL
         USING IFGRPL,R5               MAP TO THE LAYOUT
         SETLOGON RPL=(R5),OPTCD=START SETUP LOGON PROCESSING
         LTR   R15,R15                 DID IT WORK ?
         BNZ   VC0020                  NO...DUMP AND EXIT
*
* THE FOLLOWING SMALL ROUTINE JUST WAITS FOR A LOGON AND PASSES
* CONTROL TO THE CONSOLE MANAGEMENT CODE.
*
VC0050   DISPLAY 'VCON:  WAITING FOR LOGIN'
         XC    LOGONECB,LOGONECB       INIT THE ECB
         WAITM LOGONECB,TPENDECB       WAIT FOR EITHER
         CLC   TPENDECB(4),=F'0'       BRING DOWN THE SYSTEM ?
         BZ    VC0070                  NO, LET'S DO IT
*
* WHEN A SHUTDOWN IS REQUIRED, LET'S CLOSE THE ACB
*
VC0060   DISPLAY 'VCON:  SHUTDOWN'     INDICATE WHAT WE'RE DOING
         CLOSE VACB                    CLOSE THE ACB
         EOJ   RC=0                    AND TERMINATE
*
* THIS AREA IS WHERE CONSOLE SIMULATION OCCURS
*
VC0070   DS    0H                      BEGINNING OF THE EXIT
         L     R5,=A(LOGONRPL)         POINT TO THE RPL
         OI    RPLRH3,RPLBB+RPLEB      INITIAL SEND WILL BE FULL
         L     R1,X'14'                POINT TO THE COMREG
         MVC   SDATE(8),0(R1)          AND UPDATE THE DATE
         MVC   DATE(8),0(R1)           IN BOTH SCREENS
*
* DISPLAY A SCREEN OF DATA
*
         LA   R8,PWDSCRN               GET THE SCREEN AREA
         LA   R9,SCRLN                 GET THE SCREEN LENGTH
         OI   RPLRH3,RPLBB             INIT BRACKET SEQUENCE
         MVC  LUINFO(8),LUID           READY THE LU NAME
         BAL  R10,VC0110               OUTPUT A SCREEN OF DATA
         BAL  R10,VC0120               GET THE USER'S RESPONSE
         CLC   PREPPARM+2(8),PWD       PASSWORD MATCH ?
         BZ    VC0080                  YES...DISPLAY STD SCREEN
*
* BY ISSUING A CLSDST, A CINIT REJECTION OCCURS.
*
         L      R1,=A(TERMNAME)        POINT TO THE LUNAME
         MVC    PWDTERM(8),0(R1)       GET THE LUNAME
         DISPLAY PWDMSG                WARN THE OPERATOR
         B      VC0100
*
* THIS SECTION IS THE MAIN ROUTINE FOR CONSOLE TERMINAL I/O
*
VC0080   GETIME STANDARD               GET THE TIME OF DAY
         ST    R1,PARM1                HOLD THAT TIME
         MVC   TOD(9),HHMMSS           GET THE EDIT FIELD
         ED    TOD(9),PARM1            UNPACK/FORMAT TIME
         MVI   TOD,X'40'               RESET HYPHEN
*
* GET THE CONSOLE SCREEN BUFFER AND FILL IN THE 3270 MAP
*
         MODESET KEY=ZERO              MODIFY PROTECTION KEY TO 0
         ASYSCOM ,                     POINT TO SYSCOM
         USING SYSCOM,R1               MAP BACK TO LAYOUT
         ICM   R6,7,IJBCONSP+1         POINT TO CONSOLE TABLE
         USING CRTTAB,R6               MAP BACK TO LAYOUT
         L     R7,ACRTSAV              POINT TO CRT SAVEAREA
         USING CRTSAV,R7               MAP BACK TO LAYOUT
         LH    R9,SCTLINES             GET THE NUMBER OF LINES
         L     R8,ASCRIMG              POINT TO START OF CURRENT BFR
         LA    R2,LINES+3              POINT TO OUTPUT AREA
         TM    CRTFLG1,CRTREDSP        IS OPERATOR IN REDISPLAY MODE?
         BZ    VC0085                  NO..DON'T BOTHER
         L     R8,SCREENAD             GET THE SCREEN SAVE ADDRESS
VC0085   LA    R8,4(R8)                POINT TO START OF LAST BUFFER
VC0090   MVC   0(81,R2),0(R8)          GET A LINE
         LA    R8,81(R8)               POINT TO THE NEXT LINE
         LA    R2,84(R2)               POINT TO THE NEXT SPOT
         BCT   R9,VC0090               AND DO IT SOME MORE
*        MODESET KEY=USER              RESET MODE, JUST IN CASE
*
* HERE WAS SEND A SCREEN, GET A RESPONSE, AND COMPARE THE DATA
*
         LA    R8,SCR                  GET THE SCREEN AREA
         LA    R9,SCRLEN               GET THE SCREEN LENGTH
         BAL   R10,VC0110              OUTPUT A SCREEN OF DATA
         XC   RCVECB,RCVECB            INIT THE ECB
         SETIME 20,RCVECB              EVERY 20 SECONDS, REFRESH
         BAL   R10,VC0120              GET THE USER'S RESPONSE
         CLI   KEYPRESS,X'F3'          PF3 REQUEST ?
         BZ    VC0100                  YES...QUIT
         CLI   KEYPRESS,X'C3'          PF15 REQUEST ?
         BZ    VC0100                  YES...QUIT
         CLI   KEYPRESS,X'6D'          CLEAR KEY PRESSED ?
         BZ    VC0100                  YES...QUIT
         CLC   PREPPARM+2(5),=CL5'QUIT'  GET OUT ?
         BZ    VC0100                  YES...DO IT
         CLC   PREPPARM+2(5),=CL5'LOGOFF' GET OUT ?
         BZ    VC0100                  YES...DO IT
         CLC   PREPPARM+2(8),=C'SHUTDOWN'
         BZ    VC0100                  YES...DO IT
*
* PROCESS A VSE COMMAND
*
         CLI   PREPPARM+1,0            NOTHING TO PROCESS?
         BZ    VC0080                  YES...PROCEED
         MVC   VSELEN(2),PREPPARM      GET THE LENGTH OF THE COMMAND
         LA    R1,VSEPARM              POINT TO THE COMMAND PARM
         XR    R0,R0                   JUST IN CASE
         SVC   30                      PASS COMMAND TO VSE
         LTR   R15,R15                 DID IT WORK ?
         BZ    VC0080                  NO...ERROR
         MVI   KBD,BEEP                TURN ON THE ALARM
         MVC   ERROR(L'VSERR),VSERR    INSERT THE ERROR MESSAGE
         B     VC0080                  NO...GO PROCESS
*
* DROP THE TERMINAL FROM ITS SESSION
*
VC0100   L      R7,=A(PNIB)            POINT TO THE NIB
         CLSDST RPL=(R5),OPTCD=SYN,ACB=VACB,NIB=(R7)
         CLC    PREPPARM+2(8),=C'SHUTDOWN'
         BZ     VC0060                 IF SHUTDOWN REQUEST, CLOSE ACB
         B      VC0050                 AND WAIT FOR ANOTHER LOGIN
*
* THIS ROUTINE OUTPUTS A SCREEN OF DATA TO THE TERMINAL
*
VC0110   DS  0H
         SEND RPL=(R5),AREA=(R8),RECLEN=(R9),STYPE=REQ,                X
               CONTROL=DATA,CHAIN=ONLY,CHNGDIR=CMD,                    X
               OPTCD=(CA,SYN,NFMHDR,NUSERRH,LMPEO),                    X
               POST=RESP,ECB=INTERNAL,RESPOND=(NEX,FME,NRRN)
         LTR   R15,R15                 DID IT WORK ?
         BZ    VC0115                  YES...PROCEED
         DISPLAY 'SEND FAILED'         INDICATE THE PROBLEM
         B     VC0100                  AND ABORT
VC0115   DS    0H
         MVI   KBD,NOBEEP              RESET ALARM, IF SET
         XC    PREPPARM,PREPPARM       RESET WORK BUFFER
         XC    BUFFER(100),BUFFER      RESET LAST ENTRY PASSED
         MVI   ERROR,C' '              RESET THE ERROR MESSAGE AREA
         MVC   ERROR+1(35),ERROR       TO BLANKS, IF IT WAS SET
         MVI   USER,C' '               RESET THE USER AREA
         MVC   USER+1(78),USER         TO BLANKS
         LTR  R15,R15                  DID IT WORK ?
         BZR  R10                      YES...PROCEED
         ST   R15,PARM1                SAVE THIS VALUE
         CALL DSPLHEX,(PARM1)          GET THE DATA
         MVC  SRC(2),PARM1+10          INSERT INTO THE MESSAGE
         DISPLAY SMSG                  DISPLAY THE ERROR
         B    VC0100                   AND EXIT
*
* THIS ROUTINE WILL ACCEPT A RESPONSE FROM THE KEYBOARD
*
VC0120   DS  0H
         RECEIVE RPL=(R5),AREA=BUFFER,AREALEN=L'BUFFER,                X
               OPTCD=(ASY,ANY,CS,Q,TRUNC),ECB=RCVECB
         LTR   R15,R15                 DID IT WORK ?
         BZ    VC0125                  YES...PROCEED
         DISPLAY 'RECEIVE FAILED'      INDICATE THE PROBLEM
         B     VC0100                  AND ABORT
VC0125   DS    0H
         CHECK RPL=(R5)                WAIT FOR THE I/O
         LTR   R15,R15                 DID IT WORK ?
         BZ    VC0130                  YES...PROCEED
         DISPLAY 'RCV CHECK FAILED'    INDICATE THE PROBLEM
         B     VC0100                  AND ABORT
*
* THIS SECTION WILL PREPARE THE RECEIVED DATA FOR INQUIRY
*
VC0130   DS    0H
         MVC   PREPPARM(2),=H'80'      DEFAULT LENGTH SIZE
         MVC   PREPPARM+2(80),DATA     INSERT THE DATA
         LA    R2,80                   WE'LL DO THIS FOR 80 BYTES
         LA    R1,PREPPARM+2           POINT PAST THE LENGTH
VC0140   CLI   0(R1),0                 NULL INPUT FROM CONSOLE ?
         BNZ   VC0150                  NO...PROCEED
         MVI   0(R1),C' '              YES...REPLACE IT WITH BLANKS
VC0150   LA    R1,1(R1)                POINT TO THE NEXT BYTE
         BCT   R2,VC0140               AND LOOK FOR THE ENTIRE STRING
         CALL  PREPCMD,(PREPPARM)      CONVERT TO UPPER CASE, ETC.
*
* THIS SECTION WILL SETUP BRACKET PROTOCOL FOR THE NEXT SEND
*
         TM    RPLRH3,RPLBB+RPLEB      IS IT BB,EB ?
         BOR   R10                     YES...RETAIN THAT
         TM    RPLRH3,RPLBB            IS IT BB,NEB ?
         BO    VC0170                  YES...MAKE IT CENTRIST
         TM    RPLRH3,RPLEB            IS IT NBB,EB ?
         BO    VC0160                  YES...MAKE IT FULL
         BR    R10                     IT IS NBB,NEB - RETAIN
VC0160   OI    RPLRH3,RPLBB            TURN ON BEGIN BRACKET
         OI    RPLRH3,RPLEB            TURN ON END BRACKET
         BR    R10                     RETURN TO CALLER
VC0170   NI    RPLRH3,X'FF'-RPLBB      TURN OFF BEGIN BRACKET
         NI    RPLRH3,X'FF'-RPLEB      TURN OFF END BRACKET
         BR    R10                     RETURN TO CALLER
* ----------------------------------------------------------- *
         LTORG ,
*
WRTERASE EQU   X'F5'
BEEP     EQU   X'87'
NOBEEP   EQU   X'83'
*
* INITIAL PASSWORD SCREEN
*
PWDSCRN  DS   0H
         DC   AL1(WRTERASE),AL1(NOBEEP)
         DC   X'1140401D70'
LUID     DC   CL9' '
         DC   X'1140E2',C'VCON (V1.1)'
         DC   X'11C1C7'
SDATE    DC   CL8' '
         DC   X'114A5A1D78'
         DC   C'ENTER THE VCON PASSWORD:'
         DC   X'114E501D78',C'>',X'1D4C13',CL8' ',X'1D78',C'<'
SCRLN    EQU  *-PWDSCRN
*
* GENERAL CONSOLE SCREEN
*
SCR      DS    0H
         DC    AL1(WRTERASE)
KBD      DC    AL1(NOBEEP)
LINES    DC    X'1140401D60',CL79' '    ROW 1                           00195
         DC    X'11C1501D60',CL79' '    ROW 2                           00195
         DC    X'11C2601D60',CL79' '    ROW 3                           00195
         DC    X'11C3F01D60',CL79' '    ROW 4                           00195
         DC    X'11C5401D60',CL79' '    ROW 5                           00195
         DC    X'11C6501D60',CL79' '    ROW 6                           00196
         DC    X'11C7601D60',CL79' '    ROW 7                           00197
         DC    X'11C8F01D60',CL79' '    ROW 8                           00198
         DC    X'114A401D60',CL79' '    ROW 9                           00199
         DC    X'114B501D60',CL79' '    ROW 10                          00200
         DC    X'114C601D60',CL79' '    ROW 11                          00201
         DC    X'114DF01D60',CL79' '    ROW 12                          00202
         DC    X'114F401D60',CL79' '    ROW 13                          00203
         DC    X'1150501D60',CL79' '    ROW 14                          00204
         DC    X'11D1601D60',CL79' '    ROW 15                          00205
         DC    X'11D2F01D60',CL79' '    ROW 16                          00206
         DC    X'11D4401D60',CL79' '    ROW 17                          00207
         DC    X'11D5501D60',CL79' '    ROW 18                          00208
         DC    X'11D6601D60',CL79' '    ROW 19                          00209
         DC    X'11D7F01D60',CL79' '    ROW 20                          00210
         DC    X'11D9401D60',CL79' '    ROW 21                          00211
         DC    CL22'ENTER VCON COMMAND',X'1D68'   ROW 22                00212
ERROR    DC    CL36' '
DATE     DC    CL11' '                                                  00195
TOD      DC    CL9' ',X'1D4813'
         DC    X'115B601D4813'          ROW 23                          00213
USER     DC    CL79' '
         DC    X'115CF01D60',CL55' '    ROW 24                          00214
         DC    CL16'VCON (V1.1)'
LUINFO   DC    CL8' '
SCRLEN   EQU   *-SCR
*
VSEPARM  DS   0H                       USED TO PASS REQUEST TO SVC30
VSELEN   DS   H                        LENGTH OF VSE COMMAND
CMDADDR  DC   AL4(PREPPARM+2)          AREA WHERE COMMAND IS LOCATED
*
VSERR    DC   C'VSE COMMAND WAS NOT PASSED'
*
PWDMSG   DC   AL1(26),C'PASSWORD FAILURE: '
PWDTERM  DC   CL8' '
*
BUFFER   DS    0CL100                  BUFFER RETURNED FROM RECEIVE
KEYPRESS DS    X                       KEY PRESSED
INSBA    DS    XL2                     SBA WHERE CURSOR WAS
HEX11    DS    X                       X'11'
INSTART  DS    XL2                     SBA WHERE FIELD STARTED
DATA     DS    CL94                    DATA THAT WAS PASSED
*
PREPPARM DS    41H
HHMMSS   DC    X'2120207A20207A2020'
*
VACB     ACB  AM=VTAM,APPLID=APPLID,EXLST=EXLST,MACRF=LOGON
APPLID   DC   AL1(4),C'VCON'
EXLST    EXLST AM=VTAM,LOGON=LOGEXIT,SYNAD=SYNEXIT,LERAD=LEREXIT,      X
               TPEND=TPEXIT,RESP=RESPEXIT,LOSTERM=LOSTEXIT
PRPL     RPL  ACB=VACB,AM=VTAM
*
PWD      DC    CL8'VCON'               THE DEFAULT PASSWORD
RCVECB   TECB ,                        ECB USED FOR RECEIVE
LOGONECB DC    F'0'                    ECB INDICATES A LOGON OCCURRED
TPENDECB DC    F'0'                    ECB TO SHUTDOWN VCON
*
PARM1    DS    3F                      WORK PARAMETER
*
ERMSG1   DC    AL1(11),C'R15=X',X'7D'  USED TO DISPLAY ACB ERROR
MSGRC    DC    CL4' ',X'7D'
*
SMSG     DC   AL1(32)
STEXT    DC   C'CONSOLE:  SEND ERROR.  R15=X',X'7D'
SRC      DC   CL2' ',X'7D'
*
* < ----------------- LOGON EXIT ROUTINE ------------------ >
*
* AT THIS POINT, A CINIT HAS OCCURRED AND VTAM HAS SCHEDULED THIS
* PROGRAM TO BRANCH INTO THIS AREA OF CODE.
*
LOGEXIT  SUBRT TYPE=INITIAL,SAVE=LSAV  BEGIN THE EXIT
         LR   R6,R1                    GET THE PASSED PARM
*
* 0(R6) = ACB ADDRESS  4(R6) = SLU NAME  8(R6) = USERFLD IF SIMLOGON
* 12(R6) = LENGTH OF USER DATA   16(R6) = ADDR OF READ-ONLY RPL
* 20(R6) = CID OF SESSION TO BE ESTABLISHED WITH THE LU
*
         L    R9,0(R6)                 GET THE ACB ADDRESS
         L    R4,4(R6)                 GET THE SLUNAME
         L    R1,=A(LUID)              POINT TO PWD FIELD
         MVC  0(8,R1),0(R4)            MAKE LUNAME PART OF PWD SCREEN
*
* LET'S TELL THE OPERATOR EVERY TIME SOMEONE LOGS ON TO THIS APPL.
*
         MVC   TERMNAME(8),0(R4)       GET THE TERMINAL LU-NAME
         DISPLAY TERMMSG               LET THE OPERATOR KNOW WHO'S ON
         LA    R5,LOGONRPL             SETUP RPL BASE DSECT
         LA    R7,PNIB                 ADDRESS THE NIB LAYOUT
         USING IFGRPL,R5               MAP THE RPL LAYOUT
         USING ISTDNIB,R7              MAP THE NIB LAYOUT
         L     R1,16(R6)               GET THE PASSED RPL
         MVC   0(RPLEN,R5),0(R1)       AND MAKE IT OURS
*
* BUILD THE NIB FOR THIS USER BASED ON INFO PASSED.  WE WIL USE
* THE LAST 4-BYTES OF THE LUNAME AS A UNIQUE USER FIELD ID.
*
         MVC   NIBSYM(8),0(R4)         GET THE SYMBOLIC LUNAME
         MVC   NIBCID(4),20(R6)        PUT CID INTO NIB FOR OPNDST
         MVC   NIBUSER(4),4(R4)        USE LAST 4 BYTES OF LUNAME
         MVC   RPLUSFLD,4(R4)          AS A UNIQUE USER FIELD ID
*
* GET A COPY OF THE DATA PASSED TO THIS APPLICATION VIA "DATA('XXX')".
* PUT THIS INFORMATION IN "MSGAREA" AND IF THE INFORMATION IS TOO
* BIG TO FIT, THEN ALLOW TRUNCATION OF THE MESSAGE TO OCCUR.
*
         INQUIRE RPL=(R5),             USE THE DEFINED RPL             X
               OPTCD=LOGONMSG,         GET THE LOGON DATA()            X
               NIB=PNIB,               USE THE DEFINED NIB             X
               AREA=MSGAREA,           LOCATION TO PLACE DATA          X
               AREALEN=L'MSGAREA,      MAX DATALENGTH WE CAN ACCEPT    X
               ACB=(R9)                USE THE PASSED ACB
*
* MAKE SURE THAT THE INQUIRE WAS SUCCESSFUL
*
         LTR   R15,R15                 WERE WE SUCCESSFUL ?
         BZ    LG0010                  YES...PROCEED
         LR    R2,R15                  SAVE THE RETURN CODE
         DISPLAY 'LOGON: INQUIRE: ERR' INDICATE THE PROBLEM
         ST    R2,PARM2                SAVE PARM ADDRESS
         CALL  DSPLHEX,(PARM2)         MAKE IT DISPLAYABLE
         MVC   MSGRC2(4),PARM2+8       GET THE RETURN CODE
         DISPLAY ERMSG2                DISPLAY THE ERROR
         B     LG0030                  AND JUST MOVE ON
LG0010   LTR   R0,R0                   CC SET FROM ELSEWHERE ?
         BZ    LG0020                  NO...IT'S OKAY
         DISPLAY 'LOGON: INQUIRE CC>0' INDICATE THE PROBLEM
         B     LG0030                  AND JUST MOVE ON
*
* HERE WE CAN CHECK THE CONTENTS OF "MSGAREA". WE'LL CHECK TO SEE IF
* "SHUTDOWN" WAS PASSED BY THE USER.  IF SO, WE'LL TERMINATE VCON.
* "SHUTDOWN" MUST BE IN CAPS.
*
LG0020   L     R8,12(R6)               GET THE MESSAGE LENGTH
         LTR   R8,R8                   ANY DATA() ?
         BZ    LG0030                  NO...DON'T BOTHER
         CLC   MSGAREA(8),=C'SHUTDOWN' BRING DOWN VCON ?
         BNZ   LG0030                  NO...PROCEED
         L     R2,=A(TPENDECB)         POINT TO THE ECB
         B     LG0050                  AND EXIT
*
* ACCEPT THE SSCP-TO-LU REQUEST AND PERFORM AN OPNDST WHICH WILL CAUSE
* THE SSCP TO PROCESS A BIND REQUEST TO THE LU. BY ACCEPTING THE
* BIND, AN LU-TO-LU SESSION IS ESTABLISHED, MAKING IT ACTIVE.
*
LG0030   DS    0H
         OPNDST RPL=(R5),OPTCD=(SYN,ACCEPT,CA)
         LTR   R15,R15                 SESSION ESTABLISHED ?
         BZ    LG0040                  YES...PROCEED
         DISPLAY 'LOGON: OPNDST ERROR' INDICATE THE PROBLEM
LG0040   L R2,=A(LOGONECB)             POINT TO OUR ECB
LG0050   POST (R2)                     WAKE UP THE MAIN ROUTINE
 L R2,=A(RCVECB)
 POST (R2)
         SUBRT TYPE=RETURN,SAVE=LSAV   RETURN TO CALLER
* ---------------------------- DATA AREA -------------------- *
PARM2    DS   3F
ERMSG2   DC   AL1(11),C'R15=X',X'7D'
MSGRC2   DC   CL4' ',X'7D'
LOGONRPL RPL  AM=VTAM
RPLEN    EQU  *-LOGONRPL
PNIB     NIB  MODE=RECORD,PROC=(RESPX,TRUNC)
TERMMSG  DC   AL1(33),C'SESSION ESTABLISHED WITH '
TERMNAME DC   CL8' '
MSGAREA  DC   CL80' '                 LOGON MESSAGE AREA
         LTORG ,                       BEGIN LITERAL POOL
*
* < ----------------- RESPONSE EXIT ROUTINE ------------------ >
*
* WE SHOULD NEVER ENTER HERE!  THIS WOULD ONLY HAPPEN IF WE
* PERFORM ANOTHER REQ WITHOUT HANDLING AN EARLIER RESPONSE.
*
RESPEXIT SUBRT TYPE=INITIAL,SAVE=RSAV  BEGINNING OF THE EXIT
         LR    R6,R1                   GET THE PASSED PARM
*
* INCOMING REGS: 0(R6)=ACB  4(R6)=CID  8(R6)=USERFLD  20(R6)=RPL
*
         L     R9,0(R6)                GET THE ACB ADDRESS
         L     R5,20(R6)               GET THE RPL ADDRESS
         MVC   PRPLR(PRPLRLEN),0(R5)   COPY THE READ-ONLY RPL
         LA    R5,PRPLR
         MVC   RPLARG,4(R6)            INSERT THE CID
         NI    RPLEXTDS,X'FF'-RPLNIB   TURN OFF THE NIB FLAG
         USING IFGRPL,R5               MAP THE LAYOUT
         TM    RPLVTFL2,RPLEX          NEGATIVE RESPONSE ?
         BO    RE0020                  YES...RECOVER FROM IT
RE0010   DS    0H
         RESETSR RPL=(R5),OPTCD=CA,RTYPE=DFSYN,ACB=(R9)
         LTR   R15,R15                 DID THE RESET WORK ?
         BZ    RE0030                  YES...PROCEED
         DISPLAY 'RESP:  ERROR DURING RESETSR'
         B     RE0030
*
RE0020   LA    R0,4                    SETUP RETURN CODE
         L     R15,=A(SYNEXIT)         GET EXIT ADDRESS
         LR    R1,R5                   GET THE RPL
         BALR  R14,R15                 AND PERFORM THE ROUTINE
         LR    R5,R15
         LTR   R5,R5                   DID IT WORK ?
         BZ    RE0010                  YES...PROCEED
RE0030   DS    0H
         SUBRT TYPE=RETURN,SAVE=RSAV
* ----------------------- DATA AREA ------------------------ *
PRPLR    RPL  AM=VTAM
PRPLRLEN EQU  *-PRPLR
         LTORG ,
*
* < ----------------- TPEND EXIT ROUTINE ------------------ >
*
TPEXIT   SUBRT TYPE=INITIAL            BEGINNING OF THE EXIT
         LR    R6,R1                   GET THE PASSED PARM
         EOJ   RC=16                   VTAM JUST CAME DOWN!
         LTORG ,
TPENDFLG DC    X'00'
*
* < ----------------- SYNAD EXIT ROUTINE ------------------ >
*
SYNEXIT  SUBRT TYPE=INITIAL,SAVE=SSAV  BEGINNING OF THE EXIT
*
* INCOMING REGS: R1=RPL   R0=RECOVERY ACTION RETURN CODE
*
         MVC   LRPL(100),0(R1)         GET A COPY OF THE RPL
         LR    R4,R0                   GET RECOVERY ACTION RC
         ST    R4,PARMSY               SAVE THIS VALUE
         CALL  DSPLHEX,(PARMSY)        CONVERT IT
         MVC   SYNRC(2),PARMSY+10      GET THE VALUE
         DISPLAY SYNMSG                DISPLAY THE INFO
         USING IFGRPL,R5               MAP THE DSECT
* CHECK FOR RECURSIVE ENTRY INTO THIS EXIT
         LA    R5,LRPL                 POINT TO OUR RPL
         LR    R6,R5                   SAVE WHERE THE RPL IS
         CH    R4,=H'16'               IS IT OVER MAX FOR SYNAD ?
         BH    SY0020                  YES...ERROR
         B     *+4(R4)                 USE ACTION CODE IN BR TABLE
         B     SY0010                  CODE=00 IS NORMAL
         B     SY0040                  CODE=04 EXTROADINARY COMPLETE
         B     SY0090                  CODE=08 RETRYABLE
         B     SY0100                  CODE=0C DAMAGED
         B     SY0110                  CODE=10 ENVIRONMENT ERROR
*
SY0010   SR    R7,R7                   INDICATE OK
         SR    R8,R8                   NORMAL PROCESSING
SY0020   DISPLAY 'SYNAD: ERROR ENCOUNTERED'
SY0030   DS    0H
         SUBRT TYPE=RETURN,SAVE=SSAV,RC=0
*
SY0040   TM    RPLSSEI,RPLPATHI       SESSION PATH ERROR ?
         BO    SY0080                 YES...DISCONNECT
         CLI   RPLFDB2,X'03'          FEEDBACK ERROR ?
         BE    SY0060                 YES...EXCEPTION REQUEST
*
         LA    R10,SY0010             SETUP FOR NORMAL EXIT
         DROP  R5                     WE DON'T NEED THIS ANYMORE
         USING IFGRPL,R6              MAP THE RPL
SY0050   SESSIONC RPL=LRPL,STYPE=REQ,OPTCD=SYN,CONTROL=CLEAR
*
         LTR   R15,R15
         BNZ   SY0080
         SESSIONC RPL=LRPL,CONTROL=SDT START DATA TRAFFIC
         LTR   R15,R15                 DID IT WORK ?
         BNZ   SY0080                  NO...TERMINATE
         BR    R10                     YES...TERMINATE NORMALLY
         DROP R6
         USING IFGRPL,R5
SY0060   TM    RPLVTFL2,RPLNFME
         BO    SY0070
         MVC   RPLSSEO,RPLSSEI
         MVC   RPLSSMO,RPLSSMI
         DISPLAY 'SYNAD: SEND SYN'
         SEND  RPL=LRPL,STYPE=RESP,OPTCD=SYN
         LTR   R15,R15                 EXCEPTION RESPONSE WORK ?
         BNZ   SY0080                  NO...ERROR
         DROP  R5
         USING IFGRPL,R6               USE THE OTHER RPL
SY0070   BAL   R10,SY0050              DO CLEAR AND SDT
         DISPLAY 'SYNAD: SEND SYN/CA'
         RESETSR RPL=LRPL,RTYPE=DFSYN,OPTCD=(SYN,CA)
         LTR   R15,R15                 CA-MODE RESTOR WORK ?
         BNZ   SY0020                  NO...ERROR
         LA    R8,8                    INDICATE A PROBLEM
         B     SY0030                  AND EXIT
SY0080   CLSDST RPL=LRPL
         LTR   R15,R15                 CLSDST WORK ?
         BNZ   SY0020                  NO...ABORT
         LA    R8,12                   INDICATE A PROBLEM
         B     SY0030                  AND EXIT
SY0090   EXECRPL RPL=(R5)              RETRY FAILED ?
         LTR   R15,R15
         BNZ   SY0020                  YES...ERROR
         B     SY0010                  NO...IT'S OK
SY0100   DS    0H
         CLI   RPLREQ,RPLRCVCD
         BNE   SY0010                  PRETEND IT WAS OK
         LA    R8,16                   INDICATE A PROBLEM
         B     SY0030                  AND EXIT
SY0110   CLI   RPLREQ,RPLSNDCD
         BE    SY0080                  ATTEMPT TO CLSDST
         CLI   RPLREQ,RPLRSRCD
         BE    SY0080                  ATTEMPT TO CLSDST LU
         LA    R8,20                   INDICATE A PROBLEM
         B     SY0030                  AND EXIT
* ------------------------------------------------------ *
PARMSY   DS   3F
SYNMSG   DC   AL1(SYNLEN)
SYNTEXT  DC   C'SYNAD: ERROR CODE=X',X'7D'
SYNRC    DC   CL2' ',X'7D'
SYNLEN   EQU  *-SYNTEXT
LRPL     RPL  ACB=VACB,AM=VTAM
         LTORG ,
*
* < ----------------- LOSTERM EXIT ROUTINE ------------------ >
*
*
* 0(R1) = ACB ADDRESS   4(R1) = CID OF THE SESSION
* 8(R1) = USERFLD DATA FROM NIB  12(R1) = LOSTERM REASON CODE
*
LOSTEXIT SUBRT TYPE=INITIAL,SAVE=XSAV  BEGIN THE EXIT
         L     R2,=A(LUID)             GET THE LOGICAL UNIT ID
         MVC   LSTLUID(8),0(R2)        AND PUT IT IN THE MESSAGE
         L     R5,=A(LOGONRPL)         POINT TO OPNDST/CLSDST RPL
         USING IFGRPL,R5               MAP TO NEW RPL
         MVC   RPLACB,0(R1)            GET THE ACB ADDRESS
         MVC   RPLARG,4(R1)            GET THE LOST TERM CID
         MVC   RPLUSFLD,8(R1)          GET USER FIELD
         NI    RPLEXTD1,X'FF'-RPLNIB   RESET THE NIB INDICATOR
         OI    RPLSRTYP,RPLNFSYN       SEND/RECEIVE TYPE=SYNCHRONOUS
         OI    RPLOPT7,RPLRLSOP        RELEASE THE CONNECTION
         DISPLAY LSTMSG                INDICATE WHAT HAPPENED
         CLSDST RPL=(R5)               RELEASE THE LU INFO
         SUBRT TYPE=RETURN,SAVE=XSAV   RETURN TO CALLER
*
         LTORG ,
LSTMSG   DC   AL1(36),CL28'LOSTERM:  LOST SESSION WITH'
LSTLUID  DC   XL8'0'
PARM5    DS    3F
ERMSG5   DC    AL1(22),C'CLSDST ERROR: RC=X',X'7D'
MSG5RC   DC    CL2' ',X'7D'
*
* < ----------------- LERAD EXIT ROUTINE ------------------ >
*
*
* IF WE GO IN HERE, IT'S BECAUSE THE PROGRAM LOGIC IS IN ERROR!
* DON'T BOTHER RECOVERING. JUST DUMP THE RPL AND TERMINATE.
*
* FOR THIS TEST VERSION, WE'LL GO HERE AS PART OF THE LOSTERM
*
LEREXIT  SUBRT TYPE=INITIAL            BEGINNING OF THE EXIT
         LR    R5,R1                   SAVE THE RPL ADDRESS
*
* R0=RECOVERY ACTION CODE   R1 = RPL ADDRESS
*
         ST    R0,LPARM
         CALL  DSPLHEX,(LPARM)
         MVC   L0(2),LPARM+10
         DISPLAY LMSG
         USING IFGRPL,R5               MAP TO NEW RPL
         DISPLAY 'RPL INFO AND PARTITION DUMP FOLLOWS',SYSLST
         SNAP  FROM=(R5),FOR=100
         CANCEL ALL
         EOJ   RC=16
* ----------------------- DATA AREA ---------------------- *
         LTORG ,
LMSG     DC   AL1(LMSGL)
LMSGS    DC   C'LERAD:  R0=X',X'7D'
L0       DC   C'XX',X'7D'
LMSGL    EQU  *-LMSGS
LPARM    DC   3F'0'
*
* ----------------- DUMMY SECTIONS ---------------- *
SYSCOM   SYSCOM
CRTTAB   DSECT
         CRTGEN
CRTSAV   CRTSAV DSECT=YES
         IFGACB AM=VTAM
         IFGEXLST AM=VTAM
         ISTDNIB
         ISTUSFBC
         IFGRPL AM=VTAM
         END
/*
// EXEC ASSEMBLY
* REGEQU IS NOT AVAILABLE ON DOS/VS
*         YREGS
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
SUBPOOL  EQU   0
DUMP     SUBRT VRM=2.2                 START OF PROGRAM
         L     R3,0(R1)                GET PASSED PARM
         LA    R1,1                    INIT THE BIT
         SLL   R1,31                   MAKE IT HIGH
         LA    R6,DP0005               POINT TO THE AREA
         OR    R6,R1                   MAKE IT HIGH
         LA    R11,DP0080              FOR THE RETURN
         BSM   R11,R6                  SAVE THE ORIGINAL MODE IN R11
DP0005   CLC   4(4,R3),=F'0'           LENGTH TYPE ?
         BNZ   DP0010                  NO LENGTH...PROCEED
         L     R2,8(R3)                GET THE LENGTH
         A     R2,0(R3)                GET THE END
         ST    R2,4(R3)                AND INSERT THE ENDING
DP0010   AMODESW SET,AMODE=24          RESET TO 24-BIT
         CLC   0(4,R3),4(R3)           IS STARTING ADDR > ENDING ?
         BNH   DP0020                  NO...PROCEED
         L     R6,0(R3)                GET "TO" VALUE
         L     R5,4(R3)                GET "FROM" VALUE
         B     DP0030                  AND PROCEED
DP0020   L     R5,0(R3)                GET "FROM" VALUE
         L     R6,4(R3)                GET "TO" VALUE
DP0030   MODESET KEY=ZERO              JUST IN CASE
         ST    R1,KEY                  SAVE THE ENTERED KEY
         ST    R5,VADDR                SAVE STARTING ADDRESS
         ST    R6,VNEXT                SAVE ENDING ADDRESS
         ASYSCOM ,                     POINT TO SYSCOM
         L     R1,100(R1)              GO TO INFO AREA
         CLI   7(R1),X'45'             ESA 1.3 OR HIGHER ?
         BL    DP0040                  YES...SKIP IT
         CALL  VALIDADR,(VPARMS)       CHECK THE ADDRESS
         L     R15,VRCODE              GET THE RETURN CODE
         LTR   R15,R15                 DID IT WORK ?
         BZ    DP0040                  YES...EXIT
         DISPLAY 'INVALID ADDRESS RANGE PASSED TO DUMP'
DP0040   ST    R5,HOLD                 PARM SETUP
         CALL  DSPLHEX,(HOLD)          GET DISPLAYABLE VALUE
         MVC   IOAREA(8),HOLD+4        PUT IT UP FOR DISPLAY
         L     R8,=A(IOAREA+12)        POINT TO DISPLAY
         L     R9,=A(IOAREA+58)        POINT TO DISPLAY
         L     R7,=F'4'                FOR LOOPING
         LR    R2,R7                   USE THIS FOR LATER
DP0050   DS    0H
         CR    R5,R6                   END OF REQUEST ?
         BH    DP0060                  YES..PRINT AND EXIT
         AMODESW SET,AMODE=31          GO TO 31-BIT
         MVC   HOLD(4),0(R5)           GET THE DATA
         MVC   0(4,R9),0(R5)           GET THE NON-CONVERTED DATA
         AR    R5,R2                   POINT TO NEXT FULLWORD
         AMODESW SET,AMODE=24          AND BACK TO 24-BIT
         CALL  DSPLHEX,(HOLD)          CONVERT THE DATA
         MVC   0(8,R8),HOLD+4          MOVE IT IN
         AR    R9,R2                   SKIP THE POINTER
         LA    R8,10(R8)               NEXT OUTPUT COLUMN
         BCT   R7,DP0050               GET ANOTHER COLUMN
DP0060   CALL  PRINT,(BUFFER)          PRINT A LINE
         MVI   IOAREA,C' '             INIT TO BLANKS
         MVC   IOAREA+1(74),IOAREA     TO RESET THE LINE
         CR    R5,R6                   END OF REQUEST ?
         BL    DP0040                  NO DO ANOTHER LINE
         CLI   KEY+3,ZERO              DID WE COME IN AS ZERO ?
         BZ    DP0070                  YES...EXIT
         MODESET KEY=USER              RESET THE KEY
DP0070   CALL  PRINT,(BUFFER)          SKIP A LINE
         BSM   R0,R11                  SET THE MODE
DP0080   SUBRT ,                       AND RETURN TO CALLER
* ---------------------- DATA AREA ---------------------- *
HOLD     DS    3F                      USED FOR DSPLHEX
KEY      DS    F                       HOLD PROTECTION KEY
BUFFER   DC    AL1(75),C' '            USED BY THE PRINT SUBROUTINE
IOAREA   DC    CL75' '                 DATA GOES HERE
* VALIDADR PARAMETERS
VPARMS   DS  0F                        VALIDADR PASSED PARM
VPIK     DC  F'0'                      PIK VALUE GOES IN HERE
VADDR    DC  F'0'                      BEGINNING ADDRESS
VNEXT    DC  F'0'                      ENDING ADDRESS
VLEN     DC  F'0'                      LENGTH
VRCODE   DC  F'0'                      RETURN CODE
         END
/*
// EXEC ASSEMBLY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* SUBROUTINE: DSPLHEX                                               *
*                                                                   *
* (C) 1994 PF1:HELP   1600 WARWICK CT., WHEELING IL  60090-7116     *
*          (708) 459-5294   FAX/BBS: (708) 465-9520                 *
*                                                                   *
*      THIS ROUTINE IS A SAMPLE OF WHAT WAS/WILL BE AVAILABLE IN    *
*      THE "VSE INTERNALS" NEWSLETTER, PUBLISHED BI-MONTHLY.        *
*      MACROS AND SUBROUTINES INDICATED IN THIS ROUTINE ARE         *
*      AVAILABLE TO SUBSCRIBERS AT NO CHARGE.                       *
*                                                                   *
* PURPOSE: TAKE A FULLWORD AND CONVERT IT TO 8 BYTES OF             *
*   DISPLAYABLE CHARACTERS.                                         *
*                                                                   *
* INPUT: PASSES THE ADDRESS IN REGISTER 1 OF THE FOLLOWING PARM:    *
*      PARM   DS   0CL12                                            *
*      FWORD  DS   F           INPUT FULLWORD                       *
*      OUTPUT DS   CL8         DISPLAYABLE OUTPUT                   *
*                                                                   *
* OUTPUT: DISPLAYABLE TEXT MOVED TO OUTPUT FIELD.                   *
*                                                                   *
* NON-IBM MACROS: SUBRT, $SYSTIME, $SYSDATE                         *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* REGEQU IS NOT AVAILABLE ON DOS/VS
*         YREGS
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
SUBPOOL  EQU   0
DSPLHEX  SUBRT ,                       BEGIN PROGRAM
         L     R1,0(R1)                POINT TO PASSED PARM
         LA    R2,4(R1)                POINT TO OUTPUT FIELD
         UNPK  0(7,R2),0(4,R1)         EXPAND THE FULLWORD
         MVC   7(1,R2),3(R1)           MODIFY THE LAST BYTE
         NC    0(8,R2),ZEROF           SPLIT THE BYTES APART
         TR    0(8,R2),TRTABLE         TRANSLATE FOR THE DISPLAY
         SUBRT ,                       RETURN TO CALLER
* ------------------ DATA AREA ------------------------- *
         LTORG ,                       LITERAL POOL
ZEROF    DC    8XL1'0F'                FOR SPLITTING APART BYTES
TRTABLE  DC    CL16'0123456789ABCDEF'  FOR MAKING HEX DISPLAYABLE
         END
/*
// EXEC ASSEMBLY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* SUBROUTINE: DUMP                                                  *
*                                                                   *
* (C) 1994 PF1:HELP   1600 WARWICK CT., WHEELING IL  60090-7116     *
*          (708) 459-5294   FAX/BBS: (708) 465-9520                 *
*                                                                   *
*      THIS ROUTINE IS A SAMPLE OF WHAT WAS/WILL BE AVAILABLE IN    *
*      THE "VSE INTERNALS" NEWSLETTER, PUBLISHED BI-MONTHLY.        *
*      MACROS AND SUBROUTINES INDICATED IN THIS ROUTINE ARE         *
*      AVAILABLE TO SUBSCRIBERS AT NO CHARGE.                       *
*                                                                   *
* PURPOSE: TO DUMP A RANGE OF STORAGE IN A DISPLAYABLE FORMAT       *
*                                                                   *
* INPUT: PARM ADDRESS CONTAINING THE STARTING AND ENDING RANGE      *
*   TO BE DUMPED IN A PAIR OF FULLWORDS. IF THE SECOND FULLWORD IS  *
*   NULL, THEN A THIRD ADDRESS IS ASSUMED TO CONTAIN A LENGTH.      *
*                                                                   *
* CHANGES: V2.0: SUPPORT 31-BIT ADDRESSING.                         *
*          V2.1: VALIDATE ADDRESSES BEFORE DUMPING                  *
*          V2.2: RETURN TO ORIGINAL ADDRESSING MODE                 *
*                                                                   *
* OUTPUT: FOUR-COLUMN FORMATTED DISPLAY.                            *
*                                                                   *
* SUBROUTINES: PRINT, DSPLHEX, VALIDADR                             *
*                                                                   *
* NON-IBM MACROS: SUBRT, $SYSTIME, $SYSDATE                         *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* REGEQU IS NOT AVAILABLE ON DOS/VS
*         YREGS
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
SUBPOOL  EQU   0
DUMP     SUBRT VRM=2.2                 START OF PROGRAM
         L     R3,0(R1)                GET PASSED PARM
         LA    R1,1                    INIT THE BIT
         SLL   R1,31                   MAKE IT HIGH
         LA    R6,DP0005               POINT TO THE AREA
         OR    R6,R1                   MAKE IT HIGH
         LA    R11,DP0080              FOR THE RETURN
         BSM   R11,R6                  SAVE THE ORIGINAL MODE IN R11
DP0005   CLC   4(4,R3),=F'0'           LENGTH TYPE ?
         BNZ   DP0010                  NO LENGTH...PROCEED
         L     R2,8(R3)                GET THE LENGTH
         A     R2,0(R3)                GET THE END
         ST    R2,4(R3)                AND INSERT THE ENDING
DP0010   AMODESW SET,AMODE=24          RESET TO 24-BIT
         CLC   0(4,R3),4(R3)           IS STARTING ADDR > ENDING ?
         BNH   DP0020                  NO...PROCEED
         L     R6,0(R3)                GET "TO" VALUE
         L     R5,4(R3)                GET "FROM" VALUE
         B     DP0030                  AND PROCEED
DP0020   L     R5,0(R3)                GET "FROM" VALUE
         L     R6,4(R3)                GET "TO" VALUE
DP0030   MODESET KEY=ZERO              JUST IN CASE
         ST    R1,KEY                  SAVE THE ENTERED KEY
         ST    R5,VADDR                SAVE STARTING ADDRESS
         ST    R6,VNEXT                SAVE ENDING ADDRESS
         ASYSCOM ,                     POINT TO SYSCOM
         L     R1,100(R1)              GO TO INFO AREA
         CLI   7(R1),X'45'             ESA 1.3 OR HIGHER ?
         BL    DP0040                  YES...SKIP IT
         CALL  VALIDADR,(VPARMS)       CHECK THE ADDRESS
         L     R15,VRCODE              GET THE RETURN CODE
         LTR   R15,R15                 DID IT WORK ?
         BZ    DP0040                  YES...EXIT
         DISPLAY 'INVALID ADDRESS RANGE PASSED TO DUMP'
DP0040   ST    R5,HOLD                 PARM SETUP
         CALL  DSPLHEX,(HOLD)          GET DISPLAYABLE VALUE
         MVC   IOAREA(8),HOLD+4        PUT IT UP FOR DISPLAY
         L     R8,=A(IOAREA+12)        POINT TO DISPLAY
         L     R9,=A(IOAREA+58)        POINT TO DISPLAY
         L     R7,=F'4'                FOR LOOPING
         LR    R2,R7                   USE THIS FOR LATER
DP0050   DS    0H
         CR    R5,R6                   END OF REQUEST ?
         BH    DP0060                  YES..PRINT AND EXIT
         AMODESW SET,AMODE=31          GO TO 31-BIT
         MVC   HOLD(4),0(R5)           GET THE DATA
         MVC   0(4,R9),0(R5)           GET THE NON-CONVERTED DATA
         AR    R5,R2                   POINT TO NEXT FULLWORD
         AMODESW SET,AMODE=24          AND BACK TO 24-BIT
         CALL  DSPLHEX,(HOLD)          CONVERT THE DATA
         MVC   0(8,R8),HOLD+4          MOVE IT IN
         AR    R9,R2                   SKIP THE POINTER
         LA    R8,10(R8)               NEXT OUTPUT COLUMN
         BCT   R7,DP0050               GET ANOTHER COLUMN
DP0060   CALL  PRINT,(BUFFER)          PRINT A LINE
         MVI   IOAREA,C' '             INIT TO BLANKS
         MVC   IOAREA+1(74),IOAREA     TO RESET THE LINE
         CR    R5,R6                   END OF REQUEST ?
         BL    DP0040                  NO DO ANOTHER LINE
         CLI   KEY+3,ZERO              DID WE COME IN AS ZERO ?
         BZ    DP0070                  YES...EXIT
         MODESET KEY=USER              RESET THE KEY
DP0070   CALL  PRINT,(BUFFER)          SKIP A LINE
         BSM   R0,R11                  SET THE MODE
DP0080   SUBRT ,                       AND RETURN TO CALLER
* ---------------------- DATA AREA ---------------------- *
HOLD     DS    3F                      USED FOR DSPLHEX
KEY      DS    F                       HOLD PROTECTION KEY
BUFFER   DC    AL1(75),C' '            USED BY THE PRINT SUBROUTINE
IOAREA   DC    CL75' '                 DATA GOES HERE
* VALIDADR PARAMETERS
VPARMS   DS  0F                        VALIDADR PASSED PARM
VPIK     DC  F'0'                      PIK VALUE GOES IN HERE
VADDR    DC  F'0'                      BEGINNING ADDRESS
VNEXT    DC  F'0'                      ENDING ADDRESS
VLEN     DC  F'0'                      LENGTH
VRCODE   DC  F'0'                      RETURN CODE
         END
/*
// EXEC ASSEMBLY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* SUBROUTINE: PREPCMD                                               *
*                                                                   *
* (C) 1994 PF1:HELP   1600 WARWICK CT., WHEELING IL  60090-7116     *
*          (708) 459-5294   FAX/BBS: (708) 465-9520                 *
*                                                                   *
*      THIS ROUTINE IS A SAMPLE OF WHAT WAS/WILL BE AVAILABLE IN    *
*      THE "VSE INTERNALS" NEWSLETTER, PUBLISHED BI-MONTHLY.        *
*      MACROS AND SUBROUTINES INDICATED IN THIS ROUTINE ARE         *
*      AVAILABLE TO SUBSCRIBERS AT NO CHARGE.                       *
*                                                                   *
* PURPOSE: TO PREPARE A COMMAND PASSED FROM A USER BEFORE BEING     *
*    PASSED TO THE APPLICATION PROGRAM.                             *
*                                                                   *
* INPUT: ADDRESS OF 82 BYTE FIELD, WHERE THE FIRST HALFWORD IS THE  *
*   DATA LENGTH.                                                    *
*                                                                   *
* OUTPUT: DATA LENGTH IS UPDATED. PRECEEDING BLANKS ARE ELIMINATED. *
*   MULTIPLE BLANKS ARE COMPRESSED TO SINGLE BLANKS, UNLESS THEY    *
*   ARE ENCLOSED IN APOSTROPHES. LOWER CASE IS CONVERTED TO UPPER.  *
*                                                                   *
* NON-IBM MACROS: SUBRT, $SYSTIME, $SYSDATE                         *
*                                                                   *
* V 1.1  : CHANGED ALGORITHM FOR OBTAINING RESULTING LENGTH         *
* V 1.2  : TRANSLATE HEX NOTATION TO UPPER CASE - E.G. X'NNNNNN'    *
* V 1.3  : FIXED PROBLEM WITH NULL LENGTH STRINGS                   *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* REGEQU IS NOT AVAILABLE ON DOS/VS
*         YREGS
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
SUBPOOL  EQU   0
PREPCMD  SUBRT VRM=1.3                 START SUBROUTINE
         L     R1,0(R1)                POINT TO PASSED PARM
         XR    R8,R8                   INIT LENGTH VALUE
         LR    R5,R1                   SAVE THE ADDRESS
         MVI   APIND,OFF               INIT NO APOST AS DEFAULT
         MVI   HEX,OFF                 RESET IT
         MVI   PARM,C' '               INIT THE USING AREA
         MVC   PARM+1(79),PARM         TO BLANKS
         LA    R2,PARM                 POINT TO OUTPUT FIELD
         LR    R6,R2                   SAVE THE BEGINNING POINTER
         LH    R4,0(R1)                GET THE MAX SIZE TO PROCESS
         CLC   0(2,R1),=H'80'          TOO BIG ?
         BH    PC0105                  YES...FORGET IT
PC0005   CLC   0(2,R1),=H'0'           NULL DATA PASSED ?
         BZ    PC0110                  YES...RETURN TO CALLER
         LR    R7,R4                   SAVE THE DATA LENGTH
         BCTR  R7,0                    DO THIS FOR THE EX
         LA    R1,2(R1)                POINT TO INPUT FIELD
*
* ELIMINATE PRECEDING BLANKS
*
PC0010   CLI   0(R1),C' '              BLANK ?
         BNZ   PC0020                  NO..GO BEGIN
         LA    R1,1(R1)                SKIP A BYTE
         BCT   R4,PC0010               AND LOOK SOME MORE
         B     PC0100                  NO DATA...EXIT
*
* CHECK IF ITS BETWEEN QUOTES.  IF IT IS AND IT'S NOT IN HEXIDECIMAL
* NOTATION, THEN WE WON'T CONVERT IT TO UPPER CASE
*
PC0020   MVC   0(1,R2),0(R1)           GET A BYTE
         CLI   0(R2),APOST             APOSTROPHE ?
         BZ    PC0021                  YES...SET THE INDICATORS
         CLI   APIND,ON                IS IT A STRING ?
         BZ    PC0030                  YES...LEAVE AS IS
         B     PC0025                  NO...CONVERT IT
*
* IS THIS THE SECOND QUOTE ?
*
PC0021   CLI   HEX,ON                  END OF HEX NUMBER ?
         BNZ   PC0022                  NO...CHECK FOR STRING
         MVI   HEX,OFF                 MUST BE END OF HEX PHRASE
         B     PC0030                  ACCEPT IT
PC0022   CLI   APIND,ON                END OF STRING ?
         BNZ   PC0023                  NO...MUST BE A BEGINNING
         MVI   APIND,OFF               MUST BE END OF STRING
         B     PC0030                  ACCEPT THIS
*
* HANDLE THE FIRST QUOTE
*
PC0023   LR    R3,R2                   GET THIS POINTER
         BCTR  R3,0                    BACK UP
         CLI   0(R3),C'X'              IS IT A HEXIDECIMAL ?
         BNZ   PC0024                  NO...THEN IT'S A STRING
         MVI   HEX,ON                  INDICATE A HEX NUMBER
         B     PC0030                  ACCEPT IT
PC0024   MVI   APIND,ON                INDICATE STRING
         B     PC0030                  ACCEPT THIS TOO
*
* CONVERT LOWER CASE TO UPPER CASE
*
PC0025   CLI   0(R2),SMALLA            LOWER CASE ?
         BL    PC0030                  NO..PROCEED
         CLI   0(R2),SMALLZ            LOWER CASE ?
         BH    PC0030                  NO..PROCEED
         OI    0(R2),UPCASE            MAKE IT UPPER
PC0030   LA    R1,1(R1)                POINT TO NEXT INPUT BYTE
         LA    R2,1(R2)                POINT TO NEXT OUTPUT BYTE
PC0040   CLI   APIND,ON                IS THIS A STRING ?
         BZ    PC0050                  YES..SKIP IT
         CLC   0(2,R1),BLANK           NEXT TWO BYTES BLANK ?
         BNZ   PC0050                  NO..GO DO IT AGAIN
         LA    R1,1(R1)                SKIP AHEAD A BYTE
         BCT   R4,PC0040               AND CHECK AGAIN
         B     PC0100                  IF ALL DONE..FILL WITH BLANKS
PC0050   BCT   R4,PC0020               GET ANOTHER BYTE
*
* GET THE REAL ENDING LENGTH
*
PC0100   LA    R1,PARM+80              POINT TO THE END
         LA    R2,80                   THE MAX LENGTH
PC0101   BCTR  R1,0                    BACK UP
         CLI   0(R1),C' '              END OF FIELD ?
         BNZ   PC0105                  YES..EXIT
         BCT   R2,PC0101               ELSE KEEP IT UP
PC0105   STH   R2,0(R5)                AS WELL AS THE LENGTH
         MVC   2(80,R5),PARM           PASS BACK THE DATA
PC0110   SUBRT ,                       RETURN TO CALLER
* ------------------- DATA AREA ------------------------ *
         LTORG ,                       LITERAL POOL
PARM     DS    40H                     NEW FIELD
APIND    DS    X                       APOSTROPHE INDICATOR
HEX      DS    X                       HEXIDECIMAL INDICATOR
BLANK    DC    CL2' '                  CONSTANT
* ------------------- EQUATES -------------------------- *
APOST    EQU    X'7D'
ON       EQU    X'FF'
OFF      EQU    X'00'
SMALLA   EQU    X'81'
SMALLZ   EQU    X'A9'
UPCASE   EQU    X'40'
         END
/*
// EXEC ASSEMBLY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* SUBROUTINE: PRINT                                                 *
*                                                                   *
* (C) 1994 PF1:HELP   1600 WARWICK CT., WHEELING IL  60090-7116     *
*          (708) 459-5294   FAX/BBS: (708) 465-9520                 *
*                                                                   *
*      THIS ROUTINE IS A SAMPLE OF WHAT WAS/WILL BE AVAILABLE IN    *
*      THE "VSE INTERNALS" NEWSLETTER, PUBLISHED BI-MONTHLY.        *
*      MACROS AND SUBROUTINES INDICATED IN THIS ROUTINE ARE         *
*      AVAILABLE TO SUBSCRIBERS AT NO CHARGE.                       *
*                                                                   *
* PURPOSE: TO OUTPUT A LINE OF TEXT TO SYSLST.                      *
*                                                                   *
* INPUT: THIS ROUTINE CAN BE CALLED IN TWO WAYS. EITHER BY PASSING  *
*   A PARAMETER ADDRESS IN REGISTER 1 IN THE FOLLOWING FORMAT:      *
*                                                                   *
*       LINE     DC  AL1(L'DATA)  <--- THE CC LENGTH IS NOT INCL.   *
*       CC       DC  C' '                                           *
*       DATA     DC  C'THIS IS A PRINT LINE'                        *
*                                                                   *
*             CALL PRINT,(LINE)                                     *
*                                                                   *
*  OR BY PASSING TWO SEPERATE ADDRESSES. ONE FOR THE LENGTH, AND    *
*  THE OTHER THE DATA. IN THIS CASE, THE LENGTH MUST BE A FULLWORD. *
*                                                                   *
* OUTPUT: A LINE OF TEXT IS DISPLAYED UPON SYSLST, THE DATA WILL    *
*       REMAIN UNCHANGED (LINE IS NOT CLEARED AFTER PRINTING).      *
*                                                                   *
* NON-IBM MACROS: SUBRT, $SYSTIME, $SYSDATE                         *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* REGEQU IS NOT AVAILABLE ON DOS/VS
*         YREGS
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
SUBPOOL  EQU   0
PRINT    SUBRT VRM=2.2                 START OF PROGRAM
         LR    R5,R1                   SAVE THE PARM
         LA    R1,1                    INIT THE BIT
         SLL   R1,31                   MAKE IT HIGH
         LA    R6,PRT005               POINT TO THE AREA
         OR    R6,R1                   MAKE IT HIGH
         LA    R11,PRT070              FOR THE RETURN
         BSM   R11,R6                  SAVE THE ORIGINAL MODE IN R11
PRT005   AMODESW SET,AMODE=24          GOTTA BE IN 24-BIT
         LR    R1,R5                   REGAIN THE PARM
         LA    R9,PRTCCW               POINT TO CCW
         USING CCWDSECT,R9             AND MAP TO LAYOUT
         LH    R8,COUNT                GET THE LINE COUNTER
         L     R3,0(R1)                GET THE PARM ADDRESS
         TM    0(R1),X'80'             ONLY ONE PARM PASSED ?
         BO    PRT010                  YES...PROCEED NORMALLY
         L     R10,0(R3)               GET THE LENGTH
         LR    R5,R10                  SAVE A COPY
         L     R3,4(R1)                GET THE ACTUAL DATA
         B     PRT020                  AND PROCEED
PRT010   SR    R10,R10                 INIT WORK REGISTER
         IC    R10,0(R3)               GET THE LENGTH
         LR    R5,R10                  SAVE A COPY
         LA    R3,1(R3)                SKIP PAST LENGTH
PRT020   LTR   R10,R10                 NULL LENGTH ?
         BZ    PRT065                  YES...RETURN
         STCM  R10,7,CCWIOLEN          MODIFY THE PRINT LENGTH
         LA    R2,1(R3)                POINT TO THE DATA
         STCM  R2,7,CCWIOADR           MODIFY THE BUFFER ADDRESS
         CLI   0(R3),C'1'              TOP OF FORM ?
         BZ    PRT055                  YES..PROCEED
         CLI   0(R3),C'0'              DOUBLE SPACE ?
         BZ    PRT030                  YES..PROCEED
         CLI   0(R3),C'-'              TRIPLE SPACE ?
         BZ    PRT040                  YES..PROCEED
         MVI   SKIPCCW,11              SKIP A SINGLE LINE
         LA    R8,1(,R8)               ADD LINE COUNT
         B     PRT050                  AND OUTPUT IT
PRT030   MVI   SKIPCCW,19              DOUBLE SPACE
         LA    R8,2(,R8)               ADD LINE COUNT
         B     PRT050                  AND OUTPUT IT
PRT040   MVI   SKIPCCW,27              TRIPLE SPACE
         LA    R8,3(,R8)               ADD LINE COUNT
PRT050   CH    R8,MAXLINES             ALREADY MAXED OUT ?
         BNH   PRT060                  NO...PROCEED
PRT055   MVI   SKIPCCW,139             FORCE A FORMFEED
         SR    R8,R8                   INIT COUNTER
PRT060   EXCP  SKIPIT                  POSITION THE LINE
         WAIT  SKIPIT                  LET THE I/O COMPLETE
         EXCP  PRINTER                 OUTPUT THE LINE
         WAIT  PRINTER                 LET THE I/O COMPLETE
         STH   R8,COUNT                SAVE COUNTER
PRT065   BSM   R0,R11                  RESET THE AMODE
PRT070   SUBRT ,                       RETURN TO CALLER
* ---------------------- DATA AREA ---------------------- *
COUNT    DC    H'60'                   LINE COUNTER
MAXLINES DC    H'56'                   MAX LINES PER PAGE
*
PRINTER  CCB   SYSLST,PRTCCW           SYSLST DEFINITION
PRTCCW   CCW   1,*,X'00',132           PRINT WITHOUT SPACING
SKIPIT   CCB   SYSLST,SKIPCCW          USED FOR SKIPPING
SKIPCCW  CCW   1,*,X'00',1             SHIP DEFINITION
*
CCWDSECT DSECT
CCWREQ   DS    X                       CCW ACTION REQUEST
CCWIOADR DS    AL3                     I/O AREA ADDRESS
CCWCHAIN DS    X                       CCW CHAIN BYTE
CCWIOLEN DS    XL3                     I/O AREA LENGTH
	  END
/*
// EXEC ASSEMBLY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* SUBROUTINE: VALIDADR                                                *
*                                                                     *
*  (C) 1994 PF1:HELP   1600 WARWICK CT., WHEELING IL  60090-7116      *
*      (708) 459-5294   FAX/BBS: (708) 465-9520                       *
*                                                                     *
*      THIS ROUTINE IS A SAMPLE OF WHAT WAS/WILL BE AVAILABLE IN      *
*      THE "VSE INTERNALS" NEWSLETTER, PUBLISHED BI-MONTHLY.          *
*      MACROS AND SUBROUTINES INDICATED IN THIS ROUTINE ARE           *
*      AVAILABLE TO SUBSCRIBERS AT NO CHARGE.                         *
*                                                                     *
* PURPOSE: TO VALIDATE PASSED ADDRESS RANGES FOR ANY PARTITION. THESE *
*        CAN BE 24 OR 31-BIT ADDRESSES.                               *
*                                                                     *
* INPUT: A PARAMETER LIST (SEE DSECT AT END OF PROGRAM).  IF THE      *
*        ENDING ADDRESS IS NOT PROVIDED, THEN A PASSED LENGTH WILL    *
*        BE USED. A PIK OF THE PARTITION WILL BE USED IF PROVIDED.    *
*        IF NOT, THEN CURRENT PARTITION WILL BE USED AND RETURNED.    *
*                                                                     *
* OUTPUT: EADDR WILL BE UPDATED, AND RCODE WILL BE ZERO IF THE RANGE  *
*        IS VALID, OR NON-ZERO IF NOT VALID.                          *
*                                                                     *
* MACROS: SUBRT, $SYSDATE, $SYSTIME                                   *
*                                                                     *
* REVISIONS:                                                          *
*   1.1  : USE 31-BIT ADDRESSSING.  DON'T ALLOW VPOOL STORAGE ACCESS. *
*   1.2  : ELIMINATE THE NEED FOR THE EXTRACT MACRO                   *
*   1.3  : SAVE INCOMING AMODE, ELIMINATE R5 FOR ICCF                 *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* REGEQU IS NOT AVAILABLE ON DOS/VS
*         YREGS
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
SUBPOOL  EQU   0
VALIDADR SUBRT VRM=1.3                 START OF SUBROUTINE
         L     R6,0(R1)                SAVE PARM ADDRESS
         USING PARM,R6                 MAP THE LAYOUT
         LA    R1,1                    INIT THE BIT
         SLL   R1,31                   MAKE IT HIGH
         LA    R2,VA0005               POINT TO THE AREA
         OR    R2,R1                   MAKE IT HIGH
         LA    R11,VA0070              FOR THE RETURN
         BSM   R11,R2                  SAVE THE ORIGINAL MODE IN R11
VA0005   ST    R11,MODE                STORE THE INCOMING AMODE
         XC    RCODE(4),RCODE          INIT THE RETURN CODE
         L     R4,PIK                  POINT TO THE PIK
         LTR   R4,R4                   WAS ONE PASSED ?
         BNZ   VA0010                  YES...PROCEED
         COMRG ,                       POINT TO THE COMREG
         USING COMREG,R1               MAP TO THE LAYOUT
         LH    R4,PID                  GET THE PIK
         ST    R4,PIK                  AND SAVE IT
VA0010   SRL   R4,2                    DIVIDE BY 4
         L     R3,X'2C4'               POINT TO APCBTAB
         AR    R3,R4                   POINT TO CORRECT ENTRY
         L     R9,0(R3)                GO THERE
         USING PCBADR,R9               MAP TO THE LAYOUT
         ASYSCOM R11                   POINT TO SYSCOM
         USING SYSCOM,R11              MAP TO LAYOUT
         ICM   R1,7,IJBSVAAD           POINT TO SDL
         L     R7,36(R1)               GET THE 31-BIT SDL START
         L     R2,IJBSMCOM             POINT TO SMCOM
         USING SMCOM,R2                MAP TO THE LAYOUT
*
* 0 IS THE BEGINNING ADDR OF THE LOWER BLOCK OF STORAGE
* SMCVPBEG IS THE ENDING ADDR OF THE LOWER BLOCK OF STORAGE
*
* SMVPBEG CONTAINS THE BEGINNING ADDR OF THE CENTRAL BLOCK OF STORAGE
* SMVPEND CONTAINS THE ENDING ADDR OF THE CENTRAL BLOCK OF STORAGE
*
* (R7)     CONTAINS THE BEGINNING ADDR OF THE HIGH BLOCK OF STORAGE
* EOCADR   CONTAINS THE ENDING ADDR OF THE HIGH BLOCK OF STORAGE
*
* NOW WE'LL SEE WHAT ADDRESS RANGES HAVE BEEN PROVIDED
*
         L     R10,EADDR               GET THE ENDING ADDRESS
         LTR   R10,R10                 ENDING PROVIDED ?
         BNZ   VA0030                  YES...DON'T ADD
         L     R3,BADDR                GET THE ADDRESS
         L     R1,LEN                  GET THE LENGTH
         AMODESW SET,AMODE=31          GO INTO 31-BIT
         AR    R3,R1                   GET THE VALUE
         AMODESW SET,AMODE=24          GO INTO 24-BIT
         ST    R3,EADDR                AND SAVE IT
*
* NOW THAT WE HAVE THE RANGES, LET'S COMPARE
*
VA0030   DS    0H
* BLOCK 1
         CLC   EADDR(4),SMCVPBEG       LOCATED IN INITIAL BLOCK ?
         BL    VA0060                  YES...PROCEED
* BLOCK 2
         CLC   EADDR(4),SMVPBEG        END IN NO-MANS LAND ?
         BL    VA0050                  YES...ERROR
         CLC   BADDR(4),SMVPBEG        END IN NO-MANS LAND ?
         BL    VA0050                  YES...ERROR
         CLC   BADDR(4),SMVPEND        EXIST IN THE MID BLOCK ?
         BNL   VA0032                  NO...TRY UPPER BLOCK
         CLC   EADDR(4),SMVPEND        EXIST IN THE MID BLOCK ?
         BNL   VA0032                  NO...TRY UPPER BLOCK
         B     VA0060                  IT'S OK
* BLOCK 3
VA0032   C     R7,BADDR                EXIST IN LAST BLOCK ?
         BH    VA0050                  NO...OOPS
         C     R7,EADDR                EXIST IN LAST BLOCK ?
         BH    VA0050                  NO...OOPS
         CLC   BADDR(4),EOCADR         EXIST IN LAST BLOCK ?
         BNL   VA0050                  NO...ERROR
         CLC   EADDR(4),EOCADR         EXIST IN LAST BLOCK ?
         BNL   VA0050                  NO...OOPS
         B     VA0060                  YES...IT'S OK
VA0050   MVC   RCODE(4),=F'20'         INDICATE A PROBLEM
VA0060   L     R11,MODE                REGAIN THE MODE
         BSM   R0,R11                  AND SET IT
VA0070   SUBRT ,                       RETURN TO CALLER
* -------------------- WORKING STORAGE ----------------- *
         LTORG ,
MODE     DS    F
* -------------------- DUMMY SECTIONS ------------------ *
PARM     DSECT                         SIMULATE PASSED PARM
PIK      DS  F                         PIK VALUE GOES IN HERE
BADDR    DS  F                         BEGINNING ADDRESS
EADDR    DS  F                         ENDING ADDRESS OR....
LEN      DS  F                         LENGTH
RCODE    DS  F                         RETURN CODE
*
COMREG   MAPCOMR                       COMREG LAYOUT
SYSCOM   SYSCOM                        SYSCOM LAYOUT
PCBADR   MAPPCB                        PCB LAYOUT
SMCOM    SMCOM                         SMCOM LAYOUT
         END        
/*
*
* Now link the whole app
*
// EXEC LNKEDT
*
/*
/& 
* $$ EOJ
