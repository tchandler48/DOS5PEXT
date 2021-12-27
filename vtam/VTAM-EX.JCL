* $$ JOB JNM=DOSVTAM,USER='BUDROW',CLASS=0,DISP=D
* $$ LST LST=00E,JSEP=1,CLASS=A,DISP=D
// JOB DOSVTAM
// ASSGN SYSLST,X'00E'
// ASSGN SYS001,X'362'
// ASSGN SYS002,X'362'
// ASSGN SYS003,X'362'
// ASSGN SYSLNK,X'362'
// OPTION LINK
// EXEC ASSEMBLY
         PRINT  NOGEN
R0       EQU       0
R1       EQU       1
R2       EQU       2
R3       EQU       3
R4       EQU       4
R5       EQU       5
R6       EQU       6
R7       EQU       7
R8       EQU       8
R9       EQU       9
R10      EQU       10
R11      EQU       11
R12      EQU       12
R13      EQU       13
R14      EQU       14
R15      EQU       15
BUDD     CSECT  
         BALR      R12,R0
         USING     *,R12,R11
         LA        R11,4095(R12)
         SR        R15,R15
         OPEN      ACB1
         LTR       R15,R15             CHECK FOR ERROR
         BNZ       ABEOJ                 
*
PROCEED  L         R3,=V(OPMSG)
         MVC       4(60,R3),=CL60'ACB OPEN SUCCESS          '
         BALR      R2,R3
         LA        R13,SAVE1
         SR        R15,R15
         SETLOGON  RPL=PRPL,OPTCD=START
         LTR       R15,R15             DID IT WORK ?
         BNZ       ABEOJ               NO...DUMP AND EXIT
         L         R3,=V(OPMSG)
         MVC       4(60,R3),=CL60'SETLOGON SUCCESS'
         BALR      R2,R3
         WAITM     TPENDECB,LOGONECB   WAIT FOR EITHER
         B         4(R1)
*
TPDOWN   CLOSE     ACB1                CLOSE THE ACB
         L         R3,=V(OPMSG)
         MVC       4(80,R3),=CL60'APPLBUDD - VTAM SHUTDOWN'
         BALR      R2,R3
         EOJ
*
TPRERROR LR        R6,R0               PDUMP DESTROYS R0,R15
         LR        R7,R15              MOVE THEM TO R6,R7
         L         R3,=V(OPMSG)
         MVC       4(60,R3),=CL60'VTAM RECEIVE ERROR'
         B         TPGO
*
TPSERROR LR        R6,R0               PDUMP DESTORYS R0,R15
         LR        R7,R15              MOVE THEM TO R6,R7
         L         R3,=V(OPMSG)
         MVC       4(60,R3),=CL60'VTAM SEND ERROR'
*
TPGO     BALR      R2,R3
         SHOWCB    AM=VTAM,RPL=(R5),                                   X
               FIELDS=(RTNCD,FDBK2,SSENSMI),                           X
               AREA=PDS,LENGTH=16
         PDUMP     PDS,PDE
         CLOSE     ACB1                CLOSE THE ACB
         EOJ
*
ABEOJ    L         R3,=V(OPMSG)
         MVC       4(60,R3),=CL60'ABNORMAL EOJ'
         BALR      R2,R3
         EOJ
*
MAIN     L         R3,=V(OPMSG)
         MVC       4(60,R3),=CL60'MAIN ROUTINE'
         BALR      R2,R3
         L         R5,=A(LOGONRPL)
         USING     IFGRPL,R5           MAP THE RPL LAYOUT
* SENDSCRN MVI       QSCRN+1,NOBEEP
SENDSCRN L         R3,=V(OPMSG)
         MVC       4(60,R3),=CL60'SENDSCRN'
         BALR      R2,R3
         LA        R8,QSCRN
         LA        R9,QSCRNSZ
         OI        RPLRH3,RPLBB
         SR        R15,R15
         DS        0H
         SEND      RPL=(R5),AREA=(R8),RECLEN=(R9),STYPE=REQ,           X
               CONTROL=DATA,CHAIN=ONLY,                                X
               OPTCD=(SYN,CS),POST=RESP,RESPOND=(EX,FME,RRN)
         LTR       R15,R15             DID IT WORK ?
         BNZ       TPSERROR            NO - BAIL
         BAL       R10,READTRM
*
GOTINP   CLI       DATA,C'Y'
         BE        GIVEQTE
         CLI       DATA,C'N'
         BE        CLRBEEP
         CLI       DATA,C'Q'
         BNE       BEEPMSG             INDICATE INVALID KEY
         LA        R8,EOJSCN
         LA        R9,EOJSCNZ
         OI        RPLRH3,RPLEB        TURN ON END BRACKET
         SR        R15,R15
         DS        0H
         SEND      RPL=(R5),AREA=(R8),RECLEN=(R9),STYPE=REQ,           X
               CONTROL=DATA,CHAIN=ONLY,                                X
               OPTCD=(SYN,CS),POST=RESP,RESPOND=(EX,FME,RRN)
         CLOSE     ACB1                CLOSE THE ACB
         L         R3,=V(OPMSG)
         MVC       4(60,R3),=CL60'APPLBUDD - USER REQUESTS SHUTDOWN'
         BALR      R2,R3
         EOJ
*
CLRBEEP  MVI       QSCRN+1,NOBEEP 
         B         CLSBRKT
*
BEEPMSG  MVI       QSCRN+1,BEEP    SOME 3270 CLIENTS HAVE NO SOUND
         B         CLSBRKT
*
GIVEQTE  L         R3,=V(OPMSG)
         MVC       4(60,R3),=CL60'GIVE QUOTE'
         BALR      R2,R3
         LA        R4,180
         MVI       QSCRN+1,NOBEEP
         L         R3,QNBR
         A         R3,=F'1'
         C         R3,=F'4'
         BL        GQ1
         SR        R3,R3
GQ1      ST        R3,QNBR
         MR        R2,R4
         LA        R2,QDB
         AR        R3,R2
         MVC       QT1(60),0(R3)
         A         R3,=F'60'
         MVC       QT2(60),0(R3)
         A         R3,=F'60'
         MVC       QT3(60),0(R3)
         LA        R8,QUOTES
         LA        R9,QUOTESZ
         SR        R15,R15
         DS        0H
         SEND      RPL=(R5),AREA=(R8),RECLEN=(R9),STYPE=REQ,           X
               CONTROL=DATA,CHAIN=ONLY,                                X
               OPTCD=(SYN,CS),POST=RESP,RESPOND=(EX,FME,RRN)
         LTR       R15,R15             DID IT WORK ?
         BZ        CONTWAIT            YES...PROCEED
         B         TPSERROR
*
CONTWAIT BAL       R10,READTRM
*
CLSBRKT  LA        R8,ENDBRK
         LA        R9,ENDBRKZ
         OI        RPLRH3,RPLEB        SET END BRACKET
         SR        R15,R15
         DS        0H
         SEND      RPL=(R5),AREA=(R8),RECLEN=(R9),STYPE=REQ,           X
               CONTROL=DATA,CHAIN=ONLY,                                X
               OPTCD=(SYN,CS),POST=RESP,RESPOND=(EX,FME,RRN)
         LTR       R15,R15             DID IT WORK ?
         BZ        SENDSCRN            YES...PROCEED
         B         TPSERROR
*
READX    L         R3,=V(OPMSG)
         TESTCB    AM=VTAM,RPL=(R5),SSENSEI=PATH
         BNE       READX1
         MVC       4(60,R3),=CL60'SSENSEI=PATH'
         BALR      R2,R3
         B         READXX
*
READX1   L         R3,=V(OPMSG)
         TESTCB    AM=VTAM,RPL=(R5),SSENSEI=CPM
         BNE       READX2
         MVC       4(60,R3),=CL60'SSENSEI=CPM'
         BALR      R2,R3
         B         READXX
*
READX2   L         R3,=V(OPMSG)
         TESTCB    AM=VTAM,RPL=(R5),SSENSEI=STATE
         BNE       READX3
         MVC       4(60,R3),=CL60'SSENSEI=STATE'
         BALR      R2,R3
         B         READXX
READX3   L         R3,=V(OPMSG)
*
         TESTCB    AM=VTAM,RPL=(R5),SSENSEI=FI
         BNE       READX4
         MVC       4(60,R3),=CL60'SSENSEI=FI'
         BALR      R2,R3
         B         READXX
*
READX4   L         R3,=V(OPMSG)
         TESTCB    AM=VTAM,RPL=(R5),SSENSEI=RR
         BNE       READX5
         MVC       4(60,R3),=CL60'SSENSEI=RR'
         BALR      R2,R3
         B         READXX
*
READX5   L         R3,=V(OPMSG)
         MVC       4(60,R3),=CL60'SSENSEI=INVALID'
         BALR      R2,R3
*
READXX   SHOWCB    AM=VTAM,RPL=(R5),                                   X
               FIELDS=(RTNCD,FDBK2,SSENSMO),                           X
               AREA=PDS,LENGTH=12
         PDUMP     PDS,PDE
*
READTRM  SR        R15,R15
         RECEIVE   RPL=(R5),AREA=BUFFER,AREALEN=100,STYPE=REQ,         X
               OPTCD=(SYN,SPEC,CS,Q,TRUNC),RTYPE=(DFSYN,DFASY,RESP)
         LTR       R15,R15             DID IT WORK ?
         BNZ       READX               NO - BAIL
         TM        RPLRH3,RPLBB+RPLEB  IS IT BB,EB ?
         BOR       R10                 YES...RETAIN THAT
         TM        RPLRH3,RPLBB        IS IT BB,NEB ?
         BO        READ02              YES...MAKE IT CENTRIST
         TM        RPLRH3,RPLEB        IS IT NBB,EB ?
         BO        READ01              YES...MAKE IT FULL
         BR        R10                 IT IS NBB,NEB - RETAIN
READ01   OI        RPLRH3,RPLBB        TURN ON BEGIN BRACKET
         OI        RPLRH3,RPLEB        TURN ON END BRACKET
         BR        R10                 RETURN TO CALLER
READ02   NI        RPLRH3,X'FF'-RPLBB  TURN OFF BEGIN BRACKET
         NI        RPLRH3,X'FF'-RPLEB  TURN OFF END BRACKET
         BR        R10                 RETURN TO CALLER
*
PDS      DC        F'0'
PD1      DC        F'0'
PD2      DC        F'0'
PDE      DC        F'0'
BUFFER   DS        0CL1000             BUFFER RETURNED FROM RECEIVE
KEYPRESS DS        X                   3270 ACTION KEY PRESSED
INSBA    DS        XL2                 SBA WHERE CURSOR WAS
HEX11    DS        X                   X'11' START FIELD
INSTART  DS        XL2                 SBA WHERE FIELD STARTED
DATA     DS        CL94                DATA THAT WAS PASSED
*
WRTERASE EQU       X'F5'               SOME 3270 CONSTANTS
BEEP     EQU       X'87'
NOBEEP   EQU       X'83'
QSCRN    EQU       *                                                       
         DC        AL1(WRTERASE),AL1(NOBEEP)
         $SBA      (01,25)
         DC        X'2902C0C042F5'
         DC        C'VERY BASIC VTAM EXAMPLE APPLICATION'
         $SBA      (03,01)
         DC     X'2902C0C042F2'
         DC        C'DO YOU WANT A QUOTE?'
         $SBA      (05,01)
         DC     X'2902C0C042F4'
         DC        C'Enter Y OR N OR Q TO QUIT'
         $SBA      (23,20)
         DC     X'2902C0C042F3'
         DC        C'Copyright 2012 Harold Bell VintageBigBlue.org'
         $SBA      (5,27)
         $SF
         DC        X'13'
         $SBA      (5,42)
         $SF
QSCRNSZ  EQU       *-QSCRN
ENDBRK   EQU       *
         DC        AL1(WRTERASE),AL1(NOBEEP)
         $SBA      (01,01)
         $SF
         DC        C'END TRANSACTION'
ENDBRKZ  EQU       *-ENDBRK
EOJSCN   EQU       *
         DC        AL1(WRTERASE),AL1(NOBEEP)
         $SBA      (01,01)
         $SF
         DC        C'VTAM APPLICATION EOJ'
EOJSCNZ  EQU       *-EOJSCN
QUOTES   EQU       *
         DC        AL1(WRTERASE),AL1(NOBEEP)
         $SBA      (01,01)
         $SF
         DC        C'ENTER TO CONTINUE'
         $SBA      (10,10)
         DC        X'2902C0C042F5'
QT1      DS        CL60
         $SBA      (11,10)
QT2      DS        CL60
         $SBA      (12,10)
QT3      DS        CL60
         $SF
         DC        X'13'
         $SBA      (12,72)
         $SF
QUOTESZ  EQU       *-QUOTES
QNBR     DC        F'0'
QDB DC  CL60'  A man''s mind, stretched by a new idea,can never'
    DC  CL60'go back to it s original dimension.'
    DC  CL60'OLIVER WENDELL HOLMES'
    DC  CL60'  The best thing for being sad is to learn something.'
    DC  CL60'That is the only thing that never fails.'
    DC  CL60'T. H. WHITE'
    DC  CL60'  Foolproof systems don''t take into account'
    DC  CL60'the ingenuity of fools.'
    DC  CL60'GENE BROWN'
    DC  CL60'  You can judge your age by the amount of pain you'
    DC  CL60'feel when you come in contact with a new idea.'
    DC  CL60'JOHN NUVEEN'
TPENDECB DC        F'0'                ECB FOR VTAM SHUTDOWN
         B         TPDOWN
LOGONECB DC        F'0'                ECB INDICATES A LOGON OCCURRED
         B         MAIN
CNVRTHX  LA        R1,CNVWORD          POINT TO PASSED PARM
         LA        R2,4(R1)            POINT TO OUTPUT FIELD
         UNPK      0(7,R2),0(4,R1)     EXPAND THE FULLWORD
         MVC       7(1,R2),3(R1)       MODIFY THE LAST BYTE
         NC        0(8,R2),ZEROF       SPLIT THE BYTES APART
         TR        0(8,R2),TRTABLE     TRANSLATE FOR THE DISPLAY
         BR        R14
CNVWORD  DS        3F
HEXCHAR  DS        CL8
ZEROF    DC        8XL1'0F'                FOR SPLITTING APART BYTES
TRTABLE  DC        CL16'0123456789ABCDEF'  FOR MAKING HEX DISPLAYABLE
ACB1     ACB       AM=VTAM,APPLID=APPLID,EXLST=EXLST1,MACRF=LOGON
APPLID   DC        AL1(4),C'BUDD'
EXLST1   EXLST     AM=VTAM,LOGON=LOGNEXIT
PRPL     RPL       AM=VTAM,ACB=ACB1
SAVE1    DS        18F
         LTORG 
OPMSG    CSECT                         OPERATOR MESSAGE
         USING     OPMSG,R3
         B         SKIP01
MSGTXT   DC        CL80' '
CONSTAT  DC        X'0'
OPCONS  DTFCN DEVADDR=SYSLOG,IOAREA1=MSGTXT,TYPEFLE=OUTPUT,BLKSIZE=60
SAV0     DS        F
SAV1     DS        F
SKIP01   ST        R0,SAV0
         ST        R1,SAV1
         CLI       CONSTAT,X'0'
         BNE       SKIP02
         OPEN      OPCONS
         MVI       CONSTAT,X'FF'
SKIP02   PUT       OPCONS
         L         R0,SAV0
         L         R1,SAV1
         BR        R2
         LTORG
LOGNEXIT CSECT                         ACCEPT LOGON
         USING     LOGNEXIT,R15
         STM       R14,R12,LGEXTSAV+12
         LR        R12,R15             SETUP THE BASE
         DROP      R15
         USING     LOGNEXIT,R12        AND ADDRESSABILITY
         LR        R6,R1               GET THE PASSED PARM
*
* 0(R6) = ACB ADDRESS  4(R6) = SLU NAME
* 20(R6) = CID OF SESSION TO BE ESTABLISHED WITH THE LU
*
         L         R4,4(R6)            GET THE SLUNAME
         L         R3,=V(OPMSG)
         MVC       4(60,R3),=CL60'LOGON FROM '
         MVC       15(8,R3),0(R4)
         BALR      R2,R3
         LA        R5,LOGONRPL         SETUP RPL BASE DSECT
         LA        R7,PNIB             ADDRESS THE NIB LAYOUT
         USING     IFGRPL,R5           MAP THE RPL LAYOUT
         USING     ISTDNIB,R7          MAP THE NIB LAYOUT
*
* BUILD THE NIB FOR THIS USER BASED ON INFO PASSED.  WE WIL USE
* THE LAST 4-BYTES OF THE LUNAME AS A UNIQUE USER FIELD ID.
*
         MVC       NIBSYM(8),0(R4)     GET THE SYMBOLIC LUNAME
         MVC       NIBCID(4),20(R6)    PUT CID INTO NIB FOR OPNDST
         MVC       NIBUSER(4),4(R4)    USE LAST 4 BYTES OF LUNAME
         MVC       RPLUSFLD,4(R4)      AS A UNIQUE USER FIELD ID
         MVC       RPLACB(4),0(R6)     SET ACB IN RPL
         LA        R13,LGSAVE1
         SR        R15,R15
OPNDSTS  DS        0H
         OPNDST RPL=LOGONRPL,NIB=(R7),OPTCD=(ACCEPT,SPEC,NQ)
         LTR       R15,R15             SESSION ESTABLISHED ?
         BZ        LGNGD               YES...PROCEED
         LR        R6,R0               PDUMP DESTROYS R0,R15
         LR        R7,R15              MOVE THEM TO R6,R7
         PDUMP     WRKAREA,LOGTXT      FOR VIEWING
         L         R3,=V(OPMSG)
         MVC       4(60,R3),=CL60'LOGON PROBLEM'
         BALR      R2,R3
         EOJ
LGNGD    POST      LOGONECB            POINT TO OUR ECB
         LM        R14,R12,LGEXTSAV+12
         BR        R14
WRKAREA  DC        2F'0'
LOGTXT   DC        CL76'LOGGING IN'
LOGONRPL RPL       AM=VTAM,RESPOND=FME
RPLEN    EQU       *-LOGONRPL
PNIB     NIB       MODE=RECORD,PROC=(RESPX,TRUNC)
LGSAVE1  DS        18F
LGEXTSAV DS        18F
         LTORG  
         IFGACB    AM=VTAM
         IFGEXLST  AM=VTAM
         ISTDNIB
         ISTUSFBC
         IFGRPL    AM=VTAM
         END BUDD
/*
// EXEC LNKEDT
// EXEC ,SIZE=2048K
/*
/&
* $$ EOJ
