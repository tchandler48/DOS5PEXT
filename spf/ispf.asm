        CATALS   A.VSBR,0.0
        BKEND   A.VSBR
*------------------------------------------------------------------
*     NAME: VSBR
*     TYPE: SUB-ROUTINE
* FUNCTION: Browse a member
*
* INTERNAL
*           CALLERR   Call Errorprogram
*
* EXTERNAL
*           VSRCSBA   Calculate SBA --> ROW/COL and vice versa
*
*   AUTHOR: REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
* --------------------------------------------------------------------* 00000008
* Browse MEMBER
* --------------------------------------------------------------------* 00000015
VSBR     CSECT
         USING *,R15                                                    00000017
         STM   R14,R12,12(R13)         STORE REGS IN SAVEAREA           00000018
         DROP  R15                     R15 TO BE USED                   00000019
         USING VSBR,R5,R6              R5 R6
         LR    R5,R15                  ESTABLISH ADDRESSABILITY         00000021
         LA    R6,4095(R5)                                              00000022
         LA    R6,1(R6)                SECOND BASE ESTABLISHED          00000023
         B     GOON
EYECATCH DC    C'***** VSBR *****'     EYECATCHER
GOON     DS    0H
         ST    R13,SAVEAREA+4          STORE MAIN PGM SAVEAREA          00000024
         LR    R10,R13                 SAVE R13                         00000025
         LA    R13,SAVEAREA            SAVE AREA FOR THIS TASK          00000026
         ST    R13,8(R10)              STORE EXIT SAVE ADDRESS          00000027
* --------------------------------------------------------------------* 00000028
*        STATEMENTS FOR SET UP FIELDS NEEDED FOR VSBR                 * 00000029
* --------------------------------------------------------------------* 00000030
         L     R2,0(R1)                PARMS ADDRESS                    00000031
         ST    R2,VISADR               SAVE                             00000032
         LR    R7,R2
         USING WORKA,R7
         L     R9,PBUFADR              LOAD DATATAB ADDRESS             00000034
         ST    R9,TABRECSA             SET  TABRECS ADDRESS             00000035
         USING TABRECS,R9              ESTABLISH ADDRESSABILITY         00000036
         L     R10,PMAPADDR            LOAD OUTPUT MAP ADDRESS
         USING MAPDS,R10               ESTABLISH ADDRESS FOR MAPDS
         L     R8,MAPVCNT           LOAD ADDR-LIST-ADDR
         ICM   R8,8,X00             CLEAR CNT-BYTE
         L     R9,0(R8)             LOAD 1. VAR-OUT-ADDR SBAIC
         ST    R9,SBAICAD
         L     R9,4(R8)             LOAD 2. VAR-OUT-ADDR HDR1SZ
         ST    R9,HDR1SZAD
         L     R9,8(R8)             LOAD 3. VAR-OUT-ADDR LINE1
         ST    R9,LINE1AD
         L     R9,12(R8)            LOAD 4. VAR-OUT-ADDR LINE2
         ST    R9,LINE2AD
         L     R9,TABRECSA
         DROP  R10
         B     CHKFUNC              CHECK, WHAT IS THE FUNCTION
SBAICAD  DS    F
HDR1SZAD DS    F
LINE1AD  DS    F
LINE2AD  DS    F
* --------------------------------------------------------------------* 00000065
*     PGSTART PROGRAM-START                                           * 00000066
* --------------------------------------------------------------------* 00000067
CHKFUNC  DS    0H
******   CNSLOUT MSG='PGM-START'
******   CNSLOUT ADR=PARMS
         MVC   PRETCOD,=CL3'000'
         BAL   R8,CALLERR
         CLC   PFUNC,=CL3'BR0'     BROWSE MEMBER 1. TIME ?
         BE    BR0000              YES
         CLC   PFUNC,=CL3'BR7'     BROWSE BACKWARD ?
         BE    BR7000              YES
         CLC   PFUNC,=CL3'BR8'     BROWSE FORWARD ?
         BE    BR8000              YES
         CLC   PFUNC,=CL3'BRF'     FIND ARGUMENT IN PFARG
         BE    BRF000              YES
         CLC   PFUNC,=CL3'ENT'     ENTER-TASTE   ?
         BE    ENT000              YES
         MVC   PRETCOD,=CL3'882'   INVALID FUNCTION
         BAL   R8,CALLERR
         B     PGMEND
*----------------------------------------------------------------
*  GET OK, Borwse MEMBER in MAP
*----------------------------------------------------------------
BR0000   DS    0H                                                       00000761
         ST    R13,SAV13
         L     R1,PBUFADR          INPUT BUFFER ADDRESS
         L     R3,PNORL            NO OF INPUT RECORDS
         BCTR  R3,0                NUMBER - 1                           00000764
         MH    R3,RECLEN           MULTIPLY NUMBER WITH RECLEN
         AR    R3,R1               TO GET LAST INPUT RECORD ADDRESS
         ST    R3,SAVENDBA         SAVE END INPUT ADDR
BRDSPLY  DS    0H                                                       00000768
         ST    R1,SAVCURR          1.INPUT ADDR AS CURR. RECD-ADDR
         L     R2,LINE1AD          1.ROW OF 3270-OUTPUT MAP SPFM010
         LA    R4,20               NUMBER OF OUTPUT-ROWS
         L     R10,HDR1SZAD        ADDR of HEADERSIZE OF MAP
*                                                                       00000774
BRNXTREC DS    0H                                                       00000775
         ST    R4,SAV4             RH
         C     R1,SAVENDBA         IS IT THE LAST INPUT RECORD ?
         BH    BRNXTEOF            YES                                  00000777
         LR    R12,R1              LOAD CURR RECORD ADDRESS
         AH    R12,VIEW            ADD SHIFT
         MVC   0(79,R2),BLANKS     CLEAR CURR OUTPUT MAP-ROW
         LH    R11,RECLEN          LOAD RECORD LENGTH (DEFAULT 80)
         SH    R11,VIEW            MINUS SHIFT
         CH    R11,=H'79'          LENGTH > 79
         BNH   BRNXTR01            NO,
         LH    R11,=H'79'          YES, LENGTH = 79
BRNXTR01 DS    0H
         BCTR  R11,0               MINUS 1 FOR MOVE
         STC   R11,BRNXTR02+1      SET MOVE LENGTH
BRNXTR02 MVC   0(79,R2),0(R12)     DSPLY RECORD                         00000788
*                                                                       00000806
BRNXTR03 DS    0H                  NEXT INPUT RECORD
         AH    R1,RECLEN
         LA    R2,84(R2)           NEXT OUTPUT DSPLY LINE
         LA    R10,84(R10)         NEXT SBAID
         BCT   R4,BRNXTREC                                              00000810
*
         ST    R1,SAVNEXTB         SAVE NEXT REC ADDRESS                00000811
         ST    R10,MSGLEN          SAVE MSGLEN
BRNXTRD1 DS    0H                   REDISPLAY
         L     R13,SAV13
         B     PGMEND                                                   00000813
*
BRNXTEOF DS    0H                   SET EOF-MSG                         00000814
         MVC   0(79,R2),BLANKS      CLEAR
         MVC   0(24,R2),EOFMSG
         BCTR  R4,0
BREOFCLR DS    0H
         LA    R2,84(R2)            NEXT ROW
         MVC   0(79,R2),BLANKS      CLEAR
         BCT   R4,BREOFCLR          REST
*
         MVC   PRETCOD,=CL3'993'    EOF OF MEMBER REACHED               00000815
         LA    R10,84(R10)          NEXT HDR1SZ
         ST    R10,MSGLEN           MSGLEN
*
         L     R13,SAV13
         B     PGMEND                                                   00000818
SAV4     DS    F
EOFMSG   DS    0CL26
         DC    XL2'1DE8'
         DC    C'***** END OF FILE ***** '                              00001598
******   CNSLCCB
* --------------------------------------------------------------------* 00000065
*   BR7 Backward  PF7                                                 * 00000066
* --------------------------------------------------------------------* 00000067
BR7000   DS    0H
         L     R4,SAV4             RH
         LH    R2,RECLEN                                                00000390
         CLC   PSCRL,=CL2'MX'      MAX-NUMBER OF LINES UP
         BE    BR7030              YES, GO TO TOP OF LIST
         CLC   PSCRL,=H'0'         NUMBER OF LINES IN PSCRL
         BE    BR7010              NO VALUE
         MH    R2,PSCRL            PSCRL-VALUE
         B     BR7020
BR7010   DS    0H
         MH    R2,=H'20'           20 ROWS                              00000391
BR7020   DS    0H
         CLC   SAVCURR,SAVENDBA    WE POINT TO LAST RECORD ?            00000392
         BNE   BACK                NO,                                  00000393
         SH    R2,RECLEN           YES, BACK 19 LINES ONLY              00000394
BACK     DS    0H                                                       00000395
         L     R1,SAVCURR          LOAD CURRENT REC ADDRESS             00000396
         LTR   R1,R1               SOME ADDRESS ?                       00000397
         BZ    PGMEND              NO                                   00000398
         SR    R1,R2               BACK NN LINES                        00000399
         C     R1,PBUFADR          POINTER OK ?                         00000400
         BNL   BRDSPLY             YES                                  00000401
BR7030   DS    0H                  START FROM THE BEGINNING
         L     R1,PBUFADR          LOAD BEGIN BUFFER ADDRESS
         MVC   PRETCOD,=CL3'992'   TOP OF MEMBER REACHED
         B     BRDSPLY             FIRST RECORD
* --------------------------------------------------------------------* 00000065
*   BR8 Forward  PF8                                                  * 00000066
* --------------------------------------------------------------------* 00000067
BR8000   DS    0H
         CLC   PSCRL,=CL2'MX'
         BNE   BR8010
         L     R1,SAVENDBA         LAST RECORD
         SH    R1,RECLEN           -1 TO DISPLAY AT LEAST 1 RECORD
         B     BR8END
BR8010   DS    0H
         CLC   PSCRL,=H'0'         NUMBER OF LINES IN PSCRL
         BE    BR8AMT               NO
         LH    R4,PSCRL             AMOUNT LINES FORWARD
         MH    R4,RECLEN            MULTIPLY WITH RECORDLEN
         L     R1,SAVCURR           CURRENT RECORD ADDRESS
         AR    R1,R4                ADD DISPLACEMENT
         B     BR8END
BR8AMT   DS    0H
         L     R1,SAVNEXTB         LOAD NEXT REC ADDRESS
BR8END   DS    0H
         L     R4,SAV4             RH
         LTR   R1,R1               SOME ADDRESS ?
         BZ    PGMEND              NO
         B     BRDSPLY             FIRST RECORD
* --------------------------------------------------------------------* 00000065
*   BRF Find Argument (PFARG)                                         * 00000066
* --------------------------------------------------------------------* 00000067
BRF000   DS    0H
         LH    R2,PFLEN            R2 = LENGTH OF STRING                00000934
         BCTR  R2,0                -1                                   00000935
         STC   R2,BRFCLC+1         CHANGE CLC LENGTH                    00000936
         LH    R4,RECLEN           LAST COLUMN                          00000937
         SR    R4,R2               R4 = NN COLUMNS TO SCAN              00000938
         ST    R4,SAVER4           SAVE R4                              00000939
         L     R2,SAVCURR          LOAD CURRENT REC ADDRESS             00000940
         AH    R2,RECLEN           BEGIN WITH LINE 2                    00000941
BRFNEXTR DS    0H                                                       00000942
         LR    R3,R2                                                    00000942
         L     R4,SAVER4           RESTORE R4                           00000943
BRFCLC   CLC   0(00,R3),PFARG      FIND ARGUMENT FOUND ?                00000944
         BE    BRFHIT              YES                                  00000945
         LA    R3,1(R3)            NEXT COLUMN                          00000946
         BCT   R4,BRFCLC           LOOP                                 00000947
         AH    R2,RECLEN           NEXT RECORD                          00000948
         C     R2,SAVENDBA         END OF MEMBER ?                      00000949
         BNH   BRFNEXTR               NO,  SCAN NEXT RECORD                 0000
         MVC   PRETCOD,=CL3'982'      YES, STRING NOT FOUND              0000095
         BAL   R8,CALLERR          WRITE ERROR
         MVC   PSCRL,=CL2'NF'      MARK STRING NOT FOUND
BRFHIT   DS    0H                                                       00000953
         ST    R2,SAVCURR          SET CURRENT REC ADDRESS              00000953
******   CNSLOUT MSG='BRFHIT'
         L     R1,SAVCURR          LOAD CURRENT REC ADDRESS             00000263
         LTR   R1,R1               SOME ADDRESS ?                       00000264
         BZ    BRFEND              NO                                   00000265
         B     BRDSPLY             DISPLAY                              00000269
BRFEND   DS    0H
         B     PGMEND              PROGRAM-END
SAVER4   DS    F
* ---------------------------------------------------------------------
*   ENT ENTER-Taste
* ---------------------------------------------------------------------
ENT000   DS    0H
         CLC   PSCRL,=CL2'TP'         MAX TOP
         BNE   ENT010
         MVC   PSCRL,=CL2'MX'
         B     BR7000
ENT010   DS    0H
         CLC   PSCRL,=CL2'BT'         MAX BOTTOM
         BNE   ENT020
         MVC   PSCRL,=CL2'MX'
         B     BR8000
ENT020   DS    0H
         CLC   PSCRL,=CL2'NF'         STRING NOT FOUND
         BNE   ENT030
         XC    PSCRL,PSCRL         REINIT PSCRL
         L     R1,PBUFADR          LOAD CURRENT REC ADDRESS
         B     BRDSPLY             NEXT RECORD
ENT030   DS    0H
         L     R1,SAVCURR          LOAD CURRENT REC ADDRESS
         LTR   R1,R1               SOME ADDRESS ?
         BZ    ENT040 RH PGMEND    NO, RETURN
         B     BRDSPLY             NEXT RECORD
ENT040   DS    0H
         L     R1,PBUFADR          LOAD CURRENT REC ADDRESS
         B     BRDSPLY             NEXT RECORD
*----------------------------------------------------------------------
*     AUFRUF VSRCSBA
*----------------------------------------------------------------------
CALLVSRC DS    0H
         ST    R8,SAV8              SAVE R8
*
         MVC   PRETCOD,=CL3'000'
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*                                   ----------------------------------- 00000720
         CALL  VSRCSBA              AUFRUF CALC SBA --> ROW / COL VS
*                                   ----------------------------------- 00000720
         L     R8,SAV8             RESTORE R8
         BR    R8                  RETURN TO CALLER
         EJECT
         DS    0F                                                       00001640
SAV8     DS    2F                                                       00001640
SBAWORK  DS    CL2                                                      00001641
SBAROW   DS    CL1                                                      00001642
SBACOL   DS    CL1                                                      00001643
*                                                                       00001644
*-------------------------------------------------------------------
CALLERR  DS    0H
*-------------------------------------------------------------------
*******  CNSLOUT MSG='CALLERR'
         ST    R8,ERRR8          SAVE R8
*
         LA    R1,PARMS
         ST    R1,PARMLIST
         LA    R1,PARMLIST       CALL VSERROR -MAP-PROGRAM
         L     R15,VSERRAD
         BALR  R14,R15
*
         L     R8,ERRR8            -----------------------------------
         BR    R8                     RETURN TO CALLER
*-----------------------            --------------------------------
VSERRAD  DC    V(VSERROR)
ERRR8    DS    F
* --------------------------------------------------------------------* 00000065
*     PGMEND Program-ENd                                              * 00000066
* --------------------------------------------------------------------* 00000067
PGMEND   DS    0H
******   CNSLOUT ADR=PARMS
******   CNSLOUT MSG='PGM-ENDE'
         L     R9,VISADR           LOAD PARAMETER-ADDR
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
* --------------------------------------------------------------------* 00000386
*        WORKFIELDS                                                   * 00000387
* --------------------------------------------------------------------* 00000015
*        REGISTER EQUATES                                             * 00000351
* --------------------------------------------------------------------* 00000352
R0       EQU   0                                                        00000353
R1       EQU   1                                                        00000354
R2       EQU   2                                                        00000355
R3       EQU   3                                                        00000356
R4       EQU   4                                                        00000357
R5       EQU   5                                                        00000358
R6       EQU   6                                                        00000359
R7       EQU   7                                                        00000360
R8       EQU   8                                                        00000361
R9       EQU   9                                                        00000362
R10      EQU   10                                                       00000363
R11      EQU   11                                                       00000364
R12      EQU   12                                                       00000365
R13      EQU   13                                                       00000366
R14      EQU   14                                                       00000367
R15      EQU   15                                                       00000368
* --------------------------------------------------------------------* 00000388
         DS    0D                                                       00000389
* --------------------------------------------------------------------* 00000369
*        SAVE AREA DECLARATIONS                                       * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F                                                       00000372
SAVEAREA DC    XL72'00'                 MONITOR TASK SAVE AREA           0000037
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS          00000379
IOAREND  DC    F'0'                     END-ADDR OF IOAREA
INAREND  DC    F'0'                     END-ADDR OF INAREA
INLINEAD DC    F'0'                     END-ADDR OF INLINA
VISADR   DS    F                                                        00000458
SAVINPAD DS    F                        SAVE INPUT LINE ADDRESS         00000458
SAVRECAD DS    F                        SAVE INPUT LINE ADDRESS         00000458
SAV1     DS    F                                                        00000458
SAV13    DS    F                                                        00000458
X00      DC    XL1'00'                                                  00000458
WEDLIN   DS    CL6                 WORK BROWSE LINE AREA
SAVLINES DS    F                   SAVE LINES TO ADD, DUP, INS          00001630
EDLMDTAD DS    F                   SAVE EDITOR LINE MDT ADDR            00001631
STACKSW  DS    CL1                 STACK OPEN SW                        00001608
ADDSW    DC    C'0'                ADD COMMAND SW                       00001605
SWCMD    DS    C                   SW COMMAND                           00001609
SWNUM    DS    C                   SW CMD NUMERIC OPERAND               00001610
TEMP     DS    CL80                WORK                                 00001616
*                                                                       00000459
DBL      DS    D                 RH                                     00001622
BLANKS   DC    255CL1' '
SCALE    DC    C'....+....1....+....2....+....3....+....4....+....5....*00001599
               +....6....+....7... (X)'                                 00001600
*
HELPSW   DC    C'0'                HELP ACTIVE SW                       00001606
MSGLEN   DS    F                                                        00001575
FLAGS    DC    X'00'               BYTE OF FLAGS                        00001576
PRINTABL DC    256AL1(*-PRINTABL)  00-FF                                00001541
         ORG   PRINTABL                                                 00001542
         DC    64X'E1'             00-3F                                00001543
         ORG                                                            00001544
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        00001563
BINTOEBC DC    X'40C1C2C3C4C5C6C7C8C94A4B4C4D4E4F'  0                   00001564
         DC    X'50D1D2D3D4D5D6D7D8D95A5B5C5D5E5F'  1                   00001565
         DC    X'6061E2E3E4E5E6E7E8E96A6B6C6D6E6F'  2                   00001566
         DC    X'F0F1F2F3F4F5F6F7F8F97A7B7C7D7E7F'  3                   00001567
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                   * 00000456
* --------------------------------------------------------------------* 00000457
PARMLIST DS    F
* --------------------------------------------------------------------* 00000424
         LTORG                                                          00000454
* --------------------------------------------------------------------* 00000426
         DS    0D                                                       00000427
* --------------------------------------------------------------------* 00000474
WORKA    DSECT
         COPY  LIBPARMS                                                 00000460
TABRECS  DSECT                                                          00000475
TABREC   DS    CL80                                                     00000476
*---------------------------------------------------------------------
*        LOAD-AREA FOR OUTBOUND-MAPS
*---------------------------------------------------------------------
MAPDS    DSECT
MAPADDR  DS    0CL2008
MAPNAME  DS    CL8
MAPEYEC  DS    CL8
         DS    CL2
MAPLEN   DS    CL2                MAP-LEN
MAPVCNT  DS    AL1(0)             VAR-FIELD COUNT
MAPVLST  DS    AL3(0)             ADDR MAP-VAR-OUTPUT-LIST
MAPSTART DS    CL2000
         END                                                            00000481
        BKEND
        CATALS   A.VSDDIR,0.0
        BKEND   A.VSDDIR
*------------------------------------------------------------------
*     NAME: VSDDIR
*     TYPE: SUB-ROUTINE
* FUNCTION: Display a directory of a Library
*
* INTERNAL
*
*
* EXTERNAL
*              VSRCSBA   Calculate SBA --> ROW / COL VS
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
* --------------------------------------------------------------------*
* DISPLAY A DIRECTORY
* --------------------------------------------------------------------*
VSDDIR   CSECT
         USING *,R15
         STM   R14,R12,12(R13)         STORE REGS IN SAVEAREA
         DROP  R15                     R15 TO BE USED
         USING VSDDIR,R5,R6            R5 R6
         LR    R5,R15                  ESTABLISH ADDRESSABILITY
         LA    R6,4095(R5)
         LA    R6,1(R6)                SECOND BASE ESTABLISHED
         B     GOON
EYECATCH DC    C'***** VSDDIR *****'   EYECATCHER
GOON     DS    0H
******   CNSLOUT MSG='PGM-START'
         ST    R13,SAVEAREA+4          STORE MAIN PGM SAVEAREA
         LR    R10,R13                 SAVE R13
         LA    R13,SAVEAREA            SAVE AREA FOR THIS TASK
         ST    R13,8(R10)              STORE EXIT SAVE ADDRESS
* --------------------------------------------------------------------*
*        STATEMENTS FOR SET UP FIELDS NEEDED FOR VSDDIR               *
* --------------------------------------------------------------------*
         L     R2,0(R1)                PARMS ADDRESS
         ST    R2,VISADR               SAVE
         LR    R7,R2
         USING WORKA,R7
         L     R9,PBUFADR              LOAD DATATAB ADDRESS
         ST    R9,TABRECSA             SET  TABRECS ADDRESS
         USING TABRECS,R9              ESTABLISH ADDRESSABILITY
         L     R10,PMAPADDR            LOAD OUTPUT MAP ADDRESS
         USING MAPDS,R10               ESTABLISH ADDRESS FOR MAPDS
         L     R8,MAPVCNT           LOAD ADDR-LIST-ADDR
         ICM   R8,8,X00             CLEAR CNT-BYTE
         L     R9,0(R8)             LOAD 1. VAR-OUT-ADDR SBAIC
         ST    R9,SBAICAD
         L     R9,4(R8)             LOAD 2. VAR-OUT-ADDR HDR1SZ
         ST    R9,HDR1SZAD
         L     R9,8(R8)             LOAD 3. VAR-OUT-ADDR LINE1
         ST    R9,LINE1AD
         L     R9,12(R8)            LOAD 4. VAR-OUT-ADDR LINE2
         ST    R9,LINE2AD
         L     R9,TABRECSA
         DROP  R10
         B     CHKFUNC              CHECK, WHAT IS THE FUNCTION
SBAICAD  DS    F
HDR1SZAD DS    F
LINE1AD  DS    F
LINE2AD  DS    F
* --------------------------------------------------------------------*
*     PGSTART PROGRAM-START                                           *
* --------------------------------------------------------------------*
CHKFUNC  DS    0H
         CLC   PFUNC,=CL3'DI0'     BROWSE MEMBER 1. TIME ?
         BE    BR0000              YES
         CLC   PFUNC,=CL3'DI7'     BROWSE BACKWARD ?
         BE    BR7000              YES
         CLC   PFUNC,=CL3'DI8'     BROWSE FORWARD ?
         BE    BR8000              YES
         CLC   PFUNC,=CL3'DIL'     LOCATE X ?
         BE    BRL000              YES
         CLC   PFUNC,=CL3'ENT'     ENTER-TASTE   ?
         BE    ENT000              YES
         MVC   PRETCOD,=CL3'882'   INVALID FUNCTION
         B     PGMEND
*----------------------------------------------------------------
*  GET OK, Directory From Buffer into Map SPMF013
*----------------------------------------------------------------
BR0000   DS    0H
         ST    R13,SAV13
         L     R1,PBUFADR          INPUT BUFFER ADDRESS
         L     R3,PNORL            NO OF INPUT RECORDS
         BCTR  R3,0                NUMBER - 1
         MH    R3,RECLEN           MULTIPLY NUMBER WITH RECLEN
         AR    R3,R1               TO GET LAST INPUT RECORD ADDRESS
         ST    R3,SAVENDBA         SAVE END INPUT ADDR
BRDSPLY  DS    0H
         LA    R4,20               NUMBER OF OUTPUT-ROWS
         ST    R1,SAVCURR          SAVE INPUT ADDR AS CURR. RECD-ADDR
         L     R2,LINE1AD          1.ROW OF 3270-OUTPUT MAP SPFM013
*
BRNXTREC DS    0H
         ST    R4,SAV4             RH
         C     R1,SAVENDBA         IS IT THE LAST INPUT RECORD ?
         BH    BRNXTEOF            YES
         LR    R12,R1              LOAD CURR RECORD ADDRESS
         AH    R12,VIEW            ADD SHIFT
         MVC   0(77,R2),BLANKS     CLEAR CURR OUTPUT MAP-ROW
         LH    R11,RECLEN          LOAD RECORD LENGTH (DEFAULT 80)
         SH    R11,VIEW            MINUS SHIFT
         CH    R11,=H'79'          LENGTH > 79
         BNH   BRNXTR01            NO,
         LH    R11,=H'79'          YES, LENGTH = 79
BRNXTR01 DS    0H
         BCTR  R11,0               MINUS 1 FOR MOVE
         STC   R11,BRNXTR02+1      SET MOVE LENGTH
BRNXTR02 MVC   0(79,R2),0(R12)     DSPLY RECORD
*
         AH    R1,RECLEN           INCREASE INPUT ADDR
         LA    R2,88(R2)   RH 84   NEXT OUTPUT DSPLY LINE
         LA    R10,88(R10) RH 84   NEXT SBAID
         BCT   R4,BRNXTREC
*
         ST    R1,SAVNEXTB         SAVE NEXT REC ADDRESS
BRNXTRD1 DS    0H                   REDISPLAY
         L     R13,SAV13
         B     PGMEND
*
BRNXTEOF DS    0H                   SET EOF-MSG
         MVC   0(77,R2),BLANKS      CLEAR
         MVC   0(24,R2),EOFMSG
         BCTR  R4,0
BREOFCLR DS    0H
         LA    R2,88(R2)     RH 84  NEXT ROW
         MVC   0(77,R2),BLANKS      CLEAR
         BCT   R4,BREOFCLR          REST
*
******   MVC   PRETCOD,=CL3'993'    EOF OF MEMBER REACHED
*
         L     R13,SAV13
         B     PGMEND
SAV4     DS    F
EOFMSG   DS    0CL26
         DC    XL2'1DE8'
         DC    C'***** END OF FILE ***** '
******   CNSLCCB
* --------------------------------------------------------------------*
*   BR7 Backward  PF7                                                 *
* --------------------------------------------------------------------*
BR7000   DS    0H
         L     R4,SAV4             RH
         LH    R2,RECLEN
         CLC   PSCRL,=CL2'MX'      MAX-NUMBER OF LINES UP
         BE    BR7030              YES, GO TO TOP OF LIST
         CLC   PSCRL,=H'0'         NUMBER OF LINES IN PSCRL
         BE    BR7010              NO VALUE
         MH    R2,PSCRL            PSCRL-VALUE
         B     BR7020
BR7010   DS    0H
         MH    R2,=H'20'           20 ROWS
BR7020   DS    0H
         CLC   SAVCURR,SAVENDBA    WE POINT TO LAST RECORD ?
         BNE   BACK                NO,
         SH    R2,RECLEN           YES, BACK 19 LINES ONLY
BACK     DS    0H
         L     R1,SAVCURR          LOAD CURRENT REC ADDRESS
         LTR   R1,R1               SOME ADDRESS ?
         BZ    PGMEND              NO
         SR    R1,R2               BACK NN LINES
         C     R1,PBUFADR          POINTER OK ?
         BNL   BRDSPLY             YES
BR7030   DS    0H                  START FROM THE BEGINNING
         L     R1,PBUFADR          LOAD BEGIN BUFFER ADDRESS
         MVC   PRETCOD,=CL3'992'   TOP OF MEMBER REACHED
         B     BRDSPLY             FIRST RECORD
* --------------------------------------------------------------------*
*   BR8 Forward  PF8                                                  *
* --------------------------------------------------------------------*
BR8000   DS    0H
         CLC   PSCRL,=CL2'MX'
         BNE   BR8010
         L     R1,SAVENDBA         LAST RECORD
         SH    R1,RECLEN           -1 TO DISPLAY AT LEAST 1 RECORD
         B     BR8END
BR8010   DS    0H
         CLC   PSCRL,=H'0'         NUMBER OF LINES IN PSCRL
         BE    BR8AMT               NO
         LH    R4,PSCRL             AMOUNT LINES FORWARD
         MH    R4,RECLEN            MULTIPLY WITH RECORDLEN
         L     R1,SAVCURR           CURRENT RECORD ADDRESS
         AR    R1,R4                ADD DISPLACEMENT
         B     BR8END
BR8AMT   DS    0H
         L     R1,SAVNEXTB         LOAD NEXT REC ADDRESS
BR8END   DS    0H
         L     R4,SAV4             RH
         LTR   R1,R1               SOME ADDRESS ?
         BZ    PGMEND              NO
         B     BRDSPLY             FIRST RECORD
* --------------------------------------------------------------------*
*   BRL Locate a Letter (first letter of a member name) (PFARG)         *
* --------------------------------------------------------------------*
BRL000   DS    0H
         L     R1,PBUFADR          FIRST RECORD
         CLI   PLIBNAME+1,C'S'     IS IT A SOURCE STMT DIRECTORY ?
         BE    BRL005              YES
         LA    R1,2(R1)            NO, SO INCREASE R2 FOR SUBLIBNAME
BRL005   DS    0H
         LH    R2,RECLEN
BRL010   DS    0H
         CLC   0(1,R1),PFARG       HIT ?
         BE    BRLHIT
         AR    R1,R2               NEXT RECORD
         C     R1,SAVENDBA         REACHED THE END ?
         BNH   BRL010              NO, LOOP
         MVC   PRETCOD,=CL3'982'  STRING NOT FOUND
         BAL   R8,CALLERR          WRITE MESSAGE
******   MVC   PSCRL,=CL2'NF'      MARK STRING NOT FOUND
         L     R1,SAVCURR          LOAD CURRENT REC ADDRESS             00000263
BRLHIT   DS    0H
         L     R4,SAV4             RH
         LTR   R1,R1               SOME ADDRESS ?
         BZ    PGMEND              NO
         B     BRDSPLY             DISPLAY
* ---------------------------------------------------------------------
*   ENT ENTER-Taste
* ---------------------------------------------------------------------
ENT000   DS    0H
         CLC   PSCRL,=CL2'TP'         MAX TOP
         BNE   ENT010
         MVC   PSCRL,=CL2'MX'
         B     BR7000
ENT010   DS    0H
         CLC   PSCRL,=CL2'BT'         MAX BOTTOM
         BNE   ENT020
         MVC   PSCRL,=CL2'MX'
         B     BR8000
ENT020   DS    0H
         L     R1,SAVCURR          LOAD CURRENT REC ADDRESS
         LTR   R1,R1               SOME ADDRESS ?
         BZ    PGMEND              NO, RETURN
         B     BRDSPLY             NEXT RECORD
*-------------------------------------------------------------------
CALLERR  DS    0H
*-------------------------------------------------------------------
******   CNSLOUT MSG='CALLERR'
         ST    R8,ERRR8          SAVE R8
*
         LA    R1,PARMS
         ST    R1,PARMLIST
         LA    R1,PARMLIST       CALL VSERROR -MAP-PROGRAM
         L     R15,VSERRAD
         BALR  R14,R15
*
         L     R8,ERRR8            -----------------------------------
         BR    R8                     RETURN TO CALLER
*-----------------------            --------------------------------
VSERRAD  DC    V(VSERROR)
ERRR8    DS    F
*----------------------------------------------------------------------
*     AUFRUF VSRCSBA
*----------------------------------------------------------------------
CALLVSRC DS    0H
         ST    R8,SAV8              SAVE R8
*
         MVC   PRETCOD,=CL3'000'
         LA    R1,PARMS
         ST    R1,PARMLIST
         LA    R1,PARMLIST
*                                   -----------------------------------
         CALL  VSRCSBA              Call   CALC SBA --> ROW / COL VS
*                                   -----------------------------------
         L     R8,SAV8             RESTORE R8
         BR    R8                  RETURN TO CALLER
         EJECT
         DS    0F
SAV8     DS    2F
SBAWORK  DS    CL2
SBAROW   DS    CL1
SBACOL   DS    CL1
*
* --------------------------------------------------------------------*
*     PGMEND Program-ENd                                              *
* --------------------------------------------------------------------*
PGMEND   DS    0H
******   CNSLOUT MSG='PGMEND'
******   CNSLOUT ADR=PSCRL,LEN=2
         L     R9,VISADR           LOAD PARAMETER-ADDR
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
* --------------------------------------------------------------------*
*        WORKFIELDS                                                   *
* --------------------------------------------------------------------*
*        REGISTER EQUATES                                             *
* --------------------------------------------------------------------*
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
* --------------------------------------------------------------------*
         DS    0D
* --------------------------------------------------------------------*
*        SAVE AREA DECLARATIONS                                       *
* --------------------------------------------------------------------*
         DS    0F
SAVEAREA DC    XL72'00'                 MONITOR TASK SAVE AREA
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS
IOAREND  DC    F'0'                     END-ADDR OF IOAREA
INAREND  DC    F'0'                     END-ADDR OF INAREA
INLINEAD DC    F'0'                     END-ADDR OF INLINA
VISADR   DS    F
SAVINPAD DS    F                        SAVE INPUT LINE ADDRESS
SAVRECAD DS    F                        SAVE INPUT LINE ADDRESS
SAV1     DS    F
SAV13    DS    F
X00      DC    XL1'00'
WEDLIN   DS    CL6                 WORK BROWSE LINE AREA
SAVLINES DS    F                   SAVE LINES TO ADD, DUP, INS
EDLMDTAD DS    F                   SAVE EDITOR LINE MDT ADDR
STACKSW  DS    CL1                 STACK OPEN SW
ADDSW    DC    C'0'                ADD COMMAND SW
SWCMD    DS    C                   SW COMMAND
SWNUM    DS    C                   SW CMD NUMERIC OPERAND
TEMP     DS    CL80                WORK
*
DBL      DS    D                 RH
BLANKS   DC    255CL1' '
SCALE    DC    C'....+....1....+....2....+....3....+....4....+....5....*
               +....6....+....7... (X)'
*
HELPSW   DC    C'0'                HELP ACTIVE SW
MSGLEN   DS    F
FLAGS    DC    X'00'               BYTE OF FLAGS
PRINTABL DC    256AL1(*-PRINTABL)  00-FF
         ORG   PRINTABL
         DC    64X'E1'             00-3F
         ORG
*                0 1 2 3 4 5 6 7 8 9 A B C D E F
BINTOEBC DC    X'40C1C2C3C4C5C6C7C8C94A4B4C4D4E4F'  0
         DC    X'50D1D2D3D4D5D6D7D8D95A5B5C5D5E5F'  1
         DC    X'6061E2E3E4E5E6E7E8E96A6B6C6D6E6F'  2
         DC    X'F0F1F2F3F4F5F6F7F8F97A7B7C7D7E7F'  3
* --------------------------------------------------------------------*
*        PARAMETERS                                                   *
* --------------------------------------------------------------------*
PARMLIST DS    F
* --------------------------------------------------------------------*
         LTORG
* --------------------------------------------------------------------*
         DS    0D
* --------------------------------------------------------------------*
WORKA    DSECT
         COPY  LIBPARMS
TABRECS  DSECT
TABREC   DS    CL80
*---------------------------------------------------------------------
*        LOAD-AREA FOR OUTBOUND-MAPS
*---------------------------------------------------------------------
MAPDS    DSECT
MAPADDR  DS    0CL2008
MAPNAME  DS    CL8
MAPEYEC  DS    CL8
         DS    CL2
MAPLEN   DS    CL2                MAP-LEN
MAPVCNT  DS    AL1(0)             VAR-FIELD COUNT
MAPVLST  DS    AL3(0)             ADDR MAP-VAR-OUTPUT-LIST
MAPSTART DS    CL2000
         END
        BKEND
        CATALS   A.VSED,0.0
        BKEND   A.VSED
*------------------------------------------------------------------
*     NAME: VSED
*     TYPE: SUB-ROUTINE
* FUNCTION: EDIT A MEMBER
*
*
* INTERNAL  Error and Wait
*
* EXTERNAL  UTI Utility Program SBA --> row/col and vice versa
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:   Some ideas I copied from the VSEDIT-program
*            of Gustavo Torres. (clgtorres@gmail.com).
*            Thanks to him.
*
*
*
*------------------------------------------------------------------
* --------------------------------------------------------------------* 00000008
* EDIT MEMBER
* --------------------------------------------------------------------* 00000015
VSED     CSECT
         STM   R14,R12,12(R13)         STORE REGS IN SAVEAREA           00000018
         USING VSED,R5,R6              R5 R6
         LR    R5,R15                  ESTABLISH ADDRESSABILITY         00000021
         LA    R6,4095(R5)                                              00000022
         LA    R6,1(R6)                SECOND BASE ESTABLISHED          00000023
         B     GOON
EYECATCH DC    C'***** VSED *****'     EYECATCHER
GOON     DS    0H
         ST    R13,SAVEAREA+4          STORE MAIN PGM SAVEAREA          00000024
         LR    R10,R13                 SAVE R13                         00000025
         LA    R13,SAVEAREA            SAVE AREA FOR THIS TASK          00000026
         ST    R13,8(R10)              STORE EXIT SAVE ADDRESS          00000027
* --------------------------------------------------------------------* 00000028
*        STATEMENTS FOR SET UP FIELDS NEEDED FOR VSED                 * 00000029
* --------------------------------------------------------------------* 00000030
         L     R2,0(R1)                PARMS ADDRESS                    00000031
         ST    R2,VISADR               SAVE                             00000032
         LR    R7,R2
         USING WORKA,R7
         L     R9,PBUFADR              LOAD DATATAB ADDRESS             00000034
*****    ST    R9,TABRECSA             SET  TABRECS ADDRESS             00000035
*****    USING TABRECS,R9              ESTABLISH ADDRESSABILITY         00000036
         L     R10,PMAPADDR            LOAD OUTPUT MAP ADDRESS
         USING MAPDS,R10               ESTABLISH ADDRESS FOR MAPDS
******   CNSLOUT ADR=MAPNAME,LEN=8
         L     R8,MAPVCNT           LOAD ADDR-LIST-ADDR
         ICM   R8,8,X00             CLEAR CNT-BYTE
         L     R9,0(R8)             LOAD 1. VAR-OUT-ADDR SBAIC
         ST    R9,SBAICAD
         L     R9,4(R8)             LOAD 2. VAR-OUT-ADDR HDR1SZ
         ST    R9,HDR1SZAD
         L     R9,8(R8)             LOAD 3. VAR-OUT-ADDR LINE1
         ST    R9,LINE1AD
         L     R9,12(R8)            LOAD 4. VAR-OUT-ADDR LINE2
         ST    R9,LINE2AD
******   L     R9,TABRECSA
         DROP  R10
         B     CHKFUNC              CHECK, WHAT IS THE FUNCTION
SBAICAD  DS    F
HDR1SZAD DS    F
LINE1AD  DS    F
LINE2AD  DS    F
* --------------------------------------------------------------------* 00000065
*     PGSTART PROGRAM-START                                           * 00000066
* --------------------------------------------------------------------* 00000067
CHKFUNC  DS    0H
         MVC   PRETCOD,=CL3'000'
*****    BAL   R8,CALLERR
*****    CNSLOUT MSG='START'
*****    CNSLOUT ADR=PARMS
*****    CNSLOUT ADR=SAVENDBA
         CLC   PFUNC,=CL3'ED0'     EDIT MEMBER 1. TIME ?
         BE    ED0000              YES
         CLC   PFUNC,=CL3'ED7'     EDIT BACKWARD ?
         BE    ED7000              YES
         CLC   PFUNC,=CL3'ED8'     EDIT FORWARD  ?
         BE    ED8000              YES
         CLC   PFUNC,=CL3'ENT'     ENTER-TASTE   ?
         BE    ENT000              YES
         CLC   PFUNC,=CL3'EDF'     FORMAT IOAREA FROM INAREA
         BE    EDF000              YES
         CLC   PFUNC,=CL3'EFI'     FIND ARGUMENT
         BE    EFI000              YES
         CLC   PFUNC,=CL3'EDC'     CHANGE ARG1 ARG2
         BE    EDC000              YES
*****    CLC   PFUNC,=CL3'EDU'     UPDATE CHANGED DATA
*****    BE    EDU000              YES
*****    CLC   PFUNC,=CL3'EDL'     LINE COMMANDS
*****    BE    EDL000              YES
         MVC   PRETCOD,=CL3'882'   INVALID FUNCTION
*******  BAL   R8,CALLERR
*******  BAL   R8,CALLWAIT
         B     PGMEND
*----------------------------------------------------------------
*  GET OK, EDIT MEMBER in MAP
*----------------------------------------------------------------
ED0000   DS    0H                                                       00000761
         ST    R13,SAV13
         L     R1,PBUFADR          INPUT BUFFER ADDRESS FROM VSLIBRM
         L     R3,PNORL            NO OF INPUT RECORDS
         BCTR  R3,0                NUMBER - 1                           00000764
         MH    R3,RECLEN           MULTIPLY NUMBER WITH RECLEN
         AR    R3,R1               TO GET LAST INPUT RECORD ADDRESS
         ST    R3,SAVENDBA         SAVE END INPUT ADDR
EDDSPLY  DS    0H                                                       00000768
         ST    R1,SAVCURR          SAVE INPUT ADDR AS CURR. RECD-ADDR
         L     R2,LINE1AD          1.ROW OF 3270-OUTPUT MAP SPFM007
         LR    R4,R1
         L     R10,SAVENDBA
******   PDUMP (R4),(R10)
         L     R1,SAVCURR
         LA    R4,20               NUMBER OF OUTPUT-ROWS
         L     R10,HDR1SZAD        ADDR of HEADERSIZE OF MAP
*                                                                       00000774
EDNXTREC DS    0H                                                       00000775
         ST    R4,SAV4             RH
         C     R1,SAVENDBA         IS IT THE LAST INPUT RECORD ?
         BH    EDNXTEOF            YES                                  00000777
         LR    R12,R1              LOAD CURR RECORD ADDRESS
         AH    R12,VIEW            ADD SHIFT
         MVC   0(72,R2),BLANKS     CLEAR CURR OUTPUT MAP-ROW
         LH    R11,RECLEN          LOAD RECORD LENGTH (DEFAULT 80)
         SH    R11,VIEW            MINUS SHIFT
         CH    R11,=H'72'          LENGTH > 72
         BNH   EDNXTR01            NO,
         LH    R11,=H'72'          YES, LENGTH = 72
EDNXTR01 DS    0H
         BCTR  R11,0               MINUS 1 FOR MOVE
         STC   R11,EDNXTR02+1      SET MOVE LENGTH
EDNXTR02 MVC   0(72,R2),0(R12)     DSPLY RECORD                         00000788
         TR    0(72,R2),PRINTABL   TRANSLATE X'00-3F' TO X'E1'          00000789
         CLI   ADDSW,C'1'          ADD COMMAND INS CURSOR PENDING ?     00000790
         BNE   EDNXTR03            NO,                                  00000791
         CLC   0(72,R2),BLANKS     YES, IS BLANK LINE ?                 00000792
         BNE   EDNXTR03            NO                                   00000793
*                                                                       00000794
         MVI   ADDSW,C'0'          SETOFF SW                            00000795
         LA    R12,24              MAX ROWS
         SR    R12,R4              MINUS LEFTOVER ROWS OF MAP
         MH    R12,=H'80'          MULTIPLY WITH COLUMS
         LA    R13,1               COLUMN 1
         AR    R13,R12
         SR    R12,R12
         D     R12,=F'64'
         L     R11,SBAICAD         LOAD
         STC   R13,1(0,R11)
         STC   R12,2(0,R11)
         TR    1(2,R11),BINTOEBC   TR BUFADR1 BUFADR2
*                                                                       00000806
EDNXTR03 DS    0H                  NEXT INPUT RECORD
         AH    R1,RECLEN
         LA    R2,88(R2)           NEXT OUTPUT DSPLY LINE
         LA    R10,88(R10)         NEXT SBAID
         BCT   R4,EDNXTREC                                              00000810
*
         ST    R1,SAVNEXTB         SAVE NEXT REC ADDRESS                00000811
         ST    R10,MSGLEN          SAVE MSGLEN
EDNXTRD1 DS    0H                   REDISPLAY
         L     R13,SAV13
         B     PGMEND                                                   00000813
*
EDNXTEOF DS    0H                   SET EOF-MSG                         00000814
         MVC   0(72,R2),BLANKS      CLEAR
         MVC   0(24,R2),EOFMSG
         BCTR  R4,0
EDEOFCLR DS    0H
         LA    R2,88(R2)            NEXT ROW
         MVC   0(72,R2),BLANKS      CLEAR
         BCT   R4,EDEOFCLR          REST
*
******   MVC   PRETCOD,=CL3'993'    EOF OF MEMBER REACHED               00000815
         LA    R10,88(R10)          NEXT HDR1SZ
         ST    R10,MSGLEN           MSGLEN
*
         L     R13,SAV13
         B     PGMEND                                                   00000818
SAV4     DS    F
EOFMSG   DS    0CL26
         DC    XL2'1DE8'
         DC    C'***** END OF FILE ***** '                              00001598
******   CNSLCCB
* --------------------------------------------------------------------* 00000065
*   ED7 Backward  PF7                                                 * 00000066
* --------------------------------------------------------------------* 00000067
ED7000   DS    0H
         L     R4,SAV4             RH
         LH    R2,RECLEN                                                00000390
         CLC   PSCRL,=CL2'MX'      MAX-NUMBER OF LINES UP
         BE    ED7030              YES, GO TO TOP OF LIST
         CLC   PSCRL,=H'0'         NUMBER OF LINES IN PSCRL
         BE    ED7010              NO VALUE
         MH    R2,PSCRL            PSCRL-VALUE
         B     ED7020
ED7010   DS    0H
         MH    R2,=H'20'           20 ROWS                              00000391
ED7020   DS    0H
         CLC   SAVCURR,SAVENDBA    WE POINT TO LAST RECORD ?            00000392
         BNE   BACK                NO,                                  00000393
         SH    R2,RECLEN           YES, BACK 19 LINES ONLY              00000394
BACK     DS    0H                                                       00000395
         L     R1,SAVCURR          LOAD CURRENT REC ADDRESS             00000396
         LTR   R1,R1               SOME ADDRESS ?                       00000397
         BZ    PGMEND              NO                                   00000398
         SR    R1,R2               BACK NN LINES                        00000399
         C     R1,PBUFADR          POINTER OK ?                         00000400
         BNL   EDDSPLY             YES                                  00000401
ED7030   DS    0H                  START FROM THE BEGINNING
         L     R1,PBUFADR          LOAD BEGIN BUFFER ADDRESS            00000402
*******  MVC   PRETCOD,=CL3'992'   TOP OF MEMBER REACHED                 0000040
         B     EDDSPLY             FIRST RECORD                         00000405
* --------------------------------------------------------------------* 00000065
*   ED8 Forward  PF8                                                  * 00000066
* --------------------------------------------------------------------* 00000067
ED8000   DS    0H
         CLC   PSCRL,=CL2'MX'
         BNE   ED8010
         L     R1,SAVENDBA         LAST RECORD
         SH    R1,RECLEN           -1 TO DISPLAY AT LEAST 1 RECORD
         B     ED8END
ED8010   DS    0H
         CLC   PSCRL,=H'0'         NUMBER OF LINES IN PSCRL
         BE    ED8AMT               NO
         LH    R4,PSCRL             AMOUNT LINES FORWARD
         MH    R4,RECLEN            MULTIPLY WITH RECORDLEN
         L     R1,SAVCURR           CURRENT RECORD ADDRESS
         AR    R1,R4                ADD DISPLACEMENT
         B     ED8END
ED8AMT   DS    0H
         L     R1,SAVNEXTB         LOAD NEXT REC ADDRESS
ED8END   DS    0H
         L     R4,SAV4             RH
         LTR   R1,R1               SOME ADDRESS ?
         BZ    PGMEND              NO
         B     EDDSPLY             FIRST RECORD
* ---------------------------------------------------------------------
*   ENT ENTER-Taste
* ---------------------------------------------------------------------
ENT000   DS    0H
******   CNSLOUT MSG='ENT000'
******   CNSLOUT ADR=PSCRL,LEN=2
         CLC   PSCRL,=CL2'TP'         MAX TOP
         BNE   ENT010
         MVC   PSCRL,=CL2'MX'
         B     ED7000
ENT010   DS    0H
         CLC   PSCRL,=CL2'BT'         MAX BOTTOM
         BNE   ENT020
         MVC   PSCRL,=CL2'MX'
         B     ED8000
ENT020   DS    0H
         CLC   PSCRL,=CL2'NF'         STRING NOT FOUND
         BNE   ENT030
         XC    PSCRL,PSCRL         REINIT PSCRL
         L     R1,PBUFADR          LOAD CURRENT REC ADDRESS
         B     EDDSPLY             NEXT RECORD
ENT030   DS    0H
         L     R1,SAVCURR          LOAD CURRENT REC ADDRESS
         LTR   R1,R1               SOME ADDRESS ?
         BZ    ENT040 RH PGMEND    NO, RETURN
         B     EDDSPLY             NEXT RECORD
ENT040   DS    0H
         L     R1,PBUFADR          LOAD CURRENT REC ADDRESS
         B     EDDSPLY             NEXT RECORD
*----------------------------------------------------------------------
*   EDF FORMAT IOAREA FROM INAREA
*----------------------------------------------------------------------
EDF000   DS    0H                  FORMAT INAREA FROM IOAREA             0000120
******   CNSLOUT MSG='EDF000'
*                                  BLANK OUT INAREA FOR OUTPUT
*                                  CALC SBA TO RC and MOVE DATA TO
*                                  INAREA PREDEDED BY MDT
*                                  ------------------------------------
*                                  BLANK OUT INAREA (23 LINES)          00001202
         LA    R1,23               INIT CNT 23                          00001204
         L     R2,PINAREAD         START AT FIRST LINE OF AREA           0000120
EDFCLR   DS    0H                                                       00001206
         MVC   0(80,R2),BLANKS     CLEAR CURRENT WITH BLANKS            00001206
         LA    R2,80(R2)           INCREMENT FOR 1 LINE                  0000120
         BCT   R1,EDFCLR           LOOP 23 TIMES                        00001208
*----------------------------------                                     00001209
         L     R1,PIOAREAD                                               0000121
         LA    R1,1840(R1)          END OF IOAREA
         ST    R1,IOAREND
*----------------------------------
         L     R1,PIOAREAD         START AT BEGIN OF IOAREA (INPUT)     00001210
         LA    R1,3(R1)            SKIP AID+CURPOS POINT TO SBA ORDER    0000121
*                                  ---------------------------------    00001202
EDFLOOP  DS    0H                                                       00001212
         CLI   0(R1),X'51'         SBA ORDER ?                          00001212
         BE    EDFLOOP1            NO                                   00001213
         CLI   0(R1),X'11'         SBA ORDER ?                          00001212
         BNE   EDFINDEX            NO, INCREASE INPUT AND LOOP          00001213
EDFLOOP1 DS    0H                  RH 51
         CLC   1(2,R1),=XL2'C15D'  IGNORE CMD-LINE R2,C14 INPUT HERE
         BE    EDFINDEX            INCREASE INPUT AND LOOP
*                                   ----------------------------------- 00000720
         ST    R1,SAVEDFR1         SAVE R1
         MVC   PFUNC,=CL3'SRC'     FUNCTION SBA TO ROW/COL
         MVC   PSBAWORK,1(R1)      GET THE  SET BUFFER ADDR = 2 BYTE
         BAL   R8,CALLUTI
******   CNSLOUT ADR=PSBAWORK,LEN=4
*                                   ----------------------------------- 00000720
         L     R1,SAVEDFR1         RELOAD R1
         LA    R1,3(R1)            SKIP SBA                             00001237
         LR    R2,R1               R2 POINT TO BEGIN OF FIELD           00001238
*                                   ----------------------------------- 00000720
         XR    R10,R10             CLEAR R10
         IC    R10,PROW            INSERT ROW-NUMBER
         SH    R10,=H'3'           Subtract 3 ROWS / SEE SPMF007
         MH    R10,=H'80'          ROW * 80 = OFFSET
         L     R12,PINAREAD   RH   POINT TO DATA FIELD +INCMDA
         AR    R12,R10             PLUS OFFSET
         CLI   PCOL,X'07'          CMD-LINE ?
         BH    EDFDATAL            NO, DATALINE
         OI    0(R12),X'01'        SET MDT-BIT ON
         MVC   1(6,R12),0(R2)      MOVE CMD-DATA TO INAREA
         OC    1(6,R12),BLANKS     UPPERCASE LETTERS
         B     EDFINDEX
EDFDATAL DS    0H                  DATALINE TO MOVE                     00001248
         LA    R12,7(R12)          OFFSET FOR DATALINE                  00001249
         OI    0(R12),X'01'        SET MDT-BIT ON
         MVC   1(71,R12),0(R2)     MOVE DATALINE TO INAREA
         OC    1(71,R12),BLANKS    MAKE UPPERCASE
EDFINDEX DS    0H                                                       00001214
         LA    R1,1(R1)                                                 00001214
         C     R1,IOAREND          END OF IOAREA ?                      00001215
         BH    EDFEND              YES, EXTRACT LAST FIELD              00001216
         B     EDFLOOP                                                  00001217
EDFEND   DS    0H                                                       00001218
         ST    R1,SAV1
         L     R10,PIOAREAD
         L     R11,PINAREAD
         LA    R11,1840(R11)
******   PDUMP (R10),(R11)
         L     R1,SAV1
******   B     PGMEND    RH 27.3.11
******   B     EDU000
******   B     EDL000
*----------------------------------------------------------------------
*   EDU UPDATE CHANGED DATA  in DATA-LINES R1=IN R2=OUT
*----------------------------------------------------------------------
EDU000   DS    0H
******   CNSLOUT MSG='EDU000'
         L     R4,PINAREAD
         LR    R2,R4
         LA    R2,400(R4)
******   PDUMP (R4),(R2)
         SR    R4,R4               CLEAR R4                             00000445
         L     R1,PINAREAD         INDATA BUFFER                        00000446
         LA    R1,7(R1)            PROCESS DATA LINE MDT
         L     R2,SAVCURR          CURRENT REC ADDRESS                  00000447
*******  SH    R2,=H'240'   RH
         LTR   R2,R2               SOME DATA HERE ?                     00000448
*****    BZ    PGMEND              NO, RETURN                           00000449
         BZ    EDL000              NO, RETURN                           00000449
**
         LA    R3,20               NUMBER OF ROWS (USED TO BE 22)       00000450
CHKMDT   TM    0(R1),X'01'         MDT ON ?                             00000451
         BNO   NEXTUPD             NO                                   00000452
         LA    R4,1(R4)            CHANGES COUNT +1                     00000453
         MVI   PRINTABL,X'40'      TRANSLATE X'00' TO X'40'             00000454
         TR    1(72,R1),PRINTABL   TRANSLATE X'01-3F' TO X'E1'          00000455
         MVI   PRINTABL,X'E1'                                           00000456
         LR    R10,R2              RECORD ADDRESS OUTPUT
         AH    R10,VIEW            ADD SHIFT
         LA    R12,80
         SH    R12,VIEW
         BCTR  R12,0
         STC   R12,*+5             SET MOVE LENGTH
         MVC   0(00,R10),1(R1)      UPDATE MEMBER
NEXTUPD  DS    0H                                                       00000464
         LA    R1,80(R1)           NEXT INPUT LINE                      00000465
         AH    R2,RECLEN           NEXT MEMBER LINE                     00000466
         C     R2,SAVENDBA         END OF MEMBER BUFFER ?               00000467
*****    BH    PGMEND      RH  27.3.11                                  00000468
         BH    EDL000      RH      YES, RETURN                          00000468
         BCT   R3,CHKMDT                                                00000469
******   B     PGMEND      RH 27   RETURN
*----------------------------------------------------------------------
*   EDL PROCESS THE LINE COMMANDS
*---------------------------------------------------------------------- 00000471
EDL000   DS    0H                  EDITOR LINE COMMANDS (PASS 1)        00000472
*---------------------------------------------------------------------- 00000471
******   CNSLOUT MSG='EDL000'
         L     R3,PINAREAD         INDATA BUFFER                        00000446
         LR    R2,R3
         LA    R2,1840(R2)
******   PDUMP (R3),(R2)
*
         L     R1,PINAREAD         INDATA BUFFER                        00000446
         LA    R1,7(R1)            SAVE DATA LINE MDT
         ST    R1,INLINEAD
*
         L     R1,PINAREAD         INDATA BUFFER                        00000446
         LA    R1,1840(R1)         PROCESS END OF INAREA-ADDR
         ST    R1,INAREND
*
*********PDUMP INLINEAD,INAREND
*
         L     R1,PINAREAD         INDATA BUFFER                        00000446
         L     R2,SAVCURR          CURRENT REC ADDRESS                  00000474
******   SH    R2,=H'240'          RH
         LTR   R2,R2               SOME DATA HERE ?                     00000475
         BZ    EDLENDE             NO, RETURN                           00000476
         OC    1(6,R1),BLANKS      FORCE UPPER CASE LINECMD COLS        00000477
         XC    SAVINPAD,SAVINPAD   CLEAR LAST INPUT LINE ADR PENDING
         MVI   STACKSW,C'0'        SETOF STACK SW                       00000479
*----------------------------------
EDLMDT   DS    0H                                                       00000480
*******  CNSLOUT MSG='EDLMDT'
         TM    0(R1),X'01'         MDT ON ?                             00000480
         BNO   NEXTEDL             NO                                   00000481
         OC    1(6,R1),BLANKS      FORCE EDITOR LINE AREA TO UPPER CASE 00000482
         LA    R4,1(R4)            CHANGES COUNT +1                     00000483
         BAL   R12,EDLPROC         REARRANGE EDITOR LINE CMD AREA       00000484
*                                  R3 = LINES TO COPY, MOVE, ETC        00000485
CHKSLASH DS    0H                                                       00000486
         ST    R3,SAVLINES         SAVE LINES TO ADD                    00000487
*----------------------------------
         CLI   1(R1),C'/'          SET LINE POINTER CMD ?               00000488
*----------------------------------
         BNE   CHKCOPY             NO                                   00000489
         ST    R2,SAVCURR          YES, SET CURRENT REC ADDRESS         00000490
         NI    0(R1),X'FE'         SETOFF MDT                           00000491
         B     NEXTEDL                                                  00000492
*----------------------------------
CHKCOPY  DS    0H                                                       00000493
         CLI   1(R1),C'C'          COPY (C) LINE CMD ?                  00000493
*----------------------------------
         BNE   CHKKOPY             NO                                   00000494
         L     R10,PNXTSTCK        YES, ADDR OF NEXT STACK ENTRY        00000495
         CLI   STACKSW,C'1'        STACK SW ON ?                        00000496
         BE    OFFMDT              YES, NEXT STACK ENTRY                00000497
         MVI   STACKSW,C'1'        NO, SETON STACK SW                   00000498
         L     R10,PSTACK          POINT TO BEGIN OF STACK AREA         00000499
OFFMDT   NI    0(R1),X'FE'         SETOFF MDT                           00000500
MOVSTK   LR    R4,R2               R4 = ADDR OF RECORD TO STACK         00000501
MOVSTKN  C     R10,PWORK1          END OF STACK AREA ?                  00000502
         BL    MOVSTKOK                   NO,                           00000503
         MVC   PRETCOD,=CL3'898'    YES, STACK EDITOR IS FULL
*******  BAL   R8,CALLERR
*******  BAL   R8,CALLWAIT
         B     EDLENDE             EXIT                                 00000506
MOVSTKOK DS    0H                                                       00000507
         LR    R12,R4                                                   00000508
         CLC   RECLEN,=H'80'       80 BYTES RECORDS ?                   00000509
         BE    MOVST010            YES,                                 00000510
         AH    R12,VIEW            NO, USE VIEW TO OFFSET INPUT         00000511
MOVST010 DS    0H
         MVC   0(80,R10),0(R12)    MOVE RECORD TO STACK                 00000512
         LA    R10,80(R10)         POINT TO NEXT STACK ENTRY            00000513
         ST    R10,PNXTSTCK        SAVE IT                              00000514
         AH    R4,RECLEN           NEXT LINE TO STACK                   00000515
         C     R4,SAVENDBA         END OF BUFFER ?                      00000516
         BH    NEXTEDL             YES                                  00000517
         BCT   R3,MOVSTKN          NO, LOOP                             00000518
         B     NEXTEDL                                                  00000519
*----------------------------------
CHKKOPY  DS    0H                                                       00000520
         CLI   1(R1),C'K'          COPY (K) LINE CMD ?                  00000520
*----------------------------------
         BNE   CHKMOVE             NO                                   00000521
         L     R10,PNXTSTCK        YES, ADDR OF NEXT STACK ENTRY        00000522
         B     OFFMDT                                                   00000523
*----------------------------------
CHKMOVE  DS    0H                                                       00000524
         CLI   1(R1),C'M'          MOVE (M) LINE CMD ?                  00000524
*----------------------------------
         BNE   SAVEDL              NO, SAVE ADDR OF LINE                00000525
         L     R10,PNXTSTCK        YES, ADDR OF NEXT STACK ENTRY        00000526
         CLI   STACKSW,C'1'        STACK SW ON ?                        00000527
         BE    CHGCMD              YES, NEXT STACK ENTRY                00000528
         MVI   STACKSW,C'1'        NO, SETON STACK SW                   00000529
         L     R10,PSTACK           POINT TO BEGIN OF STACK AREA         0000053
*----------------------------------
CHGCMD   DS    0H                                                       00000531
         MVI   1(R1),C'D'          CHANGE TO (D) LINE CMD               00000531
*----------------------------------
         ST    R1,SAVINPAD         SAVE INPUT LINE ADR (PENDING CMD)    00000532
         ST    R2,SAVRECAD         SAVE BUFFER LINE ADR                 00000533
         B     MOVSTK                                                   00000534
SAVEDL   ST    R1,SAVINPAD         SAVE INPUT LINE ADR (PENDING CMD)    00000535
         ST    R2,SAVRECAD         SAVE BUFFER LINE ADR                 00000536
NEXTEDL  DS    0H                                                       00000537
         LA    R1,80(R1)           NEXT INPUT LINE                      00000538
         AH    R2,RECLEN           NEXT MEMBER LINE                     00000539
         C     R2,SAVENDBA         END OF MEMBER BUFFER ?               00000540
         BH    EDITORL2            YES, GO PASS 2                       00000541
         C     R1,INAREND          NO,  END OF INAREA ?                 00000542
         BNH   EDLMDT                   NO                              00000543
* --------------------------------------------------------------------- 00000544
EDITORL2 DS    0H                  EDITOR LINE COMMANDS (PASS 2)        00000545
*******  CNSLOUT MSG='EDITORL2'
         CLC   RECLEN,=H'80'       80 BYTES RECORDS ?                   00000546
         BNE   EDLENDE             NO, RETURN                           00000547
******   CNSLOUT ADR=SAVINPAD,LEN=4
         L     R1,SAVINPAD         LAST INPUT  LINE ADR PENDING         00000548
         LTR   R1,R1               PENDING CMD ?                        00000549
         BZ    EDLENDE             NO, RETURN                           00000550
         L     R2,SAVRECAD         LAST BUFFER LINE ADR PENDING         00000551
PREVMDT  TM    0(R1),X'01'         MDT ON ?                             00000552
         BNO   PREVEDL             NO                                   00000553
         PACK  DBL,2(5,R1)         CONVERT (nnn)                        00000554
         CVB   R3,DBL                           TO BINARY               00000555
*----------------------------------
CHKADD   DS    0H                                                       00000556
         CLI   1(R1),C'I'          ADD (A) LINE CMD ?   RH WAS A        00000556
*----------------------------------
         BNE   CHKDUP              NO                                   00000557
         MVC   TEMP,BLANKS         YES, ADD BLANK LINE                  00000558
         MVI   ADDSW,C'1'          SETON ADDSW                          00000559
ADDCMD   BAL   R12,OFFSET          OFFSET NN LINES                      00000560
         LA    R4,80(R2)           R4 = LOCATION TO NEW LINES           00000561
ADDLINE  MVC   0(80,R4),TEMP       ADD BLANK OR DUP LINE                00000562
         LA    R4,80(R4)           NEXT LINE                            00000563
         L     R10,PNORL           NO.LINES IN BUFFER                   00000564
         LA    R10,1(R10)          INDEX                                00000565
         ST    R10,PNORL           SAVE IT                              00000566
         L     R10,SAVENDBA                                             00000567
         LA    R10,80(R10)         INDEX END BUFFER ADDR                00000568
         ST    R10,SAVENDBA        SAVE IT                              00000569
         BCT   R3,ADDLINE          LOOP                                 00000570
         B     PREVEDL                                                  00000571
*----------------------------------
CHKDUP   DS    0H                                                       00000572
         CLI   1(R1),C'R'          DUP (") LINE CMD ? RH WAS "          00000572
*----------------------------------
         BNE   CHKINS              NO                                   00000573
         MVC   TEMP,0(R2)          YES, ADD DUPLICATE                   00000574
         B     ADDCMD              GO ADD                               00000575
*----------------------------------
CHKINS   DS    0H                                                       00000576
         CLI   1(R1),C'A'          INS (I) LINE CMD ?   RH WAS I        00000576
*----------------------------------
         BNE   CHKDEL              NO                                   00000577
         CLC   PSTACK,PNXTSTCK     YES, SOMETHING IN STACK ?
         BE    PREVEDL                  NO                              00000579
         ST    R11,SAVXX           RH
         L     R11,PNXTSTCK        NEXT STACK ENTRY                     00000580
         S     R11,PSTACK          - STACK BEGIN                        00000581
         SR    R10,R10                                                  00000582
         D     R10,=F'80'          R11 = NO. LINES IN STACK             00000583
         LR    R3,R11              R10 => R3                            00000584
         L     R11,SAVXX           RH
         BAL   R12,OFFSET          OFFSET NN LINES                      00000585
         L     R12,PSTACK          FIRST STACK ENTRY                    00000586
         LA    R4,80(R2)           R4 = LOCATION TO NEW LINES           00000587
ADDSTK   MVC   0(80,R4),0(R12)     ADD LINE FROM STACK                  00000588
         LA    R12,80(R12)         NEXT STACK ENTRY                     00000589
         LA    R4,80(R4)           NEXT BUFFER RECORD                   00000590
         L     R10,PNORL           NO.LINES IN BUFFER                   00000591
         LA    R10,1(R10)          INDEX                                00000592
         ST    R10,PNORL           SAVE IT                              00000593
         L     R10,SAVENDBA                                             00000594
         LA    R10,80(R10)         INDEX END BUFFER ADDR                00000595
         ST    R10,SAVENDBA        SAVE IT                              00000596
         BCT   R3,ADDSTK           LOOP                                 00000597
         B     PREVEDL             YES                                  00000598
SAVXX    DS    4F
*----------------------------------
CHKDEL   DS    0H                                                       00000599
         CLI   1(R1),C'D'          DEL (D) LINE CMD ?                   00000599
*----------------------------------
         BNE   PREVEDL             NO                                   00000600
         C     R2,PBUFADR          IT IS FIRST LINE ?                   00000601
         BNE   DELOK               NO                                   00000602
         C     R3,PNORL            YES, DELETE OF ENTIRE AREA ?         00000603
         BL    DELOK                    NO, GO DEL                      00000604
         MVC   PRETCOD,=CL3'897'   DELETE OF ENTIRE MEMBER INVALID      00000605
******   BAL   R8,CALLERR
******   BAL   R8,CALLWAIT
         B     EDLENDE                                                  00000606
DELOK    ST    R3,SAVLINES         SAVE LINES TO DEL                    00000607
         STM   R10,R13,SAVXX       RH
         L     R11,SAVENDBA        END BUFFER ADDR                      00000608
         SR    R11,R2              - CURRENT LINE ADDR                  00000609
         LA    R11,80(R11)         + LAST RECORD                        00000610
         SR    R10,R10
         D     R10,=F'80'          R11 = NO. REMAINDER LINES
         CR    R11,R3              R11 > LINES TO DEL ?                 00000613
         BH    MVCLD               YES, DO MVCL                         00000614
         ST    R11,SAVLINES        NO, DECREMENT REMAINDER LINES        00000615
         B     UPDCNT                                                   00000616
MVCLD    LR    R10,R2              MVCL TO ADDR (FIRST LINE TO DEL)     00000617
         MH    R3,=H'80'           BYTES TO DEL (LINES * 80)            00000618
         LA    R12,0(R3,R10)       MVCL FROM ADDR (NN LINES DOWN)       00000619
         L     R11,SAVENDBA        END BUFFER ADDR                      00000620
         SR    R11,R12             - MVCL FROM ADDR                     00000621
         LA    R11,80(R11)         + LAST RECORD                        00000622
         LR    R13,R11              L2 = L1
         MVCL  R10,R12             MOVE IT
UPDCNT   DS    0H                                                       00000625
         LM    R10,R13,SAVXX       RH
         L     R10,PNORL           NO.LINES IN BUFFER                   00000626
         S     R10,SAVLINES        - NN LINES                           00000627
         ST    R10,PNORL           SAVE IT                              00000628
         L     R3,SAVLINES         LINES TO DEL                         00000629
         MH    R3,=H'80'           BYTES TO DEL (LINES * 80)            00000630
         L     R10,SAVENDBA        END BUFFER ADDR                      00000631
         SR    R10,R3              - (LINES * 80)                       00000632
         ST    R10,SAVENDBA        SAVE IT                              00000633
         LA    R4,1(R4)            SETON UPDATE SW                      00000634
         B     PREVEDL                                                  00000635
*                                                                       00000636
OFFSET   DS    0H                  OFFSET NN LINES                      00000637
         ST    R3,SAVLINES         SAVE LINES TO ADD                    00000638
         L     R10,SAVENDBA        END BUFFER ADDR                      00000639
         MH    R3,=H'80'           COMPUTE OFFSET                       00000640
         LA    R4,0(R3,R10)        R4 = NEW LOCATION ADR                00000641
         LA    R4,80(R4)                                                00000642
         C     R4,PENDBUF          REACH END OF BUFFER ?                00000643
         BNH   OFFSETL             NO                                   00000644
         MVC   PRETCOD,=CL3'896'   OVERFLOW WORK AREA                   00000645
******   BAL   R8,CALLERR
******   BAL   R8,CALLWAIT
         B     EDLENDE                                                  00000606
OFFSETL  DS    0H                                                       00000648
         C     R2,SAVENDBA         LAST MEMBER LINE ?                   00000649
         BE    OFFSEXIT            SI, RETURN                           00000650
OFFSETA  DS    0H                                                       00000651
         LA    R4,0(R3,R10)        R4 = NEW LOCATION ADR                00000652
         MVC   0(80,R4),0(R10)     OFFSET NN LINES                      00000653
         SH    R10,=H'80'          PREV LINE                            00000654
         CR    R10,R2              DONE ?                               00000655
         BNE   OFFSETA             NO, LOOP                             00000656
OFFSEXIT L     R3,SAVLINES         RESTORE LINES TO ADD                 00000657
         BR    R12                 RETURN                               00000658
*                                                                       00000659
PREVEDL  DS    0H                                                       00000660
         SH    R1,=H'80'           PREV INPUT LINE                      00000661
         SH    R2,=H'80'           PREV MEMBER LINE                     00000662
         C     R1,INLINEAD         BEG OF INDATA BUFFER ?               00000663
         BNL   PREVMDT             NO,                                  00000664
         B     EDLENDE             RETURN                               00000665
*---------------------------------------------------------------------- 00000666
EDLPROC  DS    0H                  REARRANGE EDITOR LINE CMD AREA       00000667
*---------------------------------------------------------------------- 00000666
*******  CNSLOUT MSG='EDLPROC'
         MVC   WEDLIN,BLANKS                                            00000668
         MVI   SWCMD,C'0'          SETOF SW COMMAND                     00000669
         MVI   SWNUM,C'0'          SETOF SW NUMERIC                     00000670
         ST    R1,EDLMDTAD         SAVE EDITOR LINE MDT ADDR            00000671
         LA    R4,6(R1)            END EDITOR LINE POSITION             00000672
         LA    R10,WEDLIN+5        END WORK AREA                        00000673
EDLOOP1  CLI   0(R4),C'*'          IS LAST POSITION = *                 00000674
         BE    EDLINDX             YES, IT WAS NOT CHANGED
         CLI   0(R4),C'='          NO, CHANGED, IS IT = ?
         BE    EDLINDX             YES, IT WAS NOT CHANGED
         CLI   0(R4),C' '          NO, CHANGED, IS IT ' '
         BE    EDLINDX             YES                                  00000679
         TM    0(R4),X'F0'         NO, IS IT NUMERIC ?
         BO    EDLNUM              YES                                  00000681
*--------------------------   START CHECK CMD BY CMD_TABLE                    00
         LA    R3,TABCMD           NO, LOAD ADDR VALID COMMANDS TABLE
*                                  TABCMD =  'ACDIMK/R'
EDLOOP2  DS    0H                                                       00000683
         CLC   0(1,R4),0(R3)       IS IT A VALID CMD ?
         BE    EDLOK               YES
         LA    R3,1(R3)            NO, TRY NXT TAB ENTRY
         CLI   0(R3),X'FF'             IS IT END OF TABLE ?
         BNE   EDLOOP2             NO, LOOP
         MVC   PRETCOD,=CL3'900'      YES, CMD NOT FOUND in CMD-TABLE
******   BAL   R8,CALLERR          WRITE ERROR
******   BAL   R8,CALLWAIT RH 24.03.11
         B     EDLENDE
*--------------------------   END CHECK CMD BY CMD_TABLE                    0000
EDLOK    DS    0H                  CMD FOUND IN TABLE
         MVC   WEDLIN(1),0(R4)     MOVE CMD TO FIRST POSITION OF LINE
         CLI   SWCMD,C'1'          SW ON ?                              00000690
         BNE   EDLOK1
         MVC   PRETCOD,=CL3'903'      YES, COMMAND SWITCH ALREADY ON     0000095
*******  BAL   R8,CALLERR          WRITE ERROR
*******  BAL   R8,CALLWAIT
         B     EDLENDE
*--------------------------   ERROR COMMAND SWITCH ALREADY ON               0000
EDLOK1   DS    0H
         MVI   SWCMD,C'1'          NO, SETON SW                         00000692
EDLINDX  BCTR  R4,0                -1                                   00000693
         C     R4,EDLMDTAD         REACH THE MDT ?                      00000694
         BNE   EDLOOP1             NO, LOOP                             00000695
EDLEND   DS    0H                                                       00000696
         CLC   WEDLIN+1(5),BLANKS  NUMERIC ARE BLANKS ?                 00000696
         BNE   EDLEND01            NO, CHECK                            00000697
         MVI   WEDLIN+5,C'1'       YES, FORCE 1                         00000698
EDLEND01 DS    0H
         OC    WEDLIN+1(5),=5C'0'  FORCE ZONES TO F                     00000699
         CLC   WEDLIN+1(5),=5C'0'  ALL ZEROS ?                          00000700
         BNE   EDLEND02            YES, ERROR                           00000701
         MVC   PRETCOD,=CL3'904'      YES, ALL CMD LINE # ARE ZERO       0000095
******   BAL   R8,CALLERR          WRITE ERROR
******   BAL   R8,CALLWAIT
         B     EDLENDE
*--------------------------   ERROR ALL CMD LINE# ARE ZERO                  0000
EDLEND02 DS    0H
         MVC   1(6,R1),WEDLIN      NO, MOVE WEDLIN TO INAREA BUFFER
         PACK  DBL,WEDLIN+1(5)     PACK FIRST, THEN                      0000070
         CVB   R3,DBL              CONVERT TO BINARY
         CH    R3,=H'999'          nnn TIMES > 999 ?                    00000705
         BNHR  R12                 NO, RETURN                           00000706
*---------------------------------
*   END OF EDL-PROC
*---------------------------------
EDLNUM   DS    0H                                                       00000709
         CLI   SWNUM,C'1'          SW ON ?                              00000709
         BNE   EDLNUM1             NO                                   00000710
         MVC   PRETCOD,=CL3'895'   YES, ERROR TYPE III AREA
*******  BAL   R8,CALLERR          WRITE ERROR
*******  BAL   R8,CALLWAIT
         B     EDLENDE
*-----------------------------------    ERROR TYPE III AREA
EDLNUM1  DS    0H                                                       00000709
         MVI   SWNUM,C'1'          NO, SETON SW                         00000711
EDLOOP3  DS    0H                                                       00000712
         MVC   0(1,R10),0(R4)      MOVE NUM TO WEDLIN                   00000712
         BCTR  R4,0                - 1                                  00000713
         BCTR  R10,0               - 1                                  00000714
         C     R4,EDLMDTAD         REACH THE MDT ?                      00000715
         BE    EDLEND              YES,                                 00000716
         TM    0(R4),X'F0'         NO, NEXT NUMERIC ?                   00000717
         BO    EDLOOP3                 YES, GO MOVE IT                  00000718
         B     EDLOOP1                 NO, LOOP1                        00000719
EDLENDE  DS    0H
         B     PGMEND              RETURN
SAVEDFR1 DS    F
*---------------------------------------------------------------------* 00000065
*   EFI Find Argument (PFARG)                                         * 00000066
*---------------------------------------------------------------------* 00000067
EFI000   DS    0H
*******  CNSLOUT MSG='EFI000'
         LH    R2,PFLEN            R2 = LENGTH OF STRING                00000934
         BCTR  R2,0                -1                                   00000935
         STC   R2,EFICLC+1         CHANGE CLC LENGTH                    00000936
         LH    R4,RECLEN           LAST COLUMN                          00000937
         SR    R4,R2               R4 = NN COLUMNS TO SCAN              00000938
         ST    R4,SAVER4           SAVE R4                              00000939
         L     R2,SAVCURR          LOAD CURRENT REC ADDRESS             00000940
         AH    R2,RECLEN           BEGIN WITH LINE 2                    00000941
EFINEXTR DS    0H                                                       00000942
         LR    R3,R2                                                    00000942
         L     R4,SAVER4           RESTORE R4                           00000943
EFICLC   CLC   0(00,R3),PFARG      SCAN HIT ?                           00000944
         BE    EFIHIT              YES                                  00000945
         LA    R3,1(R3)            NEXT COLUMN                          00000946
         BCT   R4,EFICLC           LOOP                                 00000947
         AH    R2,RECLEN           NEXT RECORD                          00000948
         C     R2,SAVENDBA         END OF MEMBER ?                      00000949
         BNH   EFINEXTR               NO,  SCAN NEXT RECORD                 0000
         MVC   PRETCOD,=CL3'982'      YES, STRING NOT FOUND              0000095
         BAL   R8,CALLERR          WRITE ERROR
         MVC   PSCRL,=CL2'NF'      MARK NOT FOUND
EFIHIT   DS    0H                                                       00000953
         ST    R2,SAVCURR          SET CURRENT REC ADDRESS              00000953
******   CNSLOUT MSG='EFIHIT'
         L     R1,SAVCURR          LOAD CURRENT REC ADDRESS             00000263
         LTR   R1,R1               SOME ADDRESS ?                       00000264
         BZ    EFIEND              NO                                   00000265
         B     EDDSPLY             DISPLAY                              00000269
EFIEND   DS    0H
         B     PGMEND              PROGRAM-END
SAVER4   DS    F
*                                                                       00001236
*-------------------------------------------------------------------
* Change Replace ARGUMENT-1 by ARGUMENT-2
*-------------------------------------------------------------------
EDC000   DS    0H                  CHANGE COMMAND                       00000957
*-------------------------------------------------------------------
******   CNSLOUT MSG='EDC000'
         LH    R2,PFLEN            R2 = LENGTH OF STRING                00000934
         BCTR  R2,0                -1                                   00000935
         STC   R2,EDCCLC+1         CHANGE CLC LENGTH                    00000936
         STC   R2,EDCMVC+1         CHANGE MVC LENGTH                    00000936
         LH    R4,RECLEN           LAST COLUMN                          00000937
         SR    R4,R2               R4 = NN COLUMNS TO SCAN              00000938
         ST    R4,SAVEREDC         SAVE R4                              00000939
         L     R2,SAVCURR          LOAD CURRENT REC ADDRESS             00000940
         AH    R2,RECLEN           BEGIN WITH LINE 2                    00000941
EDCNEXTR DS    0H                                                       00000942
         LR    R3,R2                                                    00000942
         L     R4,SAVEREDC         RESTORE R4                           00000943
EDCCLC   CLC   0(00,R3),PFARG      SCAN HIT ?                           00000944
         BE    EDCHIT              YES                                  00000945
         LA    R3,1(R3)            NEXT COLUMN                          00000946
         BCT   R4,EDCCLC           LOOP                                 00000947
         AH    R2,RECLEN           NEXT RECORD                          00000948
         C     R2,SAVENDBA         END OF MEMBER ?                      00000949
         BNH   EDCNEXTR               NO,  SCAN NEXT RECORD                 0000
         MVC   PRETCOD,=CL3'982'      YES, STRING NOT FOUND              0000095
         BAL   R8,CALLERR          WRITE ERROR
         MVC   PSCRL,=CL2'NF'      MARK NOT FOUND
EDCHIT   DS    0H                                                       00000953
EDCMVC   MVC   0(00,R3),PCARG      CHANGE TEXT                          00000944
         ST    R2,SAVCURR          SET CURRENT REC ADDRESS              00000953
*******  CNSLOUT MSG='EDCHIT'
         L     R1,SAVCURR          LOAD CURRENT REC ADDRESS             00000263
         LTR   R1,R1               SOME ADDRESS ?                       00000264
         BZ    EDCEND              NO                                   00000265
         B     EDDSPLY             DISPLAY                              00000269
EDCEND   DS    0H
         B     PGMEND              PROGRAM-END
SAVEREDC DS    F
*-------------------------------------------------------------------
CALLERR  DS    0H
*-------------------------------------------------------------------
*******  CNSLOUT MSG='CALLERR'
         ST    R8,ERRR8          SAVE R8
*
         LA    R1,PARMS
         ST    R1,PARMLIST
         LA    R1,PARMLIST       CALL VSERROR -MAP-PROGRAM
         L     R15,VSERRAD
         BALR  R14,R15
*
         L     R8,ERRR8            -----------------------------------
         BR    R8                     RETURN TO CALLER
*-----------------------            --------------------------------
VSERRAD  DC    V(VSERROR)
ERRR8    DS    F
*-------------------------------------------------------------------
CALLWAIT DS    0H
*-------------------------------------------------------------------
*******  CNSLOUT MSG='CALLWAIT'
         ST    R8,WAIT8          SAVE R8
*
         LA    R1,PARMS
         ST    R1,PARMLIST
         LA    R1,PARMLIST       CALL VSERROR -MAP-PROGRAM
         L     R15,VSWAITAD
         BALR  R14,R15
*
         L     R8,WAIT8            -----------------------------------
         BR    R8                     RETURN TO CALLER
*-----------------------            --------------------------------
VSWAITAD DC    V(VSWAIT)
WAIT8    DS    F
*----------------------------------------------------------------------
*     AUFRUF VSRCSBA
*----------------------------------------------------------------------
CALLUTI  DS    0H
         ST    R8,SAV8              SAVE R8
*
         MVC   PRETCOD,=CL3'000'
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*                                   ----------------------------------- 00000720
         CALL  VSUTI                AUFRUF CALC SBA --> ROW / COL VS
*                                   ----------------------------------- 00000720
         L     R8,SAV8             RESTORE R8
         BR    R8                  RETURN TO CALLER
         EJECT
         DS    0F                                                       00001640
SAV8     DS    2F                                                       00001640
SBAWORK  DS    CL2                                                      00001641
SBAROW   DS    CL1                                                      00001642
SBACOL   DS    CL1                                                      00001643
*                                                                       00001644
*---------------------------------------------------------------------* 00000065
*     PGMEND Program-ENd                                              * 00000066
*---------------------------------------------------------------------* 00000067
PGMEND   DS    0H
*******  CNSLOUT MSG='PGMEND'
*******  L     R9,VISADR           LOAD PARAMETER-ADDR
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
*---------------------------------------------------------------------* 00000386
*        WORKFIELDS                                                   * 00000387
*---------------------------------------------------------------------* 00000015
*        REGISTER EQUATES                                             * 00000351
*---------------------------------------------------------------------* 00000352
R0       EQU   0                                                        00000353
R1       EQU   1                                                        00000354
R2       EQU   2                                                        00000355
R3       EQU   3                                                        00000356
R4       EQU   4                                                        00000357
R5       EQU   5                                                        00000358
R6       EQU   6                                                        00000359
R7       EQU   7                                                        00000360
R8       EQU   8                                                        00000361
R9       EQU   9                                                        00000362
R10      EQU   10                                                       00000363
R11      EQU   11                                                       00000364
R12      EQU   12                                                       00000365
R13      EQU   13                                                       00000366
R14      EQU   14                                                       00000367
R15      EQU   15                                                       00000368
* --------------------------------------------------------------------* 00000388
         DS    0D                                                       00000389
* --------------------------------------------------------------------* 00000369
*        SAVE AREA DECLARATIONS                                       * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F                                                       00000372
SAVEAREA DC    XL72'00'                 MONITOR TASK SAVE AREA           0000037
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS          00000379
IOAREND  DC    F'0'                     END-ADDR OF IOAREA
INAREND  DC    F'0'                     END-ADDR OF INAREA
INLINEAD DC    F'0'                     END-ADDR OF INLINA
VISADR   DS    F                                                        00000458
SAVINPAD DS    F                        SAVE INPUT LINE ADDRESS         00000458
SAVRECAD DS    F                        SAVE INPUT LINE ADDRESS         00000458
SAV1     DS    F                                                        00000458
SAV13    DS    F                                                        00000458
X00      DC    XL1'00'                                                  00000458
WEDLIN   DS    CL6                 WORK EDITOR LINE AREA                00001611
SAVLINES DS    F                   SAVE LINES TO ADD, DUP, INS          00001630
EDLMDTAD DS    F                   SAVE EDITOR LINE MDT ADDR            00001631
STACKSW  DS    CL1                 STACK OPEN SW                        00001608
ADDSW    DC    C'0'                ADD COMMAND SW                       00001605
SWCMD    DS    C                   SW COMMAND                           00001609
SWNUM    DS    C                   SW CMD NUMERIC OPERAND               00001610
TEMP     DS    CL80                WORK                                 00001616
TABCMD   DC    C'ACDIMK/R',X'FF'   EDITOR LINE COMMANDS                 00001612
*                                                                       00000459
DBL      DS    D                 RH                                     00001622
BLANKS   DC    255CL1' '
SCALE    DC    C'....+....1....+....2....+....3....+....4....+....5....*00001599
               +....6....+....7... (X)'                                 00001600
*
HELPSW   DC    C'0'                HELP ACTIVE SW                       00001606
MSGLEN   DS    F                                                        00001575
FLAGS    DC    X'00'               BYTE OF FLAGS                        00001576
PRINTABL DC    256AL1(*-PRINTABL)  00-FF                                00001541
         ORG   PRINTABL                                                 00001542
         DC    64X'E1'             00-3F                                00001543
         ORG                                                            00001544
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        00001563
BINTOEBC DC    X'40C1C2C3C4C5C6C7C8C94A4B4C4D4E4F'  0                   00001564
         DC    X'50D1D2D3D4D5D6D7D8D95A5B5C5D5E5F'  1                   00001565
         DC    X'6061E2E3E4E5E6E7E8E96A6B6C6D6E6F'  2                   00001566
         DC    X'F0F1F2F3F4F5F6F7F8F97A7B7C7D7E7F'  3                   00001567
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                   * 00000456
* --------------------------------------------------------------------* 00000457
PARMLIST DS    F
* --------------------------------------------------------------------* 00000424
         LTORG                                                          00000454
* --------------------------------------------------------------------* 00000426
         DS    0D                                                       00000427
* --------------------------------------------------------------------* 00000474
WORKA    DSECT
         COPY  LIBPARMS                                                 00000460
TABRECS  DSECT                                                          00000475
TABREC   DS    CL80                                                     00000476
*---------------------------------------------------------------------
*        LOAD-AREA FOR OUTBOUND-MAPS
*---------------------------------------------------------------------
MAPDS    DSECT
MAPADDR  DS    0CL2008
MAPNAME  DS    CL8
MAPEYEC  DS    CL8
         DS    CL2
MAPLEN   DS    CL2                MAP-LEN
MAPVCNT  DS    AL1(0)             VAR-FIELD COUNT
MAPVLST  DS    AL3(0)             ADDR MAP-VAR-OUTPUT-LIST
MAPSTART DS    CL2000
         END                                                            00000481
        BKEND
        CATALS   A.VSERROR,0.0
        BKEND   A.VSERROR
*------------------------------------------------------------------
*     NAME: VSERROR
*     TYPE: SUB-ROUTINE
* FUNCTION: writes an error message from a table to SPF
*
* INTERNAL
*
*
* EXTERNAL
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
* --------------------------------------------------------------------* 00000008
* WRITE ERROR MESSAGES                                                * 00000009
* --------------------------------------------------------------------* 00000015
*        REGISTER EQUATES                                             * 00000351
* --------------------------------------------------------------------* 00000352
R0       EQU   0                                                        00000353
R1       EQU   1                                                        00000354
R2       EQU   2                                                        00000355
R3       EQU   3                                                        00000356
R4       EQU   4                                                        00000357
R5       EQU   5                                                        00000358
R6       EQU   6                                                        00000359
R7       EQU   7                                                        00000360
R8       EQU   8                                                        00000361
R9       EQU   9                                                        00000362
R10      EQU   10                                                       00000363
R11      EQU   11                                                       00000364
R12      EQU   12                                                       00000365
R13      EQU   13                                                       00000366
R14      EQU   14                                                       00000367
R15      EQU   15                                                       00000368
VSERROR  CSECT                                                          00000016
         STM   R14,R12,12(R13)         STORE REGS IN SAVEAREA           00000018
         USING VSERROR,R11,R12         R11 NOW BASE REGISTER            00000020
         LR    R11,R15                 ESTABLISH ADDRESSABILITY         00000021
         B     GOON
         DC    C'***** VSERROR *****'  EYE-CATCHER
GOON     DS    0H
         LA    R12,4095(R11)                                            00000022
         LA    R12,1(R12)              SECOND BASE                      00000023
         ST    R13,SAVEAREA+4          STORE MAIN PGM SAVEAREA          00000024
         LR    R10,R13                 SAVE R13                         00000025
         LA    R13,SAVEAREA            SAVE AREA FOR THIS TASK          00000026
         ST    R13,8(R10)              STORE EXIT SAVE ADDRESS          00000027
* --------------------------------------------------------------------* 00000028
*        STATEMENTS FOR SET UP FIELDS NEEDED FOR LIBRM                * 00000029
* --------------------------------------------------------------------* 00000030
         L     R2,0(R1)                PARMS ADDRESS                    00000031
         ST    R2,PARMADDR             SAVE                             00000032
         USING WORKA,R2
         L     R9,PBUFADR              LOAD DATATAB ADDRESS             00000034
         ST    R9,TABRECSA             SET  TABRECS ADDRESS             00000035
         USING TABRECS,R9              ESTABLISH ADDRESSABILITY         00000036
* --------------------------------------------------------------------* 00000065
*     PGM-START                                                       * 00000066
* --------------------------------------------------------------------* 00000067
*****    CNSLOUT MSG='PGM-START'
         BAL   R8,ERRORMSG
         B     PGMEND
*----------------------------------------------------------------
*  ERRORMSG DISPLAY ERROR-MAP SPFMERR with MAPERROR
*----------------------------------------------------------------
ERRORMSG DS   0H
         ST    R8,ERRMSGR8
         CLC   PRETCOD,=CL3'000'
         BNE   ERROR001
         MVC   PRETCOD,=CL3'---'
ERROR001 DS    0H
         BAL   R8,ERRMAP            ERROR-MAP
ERRORRET DS    0H
         MVC   MAPERROR,MINUS
         MVC   PRETCOD,=CL3'000'
         L     R8,ERRMSGR8
         BR    R8
ERRMSGR8 DS    F
*----------------------------------------------------------------
*  OUTPUT: ERROR-MAP
*----------------------------------------------------------------
ERRMAP   DS    0H
         STM   R8,R10,SAVERR
         LA    R10,MSGERRTB         ADDR OF ERROR MESG TABLE
ERRMAPLP DS    0H
         CLC   PRETCOD,0(R10)       IS IT THE WANTED ERROR MESG ?
         BE    ERRMAP01             YES
         CLC   0(3,R10),=CL3'###'   IS IT THE END OF TABLE
         BE    ERRMAP02             YES
         LA    R10,34(R10)          NO, LOOK AGAIN
         B     ERRMAPLP
ERRMAP01 DS    0H
         MVC   MAPERROR,0(R10)      MESSAGE Found
         B     ERRMAP03
ERRMAP02 DS    0H
         MVC   MAPERROR,=CL34'    -no Message-Entry found'
         MVC   MAPERROR(4),PRETCOD
         B     ERRMAP03
ERRMAP03 DS    0H
         MVC   MAPERRNW,MINUS       INITIALIZE WITH -
         MVI   ERRMAPB+1,X'80'      REINSTALL BE
         LA    R8,MAPERROR+33
         LA    R9,MAPERRNW+32
         LA    R10,34
ERRMAP04 DS    0H                   MAKE MSG RIGHTBOUND
         CLI   0(R8),C' '
ERRMAPB  BE    ERRMAP05
         MVI   ERRMAPB+1,X'00'      MAKE A NOP AFTER 1 NON-BLANK
         MVC   0(1,R9),0(R8)
         BCTR  R9,0
ERRMAP05 DS    0H                   MAKE MSG RIGHTBOUND
         BCTR  R8,0
         BCT   R10,ERRMAP04
*
         LOAD  SPFMERR,MAPADDR      TOP-MAP OF SPFVS
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
         L     R8,MAPVCNT           LOAD ADDR-LIST-ADDR
         ICM   R8,8,X00             CLEAR CNT-BYTE
         L     R8,0(R8)             Load 1. VAR-OUT-ADDR MAPNAME
         MVC   0(34,R8),MAPERRNW    ERROR-MSG
         MVC   MSGINF,ERRACT        ERROR-INFORMATION
*
         LA    R1,WRCCW             YES, THEN WRITE NOERASE
         BAL   R8,EXCPWR00          EXEC / WAIT FOR I/O COMPLETION.
*
         LM    R8,R10,SAVERR        RESTORE R8
         BR    R8                   RETURN TO CALLER
*
MSGINF   DS    CL1                  INFORMATION ABOUT ERROR-MSG
MAPERRNW DS    CL34                 ERROR-MSG
BLANKS   DC    CL34' '              INITIALIZE MSG
MINUS    DC    CL34'-'              INITIALIZE MSG
MAPERROR DS    0CL34                ERROR-MSG
ERRNUM   DS    CL3
ERRACT   DS    CL1
ERRMSG   DS    CL30
SAVERR   DS    4F
*----------------------------------------------------------------
*     EXCP - EXECUTE CHANNEL PROGRAM WRITE ERASE
*----------------------------------------------------------------
EXCPWR00 DS    0H
         STCM  R1,7,EXCPCCB+9    STORE CCW-ADDR   in CCB.
         LA    R1,EXCPCCB        Issue Command
         EXCP  (1)
         WAIT  (1)
         BR    R8
*-----------------------------------------------------------------
EXCPCCB  CCB   SYS007,DUMMY,X'0000'
WRECCW   CCW   X'05',MAPSTART,X'20',L'MAPSTART   WRITE-ERASE
WRCCW    CCW   X'01',MAPSTART,X'20',L'MAPSTART   WRITE-NOERASE
DUMMY    EQU   *
         DC    XL1'C3'   WCC Reset MDT
         $SBA  (22,6,N)
         DC    C'*** DUMMY ***'
DUMMYL   EQU   *-DUMMY
*****    CNSLCCB
* --------------------------------------------------------------------* 00000065
*     PGMEND Program-ENd                                              * 00000066
* --------------------------------------------------------------------* 00000067
PGMEND   DS    0H
         MVC   PRETCOD,=CL3'000'   DELETE ERROR
******   CNSLOUT MSG='PGM-END'
         L     R9,PARMADDR         LOAD PARAMETER-ADDR
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
* --------------------------------------------------------------------* 00000386
*        WORKFIELDS                                                   * 00000387
* --------------------------------------------------------------------* 00000388
         DS    0D                                                       00000389
*                                                                       00000391
* --------------------------------------------------------------------* 00000404
*        MESSAGE TABLES                                               * 00000405
* --------------------------------------------------------------------* 00000406
         DS    0F                       ALIGNEMENT                      00000407
* --------------------------------------------------------------------* 00000369
*        SAVE AREA DECLARATIONS                                       * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F                                                       00000372
SAVEAREA DC    XL72'00'                 MONITOR TASK SAVE AREA           0000037
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS          00000379
PARMADDR DS    F                                                        00000458
X00      DC    XL1'00'                                                  00000458
*                                                                       00000459
* --------------------------------------------------------------------* 00000424
         LTORG                                                          00000454
* --------------------------------------------------------------------* 00000424
MSGERRTB DS    0H
         DC    CL34'---------------------------------'
         DC    CL34'881E-Invalid Function in VSRDIR'
         DC    CL34'882E-Invalid Function Modul VSED'
         DC    CL34'883E-Invalid Function Modul VSPUT'
         DC    CL34'884E-FREEVIS-Error in Modul VSPUT'
         DC    CL34'885E-GETVIS-Error in Modul VSPUT'
         DC    CL34'886I-Job submitted '
         DC    CL34'887E-Job not submitted - Error '
         DC    CL34'888I-Member saved '
         DC    CL34'889I-Member not saved - Error'
         DC    CL34'890E-FREEVIS failed VSSPF/VSSORT'
         DC    CL34'891E-GETVIS failed  VSSPF/VSSORT'
         DC    CL34'892E-Line CMD not found or invalid'
         DC    CL34'893E-CMD-Switch already on'
         DC    CL34'894E-All CMD Line Numbers are zero'
EDLERRM  DC    CL34'895E-ERROR in TYPE III Area '
OVFLMSG  DC    CL34'896E-Line cmd overflow work area '
DELERRM  DC    CL34'897E-DEL of entire area invalid '
STKFULLM DC    CL34'898E-Editor STACK is full '
PARMERRM DC    CL34'899E-Syntax error or missing parm   '
INVCMD   DC    CL34'900I-Line Cmd not in CMDTAB'
         DC    CL34'901I-Invalid Command  '
         DC    CL34'902I-Not yet implemented'
         DC    CL34'903E-Command-Switch already on VSED'
         DC    CL34'904E-All CMD-Line-# are Zero VSED'
         DC    CL34'905E-Member already exists in LIB'
         DC    CL34'906E-too less params specified'
         DC    CL34'907E-pls specify sublib and member'
*
         DC    CL34'982I-String not found'
         DC    CL34'983I-Invalid Scroll-Amount'
         DC    CL34'984I-Invalid Function in VSMAINT'
         DC    CL34'985I-MEMBER NOT deleted'
         DC    CL34'986I-MEMBER deleted'
         DC    CL34'987I-MEMBER NOT renamed'
         DC    CL34'988I-MEMBER renamed'
         DC    CL34'989I-LIB NOT condensed'
         DC    CL34'990I-LIB condensed'
         DC    CL34'991E-POWER/VS-ERROR in VSMAINT'
TOPMSG   DC    CL34'992  '
EOFMSG   DC    CL34'993  '
         DC    CL34'994E-Member already exists'
         DC    CL34'995A-Member not found '
         DC    CL34'996I-Please specify Sub-Library'
         DC    CL34'997I-Please specify Member-Name'
         DC    CL34'999E-Buffer too large '
STRERRM  DC    CL34'Length of strings not equal '
PFKERRM  DC    CL34'This KEY is not defined '
JOBNOF   DC    CL34'Jobname not found '
STRNOF   DC    CL34'String not found '
RESEQUEN DC    CL34'Resequenced '
WITHDATA DC    CL34'with DATA=X '
FULLMSG  DC    CL34'Member too large for work area '
         DC    CL3'###'                  Tab-End
*----------------------------------------------------
         DS    0F
MAPADDR  DS    0CL2028
MAPNAME  DS    CL8
MAPEYEC  DS    CL8
         DS    CL2
MAPLEN   DS    CL2                MAP-LEN
MAPVCNT  DS    AL1(0)             VAR-FIELD COUNT
MAPVLST  DS    AL3(0)             ADDR MAP-VAR-OUTPUT-LIST
MAPSTART DS    CL200
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                   * 00000456
* --------------------------------------------------------------------* 00000457
WORKA    DSECT                                                          00000475
         COPY  LIBPARMS                                                 00000460
         DS    0D                                                       00000427
* --------------------------------------------------------------------* 00000474
TABRECS  DSECT                                                          00000475
TABREC   DS    CL80                                                     00000476
* --------------------------------------------------------------------* 00000477
BUFFER   EQU   *                   MEMBER BUFFER BEGIN                  00001705
         END                                                            00000481
        BKEND
        CATALS   A.VSLIBRM,0.0
        BKEND   A.VSLIBRM
*------------------------------------------------------------------
*     NAME: VSLIBRM
*     TYPE: SUB-ROUTINE
* FUNCTION: ACCESS TO DOS/VS R34 LIBRARIES VIA DTFSL-MACRO
*
* INTERNAL
*
*
* EXTERNAL
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
VSLIBRM  CSECT                                                          00000016
         USING *,R15                                                    00000017
         STM   R14,R12,12(R13)         STORE REGS IN SAVEAREA           00000018
         DROP  R15                     R15 TO BE USED BY MACROS         00000019
         USING VSLIBRM,R11,R12         R11 NOW BASE REGISTER            00000020
         LR    R11,R15                 ESTABLISH ADDRESSABILITY         00000021
         LA    R12,4095(R11)                                            00000022
         LA    R12,1(R12)              SECOND BASE                      00000023
         B     GOON
         DC    C'*****VSLIBRM****'     EYECATCHER
GOON     DS    0H
         ST    R13,APIMONS1+4          STORE MAIN PGM SAVEAREA          00000024
         LR    R10,R13                 SAVE R13                         00000025
         LA    R13,APIMONS1            SAVE AREA FOR THIS TASK          00000026
         ST    R13,8(R10)              STORE EXIT SAVE ADDRESS          00000027
* --------------------------------------------------------------------* 00000028
*        STATEMENTS FOR SET UP FIELDS NEEDED FOR VSLIBRM              * 00000029
* --------------------------------------------------------------------* 00000030
         L     R2,0(R1)                PARMS ADDRESS                    00000031
         ST    R2,VISADR               SAVE
         USING WORKA,R2                ADDRESSING LIBPARMS
*
         L     R9,PBUFADR              LOAD TABRECS ADDRESS             00000034
         ST    R9,TABRECSA             SET  TABRECS ADDRESS             00000035
*****    CNSLOUT REG=9
*****    CNSLOUT ADR=PARMS
*****    CNSLOUT ADR=PMSG
         USING TABRECS,R9              ESTABLISH ADDRESSABILITY         00000036
* --------------------------------------------------------------------* 00000041
*        FNDSL                                                        * 00000041
* --------------------------------------------------------------------* 00000041
         MVC    NFMEM(1),PSLIB                                          00000042
         MVC    NFMEM+1(8),PMEMB                                        00000043
         LA     R1,NFMEM               LOAD SUBLIB AND MEM-NAME         00000044
         FNDSL  DTFSL,(1),NFADDR       CALL FIND-LOGIC
         B      FNDOK                                                   00000045
******   CNSLCCB
* --------------------------------------------------------------------* 00000046
*        NOT FOUND - ERROR                                            * 00000047
* --------------------------------------------------------------------* 00000046
NFADDR   DS     0H                                                      00000048
         MVC    PRETCOD,=CL3'995'                                       00000049
         B      PGMEND
NFMSG    DS     0CL80
NFMEM    DS     CL9
* --------------------------------------------------------------------* 00000046
*        FOUND, NOW GET IT                                            * 00000047
* --------------------------------------------------------------------* 00000046
FNDOK    DS     0H
         LA     R7,0                                                    00000050
         ST     R7,PNORL               INIT NUMBER OF RECORDS
         L      R9,PBUFADR
         CLC    PFUNC,=CL3'CRE'        FUNCTION CRE AND MEMBER EXISTS
         BNE    GETNEXT
         MVC    PRETCOD,=CL3'994'      YES, SET ERROR CODE
         B      PGMEND                 AND EXIT
GETNEXT  DS     0H                     GET NXT RECD, UNTIL EOFADDR
         GETSL  DTFSL,INAREA,EOFADDR                                    00000051
         MVC    PRETCOD,=CL3'000'      GET OK                           00000052
         MVI    INAREA+71,C' '         BLANK TO CONTINUATION COL        00000053
         L      R7,PNORL               LOAD RECD COUNTER                00000054
         LA     R7,1(R7)               INCR R7 for 1                    00000055
         ST     R7,PNORL               SAVE RECD COUNTER                00000056
         MVC    0(80,R9),INAREA        ONE LINE TO TAB                  00000057
         LA     R9,80(R9)              INCR R9 FOR ONE LINE LEN          0000005
         B      GETNEXT                LOOP FOR NXT GET RECD
EOFADDR  DS     0H                     END OF MEMBER REACHED
         B      PGMEND                 END OF ROUTINE                   00000052
* --------------------------------------------------------------------* 00000058
*        ERRORMSG Error DTFSL                                         * 00000058
* --------------------------------------------------------------------* 00000058
ERRORMSG DS     0H
         MVC    PRETCOD,=CL3'994' Execution error of DTFSL              00000049
         B      PGMEND                 END OF ROUTINE                   00000052
         LTORG
* --------------------------------------------------------------------* 00000065
*        PGMEND Program-ENd                                           * 00000066
* --------------------------------------------------------------------* 00000067
PGMEND   DS    0H
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
* --------------------------------------------------------------------* 00000350
INAREA   DS    CL80                Member Line                          00000350
* --------------------------------------------------------------------* 00000350
*        REGISTER EQUATES                                             * 00000351
* --------------------------------------------------------------------* 00000352
R0       EQU   0                                                        00000353
R1       EQU   1                                                        00000354
R2       EQU   2                                                        00000355
R3       EQU   3                                                        00000356
R4       EQU   4                                                        00000357
R5       EQU   5                                                        00000358
R6       EQU   6                                                        00000359
R7       EQU   7                                                        00000360
R8       EQU   8                                                        00000361
R9       EQU   9                                                        00000362
R10      EQU   10                                                       00000363
R11      EQU   11                                                       00000364
R12      EQU   12                                                       00000365
R13      EQU   13                                                       00000366
R14      EQU   14                                                       00000367
R15      EQU   15                                                       00000368
* --------------------------------------------------------------------* 00000369
*        SAVE AREA DECLARATIONS                                       * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F                                                       00000372
APIMONS1 DC    XL72'00'                MONITOR TASK SAVE AREA           00000373
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS          00000379
LEN1     DC    F'0'                     TEMP FIELD                      00000383
* --------------------------------------------------------------------* 00000386
*        WORKFIELDS                                                   * 00000387
* --------------------------------------------------------------------* 00000388
         DS    0D                                                       00000389
*                                                                       00000391
* --------------------------------------------------------------------* 00000392
*        PARAMETER FIELD DEFINITIONS FOR GET MEMBER                   * 00000393
* --------------------------------------------------------------------* 00000404
*        MESSAGE TABLES                                               * 00000405
* --------------------------------------------------------------------* 00000406
         DS    0F                       ALIGNEMENT                      00000407
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                   * 00000456
* --------------------------------------------------------------------* 00000457
VISADR   DS    F                                                        00000458
*                                                                       00000459
* --------------------------------------------------------------------* 00000424
         LTORG                                                          00000454
* --------------------------------------------------------------------* 00000426
         DS    0D                                                       00000427
*                                                                       00000449
* --------------------------------------------------------------------* 00000453
*        DTFSL                                                        * 00000453
* --------------------------------------------------------------------* 00000472
DTFSL    DTFSL NOTEPNT=YES,PRIVATE=YES,ERROR=ERRORMSG
*        TABLE                                (GETVIS AREA)           * 00000473
* --------------------------------------------------------------------* 00000474
WORKA    DSECT
         COPY  LIBPARMS
TABRECS  DSECT                                                          00000475
TABREC   DS    CL80                                                     00000476
* --------------------------------------------------------------------* 00000477
         END                                                            00000481
        BKEND
        CATALS   A.VSMAINT,0.0
        BKEND   A.VSMAINT
*------------------------------------------------------------------
*     NAME: VSMAINT
*     TYPE: SUB-ROUTINE
* FUNCTION: Executes a MAINT for CATALx, DELETx, RENAMx
*
* INTERNAL
*
*
* EXTERNAL
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
         TITLE 'VSMAINT - POWER UPDATE'
         PRINT GEN
VSMAINT  CSECT
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
VSMAINT CSECT
         STM   R14,R12,12(R13)         SAVE REGS in CALLERS SA
         USING VSMAINT,R5
         LR    R5,R15                  FIRST BASE READY
         B     GOON
         DC    C'***** VSMAINT*****'   EYECATCHER
GOON     DS    0H
         ST    R13,NEWSAVE+4           SAVE OLD SA IN NEW SA
         LR    R10,R13
         LA    R13,NEWSAVE             LOAD NEW SA
         ST    R13,8(R10)              AND SAVE IT IN OLD SA
* -------------------------------------------
*    SETUP PARAMETERS FOR VSMAINT           -
* -------------------------------------------
         L     R2,0(0,R1)              R2 IS MESSAGE CONTROL BLOCK
         USING WORKA,R2
         ST    R2,PARMADDR
*-----------------------------
         L     R9,PBUFADR              BUFFERADR
         ST    R9,TABRECSA
         LA    R4,11                   # OF JOB RECORDS FOR MAINT
*-----------------------------
         MVC   DOSASSS+8(3),=CL3'*  ' COMMENT ASSGN-SYSSLB-STMT
         CLI   PFUNC+2,C'C'            IS IT A CONDS COMMAND ?
         BE    CONDS000                YES
         CLI   PFUNC+2,C'R'            NO, IS IT A RENAME COMMAND ?
         BE    RENAM000                YES
         CLI   PFUNC+2,C'D'            IS IT A DELETE COMMAND ?
         BE    DELET000                YES
         B     INVFUNC                 NO, INVALID FUNCTION PGMEND
*--------------------------------------
*  CONDENSE A LIBRARY
*--------------------------------------
CONDS000 DS    0H                      CONDENSE LIBRARY
         MVC   POWJOB+21(8),=CL8'CONDENSE'
         MVC   DOSJOB+15(8),=CL8'CONDENSE'
         MVC   DOSMFUNC(7),=CL7'CONDS  '
         CLI   PLIBNAME+1,C'C'          IS IT A CORE IMAGE CONDS ?
         BNE   CONDS010                 NO
         MVC   DOSMFUNC+6(2),=CL2'CL'
         B     BUILDJOB
CONDS010 DS    0H
         CLI   PLIBNAME+1,C'R'          IS IT A RELOCATABLE CONDS ?
         BNE   CONDS020                 NO
         MVC   DOSMFUNC+6(2),=CL2'RL'
         B     BUILDJOB
CONDS020 DS    0H
         CLI   PLIBNAME+1,C'S'          IS IT A SOURCE CONDS ?
         BNE   CONDS030                 NO
         MVC   DOSMFUNC+6(2),=CL2'SL'
         CLI   PLIBNAME,C'P'            PRIVATE SLB ?
         BNE   CONDS030                 NO
         MVC   DOSASSS+8(3),=CL3'// '   ASSGN SYSSLB
         B     BUILDJOB
CONDS030 DS    0H
         CLI   PLIBNAME+1,C'P'          IS IT A PROCEDURE CONDS ?
         BNE   INVFUNC                  NO, INVALID FUNCTION
         MVC   DOSMFUNC+6(2),=CL2'PL'
         B     BUILDJOB
*-------------------------------------------------------
*  RENAME A MEMBER (OLDNAME IN PMEMB, NEWNAME IN PWORK)
*-------------------------------------------------------
RENAM000 DS    0H
         MVC   POWJOB+21(8),PMEMB      MEMBERNAME FOR POWER-JOBNAME
         MVC   DOSJOB+15(8),PMEMB      MEMBERNAME FOR DOSVS-JOBNAME
         MVC   DOSMNAME+2(8),PMEMB     OLD MEMBERNAME
         MVI   DOSMNAME+10,C','
         MVC   DOSMNAME+11(8),PWORK     NEW MEMBERNAME
RENAM010 DS    0H
         CLI   PLIBNAME+1,C'C'          IS IT A CORE IMAGE RENAME
         BNE   RENAM020                 NO
         MVC   DOSMFUNC(7),=CL7'RENAMC '
         B     BUILDJOB
RENAM020 DS    0H
         CLI   PLIBNAME+1,C'R'          IS IT A RELOCATABLE RENAME
         BNE   RENAM030                 NO
         MVC   DOSMFUNC(7),=CL7'RENAMR '
         B     BUILDJOB
RENAM030 DS    0H
         CLI   PLIBNAME+1,C'S'          IS IT A SOURCE RENAME
         BNE   RENAM040                 NO
         MVC   DOSMFUNC(7),=CL7'RENAMS '
         CLI   PLIBNAME,C'P'            PRIVATE SLB ?
         BNE   RENAM040                 NO
         MVC   DOSASSS+8(3),=CL3'// '   ASSGN SYSSLB
         B     BUILDJOB
RENAM040 DS    0H
         CLI   PLIBNAME+1,C'P'          IS IT A PROCEDURE RENAME
         BNE   INVFUNC                  NO, INVALID LIB
         MVC   DOSMFUNC(7),=CL7'RENAMP '
         B     BUILDJOB
*-------------------------------------------------------
*  DELETE A MEMBER
*-------------------------------------------------------
DELET000 DS    0H
         MVC   POWJOB+21(8),PMEMB      MEMBERNAME FOR POWER-JOBNAME
         MVC   DOSJOB+15(8),PMEMB      MEMBERNAME FOR DOSVS-JOBNAME
         MVC   DOSMNAME+2(8),PMEMB     OLD MEMBERNAME
         CLI   PLIBNAME+1,C'S'          IS IT A SOURCE DELETE
         BNE   DELET010                 NO
         MVC   DOSMNAME(1),PSLIB
         MVI   DOSMNAME+1,C'.'
         MVC   DOSMNAME+2(8),PMEMB
DELET010 DS    0H
         CLI   PLIBNAME+1,C'C'          IS IT A CORE IMAGE DELETE
         BNE   DELET020                 NO
         MVC   DOSMFUNC(7),=CL7'DELETC '
         B     BUILDJOB
DELET020 DS    0H
         CLI   PLIBNAME+1,C'R'          IS IT A RELOCATABLE DELETE
         BNE   DELET030                 NO
         MVC   DOSMFUNC(7),=CL7'DELETR '
         B     BUILDJOB
DELET030 DS    0H
         CLI   PLIBNAME+1,C'S'          IS IT A SOURCE DELETE
         BNE   DELET040                 NO
         MVC   DOSMFUNC(7),=CL7'DELETS '
         CLI   PLIBNAME,C'P'            PRIVATE SLB ?
         BNE   BUILDJOB                 NO
         MVC   DOSASSS+8(3),=CL3'// '   ASSGN SYSSLB
         B     BUILDJOB
DELET040 DS    0H
         CLI   PLIBNAME+1,C'P'          IS IT A PROCEDURE DELETE
         BNE   INVFUNC                  NO, INVALID LIB
         MVC   DOSMFUNC(7),=CL7'DELETP '
         B     BUILDJOB
*-----------------------------
BUILDJOB DS    0H
*-----------------------------
POINTSPL DS    0H
         LA    R10,POINTSPL            RH RETURN-ADDR AFTER POWWAIT
*-------------------------
*---   CONNECT TO POWER/VS
*-------------------------
         LA    R12,PUTSPL              POINT TO PUTSPOOL SPL CONTROL
         USING SPLMAP,R12
         STCM  R12,7,ICRXECB+5         SAVE ADDRESS IN XECB
         XECBTAB TYPE=DEFINE,XECB=ICRXECB,ACCESS=XWAIT
         LTR   R15,R15                 ANY ERRORS
         BNZ   POWWAIT                 YES = GO ASSUME BUSY
*--------------------------
*---   EXECUTE POWER
*--------------------------
         PUTSPOOL SPL=(R12),CBUF=POWJOB     STUFF JOB INTO READER
*---------------------------
*---   DISCONN FROM POWER/VS
*---------------------------
         XECBTAB TYPE=DELETE,XECB=ICRXECB
         CLI   SPER,0                  ANY ERRORS
         BNE   POWERR                  YES - GO HANDLE
         MVC   PRETCOD,=CL3'000'
         B     POWEXIT                 NO - GO RETURN
*
POWERR   DS    0H                      ADD ANY ERROR HANDLING HERE
         MVC   PRETCOD,=CL3'991'        POWER ERROR IN VSMAINT
         B     POWEXIT
POWWAIT  DS    0H
         SETIME 5,TECB                 WAIT 5 SECONDS AND TRY AGAIN
         WAIT  TECB
         BR    R10
POWEXIT  DS    0H
         CLC   PRETCOD,=CL3'000'
         BNE   NOTDONE
         CLI   PFUNC+2,C'C'           CONDENSE FUNCTION ?
         BNE   POWEXIT1
         MVC   PRETCOD,=CL3'990'      LIB CONDENSED
         B     PGMEND
POWEXIT1 DS    0H
         CLI   PFUNC+2,C'R'           RENAME   FUNCTION ?
         BNE   POWEXIT2
         MVC   PRETCOD,=CL3'988'      MEMBER RENAMED
         B     PGMEND
POWEXIT2 DS    0H
         CLI   PFUNC+2,C'D'           DELETE   FUNCTION ?
         BNE   PGMEND
         MVC   PRETCOD,=CL3'986'      MEMBER DELETED
         B     PGMEND
NOTDONE  DS    0H
         CLI   PFUNC+2,C'C'           CONDENSE FUNCTION ?
         BNE   NOTDON10
         MVC   PRETCOD,=CL3'989'      LIB NOT CONDENSED
         B     PGMEND
NOTDON10 DS    0H
         CLI   PFUNC+2,C'R'           RENAME   FUNCTION ?
         BNE   NOTDON20
         MVC   PRETCOD,=CL3'987'      MEMBER NOT RENAMED
         B     PGMEND
NOTDON20 DS    0H
         CLI   PFUNC+2,C'D'           DELETE   FUNCTION ?
         BNE   PGMEND
         MVC   PRETCOD,=CL3'985'      MEMBER NOT DELETED
         B     PGMEND
INVFUNC  DS    0H
         MVC   PRETCOD,=CL3'984'      INVALID FUNCTION MODUL VSMAINT
*****    CNSLOUT ADR=PARMS
         B     PGMEND
PGMEND   DS    0H
         L     R1,PARMADDR         LOAD PARAMETER-ADDR
         MVC   0(L'PARMS,R1),PARMS
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
* --------------------------------------------------------------------*
*        POWER JOB CARD DEFINITIONS                                   *
* --------------------------------------------------------------------*
         DS    0F
POWJOB   DS    0CL88
         DC    A(POWLST),A(0),CL80'* $$ JOB JNM=Y       ,CLASS=0'
POWLST   DS    0CL88
         DC    A(DOSJOB),A(0),CL80'* $$ LST CLASS=P,DISP=D,JSEP=1'
DOSJOB   DS    0CL88
         DC    A(DOSDLBL),A(0),CL80'// JOB Y       '
DOSDLBL  DS    0CL88
       DC A(DOSEXT),A(0),CL80'// DLBL IJSYSSL,''DOSVS.OPTIONAL.DOSVS'''
DOSEXT   DS    0CL88
         DC    A(DOSASSS),A(0),CL80'*  EXTENT SYSSLB,OPTLB1'
DOSASSS  DS    0CL88
         DC    A(DOSEXE),A(0),CL80'// ASSGN SYSSLB,DISK,VOL=OPTLB1,SHR'
DOSEXE   DS    0CL88
         DC    A(DOSMF),A(0),CL80'// EXEC MAINT'
*
DOSMF    DS    0CL88
         DC    A(DOSENDD)
         DC    A(0)
         DC    CL3' '
DOSMFUNC DC    CL7' '
DOSMNAME DC    CL70' '
*
DOSENDD  DS    0CL88
         DC    A(DOSENDJ),A(0),CL80'/*'
DOSENDJ  DS    0CL88
         DC    A(POWENDJ),A(0),CL80'/&&'
POWENDJ  DS    0CL88
         DC    A(0),A(0),CL80'* $$ EOJ'
* --------------------------------------------------------------------*
*        SAVE AREA DECLARATIONS                                       *
* --------------------------------------------------------------------*
         DS    0F
NEWSAVE  DC    18F'0'                   MONITOR TASK SAVE AREA
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS
LEN1     DC    F'0'                     TEMP FIELD
         SPACE 2
         LTORG
ICRXECB  DC    A(0,0)
TECB     TECB
PUTSPL   SPL   TYPE=DEFINE,PBUF=FEEDBACK,PBUFL=88
FEEDBACK DS    0CL88
         DC    CL28' '
FEEDB028 DC    CL16' '
         DC    CL16' '
         DC    CL16' '
         DC    CL12' '
         SPACE 2
POWBUFAD DC    F'0'
POWBUFLN DC    F'0'
POWBUFPT DC    F'0'    RH Test
         SPACE 2
******   CNSLCCB
         SPACE 2
* --------------------------------------------------------------------*
*        PARAMETERS                                                   *
* --------------------------------------------------------------------*
PARMADDR DS    F
* --------------------------------------------------------------------*
         LTORG
* --------------------------------------------------------------------*
SPLMAP   SPL   TYPE=MAP
WORKA    DSECT
         COPY LIBPARMS
         END
        BKEND
        CATALS   A.VSPM012,0.0
        BKEND   A.VSPM012
*------------------------------------------------------------------
*     NAME: SPMF012
*     TYPE: SUB-ROUTINE
* FUNCTION: Prepare map SPMF012 for output
*
* INTERNAL
*
*
* EXTERNAL VSUTI - Utility-program
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
         PUNCH ' CATALR VSPM012'
* --------------------------------------------------------------------* 00000008
* EDIT SSL MEMBER                                                     * 00000009
* --------------------------------------------------------------------* 00000015
*        REGISTER EQUATES                                             * 00000351
* --------------------------------------------------------------------* 00000352
R0       EQU   0                                                        00000353
R1       EQU   1                                                        00000354
R2       EQU   2                                                        00000355
R3       EQU   3                                                        00000356
R4       EQU   4                                                        00000357
R5       EQU   5                                                        00000358
R6       EQU   6                                                        00000359
R7       EQU   7                                                        00000360
R8       EQU   8                                                        00000361
R9       EQU   9                                                        00000362
R10      EQU   10                                                       00000363
R11      EQU   11                                                       00000364
R12      EQU   12                                                       00000365
R13      EQU   13                                                       00000366
R14      EQU   14                                                       00000367
R15      EQU   15                                                       00000368
VSPM012  CSECT                                                          00000016
         STM   R14,R12,12(R13)         STORE REGS IN SAVEAREA           00000018
         USING VSPM012,R8              R11 NOW BASE REGISTER            00000020
         LR    R8,R15                  ESTABLISH ADDRESSABILITY         00000021
         B     GOON
         DC    CL19'***** VSPM012 *****'
GOON     DS    0H
         ST    R13,SAVEAREA+4          STORE MAIN PGM SAVEAREA          00000024
         LR    R10,R13                 SAVE R13                         00000025
         LA    R13,SAVEAREA            SAVE AREA FOR THIS TASK          00000026
         ST    R13,8(R10)              STORE EXIT SAVE ADDRESS          00000027
* --------------------------------------------------------------------* 00000028
*        STATEMENTS FOR SET UP FIELDS NEEDED FOR LIBRM                * 00000029
* --------------------------------------------------------------------* 00000030
         L     R2,0(R1)                PARMS ADDRESS                    00000031
         USING WORKA,R2                PARAMETER-ADDRESSING
         ST    R2,PARMADDR             SAVE                             00000032
         L     R9,PBUFADR              LOAD DATATAB ADDRESS             00000034
         ST    R9,TABRECSA             SET  TABRECS ADDRESS             00000035
         USING TABRECS,R9              ESTABLISH ADDRESSABILITY         00000036
******   CNSLOUT MSG='VSPM012'
* --------------------------------------------------------------------* 00000065
*     PGM-START                                                       * 00000066
* --------------------------------------------------------------------* 00000067
         L     R7,PMAPADDR          ADDRESS SPFM012-MAP
         USING MAPDS,R7
         L     R12,MAPVCNT          LOAD ADDR-LIST-ADDR
         ICM   R12,8,X00            CLEAR CNT-BYTE
         L     R10,0(R12)           1.Addr = M012LIB
*--------------------------------
         CLI   PLIBNAME,C'P'        IS IT A PRIVATE LIB ?
         BNE   VSPM010                  NO
         MVC   0(8,R10),=CL8'PRIVATE '  YES
         B     VSPM015
VSPM010  DS    0H
         MVC   0(8,R10),=CL8'SYSTEM '
VSPM015  DS    0H
         CLI   PLIBNAME+1,C'C'        IS IT A CORE IMG LIB ?
         BNE   VSPM020                          NO
         MVC   8(14,R10),=CL14'CORE IMAGE LIB'  YES
         B     VSPM060
VSPM020  DS    0H
         CLI   PLIBNAME+1,C'R'        IS IT RELOCATABLE LIB ?
         BNE   VSPM030                          NO
         MVC   8(15,R10),=CL15'RELOCATABLE LIB'  YES
         B     VSPM060
VSPM030  DS    0H
         CLI   PLIBNAME+1,C'S'        IS IT SOURCE STMT LIB ?
         BNE   VSPM040                          NO
         MVC   8(15,R10),=CL15'SOURCE STMT LIB' YES
         B     VSPM060
VSPM040  DS    0H
         CLI   PLIBNAME+1,C'P'        IS IT PRODECURE LIB ?
         BNE   VSPM060                          NO
         MVC   8(13,R10),=CL13'PROCEDURE LIB'  YES
         B     VSPM060
VSPM060  DS    0H
******   CNSLOUT ADR=PLIBNAME
*--------------------------------
* START-ADDR C H R
*--------------------------------
         XC    WORKF,WORKF          INIT WORK FIELD
*****    CNSLOUT ADR=PDIRSA,LEN=7
*
         XC    PCCHHBI,PCCHHBI
         MVC   PCCHHBI(5),PDIRSA+2   CC START OF DIR
         MVI   PCCHHBIF,X'05'        Input Length
         MVC   PFUNC,=CL3'B2C'       BINARY TO CHAR
         BAL   R11,CALLUTI
         L     R10,4(R12)          2.Addr = M012SAD CC HH R
         MVC   0(9,R10),PCCHHAR
*
******   CNSLOUT ADR=PLIBSA,LEN=7
         XC    PCCHHBI,PCCHHBI
         MVC   PCCHHBI(5),PLIBSA+2   CC START OF DIR
         MVI   PCCHHBIF,X'05'        Input Length
         MVC   PFUNC,=CL3'B2C'       BINARY TO CHAR
         BAL   R11,CALLUTI
         L     R10,8(R12)          3.Addr = M012SAL CC HH R
         MVC   0(9,R10),PCCHHAR
*--------------------------------
* Next-ADDR C H R
*--------------------------------
         XC    WORKF,WORKF          INIT WORK FIELD
******   CNSLOUT ADR=PDIRNA,LEN=8
*
         XC    PCCHHBI,PCCHHBI
         MVC   PCCHHBI(6),PDIRNA+2   CC START OF DIR
         MVI   PCCHHBIF,X'06'        Input Length
         MVC   PFUNC,=CL3'B2C'       BINARY TO CHAR
         BAL   R11,CALLUTI
         L     R10,12(R12)          4.Addr =
         MVC   0(12,R10),PCCHHAR
*-----------
******   CNSLOUT ADR=PLIBNA,LEN=8
         XC    PCCHHBI,PCCHHBI
         MVC   PCCHHBI(6),PLIBNA+2   CC START OF DIR
         MVI   PCCHHBIF,X'06'        Input Length
         MVC   PFUNC,=CL3'B2C'       BINARY TO CHAR
         BAL   R11,CALLUTI
         L     R10,16(R12)          5.Addr =
         MVC   0(12,R10),PCCHHAR
*--------------------------------
* LAST-ADDR C H R
*--------------------------------
*******  CNSLOUT ADR=PDIRLA,LEN=8
*
         XC    PCCHHBI,PCCHHBI
         MVC   PCCHHBI(6),PDIRLA+2   CC START OF DIR
         MVI   PCCHHBIF,X'06'         Input Length
         MVC   PFUNC,=CL3'B2C'       BINARY TO CHAR
         BAL   R11,CALLUTI
         L     R10,20(R12)          6.Addr =
         MVC   0(12,R10),PCCHHAR
*
*****    CNSLOUT ADR=PLIBLA,LEN=8
         XC    PCCHHBI,PCCHHBI
         MVC   PCCHHBI(6),PLIBLA+2   CC START OF DIR
         MVI   PCCHHBIF,X'06'         Input Length
         MVC   PFUNC,=CL3'B2C'       BINARY TO CHAR
         BAL   R11,CALLUTI
         L     R10,24(R12)          7.Addr =
         MVC   0(12,R10),PCCHHAR
*--------------------------------
* Active Dir-Entries
*--------------------------------
******   CNSLOUT ADR=PDIRAE,LEN=4
         MVC   WORKF,PDIRAE        ACTIVE DIR ENTRIES
         BAL   R11,CALCST00
         L     R10,28(R12)          8.Addr = M012AED ACTIVE DIR ENTR
         MVC   0(9,R10),RESULT+6
*--------------------------------
* Allocated Lib-Blks
*--------------------------------
******   CNSLOUT ADR=PLIBAL,LEN=4
         MVC   WORKF,PLIBAL         ALLOCATED LIBRARY BLOCKS
         BAL   R11,CALCST00
         L     R10,32(R12)          9.Addr = M012ALB ALLOC LIB BLKS
         MVC   0(9,R10),RESULT+6
*--------------------------------
* Active Lib-Blks
*--------------------------------
*****    CNSLOUT ADR=PLIBAC,LEN=4
         MVC   WORKF,PLIBAC         ACTIVE BLOCKS LIBRARY
         BAL   R11,CALCST00
         L     R10,36(R12)         10.Addr = M012ACB ACT BLKS LIB
         MVC   0(9,R10),RESULT+6
*--------------------------------
* Deleted Lib-Blks
*--------------------------------
******   CNSLOUT ADR=PLIBDL,LEN=4
         MVC   WORKF,PLIBDL         DELETED   LIBRARY BLOCKS
         BAL   R11,CALCST00
         L     R10,40(R12)         11.Addr = M012DLB DELET,LIB BLKS
         MVC   0(9,R10),RESULT+6
*--------------------------------
* Available Lib-Blks
*--------------------------------
******   CNSLOUT ADR=PLIBAV,LEN=4
         MVC   WORKF,PLIBAV         AVAILABLE LIBRARY BLOCKS
         BAL   R11,CALCST00
         L     R10,44(R12)         12.Addr = M012AVB AVAIL LIB BLKS
         MVC   0(9,R10),RESULT+6
*--------------------------------
* Condense Limit-Blks
*--------------------------------
******   CNSLOUT ADR=PLIBACL,LEN=2
         XC    WORKF,WORKF
         MVC   WORKH,PLIBACL        AUTOMATIC CONDENSE LIMIT
         BAL   R11,CALCST00
         L     R10,48(R12)         13.Addr = M012ACL AUTO; COND LIMIT
         MVC   0(9,R10),RESULT+6
*--------------------------------
* Allocated Cyls Lib+Dir
*--------------------------------
******   CNSLOUT ADR=PDLINCY,LEN=2
         XC    WORKF,WORKF
         MVC   WORKH,PDLINCY        NUMBER CYLS FOR DIR AND LIB
         BAL   R11,CALCST00
         L     R10,52(R12)         14.Addr = M012ACLD
         MVC   0(9,R10),RESULT+6
*--------------------------------
* Allocated Dir-Tracks
*--------------------------------
******   CNSLOUT ADR=PDIRTRKS,LEN=2
         XC    WORKF,WORKF
         MVC   WORKH,PDIRTRKS       TRACKS ALLOC FOR DIR
         BAL   R11,CALCST00
         L     R10,56(R12)         15.Addr = M012ADT ALLOC DIR TRKS
         MVC   0(9,R10),RESULT+6
         B     PGMEND
*----------------
CALCST00 DS    0H
*----------------
         MVC   RESULT,EDPAT
         L     R4,WORKF
         CVD   R4,DWORK
         OI    DWORK+7,X'0F'
         ED    RESULT,DWORK
         BR    R11                 RETURN TO CALLER
*----------------
DWORK    DS    D
SAVEST8  DS    5F
WORKF    DS    0F
         DS    H
WORKH    DS    H
EDPAT    DC    XL15'202020202020202020202020212020'
RESULT   DS    CL15
X00      DC    XL1'00'
*-------------------------------------------------------------------
CALLUTI  DS    0H
*-------------------------------------------------------------------
******   CNSLOUT MSG='CALLUTI'
         ST    R11,UTI11          SAVE R11
*
         MVC   PRETCOD,=CL3'000'
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*
         L     R15,VSUTIAD
         BALR  R14,R15
*                                   ----------------------------------- 00000720
*        VSUTI                      SOME UTILITY FUNCTIONS
         L     R11,UTI11            ----------------------------------- 00000720
         BR    R11                  RETURN TO CALLER
VSUTIAD  DC    V(VSUTI)
UTI11    DS    F
* --------------------------------------------------------------------* 00000065
*     PGMEND Program-ENd                                              * 00000066
* --------------------------------------------------------------------* 00000067
PGMEND   DS    0H
         L     R9,PARMADDR         LOAD PARAMETER-ADDR
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
* --------------------------------------------------------------------* 00000386
*        WORKFIELDS                                                   * 00000387
* --------------------------------------------------------------------* 00000388
*******  CNSLCCB
         DS    0D                                                       00000389
*                                                                       00000391
* --------------------------------------------------------------------* 00000404
*        MESSAGE TABLES                                               * 00000405
* --------------------------------------------------------------------* 00000406
         DS    0F                       ALIGNEMENT                      00000407
* --------------------------------------------------------------------* 00000369
*        SAVE AREA DECLARATIONS                                       * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F                                                       00000372
SAVEAREA DC    XL72'00'                 MONITOR TASK SAVE AREA           0000037
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS          00000379
PARMADDR DS    F                                                        00000458
PARMLIST DC    A(0)                     ADDRESS OF SPFPARMS
*                                                                       00000459
* --------------------------------------------------------------------* 00000424
         LTORG                                                          00000454
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                   * 00000456
* --------------------------------------------------------------------* 00000457
WORKA    DSECT                                                          00000475
         COPY  LIBPARMS                                                 00000460
         DS    0D                                                       00000427
* --------------------------------------------------------------------* 00000474
TABRECS  DSECT                                                          00000475
TABREC   DS    CL80                                                     00000476
* --------------------------------------------------------------------* 00000477
         DS    0F
MAPDS    DSECT
MAPADDR  DS    0CL2028
MAPNAME  DS    CL8
MAPEYEC  DS    CL8
         DS    CL2
MAPLEN   DS    CL2                MAP-LEN
MAPVCNT  DS    AL1(0)             VAR-FIELD COUNT
MAPVLST  DS    AL3(0)             ADDR MAP-VAR-OUTPUT-LIST
MAPSTART DS    CL2000
BUFFER   EQU   *                   MEMBER BUFFER BEGIN                  00001705
         END                                                            00000481
        BKEND
        CATALS   A.VSPUT,0.0
        BKEND   A.VSPUT
*------------------------------------------------------------------
*     NAME: VSPUT
*     TYPE: SUB-ROUTINE
* FUNCTION:
*          INITIATE a MAINT-CATALx in SSL, RLB, PLB
*
* INTERNAL
*
*
* EXTERNAL
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
         TITLE 'VSPUT - POWER UPDATE'
         PRINT GEN
VSPUT    CSECT
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
VSPUT   CSECT
         USING VSPUT,R15               ESTABLISH ADDRESSABILITY
         STM   R14,R12,12(R13)         SAVE REGS in CALLERS SA
         DROP  R15                     R5,R6,R7 PROGRAM BASE REGS
         USING VSPUT,R5,R6,R7
         LR    R5,R15                  FIRST BASE READY
         LR    R6,R15
         LA    R6,4095(R6)
         LA    R6,1(R6)                SECND BASE READY
         LR    R7,R6
         LA    R7,4095(R7)
         LA    R7,1(R7)                THIRD BASE READY
         B     GOON
         DC    C'***** VSPUT*****'     EYECATCHER
GOON     DS    0H
         ST    R13,NEWSAVE+4           SAVE OLD SA IN NEW SA
         LR    R10,R13
         LA    R13,NEWSAVE             LOAD NEW SA
         ST    R13,8(R10)              AND SAVE IT IN OLD SA
* -------------------------------------------
*    SETUP PARAMETERS FOR VSPUT             -
* -------------------------------------------
         L     R2,0(0,R1)              R2 IS MESSAGE CONTROL BLOCK
         USING WORKA,R2
         ST    R2,PARMADDR
*-----------------------------
         CLC   PFUNC,=CL3'PUT'         IS IT A POWER PUT COMMAND ?
         BNE   INVFUNC                 NO, INVALID FUNCTION PGMEND
*-----------------------------
         L     R9,PBUFADR              BUFFERADR
         ST    R9,TABRECSA
         L     R4,PNORL                # OF JOB RECORDS
         AH    R4,=H'15'               INCR FOR 10 JECL-CARDS
         MH    R4,=H'88'               LENGTH = NUMBER * BUFFER-LEN
         GETVIS ADDRESS=(1),LENGTH=(4)
         LTR   R15,R15
         BNZ   GVERROR
         ST    R1,POWBUFPT             SAVE POWER-BUFFER-TEST
         ST    R1,POWBUFAD             SAVE POWER-BUFFER-ADDR
         ST    R4,POWBUFLN             SAVE POWER-BUFFER-LEN
*---------------------------------------------------------------
*    POWER - UPDATE MEMBER           PUT PRE-JCL               *
*---------------------------------------------------------------
         CLI   PLIBNAME+1,C'S'          IS IT A SOURCE CATALS ?
         BNE   CRERELO                   NO
         MVC   DOSMFUNC(7),=CL7'CATALS ' YES
         MVC   DOSMNAME(1),PSLIB       SO, MOVE SUBLIB
         MVI   DOSMNAME+1,C'.'
         MVC   DOSMNAME+2(8),PMEMB     MEMBERNAME
         MVC   POWJOB+21(8),PMEMB      MEMBERNAME FOR POWER-JOBNAME
         MVC   DOSJOB+15(8),PMEMB      MEMBERNAME FOR DOSVS-JOBNAME
         LA    R4,9                    9 LEADING JCL-CARDS
         B     CRE000
CRERELO  DS    0H
         CLI   PLIBNAME+1,C'R'          IS IT A RELOCATABLE LIB ?
         BNE   CREPROC                 NO
         MVC   DOSMFUNC(7),=CL7'CATALR ' YES
         MVC   DOSMNAME(8),PMEMB       MEMBERNAME
         MVC   POWJOB+21(8),PMEMB      MEMBERNAME FOR POWER-JOBNAME
         MVC   DOSJOB+15(8),PMEMB      MEMBERNAME FOR DOSVS-JOBNAME
         LA    R4,8                    8 LEADING JCL-CARDS
         B     CRE000
CREPROC  DS    0H
         CLI   PLIBNAME+1,C'P'          IS IT A PROCEDURE LIB ?
         BNE   INVFUNC                 NO
         MVC   DOSMFUNC(7),=CL7'CATALP ' YES
         MVC   DOSMNAME(8),PMEMB       MEMBERNAME
         MVC   POWJOB+21(8),PMEMB      MEMBERNAME FOR POWER-JOBNAME
         MVC   DOSJOB+15(8),PMEMB      MEMBERNAME FOR DOSVS-JOBNAME
         LA    R4,8                    8 LEADING JCL-CARDS
         B     CRE000
CRE000   DS    0H
         L     R1,POWBUFAD             POWER-CARD OUTPUT
         LA    R9,POWJOB               1. POWER-JOB-CARD INPUT
CREJCLP  DS    0H
         LR    R15,R1                  CURRENT POWER CARD-POSITION
         LA    R15,88(R15)             NEXT POSITION
         MVC   0(88,R1),0(R9)
         ST    R15,0(R1)               SAVE NEXT POS IN PREV RECD
         LA    R1,88(R1)               NXT-JCL-CARD
         LA    R9,88(R9)               NXT-OUT-CARD
         BCT   R4,CREJCLP              LOOP
*                                      YES, LAST REC-ADR = 0
         ST    R1,POWBUFAD             SAVE NXT OUTPUT ADDR
*                                      YES, LAST REC-ADR = 0
*----------------------------------------------------------------
*      POWER                       PUT MEMBER IN BETWEEN
*----------------------------------------------------------------
         L     R9,PBUFADR              R9 = A(FIRST CARD) FROM BUFFER
         L     R1,POWBUFAD             R1 = A(FIRST POWER CARD)
         L     R4,PNORL                R4 = # OF RECORDS
CREMEM1  DS    0H
         LR    R15,R1                  R15 = A(CURRENT POWER CARD)
         LA    R15,88(R15)             ADDR OF NEXT POWER CARD
         MVC   8(80,R1),0(R9)          MOVE TO POWER WITH 8 OFFSET
         ST    R15,0(R1)               STORE IT IN PREVIOUS CARD
         LA    R9,80(R9)               POINT TO NEXT INPUT CARD
         LA    R1,88(R1)               POINT TO NEXT OUTPUT CARD
         BCT   R4,CREMEM1              ANY MORE CARDS ?
*                                      YES, LAST REC-ADR = 0
         ST    R1,POWBUFAD             SAVE NXT OUTPUT ADDR
*                                      YES, LAST REC-ADR = 0
*----------------------------------------------------------------
*      POWER                       PUT POST -JCL
*----------------------------------------------------------------
POWPUT2  DS    0H
         CLI   PLIBNAME+1,C'S'        IS IT A SOURCE LIB ?
         BNE   POWPUT21                NO
         L     R1,POWBUFAD             POWER-CARD
         LA    R9,DOSBK2               2. BKEND-CARD
         LA    R4,4                    4 TRAILING JCL-CARDS
         B     CREJCLS
POWPUT21 DS    0H
         L     R1,POWBUFAD             POWER-CARD
         LA    R9,DOSENDD              DOS-END-OF-DATA
         LA    R4,3                    3 TRAILING JCL-CARDS
CREJCLS  DS    0H
         LR    R15,R1                  CURRENT POWER CARD-POSITION
         LA    R15,88(R15)             NEXT POSITION
         MVC   0(88,R1),0(R9)
         ST    R15,0(R1)               SAVE NEXT POS IN PREV RECD
         LA    R1,88(R1)               NXT-JCL-CARD
         LA    R9,88(R9)               NXT-OUT-CARD
         BCT   R4,CREJCLS              LOOP
*                                      YES, LAST REC-ADR = 0
         S     R1,=F'88'               SUBTR
         XR    R4,R4
         ST    R4,0(R1)                LAST RECD WITH NEXT ADR 0
         L     R4,POWBUFPT             TEST FOR PDUMP
         LR    R9,R4                   ""
         LA    R9,1760(R9)             ""
*****    PDUMP (R4),(R9)               ""
POINTSPL DS    0H
         LA    R10,POINTSPL            RH RETURN-ADDR AFTER POWWAIT
*-------------------------
*---   CONNECT TO POWER/VS
*-------------------------
         LA    R12,PUTSPL              POINT TO PUTSPOOL SPL CONTROL
         USING SPLMAP,R12
         STCM  R12,7,ICRXECB+5         SAVE ADDRESS IN XECB
         XECBTAB TYPE=DEFINE,XECB=ICRXECB,ACCESS=XWAIT
         LTR   R15,R15                 ANY ERRORS
         BNZ   POWWAIT                 YES = GO ASSUME BUSY
*--------------------------
*---   EXECUTE POWER
*--------------------------
         L     R8,POWBUFPT             R8 = A(JCL)
         PUTSPOOL SPL=(R12),CBUF=(R8)  STUFF JOB INTO READER
*---------------------------
*---   DISCONN FROM POWER/VS
*---------------------------
         XECBTAB TYPE=DELETE,XECB=ICRXECB
         CLI   SPER,0                  ANY ERRORS
         BNE   POWERR                  YES - GO HANDLE
         MVC   PRETCOD,=CL3'000'
         B     POWEXIT                 NO - GO RETURN
POWERR   DS    0H                      ADD ANY ERROR HANDLING HERE
         MVC   PRETCOD,=CL3'999'        SET RC NOT OK
         B     POWEXIT
GVERROR  DS    0H                      ADD ANY ERROR HANDLING HERE
         MVC   PRETCOD,=CL3'885'       GETVIS-ERROR
         B     POWEXIT
POWWAIT  DS    0H
         SETIME 5,TECB                 WAIT 5 SECONDS AND TRY AGAIN
         WAIT  TECB
         BR    R10
POWEXIT  DS    0H
         L     R0,POWBUFLN
         L     R1,POWBUFPT
         FREEVIS ADDRESS=(1),LENGTH=(0)
         LTR   R15,R15
         BZ    FVOK
         MVC   PRETCOD,=CL3'894'       FREEEVIS-ERROR
         B     END001
FVOK     DS    0H                      ADD ANY ERROR HANDLING HERE
         MVC   PRETCOD,=CL3'000'       GETVIS-ERROR
END001   DS    0H
         CLC   PRETCOD,=CL3'000'
         BNE   ENDPUT00
         MVC   PRETCOD,=CL3'888'      MEMBER SAVED
         B     PGMEND
ENDPUT00 DS    0H
         MVC   PRETCOD,=CL3'889'      MEMBER NOT SAVED  ERROR
         B     PGMEND
INVFUNC  DS    0H
         MVC   PRETCOD,=CL3'883'      FALSCHE FUNKTION MODUL VSPUT
         B     PGMEND
PGMEND   DS    0H
******   BAL   R8,CALLERR
*******  BAL   R8,CALLWAIT
         L     R1,PARMADDR         LOAD PARAMETER-ADDR
         MVC   0(L'PARMS,R1),PARMS
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
*-------------------------------------------------------------------
CALLERR  DS    0H
*-------------------------------------------------------------------
*****    CNSLOUT MSG='CALLERR'
         ST    R8,ERRR8          SAVE R8
*
         LA    R1,PARMS
         ST    R1,PARMLIST
         LA    R1,PARMLIST       CALL VSERROR -MAP-PROGRAM
         L     R15,VSERRAD
         BALR  R14,R15
*
         L     R8,ERRR8            -----------------------------------
         BR    R8                     RETURN TO CALLER
*-----------------------            --------------------------------
VSERRAD  DC    V(VSERROR)
ERRR8    DS    F
READMSW  DC    XL1'00'
*-------------------------------------------------------------------
CALLWAIT DS    0H
*-------------------------------------------------------------------
*****    CNSLOUT MSG='CALLWAIT'
         ST    R8,WAIT8          SAVE R8
*
         LA    R1,PARMS
         ST    R1,PARMLIST
         LA    R1,PARMLIST       CALL VSERROR -MAP-PROGRAM
         L     R15,VSWAITAD
         BALR  R14,R15
*
         L     R8,WAIT8            -----------------------------------
         BR    R8                     RETURN TO CALLER
*-----------------------            --------------------------------
VSWAITAD DC    V(VSWAIT)
WAIT8    DS    F
* --------------------------------------------------------------------* 00000369
*        POWER JOB CARD DEFINITIONS                                   * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F
POWJOB   DS    0CL88                                                    00000372
         DC    A(0),A(0),CL80'* $$ JOB JNM=Y       ,CLASS=0'
POWLST   DS    0CL88
         DC    A(0),A(0),CL80'* $$ LST CLASS=P,DISP=D,JSEP=1'
DOSJOB   DS    0CL88
         DC    A(0),A(0),CL80'// JOB Y       '
DOSDLBL  DS    0CL88
         DC    A(0),A(0),CL80'// DLBL IJSYSSL,''SPF.DOSR34.SOURCE'''
***      DC    A(0),A(0),CL80'// DLBL IJSYSSL,''DOSVS.OPTIONAL.DOSVS'''
DOSEXT   DS    0CL88
         DC    A(0),A(0),CL80'// EXTENT SYSSLB,OPTDOS'
***      DC    A(0),A(0),CL80'// EXTENT SYSSLB,OPTLB1'
DOSASS   DS    0CL88
         DC    A(0),A(0),CL80'// ASSGN SYSSLB,DISK,VOL=OPTDOS,SHR'
***      DC    A(0),A(0),CL80'// ASSGN SYSSLB,DISK,VOL=OPTLB1,SHR'
DOSEXE   DS    0CL88
         DC    A(0),A(0),CL80'// EXEC MAINT'
*
DOSMF    DS    0CL88
         DC    A(0)
         DC    A(0)
         DC    CL3'   '
DOSMFUNC DC    CL7'       '    CATALX
DOSMNAME DC    CL70' '
*
DOSBK1   DS    0CL88
         DC    A(0),A(0),CL80'   BKEND  '
DOSBK2   DS    0CL88
         DC    A(0),A(0),CL80'   BKEND  '
DOSENDD  DS    0CL88
         DC    A(0),A(0),CL80'/*'
DOSENDJ  DS    0CL88
         DC    A(0),A(0),CL80'/&&'
POWENDJ  DS    0CL88
         DC    A(0),A(0),CL80'* $$ EOJ'
* --------------------------------------------------------------------* 00000369
*        SAVE AREA DECLARATIONS                                       * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F                                                       00000372
NEWSAVE  DC    18F'0'                   MONITOR TASK SAVE AREA           0000037
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS          00000379
LEN1     DC    F'0'                     TEMP FIELD                      00000383
         SPACE 2
         LTORG
ICRXECB  DC    A(0,0)
TECB     TECB
PUTSPL   SPL   TYPE=DEFINE,PBUF=FEEDBACK,PBUFL=88
FEEDBACK DS    0CL88
         DC    CL28' '
FEEDB028 DC    CL16' '
         DC    CL16' '
         DC    CL16' '
         DC    CL12' '
         SPACE 2
POWBUFAD DC    F'0'
POWBUFLN DC    F'0'
POWBUFPT DC    F'0'    RH Test
PARMLIST DC    A(0)                BASE-ADDRESS OF SPFPARMS-DSECT
         SPACE 2
*****    CNSLCCB
         SPACE 2
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                   * 00000456
* --------------------------------------------------------------------* 00000457
PARMADDR DS    F                                                        00000458
* --------------------------------------------------------------------* 00000424
         LTORG                                                          00000454
* --------------------------------------------------------------------* 00000426
SPLMAP   SPL   TYPE=MAP
WORKA    DSECT
         COPY LIBPARMS                                                  00000459
         END
        BKEND
        CATALS   A.VSRCSBA,0.0
        BKEND   A.VSRCSBA
*------------------------------------------------------------------
*     NAME: VSRCSBA
*     TYPE: SUB-ROUTINE
* FUNCTION: Calculate SBA-AQdr from row/ cow and vice versa
*
* INTERNAL
*
*
* EXTERNAL
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
VSRCSBA  CSECT                                                          00000016
         USING *,R15                                                    00000017
         STM   R14,R12,12(R13)         STORE REGS IN SAVEAREA           00000018
         DROP  R15                     R15 TO BE USED BY MACROS         00000019
         USING VSRCSBA,R11,R12         R11 NOW BASE REGISTER            00000020
         LR    R11,R15                 ESTABLISH ADDRESSABILITY         00000021
         LA    R12,4095(R11)                                            00000022
         LA    R12,1(R12)              SECOND BASE                      00000023
         B     GOON
         DC    C'***** VSRCSBA***'     EYECATCHER
GOON     DS    0H
         ST    R13,SAVEAREA+4          STORE MAIN PGM SAVEAREA          00000024
         LR    R10,R13                 SAVE R13                         00000025
         LA    R13,SAVEAREA            SAVE AREA FOR THIS TASK          00000026
         ST    R13,8(R10)              STORE EXIT SAVE ADDRESS          00000027
* --------------------------------------------------------------------* 00000028
*        STATEMENTS FOR SET UP FIELDS NEEDED FOR VSRCSBA              * 00000029
* --------------------------------------------------------------------* 00000030
         L     R2,0(R1)                PARMS ADDRESS                    00000031
         ST    R2,VISADR               SAVE                             00000032
         USING WORKA,R2
         L     R9,PBUFADR              LOAD DATATAB ADDRESS             00000034
         ST    R9,TABRECSA             SET  TABRECS ADDRESS             00000035
         USING TABRECS,R9              ESTABLISH ADDRESSABILITY         00000036
         CLC   PFUNC,=CL3'SRC'         SBA TO ROW / COLUM
         BE    SRC000
         CLC   PFUNC,=CL3'RCS'         ROW / COLUM  TO SBA
         BE    SRC000
         CLC   PFUNC,=CL3'BRC'         BUFFER TO ROW / COLUM
         BE    SRC000
         MVC   PRETCOD,=CL3'991'        ERROR: INVALID FUNCTION
* --------------------------------------------------------------------* 00000065
*     PGMEND Program-ENd                                              * 00000066
* --------------------------------------------------------------------* 00000067
PGMEND   DS    0H
         L     R9,VISADR           LOAD PARAMETER-ADDR
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
* --------------------------------------------------------------------* 00000065
*     SRC - SBA TO ROW / COLUMN CALC                                  * 00000066
* --------------------------------------------------------------------* 00000067
SRC000   DS    0H
         MVC   SBAWORK,PSBAWORK
         TR    SBAWORK,EBCTOBIN    ECBDIC TO BIN
         NC    SBAWORK,=X'3F3F'    BIT 15 AND BIT 7 OFF
         SR    R4,R4
         SR    R5,R5
         IC    R4,SBAWORK
         IC    R5,SBAWORK+1
         SLL   R4,6
         AR    R5,R4
         SR    R4,R4
         D     R4,=F'80'
         LA    R4,1(R4)
         LA    R5,1(R5)
         STC   R5,PROW           1-43    1 => ROW 1
         STC   R4,PCOL           1-80    2 => COLUMN 2
         B     PGMEND              RETURN
* --------------------------------------------------------------------* 00000065
*     RCS - ROW / COLUMN TO SBA CALC                                  * 00000066
* --------------------------------------------------------------------* 00000067
RCS000   DS    0H
         XR    R0,R0
         XR    R1,R1
         IC    R0,PROW
         IC    R1,PCOL
         BCTR  R0,0
         BCTR  R1,0
         MH    R0,=H'80'
         AR    R1,R0
         SLL   R1,2
         STCM  R1,2,SBAWORK
         SRL   R1,2
         STCM  R1,2,SBAWORK+1
         NC    SBAWORK,=XL2'3F3F'
         TR    SBAWORK,BINTOEBC
         MVC   PSBAWORK,SBAWORK
         B     PGMEND              RETURN
* --------------------------------------------------------------------* 00000065
*     BRC - BUFFER TO ROW / COLUMN CALC                               * 00000066
* --------------------------------------------------------------------* 00000067
BRC000   DS    0H
         MVC   SBAWORK,PSBAWORK
         LH    R0,SBAWORK
         CH    R0,=H'4096'
         BH    BRC010            ERROR: BUFFER-ADDR TOO LARGE
*
         STC   R0,SBAWORK+1
         NI    SBAWORK+1,X'3F'
         SRL   R0,6
         STC   R0,SBAWORK
         TR    SBAWORK,BINTOEBC
         MVC   PROW(2),SBAWORK
         B     PGMEND              RETURN
BRC010   DS    0H
         MVC   PRETCOD,=CL3'990'  ERROR: BUFFER-ADDR TOO LARGE
         B     PGMEND
* --------------------------------------------------------------------* 00000386
******   CNSLCCB
* --------------------------------------------------------------------* 00000386
*        WORKFIELDS                                                   * 00000387
* --------------------------------------------------------------------* 00000388
         DS    0D                                                       00000389
*                                                                       00000391
* --------------------------------------------------------------------* 00000404
*        MESSAGE TABLES                                               * 00000405
* --------------------------------------------------------------------* 00000406
         DS    0F                       ALIGNEMENT                      00000407
* --------------------------------------------------------------------* 00000369
*        SAVE AREA DECLARATIONS                                       * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F                                                       00000372
SAV8     DS    2F
SAVEAREA DC    XL72'00'                 MONITOR TASK SAVE AREA           0000037
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS          00000379
VISADR   DS    F                                                        00000458
SBAWORK  DS    CL2                                                      00000458
*                                                                       00000459
EBCTOBIN DC    256AL1(*-EBCTOBIN)                                       00001546
         ORG   EBCTOBIN+X'40'                                           00001547
         DC    X'C0'                                                    00001548
         ORG   EBCTOBIN+X'4A'                                           00001549
         DC    X'CACBCCCDCECF'                                          00001550
         ORG   EBCTOBIN+X'50'                                           00001551
         DC    X'D0'                                                    00001552
         ORG   EBCTOBIN+X'5A'                                           00001553
         DC    X'DADBDCDDDEDF'                                          00001554
         ORG   EBCTOBIN+X'60'                                           00001555
         DC    X'E0E1'                                                  00001556
         ORG   EBCTOBIN+X'6A'                                           00001557
         DC    X'EAEBECEDEEEF'                                          00001558
         ORG   EBCTOBIN+X'7A'                                           00001559
         DC    X'FAFBFCFDFEFF'                                          00001560
         ORG                                                            00001561
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        00001563
BINTOEBC DC    X'40C1C2C3C4C5C6C7C8C94A4B4C4D4E4F'  0                   00001564
         DC    X'50D1D2D3D4D5D6D7D8D95A5B5C5D5E5F'  1                   00001565
         DC    X'6061E2E3E4E5E6E7E8E96A6B6C6D6E6F'  2                   00001566
         DC    X'F0F1F2F3F4F5F6F7F8F97A7B7C7D7E7F'  3                   00001567
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                   * 00000456
* --------------------------------------------------------------------* 00000015
*        REGISTER EQUATES                                             * 00000351
* --------------------------------------------------------------------* 00000352
R0       EQU   0                                                        00000353
R1       EQU   1                                                        00000354
R2       EQU   2                                                        00000355
R3       EQU   3                                                        00000356
R4       EQU   4                                                        00000357
R5       EQU   5                                                        00000358
R6       EQU   6                                                        00000359
R7       EQU   7                                                        00000360
R8       EQU   8                                                        00000361
R9       EQU   9                                                        00000362
R10      EQU   10                                                       00000363
R11      EQU   11                                                       00000364
R12      EQU   12                                                       00000365
R13      EQU   13                                                       00000366
R14      EQU   14                                                       00000367
R15      EQU   15                                                       00000368
* --------------------------------------------------------------------* 00000424
         LTORG                                                          00000454
* --------------------------------------------------------------------* 00000426
         DS    0D                                                       00000427
* --------------------------------------------------------------------* 00000474
WORKA    DSECT
         COPY  LIBPARMS                                                 00000460
TABRECS  DSECT                                                          00000475
TABREC   DS    CL80                                                     00000476
*                                                                       00001545
*                                                                       00001562
*                                                                       00001562
* --------------------------------------------------------------------* 00000477
         END                                                            00000481
        BKEND
        CATALS   A.VSRDIR,0.0
        BKEND   A.VSRDIR
*------------------------------------------------------------------
*     NAME: VSRDIR
*     TYPE: SUB-ROUTINE
* FUNCTION: READ DIRECTORIES RLB SLB PLB
*
* INTERNAL
*
*
* EXTERNAL
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:   DTFCP
*
*
*
*
*------------------------------------------------------------------
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
VSRDIR   CSECT
         STM   R14,R12,12(R13)
         USING VSRDIR,R11,R12
         LR    R11,R15
         B     GOON
         DC    C'**** VSRDIR ****'     EYECATCHER
GOON     DS    0H
         LA    R12,4095(R11)
         LA    R12,1(R12)
         ST    R13,SAVEAREA+4
         LR    R10,R13
         LA    R13,SAVEAREA
         ST    R13,8(R10)
*--------------------------------------------------------------
*      STATEMENT FOR SETUP VSRDIR
*--------------------------------------------------------------
         L     R2,0(R1)
         ST    R2,PARMADDR
         USING WORKA,R2
         L     R9,PBUFADR
         ST    R9,TABRECSA
         USING TABRECS,R9
*--------------------------------------------------------------
*      STATEMENT FOR SETUP VSRDIR
*--------------------------------------------------------------
         L     R9,PBUFADR              LOAD DATATAB ADDRESS
* RSSETSLB MVI   RSCBSYM,=X'06'        SLB
* RSSOURCE MVC   RSSEEKCC(FIVE),=XL5'01A3000001'
******   B     DIR070
******   CNSLOUT MSG='PGM-START'
         CLC   PFUNC(2),=CL2'SC'       SYSTEM CORE IMAGE LIB ?
         BNE   DIR010
*****    MVC   PRETCOD,=CL3'902'  RH   NOT YET IMPLEMENTED
*****    B     PGMEND             RH
         MVC   RSSEEKCC(FIVE),=XL5'0000000201'
         MVI   RSCBSYM,X'06'           LOGICAL UNIT TO CCB
         MVC   PLIBNAME,=CL2'SC'
         B     DIR070
DIR010   DS    0H
         CLC   PFUNC(2),=CL2'SR'       SYSTEM RELOCATABLE LIB ?
         BNE   DIR020
*******  CNSLOUT ADR=PSRLB,LEN=6
         MVC   RSSEEKCC(5),PSRLB       CCHHR
         MVI   RSSEEKCC+4,X'01'
         MVI   RSCBSYM,X'06'           LOGICAL UNIT TO CCB
         MVC   PLIBNAME,=CL2'SR'
         B     DIR070
DIR020   DS    0H
         CLC   PFUNC(2),=CL2'SS'       SYSTEM SOURCE STMT LIB ?
         BNE   DIR030
         MVC   RSSEEKCC(5),PSSLB       CCHHR
         MVI   RSSEEKCC+4,X'01'
*******  CNSLOUT ADR=PSSLB,LEN=6
         MVI   RSCBSYM,X'06'           LOGICAL UNIT TO CCB
         MVC   PLIBNAME,=CL2'SS'
         B     DIR070
DIR030   DS    0H
         CLC   PFUNC(2),=CL2'SP'       SYSTEM PROCEDURE LIB ?
         BNE   DIR040
         MVC   RSSEEKCC(5),PSPLB       CCHHR
         MVI   RSSEEKCC+4,X'01'
*******  CNSLOUT ADR=PSPLB,LEN=6
         MVI   RSCBSYM,X'06'           LOGICAL UNIT TO CCB
         MVC   PLIBNAME,=CL2'SP'
         B     DIR070
DIR040   DS    0H
         CLC   PFUNC(2),=CL2'PC'       PRIVATE CORE IMAGE LIB ?
         BNE   DIR050
******   MVC   PRETCOD,=CL3'902'  RH   NOT YET IMPLEMENTED
******   B     PGMEND             RH
         MVC   RSSEEKCC(5),PPCLB       CCHHR
         MVI   RSSEEKCC+4,X'01'
****     CNSLOUT ADR=PPCLB,LEN=6
         MVI   RSCBSYM,X'0B'           LOGICAL UNIT TO CCB
         MVC   PLIBNAME,=CL2'PC'
         B     DIR070
DIR050   DS    0H
         CLC   PFUNC(2),=CL2'PR'       PRIVATE RELOCATABLE LIB ?
         BNE   DIR060
         MVC   RSSEEKCC(5),PPRLB       CCHHR
         MVI   RSSEEKCC+4,X'01'
         MVI   RSCBSYM,X'08'           LOGICAL UNIT TO CCB
         MVC   PLIBNAME,=CL2'PR'
         B     DIR070
DIR060   DS    0H
         CLC   PFUNC(2),=CL2'PS'       PRIVATE SOURCE STMT LIB ?
         BNE   DIR069
         MVC   RSSEEKCC(5),PPSLB       CCHHR
         MVI   RSSEEKCC+4,X'01'
******   CNSLOUT ADR=PPSLB,LEN=6
         MVI   RSCBSYM,X'07'           LOGICAL UNIT TO CCB
         MVC   PLIBNAME,=CL2'PS'
         B     DIR070
DIR069   DS    0H
         MVC   PRETCOD,=CL3'881'       INVALID FUNCTION IN VSRDIR
         B     PGMEND
DIR070   DS    0H
******   LA    R7,PDIRAREA             ADDR FOR DIR-INFOS
         SR    R5,R5
RSFULL0  EQU   *                                                     VB 66900528
         CLI   PFUNC+1,C'R'     RH     IS IT A SYSTEM OR PRIV. RLB ?
         BNE   RSFULL10         RH     # 20 OF RECORDS IN A BLOCK
         MVI   RSRECDS1,TWENTY
         B     RSFULL
RSFULL10 MVI   RSRECDS1,TEN            NO. OF RECORDS IN SOURCE BLOCK   66930025
RSFULL   LH    R4,RSRECDS              NO. OF RECDS IN A BLOCK          66960025
         SH    R4,RSFIVE               NO. OF RECORDS IN 1ST BLOCK      66990025
         LA    R3,RSAREA+EIGHTY        ADDR OF 6TH RECD IN 1ST BLOCK    67020025
*                                                                       67050025
RSREAD   DS    0H                      READ LAST BLOCK PROCESSED        67230025
         BAL   R10,RSEXCP              READ LAST BLOCK PROCESSED        67230025
         CLI   PFUNC+1,C'C'            IS IT A SYSTEM OR PRIV CLB ?
         BE    PCLB00                  YES
         MVC   PDIRAREA,RSAREA         NO, SAVE DIR INFOS IN PARMS
         B     RSCOMP
*                                                                       67290025
RSREAD1  BAL   R10,RSEXCP              READ A DIRECTORY BLOCK           67320025
         LH    R4,RSRECDS              10/20 RECORDS PER BLOCK          67350025
         LA    R3,RSAREA               ADDR OF READIN AREA              67380025
RSCOMP   DS    0H                                                       67410025
         CLI   ZERO(R3),ASTER          THIS THE END OF THE DIR.         67410025
         BE    SORT             RH     YES                              67440025
         CLC   ZERO(2,R3),=XL2'FFFF'   THIS THE END OF THE DIR.         67410025
         BE    SORT                    YES                              67440025
         CLC   ZERO(4,R3),=XL4'00000000'  THE END OF THE DIR PLB
         BE    SORT                    YES                              67440025
         CLC   ZERO(SIXTEEN,R3),LASTREC  IF ENC OF LAST PASS   @DA01108 67450029
         BE    RSNEXT                  SKIP THIS ENTRY         @DA01108 67460029
         CLI   ZERO(R3),C' '  BLANK    IS THIS A DELETED ENTRY          67470025
         BNE   RSMOVE1                 NO, MOVE TO SORT AREA            67500025
RSNEXT   DS    0H                                                       67530025
         LA    R3,SIXTEEN(R3)          BUMP TO NEXT ENTRY IN BLOCK      67530025
         BCT   R4,RSCOMP               CONTINUE UNTIL BLOCK IS DONE     67560025
         MVC   RSSEEKCC,RSCOUNT        GET NEXT RECORD ADDR             67590025
******   CNSLOUT ADR=RSSEEKCC,LEN=18   FOR TEST PURPOSE ONLY
         B     RSREAD1                 GO TO READ SAME                  67620025
*                                                                       67680025
RSMOVE1  DS    0H                                                       67740025
         MVC   LASTREC(SIXTEEN),ZERO(R3) SAVE REC FOR COMPARE
         BAL   R10,PREPRECD            PREPARE RECORD FOR DISPLAY
         MVC   0(34,R9),CRECD
         LA    R9,34(R9)
         LA    R5,ONE(R5)              INCREMENT RECORD COUNTER         67860025
         C     R5,PENDBUF  CR R7 RH    IS SORT AREA FULL WITH RECORDS   67890025
         BL    RSNEXT                  NO, GET NEXT RECORD              67920025
         B     SORT
*--------------------------------------------------------------------
PCLB00   DS    0H
         ST    R10,PCLB10
         BAL   R10,PREPCLB             YES, PREP CLB-INFO FOR OUTPUT    67260025
         MVC   CLBLKL,RSAREA           SAVE BLK-LENGTH OF 1.DIR-ENTRY
         LA    R3,RSAREA               INPUT AREA
         AH    R3,CLBLKL               ADD BLKLEN
         ST    R3,CLBLKEND             BLK-END-ADDR
         LA    R3,RSAREA+2             START DATA-ADDR
         BAL   R10,CLBENTL             GO, CALCULATE ENTRY LENGTH
         AH    R3,CLENTRYL             FIRST-PHASE-ADDR IN 1.ENTRY
         B     RSCL002
*                                                                       67290025
RSCLREAD BAL   R10,RSEXCP              READ A DIRECTORY BLOCK           67320025
         MVC   CLBLKL,RSAREA           SAVE BLK-LENGTH OF 1.DIR-ENTRY
         LA    R3,RSAREA               INPUT AREA
         AH    R3,CLBLKL               ADD BLKLEN
         ST    R3,CLBLKEND             BLK-END-ADDR
         LA    R3,RSAREA+2             START DATA-ADDR
RSCL002  DS    0H                                                       67410025
         BAL   R10,CLBENTL             GO, CALCULATE ENTRY LENGTH
         CLC   ZERO(2,R3),=XL2'FFFF'   THIS THE END OF THE DIR.         67410025
         BE    SORT                    YES                              67440025
         CLC   ZERO(SIXTEEN,R3),LASTREC  IF ENC OF LAST PASS   @DA01108 67450029
         BE    RSCLNEXT                SKIP THIS ENTRY         @DA01108 67460029
         CLI   ZERO(R3),C' '  BLANK    IS THIS A DELETED ENTRY          67470025
         BNE   RSCLMOVE                NO, MOVE TO SORT AREA            67500025
RSCLNEXT DS    0H                                                       67530025
         AH    R3,CLENTRYL
         C     R3,CLBLKEND             IS IT BLKEND ?                   67560025
         BL    RSCL002                 NO, CONTINUE
         MVC   RSSEEKCC,RSCOUNT        YES,PREP FOR NEXT RECORD ADDR
******   CNSLOUT ADR=RSSEEKCC,LEN=18   FOR TEST PURPOSE ONLY
         B     RSCLREAD                GO TO READ SAME                  67620025
*                                                                       67680025
RSCLMOVE DS    0H                                                       67740025
         MVC   LASTREC(SIXTEEN),ZERO(R3) SAVE REC FOR COMPARE
         BAL   R10,PREPRECD            PREPARE RECORD FOR DISPLAY
         MVC   0(34,R9),CRECD
         LA    R9,34(R9)
         LA    R5,ONE(R5)              INCREMENT RECORD COUNTER         67860025
         C     R5,PENDBUF  CR R7 RH    IS SORT AREA FULL WITH RECORDS   67890025
         BL    RSCLNEXT                NO, GET NEXT RECORD              67920025
         B     SORT
         L     R10,PCLB10
         BR    R10
PCLB10   DS    F
CLBLKEND DS    F
*--------------------------------------------------------------------
CLBENTL  DS    0H                      CALCULATE ENTRY LENGTH
         ST    R10,CLB10
         XR    R4,R4
         IC    R4,11(R3)               # OF HALFWORDS FOLLOWING
         LA    R4,12(R4,R4)            12 = CONST DISPLMNT
         STH   R4,CLENTRYL
         L     R10,CLB10
         BR    R10                     RETURN TO CALLER
CLB10    DS    F
CLBLKL   DS    H
CLENTRYL DS    H
*--------------------------------------------------------------------
PREPCLB  DS    0H                PREPARE INFOS FOR CLB  SEE $LIBSTAT
         STM   R6,R10,SAVTO10                           FOR HELP
         XC    PDIRAREA,PDIRAREA
         MVC   CLDES,RSAREA            SAVE CLB DECRRIPTOR ENTRY
*******  PDUMP CLDES,CLRES
*                                ALLOCATED = USED+DEL+AVAIL.
         MVC   ACTIVE,CLLU            USED/ACTIVE LIB.BLKS
         MVC   PLIBAC,CLLU            ACTIVE LIB.BLKS
         L     R10,ACTIVE
         MVC   DELETED,CLLD            DELETED LIB-BLKS
         MVC   PLIBDL,CLLD             DELETED LIB-BLKS
         AL    R10,DELETED
         MVC   AVAILAB,CLLA            AVAILABLE LIB-BLKS
         MVC   PLIBAV,CLLA             AVAILABLE LIB-BLKS
         AL    R10,AVAILAB
         ST    R10,WORDWRK             SAVE ALLOCATED LIBR-BLKS
         MVC   PLIBAL,WORDWRK          SAVE ALLOCATED LIBR-BLKS
*                                START-ADDR OF CLB-DIECTORY
         CLI   PFUNC,C'S'              SYSTEM OR PRIVATE CLB ?
         BNE   PREPCL10
         MVC   PDIRSA+2(4),PSCLB       BBCCHH SYSTEM-CLB STATUSTABLE
         MVI   PDIRSA+6,X'01'          R = 1 (DIRSTRTR = 1)
******   CNSLOUT ADR=PSCLB,LEN=6
******   CNSLOUT ADR=PDIRSA,LEN=7
         B     PREPCL15
*-----------------------------------------------------------------
*--------------------------- DIRECTORY ---------------------------
*-----------------------------------------------------------------
*--------------------------- DIR START ALL -----------------------
PREPCL10 DS    0H
         MVC   PDIRSA+2(4),PPCLB       BBCCHH PRIVATE-CLB STATUSTABLE
         MVI   PDIRSA+6,X'01'          R=1 (DIRSTRTR = 1)F
*******  CNSLOUT ADR=PPCLB,LEN=6
*******  CNSLOUT ADR=PDIRSA,LEN=7
*                                START-ADDR OF CLB-DIECTORY
*--------------------------- DIR NEXT ----------------------------
PREPCL15 DS    0H
*                                REST=(DIRSTARTR+USED)/CLDB (BLK/TR)
         LA    R10,1                   DIR STRT REC
         LH    R7,CLDB                 DIR-BLKS PER TRACK
         AH    R10,CLDU                DIR-BLKS USED / ACTIVE
         LR    R0,R10                  RELOAD FOR DIVIDE
         SRDA  R0,32                   DIVIDEND NOW IN R1
         DR    R0,R7                   DIVIDE BY DIR-BLKS PER TRACKS
         ST    R0,REST                 SAVE REST FROM R0
*
*                                QUOT=(DIRSTARTR+USED)/CLDB (BLK/TRK)
         LR    R0,R10                  RELOAD FOR DIVIDE
         SRDA  R0,32                   DIVIDEND NOW IN R1
         DR    R0,R7                   DIVIDE BY DIR-BLKS PER TRACKS
         ST    R1,QUOT                 SAVE QUOT FROM R1
*                                IF REST = 0 THEN REST=CLDB
*                                                 QUOT=QUOT-1
         L     R6,REST
         LTR   R6,R6                   REST = 0 ?
         BNZ   PREPCL20                NO        BRANCH RF00154
         LR    R6,R7                   REST=DIR-BLKS PER TRACK
         BCTR  R1,R0                   QUOT=QUOT - 1
         ST    R1,QUOT                 SAVE QUOT FROM R1
PREPCL20 DS    0H                      RF00154
******   CNSLOUT ADR=QUOT,LEN=4
******   CNSLOUT ADR=REST,LEN=4
******   CNSLOUT REG=6
*----------------------------- DIR NEXT  RECD -----------------
         STC   R6,PDIRNA+6             DIRNXTR = REST
*                                DIRNXTH=(DIRSTRTH+QUOT)/(TRK/CYL)
         LH    R10,CLTC                TRACKS PER CYL
         MVC   WORDWRK(2),PDIRSA+4     FOR ALIGNMENT
         LH    R6,WORDWRK              LOAD DIRSTRTHH
         L     R7,QUOT
         ALR   R7,R6                   ADD DIR-START-HH
         LR    R0,R7                   RELOAD FOR DIVIDE
         SRDA  R0,32
         DR    R0,R10                  DIVIDE BY TRACKS PER CYL
*----------------------------- DIR NEXT HH --------------------
         STH   R0,WORDWRK
         MVC   PDIRNA+4(2),WORDWRK     DIR-NEXT-HH
*                                 QUOT=(DIRSTRTC+QUOT)/TRKS PER CYP
         LR    R0,R7                   RELOAD FOR DIVIDE
         SRDA  R0,32
         DR    R0,R10                  DIVIDE BY TRACKS PER CYL
         ST    R1,QUOT                 SAVE QUOT FROM R1
*                                 DIRNXTC=DIRSTRTC+QUOT
         MVC   WORDWRK(2),PDIRSA+2     FOR ALIGNMENT
         LH    R7,WORDWRK              LOAD DIRSTRTHH
         L     R15,QUOT
         ALR   R15,R7                  ADD DIR START CYL
         STH   R15,WORDWRK
*----------------------------- DIR NEXT CYL -------------------
         MVC   PDIRNA+2(2),WORDWRK    DIR NEXT CYL
******   CNSLOUT ADR=PDIRNA,LEN=8
*--------------------------- DIR LAST ----------------------------
*                                 DIRLASTR=CLDB (DIR-BLKS PER TRK)
         MVC   PDIRLA+6(1),CLDB+1      DIR LAST RECD
*                                 DIRLASTH=(DIRSTRTH+DIR-TRKS-1)/DESTC
         MVC   WORDWRK(2),PDIRSA+4     FOR ALIGNMENT
         LH    R6,WORDWRK              LOAD DIRSTRTHH
         AH    R6,CLDT                 # OF DIR-TRACKS
         BCTR  R6,R0                   -1
         LR    R0,R6                   RELOAD FOR DIVIDE
         SRDA  R0,32
         DR    R0,R10                  DIVIDE BY TRACKS PER CYL
         ST    R1,WORDWRK              SAVE RESULT FROM R1
         MVC   PDIRLA+4(2),WORDWRK+2   DIR LAST HH
*                                 QUOT=(DIRSTRH+CLDT-1)/CLTC
         LR    R8,R6
         SRDA  R8,32
         DR    R8,R10                  TRKS PER CYL
         ST    R9,QUOT
*                                 DIRLASTC=DIRSTRC+QUOT
         A     R7,QUOT            DIRSTRTHH+QUOT
         ST    R7,WORDWRK
         MVC   PDIRLA+2(2),WORDWRK+2    DIR LAST CYL
******   CNSLOUT ADR=PDIRLA,LEN=8
*---------------------------------------------------------------
*                                 LIBRARY
*---------------------------------------------------------------
*---------------------------------LIB START RECD ---------------
         MVI   PLIBSA+6,X'01'          LIB START RECD (MUST BE 1)
*                             IF DIR-LAST-HH = (TRKS PER CYL)-1
*                             THEN LIBSTRTH = 0
         BCTR  R10,R0
         CR    R0,R10
         BNE   PREPCL30        RF00168
         SLR   R10,R10
         STH   R10,WORDWRK
*---------------------------------LIB START HH -----------------
         MVC   PLIBSA+4(2),WORDWRK     LIB START HH
*                                      LIBSTRTC=DIRLSTC+1
         LA    R7,1(R7)
         STH   R7,WORDWRK
         MVC   PLIBSA+2(2),WORDWRK     LIB START CC
         B     PREPCL35            RC00168
*
PREPCL30 DS    0H              RF00168
*                              ELSE    LIBSTRTH=DIRLSTH+1
         MVC   WORDWRK(2),PDIRLA+4
         LH    R10,WORDWRK
         LA    R10,1(R10)
         STH   R10,WORDWRK
         MVC   PLIBSA+4(2),WORDWRK      LIB START HH
*---------------------------------LIB START CC -----------------
         MVC   PLIBSA+2(2),PDIRLA+2     LIBSTRTC=DIRLASTC
*
PREPCL35 DS    0H   RF00168    REST=(LIBSTRTR+ACTIVE+DELETED)/CLLT
*                                       CLLT=LIB-BLKS PER TRACK
*
         LH    R10,CLLT         LIB-BLKS PER TRACK
         XR    R6,R6
         IC    R6,PLIBSA+6              LIB START RECD
         A     R6,ACTIVE
         A     R6,DELETED
         LR    R0,R6
         SRDA  R0,32
         DR    R0,R10
         ST    R6,SAV6TF01
         LR    R6,R0           REST
         ST    R6,REST         SAVE REST
*                              QUOT=(LIBSTRTR+ACTIVE+DELETED)/CLLT
         L     R0,SAV6TF01
         SRDA  R0,32
         DR    R0,R10
         ST    R1,QUOT
*                              IF REST=0 THEN REST=CLLT
*                                             QUOT=QUOT-1
         LTR   R6,R6                   REST = ZERO ?
         BNZ   PREPCL40                NO RF00179
         LR    R6,R10                  REST = CLLT
         BCTR  R1,R0                   QUOT=QUOT-1
         ST    R1,QUOT
*---------------------------------LIB NEXT RECD ----------------
*                              LIBNXTR=REST
PREPCL40 DS    0H                      RF00179
         STC   R6,PLIBNA+6             LIB NEXT RECD
*                              LIBNXTH=(LIBSTRTH+QUOT)/CLTC
         LH    R10,CLTC                TRACKS PER CYL
         LR    R6,R1                   LOAD QUOT INTO R6
         MVC   WORDWRK(2),PLIBSA+4     LIB START HH
         AH    R6,WORDWRK
         LR    R0,R6
         SRDA  R0,32
         DR    R0,R10
*---------------------------------LIB NEXT HH ------------------
         STH   R0,WORDWRK
         MVC   PLIBNA+4(2),WORDWRK     LIB NEXT HH
*                                QUOT=(LIBSTRTH+QUOT/CLTC)
         LR    R8,R6
         SRDA  R8,32
         DR    R8,R10
         ST    R9,QUOT
*                                LIBNXTC=LIBSTRTC+QUOT
         L     R6,QUOT
         MVC   WORDWRK(2),PLIBSA+2     LIB START CC
         AH    R6,WORDWRK
         STH   R6,WORDWRK
*---------------------------------LIB NEXT CC ------------------
         MVC   PLIBNA+2(2),WORDWRK     LIB NEXT CC
*                                LIBLSTR=CLLT (LIB-BLK PER TRACK)
         MVC   PLIBLA+6(1),CLLT+1      LIB LAST RECD
*---------------------------------LIB LAST RECD ----------------
*                                QUOT=(LIBNXTR+AVAILAB)/CLLT-1
         XR    R1,R1
         IC    R1,PLIBNA+6             LIB NXT RECD
         LR    R8,R1
         AL    R8,AVAILAB
         SRDA  R8,32
         LH    R7,CLLT
         DR    R8,R7
         BCTR  R9,R0
         ST    R9,QUOT
*                                 LIBLSTH=LIBNXTH+QUOT)/LCTC
         A     R0,QUOT
         LR    R8,R0
         SRDA  R8,32
         DR    R8,R10
*---------------------------------LIB LAST HH ------------------
         STH   R8,WORDWRK
         MVC   PLIBLA+4(2),WORDWRK      LIB LAST HH
*                                 QUOT=(LIBNXTH+QUOT)/LCTC
         SRDA  R0,32
         DR    R0,R10
         ST    R1,QUOT
*                                 LIBLSTC=LIBNXTC+QUOT
         A     R6,QUOT
*---------------------------------LIB LAST HH ------------------
         STH   R6,WORDWRK
         MVC   PLIBLA+2(2),WORDWRK      LIB LAST CC
*
         XC    PDIRAE,PDIRAE
         MVC   PDIRAE+2(2),CLPP        PHASES ACTIVE
*
         MVC   PDIRTRKS,CLDT           # OF DIR TRKS
*
         MVC   PDLINCY,CLLC            # OF LIB CYLS
*
         MVC   PLIBACL,CLCL            CONDENSE LIMIT
*
         LM    R6,R10,SAVTO10
         BR    R10                     RETURN TO CALLER
SAVTO10  DS    5F
WORDWRK  DS    F
REST     DS    F
QUOT     DS    F
ACTIVE   DS    F                       USED / ACTIVE LIB BLKS
DELETED  DS    F                       DELETED LIB BLKS
AVAILAB  DS    F                       AVAILABLE LIB BLKS
SAV6TF01 DS    F                       TF00001
*--------------------------------------------------------------------
SORT     DS    0H
****     CNSLOUT REG=5
         BAL   R10,SORT00
         B     PGMEND                  BUMP TO NEXT STORE POSITION
*--------------------------------------------------------------------
PREPRECD DS    0H
         ST    R10,SAVR10
         STM   R6,R8,SAVPRE6
         CLI   PLIBNAME+1,C'S'        IS IT A SOURCE STMT LIB ?
         BNE   PREP010                NO
         MVC   TRECD,0(R3)
         MVC   CSL,TSL                SUBLIB
         MVC   CNAME,TNAME            MEMBER-NAME
         B     PREP030
PREP010  DS    0H
         CLI   PLIBNAME+1,C'R'        IS IT A RELOCATABLE LIB ?
         BNE   PREP020                NO
         MVC   RRECD,0(R3)            YES
         MVI   CSL,C' '               SUBLIB
*******  CNSLOUT ADR=RNAME,LEN=16
         MVC   CNAME,RNAME            MEMBER-NAME
         MVC   TNREC,RNREC            NUMBER OF RECDS
         MVC   TCHR(1),RCYL           CYL
         MVC   TCHR+1(1),RH2          HEADER
         MVC   TCHR+2(1),RR           RECORD
         TM    RH1,X'01'              FLAG FOR +256 CYL ?
         BNO   PREP015
         OI    TCHR+1,X'40'           REPOS FLAG
PREP015  DS    0H
         MVC   TVL(1),RVL             VERSION LEVEL
         MVC   TML(1),RML             MODIFICATION LEVEL
PREP020  DS    0H
         CLI   PLIBNAME+1,C'P'        IS IT A PROCEDURE LIB ?
         BNE   PREP030                NO
         MVC   PRECD,0(R3)            YES
         MVI   CSL,C' '               SUBLIB
         MVC   CNAME,PNAME            MEMBER-NAME
         MVC   TNREC,PNREC            NUMBER OF RECDS
         MVC   TCHR(1),PCYL           CYL
         MVC   TCHR+1(1),PH           HEADER
         MVC   TCHR+2(1),PR           RECORD
         MVC   TVL(1),RVL             VERSION LEVEL
         MVC   TML(1),RML             MODIFICATION LEVEL
*
PREP030  DS    0H                     ------CORE IMAGE --------------
         CLI   PLIBNAME+1,C'C'        IS IT A CORE IMAGE LIB ?
         BNE   PREP040                NO
         XR    R10,R10
         IC    R10,CLENTRYL+1         ENTRY-LEN OF CIL-ENTRY
         STC   R10,PREP031+1              VAR MOVE
PREP031  MVC   CILRECD(0),0(R3)         YES
******   CNSLOUT ADR=CILRECD
         MVI   CSL,C' '               SUBLIB
         MVC   CNAME,CILNAME          MEMBER-NAME
         MVC   TCHR(3),CILTTR         TTR
         MVC   TNREC,CILNTXT          NUMBER OF RECDS
****     MVC   TCHR+1(1),PH           HEADER
****     MVC   TCHR+2(1),PR           RECORD
         MVI   TVL,X'00'           VERSION LEVEL
         MVI   TML,X'00'           MODIFICATION LEVEL
*
         MVC   WORDWRK,TCHR           RELATIVE HH TO DIR-START-HH
         LH    R7,WORDWRK
         MVC   WORDWRK(2),PDIRSA+4    ADD HH OF DIR START
         AH    R7,WORDWRK
         SR    R6,R6
         XC    WORDWRK,WORDWRK
         MVC   WORDWRK+2(2),CLTC
         D     R6,WORDWRK             REL TRACHS / (TRKS PER CYL)
         MVC   WORDWRK(2),PDIRSA+1    CC OF DIR-START-CC
         AH    R7,WORDWRK
         STH   R6,WORDWRK+2           HH
         STH   R7,WORDWRK             CC
         MVC   PCCHHBI(4),WORDWRK
         MVC   PCCHHBI+4(1),TCHR+2     R
         MVI   PCCHHBIF,X'05'
         B     PREP045
PREP040  DS    0H
         XC    PCCHHBI,PCCHHBI
         MVC   PCCHHBI(3),TCHR        DSK-ADDR
         MVI   PCCHHBIF,X'03'         LENGTH
******   CNSLOUT ADR=PCCHHBI
PREP045  DS    0H
         MVC   PFUNC,=CL3'B2C'        BINARY TO CHAR
         BAL   R10,CALLUTI
         MVC   CC,PCCHHAR             CYL
         MVC   CH,PCCHHAR+4           HEADS
         MVC   CR,PCCHHAR+7           RECORDS
*
         XC    PCCHHBI,PCCHHBI
         MVC   PCCHHBI+4(2),TNREC     NUMMBER OF RECDS
         MVC   PFUNC,=CL3'BNC'        BINARY TO CHAR
         BAL   R10,CALLUTI
         MVC   CNREC,PCCHHAR+7
*
         XC    PCCHHBI,PCCHHBI
         MVC   PCCHHBI+5(1),TVL       VERSION LEVEL
         MVC   PFUNC,=CL3'BNC'        BINARY TO CHAR
         BAL   R10,CALLUTI
         MVC   CVL,PCCHHAR+9
*
         XC    PCCHHBI,PCCHHBI
         MVC   PCCHHBI+5(1),TML       MODIFICATION LEVEL
         MVC   PFUNC,=CL3'BNC'        BINARY TO CHAR
         BAL   R10,CALLUTI
         MVC   CML,PCCHHAR+9
*
         LM    R6,R8,SAVPRE6
         L     R10,SAVR10
         BR    R10                    RETURN TO CALLER
SAVPRE6  DS    3F
*-------------------------------------------------------------------
SORT00   DS    0H
*-------------------------------------------------------------------
******   CNSLOUT MSG='SORT00'
         ST    R10,SORTR10         SAVE R10
******   CNSLOUT ADR=PSORTNUM,LEN=3
         MVC   PRETCOD,=CL3'000'
         MVC   PSORTLEN,=XL1'0A'      10 (2+8)
         STH   R5,PSORTNUM
         MVC   RECLEN,=H'34'
******   CLI   PLIBNAME+1,C'S'        IS IT A SOURCE STMT LIB ?
******   BNE   SORT010
******   MVC   PSORTLEN,=XL1'9'
SORT010  DS    0H
         LA    R1,PARMS
         ST    R1,PARMLIST
         LA    R1,PARMLIST
         L     R15,VSSORTAD       --------------------------
         BALR  R14,R15            CALL VSSORT
         L     R10,SORTR10        ----------------------------------
         BR    R10                    RETURN TO CALLER
VSSORTAD DC    V(VSSORT)
SORTR10  DS    F
*-------------------------------------------------------------------
CALLUTI  DS    0H
*-------------------------------------------------------------------
*******  CNSLOUT MSG='CALLUTI'
         ST    R10,UTI10          SAVE R10
*
         MVC   PRETCOD,=CL3'000'
         LA    R1,PARMS
         ST    R1,PARMLIST
         LA    R1,PARMLIST
         L     R15,VSUTIAD
         BALR  R14,R15
         L     R10,UTI10            -----------------------------------
         BR    R10                    RETURN TO CALLER
*-----------------------            --------------------------------
VSUTIAD  DC    V(VSUTI)
UTI10    DS    F
*-------------------------------------------------------------------
PGMEND   DS    0H
******   CNSLOUT MSG='PGM-END'
*                                    -------------------------------
         ST    R5,PNORL
         MVC   RECLEN,=H'34'
         L     R9,PARMADDR
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                    RETURN TO CALLER
*-------------------------------------------------------------------
*
TRECD    DS    0CL16
TSL      DS    CL1                     SUBLIB
TNAME    DS    CL8                     MEMBER-NAME
TCHR     DS    XL3                     DSK-ADDRESS
TNREC    DS    XL2                     # OF RECORDS
TVL      DS    XL1                     VERSION LEVEL
TML      DS    XL1                     MODIFICATION LEVEL
*-----------------------
RRECD    DS    0CL16
RNAME    DS    CL8                     MEMBER-NAME
RNREC    DS    XL2                     # OF RECORDS / B?OCKS
RCYL     DS    XL1                     CYL-NUMBER
RH1      DS    XL1                     HDR1 X'40' = ADD  256 CYLS
RH2      DS    XL1                     HEADER 2
RR       DS    XL1                     # OF RECORD
RVL      DS    XL1                     VERSION LEVEL
RML      DS    XL1                     MODIFICATION LEVEL
*-----------------------
PRECD    DS    0CL16
PNAME    DS    CL8                     MEMBER-NAME
PNREC    DS    XL2                     # OF RECORDS
PCYL     DS    XL1                     DSK-ADDRESS
PH       DS    XL1                     # OF RECORDS
PR       DS    XL1                     VERSION LEVEL
PRES     DS    XL1                     RESERVED
PVL      DS    XL1                     MODIFICATION LEVEL
PML      DS    XL1                     MODIFICATION LEVEL
*-----------------------
CILRECD  DS    0CL24
*CILENTRL DS    CL2                     ENTRY-LEN
CILNAME  DS    CL8                     PHASE-NAME
CILTTR   DS    XL3                     TTR
CILNHW   DS    XL1                     # OF FOLLOWING HW
CILNTXT  DS    XL2                     # OF TXT-RECDS
CILNBLR  DS    XL2                     # OF BYTES LAST RECD
CILSTAT  DS    XL2                     X'10'=RELOC X'20'=SVA-ELIG.
CILLADR  DS    XL4                     LOAD-ADDR
CILEADR  DS    XL4                     ENTRY-ADDR
CILPADR  DS    XL4                     PARTITION-ADDR
CILRES   DS    XL20                    RESERVE
*-----------------------
CRECD    DS    0CL34
CSL      DS    CL1                     SUBLIB
         DC    CL1' '
CNAME    DS    CL8                     MEMBER-NAME
         DC    CL1' '
CCHR     DS    0CL9                    DSK-ADDRESS
CC       DS    CL3                     CYLS
         DC    CL1'-'
CH       DS    CL2                     HEADS
         DC    CL1'-'
CR       DS    CL2                     RECORDS
         DC    CL1' '
CNREC    DS    CL5                     # OF RECORDS
         DC    CL1' '
CVL      DS    CL3                     VERSION LEVEL
         DC    CL1' '
CML      DS    CL3                     MODIFICATION LEVEL
SAVR10   DS    F
PARMLIST DS    F
TABRECSA DS    F
PARMADDR DS    F
SAVEAREA DS    18F
*                       DIRECTORY INFO AREA (CLB)
        DS    0F        LIBRARY DECRIPTOR ENTRY (CLB)
CLDES   DS    0CL80
CLBU    DS    XL2                 # BYTES USED (TOTAL ENTRY)
CLNAME  DS    CL8                 DUMMY PHASE NAME
CLTTR   DS    CL3                 DUMMY TTR
CLSN    DS    XL1                 # OF HALFWORDS USED AFTER THIS BYTE
CLTC    DS    XL2                 TRACKS PER CYL
CLDT    DS    XL2                 NUMBER OF TRACKS FOR DIRECTORY
CLLC    DS    XL2                 NUMBER OF CYLS FOR LIBRARY
CLPP    DS    XL2                 NUMBER OF ACTIVE PHASES
CLDB    DS    XL2                 NUMBER OF DIR BLOCKS PER TRACK
CLDU    DS    XL2                 NUMBER OF DIR BLOCKS USED
CLDA    DS    XL2                 NUMBER OF DIR BLOCKS AVAILABLE
CLLT    DS    XL2                 NUMBER OF LIB BLOCKS PER TRACK
CLLU    DS    XL4                 NUMBER OF LIB BLOCKS USED
CLLD    DS    XL4                 NUMBER OF LIB BLOCKS DELETED
CLLA    DS    XL4                 NUMBER OF LIB BLOCKS AVAILABLE
CLCL    DS    XL2                 CONDENSE LIMIT IN BLOCKS
CLCLT   DS    CL8                 LAST UPDATE TIME
CLFP    DS    CL8                 FIRST LINKED PHASE
CLRES   DS    CL20                RESERVED
******   CNSLCCB
***********************************************************************
*****    DIRECTORY READ ROUTINE FOR RLB, SLB AND PLB                  *
*********************************************************************** 70200025
         SPACE 2                                                        70230025
RSEXCP   NOP   RSRPSOK                 FIRST TIME SW           @DM15919 70230834
         OI    RSEXCP+1,BRANCH         SET SW TO BRANCH        @DM15919 70231634
         COMRG ,                                               @DM15919 70232434
         USING COMREG,R1                                       @DM15919 70233234
         TM    RMSROPEN,RPSUPER        DOES SV SUPPORT RPS     @DM15919 70234034
         DROP  R1                                              @DM15919 70234834
         BNZ   RSSYSTST                YES                     @DM15919 70235634
         SPACE 1                                               @DM15919 70236434
RSNORPS  EQU   *                                               @DM15919 70237234
         MVC   RSCCWS,RSCCWTIC         REPLACE SETSECT BY TIC  @DM15919 70238034
         MVI   RSCCW3+FOUR,SLI         DISCHAIN READ SECTOR    @DM15919 70238834
         B     RSRPSOK                                         @DM15919 70239634
         SPACE 1                                               @DM15919 70240434
RSSYSTST EQU   *                                               @DM15919 70241234
         CLI   RSCBSYM,RES             IS THIS A SYSTEM LIB    @DM15919 70242034
         BNE   RSTSTPRV                NO, MUST BE ANY PRV     @DM15919 70242834
         TM    RPSYSDV,RPSDEV          ANY KIND OF RPS DEVICE  @DL30SCM 70243634
         BNZ   RSRPSOK                 YES                     @DL30SCM 70244434
         B     RSNORPS                 NO                      @DM15919 70245234
         SPACE 1                                               @DM15919 70246034
RSTSTPRV EQU   *                                               @DM15919 70246834
         CLI   RSCBSYM,RLB             IS THIS A PRV RLB       @DM15919 70247634
         BNE   RSPRVSLB                NO, MUST BE A PRV SLB   @DM15919 70248434
         TM    IJRRPS,RPSDTF           IS PRV RLB ON RPS DEV   @DM15919 70249234
         BZ    RSNORPS                 NO                      @DM15919 70250034
         B     RSRPSOK                 YES                     @DM15919 70250834
         SPACE 1                                               @DM15919 70251634
RSPRVSLB EQU   *                                               @DM15919 70252434
         TM    IJSRPS,RPSDTF           IS PRV SLB ON RPS DEV   @DM15919 70253234
         BZ    RSNORPS                 NO                      @DM15919 70254034
         SPACE 1                                               @DM15919 70254834
RSRPSOK  EQU   *                                               @DM15919 70255634
         SPACE 1                                               @DM15919 70256434
         LA    R1,RSCCB                ADDR OF CCB FOR EXCP             70260034
         EXCP  (1)                     READ DIRECTORY BLOCK             70280034
*                                                                       70320025
         WAIT  (1)                     WAIT FOR COMPLETION OF READ      70350034
         ST    R1,SAVR1                                                 70380025
         L     R1,SAVR1
         TM    RSCCB3,EOC              END OF CYLINDER                  70410025
         BCR   EIGHT,R10               NO, EXIT TO CALLER               70440025
         LH    R1,RSSEEKCC             GET OLD CC OF SEEK ADDRESS   4-0 70470027
         LA    R1,ONE(R1)              INCREMENT TO NEXT CYLINDER       70500025
         STH   R1,RSCOUNT              STORE UPDATE CC OF SEEK ADDR.4-0 70530027
         MVC   RSCOUNT3(THREE),RSHR    SET HD AND RECD TO ONES @DL30SCM 70560030
         BR    R10                     EXIT TO CALLER                   70590025
SAVR1    DS    F
         EJECT                                                          70620025
*********************************************************************** 70650025
*                    CCB'S, CCW'S, DC'S AND DS'S                      * 70680025
*********************************************************************** 70710025
         SPACE 2                                                        70740025
*****    CCB FOR RELOCATABLE AND SOURCE STATEMENT DIRECTORIES           70770025
         SPACE 1                                                        70800025
         DS    0D                                                       70830025
RSCCB    CCB   SYSRES,RSCCW1           CCB FOR RELOCATABLE/SOURCE DIR.  70860025
         SPACE 3                                                        70890025
*****    CCW,S TO READ RELOCATABLE AND SOURCE STATEMENT DIRECTORIES     70920025
         SPACE 1                                                        70950025
RSCCW1   CCW   SEEK,RSSEEKBB,CCSLI,SIX       SEEK BBCCHH
RSCCWS   CCW   SETSECT,RSSSTST,CCSLI,ONE SET SECT TO THE REC
RSCCW2   CCW   SIDE,RSSEEKCC,CCSLI,FIVE     SEARCH ID EQUAL
RSCCWTIC CCW   TIC,RSCCW2,CCSLI,ONE         TRANSFER IN CHANNEL
         CCW   READ,RSAREA,CCSLI,THREE20    READ DATA - 320 BYTES
RSCCW3   CCW   RDCNT,RSCOUNT,CCSLI,FIVE     RD CNT OF NEXT REC
         CCW   RDSECT,RSSRDST,SLI,ONE  READ SECTOR OF NEXT REC
         SPACE 3
*****    SEEK ADDRESS BUCKET
         SPACE 1
* THE LAST BYTE OF RSSEEKCC AND SVECCHHR IS USED AS THE SECTOR
* ARGUMENT LOCATION.
         SPACE 1
RSSEEKBB DC    X'0000'                 BB PORTION OF SEEK ADDR
RSSEEKCC DC    XL6'00'                 CCHHR PART OF SEEK ADDR
SVECCHHR DC    XL6'00'                 CCHHR SAVE AREA
         SPACE 3
*****    DISK COUNT READIN FIELD
         SPACE 1
RSCOUNT  DC    XL6'00'                 COUNT FIELD READIN AREA
         SPACE 3
*****    RELOCATABLE AND SOURCE STATEMENT DIRECTORY READIN AREA
         SPACE 1
RSAREA   DS    320C' '                 DIRECTORY READIN AREA
         EJECT
*****    CONSTANTS - COMMON TO THIS PHASE ONLY
         SPACE 1
RSWORK   DC    4F'0'                   DIRECTORY MOVE WORK AREA
PH03     DC    A(ENDPH0)               END ADDRESS OF PHASE 0
RSRECNO  DC    H'0'                    RECORD COUNT SAVE AREA
RSHR     DC    X'000100'               HD, REC & SECT RESET
RSFIVE   DC    H'5'                    STATEMENT 1ST 5 RECORDS
RSRECDS  DC    H'20'                   RECORDS IN RELOCATABLE BLOCK     71760025
HW2      DC    H'2'                    MIN. NUMBER TO BE SORTED     4-0 71770027
         DS    0D                      DW ALIGNMENT                 5-0 71770128
ENDSERV3 EQU   *                       BEGIN OF SORT AREA           5-0 71810028
         SPACE 3                                                        71820025
*********************************************************************** 71850025
*****    EQUATES -- COMMON TO THIS PHASE ONLY                         * 71880025
*********************************************************************** 71910025
FIVE     EQU   5                                                        10110025
SIX      EQU   6                                                        10140025
TEN      EQU   10                                                       10260025
TWENTY   EQU   20                                                       10260025
EIGHTY   EQU   80                                                       11280025
ZERO     EQU   0                                                        09960025
SIXTEEN  EQU   16                                                       10410025
BRANCH   EQU   X'F0'                                           @DM15919 11975034
ONE      EQU   1                                                        09990025
SLI      EQU   X'20'                   SUPPRESS WRONG LNG CCW IND       09600025
FOUR     EQU   4                                                        10080025
         SPACE 1                                                        71940025
RSCCB3   EQU   RSCCB+3                 BYTE 2 OF TRANSFERRED INFO       71970025
RSCBSYM  EQU   RSCCB+7                 LOC OF SYMBOLIC UNIT IN CCB      72000025
RSCOUNT3 EQU   RSCOUNT+3               USING TRACK IN COUNT FIELD       72090025
RSSSTST  EQU   RSSEEKCC+FIVE           SET SECTOR VALUE        @DL30SCM 72100030
RSSRDST  EQU   RSCOUNT+FIVE            RD SECT INTO HERE       @DL30SCM 72110030
RSRECDS1 EQU   RSRECDS+1               LOCATION OF BLOCK BYTE           72120025
THREE20  EQU   320                                                      72150025
ASTER    EQU   C'*'                    END OF LIBRARY RECORD IN@DL29ZCL 72160029
BLANK    DC    C'  '                   BLANKS IF INVALID VM             60480025
RES      EQU   X'06'                   SYSRES LOGICAL UNIT     @DL29ZCL 09670029
SLB      EQU   X'07'                   SYSSLB LOGICAL UNIT NUMBER       09690025
RLB      EQU   X'08'                   SYSRLB LOGICAL UNIT NUMBER       09720025
RPSYSDV  DC    X'00'                   SYSRES DTF DEV TYPE     @DL30SCM 06710030
EOC      EQU   X'20'                   END OF CYLINDER INDICATOR        11910025
EIGHT    EQU   8                                                        10200025
THREE    EQU   3                                                        10050025
CCSLI    EQU   X'60'                   CHAIN AND WRONG LNG CCW IND      09630025
SEEK     EQU   X'07'                   SEEK BBCCHH COMMAND              09420025
SIDE     EQU   X'31'                   SEARCH ID EQUAL COMMAND          09450025
TIC      EQU   X'08'                   TRANSFER IN CHANNEL COMMAND      09480025
READ     EQU   X'06'                   READ DATA COMMAND                09510025
RDCNT    EQU   X'92'                   READ COUNT/MULTI TRACK COMMAND   09540025
SETSECT  EQU   X'23'                   SET SECTOR CCW OP CODE  @DL30SCM 12002030
RDSECT   EQU   X'22'                   RD SECTOR CCW OP CODE   @DL30SCM 12003030
RPSUPER  EQU   X'01'                   RPS SUPPORT IN SUPERVIS @DL30SCM 12004030
RPSPUBDV EQU   X'04'                   RPS DEVICE IN PUB       @DL30SCM 12005030
RPSDTF   EQU   X'01'                   RPS DEVICE IN DTF       @DL30SCM 12006030
RPSDEV   EQU   X'FF'                   MASK TO CHECK FOR RPS   @DL30SCM 12007030
M8       EQU   X'08'                   ICM MASK                @DL30SCM 12008030
M12      EQU   X'0C'                   ICM MASK                @DL30SCM 12009030
JCTLBYT  EQU   135                     FIELD IN COMREG - RPS   @DL30SCM 12009430
RPSFTDV  EQU   6                       FIELD IN FETCH TAB -RPS @DL30SCM 12009830
DFTBDIR  EQU   10                      =                  -DIR @DM15919 12013834
SCVRT    EQU   75                      SVC 75 SECTOR CONVERT   @DL30SCM 12018034
         SPACE 1                                               @DL30SCM 12022034
LASTREC  DS    2D               HOLD LAST RECORD FOR NEXT PASS @DA01108 12026034
ENDPH0   DS    0D                                                       12030025
LOADAREA EQU   *                       START OF LOAD AREA               12060025
         SPACE 3                                                        72180025
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 72210025
* --------------------------------------------------------------------* 00000065
         ST    R5,PNORL            # OF RECORDS TO PARMS
         MVC   RECLEN,=H'34'       RECORD-LENGTH
         L     R9,PARMADDR         LOAD PARAMETER-ADDR
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
* --------------------------------------------------------------------* 00000386
         SPACE 2                                                        05883034
IJSYSRL  DTFCP TYPEFLE=INPUT,                                          X05884034
               DISK=YES,                                               X05885034
               DEVADDR=SYSRLB,                                         X05886034
               EOFADDR=*,                                              X05887034
               IOAREA1=*                                            5-0 05888034
         SPACE 2                                                    5-0 05889034
* THE LABEL 'IJJCPD3' IS ONLY USED TO PROVIDE THE PROGRAM           5-0 05890034
* WITH A DUMMY ENTRY, SO THAT THERE WILL BE NOT                     5-0 05891034
* UNRESOLVED ADDRESS CONSTANTS DURING LINK EDITING.                 5-0 05892034
         SPACE 1                                                    5-0 05893034
IJJCPD3  EQU   *                                                    5-0 05894034
         ENTRY IJJCPD3                 DUMMY ENTRY             @DM11927 05895034
IJRLL    EQU   IJSYSRL+60              LOC OF LOWER LIMIT IN DTF        05896034
IJRRPS   EQU   IJSYSRL+42              RPS DEV TYPE BYTE   DTF @DM15919 05897034
         SPACE 3                                                    5-0 05898034
*****    DTF FOR OPENING PRIVATE SOURCE STATEMENT LIBRARY               05899034
         SPACE 2                                                        05900034
IJSYSSL  DTFCP TYPEFLE=INPUT,                                          X05901034
               DISK=YES,                                               X05902034
               DEVADDR=SYSSLB,                                         X05903034
               EOFADDR=*,                                              X05904034
               IOAREA1=*                                            5-0 05905034
         SPACE 2                                               @DM15919 05906034
IJSLL    EQU   IJSYSSL+60              LOC OF LOWER LIMIT IN DTF        05907034
IJSRPS   EQU   IJSYSSL+42              RPS DEV TYPE BYTEDTF    @DM15919 05908034
         SPACE 3                                                        05910025
         LTORG                                                          05940025
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                     00000455
* --------------------------------------------------------------------* 00000455
         MAPCOMR
         DS    0D                                                       00000427
TABRECS  DSECT
TABREC   DS    CL80                                                     00000460
*
WORKA    DSECT
         COPY  LIBPARMS                                                 00000460
         END
        BKEND
        CATALS   A.VSRDSK,0.0
        BKEND   A.VSRDSK
*------------------------------------------------------------------
*     NAME: VSRDSK
*     TYPE: SUB-ROUTINE
* FUNCTION: Read a diskfile via EXCP
*
* INTERNAL
*
*
* EXTERNAL
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
*        REGISTER EQUATES                                             * 00000351
* --------------------------------------------------------------------* 00000352
R0       EQU   0                                                        00000353
R1       EQU   1                                                        00000354
R2       EQU   2                                                        00000355
R3       EQU   3                                                        00000356
R4       EQU   4                                                        00000357
R5       EQU   5                                                        00000358
R6       EQU   6                                                        00000359
R7       EQU   7                                                        00000360
R8       EQU   8                                                        00000361
R9       EQU   9                                                        00000362
R10      EQU   10                                                       00000363
R11      EQU   11                                                       00000364
R12      EQU   12                                                       00000365
R13      EQU   13                                                       00000366
R14      EQU   14                                                       00000367
R15      EQU   15                                                       00000368
VSRDSK    CSECT                                                          0000001
         STM   R14,R12,12(R13)         STORE REGS IN SAVEAREA           00000018
         USING VSRDSK,R11,R12          R11 NOW BASE REGISTER            00000020
         LR    R11,R15                 ESTABLISH ADDRESSABILITY         00000021
         B     GOON
         DC    C'***** VSRDSK *****'   EYE-CATCHER
GOON     DS    0H
         LA    R12,4095(R11)                                            00000022
         LA    R12,1(R12)              SECOND BASE                      00000023
         ST    R13,SAVEAREA+4          STORE MAIN PGM SAVEAREA          00000024
         LR    R10,R13                 SAVE R13                         00000025
         LA    R13,SAVEAREA            SAVE AREA FOR THIS TASK          00000026
         ST    R13,8(R10)              STORE EXIT SAVE ADDRESS          00000027
* --------------------------------------------------------------------* 00000028
*        STATEMENTS FOR SET UP FIELDS NEEDED FOR LIBRM                  00000029
* --------------------------------------------------------------------* 00000030
         L     R2,0(R1)                PARMS ADDRESS                    00000031
         ST    R2,PARMADDR             SAVE                             00000032
         USING WORKA,R2
         L     R9,PBUFADR              LOAD DATATAB ADDRESS             00000034
         ST    R9,TABRECSA             SET  TABRECS ADDRESS             00000035
         USING TABRECS,R9              ESTABLISH ADDRESSABILITY         00000036
* --------------------------------------------------------------------* 00000065
*     PGM-START                                                       * 00000066
* --------------------------------------------------------------------* 00000067
******   CNSLOUT MSG='PGM-START'
*RSSETSLB MVI   RSCBSYM,X'06'        SYSTEM PLB
*RSSOURCE MVC   RSSEEKCC(FIVE),=XL5'01F3000F08' 499-15-08 CONDENSE PLB
RSSOURCE MVC   RSSEEKCC(FIVE),PCCHHBI
         L     R3,PNORL             NUMBER OF RECORDS
         MVC   RECLEN,=H'80'
READLOOP DS    0H
         BAL   R10,RSEXCP
         MVC   0(80,R9),RSAREA
         LA    R9,80(R9)            INCREASE OUTPUT ADDRESS
         BCT   R3,READLOOP          LOOP UNTIL NUMBER = 0
* --------------------------------------------------------------------* 00000065
*     PGMEND Program-ENd                                              * 00000066
* --------------------------------------------------------------------* 00000067
PGMEND   DS    0H
******   CNSLOUT MSG='PGM-END'
         L     R9,PARMADDR         LOAD PARAMETER-ADDR
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
*****    CNSLCCB
*---------------------------------------------------------------------*
*        ROUTINE FOR READING FROM DSK
*---------------------------------------------------------------------*
         SPACE 2                                                        70230025
RSEXCP   NOP   RSRPSOK                 FIRST TIME SW           @DM15919 70230834
         OI    RSEXCP+1,BRANCH         SET SW TO BRANCH        @DM15919 70231634
         COMRG ,                                               @DM15919 70232434
         USING COMREG,R1                                       @DM15919 70233234
         TM    RMSROPEN,RPSUPER        DOES SV SUPPORT RPS     @DM15919 70234034
         DROP  R1                                              @DM15919 70234834
         BNZ   RSSYSTST                YES                     @DM15919 70235634
RSNORPS  EQU   *                                               @DM15919 70237234
         MVC   RSCCWS,RSCCWTIC         REPLACE SETSECT BY TIC  @DM15919 70238034
         MVI   RSCCW3+FOUR,SLI         DISCHAIN READ SECTOR    @DM15919 70238834
         B     RSRPSOK                                         @DM15919 70239634
RSSYSTST EQU   *                                               @DM15919 70241234
         TM    RPSYSDV,RPSDEV          ANY KIND OF RPS DEVICE  @DL30SCM 70243634
         BNZ   RSRPSOK                 YES                     @DL30SCM 70244434
         B     RSNORPS                 NO                      @DM15919 70245234
RSRPSOK  EQU   *                                               @DM15919 70255634
         LA    R1,RSCCB                ADDR OF CCB FOR EXCP             70260034
         EXCP  (1)                     READ DIRECTORY BLOCK             70280034
         WAIT  (1)                     WAIT FOR COMPLETION OF READ      70350034
         MVC    RSSEEKCC,RSCOUNT  RH NXT RECORD                         70320025
*                                                                       70320025
         TM    RSCCB3,EOC              END OF CYLINDER                  70410025
         BCR   EIGHT,R10               NO, EXIT TO CALLER               70440025
         LH    R1,RSSEEKCC             GET OLD CC OF SEEK ADDRESS   4-0 70470027
         LA    R1,ONE(R1)              INCREMENT TO NEXT CYLINDER       70500025
         STH   R1,RSCOUNT              STORE UPDATE CC OF SEEK ADDR.4-0 70530027
         MVC   RSCOUNT3(THREE),RSHR    SET HD AND RECD TO ONES @DL30SCM 70560030
         BR    R10                     EXIT TO CALLER                   70590025
*                                                                       70320025
SAVR1    DS    F
         ST    R1,SAVR1                                                 70380025
         L     R1,SAVR1
         EJECT                                                          70620025
*---------------------------------------------------------------------* 70650025
*                    CCB'S, CCW'S, DC'S AND DS'S                        70680025
*---------------------------------------------------------------------* 70650025
         DS    0D                                                       70830025
RSCCB    CCB   SYSRES,RSCCW1           CCB                              70860025
*---------------------------                                            70890025
*        CCW,S TO READ DISK                                             70920025
*---------------------------                                            70950025
RSCCW1   CCW   SEEK,RSSEEKBB,CCSLI,SIX       SEEK BBCCHH
RSCCWS   CCW   SETSECT,RSSSTST,CCSLI,ONE SET SECT TO THE REC
RSCCW2   CCW   SIDE,RSSEEKCC,CCSLI,FIVE     SEARCH ID EQUAL
RSCCWTIC CCW   TIC,RSCCW2,CCSLI,ONE         TRANSFER IN CHANNEL
         CCW   READ,RSAREA,CCSLI,EIGHTY     READ DATA - 320 BYTES
RSCCW3   CCW   RDCNT,RSCOUNT,CCSLI,FIVE     RD CNT OF NEXT REC
         CCW   RDSECT,RSSRDST,SLI,ONE  READ SECTOR OF NEXT REC
*---------------------------
*        SEEK ADDRESS BUCKET
*---------------------------
* THE LAST BYTE OF RSSEEKCC AND SVECCHHR IS USED AS THE SECTOR
* ARGUMENT LOCATION.
*---------------------------
*
RSSEEKBB DC    X'0000'                 BB PORTION OF SEEK ADDR
RSSEEKCC DC    XL6'00'                 CCHHR PART OF SEEK ADDR
SVECCHHR DC    XL6'00'                 CCHHR SAVE AREA
*-------------------------------
*        DISK COUNT READIN FIELD
*-------------------------------
RSCOUNT  DC    XL6'00'                 COUNT FIELD READIN AREA
*-------------------------------
*        READIN AREA
*-------------------------------
RSAREA   DS    320C' '                 DIRECTORY READIN AREA
*
*****    CONSTANTS - COMMON TO THIS PHASE ONLY
*
RSWORK   DC    4F'0'                   DIRECTORY MOVE WORK AREA
PH03     DC    A(ENDPH0)               END ADDRESS OF PHASE 0
RSRECNO  DC    H'0'                    RECORD COUNT SAVE AREA
RSHR     DC    X'000100'               HD, REC & SECT RESET
RSFIVE   DC    H'5'                    STATEMENT 1ST 5 RECORDS
RSRECDS  DC    H'20'                   RECORDS IN RELOCATABLE BLOCK     71760025
HW2      DC    H'2'                    MIN. NUMBER TO BE SORTED
         DS    0D                      DW ALIGNMENT
ENDSERV3 EQU   *                       BEGIN OF SORT AREA
*
*---------------------------------------------------------------------* 00000386
*        WORKFIELDS                                                   * 00000387
*---------------------------------------------------------------------* 00000388
         DS    0D                                                       00000389
*---------------------------------------------------------------------* 71850025
*        EQUATES -- COMMON TO THIS PHASE ONLY                         - 71880025
*---------------------------------------------------------------------* 71910025
FIVE     EQU   5                                                        10110025
SIX      EQU   6                                                        10140025
TEN      EQU   10                                                       10260025
EIGHTY   EQU   80                                                       11280025
ZERO     EQU   0                                                        09960025
SIXTEEN  EQU   16                                                       10410025
BRANCH   EQU   X'F0'                                           @DM15919 11975034
ONE      EQU   1                                                        09990025
SLI      EQU   X'20'                   SUPPRESS WRONG LNG CCW IND       09600025
FOUR     EQU   4                                                        10080025
         SPACE 1                                                        71940025
RSCCB3   EQU   RSCCB+3                 BYTE 2 OF TRANSFERRED INFO       71970025
RSCBSYM  EQU   RSCCB+7                 LOC OF SYMBOLIC UNIT IN CCB      72000025
RSCOUNT3 EQU   RSCOUNT+3               USING TRACK IN COUNT FIELD       72090025
RSSSTST  EQU   RSSEEKCC+FIVE           SET SECTOR VALUE        @DL30SCM 72100030
RSSRDST  EQU   RSCOUNT+FIVE            RD SECT INTO HERE       @DL30SCM 72110030
RSRECDS1 EQU   RSRECDS+1               LOCATION OF BLOCK BYTE           72120025
THREE20  EQU   320                                                      72150025
ASTER    EQU   C'*'                    END OF LIBRARY RECORD IN@DL29ZCL 72160029
BLANK    DC    C'  '                   BLANKS IF INVALID VM             60480025
RES      EQU   X'06'                   SYSRES LOGICAL UNIT     @DL29ZCL 09670029
SLB      EQU   X'07'                   SYSSLB LOGICAL UNIT NUMBER       09690025
RLB      EQU   X'08'                   SYSRLB LOGICAL UNIT NUMBER       09720025
RPSYSDV  DC    X'00'                   SYSRES DTF DEV TYPE     @DL30SCM 06710030
EOC      EQU   X'20'                   END OF CYLINDER INDICATOR        11910025
EIGHT    EQU   8                                                        10200025
THREE    EQU   3                                                        10050025
CCSLI    EQU   X'60'                   CHAIN AND WRONG LNG CCW IND      09630025
SEEK     EQU   X'07'                   SEEK BBCCHH COMMAND              09420025
SIDE     EQU   X'31'                   SEARCH ID EQUAL COMMAND          09450025
TIC      EQU   X'08'                   TRANSFER IN CHANNEL COMMAND      09480025
READ     EQU   X'06'                   READ DATA COMMAND                09510025
RDCNT    EQU   X'92'                   READ COUNT/MULTI TRACK COMMAND   09540025
SETSECT  EQU   X'23'                   SET SECTOR CCW OP CODE  @DL30SCM 12002030
RDSECT   EQU   X'22'                   RD SECTOR CCW OP CODE   @DL30SCM 12003030
RPSUPER  EQU   X'01'                   RPS SUPPORT IN SUPERVIS @DL30SCM 12004030
RPSPUBDV EQU   X'04'                   RPS DEVICE IN PUB       @DL30SCM 12005030
RPSDTF   EQU   X'01'                   RPS DEVICE IN DTF       @DL30SCM 12006030
RPSDEV   EQU   X'FF'                   MASK TO CHECK FOR RPS   @DL30SCM 12007030
M8       EQU   X'08'                   ICM MASK                @DL30SCM 12008030
M12      EQU   X'0C'                   ICM MASK                @DL30SCM 12009030
JCTLBYT  EQU   135                     FIELD IN COMREG - RPS   @DL30SCM 12009430
RPSFTDV  EQU   6                       FIELD IN FETCH TAB -RPS @DL30SCM 12009830
DFTBDIR  EQU   10                      =                  -DIR @DM15919 12013834
SCVRT    EQU   75                      SVC 75 SECTOR CONVERT   @DL30SCM 12018034
         SPACE 1                                               @DL30SCM 12022034
LASTREC  DS    2D               HOLD LAST RECORD FOR NEXT PASS @DA01108 12026034
ENDPH0   DS    0D                                                       12030025
LOADAREA EQU   *                       START OF LOAD AREA               12060025
         SPACE 3                                                        72180025
*                                                                       00000391
* --------------------------------------------------------------------* 00000404
*        MESSAGE TABLES                                               * 00000405
* --------------------------------------------------------------------* 00000406
         DS    0F                       ALIGNEMENT                      00000407
* --------------------------------------------------------------------* 00000369
*        SAVE AREA DECLARATIONS                                       * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F                                                       00000372
SAVEAREA DC    XL72'00'                 MONITOR TASK SAVE AREA           0000037
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS          00000379
PARMADDR DS    F                                                        00000458
*                                                                       00000459
* --------------------------------------------------------------------* 00000424
         LTORG                                                          00000454
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                   * 00000456
* --------------------------------------------------------------------* 00000457
         MAPCOMR
WORKA    DSECT                                                          00000475
         COPY  LIBPARMS                                                 00000460
         DS    0D                                                       00000427
* --------------------------------------------------------------------* 00000474
TABRECS  DSECT                                                          00000475
TABREC   DS    CL80                                                     00000476
* --------------------------------------------------------------------* 00000477
MAPOUT   DSECT
         DS    0F
MAPADDR  DS    0CL2028
MAPNAME  DS    CL8
MAPEYEC  DS    CL8
         DS    CL2
MAPLEN   DS    CL2                MAP-LEN
MAPVCNT  DS    AL1(0)             VAR-FIELD COUNT
MAPVLST  DS    AL3(0)             ADDR MAP-VAR-OUTPUT-LIST
MAPSTART DS    CL2000
BUFFER   EQU   *                   MEMBER BUFFER BEGIN                  00001705
         END                                                            00000481
        BKEND
        CATALS   A.VSSORT,0.0
        BKEND   A.VSSORT
*------------------------------------------------------------------
*     NAME: VSSORT
*     TYPE: SUB-ROUTINE
* FUNCTION: Sort a directory of a library
*
* INTERNAL
*
*
* EXTERNAL
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
*        REGISTER EQUATES                                             * 00000351
* --------------------------------------------------------------------* 00000352
R0       EQU   0                                                        00000353
R1       EQU   1                                                        00000354
R2       EQU   2                                                        00000355
R3       EQU   3                                                        00000356
R4       EQU   4                                                        00000357
R5       EQU   5                                                        00000358
R6       EQU   6                                                        00000359
R7       EQU   7                                                        00000360
R8       EQU   8                                                        00000361
R9       EQU   9                                                        00000362
R10      EQU   10                                                       00000363
R11      EQU   11                                                       00000364
R12      EQU   12                                                       00000365
R13      EQU   13                                                       00000366
R14      EQU   14                                                       00000367
R15      EQU   15                                                       00000368
VSSORT   CSECT                                                          00000016
         STM   R14,R12,12(R13)         STORE REGS IN SAVEAREA           00000018
         USING VSSORT,R11,R12          R11 NOW BASE REGISTER            00000020
         LR    R11,R15                 ESTABLISH ADDRESSABILITY         00000021
         B     GOON
         DC    C'***** VSSORT *****'   EYE-CATCHER
GOON     DS    0H
         LR    R12,R11
         LA    R12,4095(R12)                                            00000022
         LA    R12,1(R12)              SECOND BASE                      00000023
         ST    R13,SAVEAREA+4          STORE MAIN PGM SAVEAREA          00000024
         LR    R10,R13                 SAVE R13                         00000025
         LA    R13,SAVEAREA            SAVE AREA FOR THIS TASK          00000026
         ST    R13,8(R10)              STORE EXIT SAVE ADDRESS          00000027
* --------------------------------------------------------------------* 00000028
*        STATEMENTS FOR SET UP FIELDS NEEDED FOR LIBRM                * 00000029
* --------------------------------------------------------------------* 00000030
         L     R2,0(R1)                PARMS ADDRESS                    00000031
         ST    R2,PARMADDR             SAVE                             00000032
         USING WORKA,R2
         L     R9,PBUFADR              LOAD DATATAB ADDRESS             00000034
         ST    R9,TABRECSA             SET  TABRECS ADDRESS             00000035
*****    CNSLOUT MSG='START-VSSORT'
         USING TABRECS,R9              ESTABLISH ADDRESSABILITY         00000036
* --------------------------------------------------------------------* 00000065
*     PGM-START                                                       * 00000066
* --------------------------------------------------------------------* 00000067
         LH    R3,PSORTNUM             NUMBER OF RECORDS
         MH    R3,RECLEN               * RECORD LENTH
         AH    R3,RECLEN
         GETVIS ADDRESS=(1),LENGTH=(3)                                  00000085
         ST    R3,VISLEN
         LTR   R15,R15                 GETVIS RC ZERO ?                     0000
         BZ    GETVISOK
         MVC   PRETCOD,=CL8'891'       GETVIS failed
         B     PGMEND
GETVISOK DS    0H
         ST    R1,GETVISAD
         LH    R6,RECLEN               RECORD LENGTH
         BCTR  R6,0                    -1 FOR MVC
         STC   R6,MVCLEN
         IC    R6,PSORTLEN             SORT FIELD LENGTH
         BCTR  R6,0                    -1 FOR MVC
         STC   R6,SORTLEN
         MVC   MVC0001+1(1),MVCLEN
         MVC   MVC0002+1(1),MVCLEN
         MVC   MVCMINX+1(1),MVCLEN
         MVC   MVCMIN+1(1),SORTLEN
         MVC   MVCR5+1(1),MVCLEN
         MVC   MVCR7+1(1),MVCLEN
         MVC   CLCL010+1(1),SORTLEN
*
         LH    R3,PSORTNUM             NUMBER OF RECORDS
         L     R4,PBUFADR              ADDR OF INPUT BUFFER
         L     R5,GETVISAD             ADDR OF OUTPUTBUFFER
         LH    R6,RECLEN               RECORD LENGTH
*--------------------------------------------------------
* MOVE ALL RECORDS TO GETVIS-AREA AND CLEAR OLD AREA
*--------------------------------------------------------
MVCLOOP  DS    0H                      SAVE INPUT AREA
         CLC   0(4,R4),=XL4'00000000'
         BNE   MVC0001
         MVC   0(4,R4),=XL4'FFFFFFFF'
MVC0001  MVC   0(0,R5),0(R4)           FILL GETVIS-AREA
MVC0002  MVC   0(0,R4),BLANKS          CLEAR INPUT AREA
MVC0003  DS    0H
         AR    R4,R6
         AR    R5,R6
         BCT   R3,MVCLOOP
*
*--------------------------------------------------------
* NOW SORT
*--------------------------------------------------------
SORT000  DS    0H
         L     R5,PBUFADR              ADDR OF OUTPUTBUFFER
         LH    R6,RECLEN               RECORD LENGTH
CLCL000  DS    0H
MVCMIN   MVC   MINREC(0),MAXCONST      MINREC IS NOW MAXREC TO START
         LH    R3,PSORTNUM             NUMBER OF RECORDS
         L     R4,GETVISAD             ADDR OF INPUT BUFFER
CLCL010  DS    0H
         CLC   0(0,R4),MINREC          COMPARE
         BL    MVCMINX
         B     CLCL015
MVCMINX  MVC   MINREC(0),0(R4)
         ST    R4,LASTMIN              LAST ADDR OF MIN
         MVI   ACTFLAG,X'FF'           MARK, NOT FINISHED YET
CLCL015  DS    0H
         AR    R4,R6                   INCR INPUT REG FOR NXT RECD
         BCT   R3,CLCL010              LOOP
*
         CLI   ACTFLAG,X'FF'             JOB FINISHED ?
         BNE   PGMEND                    YES
         MVI   ACTFLAG,X'00'             RESET
MVCR5    MVC   0(0,R5),MINREC          NO, THIS IS A MINRECORD
         MVC   TEMPNAME(10),MINREC     RH
*****    CNSLOUT ADR=TEMPNAME,LEN=11
         L     R7,LASTMIN
MVCR7    MVC   0(0,R7),MAXCONST        MAKE LAST MIN RECORD TO A MAX
         AR    R5,R6                   FOR NEXT OUTPUT RECORD
         MVI   ACTFLAG,X'00'           RESET ACTIVITY FLAG
*
         B     CLCL000
* --------------------------------------------------------------------* 00000065
*     PGMEND Program-ENd                                              * 00000066
* --------------------------------------------------------------------* 00000067
PGMEND   DS    0H
******   CNSLOUT ADR=MINREC,LEN=8
******   CNSLOUT MSG='PGMEND'
         L     R1,GETVISAD
         L     R3,VISLEN
         FREEVIS ADDRESS=(1),LENGTH=(3)                                  0000008
         LTR   R15,R15                 FREVIS RC ZERO ?                     0000
         BZ    FREVISOK
         MVC   PRETCOD,=CL8'890'       FREVIS failed
FREVISOK DS    0H
         L     R9,PARMADDR         LOAD PARAMETER-ADDR
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
*****    CNSLCCB
* --------------------------------------------------------------------* 00000386
*        WORKFIELDS                                                   * 00000387
* --------------------------------------------------------------------* 00000388
         DS    0D                                                       00000389
*                                                                       00000391
* --------------------------------------------------------------------* 00000404
*        MESSAGE TABLES                                               * 00000405
* --------------------------------------------------------------------* 00000406
         DS    0F                       ALIGNEMENT                      00000407
* --------------------------------------------------------------------* 00000369
*        SAVE AREA DECLARATIONS                                       * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F                                                       00000372
SAVEAREA DC    XL72'00'                 MONITOR TASK SAVE AREA           0000037
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS          00000379
PARMADDR DS    F                                                        00000458
GETVISAD DS    F
VISLEN   DS    F
LASTMIN  DS    F
BLANKS   DC    CL255'                                              '
MAXCONST DC    XL12'FFFFFFFFFFFFFFFFFFFFFFFF'
MINCONST DC    XL12'000000000000000000000000'
MINREC   DS    CL255
MAXREC   DS    CL255
TEST     DC    CL34' '                                                  00000458
MVCLEN   DS    CL1
SORTLEN  DS    CL1
TEMPNAME DS    CL10
ACTFLAG  DS    XL1'00'
*                                                                       00000459
* --------------------------------------------------------------------* 00000424
         LTORG                                                          00000454
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                   * 00000456
* --------------------------------------------------------------------* 00000457
WORKA    DSECT                                                          00000475
         COPY  LIBPARMS                                                 00000460
         DS    0D                                                       00000427
* --------------------------------------------------------------------* 00000474
TABRECS  DSECT                                                          00000475
TABREC   DS    CL80                                                     00000476
* --------------------------------------------------------------------* 00000477
MAPOUT   DSECT
         DS    0F
MAPADDR  DS    0CL2028
MAPNAME  DS    CL8
MAPEYEC  DS    CL8
         DS    CL2
MAPLEN   DS    CL2                MAP-LEN
MAPVCNT  DS    AL1(0)             VAR-FIELD COUNT
MAPVLST  DS    AL3(0)             ADDR MAP-VAR-OUTPUT-LIST
MAPSTART DS    CL2000
BUFFER   EQU   *                   MEMBER BUFFER BEGIN                  00001705
         END                                                            00000481
        BKEND
        CATALS   A.VSSPF,0.0
        BKEND   A.VSSPF
*---------------------------------------------------------------------* 00000041
*  Main SPF-Control-Pgm
*  VSSPF:                        Sub-Pgm
*  BaseRegs:                     5,6,7
*  Internal Functions:
*    1. MAIN000                  Display SPFM005 (Primary)          +
*    2. SELE000                  SELECT Member   (Sel Mem)          +
*    3. GETMEM00                 Get Member      (Call VSLIBRM      +
*                                Edit Member     (Call VSED)        +
*    4. SAVEMEMB                 Save Member     (Call VSPUT)       +
*    5. SUBMITMB                 Submit Member   (Call VSSUB)       +
*    6. UTIL000                  Display SPFM008 (Utilities)        +
*    7. UTIDIR                   Search  Libraries Directories      +
*                                and build StatusTable PSTATAB      +
*                                and Read them   (Call VSRDIR)      +
*    8. GETVIS                   Get virtual storage for Member     +
*    8. FREEVIS                  Free this storage at VSSPF-PgmEnd  +
*  External Functions:
*    1.
*    2.
*    3.
*    4.
*  Parameter in:
*    -                           none
*  Parameter out:
*    LIBPARMS
*  Maps loaded:
*    1.
*    2.
*  Planned:
*    1. TSO-Functionalitiy       more Functions
*    2.
*---------------------------------------------------------------------* 00000041
         TITLE 'VSSPF DISPLAY CONTROL PROGRAM'
         PRINT NOGEN
* --------------------------------------------------------------------* 00000041
VSSPF    START X'000000'
* --------------------------------------------------------------------  00000043
*        REGISTER EQUATES                                               00000044
* --------------------------------------------------------------------  00000045
*                                                                       00000065
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
VSSPF   CSECT
         STM   R14,R12,12(R13)         SAVE REGS in CALLERS SA
         USING VSSPF,R4,R5,R6,R7
         LR    R4,R15                  FIRST BASE READY
         LR    R5,R4
         LA    R5,4095(R5)
         LA    R5,1(R5)                SECND BASE READY
         LR    R6,R5
         LA    R6,4095(R6)
         LA    R6,1(R6)                SECND BASE READY
         LR    R7,R6
         LA    R7,4095(R7)
         LA    R7,1(R7)                THIRD BASE READY
         B     GOON
         DC    C'***** VSSPF*****'     EYECATCHER
GOON     DS    0H
         ST    R13,SAVEAREA+4           SAVE OLD SA IN NEW SA
         LR    R10,R13
         LA    R13,SAVEAREA            LOAD NEW SA
         ST    R13,8(R10)              AND SAVE IT IN OLD SA
* -------------------------------------------
*    SETUP PARAMETERS FOR VSSPF             -
* -------------------------------------------
         L     R2,0(0,R1)              R2 IS MESSAGE CONTROL BLOCK
         ST    R2,PARMADDR
         MVC   SPFPARMS,0(R2)
         BAL   R8,GETVIS00             GET STORAGE DYNAMIC
         L     R12,WORK1                SWITCH TO EDITOR-WORK-1
         USING WORKA,R12                                                 0000009
*----------------------------------------------------------------
*     MAIN-CONTROL COMMUNICATIONS PRIMARY MAP
*----------------------------------------------------------------
MAIN000  DS    0H                   BEGIN TO COMMUNICATE
*****    CNSLOUT MSG='MAIN000'
         MVC   CTOPNAME,=CL10' Primary  '
         MVI   CTOPSW,C'E'          ERASE CCW
         BAL   R8,CTOPMAP           TOP-MAP
         BAL   R8,CUDTMAP           USER/DATE/TIME
         BAL   R8,CPRIMAP           PRIMARY-MAP
         BAL   R8,CBOTMAP           PFKEY-MAP
         LA    R1,READMDAT
         BAL   R8,EXCPRD00          READ MODIFIED
* IOAID --------------------------
         CLC   IOAID(1),=C'3'       PF3 / Back
         BE    PGMEND
* IOCMD --------------------------
         OC    IOCMD(30),BLANKS         FORCE UPPER CASE |                   000
         CLC   IOCMD(30),BLANKS
         BE    MAIN000              No, Input - Do nothing              00000227
*
         CLC   IOCMD(1),=C'1'       BROWSE ?
         BE    SELE000
         CLC   IOCMD(1),=C'2'       EDIT ?
         BE    SELE000
         CLC   IOCMD(1),=C'3'       Utilities ?
         BE    UTIL000
         CLC   IOCMD(1),=C'X'       Exit ?
         BE    PGMEND
*                                   ------------------------------
         MVC  RETADR,=A(MAIN000)    SET RETURN-ADDR
         B    CMD000                Check if other Command entered
*----------------------------------------------------------------
*     SELECT PROCESSING FOR  EDIT/BROWSE-Selection
*----------------------------------------------------------------
SELE000  DS    0H                   BEGIN OF BROWSE-PROCESSING
*****    CNSLOUT MSG='SELE000'
         CLC   IOCMD(1),=C'1'       BROWSE ?
         BNE   SELE005              NO SET EDIT IN LIBPARMS
         MVI   PEDBRSW,C'B'         YES
         B     SELE006
SELE005  DS    0H
         MVI   PEDBRSW,C'E'         EDIT
SELE006  DS    0H
         MVC   RETADR,=A(SELE006)   SET RETURN ADDRESS
*****    CNSLOUT MSG='SELE006'
         MVC   CTOPNAME,=CL10'Selection'
         MVI   CTOPSW,C'E'          ERASE CCW
         BAL   R8,CTOPMAP           TOP-MAP
         BAL   R8,CUDTMAP           USER/DATE/TIME
         BAL   R8,CSELMAP           EDIT-MAP
         BAL   R8,CBOTMAP           PFKEY-MAP
SELE010  DS    0H
         LA    R1,READMDAT
         BAL   R8,EXCPRD00          READ MODIFIED
* IOAID --------------------------
         OC    IOCMD(30),BLANKS     FORCE UPPER CASE |                   0000022
         CLC   IOAID(1),=C'3'       PFK3
         BE    MAIN000              BACKWARDS
         CLC   IOCMD(1),=C'X'
         BE    MAIN000              BACKWARDS
* IOCMD --------------------------
         CLC   IOCMD(30),BLANKS         INPUT ?                              000
         BNE   CMD000
*******  PDUMP IOAREA,IOAREA+100
*******  CNSLOUT ADR=IOAREA
*******  CNSLOUT ADR=IODATA
*
SELE015  DS    0H
         OC    M006SUBL,BLANKS
         OC    M006MEMB,BLANKS
         CLI   M006SUBL,C' '        SUBLIB Missing ?
         BNE   SELE020
         MVC   PRETCOD,=CL3'996'    PLease specify Sublib
         BAL   R8,CALLERR           READMDAT AT SELE010
         B     SELE010              And TRY AGAIN
*
SELE020  DS    0H
         CLI   M006MEMB,C' '        MEMBER FROM SPFM006 MISSING ?
         BNE   SELE025
         MVC   PRETCOD,=CL3'997'    Please specify Member-Name
         BAL   R8,CALLERR           READMDAT AT SELE010
         B     SELE010              And TRY AGAIN
*
SELE025  DS    0H                   Everything OK so far
         MVC   PSLIB(1),M006SUBL    SUBLIB FROM MAP SPFM006
         MVC   PMEMB(8),M006MEMB    MEMBER-NAME
         MVC   RETADR,=A(SELE006)   SET RETURN-ADDR
         MVI   PLIBNAME+1,C'S'      IT IS A SOURCE STMT LIB
         BAL   R8,GETMEM00          GET MEMBER FROM SSLIB
         CLI   PEDBRSW,C'B'
         BE    BROW000              NOW BROWSE MEMBER
         B     EDIT000              OR  EDIT IT
*----------------------------------------------------------------
*     Utility-PROCESSING
*----------------------------------------------------------------
UTIL000  DS    0H                   BEGIN OF Utility-PROCESSING
******   CNSLOUT MSG='UTIL000'
         MVC   CTOPNAME,=CL10' Utilities'
         MVI   CTOPSW,C'E'          ERASE CCW
         BAL   R8,CTOPMAP           TOP-MAP
         BAL   R8,CUDTMAP           USER/DATE/TIME
         BAL   R8,CUTIMAP           EDIT-MAP
         BAL   R8,CBOTMAP           PFKEY-MAP
UTIL001  DS    0H
         LA    R1,READMDAT
         BAL   R8,EXCPRD00          READ MODIFIED
* IOAID --------------------------
         CLC   IOAID(1),=C'3'       PFK3
         BE    MAIN000              BACKWARDS
* IOCMD --------------------------
         OC    IOCMD(30),BLANKS         FORCE UPPER CASE |                   000
         CLC   IOCMD(30),BLANKS
         BE    UTIL000              No, Input - Do nothing              00000227
         CLC   IOCMD(1),=C'X'
         BE    MAIN000              BACKWARDS
         CLC   IOCMD(1),=C'4'       File List
         BE    UTIDIR
         CLC   IOCMD(1),=C'V'       VTOC
         BE    VTOC000
*                                   ------------------------------
         MVC  RETADR,=A(UTIL000)    SET RETURN-ADDR
         B    CMD000                Check if other Command entered
*
*----------------------------------------------------------------
*     VTOC-PROCESSING
*----------------------------------------------------------------
VTOC000  DS    0H                   BEGIN OF Utility-PROCESSING
*****    CNSLOUT MSG='VTOC000'
         BAL   R8,CDASDMAP
         BAL   R8,CALLVTOC
         B     PGMEND
*
*----------------------------------------------------------------
*     GET MEMBER - CALL VSLIBRM - EDIT MEMBER
*----------------------------------------------------------------
GETMEM00 DS    0H
         ST    R8,SAVGETM
*****    CNSLOUT MSG='GETMEM00'
         MVC   RECLEN,=H'80'        SET RECORD-LENGTH TO DEFAULT AGAIN
         L     R12,WORK1             SWITCH TO WORK1                      000001
*
         CLI   PLIBNAME+1,C'S'     IS IT A SOURCE STMT LIB MEMBER ?
         BNE   GETMEM10            NO,READ IT FROM DISK DIRECT
*                                  YES, READ IT WITH VSLIBRM
         MVC   PFUNC,=CL3'GET'
*
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*
         CALL  VSLIBRM              GET MEMBER
         B     GETMEM20
*
GETMEM10 DS    0H
*                                  CCC-HH-RR IS IN PCCHHAR(9)
         MVC   PFUNC,=CL3'C2B'     FUNCTION C2B
         MVI   PCCHHBIF,X'00'      SET RIGHT BINARY FORMAT
         BAL   R8,CALLUTI          CCHHR FROM CHAR TO BIN
         MVC   SAVCCHHR,PCCHHBI    SAVE CCHHR
*******  CNSLOUT ADR=SAVCCHHR,LEN=6
*                                  CCC-HH-RR IS IN PCCHHAR(9)
         MVC   PFUNC,=CL3'CNB'     FUNCTION CHAR NUMBER TO BINARY
         MVC   PCCHHAR(8),=C'00000000'
         MVC   PCCHHAR+4(8),PWORK  NUMBER OF RECORDS
         BAL   R8,CALLUTI          CCHHR FROM CHAR TO BIN
         MVC   PNORL,PCCHHBI+2
         MVC   PCCHHBI,SAVCCHHR
*                                  CCC-HH-RR IS IN PCCHHAR(9)
         BAL   R8,CVSRDSK          GET MEMBER FROM DISK INTO BUFFER
*
GETMEM20 DS    0H
         CLC   PRETCOD,=CL3'000'    EVERYTHING OK ?
         BE    GETMEM30
         CLC   LP1,=CL3'NEW'
         BNE   GETMEND             RC IS OK FOR FUNCTION NEW
         B     GETMEM35
GETMEM30 DS    0H
         CLC   LP1,=CL3'NEW'        FUNCTION NEW
         BNE   GETMEND
         MVC   PRETCOD,=CL3'905'    MEMBER ALREADY ESISTS
GETMEM35 DS    0H
         BAL   R8,CALLERR
         BAL   R8,CALLWAIT
GETMEND  DS    0H
         L     R8,SAVGETM
         BR    R8                   RETURN TO CALLER
SAVGETM  DS    F
SAVCCHHR DS    XL6
* --------------------------------------------------------------------- 00000720
*    MEMBER was found, NOW BROWSE with VSBR
* --------------------------------------------------------------------- 00000720
BROW000  DS    0H
*******  CNSLOUT MSG='BROW000'
         MVC   RETBROW,RETADR       SAVE GLOBAL RETURN-ADDR
         MVC   CTOPNAME(1),PSLIB
         MVI   CTOPNAME+1,C'.'
         MVC   CTOPNAME+2(8),PMEMB
BROW011  DS    0H
         MVI   CTOPSW,C'E'          ERASE CCW
         BAL   R8,CTOPMAP           TOP-MAP
         BAL   R8,CBOTMAP           PFKEY-MAP
         MVC   PFUNC,=CL3'BR0'      YES, FUNKTION ED0 1.BROW
*
BROW012  DS    0H
*****    CNSLOUT MSG='BROW012'
*------------------------------------------------------------
         BAL   R8,BRMEMAP           BROW-MAP-MEMBER
         BAL   R8,CALLVSBR
         LA    R1,WRCCW             LOAD WRITE / NO-ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
*------------------------------------------------------------
BROW015  DS    0H                   NO, GO AHEAD
*****    CNSLOUT MSG='BROW015'
*
*
         LA    R1,READMDAT
         BAL   R8,EXCPRD00          READ MODIFIED
*
         CLI   IOAID,NOAID         GOT ANY DATA ?                       00000176
         BE    BROW015             NO, IGNORE AND GO READ               00000177
         CLI   IOAID,PA1           PA1                                  00000178
         BE    BROW015             YES, IGNORE AND GO READ              00000179
         CLI   IOAID,PA2           PA2                                  00000180
         BE    BROW015             YES, IGNORE AND GO READ              00000181
         CLI   IOAID,PA3           PA3                                  00000182
         BE    BROW015             YES, IGNORE AND GO READ              00000183
         CLI   IOAID,CLEAR         CLEAR KEY                            00000188
         BE    BROW012             YES, GO REDISPLAY                    00000189
*
         CLI   IOAID,PF3           PF3                                  00000201
         BE    PF3BR               YES, GO RETURN                       00000202
         CLI   IOAID,PF7                                                00000201
         BE    PF7BR               PF7   BACKWARDS                      00000202
         CLI   IOAID,PF8                                                00000201
         BE    PF8BR               PF8   FORWARD                        00000202
         CLI   IOAID,ENTER                                              00000201
         BE    ENTERBR             ENTER                                00000202
         B     BROW000
*-----------------------------------------------------------------
PF3BR    DS    0H                                                       00000389
         MVC   PFUNC,PPFUNC        RESTORE PREVIOUS FUNCTION
         MVC   RETADR,RETBROW      RELOAD GLOBAL RETURN ADDR
         L     R8,RETADR           LOAD SELECTED RETURN-ADDR
         BR    R8
*-----------------------------------------------------------------
PF7BR    DS    0H                  BACK COMMAND                         00000389
         CLI   IOCMD,C' '          any CMD-INPUT ?
         BE    PF7BR00             NO
         CLI   IOCMD,C'M'          CMD-INPUT ?
         BE    PF7BRMX             NO
         BAL   R8,PARSANA          YES, CHECK IT
         CLI   NELP+1,X'01'        ONLY 1 PARAMETER
         BNE   PF7BRCMD            NO
         CLI   LP1L,C'N'           AND IT MUST BE NUMERIC
         BNE   PF7BRCMD            NO
         MVC   PSCRL,LP1+6         THATS THE SCOLL AMOUNT
         MVC   PFUNC,=CL3'BR7'     FUNKTION BACKWARD
         B     BROW012
*
PF7BRMX  DS    0H
         MVC   PSCRL,=CL2'MX'      MAXIMAL BACK
         MVC   PFUNC,=CL3'BR7'     FUNKTION BACKWARD
         B     BROW012
*
PF7BR00  DS    0H
         MVC   PSCRL,=H'0'         NORMAL PAGING
         MVC   PFUNC,=CL3'BR7'     FUNKTION BACKWARD
         B     BROW012
*
PF7BRCMD DS    0H                  CHECK IF OTHER CMD ENTERED
         MVC   RETADR,=A(BROW015)  RERURN ADDR, IF INVALID CMD
         B     CMD000
*-------------------------------------------------------------------    00000406
PF8BR    DS    0H                  FORWARD COMMAND                      00000407
         XC    PSCRL,PSCRL         NUMBER OF LINES TO SCROLL
         MVC   PFUNC,=CL3'BR8'     FUNKTION BACKWARD
         CLI   IOCMD,C' '          any CMD-INPUT ?
         BE    PF8BR00             NO
         CLI   IOCMD,C'M'          CMD-INPUT ?
         BE    PF8BRMX             NO
         BAL   R8,PARSANA          YES, CHECK IT
         CLI   NELP+1,X'01'        ONLY 1 PARAMETER
         BNE   PF8BRCMD            NO
         CLI   LP1L,C'N'           AND IT MUST BE NUMERIC
         BNE   PF8BRCMD            NO
         MVC   PSCRL,LP1+6         THATS THE SCOLL AMOUNT
         MVC   PFUNC,=CL3'BR8'     FUNKTION BACKWARD
         B     BROW012
*
PF8BRMX  DS    0H
         MVC   PSCRL,=CL2'MX'      MAXIMAL FORWARD
         MVC   PFUNC,=CL3'BR8'     FUNKTION BACKWARD
         B     BROW012
*
PF8BR00  DS    0H
         MVC   PSCRL,=H'0'         NORMAL PAGING
         MVC   PFUNC,=CL3'BR8'     FUNKTION BACKWARD
         B     BROW012
*
PF8BRCMD DS    0H                  CHECK IF OTHER CMD ENTERED
         MVC   RETADR,=A(BROW015)  RERURN ADDR, IF INVALID CMD
         B     CMD000
*-------------------------------------------------------------------    00000406
ENTERBR  DS    0H                  ENTER-KEY                            00000407
*******  CNSLOUT MSG='ENTERBR'
*                                   allowed Commands for Browse here
         CLC   PRETCOD,=CL3'000'
         BE    ENTERBR1
         MVI   READMSW,X'FF'        SWITCH FOR READ MODIFIED ON
         BAL   R8,CALLERR
         B     ENTERBR9
*                                   allowed Commands for Browse here
ENTERBR1 DS    0H
         MVI   WARNSW,C'0'         WARNING SESSION INACTIVE
         OC    IOCMD(30),BLANKS        FORCE UPPER CASE                     0000
         CLC   IOCMD(30),BLANKS
         BE    ENTERBR9            GO DOWN ONE LINE                     00000227
         CLI   IOCMD,C'X'          GO BACK SAME AS PF3
         BE    PF3BR
         CLC   IOCMD(3),=CL3'TOP'  Top of LIST ?
         BNE   ENTERBR2
         MVC   PSCRL,=CL2'MX'
         MVC   PFUNC,=CL3'BR7'
         B     BROW012
ENTERBR2 DS    0H                  ENTER-KEY                            00000407
******   CNSLOUT MSG='ENTERBR2'
         CLC   IOCMD(3),=CL3'BOT'  Bottom of LIST ?
         BNE   ENTERBR3            NO
         MVC   PSCRL,=CL2'MX'
         MVC   PFUNC,=CL3'BR8'
         B     BROW012
ENTERBR3 DS    0H
******   CNSLOUT MSG='ENTERBR3'
         BAL   R8,PARSANA          PARSE ENTERED PARAMETER
         CLI   LP1,C'F'
         BNE   ENTERBR9
         MVC   PFARG,LP2           2.PARAM HAS TO BE FIND-ARGUMENT
         MVC   PFLEN,LP2L          LENGTH OF PARAM
         MVC   PFUNC,=CL3'BRF'     FUNCTION FIND
         B     BROW012
*        --------------------------------------------------------
ENTERBR9 DS    0H                  ONLY ENTER-KEY - DO NOTHING
*******  CNSLOUT MSG='ENTERBR9'
         MVC   PFUNC,=CL3'ENT'
         B     BROW012
*
ENTERBRE DS    0H                   LOOK IF OTHER COMMANDS ENTERED
******   CNSLOUT MSG='ENTERBRE'
         MVC  RETADR,=A(MAIN000)    SET RETURN-ADDR, IF NO CMD FOUND
         B     CMD000              Check if another Command entered ?
RETBROW  DS    F
* --------------------------------------------------------------------- 00000720
*    Directory List OK, NOW BROWSE with VSDDIR
* --------------------------------------------------------------------- 00000720
BRDIR00  DS    0H
****     CNSLOUT MSG='BRDIR00'
         MVC   RETBRDIR,RETADR      SAVE GLOBAL,RETURN-ADDR
         MVC   CTOPNAME(2),PLIBNAME
         MVC   CTOPNAME+2(8),=CL8'-Direct.'
BRDIR11  DS    0H
         MVI   CTOPSW,C'E'          ERASE CCW
         BAL   R8,CTOPMAP           TOP-MAP
         BAL   R8,CBOTMAP           PFKEY-MAP
         MVC   PFUNC,=CL3'DI0'      YES, FUNKTION DI0 1.BROW
         XC    PSCRL,PSCRL          NUMBER OF LINES TO SCROLL
         MVC   RECLEN,=H'34'
*
BRDIR12  DS    0H
******   CNSLOUT MSG='BRDIR12'
         BAL   R8,CALLERR
*-----------------------------------------------------
         BAL   R8,BRDIRMAP          BROW-MAP-DIRECTORY
         BAL   R8,CVSDDIR           CALL VSDDIR
         LA    R1,WRCCW             LOAD WRITE / NO-ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
*-----------------------------------------------------
BRDIR15  DS    0H                   NO, GO AHEAD
******   CNSLOUT MSG='BRDIR15'
*
         LA    R1,READMDAT
         BAL   R8,EXCPRD00          READ MODIFIED
*
*****    CNSLOUT MSG='NACH VSDDIR'
******   PDUMP  IOAREA,IOAREA+100
*
         CLI   IOAID,NOAID         GOT ANY DATA ?                       00000176
         BE    BRDIR15             NO, IGNORE AND GO READ               00000177
         CLI   IOAID,PA1           PA1                                  00000178
         BE    BRDIR15             YES, IGNORE AND GO READ              00000179
         CLI   IOAID,PA2           PA2                                  00000180
         BE    BRDIR15             YES, IGNORE AND GO READ              00000181
         CLI   IOAID,PA3           PA3                                  00000182
         BE    BRDIR15             YES, IGNORE AND GO READ              00000183
         CLI   IOAID,CLEAR         CLEAR KEY                            00000188
         BE    BRDIR12             YES, GO REDISPLAY                    00000189
*
         CLI   IOAID,PF3           PF3                                  00000201
         BE    UTIDIR00                                                 00000202
         CLI   IOAID,PF7                                                00000201
         BE    PF7BRD              PF7   BACKWARDS                      00000202
         CLI   IOAID,PF8                                                00000201
         BE    PF8BRD              PF8   FORWARD                        00000202
         CLI   IOAID,ENTER                                              00000201
         BE    ENTERBRD            ENTER                                00000202
         B     BRDIR00
*-----------------------------------------------------------------
PF7BRD   DS    0H                  BACK COMMAND                         00000389
         XC    PSCRL,PSCRL         NUMBER OF LINES TO SCROLL
         CLI   IOCMD,C' '          CMD-INPUT ?
         BE    PF7BRD00            NO
         CLI   IOCMD,C'M'          CMD-INPUT ?
         BE    PF7BRDMX            NO
         BAL   R8,PARSANA          YES, CHECK IT
         CLI   NELP+1,X'01'        ONLY 1 PARAMETER
         BNE   PF7BRDCM            NO
         CLI   LP1L,C'N'           AND IT MUST BE NUMERIC
         BNE   PF7BRDCM            NO
         MVC   PSCRL,LP1+6         THATS THE SCOLL AMOUNT
PF7BRD01 DS    0H
         MVC   PFUNC,=CL3'DI7'     FUNKTION BACKWARD
         B     BRDIR12
PF7BRD00 DS    0H
         MVC   PSCRL,=H'0'         NORMAL PAGING
         B     PF7BRD01            NO
PF7BRDMX DS    0H
         MVC   PSCRL,=CL2'MX'      NORMAL PAGING
         B     PF7BRD01            NO
PF7BRDCM DS    0H                  CHECK IF OTHER CMD ENTERED
         MVC   RETADR,=A(BRDIR15)  RERURN ADDR, IF INVALID CMD
         B     CMD000
*-------------------------------------------------------------------    00000406
PF8BRD   DS    0H                  FORWARD COMMAND                      00000407
         XC    PSCRL,PSCRL         NUMBER OF LINES TO SCROLL
         CLI   IOCMD,C' '          CMD-INPUT ?
         BE    PF8BRD00            NO
         CLI   IOCMD,C'M'          CMD-INPUT ?
         BE    PF8BRDMX            NO
         BAL   R8,PARSANA          YES, CHECK IT
         CLI   NELP+1,X'01'        ONLY 1 PARAMETER
         BNE   PF8BRDCM            NO
         CLI   LP1L,C'N'           AND IT MUST BE NUMERIC
         BNE   PF8BRDCM            NO
         MVC   PSCRL,LP1+6         THATS THE SCOLL AMOUNT
PF8BRD01 DS    0H
         MVC   PFUNC,=CL3'DI8'     FUNKTION BACKWARD
         B     BRDIR12
PF8BRD00 DS    0H
         MVC   PSCRL,=H'0'         NORMAL PAGING
         B     PF8BRD01            NO
PF8BRDMX DS    0H
         MVC   PSCRL,=CL2'MX'      NORMAL PAGING
         B     PF8BRD01            NO
PF8BRDCM DS    0H                  CHECK IF OTHER CMD ENTERED
         MVC   RETADR,=A(BROW015)  RERURN ADDR, IF INVALID CMD
         B     CMD000
*-------------------------------------------------------------------    00000406
ENTERBRD DS    0H                  ENTER-KEY                            00000407
*****    CNSLOUT MSG='ENTERBRD'
         MVI   WARNSW,C'0'         WARNING SESSION INACTIVE
*****    CNSLOUT ADR=IOCMD,LEN=30
*****    CNSLOUT ADR=IODATA,LEN=30
         OC    IOCMD(30),BLANKS        FORCE UPPER CASE                     0000
         CLC   IOCMD(30),BLANKS
         BE    M013CMDK            GO FOR MAP013-CMD                    00000227
         CLI   IOCMD,C'X'          BACK ?
         BE    UTIDIR00            YES
         CLC   IOCMD(3),=CL3'TOP'  Top of LIST ?
         BNE   ENTBRD01            NO
         MVC   PSCRL,=CL2'MX'
         MVC   PFUNC,=CL3'DI7'
         B     BRDIR12
ENTBRD01 DS    0H
         CLC   IOCMD(3),=CL3'BOT'  Bottom of LIST ?
         BNE   ENTBRD02            CHeck if another Command entered ?
         MVC   PSCRL,=CL2'MX'
         MVC   PFUNC,=CL3'DI8'
         B     BRDIR12
*                                   allowed Commands for Browse here
ENTBRD02 DS    0H
         BAL   R8,PARSANA
         CLI   LP1,C'L'             LOCATE COMMAND
         BNE   ENTBRD03
         BAL   R8,CTOPMAP
         MVC   PFARG,LP2            LOCATE ARGUMENT
         MVC   PFLEN,LP2L           LOCATE ARGUMENT
         MVC   PFUNC,=CL3'DIL'      FUNCTION LOCATE FOR PGM VSDDIR
         B     BRDIR12
ENTBRD03 DS    0H
         MVC  RETADR,=A(BRDIR12)    SET RETURN-ADDR
         B     CMD000               Check if another Command entered ?
SAVR2    DS    2F
*        --------------------------------------------------------
M013CMDK DS    0H                  ENTER-KEY                            00000407
         OC    M013CMD,BLANKS      FORCE UPPER CASE                     00000225
         CLI   M013CMD,C' '        NO INPUT ?
         BE    BRDIR12             YES, DO NOTHING
         MVC   RETADR,=A(UTIRETRY) SET RETADDR TO READ DIRECT. AGAIN
         MVC   PSBAWORK,IOSBA      TO GET INPUT ROW
         MVC   PFUNC,=CL3'SRC'     FUNCTION SBA TO ROW/COL
         BAL   R8,CALLUTI
         XR    R8,R8               CLEAR R8 FOR IC
         IC    R8,PROW             GET THE CALCULATED INPUT ROW
         SH    R8,=H'3'
         MH    R8,RECLEN
         A     R8,SAVCURR
         MVC   PSLIB,0(R8)         SUBLIB-NAME
         MVC   PMEMB,2(R8)         MEMBER-NAME
         MVC   PCCHHAR(9),11(R8)   CCC-HH-RR
         MVC   PWORK,=CL8'00000000'
         MVC   PWORK+3(5),21(R8)   NUMBER OF MEMBER-RECORDS
         BAL   R8,GETMEM00         GET MEMBER FROM SSLIB
         MVC   PFUNC(2),PLIBNAME   BUILD NEW FUNCTION
         MVC   PFUNC+2(1),M013CMD  BUILD NEW FUNCTION
         CLI   M013CMD,C'B'        SELECTED A MEMBER FOR BROWSE ?
         BE    BROW000             YES
         CLI   M013CMD,C'E'        SELECTED A MEMBER FOR EDIT ?
         BE    EDIT000             YES
         CLI   M013CMD,C'S'        SELECTED A MEMBER FOR EDIT ?
         BE    EDIT000             YES
         CLI   M013CMD,C'D'        SELECTED A MEMBER FOR DELETE ?
         BE    DELET000            YES
******   CLI   M013CMD,C'R'        SELECTED A MEMBER FOR RENAME ?
******   BE    RENAM000            YES
         MVC   PFUNC,=CL3'ENT'
         B     BRDIR12
         EJECT
RETADR   DC    F'0'
RETBRDIR DC    F'0'
*---------------------------------------------------------------------
*    NEW MEMBER REQUESTED, EDIT IT WITH VSED
*---------------------------------------------------------------------
EDITNEW  DS    0H
*****    CNSLOUT MSG='EDITNEW'
         MVC   PSLIB,LP2
         MVC   PMEMB,LP2+2
         MVC   CTOPNAME(1),PSLIB
         MVI   CTOPNAME+1,C'.'
         MVC   CTOPNAME+2(8),PMEMB
         MVI   PLIBNAME+1,C'S'           MAKE IT A SOURCE STMT MEMBER
         BAL   R8,GETMEM00
         MVC   PNORL,=F'05'
         MVC   RECLEN,=H'80'
         L     R8,PBUFADR
         MVC   0(160,R8),BLANKS
         B     EDIT000
         L     R8,RETADR
         BR    R8
*---------------------------------------------------------------------
*    MEMBER was found, NOW EDIT IT WITH VSED
*---------------------------------------------------------------------
EDIT000  DS    0H
         MVC   RETEDIT,RETADR      SAVE GLOBAL RETURN ADDR
******   CNSLOUT MSG='EDIT000'
         MVC   CTOPNAME(1),PSLIB
         MVI   CTOPNAME+1,C'.'
         MVC   CTOPNAME+2(8),PMEMB
EDIT011  DS    0H
         MVI   CTOPSW,C'E'          ERASE CCW
         BAL   R8,CTOPMAP           TOP-MAP
         BAL   R8,CBOTMAP           PFKEY-MAP
         MVC   PFUNC,=CL3'ED0'      YES, FUNKTION ED0 1.Edit
*
EDIT012  DS    0H
*****    CNSLOUT MSG='EDIT012'
******   MVI   READMSW,X'FF'        SWITCH FOR READ MODIFIED ON
******   BAL   R8,CALLERR     RH 24.03.11
*
         BAL   R8,EDITMAP           EDIT-MAP-MEMBER
         BAL   R8,CALLVSED
*
         LA    R1,WRCCW             LOAD WRITE / NO-ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
EDIT015  DS    0H                   NO, GO AHEAD
******   CNSLOUT MSG='EDIT015'
*
         LA    R1,READMDAT
         BAL   R8,EXCPRD00          READ MODIFIED
*
         CLI   IOAID,NOAID         GOT ANY DATA ?                       00000176
         BE    EDIT015             NO, IGNORE AND GO READ               00000177
         CLI   IOAID,PA1           PA1                                  00000178
         BE    EDIT015             YES, IGNORE AND GO READ              00000179
         CLI   IOAID,PA2           PA2                                  00000180
         BE    EDIT015             YES, IGNORE AND GO READ              00000181
         CLI   IOAID,PA3           PA3                                  00000182
         BE    EDIT015             YES, IGNORE AND GO READ              00000183
         CLI   IOAID,CLEAR         CLEAR KEY                            00000188
         BE    EDIT015             YES, GO REDISPLAY                    00000189
*
         MVC   PFUNC,=CL3'EDF'     FORMAT INAREA FROM IOAREA
         BAL   R8,CALLVSED
*
******   MVC   PFUNC,=CL3'EDU'     UPDATE CHANGED DATA
******   BAL   R8,CALLVSED
*
******   MVC   PFUNC,=CL3'EDL'     PROCESS LINE COMMANDS
******   BAL   R8,CALLVSED
*
         CLI   IOAID,PF3           PF3                                  00000201
         BE    PF3INT              YES, GO DEACTIVATE TERMINAL          00000202
****     CLI   INAID,PF5           PF5                                  00000205
****     BE    PF5INT              YES, GO UP 5 LINES                   00000206
****     CLI   INAID,PF6           PF6                                  00000207
****     BE    PF6INT              YES, REPEAT THE PREVIOUS COMMAND     00000208
         CLI   IOAID,PF7                                                00000201
         BE    PF7INT              PF7   BACKWARDS                      00000202
         CLI   IOAID,PF8                                                00000201
         BE    PF8INT              PF8   FORWARD                        00000202
         CLI   IOAID,ENTER                                              00000201
         BE    ENTERINT            ENTER                                00000202
         L     R8,RETADR
         BR    R8                  RETURN TO CALLER
*-----------------------------------------------------------------
PF3INT   DS    0H                                                       00000389
         MVC   PFUNC,PPFUNC        RESTORE PREVIOUS FUNCTION
         L     R8,RETEDIT          LOAD SELECTED RETURN-ADDR
         BR    R8
*
*-----------------------------------------------------------------
PF7INT   DS    0H                  BACK COMMAND                         00000389
         OC    IOCMD,BLANKS        FORCE UPPER CASE                     00000225
         CLI   IOCMD,C' '          any CMD-INPUT ?
         BE    PF7INT00            NO
         CLI   IOCMD,C'M'          CMD-INPUT ?
         BE    PF7INTMX            NO
         BAL   R8,PARSANA          YES, CHECK IT
         CLI   NELP+1,X'01'        ONLY 1 PARAMETER
         BNE   PF7INTCM            NO
         CLI   LP1L,C'N'           AND IT MUST BE NUMERIC
         BNE   PF7INTCM            NO
         MVC   PSCRL,LP1+6         THATS THE SCOLL AMOUNT
         MVC   PFUNC,=CL3'ED7'     FUNKTION BACKWARD
         B     EDIT012
*
PF7INTMX DS    0H
         MVC   PFUNC,=CL3'ED7'     FUNKTION BACKWARD
         MVC   PSCRL,=CL2'MX'      MAXIMAL BACK ?
         B     EDIT012
*
PF7INT00 DS    0H
         MVC   PSCRL,=H'0'         NORMAL PAGING
         MVC   PFUNC,=CL3'ED7'     FUNKTION BACKWARD
         B     EDIT012
PF7INTCM DS    0H                  CHECK IF OTHER CMD ENTERED
         MVC   RETADR,=A(EDIT012)  RERURN ADDR, IF INVALID CMD
         B     CMD000
* ------------------------------------------------------------------    00000406
PF8INT   DS    0H                  FORWARD COMMAND                      00000407
         OC    IOCMD,BLANKS        FORCE UPPER CASE                     00000225
         CLI   IOCMD,C' '          any CMD-INPUT ?
         BE    PF8INT00            NO
         CLI   IOCMD,C'M'          CMD-INPUT ?
         BE    PF8INTMX            NO
         BAL   R8,PARSANA          YES, CHECK IT
         CLI   NELP+1,X'01'        ONLY 1 PARAMETER
         BNE   PF8INTCM            NO
         CLI   LP1L,C'N'           AND IT MUST BE NUMERIC
         BNE   PF8INTCM            NO
         MVC   PSCRL,LP1+6         THATS THE SCOLL AMOUNT
         MVC   PFUNC,=CL3'ED8'     FUNKTION BACKWARD
         B     EDIT012
*
PF8INTMX DS    0H
         MVC   PSCRL,=CL2'MX'      MAXIMAL BACK ?
         MVC   PFUNC,=CL3'ED8'     FUNKTION BACKWARD
         B     EDIT012
*
PF8INT00 DS    0H
         MVC   PSCRL,=H'0'         NORMAL PAGING
         MVC   PFUNC,=CL3'ED8'     FUNKTION BACKWARD
         B     EDIT012
PF8INTCM DS    0H                  CHECK IF OTHER CMD ENTERED
         MVC   RETADR,=A(EDIT012)  RERURN ADDR, IF INVALID CMD
         B     CMD000
* ------------------------------------------------------------------    00000406
ENTERINT DS    0H                  ENTER-KEY                            00000407
         CLC   PRETCOD,=CL3'000'
         BE    ENTERIN1
         MVI   READMSW,X'FF'        SWITCH FOR READ MODIFIED ON
         BAL   R8,CALLERR
         B     ENTERIN9
ENTERIN1 DS    0H                  ENTER-KEY                            00000407
*****    CNSLOUT MSG='ENTERIN1'
         MVI   WARNSW,C'0'         WARNING SESSION INACTIVE
         OC    IOCMD(30),BLANKS        FORCE UPPER CASE                     0000
         CLC   IOCMD(30),BLANKS                                          0000022
         BE    ENTERIN9            NO INPUT - DO NOTHING                  000002
*        ------------------------
         CLC   IOCMD(3),=CL3'SAV'
         BE    SAVEMEMB            GO TO SAVE MEMBER
*        ------------------------
         CLC   IOCMD(3),=CL3'SUB'
         BE    SUBMITMB            GO TO SUBMIT MEMER
*        ------------------------
         CLC   IOCMD(3),=CL3'TOP'  Top of LIST ?
         BNE   ENTERIN2
         MVC   PSCRL,=CL2'MX'
         MVC   PFUNC,=CL3'ED7'
         B     EDIT012
*        ------------------------
ENTERIN2 DS    0H
*****    CNSLOUT MSG='ENTERIN2'
         CLC   IOCMD(3),=CL3'BOT'  Bottom of LIST ?
         BNE   ENTERIN3            NO
         MVC   PSCRL,=CL2'MX'
         MVC   PFUNC,=CL3'ED8'
         B     EDIT012
*        ------------------------
ENTERIN3 DS    0H
******   CNSLOUT MSG='ENTERIN3'
         BAL   R8,PARSANA           PARSE AND ANALYZE LINE PARAMETERS
         CLI   LP1,C'F'             FIND COMMAND ?
         BNE   ENTERIN4             NO
         MVC   PFARG,LP2            YES, 2.PARAM IS SEARCH ARGUMENT
         MVC   PFLEN,LP2L                AND LENGTH OF 2.PARAM
         MVC   PFUNC,=CL3'EFI'      FIND-COMMAND FOR VSED
         B     EDIT012              AND BACK TO VSED
*        ------------------------
ENTERIN4 DS    0H
*****    CNSLOUT MSG='ENTERIN4'
         CLI   LP1,C'C'             CHANGE CMD ?
         BNE   ENTEREND             NO
         MVC   PFARG,LP2            YES, 2.PARAM IS SEARCH ARGUMENT
         MVC   PCARG,LP3            YES, 3.PARAM IS CHANGE ARGUMENT
         MVC   PFLEN,LP2L                AND LENGTH OF 2.PARAM
         MVC   PCSCOPE,=H'0'         INIT LINE-SCOPE
         CLC   LP4(3),=CL3'ALL'
         BNE   ENTERIN5
         MVC   PCSCOPE,=H'9999'      CHANGE FOR ALL LINES
ENTERIN5 DS    0H
         MVC   PFUNC,=CL3'EDC'      CHANGE-COMMAND FOR VSED
         B     EDIT012              AND BACK TO VSED
*        ------------------------
ENTERIN9 DS    0H                   NO INPUT - DO NOTHING
         MVC   PFUNC,=CL3'ENT'
         B     EDIT012
ENTEREND DS    0H                   allowed Commands for Browse here
         MVC   PFUNC,=CL3'ENT'
         MVC   RETADR,=A(EDIT012)   SAVE RETURN-ADDR
         B     CMD000               CHECK FOR ATHE COMMANDS
RETEDIT  DS    F
*----------------------------------------------------------------
*  AUFRUF VSPUT TO SAVE EDITED MEMBER
*----------------------------------------------------------------
SAVEMEMB DS    0H
******   CNSLOUT MSG='SAVEMEMB'
         MVC   PFUNC,=CL3'PUT'
         MVC   IOCMD,BLANKS
         MVC   PRETCOD,=CL3'000'
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*                                   ----------------------------------- 00000720
         CALL  VSPUT                SAVE MEMBER
*
         MVI   READMSW,X'FF'        SWITCH FOR READ MODIFIED ON
         BAL   R8,CALLERR
         MVI   CTOPSW,C'N'          NOERASE
         BAL   R8,CTOPMAP
         B     EDIT012
*----------------------------------------------------------------
*  PARSER-INPUT: UP TO 4 PARAMETERS FROM COMMAND-LINE
*----------------------------------------------------------------
*   OUTPUT: 1.  NELP (NUMBER OF ENTERED PARAMETERS (10 BYTES)
*----------------------------------------------------------------
*          BYTE  1-2 :  NUMBER OF PARAMETERS  (BINARY)
*          BYTE    3 :  TYPE OF   PARAMETER 1 (N=NUM/OTHER=ALPHA)
*          BYTE    4 :  LENGTH OF PARAMETER 1 (BINARY)
*          BYTE    5 :  TYPE OF   PARAMETER 2 (N=NUM/OTHER=ALPHA)
*          BYTE    6 :  LENGTH OF PARAMETER 2 (BINARY)
*          BYTE    7 :  TYPE OF   PARAMETER 3 (N=NUM/OTHER=ALPHA)
*          BYTE    8 :  LENGTH OF PARAMETER 3 (BINARY)
*          BYTE    9 :  TYPE OF   PARAMETER 4 (N=NUM/OTHER=ALPHA)
*          BYTE   10 :  LENGTH OF PARAMETER 4 (BINARY)
*----------------------------------------------------------------
*   OUTPUT: 2.  LINE PARAMETER ( LP1 - LP4  each 8 BYTES)
*----------------------------------------------------------------
*         BYTE   1-4 :  NUMBER IN CHAR, IF TYPE = NUM
*                5-8 :  BINARY NUMBER,  IF TYPE = NUM
*         BYTE   1-8 :  CHARACTERS, IF TYPE NOT NUM
*----------------------------------------------------------------
PARSANA  DS    0H
         STM   R8,R11,PARSANR8      SAVE REGISTERS
*****    CNSLOUT MSG='PARSANA'
*                                                                       00000226
         XC    NELP(10),NELP        CLEAR NELP AND ALL LENGTH FIELDS
         MVC   LPARM,BLANKS         CLEAR ALL TEMP PARAMS
         MVC   LP1(32),BLANKS       CLEAR ALL PARAM-FIELDS
*                                                                       00000226
         LA    R11,LPARM        ADDR OF 1.LINE PARM OUTPUT
         MVI   PARMSW,X'00'     SET SWITCH INACTIVE
PARSA000 DS    0H
         LA    R8,IOCMD         GET START-ADDR OF COMMAND               00000226
         LA    R9,40            LENGTH OF COMMAND FIELD (MAX 40 BYTES)
         SR    R10,R10          LENGTH COUNT PER PARAM
PARSA010 DS    0H
         CLI   0(R8),C' '       IS IT A BLANK ?                         00000735
         BNE   PARSA030         NO,                                     00000736
         CLI   PARMSW,X'00'     YES, SOMETHING ACTIVE ?
         BE    PARSA020         NO
         LA    R10,1(R10)       YES, THEN INCREASE COUNT OF PARAMS
         MVC   0(1,R11),0(R8)        AND MOVE BLANK TO OUTPUT
         LA    R11,1(R11)            AND INCREASE OURPUT ADDR
         MVI   PARMSW,X'00'          AND SWITCH ACTIVE OFF (NXT BLNK)
PARSA020 DS    0H
         LA    R8,1(R8)         INCREASE INPUT ADDR
         BCT   R9,PARSA010      LOOP FOR 40 TIMES
         B     PARSA040
PARSA030 DS    0H
         MVI   PARMSW,X'FF'     SWITCH ACTIVE ON
         MVC   0(1,R11),0(R8)   AND MOVE CHAR  TO OUTPUT
         LA    R11,1(R11)       AND INCREASE OUTPUT ADDR
         B     PARSA020
*
PARSA040 DS    0H
         STH   R10,NELP         SAVE NUMBER OF PARAMS FOUND
         CLI   0(R8),C' '            IF LAST BYTE WAS NOT BLANK
         BE    PARSAEND              THEN
         LA    R10,1(R10)            INCREASE THE NUMBER OF PARMS
         STH   R10,NELP              AND SAVE NUMBER AGAIN
PARSAEND DS    0H
*------------------------------------------------------------------
* SECOND PASS - FIND THE LENGTH AND TYPE OF EACH PARAMETER
*------------------------------------------------------------------
         LA    R8,LPARM             INPUT PARAMETER SEPAR. By 1 BLANK
         LA    R10,LPADLST          LINE PARAMETER ADDRESS LIST
         L     R9,0(R10)            LOAD ADDR OF 1.OUTPUT-VARIABLE
         SR    R11,R11              R11 USED FOR LENGTH COUNT
PARS2010 DS    0H
         CLI   0(R8),C' '           IST IT A BLANK ?
         BE    PARS2020
         MVC   0(1,R9),0(R8)        NO, MOVE IT TO THE OUTPUT-VAR
         LA    R8,1(R8)                 AND INCREASE INPUT ADDR
         LA    R9,1(R9)                 AND INCREASE OUTPUT ADDR
         LA    R11,1(R11)               AND INCREASE COUNT
         B     PARS2010             LOOP
PARS2020 DS    0H                   YES, WAS A BLANK (SEPARATOR)
         LA    R8,1(R8)                  INCREASE INPUT-ADDR
         LA    R10,4(R10)                 LOAD NEXT ADDR (LENGTH)
         L     R9,0(R10)                  LOAD NEXT ADDR (LENGTH)
         STH   R11,0(R9)                 AND SAVE THE LENGTH
         SR    R11,R11                   REINIT LEN REGISTER
         LA    R10,4(R10)                LOAD ADDR OF NEXT VARIABLE
         L     R9,0(R10)                 INTO REG 9
         C     R9,=F'-1'                 IS IT THE END OF ADDR LIST ?
         BNE   PARS2010              NO, DO IT AGAIN
*------------------------------------------------------------------
* THIRD  PASS - FIND OUT IF PARAMETER NUMERIC AND CONVERT TO BIN
*------------------------------------------------------------------
         LA    R10,LPADLST          LINE PARAMETER ADDRESS LIST
PARS3010 DS    0H
         L     R9,0(R10)            LOAD ADDR OF 1.OUTPUT-VARIABLE
         C     R9,=F'-1'                 IS IT THE END OF ADDR LIST ?
         BE    PARS3090             YES, IT'S THE END
         ST    R9,PARMVADR          SAVE ADDR FOR LATER USE
         LA    R10,4(R10)                LOAD NEXT ADDR (LENGTH)
         L     R8,0(R10)                 INTO REG 8
         SR    R11,R11                   INIT R11 FOR FOLLOW IC
         IC    R11,1(R8)                 LENGTH IN REG 8
         BCTR  R11,0                     -1 FOR CLC
         STC   R11,PARSCLCL+1            STORE LEN FOR CLC
         STC   R11,PARSCLCH+1            STORE LEN FOR CLC
PARSCLCL CLC   0(0,R9),LOWNUM       IST IT LOWER F0 ?
         BL    PARSNOTN             YES NOT NUMERIC
PARSCLCH CLC   0(0,R9),HIGHNUM      NO, BUT IS IT > F9 ?
         BH    PARSNOTN             YES, ITS NOT NUMERIC
         MVI   0(R8),C'N'           NO, IT'S NUMERIC, MARK IT IN LEN
         XC    PCCHHAR,PCCHHAR
         LA    R8,PCCHHAR+11        LAST BYTE OF PCCHHAR
         LA    R9,0(R11,R9)         POINT TO LAST BYTE OF VARIABLE
         LA    R11,1(R11)  RH       RELOAD LEN INTO R11
PARS3020 DS    0H
         MVC   0(1,R8),0(R9)        MOVE TO PCCHHAR
         BCTR  R8,0
         BCTR  R9,0
         BCT   R11,PARS3020         LOOP FOR LENGTH
*
         MVC   PFUNC,=CL3'CNB'      FUNCTION CHAR NUMBER TO BINARY
         BAL   R8,CALLUTI           CCHHR FROM CHAR TO BIN
         L     R9,PARMVADR
         MVC   4(4,R9),PCCHHBI+2    SAVE BIN-COUNT
PARSNOTN DS    0H
         LA    R10,4(R10)                LOAD ADDR OF NEXT VARIABLE
         B     PARS3010              AND DO IT AGAIN
*
*
PARS3090 DS    0H
*****    CNSLOUT MSG='3. PASS'
*****    CNSLOUT ADR=LP1
*****    CNSLOUT ADR=NELP,LEN=10
         LM    R8,R11,PARSANR8       YES, RETURN TO CALLER
         BR    R8
PARSANR8 DS    4F
PARMVADR DS    F
LOWNUM   DC    CL4'0000'
HIGHNUM  DC    CL4'9999'
*                                                                       00001540
LP1      DC    CL8' '        LINE PARAM1
LP2      DC    CL8' '        LINE PARAM1
LP3      DC    CL8' '        LINE PARAM1
LP4      DC    CL8' '        LINE PARAM1
*                                                                       00001562
LPARM    DC    CL40' '       TEMP PARAMS
LPADLST  DC    A(LP1)
         DC    A(LP1L)
         DC    A(LP2)
         DC    A(LP2L)
         DC    A(LP3)
         DC    A(LP3L)
         DC    A(LP4)
         DC    A(LP4L)
         DC    XL4'FFFFFFFF'
NELP     DC    H'0'          NUMBER OF ENTERED LINE PARAMS
LP1L     DC    H'0'          LENGTH OF LINE PARAM1
LP2L     DC    H'0'          LENGTH OF LINE PARAM2
LP3L     DC    H'0'          LENGTH OF LINE PARAM3
LP4L     DC    H'0'          LENGTH OF LINE PARAM4
PARMSW   DC    XL1'00'
*----------------------------------------------------------------
*  PROCESS COMMAND (SPFM004 TOP)
*----------------------------------------------------------------
CMD000   DS    0H
*****    CNSLOUT MSG='CMD000'
         BAL   R8,PARSANA           PARSE AND ANALYSE CMD-LINE
*        ------------------------
         CLC   LP1(3),=CL3'DIR'     LINE PARAMETER 1 = DIR
         BE    UTIDIR               YES, GO TO SHOW DIRECTORIES
*        ------------------------
         CLC   LP1(3),=CL3'NEW'     LINE PARAMETER 1 = NEW
         BE    CHECKNEW             YES, EDIT NEW MEMBER
*        ------------------------
         MVC   PRETCOD,=CL3'901'    INVALID COMMAND
         MVI   READMSW,X'FF'        SWITCH FOR READ MODIFIED ON
         BAL   R8,CALLERR
         MVI   CTOPSW,C'N'          NON_ERASE
         BAL   R8,CTOPMAP           RE-INIT TOP-MAP
*        ------------------------
CMDEND   DS    0H
         L     R8,RETADR            LOAD RETURN-ADDR
         BR    R8                   AND RETURN
CHECKNEW DS    0H
         CLC   NELP,=H'2'          PARAM: NEW J.MYJOB
         BNL   EDITN010
         MVC   PRETCOD,=CL3'906'   TOO LESS PARAMS
         BAL   R8,CALLERR
         B     CMDEND
EDITN010 DS    0H
         CLI   LP2+1,C'.'
         BE    EDITNEW
         MVC   PRETCOD,=CL3'907'   PLS SPECIFY SUBLIB AND MEMBER
         BAL   R8,CALLERR
         B     CMDEND
*----------------------------------------------------------------
*  AUFRUF VSSUB TO SUBMIT A MEMBER TO POWER
*----------------------------------------------------------------
SUBMITMB DS    0H
******   CNSLOUT MSG='SUBMITMEMBER'
         MVC   PFUNC,=CL3'SUB'
         MVC   IOCMD,BLANKS
         MVC   PRETCOD,=CL3'000'
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*                                   ----------------------------------- 00000720
         CALL  VSSUB                SAVE MEMBER
*
         MVI   READMSW,X'FF'        SWITCH FOR READ MODIFIED ON
         BAL   R8,CALLERR
         B     EDIT012
*----------------------------------------------------------------
*  DELETE A MEMBER
*----------------------------------------------------------------
DELET000 DS    0H
******   CNSLOUT MSG='DELET000'
         MVC   IOCMD,BLANKS
         MVC   WARNTXT2,=CL25'Are you sure to delete'
         MVC   WARNTXT3,=CL25'Member            ?'
         MVC   WARNTXT3+7(10),PSLIB
         BAL   R8,WARNMAP
         LA    R1,READMDAT
         BAL   R8,EXCPRD00          READ MODIFIED
*       --------------------------
         CLI   IOAID,C'3'           PF3 / Back
         BE    DELETRET
*       --------------------------
         CLI   IOAID,X'7D'          ENTER
         BNE   DELETRET
*       --------------------------
         BAL   R8,MAINT000          CALL MAINT FOR DELETE
*       --------------------------
         MVI   READMSW,X'FF'        SWITCH FOR READ MODIFIED ON
         BAL   R8,CALLERR
DELETRET DS    0H
         L     R8,RETADR
         BR    R8
*----------------------------------------------------------------
*  AUFRUF VSSUB TO CONDENSE A LIBRARY VIA SUBMIT TO POWER
*----------------------------------------------------------------
CONDS000 DS    0H
*****    CNSLOUT MSG='CONDS000'
         MVC   IOCMD,BLANKS
         MVC   PRETCOD,=CL3'000'
         MVC   RETADR,=A(UTIDIR00)
         B     MAINT000
*----------------------------------------------------------------
MAINT000 DS    0H                 CALLS MAINT
*----------------------------------------------------------------
*****    CNSLOUT MSG='MAINT000'
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*                                   ----------------------------------- 00000720
         CALL  VSMAINT              MAINTENANCE PGM
*                                   ----------------------------------- 00000720
*
         MVI   READMSW,X'FF'        SWITCH FOR READ MODIFIED ON
         BAL   R8,CALLERR
         L     R8,RETADR
         BR    R8
*----------------------------------------------------------------
*  AUFRUF VSTAB / STATUSTABLE OF DIRECTORIES
*----------------------------------------------------------------
UTIDIR   DS    0H
******   CNSLOUT MSG='UTIDIR'
         STM   R8,R11,UTISAV
         MVC   PFUNC,=CL3'DIR'
         MVC   IOCMD,BLANKS
         MVC   PRETCOD,=CL3'000'
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*                                   ----------------------------------- 00000720
         CALL  VSTAB                 CREATES SYSTEM STATUS TABLE
*                                   ----------------------------------- 00000720
UTIDIR00 DS    0H
******   CNSLOUT MSG='UTIDIR00'
         MVC   CTOPNAME,=CL10'Libraries '
         MVI   CTOPSW,C'E'          ERASE CCW
         BAL   R8,CTOPMAP           TOP-MAP
         BAL   R8,CUDTMAP           USER/DATE/TIME
         BAL   R8,DIRMAP            DIRECTORY - MAP
         BAL   R8,CBOTMAP           PFKEY-MAP
*                                   ----------------------------------- 00000720
         LA    R1,READMDAT
         BAL   R8,EXCPRD00         READ MODIFIED
*                                   ----------------------------------- 00000720
         CLI   IOAID,C'3'          PF3 ?
         BE    UTIL000             YES, GO BACK
         OC    IOCMD(30),BLANKS        FORCE UPPER CASE                     0000
         CLI   IOCMD,C'X'          BACK ?
         BE    UTIL000             YES, GO BACK
         CLC   IOCMD(30),BLANKS
         BNE   CMD000              GO, CHECK COMMAND                    00000227
******   CNSLOUT ADR=IOAREA
******   CNSLOUT ADR=IODATA
         OC    M009CMD,BLANKS      FORCE UPPER CASE                     00000225
         CLI   M009CMD,C' '        COMMAND ENTERED ?
         BE    UTIDIR              NO, DO NOTHING, LOOP
         CLI   M009CMD,C'I'        INFO ONLY ?
         BNE   UTIC000
         MVC   PFUNC+2(1),M009CMD  TRANSFER COMMAND S/I
         B     UTIC010
UTIC000  DS    0H
         CLI   M009CMD,C'S'        DIRECTORY DISPLAY ?
         BNE   UTIC020
         MVC   PFUNC+2(1),M009CMD  TRANSFER COMMAND S/I
         B     UTIC010
UTIC020  DS    0H
         CLI   M009CMD,C'C'        LIBRARY CONDENSE  ?
         BNE   UTIC030
         MVC   PFUNC+2(1),M009CMD  TRANSFER COMMAND C
         B     UTIC010
UTIC030  DS    0H
         MVC   PRETCOD,=CL3'901'   INVALID FUNCTION
         MVI   READMSW,X'FF'        SWITCH FOR READ MODIFIED ON
         BAL   R8,CALLERR
         MVI   CTOPSW,C'N'          NON_ERASE
         BAL   R8,CTOPMAP           RE-INIT TOP-MAP
         B     UTIDIR    00 RH     GO, TRY AGAIN
UTIC010  DS    0H
         MVC   PPFUNC,PFUNC        SAVE PFUNCTION
         MVC   PSBAWORK,IOSBA      TO GET INPUT ROW
         MVC   PFUNC,=CL3'SRC'     FUNCTION SBA TO ROW/COL
         BAL   R8,CALLUTI
         MVC   PFUNC,PPFUNC        RESTORE PFUNCTION
*
         CLI   PROW,X'07'          WAS SYSTEM CLB SELECTED ?
         BNE   UTISRL00            NO, CHECK SRLB
         MVC   PFUNC(2),=CL2'SC'   YES, SYST CORE IMAGE LIB
         B     UTI100
*
UTISRL00 DS    0H
         CLI   PROW,X'08'          WAS SYSTEM RLB SELECTED ?
         BNE   UTISSL00            NO, CHECK SSLB
         MVC   PFUNC(2),=CL2'SR'   YES, SYST RELOCATABLE LIB
         B     UTI100
*
UTISSL00 DS    0H
         CLI   PROW,X'09'          WAS SYSTEM SLB SELECTED ?
         BNE   UTISPL00            NO, CHECK SPLB
         MVC   PFUNC(2),=CL2'SS'   YES, SYST SOURCE STMT LIB
         B     UTI100
*
UTISPL00 DS    0H
         CLI   PROW,X'0A'          WAS SYSTEM PLB SELECTED ?
         BNE   UTIPCL00            NO, CHECK PCLB
         MVC   PFUNC(2),=CL2'SP'   YES, SYST PROCEDURE LIb
         B     UTI100
*
UTIPCL00 DS    0H
         CLI   PROW,X'0F'          WAS PRIVATE CLB SELECTED ?
         BNE   UTIPRL00            NO, CHECK PRLB
         MVC   PFUNC(2),=CL2'PC'   YES, Priv Core Image LIb
         B     UTI100
*
UTIPRL00 DS    0H
         CLI   PROW,X'10'          WAS PRIVATE RLB SELECTED ?
         BNE   UTIPSL00            NO, CHECK PSLB
         MVC   PFUNC(2),=CL2'PR'   YES, Priv Core Image LIb
         B     UTI100
*
UTIPSL00 DS    0H
         CLI   PROW,X'11'          WAS PRIVATE SLB SELECTED ?
         BNE   UTIDIR00            NO, LOOP
         MVC   PFUNC(2),=CL2'PS'   YES, Priv Core Image LIb
         B     UTI100
UTI100   DS    0H                  CALL VSRDIR
         MVC   PPFUNC,PFUNC        SAVE FUNCTION FOR LATER USE
UTIRETRY DS    0H                  CALL VSRDIR
*******  CNSLOUT ADR=PFUNC
         MVC   PRETCOD,=CL3'000'
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*----------------------------------------------------------
         CALL  VSRDIR               READ SELECTED DIRECTORY
*----------------------------------------------------------
         CLC   PRETCOD,=CL3'000'
         BE    UTI101
         MVI   READMSW,X'FF'        SWITCH FOR READ MODIFIED ON
         BAL   R8,CALLERR           WRITE ERROR
         BAL   R8,CALLERR           AND CLEAR AGAIN
         B     UTIDIR00
UTI101   DS    0H                   EVERYTHING OK
*****    CNSLOUT ADR=PPFUNC,LEN=3
         CLI   PPFUNC+2,C'I'        DIR/LIB-INFORMATION ONLY ?
         BNE   UTI102               NO, SHOW DIRECTORY-MEMBERS
         BAL   R8,STATMAP
         MVI   CTOPSW,C'N'          NOERASE MAP TOPMAP
         BAL   R8,CTOPMAP
         B     UTI110
UTI102   DS    0H
         CLI   PPFUNC+2,C'S'
         BE    BRDIR00              BROWSE DIRECTORY
         B     CONDS000             CONDENSE LIBRARY
UTI110   DS    0H
         LA    R1,READMDAT
         BAL   R8,EXCPRD00          READ MODIFIED
* IOAID --------------------------
         CLC   IOAID(1),=C'3'       PFK3
         BE    UTIDIR00             BACKWARDS
* IOCMD --------------------------
         OC    IOCMD(30),BLANKS         FORCE UPPER CASE |                   000
         CLC   IOCMD(30),BLANKS
         BE    UTI110               No, Input - Do nothing              00000227
         CLC   IOCMD(1),=C'X'
         BE    UTIDIR               BACKWARDS
*                                   ------------------------------
         LM    R8,R11,UTISAV
         B    CMD000                Check if other Command entered
*
         MVI   READMSW,X'FF'        SWITCH FOR READ MODIFIED ON
         BAL   R8,CALLERR
UTIEND   DS    0H
         LM    R8,R11,UTISAV
         B     UTIDIR00
UTISAV   DS    4F
*----------------------------------------------------------------
*  OUTPUT: TOP-MAP
*----------------------------------------------------------------
CTOPMAP  DS    0H
******   CNSLOUT MSG='CTOPMAP'
         STM   R8,R11,SAV8
*
         LOAD  SPFM004,MAPADDR      TOP-MAP OF SPFVS
         L     R10,MAPVCNT          LOAD ADDR-LIST-ADDR
         ICM   R10,8,X00            CLEAR CNT-BYTE
         L     R11,0(R10)           Load 1. VAR-OUT-ADDR MAPNAME
         MVC   0(10,R11),CTOPNAME
         MVC   MAPSTART(1),CTOPWCC  WCC in 1.BYTE OF MAP
*
         CLI   CTOPSW,C'E'          ERASE MAP
         BNE   CTOPM010             NO
         MVC   WRECCW+6(2),MAPLEN   YES, LEN TO CCW
         LA    R1,WRECCW            LOAD WRITE / ERASE-CCW-ADDR
         B     CTOPM020
CTOPM010 DS    0H                   NOERASE
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
         LA    R1,WRCCW             LOAD WRITE / NOERASE-CCW-ADDR
*
CTOPM020 DS    0H                   NOERASE
         BAL   R8,EXCPWR00          EXEC / WAIT FOR I/O COMPLETION.
         MVI   CTOPSW,C' '          RESET ERASE SWITCH
         LM    R8,R11,SAV8          RESTORE R10-R11
         BR    R8                   RETURN TO CALLER
*
CTOPNAME DC    CL10' '              NAME OF TOP-MAP
CTOPSW   DC    CL1' '               SWITCH ERASE CCW OR NOERASE CCW
CTOPWCC  DC    XL1'42'              WRITE CC RESET MDT / NORESET MDT
*----------------------------------------------------------------
*  OUTPUT: USER/DATE/TIME/-MAP
*----------------------------------------------------------------
CUDTMAP  DS    0H                   BEGIN TO COMMUNICATE
******   CNSLOUT MSG='CUDTMAP'
         STM   R8,R11,SAV8          SAVE R8
*
         LOAD  SPFM003,MAPADDR      USER/DATE/TIME-Infos
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
         L     R8,MAPVCNT           LOAD ADDR-LIST-ADDR
         ICM   R8,8,X00             CLEAR CNT-BYTE
         L     R11,0(R8)            Load 1. VAR-OUT-ADDR In R11
         MVC   0(8,R11),LOGUSER     USER-ID
         L     R11,4(R8)            Load 2. VAR-OUT-ADDR In R11
         MVC   0(8,R11),LOGDATE     DATE
         L     R11,8(R8)            Load 3. VAR-OUT-ADDR In R11
         MVC   0(5,R11),LOGTIME     TIME
         LA    R1,WRCCW             LOAD WRITE / NO-ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
*
         LM    R8,R11,SAV8          RESTORE R8-R11
         BR    R8                   RETURN TO CALLER
*----------------------------------------------------------------
*  OUTPUT: WARNING-MAP
*----------------------------------------------------------------
WARNMAP  DS    0H                   BEGIN TO COMMUNICATE
******   CNSLOUT MSG='WARNMAP'
         STM   R8,R11,SAV8          SAVE R8
*
         LOAD  SPFMWARN,MAPADDR     USER/DATE/TIME-Infos
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
         L     R8,MAPVCNT           LOAD ADDR-LIST-ADDR
         ICM   R8,8,X00             CLEAR CNT-BYTE
         L     R11,0(R8)            Load 1. VAR-OUT-ADDR In R11
         MVC   2(25,R11),WARNTXT1   TEXT-1
         L     R11,4(R8)            Load 2. VAR-OUT-ADDR In R11
         MVC   2(25,R11),WARNTXT2   TEXT-2
         L     R11,8(R8)            Load 3. VAR-OUT-ADDR In R11
         MVC   2(25,R11),WARNTXT3   TEXT-3
         L     R11,12(R8)           Load 4. VAR-OUT-ADDR In R11
         MVC   2(25,R11),WARNTXT4   TEXT-4
         LA    R1,WRCCW             LOAD WRITE / NO-ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
*
         MVC   WARNTXT1(100),BLANKS
         LM    R8,R11,SAV8          RESTORE R8-R11
         BR    R8                   RETURN TO CALLER
WARNTXT1 DC    CL25' '
WARNTXT2 DC    CL25' '
WARNTXT3 DC    CL25' '
WARNTXT4 DC    CL25' '
*----------------------------------------------------------------
*  OUTPUT: Primary SPFVS-MAP
*----------------------------------------------------------------
CPRIMAP  DS    0H
******   CNSLOUT MSG='CPRIMAP'
         ST    R8,SAV8              SAVE R8
*
         LOAD  SPFM005,MAPADDR      PRIMARY-MAP OF SPFVS
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
         LA    R1,WRCCW             LOAD WRITE / NO-ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
*
         L     R8,SAV8              RESTORE R8
         BR    R8                   RETURN TO CALLER
*----------------------------------------------------------------
*  OUTPUT: Utility SPFVS-MAP
*----------------------------------------------------------------
CUTIMAP  DS    0H
*****    CNSLOUT MSG='CUTIMAP'
         ST    R8,SAV8              SAVE R8
*
         LOAD  SPFM008,MAPADDR      PRIMARY-MAP OF SPFVS
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
         LA    R1,WRCCW             LOAD WRITE / NO-ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
*
         L     R8,SAV8              RESTORE R8
         BR    R8                   RETURN TO CALLER
*----------------------------------------------------------------
*  OUTPUT: SPFM009-Directories
*----------------------------------------------------------------
DIRMAP   DS    0H                   BEGIN TO COMMUNICATE
****     CNSLOUT MSG='DIRMAP'
         STM   R8,R11,SAV8          SAVE R8
*
         L     R12,WORK1             LOAD ADDR OF STATUSTABLE
         LOAD  SPFM009,MAPADDR      Directories / Libraries
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
         L     R10,MAPVCNT          LOAD ADDR-LIST-ADDR
         ICM   R10,8,X00            CLEAR CNT-BYTE
         L     R11,0(R10)           Load 1. VAR-OUT-ADDR SCLBS
         BCTR  R11,0                -1 TO POINT TO ATTRIBUTE CHAR
         CLC   PSCLB,=XL6'000000000000'   SYSTEM CLB EXISTS ?
         BNE   DIRSR00                    YES
         MVI   0(R11),X'6C'               NO  PROT / DARK
DIRSR00  DS    0H
         L     R11,4(R10)                 Load 2. VAR-OUT-ADDR SRLBS
         BCTR  R11,0                      -1 TO POINT TO ATTRIBUTE CHAR
         CLC   PSRLB,=XL6'000000000000'   SYSTEM CLB EXISTS ?
         BNE   DIRSS00                    YES
         MVI   0(R11),X'6C'               PROT / DARK
DIRSS00  DS    0H
         L     R11,8(R10)                 Load 3. VAR-OUT-ADDR SSLBS
         BCTR  R11,0                      -1 TO POINT TO ATTRIBUTE CHAR
         CLC   PSSLB,=XL6'000000000000'   SYSTEM CLB EXISTS ?
         BNE   DIRSP00                    YES
         MVI   0(R11),X'6C'               NO, PROT / DARK
DIRSP00  DS    0H
         L     R11,12(R10)                Load 4. VAR-OUT-ADDR SPLBS
         BCTR  R11,0                      -1 TO POINT TO ATTRIBUTE CHAR
         CLC   PSPLB,=XL6'000000000000'   SYSTEM PLB EXISTS ?
         BNE   DIRPC00                    YES
         MVI   0(R11),X'6C'               PROT / DARK
DIRPC00  DS    0H
         L     R11,16(R10)                Load 5. VAR-OUT-ADDR PCLBS
         BCTR  R11,0                      -1 TO POINT TO ATTRIBUTE CHAR
         CLC   PPCLB,=XL6'000000000000'   PRIVATE CLB EXISTS ?
         BNE   DIRPR00                    NO
         MVI   0(R11),X'6C'               PROT / DARK
DIRPR00  DS    0H
         L     R11,20(R10)                Load 6. VAR-OUT-ADDR PRLBS
         BCTR  R11,0                      -1 TO POINT TO ATTRIBUTE CHAR
         CLC   PPRLB,=XL6'000000000000'   PRIVATE RLB EXISTS ?
         BNE   DIRPS00                    NO
         MVI   0(R11),X'6C'               PROT / DARK
DIRPS00  DS    0H
         L     R11,24(R10)                Load 7. VAR-OUT-ADDR PSLBS
         BCTR  R11,0                      -1 TO POINT TO ATTRIBUTE CHAR
         CLC   PPSLB,=XL6'000000000000'   PRIVATE SLB EXISTS ?
         BNE   DIRPEND                    NO
         MVI   0(R11),X'6C'               PROT / DARK
DIRPEND  DS    0H
*
*
         LA    R1,WRCCW             LOAD WRITE / NO-ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
*
         LM    R8,R11,SAV8          RESTORE R8-R11
         BR    R8                   RETURN TO CALLER
*----------------------------------------------------------------
*  OUTPUT: SPFM014-DASDs-MAP
*----------------------------------------------------------------
CDASDMAP DS    0H                   BEGIN TO COMMUNICATE
******   CNSLOUT MSG='DASDMAP'
         STM   R8,R11,DASDR8        SAVE R8
         MVI   CTOPSW,C'E'          ERASE CCW
         BAL   R8,CTOPMAP           TOP-MAP
         BAL   R8,CUDTMAP           USER/DATE/TIME
         BAL   R8,CBOTMAP           PFKEY-MAP
*
         L     R12,WORK1             LOAD ADDR OF STATUSTABLE
         LOAD  SPFM014,MAPADDR      DASDs-Map
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
         L     R9,MAPVCNT           LOAD ADDR-LIST-ADDR
         ICM   R9,8,X00             CLEAR CNT-BYTE
         L     R11,0(R9)            Load 1. VAR-OUT-ADDR LINE1
         ST    R11,COL1AD           SAVe 1ST MDT-FIELD
         LA    R10,20               X'14' for Partition COMREG ADDR
         L     R11,0(R10)           LOAD PUBTAB-ADDR
*****    CNSLOUT REG=11
         LH    R10,64(R11)          DISPL X'40' IS PUBTAB-ADDR
*****    CNSLOUT REG=10
         L     R11,COL1AD           RELOAD MAP-ADDR
DASD000  DS    0H
         CLI   0(R10),X'FF'               IS IT PUBTAB-END  ?
         BE    DASDEND                    YES
         CLI   4(R10),X'60'               IS IT A 2311 ?
         BE   DASD2311                    YES
         CLI   4(R10),X'62'               IS IT A 2314 ?
         BE   DASD2314                    YES
         CLI   4(R10),X'63'               IS IT A 3330 M1 ?
         BE   DASD3330                    YES
         CLI   4(R10),X'65'               IS IT A 3330M11 ?
         BE   DASD3331                    YES
         CLI   4(R10),X'67'               IS IT A 3350 ?
         BE   DASD3350                    YES
         CLI   4(R10),X'68'               IS IT A 3340 RPS ?
         BE   DASD334R                    YES
         CLI   4(R10),X'69'               IS IT A 3340 ?
         BE   DASD3340                    YES
         CLI   4(R10),X'70'               IS IT A 3340M70 ?
         BE   DASD3340                    YES
         CLI   4(R10),X'90'               IS IT A 3310/2270 FBA ?
         BE   DASD3310                    YES
DASD010  DS    0H                   INCREASE REGISTER
         LA    R10,8(R10)           NEXT PUBTAB-ENTRY
         CLI   0(R10),X'FF'               IS IT PUBTAB-END  ?
         BE    DASDEND                    YES
         B     DASD000              LOOP
DASD2311 DS    0H
         TM    7(R10),X'F8'        DEVICE ONLINE ?
         BNO   DASD010             NO, IGNORE
         BAL   R8,DASDCUU
         MVC   0(3,R11),PCCHHAR+9
         MVC   4(6,R11),=CL6'- 2311'
         LA    R9,4(R9)             INCREASE OUTPUT ADDR
         L     R11,0(R9)                     OUTPUT ADDR
         B     DASD010              INCREASE AND LOOP
DASD2314 DS    0H
         TM    7(R10),X'F8'        DEVICE ONLINE ?
         BNO   DASD010             NO, IGNORE
         BAL   R8,DASDCUU
         MVC   0(3,R11),PCCHHAR+9
         MVC   4(6,R11),=CL6'- 2314'
         LA    R9,4(R9)             INCREASE OUTPUT ADDR
         L     R11,0(R9)                     OUTPUT ADDR
         B     DASD010              INCREASE AND LOOP
DASD3330 DS    0H
         TM    7(R10),X'F8'        DEVICE ONLINE ?
         BNO   DASD010             NO, IGNORE
         BAL   R8,DASDCUU
         MVC   0(3,R11),PCCHHAR+9
         MVC   4(8,R11),=CL8'- 3330-1'
         LA    R9,4(R9)             INCREASE OUTPUT ADDR
         L     R11,0(R9)                     OUTPUT ADDR
         B     DASD010              INCREASE AND LOOP
DASD3331 DS    0H
         TM    7(R10),X'F8'        DEVICE ONLINE ?
         BNO   DASD010             NO, IGNORE
         BAL   R8,DASDCUU
         MVC   0(3,R11),PCCHHAR+9
         MVC   4(8,R11),=CL8'- 333011'
         LA    R9,4(R9)             INCREASE OUTPUT ADDR
         L     R11,0(R9)                     OUTPUT ADDR
         B     DASD010              INCREASE AND LOOP
DASD334R DS    0H
         TM    7(R10),X'F8'        DEVICE ONLINE ?
         BNO   DASD010             NO, IGNORE
         BAL   R8,DASDCUU
         MVC   0(3,R11),PCCHHAR+9
         MVC   4(7,R11),=CL7'- 3340R'
         LA    R9,4(R9)             INCREASE OUTPUT ADDR
         L     R11,0(R9)                     OUTPUT ADDR
         B     DASD010              INCREASE AND LOOP
DASD3340 DS    0H
         TM    7(R10),X'F8'        DEVICE ONLINE ?
         BNO   DASD010             NO, IGNORE
         BAL   R8,DASDCUU
         MVC   0(3,R11),PCCHHAR+9
         MVC   4(6,R11),=CL6'- 3340'
         LA    R9,4(R9)             INCREASE OUTPUT ADDR
         L     R11,0(R9)                     OUTPUT ADDR
         B     DASD010              INCREASE AND LOOP
DASD3350 DS    0H
         TM    7(R10),X'F8'        DEVICE ONLINE ?
         BNO   DASD010             NO, IGNORE
         BAL   R8,DASDCUU
         MVC   0(3,R11),PCCHHAR+9
         MVC   4(6,R11),=CL6'- 3350'
         LA    R9,4(R9)             INCREASE OUTPUT ADDR
         L     R11,0(R9)                     OUTPUT ADDR
         B     DASD010              INCREASE AND LOOP
DASD3310 DS    0H
         TM    7(R10),X'F8'        DEVICE ONLINE ?
         BNO   DASD010             NO, IGNORE
         BAL   R8,DASDCUU
         MVC   0(3,R11),PCCHHAR+9
         MVC   4(6,R11),=CL6'- 3310'
         LA    R9,4(R9)             INCREASE OUTPUT ADDR
         L     R11,0(R9)                     OUTPUT ADDR
         B     DASD010              INCREASE AND LOOP
DASDEND  DS    0H
*
*
         LA    R1,WRCCW             LOAD WRITE / NO-ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
         BAL   R8,CALLWAIT
*
         LM    R8,R11,DASDR8        RESTORE R8-R11
         BR    R8                   RETURN TO CALLER
*-----------------------------------
DASDCUU  DS    0H                   GET CHAR CUU
         ST    R8,CUUR8
         MVC   PFUNC,=CL3'B2H'      BINARY TO HEXA CHARS
         XC    PCCHHBI,PCCHHBI
         MVC   PCCHHBI+4(2),0(R10)  CUU
*****    CNSLOUT ADR=PCCHHBI
         BAL   R8,CALLUTI           CALL UTILITY PROGRAM
*****    CNSLOUT ADR=PCCHHAR
         L     R8,CUUR8
         BR    R8
*-----------------------------------
DASDR8   DS    4F
CUUR8    DS    F
COL1AD   DS    F
COL2AD   DS    F
COL3AD   DS    F
COL4AD   DS    F
COL5AD   DS    F
*----------------------------------------------------------------
*  OUTPUT:EDIT-SEL-MAP
*----------------------------------------------------------------
CSELMAP  DS    0H
******   CNSLOUT MSG='CSELMAP'
         STM   R8,R11,SAVCSR8
*
         LOAD  SPFM006,MAPADDR      SELECT-MAP OF SPFVS
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
         LA    R1,WRCCW             LOAD WRITE / NO-ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
*
         LM    R8,R11,SAVCSR8
         BR    R8                   RETURN TO CALLER
SAVCSR8  DS    4F
*----------------------------------------------------------------
*  OUTPUT:BROWSE-MAP-DIRECTORY
*----------------------------------------------------------------
BRDIRMAP DS    0H
*******  CNSLOUT MSG='BRDIRMAP'
         ST    R8,SAVBRM8           SAVE R8
*
         LOAD  SPFM013,MAPADDR      BROWSE-MAP OF SPFVS
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
         L     R8,SAVBRM8           RESTORE R8
         BR    R8                   RETURN TO CALLER
SAVBRM8  DS    F
*----------------------------------------------------------------
*  OUTPUT:BROWSE-MAP-MEMBER
*----------------------------------------------------------------
BRMEMAP  DS    0H
******   CNSLOUT MSG='BRMEMAP'
         ST    R8,SAVBRD8           SAVE R8
*
         LOAD  SPFM010,MAPADDR      BROWSE-MAP OF SPFVS
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
         L     R8,SAVBRD8           RESTORE R8
         BR    R8                   RETURN TO CALLER
SAVBRD8  DS    F
*----------------------------------------------------------------
*  OUTPUT:EDIT-MAP SPFM007
*----------------------------------------------------------------
EDITMAP  DS    0H
******   CNSLOUT MSG='EDITMAP'
         ST    R8,SAVED8            SAVE R8
*
         LOAD  SPFM007,MAPADDR      EDIT-MAP OF SPFVS
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
         L     R8,SAVED8            RESTORE R8
         BR    R8                   RETURN TO CALLER
SAVED8   DS    F
*----------------------------------------------------------------
*  OUTPUT:LIB/DIR-STATUS-MAP SPFM012
*----------------------------------------------------------------
STATMAP  DS    0H
*******  CNSLOUT MSG='STATMAP'
         ST    R8,SAVEST8           SAVE REG8
*
         LOAD  SPFM012,MAPADDR      EDIT-MAP OF SPFVS
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
         MVC   PMAPADDR,=A(MAPADDR) ADDR MAP TO PARMS
         MVC   PRETCOD,=CL3'000'
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*------------------------
         CALL  VSPM012              SUBROTINE TO PREP SPMF012
*------------------------
         LA    R1,WRCCW            LOAD WRITE / NO-ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
*
         L     R8,SAVEST8          RESTORE REG8
         BR    R8                  RETURN TO CALLER
SAVEST8  DS    F
*----------------------------------------------------------------
*  OUTPUT: BOTTOM-MAP PFKEYS
*----------------------------------------------------------------
CBOTMAP  DS    0H
******   CNSLOUT MSG='CBOTMAP'
         ST    R8,SAV8              SAVE R8
*
         LOAD  SPFM001,MAPADDR     PFKEY-MAP OF SPFVS
         MVC   WRCCW+6(2),MAPLEN   LEN TO CCW
         LA    R1,WRCCW            LOAD WRITE / NO-ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
*
         L     R8,SAV8              RESTORE R8
         BR    R8                  RETURN TO CALLER
*****    CNSLCCB
*-------------------------------------------------------------------
CALLUTI  DS    0H
*-------------------------------------------------------------------
******   CNSLOUT MSG='CALLUTI'
         ST    R8,UTIR8          SAVE R8
*
         MVC   PRETCOD,=CL3'000'
         LA    R1,PARMS
         ST    R1,PARMLIST
         LA    R1,PARMLIST
         L     R15,VSUTIAD
         BALR  R14,R15
         L     R8,UTIR8            -----------------------------------
         BR    R8                     RETURN TO CALLER
VSUTIAD  DC    V(VSUTI)
UTIR8    DS    F
*-------------------------------------------------------------------
CALLVTOC DS    0H
*-------------------------------------------------------------------
******   CNSLOUT MSG='CALLVTOC'
         ST    R8,VTOCR8         SAVE R8
*
         MVC   PRETCOD,=CL3'000'
         LA    R1,PARMS
         ST    R1,PARMLIST
         LA    R1,PARMLIST
         L     R15,VSVTOCAD
         BALR  R14,R15
         L     R8,VTOCR8           -----------------------------------
         BR    R8                     RETURN TO CALLER
*-----------------------            --------------------------------
VSVTOCAD DC    V(VSVTOC)
VTOCR8   DS    F
*-------------------------------------------------------------------
CALLERR  DS    0H
*-------------------------------------------------------------------
*****    CNSLOUT MSG='CALLERR'
         ST    R8,ERRR8          SAVE R8
*
         LA    R1,PARMS
         ST    R1,PARMLIST
         LA    R1,PARMLIST       CALL VSERROR -WRITE MSG
         L     R15,VSERRAD
         BALR  R14,R15
*
         CLI   READMSW,X'00'     READ MODIFIED WANTED ?
         BE    CALLERR9          NO,
         LA    R1,READMDUM       YES, JUST WAIT OR READ DUMMY
         BAL   R8,EXCPRD00          READ MODIFIED
         MVI   READMSW,X'00'        RESET READM-SWITCH
*
****     LA    R1,PARMS
****     ST    R1,PARMLIST
****     LA    R1,PARMLIST       CALL VSERROR -CLEAR MSG AGAIN
****     L     R15,VSERRAD
****     BALR  R14,R15
*
CALLERR9 DS    0H
         L     R8,ERRR8            -----------------------------------
         BR    R8                     RETURN TO CALLER
*-----------------------            --------------------------------
VSERRAD  DC    V(VSERROR)
ERRR8    DS    F
READMSW  DC    XL1'00'
*-------------------------------------------------------------------
CALLWAIT DS    0H
*-------------------------------------------------------------------
*****    CNSLOUT MSG='CALLWAIT'
         ST    R8,WAIT8          SAVE R8
*
         LA    R1,PARMS
         ST    R1,PARMLIST
         LA    R1,PARMLIST       CALL VSWAIT
         L     R15,VSWAITAD
         BALR  R14,R15
*
         L     R8,WAIT8            -----------------------------------
         BR    R8                     RETURN TO CALLER
*-----------------------            --------------------------------
VSWAITAD DC    V(VSWAIT)
WAIT8    DS    F
*----------------------------------------------------------------
*  CALLVSBR CALL BROWSE-PGM
*----------------------------------------------------------------
CALLVSBR DS    0H
*****    CNSLOUT MSG='CALLVSBR'
         ST    R8,CVSBRR8        SAVE R8
*
         MVC   PMAPADDR,=A(MAPADDR) PREFIX OF MAP SPFM010
         MVC   PRETCOD,=CL3'000'
         LA    R1,IOAREA
         ST    R1,PIOAREAD          IOAREA-ADDR FOR VSBR
         LA    R1,INAREA
         ST    R1,PINAREAD          INAREA-ADDR FOR VSBR
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*
         L     R15,VSBRADR
         BALR  R14,R15
*                                   ----------------------------------- 00000720
*        VSBR                       BROWSE MEMBER
*                                   ----------------------------------- 00000720
         L     R8,CVSBRR8        SAVE R8
         BR    R8                   RETURN TO CALLER
*
CVSBRR8  DS    F
VSBRADR  DC    V(VSBR)
         EJECT
*----------------------------------------------------------------
*  CVSRDSK CALL READ-DISK-PGM
*----------------------------------------------------------------
CVSRDSK  DS    0H
******   CNSLOUT MSG='CALLVSRDSK'
         ST    R8,CRDSKR8          SAVE R8
*
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*
         L     R15,VSRDSKAD
         BALR  R14,R15
*                                   ----------------------------------- 00000720
*        VSRDSK                     READ FROM DSK
*                                   ----------------------------------- 00000720
         L     R8,CRDSKR8           SAVE R8
         BR    R8                   RETURN TO CALLER
*
CRDSKR8  DS    F
VSRDSKAD DC    V(VSRDSK)
         EJECT
*----------------------------------------------------------------
*  CVSDDIR   Directory - BROWSE-PGM
*----------------------------------------------------------------
CVSDDIR  DS    0H
******   CNSLOUT MSG='CVSDDIR'
         ST    R8,DIR8          SAVE R8
*
         MVC   PMAPADDR,=A(MAPADDR) PREFIX OF MAP SPFM010
         MVC   PRETCOD,=CL3'000'
         LA    R1,IOAREA
         ST    R1,PIOAREAD          IOAREA-ADDR FOR VSDDIR
         LA    R1,INAREA
         ST    R1,PINAREAD          INAREA-ADDR FOR VSDDIR
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*
         L     R15,VSDDIRAD
         BALR  R14,R15
*                                   ----------------------------------- 00000720
*        VSDDIR                     Browse Directory
*                                   ----------------------------------- 00000720
         L     R8,DIR8              RELOAD R8
         BR    R8                   RETURN TO CALLER
*
VSDDIRAD DC    V(VSDDIR)
DIR8     DS    F
         EJECT
*----------------------------------------------------------------
*  CALLVSED CALL EDIT-PGM
*----------------------------------------------------------------
CALLVSED DS    0H
*******  CNSLOUT MSG='CALLVSED'
         ST    R8,CALLVSR8          SAVE R8
*
         MVC   PMAPADDR,=A(MAPADDR) PREFIX OF MAP SPFM007
         MVC   PRETCOD,=CL3'000'
         LA    R1,IOAREA
         ST    R1,PIOAREAD          IOAREA-ADDR FOR VSED
         LA    R1,INAREA
         ST    R1,PINAREAD          INAREA-ADDR FOR VSED
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*
         L     R15,VSEDADR
         BALR  R14,R15
*                                   ----------------------------------- 00000720
*        VSED                       EDIT MEMBER
*                                   ----------------------------------- 00000720
         BR    R8                   RETURN TO CALLER
*
CALLVSR8 DS    F
VSEDADR  DC    V(VSED)
         EJECT
*----------------------------------------------------------------
PDUMP    DS   0H
*----------------------------------------------------------------
         ST   R1,SAV1
         PDUMP VSSPF,IOEND
         L    R1,SAV1
         BR   R8
SAV1     DS   F
SAV7     DS   F
SAV8     DS   F
SAV9     DS   F
SAV10    DS   F
SAV11    DS   F
SAV12    DS   F
SAV13    DS   F
X00      DC   XL1'00'
SAVR8    DS    F
LOGONSW  DC    CL1'1'
*----------------------------------------------------------------
         LTORG
*----------------------------------------------------------------
*     PGMEND - Programm END
*----------------------------------------------------------------
PGMEND   DS   0H
******   CNSLOUT MSG='PGMEND'
         CLC   LOGUSER(3),=CL3'891'         GETVIS failed ?
         BE    PGMEND00                  YES, THEN NO FREEVIS
         BAL   R8,FREVIS00               SET STOR FREE
PGMEND00 DS    0H
         L     R1,PARMADDR               LOAD PARAMETER-ADDR
         MVC   0(L'SPFPARMS,R1),SPFPARMS
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                        RETURN TO CALLER-PGM
         EJECT
*----------------------------------------------------------------
*     EXCP - EXECUTE CHANNEL PROGRAM READ MOFIFIED
*----------------------------------------------------------------
EXCPRD00 DS    0H                Loop
         STCM  R1,7,EXCPCCB+9    STORE CCW-ADDR in CCB.
         STM   R8,R9,SAVX8
         C     R1,=A(READMDAT)   DATA-CCW ?
         BNE   EXCPRD0A          NO, DUMM-CCW
         LA    R8,24
         LA    R9,IOAREA
EXCPRDLO DS    0H
         MVC   0(80,R9),BLANKS   CLEAR IOAREA
         LA    R9,80(R9)         INCREASE OUTADDR
         BCT   R8,EXCPRDLO       LOOP
         LA    R8,IOAID          YES
         B     EXCPRD0E
EXCPRD0A DS    0H                                                       00001206
         LA    R8,IODAID           CLEAR CURRENT WITH BLANKS            00001206
EXCPRD0E DS    0H                  CLEAR IOAREA WITH BLANKS             00001206
         LA    R1,EXCPCCB        Issue COMMAND
EXCPRD01 DS    0H                Loop
         EXCP  (1)
*
         ST     1,SAV1
         SETIME 1,EXCPTECB  INTERVAL SET 1 SECDS
         WAIT     EXCPTECB  WAIT
         L      1,SAV1
*
         TM    2(1),X'80'    ECP POSTED ?
         BO    EXCPRD02      Yes, CHECK AID
         SVC   7             NO SVC 7
EXCPRD02 DS    0H
         CLI   0(R8),X'60'   NO DATA INPUT YET ? IOAID / IODAID
         BE    EXCPRD01      YES, LOOP FOR READ
         L     R8,SAVX8
         BR    R8            OK, FINISHED
SAVX8    DS    2F
EXCPTECB TECB                CREATE TIMER EVENT CONTROL BLOCK
*----------------------------------------------------------------
*     EXCP - EXECUTE CHANNEL PROGRAM WRITE ERASE
*----------------------------------------------------------------
EXCPWR00 DS    0H
         STCM  R1,7,EXCPCCB+9    STORE CCW-ADDR   in CCB.
         LA    R1,EXCPCCB        Issue Command
         EXCP  (1)
         WAIT  (1)
         BR    R8
*
* ------------------------------------------------------------------
*    GETVIS - GET THE NESSESSARY STORAGE                           -
* ------------------------------------------------------------------
GETVIS00 DS    0H                                                       00000078
******   CNSLOUT MSG='GETVIS00'
         L     R0,=F'737600'       BUFFER STORAGE (9220 * 80)           00000079
*                                         1000 LINES  (EDITOR STACK)    00000080
*                                  5000 + 3000 LINES  (WORK1, WORK2)    00000081
*                                       +  220 LINES  (HELP)            00000082
         A     R0,=A(PARMSL*3)          +              CONTROL AREAS    00000083
*                                                                       00000084
         GETVIS ADDRESS=(1),LENGTH=(0)                                  00000085
         LTR   R15,R15             GETVIS RC ZERO ?                     00000087
         BZ    GETVISOK
         MVC   LOGUSER,=CL8'891'    GETVIS failed
         B     PGMEND
*                                                                       00000089
GETVISOK DS    0H                                                       00000090
******   CNSLOUT MSG='GETVISOK'
         ST    R1,VISADR           GETVIS ADDRESS
         ST    R1,NXTSTCK
         A     R1,=F'80000'        1000 LINES OFFSET                    00000093
*                                                                       00000094
         LR    R12,R1               ADDRESSING WORK1                     0000009
         USING WORKA,R12                                                 0000009
*                                                                       00000094
         MVC   PSTACK,VISADR       SAVE EDITOR STACK ADDR
         MVC   PNXTSTCK,NXTSTCK
*                                                                       00000094
         ST    R1,WORK1            WORK1 ADDRESS
         ST    R1,PWORK1           WORK1 ADDRESS
         LA    R1,BUFFER           GET BUFFER ADDRESS                   00000098
         ST    R1,PBUFADR          SAVE IT                              00000099
         A     R1,=F'400000'       5000 LINES OFFSET                    00000100
         ST    R1,PENDBUF          SAVE END BUFFER                      00000101
         MVC   RECLEN,=H'80'       REC LENGTH DEFAULT                   00000102
*                                                                       00000103
         LR    R12,R1               ADDRESSING WORK2                     0000010
         ST    R1,PWORK2           WORK2 ADDRESS                        00000105
         LA    R1,BUFFER           GET BUFFER ADDRESS                   00000106
         ST    R1,PBUFADR          SAVE IT                              00000107
         A     R1,=F'240000'       3000 LINES OFFSET                    00000108
         ST    R1,PENDBUF          SAVE END BUFFER                      00000109
         MVC   RECLEN,=H'80'       REC LENGTH DEFAULT                   00000110
*                                                                       00000111
         LR    R12,R1               ADDRESSING HELP                      0000011
         ST    R1,PHELP            HELP ADDRESS                         00000113
         LA    R1,BUFFER           GET  BUFFER ADDRESS                  00000114
         ST    R1,PBUFADR          SAVE IT                              00000115
         A     R1,=F'17600'        220 LINES OFFSET                     00000116
         ST    R1,PENDBUF          SAVE END BUFFER                      00000117
         L     R12,PWORK1           SWITCH TO WORK1                      0000011
         BR    R8                  RETURN TO CALLER                     00000119
*                                                                       00000111
* ------------------------------------------------------------------
*    FREEVIS - FREE THE OCCUPIED STORAGE                           -
* ------------------------------------------------------------------
FREVIS00 DS    0H                                                       00000078
******   CNSLOUT MSG='FREVIS00'
         L     R0,=F'737600'
         A     R0,=A(PARMSL*3)
         L     R1,VISADR
         FREEVIS ADDRESS=(1),LENGTH=(0)                                  0000008
         LTR   R15,R15            FREEVIS RC ZERO ?                     00000087
         BZ    FREVISOK
         MVC   LOGUSER(3),=CL3'890'  FREEVIS failed
         BR    R8                                                       00000089
FREVISOK DS    0H                                                       00000090
*****    CNSLOUT MSG='FREVISOK'
         BR    R8                                                       00000088
*----------------------------------------------------------------
         TITLE 'EXCP DISPLAY HANDLER - CONSTANTS AND VARIABLES'
*----------------------------------------------------------------
         DS    0D
EXCPCCB  CCB   SYS007,DUMMY,X'0000'
*
*
WRECCW   CCW   X'05',MAPSTART,X'20',L'MAPSTART   WRITE-ERASE
WRCCW    CCW   X'01',MAPSTART,X'20',L'MAPSTART   WRITE-NOERASE
READMDAT CCW   X'06',IOAREA,X'20',L'IOAREA       READ MODIFIED DATA
READMDUM CCW   X'06',IODUMMY,X'20',L'IODUMMY     READ MODIFIED DUMMY
READBCCW CCW   X'02',IOAREA,X'20',L'IOAREA       READ WHOLE BUFFER
*
*----------------------------------------------------------------
* BUFFER CONTROL ORDERS AND ORDER CODES
*----------------------------------------------------------------
* SF     X'1D',ATTR             START FIELD, ATTRIBUTE
* SBA    X'11',ADR1,ADR2        SET BUFFER ADDRESS, ADR1, ADR2
* IC     X'13'                  INSERT CURSOR
* PT     X'05'                  PROGRAM TAB
* RA     X'3C',ADR1,ADR2,C' '   REPEAT TO ADRESS, CHAR TO REPEAT
* SFE    X'29',COUNT,TYPE,VALUE START FIELD EXTENDED
* EUA    X'12',ADR1,ADR2        ERASE UNPROT TO ADDRESS
* MF     X'2C',COUNT            MODIFY FIELD
* SA     X'2B',TYPE,VALUE       SET ATTRIBUTE
*-------------------+---------1----------2----------3----------------
SCALE    DC    C'....+....1....+....2....+....3....+....4....+....5....*00001599
               +....6....+....7... (X)'                                 00001600
*
ADDSW    DC    C'0'                ADD COMMAND SW                       00001605
WARNSW   DC    C'0'                ADD COMMAND SW                       00001605
HELPSW   DC    C'0'                HELP ACTIVE SW                       00001606
MSGLEN   DS    F                                                        00001575
FLAGS    DC    X'00'               BYTE OF FLAGS                        00001576
*
PARMLIST DC    A(0)                BASE-ADDRESS OF SPFPARMS-DSECT
HELPBEG  DS    F                   HELP BEG SCREEN ADDR                 00001627
SAVINPA  DS    F                   SAVE INPUT LINE ADDR                 00001628
SAVRECA  DS    F                   SAVE BUFFER LINE ADDR                00001629
SAVLINES DS    F                   SAVE LINES TO ADD, DUP, INS          00001630
EDLMDTA  DS    F                   SAVE EDITOR LINE MDT ADDR            00001631
VISADR   DS    F                   SAVE EDITOR LINE MDT ADDR            00001631
WORK1    DS    F                   SAVE EDITOR LINE MDT ADDR            00001631
NXTSTCK  DS    F                   SAVE EDITOR LINE MDT ADDR            00001631
*
BLANKS   DC    255CL1' '
SAVEAREA DS    18F
PARMADDR DS    F
SPFPARMS DS    0CL21
LOGUSER  DS    CL8                                                      00001658
LOGDATE  DS    CL8                                                      00001659
LOGTIME  DS    CL5                                                      00001660
*---------------------------------------------------------------------
*    DUMMY
*---------------------------------------------------------------------
DUMMY   EQU   *
  DC    XL1'C3'   WCC Reset MDT
       $SBA  (22,6,N)
  DC    C'*** DUMMY ***'
DUMMYL   EQU   *-DUMMY
IODUMMY  DS    0XL20
IODAID   DS    XL1
*---------------------------------------------------------------------
*        STORAGE-AREA FOR INBOUND-MSGS
*---------------------------------------------------------------------
         DS    CL2
IOAREA   DS    0CL1840                                                   0000166
IOAID    DS    CL1                 AID                                  00001658
IOCURSBA DS    CL2                 CURSOR SBA                           00001659
IOCMD11  DS    CL1                 CONTROL SBA                          00001660
IOCMDSBA DS    CL2                 SBA                                  00001660
IOCMD    DS    CL40    8 RH        TOP-MAP-COMMAND OPTION===>           00001661
IOSBA11  DS    CL1                 1 IOSBA-CMD = X'11' or X'51'         00001662
IOSBA    DS    CL2                 SET BUFFER ADDRESS SBA               00001662
IODATA   EQU   *                   INPUT ADDR FOR ALL MAPS
         ORG   IODATA              *----------------SPFM006--------
M006SUBL DS    CL1                 *  SUBLIB
         DS    CL3                 *  SBA                                  00001
M006MEMB DS    CL8                 *  MEMBER
         DS    CL8                 *                                     0000166
         ORG   IOAREA+1840         *--------------------------------
         ORG   IODATA              *----------------SPFM009---------
M009CMD  DS    CL1                 *  SELECTION I - INFORMATION
         DS    CL3                 *            S - DIRECTORY
         ORG   IOAREA+1840         *--------------------------------
         ORG   IODATA              *----------------SPFM009---------
M013CMD  DS    CL1                 *  SELECTION I - INFORMATION
         DS    CL3                 *            S - DIRECTORY
         ORG   IOAREA+1840         *--------------------------------
IOEND    EQU   *                                                        00001665
         ORG
         DC    CL16'****************'
* --------------------------------------------------------------------  00001509
         DS    CL2                 DON'T REMOVE IT                      00001645
INAREA   DS    0CL1840             FORMATTED INPUT AREA                 00001646
INEDIA   DS    CL1                 EDITOR LINE MDT                      00001652
INEDI1   DS    CL6                 EDITOR LINE COMMAND * 22             00001653
INLINA   DS    CL1                 DATA LINE MDT                        00001650
INLIN1   DS    CL72                DATA LINE * 22                       00001651
         ORG   INAREA+1840                                              00001654
INEND    EQU   *                                                        00001655
         ORG
*                                                                       00001656
* --------------------------------------------------------------------  00001509
*        EQUATES                                                        00001510
* --------------------------------------------------------------------  00001511
FOURTY2  EQU   X'42'               RFT RECEIVED COMP CODE               00001512
SIXTY4   EQU   X'64'               DEVICE BUFFER CLOBERED COMP CODE     00001513
NOALARM  EQU   X'C3'               WCC WITHOUT ALARM                    00001514
ALARM    EQU   X'C7'               WCC WITH ALARM                       00001515
NOAID    EQU   X'60'               NO ATTENTION ID GENERATED            00001516
CLEAR    EQU   X'6D'               CLEAR KEY                            00001517
PA1      EQU   X'6C'               PA1 KEY                              00001518
PA2      EQU   X'6E'               PA2 KEY                              00001519
PA3      EQU   X'6B'               PA3 KEY                              00001520
PF1      EQU   X'F1'               PF1 KEY  - HELP                      00001521
PF2      EQU   X'F2'               PF2 KEY  - RECALL LAST COMMAND       00001522
PF3      EQU   X'F3'               PF3 KEY  - QUIT                      00001523
PF4      EQU   X'F4'               PF4 KEY  - SWITCH LOGICAL SCREEN     00001524
PF5      EQU   X'F5'               PF5 KEY  - UP 5                      00001525
PF6      EQU   X'F6'               PF6 KEY  - REPEAT PREVIOUS COMMAND   00001526
PF7      EQU   X'F7'               PF7 KEY  - BACK                      00001527
PF8      EQU   X'F8'               PF8 KEY  - FORWARD                   00001528
PF9      EQU   X'F9'               PF9 KEY  - TOP                       00001529
PF10     EQU   X'7A'               PF10 KEY - VIEW 1 72                 00001530
PF11     EQU   X'7B'               PF11 KEY - VIEW 9 80                 00001531
PF12     EQU   X'7C'               PF12 KEY - BOTTOM                    00001532
ENTER    EQU   X'7D'               ATTENTION ID FOR ENTER KEY           00001533
*---------------------------------------------------------------------
         LTORG
*---------------------------------------------------------------------  00001666
*        LOAD-AREA FOR OUTBOUND-MAPS
*---------------------------------------------------------------------
         DS    0F
MAPADDR  DS    0CL2028
MAPNAME  DS    CL8
MAPEYEC  DS    CL8
         DS    CL2
MAPLEN   DS    CL2                MAP-LEN
MAPVCNT  DS    AL1(0)             VAR-FIELD COUNT
MAPVLST  DS    AL3(0)             ADDR MAP-VAR-OUTPUT-LIST
MAPSTART DS    CL2000
* --------------------------------------------------------------------  00001666
*        LTORG                                                          00001666
* --------------------------------------------------------------------  00001666
WORKA    DSECT                     DYNAMIC WORK AREAS                   00001667
*---------------------------------------------------------------------  00001666
         COPY  LIBPARMS
BUFFER   EQU   *                   MEMBER BUFFER BEGIN                  00001705
* --------------------------------------------------------------------  00001666
         END
        BKEND
        CATALS   A.VSSUB,0.0
        BKEND   A.VSSUB
*------------------------------------------------------------------
*     NAME: VSSUB
*     TYPE: SUB-ROUTINE
* FUNCTION: Submit a member to PoWER/VS-Rdr-Queue for execution
*
* INTERNAL
*
*
* EXTERNAL
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
         TITLE 'VSSUB - POWER SUMBIT'
         PRINT GEN
VSSUB    CSECT
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
VSSUB   CSECT
         USING VSSUB,R15               ESTABLISH ADDRESSABILITY
         STM   R14,R12,12(R13)         SAVE REGS in CALLERS SA
         DROP  R15                     R5,R6,R7 PROGRAM BASE REGS
         USING VSSUB,R5,R6,R7
         LR    R5,R15                  FIRST BASE READY
         LR    R6,R15
         LA    R6,4095(R6)
         LA    R6,1(R6)                SECND BASE READY
         LR    R7,R6
         LA    R7,4095(R7)
         LA    R7,1(R7)                THIRD BASE READY
         B     GOON
         DC    C'***** VSSUB*****'     EYECATCHER
GOON     DS    0H
         ST    R13,NEWSAVE+4           SAVE OLD SA IN NEW SA
         LR    R10,R13
         LA    R13,NEWSAVE             LOAD NEW SA
         ST    R13,8(R10)              AND SAVE IT IN OLD SA
* -------------------------------------------
*    SETUP PARAMETERS FOR VSSUB             -
* -------------------------------------------
         L     R2,0(0,R1)              R2 IS MESSAGE CONTROL BLOCK
         USING WORKA,R2
         ST    R2,PARMADDR
*-----------------------------
         CLC   PFUNC,=CL3'SUB'         IS IT A POWER SUB COMMAND ?
         BNE   INVFUNC                 NO, THEN INVALID FUNCTION
*-----------------------------
         MVC   JOBNAME,PMEMB           JOBNAME SEE R11
         L     R9,PBUFADR              BUFFERADR
         ST    R9,TABRECSA
         L     R4,PNORL                # OF JOB RECORDS
         AH    R4,=H'15'               INCR FOR 10 JECL-CARDS
         MH    R4,=H'88'               LENGTH = NUMBER * BUFFER-LEN
         GETVIS ADDRESS=(1),LENGTH=(4)
         LTR   R15,R15
         BNZ   GVERROR
         ST    R1,PWRBUFPT             SAVE POWER-BUFFER-TEST
         ST    R1,PWRBUFAD             SAVE POWER-BUFFER-ADDR
         ST    R4,PWRBUFLN             SAVE POWER-BUFFER-LEN
*--------------------------------------------------------------------
*    POWER - SUBMIT MEMBER to POWER/VS FOR EXECUTIOM                *
*--------------------------------------------------------------------
         L     R9,PBUFADR              R9 = A(FIRST CARD) FROM BUFFER
         L     R1,PWRBUFAD             R1 = A(FIRST POWER CARD)
         L     R4,PNORL                R4 = # OF RECORDS
PWRJOBL1 DS    0H
         LR    R15,R1                  R15 = A(CURRENT POWER CARD)
         LA    R15,88(R15)             ADDR OF NEXT POWER CARD
         MVC   8(80,R1),0(R9)          MOVE TO POWER WITH 8 OFFSET
         ST    R15,0(R1)               STORE IT IN PREVIOUS CARD
         LA    R9,80(R9)               POINT TO NEXT INPUT CARD
         LA    R1,88(R1)               POINT TO NEXT OUTPUT CARD
         BCT   R4,PWRJOBL1             ANY MORE CARDS ?
*                                      YES, LAST REC-ADR = 0
         MVC   0(88,R1),DOSENDJ        /&
         ST    R1,PWRBUFAD             SAVE NXT OUTPUT ADDR
*                                      YES, LAST REC-ADR = 0
*****    LA    R15,88
*****    SR    R1,R15                  POINT TO LAST CARD
*****    SLR   R15,R15
*****    ST    R15,0(R1)               SET LAST CARD
         B     POINTSPL                GO, and exec
POINTSPL DS    0H
         LA    R10,POINTSPL            RH RETURN-ADDR AFTER PWRWAIT
*-------------------------
*---   CONNECT TO POWER/VS
*-------------------------
         LA    R12,SUBSPL              POINT TO PUTSPOOL SPL CONTROL
         USING SPLMAP,R12
         STCM  R12,7,ICRXECB+5         SAVE ADDRESS IN XECB
         XECBTAB TYPE=DEFINE,XECB=ICRXECB,ACCESS=XWAIT
         LTR   R15,R15                 ANY ERRORS
         BNZ   PWRWAIT                 YES = GO ASSUME BUSY
*--------------------------
*---   EXECUTE POWER
*--------------------------
         L     R8,PWRBUFPT             R8 = A(JCL)
         LA    R11,JOBNAME
         PUTSPOOL SPL=(R12),CBUF=(R8),                                 X
               JOBN=(R11),                                             X
               DISP=D,                                                 X
               CLASS=P,                                                X
               PBUF=FEEDBACK
*---------------------------
*---   DISCONN FROM POWER/VS
*---------------------------
         XECBTAB TYPE=DELETE,XECB=ICRXECB
         CLI   SPER,0                  ANY ERRORS
         BNE   PWRERR                  YES - GO HANDLE
         MVC   PRETCOD,=CL3'000'
         B     PWREXIT                 NO - GO RETURN
PWRERR   DS    0H                      ADD ANY ERROR HANDLING HERE
         MVC   PRETCOD,=CL3'999'        SET RC NOT OK
         B     PWREXIT
GVERROR  DS    0H                      ADD ANY ERROR HANDLING HERE
         MVC   PRETCOD,=CL3'885'       GETVIS-ERROR
         B     PWREXIT
PWRWAIT  DS    0H
         SETIME 5,TECB                 WAIT 5 SECONDS AND TRY AGAIN
         WAIT  TECB
         BR    R10
PWREXIT  DS    0H
         L     R0,PWRBUFLN
         L     R1,PWRBUFPT
         FREEVIS ADDRESS=(1),LENGTH=(0)
         LTR   R15,R15
         BZ    FVOK
         MVC   PRETCOD,=CL3'894'       FREEEVIS-ERROR
         B     END001
FVOK     DS    0H                      ADD ANY ERROR HANDLING HERE
         MVC   PRETCOD,=CL3'000'       GETVIS-ERROR
END001   DS    0H
         CLC   PRETCOD,=CL3'000'
         BNE   ENDSUB10
         MVC   PRETCOD,=CL3'886'      MEMBER SAVED
         B     PGMEND
ENDSUB10 DS    0H
         MVC   PRETCOD,=CL3'887'      MEMBER SAVED
         B     PGMEND
INVFUNC  DS    0H
         MVC   PRETCOD,=CL3'883'      FALSCHE FUNKTION MODUL VSSUB
         B     PGMEND
PGMEND   DS    0H
         L     R1,PARMADDR         LOAD PARAMETER-ADDR
*******  MVC   0(L'PARMS,R1),PARMS
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
* --------------------------------------------------------------------* 00000369
*        SAVE AREA DECLARATIONS                                       * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F                                                       00000372
NEWSAVE  DC    18F'0'                   MONITOR TASK SAVE AREA           0000037
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS          00000379
LEN1     DC    F'0'                     TEMP FIELD                      00000383
         SPACE 2
         LTORG
ICRXECB  DC    A(0,0)
TECB     TECB
SUBSPL   SPL   TYPE=DEFINE,PBUF=FEEDBACK,PBUFL=88
FEEDBACK DS    0CL88
         DC    CL28' '
FEEDB028 DC    CL16' '
         DC    CL16' '
         DC    CL16' '
         DC    CL12' '
         SPACE 2
PWRBUFAD DC    F'0'
PWRBUFLN DC    F'0'
PWRBUFPT DC    F'0'    RH Test
JOBNAME  DC    CL8' '
DOSENDJ  DS    0CL88
         DC    A(0),A(0),CL80'/&&'
         SPACE 2
*****    CNSLCCB
         SPACE 2
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                   * 00000456
* --------------------------------------------------------------------* 00000457
PARMADDR DS    F                                                        00000458
* --------------------------------------------------------------------* 00000424
         LTORG                                                          00000454
* --------------------------------------------------------------------* 00000426
SPLMAP   SPL   TYPE=MAP
WORKA    DSECT
         COPY LIBPARMS                                                  00000459
         END
        BKEND
        CATALS   A.VSTAB,0.0
        BKEND   A.VSTAB
*------------------------------------------------------------------
*     NAME: VSTAB
*     TYPE: SUB-ROUTINE
* FUNCTION:
*
* INTERNAL
*
*
* EXTERNAL
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:   FOR ASSEMBLING THIS MODULE YOU MUST USE THE
*            'PUNCH CATALR VSTAB'-CARD OF THIS SOURCE MODULE,
*            BECAUSE THE MACRO-INSTRUCTION MUST BE THE 1.STMT
*            AND COMMENT ANY POSSIBLE PUNCH-CARD OF JCL-INPUT.
*
*------------------------------------------------------------------
         MACRO
         STTPRP &LUN,&TYP,&FROM,&ARC 09/01/76 PREPARE STAT TAB
.*   SEE EQUATES FOR OPERANDS IN MACRO IJBLBSTT
         LA    R7,STTLEN(R7)           NEXT ENTRY
         MVI   STTLUN+1,STT&LUN        LOGICAL UNIT
         MVI   STTTYP,STT&TYP          LIBRARY TYPE
         MVC   STTSTA+2(FOUR),&FROM    LIB START ADDR
         MVI   STTARC,STT&ARC          DEV TYPE ARCHITECTURE
         MEND
         PUNCH ' CATALR VSTAB'
VSTAB    CSECT
         USING *,R15                                                    00000017
         STM   R14,R12,12(R13)         STORE REGS IN SAVEAREA           00000018
         DROP  R15                     R15 TO BE USED BY MACROS         00000019
         USING VSTAB,R12               R12 NOW BASE REGISTER
         LR    R12,R15
         B     GOON
         DC    C'***** VSTAB ****'
GOON     DS    0H
         ST    R13,SAVEAREA+4          STORE MAIN PGM SAVEAREA          00000024
         LR    R10,R13                 SAVE R13                         00000025
         LA    R13,SAVEAREA            SAVE AREA FOR THIS TASK          00000026
         ST    R13,8(R10)              STORE EXIT SAVE ADDRESS          00000027
         ST    R1,SAVE1                                                  0000002
GETLUB   SYSIR (R1,R2,R3,R4),LUB,FG    POINT TO LUB
         LR    R5,R1                   SAVE LUB PTR FOR PRI DIR COMP
         USING LUB,R5
*****    CNSLOUT MSG='GETLUB'
*
         LA    R7,STATTAB-STTLEN       GET A(STATUS TABLE)
         USING STTDSERV,R7             STATUS TAB ADDRESSIBILITY
*
         ASYSCOM R3
         USING SYSCOM,R3               ESTABLISH R3 AS BASE
         L     R2,IJBFTTAB             GET ADDR OF FETCH TABLE
         DROP  R3                      DROP R3 AS BASE REG
* --------------------------------------------------------------------* 00000028
*        STATEMENTS FOR SET UP FIELDS NEEDED FOR LIBRM                * 00000029
* --------------------------------------------------------------------* 00000030
         L     R1,SAVE1
         L     R10,0(R1)                PARMS ADDRESS                    0000003
         USING WORKA,R10
         ST    R10,PARMADDR             SAVE                             0000003
         L     R9,PBUFADR              LOAD DATATAB ADDRESS             00000034
         ST    R9,TABRECSA             SET  TABRECS ADDRESS             00000035
         USING TABRECS,R9              ESTABLISH ADDRESSABILITY         00000036
******   CNSLOUT MSG='START VSTAB'
******   CNSLOUT MSG='ASYSCOM'
*----------
* SYSRES
*----------
******   CNSLOUT MSG='SYSRES'
         MVC   TRCYLCIL+2(TWO),FOUR(R2) INIT TR/CYL FOR SYSRES
         IC    R3,RPSFTDV(R2)          GET DEVICE TYPE
         STC   R3,RPSCLDV              SAVE FOR RPS, MAY BE 0
         STC   R3,RPSYSDV              CIL = SYSRES DEVTYPE
         STTPRP SYSR,CIL,DFTBDIR(R2),CKD    SYS CLB
         BAL   R8,READDIR              READ SYSTEM DIR RECS
         STTPRP SYSR,REL,RLAREA2,CKD        SYS RLB
         STTPRP SYSR,SSL,SLAREA2,CKD        SYS SLB
         STTPRP SYSR,PRL,PLAREA2,CKD        SYS PRL
         CLI   LUBCLB+LUBP,HEXFF       IS SYSCLB ASSIGNED
         BE    CHKRLB                  NO
         LH    R3,PIK(R4)              GET PIK
         SRL   R3,4                    PIK/16*20 TO GET
         MH    R3,HALFW20              DISP IN FETCH TABLE
         LA    R4,ZERO(R2,R3)          SET UP PTR TO ENTRY
         MVC   TRCYLCIL+2(TWO),FOUR(R4) GET TR/CYL FOR PCIL
         MVC   DISKCID(FOUR),TEN(R4)   GET PCIL START ADDRESS
         OI    SWD,PCST                SETUP PCIL ASSIGNED IND
*----------
* PRIV CLB
*----------
         STTPRP PCIL,CIL,DISKCID,CKD   START ADDR TO STAT TBL
         MVC   RPSCLDV(ONE),RPSFTDV(R4) CIL MAY NOT = SYSRES
         EJECT
CHKRLB   EQU   *
******   CNSLOUT MSG='CHKRLB'
         MVI   CCW4FLAG,SLI            BRAKE CHAIN OFF
         MVI   SECTVAL1,ZERO           ORIENT TO TRK START
         SPACE 1
         CLI   LUBRLB+LUBP,HEXFF       IS SYSRLB ASSIGNED
         BE    CHKRL                   NO
         MVC   IJSYSRL+17(3),=VL3(IJJCPDV1)  YES
         LA    R6,IJSYSRL              ADDR OF PRIV REL LIB
         SPACE 1
         OPENR (R6)                    OPEN PRIVATE RELOCATE LIB
*----------
* PRIV RLB
*----------
         STTPRP PREL,REL,IJRLL,CKD     START ADDR TO STAT TBL
         MVI   CCBSYM2,RLB             SET SYSRLB SYMBOLIC UNIT IN CCB
         MVC   SEEKCC(FIVE),IJRLL      CCHH TO SEEK BUCKET
         BAL   R8,READDIR              READ PRI RD 80 BYTES DIR REC
         MVI   CCBSYM2,RES             SET SYSRES LOGICAL UNIT
         OI    SWD,PRST                SETUP PREL ASSIGNED IND
         SR    R0,R0                   ZERO REG FOR COMPARE
         C     R0,RACTENT              ANY PRI REL ACTIVE ENTRIES
         BNE   SAVERLD                 YES
         OI    SWE,ERR6                NO, SETUP NO PRI REL ERROR IND
         B     CHKSLB                  CHECK SYSSLB IF IT WAS ASSIGNED
         SPACE 1
CHKRL    CLI   RLAREA,BLANK            IS THERE A SYSTEM REL
*****    CNSLOUT MSG='CHKRL'
         BE    NORLB                   NO
         MVC   SEEKCC(FIVE),RLAREA2    YES, CCHHR TO SEEK BUCKET
         BAL   R8,READDIR              READ SYSDIR REC OF REL
         SR    R0,R0                   ZERO REG FOR COMPARE
         C     R0,RACTENT              ANY ACTIVE ENTR IN REL
         BNE   SAVERLD                 YES
NORLB    OI    SWE,ERR3                INDICATE 'NO SYS REL '
         B     CHKSLB                  CHECK FOR SYSSLB ASSIGN
         SPACE 1
SAVERLD  MVC   DISKRLD,RLAREA2         SAVE PRIV REL DIR  DISK
         EJECT
CHKSLB   EQU   *
******   CNSLOUT MSG='CHKSLB'
         MVC   CCW4(EIGHT),CCW5        MODIFY CCW TO READ 1ST
*                                        REC OF PRIV SSL
         MVI   CCW4FLAG,SLI            BRAKE CHAIN OFF
         CLI   LUBSLB+LUBP,HEXFF       IS SYSSLB ASSIGNED
         BE    CHKSL                   NO
         MVC   IJSYSSL+17(3),=VL3(IJJCPDV1)
         LA    R6,IJSYSSL              ADDR OF PRIV SOURCE LIB
         SPACE 1
         OPENR (R6)                    OPEN PRIVATE SOURCE LIB
*----------
* PRIV SLB
*----------
         SPACE 1
         STTPRP PSSL,SSL,IJSLL,CKD     START ADDR TO STAT TBL
         MVI   CCBSYM2,SLB             SET SYSSLB SYMBOLIC UNIT IN CCB
         MVC   SEEKCC(FIVE),IJSLL      CCHH TO SEEK BUCKET
         BAL   R8,READDIR              READ PRI SOURCE 80 BYTES DIR REC
         MVI   CCBSYM2,RES             SET SYSRES LOGICAL UNIT
         OI    SWD,PSST                SETUP PRIVATE SD STATUS IND
         SR    R0,R0                   ZERO REG FOR COMPARE
         C     R0,SACTENT              ANY PRI SOURCE ACTIVE ENTRIES
         BNE   SAVESLD                 YES
         OI    SWE,ERR7                NO, SETUP NO PRI SOR ERROR IND
         B     CHKPL                   CHECK PROC LIB
         SPACE 1
CHKSL    CLI   SLAREA,BLANK            IS THERE A SYSTEM SSL
*****    CNSLOUT MSG='CHKSL'
         BE    NOSSL                   NO
         MVC   SEEKCC(FIVE),SLAREA2    CCHHR TO SEEK BUCKET
         BAL   R8,READDIR              READ 1ST REC OF SSL DIR
         SR    R0,R0                   ZERO REG FOR COMPARE
         C     R0,SACTENT              ANY SSL ACTIVE ENTRIES
         BNE   SAVESLD                 YES
NOSSL    OI    SWE,ERR4                INDICATE 'NO SSL '
*****    CNSLOUT MSG='NOSSL'
         B     CHKPL                   CHECK PROC LIB
         SPACE 1
SAVESLD  MVC   DISKSLD,SLAREA2         SAVE ADDR OF PRIV SOURCE DIR 4-0
         SPACE 1
CHKPL    EQU   *                       *
*****    CNSLOUT MSG='CHKPL'
         CLI   PLAREA,BLANK            IS THERE A PROC LIBR
         BE    NOPLB                   NO - POST ERROR ERR9
         CLI   PLAREA+SIX,HEX00        REALLY A PROCLIB
         BE    NOPLB                   NO - POST ERROR ERR9
         OI    SWE,ERR10               SET MAINT USE FLAG ON
         USE   SYSPL,S,FAIL=RETURN     ENQUEUE THE PROC LIB
         LTR   R0,R0                   SUCESSFULL
         BNZ   CHKSDL                  NO-
         NI    SWE,HEXFF-ERR10         RESET MAINT USE FLAG
         MVC   CCW4(EIGHT),CCW6        MODIFY CCW TO READ FIRST
*                                      80 BYTES OF PROC. DIR.
*----------
* SYS PLB
*----------
         MVC   SEEKCC(FIVE),PLAREA2    CCHHR TO SEEK BUCKET
         BAL   R8,READDIR              READ PROC. DIR. HEADER
         MVC   DISKPLD,PLAREA2         SAVE PD ADDR FOR LATER
         SR    R0,R0
         C     R0,PACTENT              ANY ENTRY OF ACTIVE
         BNE   CHKSDL                  YES -
NOPLB    OI    SWE,ERR9                POST NO PROC LIBR IND
         SPACE 2
**************************************************************
*
*    TEST IF SVA RESP. SDL IS AVAILABLE
*
**************************************************************
         SPACE 2
CHKSDL   ASYSCOM R3
******   CNSLOUT MSG='CHKSDL'
         USING SYSCOM,R3               ESTABLISH  BASE
         TM    IJBSVA,SVASW            IS THERE AN SDL
         BNO   NOSDL                   NO
         DROP  R3                      RELEASE
         OI    SWD,SVADIR              INDICATE IN SWD
         B     LOADSTAT                GO ON
         SPACE 1
NOSDL    OI    SWE1,ERR11              POST NO SDL IND
         EJECT
*************************************************************
* MOVE THE VALID ENTRIES IN THE STATUS TABLE TOGETHER AND
* LOAD $LIBSTAT TO DISPLAY THE STATUS REPORTS OF SYSRES AND
* THE ASSIGNED PRIVATE LIBRARIES.
*************************************************************
         SPACE 1
LOADSTAT LA    R7,STATTAB              GET ADDR OF 1ST ENTRY
******   CNSLOUT MSG='LOADSTAT'
         LA    R6,STATEND-L'STTNOE     END ADDR OF STAT TBL
         SR    R5,R5                   CLEAR REGISTER
COMPARE  CLC   STTCCHH,ZEROS           ENTRY IN STAT TBL VALID
         BNE   UPDENT                  YES, UPDATE ENTRY PTR
         LR    R11,R6                  GET END ADDR
         SR    R11,R7                  CALC LENGTH FOR MOVE
         BCTR  R11,0                   LENGTH CODE FOR MVC
         EX    R11,MOVE                MOVE ENTRIES TOGETHER
         SH    R6,=AL2(STTLEN)         UPDATE END PTR
         B     READYTST                CHECK IF TBL IS READY
         SPACE 1
MOVE     MVC   STTTAB,STTTAB+STTLEN    MOVE ENTRIES
         SPACE 1
UPDENT   LA    R5,ONE(R5)              UPDATE NO OF ENTRIES
         LA    R7,STTLEN(R7)           UPDATE TO NEXT ENTRY
READYTST CR    R7,R6                   ALL ENTRIES PROCESSED
         BL    COMPARE                 NO, PRECESS NEXT ENTRY
         STH   R5,STATTAB              STORE NUMBER OF ENTR
         LA    R7,STATTAB              ADDR OF 1.ENTRY STATUS TAB
         PDUMP STATTAB,STATTAB+100
STATLOOP DS    0H
         CLC   STTLUN,=XL2'0006'       SYSRES-DIRECTORIES ?
         BNE   STAT0010                NO
*
         CLI   STTTYP,C'C'             SYSTEM CLB ?
         BNE   STATSR00
         MVC   PSCLB(4),STTSTA+2       START-ADDR BBCCHH
         MVC   PSCLBA,STTARC           CKD OR FBA
         B     STAT0040
STATSR00 DS    0H
         CLI   STTTYP,C'R'             SYSTEM RLB ?
         BNE   STATSR10
         MVC   PSRLB(4),STTSTA+2       START-ADDR BBCCHH
         MVC   PSRLBA,STTARC           CKD OR FBA
         B     STAT0040
STATSR10 DS    0H
         CLI   STTTYP,C'S'             SYSTEM SLB ?
         BNE   STATSR20
         MVC   PSSLB(4),STTSTA+2       START-ADDR BBCCHH
         MVC   PSSLBA,STTARC           CKD OR FBA
         B     STAT0040
STATSR20 DS    0H
         CLI   STTTYP,C'P'             SYSTEM PLB ?
         BNE   STATSR30
         MVC   PSPLB(4),STTSTA+2       START-ADDR BBCCHH
         MVC   PSPLBA,STTARC           CKD OR FBA
STATSR30 DS    0H
         B     STAT0040
*
STAT0010 DS    0H
         CLC   STTLUN,=XL2'0007'       PRIVATE SYSSLB ?
         BNE   STAT0020
         MVC   PPSLB(4),STTSTA+2       START-ADDR BBCCHH
         MVC   PPSLBA,STTARC           CKD / FBA
         B     STAT0040
STAT0020 DS    0H
         CLC   STTLUN,=XL2'0008'       PRIVATE SYSRLB ?
         BNE   STAT0030
         MVC   PPRLB(4),STTSTA+2       START-ADDR BBCCHH
         MVC   PPRLBA,STTARC           CKD / FBA
         B     STAT0040
STAT0030 DS    0H
         CLC   STTLUN,=XL2'000B'       PRIVATE SYSCLB ?
         BNE   STAT0040
         MVC   PPCLB(4),STTSTA+2       START-ADDR BBCCHH
         MVC   PPCLBA,STTARC           CKD / FBA
         B     STAT0040
STAT0040 DS    0H
         LA    R7,STTLEN(R7)           INCREASE INPUT-ADDR
         BCT   R5,STATLOOP             COUNT -1
******   CNSLOUT ADR=PSTATAB,LEN=28
******   CNSLOUT ADR=PPCLB,LEN=21
         B     PGMEND   RH             FINISHED
         EJECT
*********************************************************************** 41280025
*****    DIRECTORY READ ROUTINE
*********************************************************************** 41340025
         SPACE 1
READDIR  NOP   RDSYSTST                FIRST TIME SW
******   CNSLOUT MSG='READDIR'
*                            FURTHER   BRANCH IF SV SUPPRT RPS
         OI    READDIR+1,BRANCH        SET SW TO BRANCH
         COMRG ,
         USING COMREG,R1
         TM    RMSROPEN,RPSUPER        DOES SV SUPPORT RPS
         DROP  R1
         BNZ   RDSYSTST                 YES
         MVC   READDIR+2(2),SRDEXCP     NO
*                  FURTHER BR OF FIRST TIME SW IF SV DOESN'T
*                                      SUPPORT RPS
         SPACE 1
RDNORPS  EQU   *
******   CNSLOUT MSG='RDNORPS'
         MVC   CCWRPS1,CCWTIC          REPLACE SETSECT BY TIC
         B     RDEXCP
         SPACE 1
RDSYSTST EQU   *
         CLI   CCBSYM2,RES             IS THIS A SYSTEM LIB
         BNE   RDTSTPRV                NO, MUST BE ANY PRV
         TM    RPSYSDV,RPSDEV          ANY KIND OF RPS DEVICE
         BNZ   RDRPS                   YES
         B     RDNORPS                 NO
         SPACE 1
RDTSTPRV EQU   *
         CLI   CCBSYM2,RLB             IS THIS A PRV RLB
         BNE   RDPRVSLB                NO, MUST BE A PRV SLB
         TM    IJRRPS,RPSDTF           IS PRV RLB ON RPS DEV
         BZ    RDNORPS                 NO
         B     RDRPS                   YES
         SPACE 1
RDPRVSLB EQU   *
         TM    IJSRPS,RPSDTF           IS PRV SLB ON RPS DEV
         BZ    RDNORPS                 NO
         SPACE 1
RDRPS    EQU   *
         MVC   CCWRPS1,CCWRPS11        MVC
         SPACE 1
RDEXCP   EQU   *
******   CNSLOUT MSG='RDEXCP'
         LA    R1,RDCCB                ADDR OF CCB FOR EXCP
         EXCP  (1)                     READ SYSTEM DIRECTORY RECORD
         WAIT  (1)                     WAIT FOR COMPLETION OF READ
         BR    R8                      RETURN TO CALLER
         EJECT
SAVE1    DS    F
*****    CNSLCCB
*******************************************************************
*****    INSTRUCTIONS TO BE EXECUTED VIA THE EX INSTRUCTION
*******************************************************************
         SPACE 2
SETIND   OI    SWA,ZERO                SET DISPLAY INDICATOR INSTR.
         SPACE 1
SETIND1  OI    SWA1,ZERO               SET DISPLAY INDICATOR
         SPACE 1
MOVENAME MVC   PNBUCKET(ZERO),ZERO(R2) MOVE SPECIFIED PHASENAME
         SPACE 1
PACK1    PACK  DBLWORD(EIGHT),ZERO(ZERO,R2) CONVERT TO PACKED DECIMA
         SPACE 3
********************************************************************
*****    END OF DSERV
********************************************************************
         SPACE
CLEANUP  DS    0H
         TM    SWE,ERR10               IS ERROR 'PROC. LIB. IN USE'
*                                      POSTED
         BO    CLNUP02                 YES -
*
         RELEASE SYSPL
*
CLNUP02  EQU   *
*---------------------------------------------------------------------* 00000065
*     PGMEND Program-ENd                                              * 00000066
*---------------------------------------------------------------------* 00000067
PGMEND   DS    0H
*****    CNSLOUT MSG='PGM-END'
         L     R9,PARMADDR         LOAD PARAMETER-ADDR
         L     R13,SAVEAREA+4      STORE MAIN PGM SAVEAREA
******   L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
         EJECT
*---------------------------------------------------------------------* 00000067
*****    CONSTANTS - COMMON TO ALL PHASES, LOCATED IN THE ROOT PHASE    06000025
*****    SAVE AREAS, BUCKETS, FLAGS, ETC FOR FOLLOWING PHASES           06030025
*---------------------------------------------------------------------* 00000067
         SPACE 1                                                        06060025
SAVEREGS DC    2F'0'                   REGISTER SAVE AREA               06090025
SORTSTRT DC    F'0'                    START OF SORT AREA ADDR          06120025
VMDISP   DC    F'12'                   INIT STANDARD VM LOCATION        06150025
VMDISP1  EQU   VMDISP+3                VERSION BYTE                     06200034
PNBUCKET DC    CL8' '                  PHASE NAME BUCKET                06330025
PHASNAME DC    C'DSERV1  '             PHASE NAME TO BE FETCHED         06360025
PHASENO  EQU   PHASNAME+5              PHASE CHARACTER NAME             06390025
ENTRIES  DC    F'0'                    NO. OF ENTRIES ON FULL PAGES     06420025
SORTEND  DC    F'0'                    END OF SORT AREA ADDR        5-0 06430028
TRCYLCIL DC    F'0'                    TRACKS/CYL IN (P)CIL    @DL29ZCL 06450029
RESBLOCK DC    H'1024'                 CIL BLOCK SIZE          @DL29ZCL 06470029
REMAINS  DC    H'0'                    NO. OF ENTRIES ON LAST PAGE      06510025
RECORDS  DC    H'0'                    NUMBER OF RECORDS SORTED         06540025
HALFW9   DC    H'9'                    CONSTANT OF 9           @DL29ZCL 06541029
DISKCID  DC    X'0000000201'           CORE IMAGE DIR DISK ADDR@DL29ZCL 06545029
DISKRLD  DC    X'0000000000'           RELOC DIR DISK ADDR          4-0 06550027
DISKSLD  DC    X'0000000000'           SOURC STMT DIR DISK ADDR     4-0 06555027
DISKPLD  DC    XL5'0'                  PROCEDURE DIR. ADDRESS        VB 06555528
DISKSAVE DC    X'000000000000'         DISK ADDR SAVE AREA     @DL30SCM 06560030
LINECTR  DC    X'00'                   SYSLST LINE COUNTER     @DA01039 06570029
LINES    DC    X'00'                   LINES REMAINING ON PAGE @DA01039 06600029
NAMELNG  DC    X'0'                    PHASE NAME LENGTH                06630025
PASSCTR  DC    C'1'                    SORT PASS COUNTER                06660025
PAGECTR  DC    X'01'                   PAGE COUNTER                     06690025
RPSCLDV  DC    X'00'                   CIL DTF DEVICE TYPE     @DL30SCM 06700030
RPSYSDV  DC    X'00'                   SYSRES DTF DEV TYPE     @DL30SCM 06710030
         SPACE 2                                                        06720025
         DC    C' '                    ***  WORK AREA FOR SECOND  ***   06840025
*                                      ***  COLUMN DIR. THIS BYTE ***   06870025
*                                      ***   MUST NOT BE REMOVED  ***   06900025
         EJECT                                                          06930025
SWA      DC    X'0'                    DISPLAY SWITCH A                 06960025
         SPACE 1                                                        06990025
VMIND    EQU   X'80'                   VER AND MOD LEVEL IND         VB 06990528
HEADIND  EQU   X'40'                   HEADER NEEDED IND                07050025
NONAME   EQU   X'20'                   PHASE NAME NOT FOUND IND.        07080025
PDIND    EQU   X'10'                   DISPLAY PROCEDURE DIRECTORY   VB 07140528
SDIND    EQU   X'08'                   DISPLAY SOURCE STMNT DIRECTORY   07170025
RDIND    EQU   X'04'                   DISPLAY RELOCATABLE DIRECTORY    07200025
CDIND    EQU   X'02'                   DISPLAY CORE IMAGE DIRECTORY     07230025
TDIND    EQU   X'01'                   DISPLAY TRANSIENT DIRECTORY      07260025
         SPACE 2                                               @D33GDVS 07266033
SWA1     DC    X'0'                    DISPLAY SWITCH A1       @D33GDVS 07272033
         SPACE 1                                               @D33GDVS 07278033
SDLIND   EQU   X'80'                   DISPLAY SYS. DIR. LIST  @D33GDVS 07284033
         SPACE 2                                               @DL29ZCL 07290029
SWB      DC    X'0'                    SWITCH BYTE B                    07320025
         SPACE 1                                                        07350025
SYSTD    EQU   X'80'                   DISPLAY TRANSIENT DIRECTORY      07380025
SYSCL    EQU   X'40'                   DISPLAY SYSTEM CORE IMAGE DIR.   07410025
SYSRL    EQU   X'20'                   DISPLAY SYSTEM REL DIR           07440025
SYSSL    EQU   X'10'                   DISPLAY SYSTEM SOURCE DIR        07470025
SYSPL    EQU   X'08'                   DISPLAY SYSTEM PROC. DIR.     VB 07470528
SYSSDL   EQU   X'04'                   DISPLAY SYS. DIR. LIST  @D33GDVS 07520533
         SPACE 1                                                        07650025
SWB1     DC    X'0'                    DISPLAY SWITCH B1             VB 07650528
         SPACE 1                                                     VB 07650628
PCLB     EQU   X'40'                   DISPLAY PRIVATE CORE IMAGE D. VB 07651028
PRLB     EQU   X'20'                   DISPLAY PRIVATE REL DIR       VB 07651528
PSLB     EQU   X'10'                   DISPLAY PRIVATE SOURCE DIR.   VB 07652028
PTD      EQU   X'80'                   PRIVATE TRANSIENT DIR IND     VB 07652528
RESERVE  EQU   X'08'                   RESERVED                      VB 07653028
FIRST    EQU   X'01'                   FIRST TIME IND                VB 07653528
SVASW    EQU   X'40'                   IND SVA HAS BEEN BUILD  @DL29ZCL 62939829
         SPACE 1                                                     VB 07654028
SWC      DC    X'0'                    SWITCH BYTE C                    07680025
         SPACE                                                          07710025
FULLTBL  EQU   X'80'                   FULL TABLE IND                   07740025
RELOOP   EQU   X'40'                   GO THROUGH SORT LOOP AGAIN       07770025
ONEIND   EQU   X'10'                   DISPLAY SINGLE PHASE             07830025
LEVELNO  EQU   X'08'                   NEED LEVEL NO. FROM NEXT RECORD  07860025
SKIPNAME EQU   X'04'                   DO NOT SCAN PHASE NAME IND.      07890025
DUMYCNT  EQU   X'02'                   DUMY LOOP-COUNT RECDS LEFT IND.  07920025
DISPLACE EQU   X'01'                   DISPLACEMENT SEPECIFIED IND.     07950025
         SPACE 2                                               @D33GDVS 07956033
SWE1     DC    X'0'                    SWITCH BYTE E1          @D33GDVS 07962033
         SPACE 1                                               @D33GDVS 07968033
ERR11    EQU   X'80'                   NO SDL PRESENT          @D33GDVS 07974033
         SPACE 2                                               @DL29ZCL 07980029
SWD      DC    X'0'                    SWITCH BYTE D                    08010025
        SPACE                                                           08040025
SORT     EQU   X'80'                   ALPHANUMERICALLY DISPLAY         08070025
SVADIR   EQU   X'40'                   SVA PRESENT INDICATOR   @DL29ZCL 08075029
PCST     EQU   X'20'                   PRIVATE CORE IMAGE STATUS IND    08130025
PRST     EQU   X'10'                   PRIVATE REL STATUS IND           08160025
PSST     EQU   X'08'                   PRIVATE SOURCE STATUS IND        08190025
SECOND   EQU   X'04'                   SECOND TIME INDICATOR            08220025
DIREND   EQU   X'02'                   IND END OF DIR REACHED  @DL29ZCL 08225029
         SPACE 2                                               @DL29ZCL 08230029
SWE      DC    X'00'                   SWITCH BYTE E                    08340025
        SPACE                                                           08370025
ERR3     EQU   X'80'                   NO SYSTEM REL ACTIVE ENTRIES 4-0 08400027
ERR4     EQU   X'40'                   NO SYSTEM SOR ACTIVE ENTRIES IND 08430025
ERR5     EQU   X'20'                   NO PRI CI ACTIVE ENTRIES IND     08460025
ERR6     EQU   X'10'                   NO PRI REL ACTIVE ENTRIES IND    08490025
ERR7     EQU   X'08'                   NO PRI SOR ACTIVE ENTRIES IND    08520025
ERR8     EQU   X'04'                   NO PRIV TD ACTIVE ENTRIES IND4-0 08550027
ERR9     EQU   X'02'                   NO SYSTEM PROC LIBRARY        VB 08580028
ERR10    EQU   X'01'                   SYTEM PROC LIB CANNOT BE USED VB 08610028
         SPACE 2                                                        08617033
IPTSW    DC    X'00'                   INPUT  SWITCH BYTE      @DM15531 08624033
IPT81BYT EQU   X'80'                   81 BYTE SYSIPT INDICAT. @DM15351 08631033
         SPACE 2                                                        08640025
*********************************************************************** 08670025
*****    EQUATES -- COMMON TO ALL PHASES, LOCATED IN ROOT PHASE       * 08700025
*********************************************************************** 08730025
         SPACE 2                                                        08760025
*****    REGISTER EQUATES                                               08790025
         SPACE 1                                                        08820025
R0       EQU   0                       WORK REG                         08850025
R1       EQU   1                       I/O AND WORK REG                 08880025
R2       EQU   2                       WORK REG                         08910025
R3       EQU   3                       WORK REG                         08940025
R4       EQU   4                       WORK REG                         08970025
R5       EQU   5                       WORK REG                         09000025
R6       EQU   6                       WORK REG                         09030025
R7       EQU   7                       WORK REG                         09060025
R8       EQU   8                       WORK AND ERROR MESSG POINTER     09090025
R9       EQU   9                       LINK REG TO SUBROUTINES          09120025
R10      EQU   10                      WORK REG                         09150025
R11      EQU   11                      WORK REG                         09180025
R12      EQU   12                      BASE REG FOR ROOT PHASE @D347DPR 09210034
R13      EQU   13                      BASE REG FOR OVERLAY PHASES      09230034
R14      EQU   14                      GET/PUT REG FOR LIOCS            09270025
R15      EQU   15                      LIOCS BASE REG                   09300025
CRREG    EQU   1                       COMMUNIC. REGION ADDR.  @DM15351 09310033
*---------------------------------------------------------------------  09330025
*        CCW OP CODE AND FLAG EQUATES                                   09360025
*---------------------------------------------------------------------  09330025
SEEK     EQU   X'07'                   SEEK BBCCHH COMMAND              09420025
SIDE     EQU   X'31'                   SEARCH ID EQUAL COMMAND          09450025
TIC      EQU   X'08'                   TRANSFER IN CHANNEL COMMAND      09480025
READ     EQU   X'06'                   READ DATA COMMAND                09510025
RDCNT    EQU   X'92'                   READ COUNT/MULTI TRACK COMMAND   09540025
         SPACE 2                                                        09570025
SLI      EQU   X'20'                   SUPPRESS WRONG LNG CCW IND       09600025
CCSLI    EQU   X'60'                   CHAIN AND WRONG LNG CCW IND      09630025
WRNGLN   EQU   X'40'                   WRONG LENGTH BIT IN CCB          09640026
         SPACE 3                                                        09660025
RES      EQU   X'06'                   SYSRES LOGICAL UNIT     @DL29ZCL 09670029
SLB      EQU   X'07'                   SYSSLB LOGICAL UNIT NUMBER       09690025
RLB      EQU   X'08'                   SYSRLB LOGICAL UNIT NUMBER       09720025
         SPACE 3                                               @D33GDVS 09810033
SVAPRES  EQU   X'10'                   PHASE PRES IN SVA       @D33GDVS 09840033
         EJECT                                                          09870025
*****    CONSTANT EQUATES                                               09900025
         SPACE 2                                                        09930025
ZERO     EQU   0                                                        09960025
ONE      EQU   1                                                        09990025
TWO      EQU   2                                                        10020025
THREE    EQU   3                                                        10050025
FOUR     EQU   4                                                        10080025
FIVE     EQU   5                                                        10110025
SIX      EQU   6                                                        10140025
SEVEN    EQU   7                                                        10170025
EIGHT    EQU   8                                                        10200025
NINE     EQU   9                                                        10230025
TEN      EQU   10                                                       10260025
ELEVEN   EQU   11                                                       10290025
TWELVE   EQU   12                                                       10320025
FOURTEEN EQU   14                                                       10350025
SIXTEEN  EQU   16                                                       10410025
SEVETEEN EQU   17                                                       10440025
EIGHTEEN EQU   18                                                       10470025
TWENTY   EQU   20                                                       10500025
TWENTY1  EQU   21                                                       10530025
TWENTY2  EQU   22                                                       10560025
TWENTY3  EQU   23                                                       10590025
TWENTY4  EQU   24                                                   5-0 10610028
TWENTY5  EQU   25                                                       10620025
TWENTY6  EQU   26                                                       10650025
TWENTY7  EQU   27                      DISP OF 27              @DL29ZCL 10660029
TWENTY8  EQU   28                                              @D33GDVS 10670033
TWENTY9  EQU   29                                                       10680025
THIRTY2  EQU   32                                                       10740025
THIRTY3  EQU   33                                                       10770025
THIRTY4  EQU   34                                                       10800025
THIRTY5  EQU   35                                                       10830025
THIRTY6  EQU   36                      DISP OF 36              @DL29ZCL 10835029
THIRTY9  EQU   39                                                       10890025
FORTY    EQU   40                                                       10920025
FORTY1   EQU   41                                              @D33GDVS 10930033
FORTY2   EQU   42                                                       10950025
FORTY3   EQU   43                                                       10980025
FORTY5   EQU   45                                                       11040025
FORTY9   EQU   49                                              @D33GDVS 11050033
FIFTY    EQU   50                                                       11070025
FIFTY2   EQU   52                                                       11100025
FIFTY4   EQU   54                                              @D33GDVS 11105033
FIFTY5   EQU   55                                                   5-0 11110028
FIFTY7   EQU   57                                                       11160025
SIXTY1   EQU   61                                              @D33GDVS 11190033
SIXTY6   EQU   66                                                       11220025
SIXTY9   EQU   69                                              @D33GDVS 11222033
SEVENTY  EQU   70                                              @D33GDVS 11224033
SEVENTY3 EQU   73                                              @D33GDVS 11226033
SEVENTY4 EQU   74                                              @D33GDVS 11228033
SEVENTY7 EQU   77                                              @D33GDVS 11230033
SEVENTY8 EQU   78                      DISP OF 78              @DL29ZCL 11235029
SEVENTY9 EQU   79                                              @D33GDVS 11255033
EIGHTY   EQU   80                                                       11280025
EIGHTY1  EQU   81                                                       11310025
EIGHTY2  EQU   82                                              @D33GDVS 11317033
EIGHTY4  EQU   84                                              @D33GDVS 11324033
EIGHTY8  EQU   88                                              @D33GDVS 11331033
EIGHTY9  EQU   89                                                       11340025
NINETY1  EQU   91                      DISP OF 91              @DL29ZCL 11345029
NINETY2  EQU   92                      DISP OF 92              @DL29ZCL 11350029
NINETY6  EQU   96                                                       11370025
LCILH    EQU   101                     LENGTH CIL HEADER       @DL29ZCL 11400129
LDASH    EQU   67                      NO. OF DASHES IN HEADER @DL29ZCL 11400229
FORTY6   EQU   46                                                   5-0 11400328
SIXTY3   EQU   63                                                   5-0 11400728
SIXTY5   EQU   65                                                   5-0 11400928
SEVENTY1 EQU   71                                                   5-0 11401228
ONE02    EQU   102                                             @D33GDVS 11411233
ONE03    EQU   103                                                      11430025
ONE11    EQU   111                                             @D33GDVS 11440033
ONE18    EQU   118                                                      11460025
ONE19    EQU   119                                                      11490025
         SPACE 1                                                    5-0 11550028
DASH     EQU   C'-'                                                     11580025
BLANK    EQU   C' '                                                     11610025
SKIP1    EQU   C'1'                    SKIP TO 1 ASA CHAR               11640025
F1       EQU   C'1'                    SCAN CARD ROUTINE PHASE NO.      11700025
F2       EQU   C'2'                    TD/CD PRINT SGL COL     @DL29ZCL 11730029
F3       EQU   C'3'                    RD/SD PHASE NO.                  11760025
F4       EQU   C'4'                    RD AND SD PRINT PHASE   @DL29ZCL 11820029
F5       EQU   C'5'                    PD PRINT PHASE          @DL29ZCL 11830529
F6       EQU   C'6'                    SDL PRINT PHASE         @D33GDVS 11839533
         SPACE 1                                                        11850025
CLB      EQU   X'0B'                   SYSCLB SYMBOLIC UNIT             11880025
EOC      EQU   X'20'                   END OF CYLINDER INDICATOR        11910025
RELPHASE EQU   X'40'                   REL PHASE INDICATOR     @DL29ZCL 11912029
HEX00    EQU   X'00'                                                4-0 11924027
HEX3F    EQU   X'3F'                                                4-0 11931027
HEXF0    EQU   X'F0'                                                    11940025
HEXFF    EQU   X'FF'                                                    11970025
BRANCH   EQU   X'F0'                                           @DM15919 11975034
IPT81T   EQU   X'10'                   81 BYTE SYIPT SWITCH    @DM15351 11980033
         SPACE 1                                                    5-0 12000028
*******   VARIOUS EQUATES FOR ROTATIONAL POSITION SENSING      @DL30SCM 12001030
SETSECT  EQU   X'23'                   SET SECTOR CCW OP CODE  @DL30SCM 12002030
RDSECT   EQU   X'22'                   RD SECTOR CCW OP CODE   @DL30SCM 12003030
RPSUPER  EQU   X'01'                   RPS SUPPORT IN SUPERVIS @DL30SCM 12004030
RPSPUBDV EQU   X'04'                   RPS DEVICE IN PUB       @DL30SCM 12005030
RPSDTF   EQU   X'01'                   RPS DEVICE IN DTF       @DL30SCM 12006030
RPSDEV   EQU   X'FF'                   MASK TO CHECK FOR RPS   @DL30SCM 12007030
M8       EQU   X'08'                   ICM MASK                @DL30SCM 12008030
M12      EQU   X'0C'                   ICM MASK                @DL30SCM 12009030
JCTLBYT  EQU   135                     FIELD IN COMREG - RPS   @DL30SCM 12009430
RPSFTDV  EQU   6                       FIELD IN FETCH TAB -RPS @DL30SCM 12009830
DFTBDIR  EQU   10                      =                  -DIR @DM15919 12013834
SCVRT    EQU   75                      SVC 75 SECTOR CONVERT   @DL30SCM 12018034
         SPACE 1                                               @DL30SCM 12022034
LASTREC  DS    2D               HOLD LAST RECORD FOR NEXT PASS @DA01108 12026034
ENDPH0   DS    0D                                                       12030025
SAVEAREA DS    18F                     START OF SAVEAREA
PARMADDR DS    F                       ADDR OF PARAMETERS
TABRECSA DS    F
         SPACE 2                                                    5-0 12090028
*********************************************************************** 42120025
*                      CCB'S, CCW'S, DC'S AND DS'S                    * 42150034
*********************************************************************** 42180025
         SPACE 3                                                        42630025
         LTORG                                                          42660025
         DS    0F                                                   4-0 42670027
         EJECT                                                          42690025
*****    CCB FOR READING SYSTEM DIRECTORIES                             42720025
         SPACE 1                                                        42750025
RDCCB    CCB   SYSRES,CCW1             CCB FOR READING SYSTEM DIRECTORY 42780025
         SPACE 3                                                        42810025
*****    CCW CHAIN TO READ 4 - 80 BYTE SYSTEM RECORDS                VB 42840028
         SPACE 1                                                        42870025
CCW1     CCW   SEEK,SEEKBB,CCSLI,SIX   SEEK BBCCHH                      42900025
CCWRPS1  CCW   SETSECT,SECTVAL1,CCSLI,ONE SET SECTOR TO R0     @DL30SCM 42910030
CCW2     CCW   SIDE,SEEKCC,CCSLI,FIVE  SEARCH ID EQUAL                  42930025
CCWTIC   CCW   TIC,CCW2,CCSLI,ONE      TRANSFER IN CHANNEL              42960034
CCW4     CCW   READ,RLAREA,CCSLI,EIGHTY  READ DATA             @DL29ZCL 43020029
CCW5     CCW   READ,SLAREA,CCSLI,EIGHTY  READ DATA             @DL29ZCL 43050029
CCW6     CCW   READ,PLAREA,SLI,EIGHTY    READ DATA             @DL29ZCL 43051029
*----------------- DO NOT REARRANGE THE FOLLOWING CCW -------- @DM15919 43060034
CCWRPS11 CCW   SETSECT,SECTVAL1,CCSLI,ONE MODEL CCW TO BE MOV  @DL30SCM 43069034
         SPACE 3                                                        43080025
*****    DISK ADDRESS SEEK BUCKET TO READ SYSTEM RECORDS                43110025
         SPACE 1                                                        43140025
SEEKBB   DC    H'0'                    BB PORTION OF SEEK ADDR          43170025
SEEKCC   DC    X'0000'                 CC PORTION OF SEEK ADDR          43200025
         DC    X'0001'                 HH PORTION OF SEEK ADDR @DL29ZCL 43230029
         DC    X'02'                   R - RECORD NO.          @DL29ZCL 43260029
SECTVAL1 DC    X'FF'                   ORIENT TO TRACK BEGIN   @DL30SCM 43270030
*                                      BUT CAUSE NOP 1ST TIME  @DL30SCM 43280030
         EJECT                                                          43290025
*****    CONSTANTS - COMMON TO THIS PHASE ONLY                          43320025
         SPACE 1                                                        43350025
DBLWORD  DC    D'0'                    DOUBLE WORD WORK AREA            43380025
MAXADDR  DC    F'16777214'             MAX. VM LOCATION IN A PHASE      43410025
R1SAVE   DC    F'0'                    SAVE AREA REGISTER 1    @DL29ZCL 43415029
R24SAVE  DC    F'0'                    SAVE AREA FOR                    43440025
R34SAVE  DC    F'0'                      REGISTERS                      43470025
         DC    F'0'                        R2, R3 AND R4                43500025
SAVEIN   DC    F'0'                    READIN SAVE AREA                 43530025
PH01     DC    A(ENDPH0)               END ADDRESS OF PHASE 0           43590025
SRDEXCP  DC    S(RDEXCP)               BRANCH SWITCH           @DM15919 43630034
CON8     DC    H'8'                    CONSTANT OF 8                    43680025
FIVE10   DC    H'15'                   CONSTANT OF 15                   43710025
HALFW20  DC    H'20'                   CONSTANT OF 20          @DL29ZCL 43720029
KDSPLY   DC    C'DSPLY '               DISPLAY CARD OPERATION           43740025
KDSPLYS  DC    C'DSPLYS '              SORTED DISPLAY CARD OPERATION    43770025
KALL     DC    C'ALL'                  DISPLAY CARD OPERAND             43800025
KTD      DC    C'TD'                   DISPLAY CARD OPERAND             43830025
KCD      DC    C'CD'                   DISPLAY CARD OPERAND             43860025
KRD      DC    C'RD'                   DISPLAY CARD OPERAND             43890025
KSD      DC    C'SD'                   DISPLAY CARD OPERAND             43920025
KPD      DC    C'PD'                   DISPLAY CARD OPERAND          VB 43920528
KSDL     DC    C'SDL'                  DISPLAY CARD OPERAND    @D33GDVS 43950533
SLASHAST DC    C'/*'                   EOF ON 81 BYTE INPUT CTRL CD 4-0 43990027
$LIBSTAT DC    C'$LIBSTAT'             THIS PHASE DISPLAYS THE          44000029
*                                        STATUS REPORTS        @DL29ZCL 44002029
ZEROS    DC    XL4'0'                  ZEROS                   @DL29ZCL 44007029
         SPACE 1                                                        44010025
ST1      DC    C'SYSTEM '                                      @DL29ZCL 44350129
         SPACE 2                                                        45060025
         DS    0F                      ALIGN AREAS ON FULL WORD     4-0 45090027
RLAREA   DC    XL80'0'                 RELOCATABLE DIR READ IN      3-9 45160026
SLAREA   DC    XL80'0'                 SOURCE DIRECTORY READ IN     3-9 45170026
PLAREA   DC    XL80'0'                 PROCEDURE DIRECTORY READ IN   VB 45170528
         EJECT
*********************************************************************** 45270025
*****    EQUATES -- COMMON TO THIS PHASE ONLY                         * 45300025
*********************************************************************** 45330025
         SPACE 1                                                        45360025
CCBSYM2  EQU   RDCCB+7                 2ND BYTE OF SYMBOLIC UNIT        45390025
CCW4FLAG EQU   CCW4+4                  CCW FLAG BYTE                    45420025
RLAREA2  EQU   RLAREA+2                STARTING ADDR OF RD DIR          45750025
SLAREA2  EQU   SLAREA+2                STARTING ADDR OF SD DIR          45780025
PLAREA2  EQU   PLAREA+2                STARTING ADDRESS OF PD DIR    VB 45780528
RACTENT  EQU   RLAREA+44               ACTIVE ENTRY BYTE OF RD REC      45840025
SACTENT  EQU   SLAREA+44               ACTIVE ENTRY BYTE OF SD REC      45870025
PACTENT  EQU   PLAREA+44               ACTIVE ENTRY BYTE OF PD REC   VB 45870528
PIK      EQU   46                                                       46290025
SEVEN1   EQU   71                                                       46440025
         SPACE 1                                                        46590025
COMMA    EQU   C','                                                     46800025
LPAREN   EQU   C'('                    FOR PARAENTHESIS CHECK  @DL29ZCL 46850029
RPAREN   EQU   C')'                    FOR PARENTHESIS CHECK   @DL29ZCL 46852029
         SPACE                                                          46860025
THIRTY1  EQU   31                                                       46890025
         SPACE 4                                               @DM15919 46940034
         DS    0D                                              @DL29ZCL 46990034
PHASAREA EQU   *                       $LIBSTAT LOAD ADDR      @DL29ZCL 47040034
         SPACE 6                                                        47160034
*********************************************************************** 05310025
*                         DTF'S AND DC'S                              * 05340025
IJSYSRL  DTFCP TYPEFLE=INPUT,                                          X05884034
               DISK=YES,                                               X05885034
               DEVADDR=SYSRLB,                                         X05886034
               EOFADDR=PGMEND,                                         X05887034
               IOAREA1=*                                            5-0 05888034
         SPACE 2                                                    5-0 05889034
* THE LABEL 'IJJCPD3' IS ONLY USED TO PROVIDE THE PROGRAM           5-0 05890034
* WITH A DUMMY ENTRY, SO THAT THERE WILL BE NOT                     5-0 05891034
* UNRESOLVED ADDRESS CONSTANTS DURING LINK EDITING.                 5-0 05892034
         SPACE 1                                                    5-0 05893034
IJJCPD3  EQU   *                                                    5-0 05894034
         ENTRY IJJCPD3                 DUMMY ENTRY             @DM11927 05895034
IJRLL    EQU   IJSYSRL+60              LOC OF LOWER LIMIT IN DTF        05896034
IJRRPS   EQU   IJSYSRL+42              RPS DEV TYPE BYTE   DTF @DM15919 05897034
         SPACE 3                                                    5-0 05898034
*********************************************************************** 05370025
*****    DTF FOR OPENING PRIVATE SOURCE STATEMENT LIBRARY               05899034
         SPACE 2              RH PGMEND=*                               05900034
IJSYSSL  DTFCP TYPEFLE=INPUT,                                          X05901034
               DISK=YES,                                               X05902034
               DEVADDR=SYSSLB,                                         X05903034
               EOFADDR=PGMEND,                                         X05904034
               IOAREA1=*                                            5-0 05905034
         SPACE 2                                               @DM15919 05906034
IJSLL    EQU   IJSYSSL+60              LOC OF LOWER LIMIT IN DTF        05907034
IJSRPS   EQU   IJSYSSL+42              RPS DEV TYPE BYTEDTF    @DM15919 05908034
         SPACE 3                                                        05910025
         LTORG                                                          05940025
         DS    0H
         EJECT
STTDSERV DSECT                                                 @DM15919 45230234
         IJBLBSTT                                              @DM15919 45230434
         EJECT                                                 @DM15919 45230634
*IJBDS141 CSECT                                                @DM15919 45230834
VSTAB    CSECT                                                 @DM15919 45230834
         DS    0H                                              @DM15919 45231034
STATTAB  DC   XL(7*STTLEN+L'STTNOE)'0' STATUS TABLE AREA       @DM15919 45231234
         SPACE 1                                               @DM15919 45231434
STATEND  EQU   *                                               @DM15919 45231634
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 47190025
**********                                                   ********** 47220025
**********         END OF DSERV1 - OVERLAY PHASE 1           ********** 47250025
**********                                                   ********** 47280025
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 12120025
         EJECT                                                 @D33GDVS 99745733
         SYSIR DSECT                                           @D33GDVS 99740733
         EJECT                                                 @D33GDVS 99745733
SYSCOM   SYSCOM                                                @D33GDVS 99750733
         SPACE 2                                               @D33GDVS 99755733
         MAPCOMR                                               @DM15919 99756734
         SPACE 3                                               @DM15919 99757734
TABRECS  DSECT                                                          00000475
TABREC   DS    CL80                                                     00000476
WORKA    DSECT
         COPY LIBPARMS
         END
        BKEND
        CATALS   A.VSTSO,0.0
        BKEND   A.VSTSO
*---------------------------------------------------------------------* 00000041
*  VSTSO:                        MAIN-PGM                   +++
*  BaseRegs:                     10,11,12
*  Internal Functions:
*    1. Display SPFM000/SPFM0002 (DOS/VS-Logon)             +
*    2. Verify USERID / PASSWORD (IBMUSER/IBM)              +
*    3. GETDATE / GETTIME                                   +
*    4. TSO-Ready                                           +
*  External Functions:
*    1. SPF                      (Call for Pgm VSSPF)       +
*    2. Help                     (Call for Pgm VSHELP)      -
*    3. Logoff                   (Change User)              +
*    4. Exit                     (Pgm-End)                  +
*  Parameter in:                    sub
*    -                           none
*  Parameter out:
*    SPFPARMS                    LOGUSER,LOGDATE,LOGTIME
*  Maps loaded:
*    1. SPMF000                  DOS/VS
*    2. SPMF002                  Logon User
*  Planned:
*    1. TSO-Functionalitiy       more Functions
*    2. GETVIS-Service
*---------------------------------------------------------------------* 00000041
         TITLE 'VSTSO - Main-Pgm'
         PRINT GEN
* --------------------------------------------------------------------* 00000041
VSTSO    START X'000078'
* --------------------------------------------------------------------  00000043
*        REGISTER EQUATES                                               00000044
* --------------------------------------------------------------------  00000045
R0       EQU   0                                                        00000046
R1       EQU   1                                                        00000047
R2       EQU   2                   WORK REGISTER                        00000048
R3       EQU   3                   WORK REGISTER                        00000049
R4       EQU   4                   WORK REGISTER                        00000050
R5       EQU   5                   LENGTH OF OUTPUT MESSAGE             00000052
R6       EQU   6                                                        00000053
R7       EQU   7                                                        00000055
R8       EQU   8                                                        00000057
R9       EQU   9                   DYNAMIC WORK AREAS                   00000058
BASEREG  EQU   10                  FIRST BASE REG                       00000059
BASEREG2 EQU   11                  SECOND BASE REG                      00000060
BASEREG3 EQU   12                  THIRD BASE REG                       00000061
R13      EQU   13                  SAVE AREA FOR THIS TASK              00000062
R14      EQU   14                                                       00000063
R15      EQU   15                                                       00000064
*                                                                       00000065
BASE     BALR  BASEREG,0                     ESTABLISH                  00000066
         USING *,BASEREG,BASEREG2,BASEREG3   ADDRESSABILITY             00000067
         LA    BASEREG2,2048(BASEREG)        INITIALIZE                 00000068
         LA    BASEREG2,2048(BASEREG2)       SECOND BASE                00000069
         LA    BASEREG3,2048(BASEREG2)       INITIALIZE                 00000070
         LA    BASEREG3,2048(BASEREG3)       THIRD BASE                 00000071
         B     GOON
         DC    C'***** VSTSO*****'     EYECATCHER
GOON     DS    0H
         LA    R13,SAVEAREA                  SAVE AREA FOR THIS TASK    00000072
*----------------------------------------------------------------
*     MAIN-CONTROL COMMUNICATIONS
*----------------------------------------------------------------
BCOM000  DS    0H                   BEGIN TO COMMUNICATE
         BAL   R7,STRTLOG           START TO LOGON
         BAL   R7,READY000          TSO-READY-ROUTINE
*
BCOM010  DS    0H
         BAL   R7,READM000          READ-MODIFIED
         CLC   INCMD(3),=CL3'SPF'
         BE    SPF000
         CLC   INCMD(4),=CL4'HELP'
         BE    HELP000
         CLC   INCMD(6),=CL6'LOGOFF'
         BE    BCOM000               START AGAIN FROM BEGINNING
         CLC   INCMD(4),=CL4'EXIT'
         BE    PGMEND00              LEAVE THE SYSTEM
         CLC   INCMD(4),=CL4'    '
         BE    BCOM020               NO INPUT, EXCEPT ENTER
         MVC   ICMD,INCMD
         BAL   R7,INV000             WRITE COMMAND NOT FOUND MSG
BCOM020  DS    0H
         BAL   R7,READY000
         B     BCOM010
*
*---------------------------------------------------------------------  00000075
*  TSO READY
*---------------------------------------------------------------------  00000075
READY000 DS    0H
         MVC   SRCHRC(2),RROW
         BAL   R8,NEWRC
         MVC   RROW(2),SRCHRC
         CLC   RROW(2),=XL2'4040'   ROW 1 COL 1 THEN WRITE ERASE
         BNE   READY010
         LA    R1,RDYECCW           LOAD WRITE / ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
         BR    R7                   RETURN TO CALLER
*
READY010 DS    0H
         LA    R1,RDYCCW            LOAD WRITE / NOERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
         BR    R7                   RETURN TO CALLER
*
*---------------------------------------------------------------------  00000075
*  FIND NEW ROW / COL-ADDR
*---------------------------------------------------------------------  00000075
NEWRC    DS    0H
         LA    R5,ROWCOLTB
NEWRCL00 DS    0H
         CLC   0(2,R5),SRCHRC
         BE    NEWRCEND
         CLC   0(2,R5),=XL2'FFFF'
         BE    NEWRCERR
         LA    R5,6(R5)             NXT TAB-ENTRY
         B     NEWRCL00
NEWRCERR DS    0H
         MVC   SRCHRC,=XL2'FFFF'    ERROR, NOT FOUND
         BR    R8                   RETURN TO CALLER
NEWRCEND DS    0H
         MVC   SRCHRC,2(R5)         FOUND
         MVC   IROW(2),4(R5)        COMMAND NOT FOUND
         BR    R8                   RETURN TO CALLER
*
SRCHRC   DS     XL2
*---------------------------------------------------------------------  00000075
*  SPF CALLING
*---------------------------------------------------------------------  00000075
SPF000   DS    0H
         LA    R1,PARMLIST                                              00000876
         CALL VSSPF
         CLC   LOGUSER,=CL8'IBMUSER'
         BE    WRREADY
         MVC   SERR(8),LOGUSER
         LA    R1,SPFCCW            ERROR IN VSSPF
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
         B     BCOM010              READY TSO READY
WRREADY  DS    0H
         LA    R1,RDYECCW           LOAD WRITE / ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
         B     BCOM010              READY TSO READY
*****    CNSLCCB
*
*---------------------------------------------------------------------  00000075
*  HELP INFORMATION
*---------------------------------------------------------------------  00000075
HELP000  DS    0H
         BR    R7                   RETURN TO CALLER
*
*---------------------------------------------------------------------  00000075
*  READ MODIFIED
*---------------------------------------------------------------------  00000075
READM000 DS    0H
         BAL   R8,CLRINAR           CLEAR INPUT AREA BEFORE READM
         LA    R1,READMCCW          READ Modified into BUFFER
         BAL   R8,EXCPRD00          READM/ Wait for I/O completion.
         OC    INCMD(64),BLANKS     UPPER CAPS
         BR    R7                   RETURN TO CALLER
*
*---------------------------------------------------------------------  00000075
*  INVALID INPUT
*---------------------------------------------------------------------  00000075
INV000   DS    0H
         LA    R1,INVCCW            WRITE NOREASE
         BAL   R8,EXCPWR00          READM/ Wait for I/O completion.
         BR    R7                   RETURN TO CALLER
*
*---------------------------------------------------------------------  00000075
*  CLEAR INPUT AREA
*---------------------------------------------------------------------  00000075
CLRINAR  DS    0H
         MVI   BUFFER,C' '
         MVC   BUFFER+1(255),BUFFER
         BR    R8                   RETURN TO CALLER
*
*---------------------------------------------------------------------  00000075
*  LOGON
*---------------------------------------------------------------------  00000075
STRTLOG  DS    0H
         LOAD  SPFM000,MAPADDR      LOAD DOSVS-MAP nach MAPADDR
         MVC   WRECCW+6(2),MAPLEN   LEN TO CCW
         LA    R1,WRECCW            LOAD WRITE / ERASE-CCW-ADDR
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
*
         LOAD  SPFM002,MAPADDR      LOGON-MAP
         MVC   WRCCW+6(2),MAPLEN    LEN TO CCW
LOGERMSG DS    0H
         LA    R1,WRCCW             WRITE w/o Erase
         BAL   R8,EXCPWR00          EXEC / Wait for I/O completion.
*
LOGON    DS    0H
         LA    R1,READMCCW          READ Modified into BUFFER
         BAL   R8,EXCPRD00          READM/ Wait for I/O completion.
         MVI   LOGONSW,C'1'         LOGON IS NESS.
         BAL   R8,CHKLOGON
         CLI   LOGONSW,C'0'         LOGON SUCC. ?
         BNE   LOGERMSG             NO, RETRY
         BR    R7                   RETURN TO CALLER
*
*----------------------------------------------------------------
CHKLOGON EQU   *                   CHECK LOGON                          00001189
*----------------------------------------------------------------
         ST    R8,SAVR8
         CLI   INAID,X'F3'         PF3 ?                                00001190
         BE    PGMEND00            YES, GOOD-BYE FOR NOW                00001191
         OC    INUSR,BLANKS        FORCE UPPER CASE                     00001192
         OC    INPWD,BLANKS                                             00001193
         CLC   INUSR(7),=C'IBMUSER'                                     00001194
         BNE   LOGONERR                                                 00001195
         CLC   INPWD(3),=C'IBM'
         BNE   LOGONERR                                                 00001197
         MVI   LOGONSW,C'0'                                             00001198
         MVC   LOGUSER,INUSR       SAVE USERID
         BAL   R8,GETDATE
         BAL   R8,GETTIME
         L     R8,SAVR8
         BR    R8                  OK, START COMMUNICATION NOW          00001199
LOGONERR DS    0H
         L     R9,MAPVCNT           LOAD ADDR-LIST-ADDR
         ICM   R9,8,X00             CLEAR CNT-BYTE
         L     R9,0(R9)             Load 1. VAR-OUT-ADDR
         MVC   0(41,R9),=C'Incorrect User-ID or Password - try again'
         L     R8,SAVR8
         BR    R8                                                       00001201
SAVR8    DS    F
*
PARMLIST DC    A(SPFPARMS)
SPFPARMS DS    0CL21
LOGUSER  DS    CL8
LOGDATE  DS    CL8
LOGTIME  DS    CL5
*
LOGONSW  DC    CL1'1'
*----------------------------------------------------------------
*         Handle EXCP Error
*----------------------------------------------------------------
*
*----------------------------------------------------------------
*     PGMEND - Programm END
*----------------------------------------------------------------
PGMEND00 DS   0H                 BYE for now, write erase
         LA    R1,LOGOFCCW       WRITE initialized with Logoff-Map
         BAL   R8,EXCPWR00       EXEC / Wait for I/O completion.
PGMEND   DS   0H
         EOJ
         EJECT
*----------------------------------------------------------------
PDUMP    DS   0H
*----------------------------------------------------------------
         ST   R1,RET1
         PDUMP VSTSO,IOEND
         L    R1,RET1
         BR   R8
RET1     DS   F
*----------------------------------------------------------------
*----------------------------------------------------------------
GETDATE  DS    0H
         COMRG
         MVC   WDATE,0(R1)    DATE From COMRG
         MVC   LOGDATE(2),WDD
         MVI   LOGDATE+2,C'.'
         MVC   LOGDATE+3(2),WMM
         MVI   LOGDATE+5,C'.'
         MVC   LOGDATE+6(2),WYY
         BR    R8
WDATE    DS    0CL8
WMM      DS    CL2
         DS    CL1
WDD      DS    CL2
         DS    CL1
WYY      DS    CL2
X00      DC    XL1'00'
*
GETTIME  DS    0H
         GETIME
         ST    R1,TIMEPD         TIME 00hhmmss
         UNPK  WTIME,TIMEPD
         MVC   LOGTIME(2),WHOUR
         MVI   LOGTIME+2,C':'
         MVC   LOGTIME+3(2),WMIN
         BR    R8
TIMEPD   DS    F
WTIME    DS    0CL7
         DS    CL1
WHOUR    DS    CL2
WMIN     DS    CL2
         DS    CL2
*----------------------------------------------------------------
*     EXCP - EXECUTE CHANNEL PROGRAM READ MOFIFIED
*----------------------------------------------------------------
EXCPRD00 DS    0H                Loop
         STCM  R1,7,EXCPCCB+9    STORE CCW-ADDR in CCB.
         LA    R1,EXCPCCB        Issue COMMAND
EXCPRD01 DS    0H                Loop
         EXCP  (1)
*
         ST    1,SAV1
         SETIME 1,EXCPTECB  INTERVAL SET 2 SECDS
         WAIT     EXCPTECB  WAIT
         L     1,SAV1
*
         TM    2(1),X'80'    ECP POSTED ?
         BO    EXCPRD02      Yes, CHECK AID
         SVC   7             NO SVC 7
EXCPRD02 DS    0H
         CLI   INAID,X'60'   NO DATA INPUT YET ?
         BE    EXCPRD01      YES, LOOP FOR READ
*
         BR    R8            OK, FINISHED
SAV1     DS    F
EXCPTECB TECB                CREATE TIMER EVENT CONTROL BLOCK
*----------------------------------------------------------------
*     EXCP - EXECUTE CHANNEL PROGRAM WRITE ERASE
*----------------------------------------------------------------
EXCPWR00 DS    0H
         STCM  R1,7,EXCPCCB+9    STORE CCW-ADDR   in CCB.
         LA    R1,EXCPCCB        Issue Command
         EXCP  (1)
         WAIT  (1)
         BR    R8
*----------------------------------------------------------------
         TITLE 'EXCP DISPLAY HANDLER - CONSTANTS AND VARIABLES'
*----------------------------------------------------------------
         DS    0D
EXCPCCB  CCB   SYS007,DUMMY,X'0000'
*
*
WRECCW   CCW   X'05',MAPSTART,X'20',L'MAPSTART   WRITE-ERASE
WRCCW    CCW   X'01',MAPSTART,X'20',L'MAPSTART   WRITE-NOERASE
LOGOFCCW CCW   X'05',LOGOFMG,X'20',LOGOFMGL      WRITE LOGOF SCREEN
RDYECCW  CCW   X'05',READYMG,X'20',L'READYMG     READY ERASE CCW
RDYCCW   CCW   X'01',READYMG,X'20',L'READYMG     READY NOERASE CCW
INVCCW   CCW   X'01',INVINPT,X'20',L'INVINPT     INVALID INPUT CCW
SPFCCW   CCW   X'01',SPFERR,X'20',L'SPFERR       ERROR IN SPF
*
EWALTCCW CCW   X'0D',BLANKS,X'20',1              ERASE/WRITE ALT.
ERASAUNP CCW   X'0F',BLANKS,X'20',1              ERASE ALL UNPROT
SENSECCW CCW   X'04',BLANKS,X'20',L'BLANKS       SENSE
WRITECCW CCW   X'01',BUFFER,X'20',L'BUFFER       WRITE SCREEN
WRITESF  CCW   X'11',WSFMSG,X'20',WSFMSGL        WRITE STRUC FIELD
READMCCW CCW   X'06',BUFFER,X'20',L'BUFFER       READ MODIFIED.
READBCCW CCW   X'02',BUFFER,X'20',L'BUFFER       READ BUFFER
NOPCCW   CCW   X'03',BUFFER,X'20',L'BUFFER       NO OPERATION
*
*----------------------------------------------------------------
* BUFFER CONTROL ORDERS AND ORDER CODES
*----------------------------------------------------------------
* SF     X'1D',ATTR             START FIELD, ATTRIBUTE
* SBA    X'11',ADR1,ADR2        SET BUFFER ADDRESS, ADR1, ADR2
* IC     X'13'                  INSERT CURSOR
* PT     X'05'                  PROGRAM TAB
* RA     X'3C',ADR1,ADR2,C' '   REPEAT TO ADRESS, CHAR TO REPEAT
* SFE    X'29',COUNT,TYPE,VALUE START FIELD EXTENDED
* EUA    X'12',ADR1,ADR2        ERASE UNPROT TO ADDRESS
* MF     X'2C',COUNT            MODIFY FIELD
* SA     X'2B',TYPE,VALUE       SET ATTRIBUTE
*----------------------------------------------------------------
*---------------------------------------------------------------------
*    READY
*---------------------------------------------------------------------
READYMG  DS  0CL25
         DC  XL1'C3'             WCC Reset MDT
         DC  XL1'11'             SBA CONTROL ORDER
RROW     DC  XL1'5B'             ROW
RCOL     DC  XL1'60'             COLUMN
         DC  XL5'1DF02842F2'     SF PROT  SA EXTENDED ATTR RED
         DC  CL6'READY>'
         DC  XL1'1D'             SF
         DC  XL1'40'             ATTR UNPROT
         DC  XL1'13'             IC   INSERT CURSOR
         DC  10XL1'00'           FILLER
*---------------------------------------------------------------------
*    INVALID INPUT
*---------------------------------------------------------------------
INVINPT  DS  0CL35
         DC     XL1'C3'   WCC Reset MDT
         DC     XL1'11'             SBA CONTROL ORDER
IROW     DC     XL1'5C'             ROW
ICOL     DC     XL1'F0'             COLUMN
         DC     XL5'1DF02842F2'     SF PROT  SA EXTENDED ATTR RED
         DC     C'COMMAND '
ICMD     DC     C'        '
         DC     C'NOT FOUND'
*---------------------------------------------------------------------
*    INVALID INPUT
*---------------------------------------------------------------------
SPFERR   DS  0CL35
         DC     XL1'C3'   WCC Reset MDT
         DC     XL1'11'             SBA CONTROL ORDER
SROW     DC     XL1'5C'             ROW
SCOL     DC     XL1'F0'             COLUMN
         DC     XL5'1DF02842F2'     SF PROT  SA EXTENDED ATTR RED
         DC     C'ERROR:  '
SERR     DC     C'        '
         DC     C'IN VSSPF '
*---------------------------------------------------------------------
*    ROW / COL TAB
*---------------------------------------------------------------------
ROWCOLTB DS     0H
         DC     XL6'4040C260C3F0'     Row  1 Col 1
         DC     XL6'C150C3F0C540'        .
         DC     XL6'C260C540C650'        .
         DC     XL6'C3F0C650C760'        .
         DC     XL6'C540C760C8F0'        .
         DC     XL6'C650C8F04A40'        .
         DC     XL6'C7604A404B50'        .
         DC     XL6'C8F04B504C60'        .
         DC     XL6'4A404C604DF0'        .
         DC     XL6'4B504DF04F40'        .
         DC     XL6'4C604F405050'        .
         DC     XL6'4DF05050D160'        .
         DC     XL6'4F40D160D2F0'        .
         DC     XL6'5050D2F0D440'        .
         DC     XL6'D160D440D550'        .
         DC     XL6'D2F0D550D660'        .
         DC     XL6'D440D660D7F0'        .
         DC     XL6'D550D7F0D940'        .
         DC     XL6'D660D9405A50'        .
         DC     XL6'D7F05A505B60'        .
         DC     XL6'D9405B605CF0'        .
         DC     XL6'5A505CF04040'        .
         DC     XL6'5B604040C150'        .
         DC     XL6'5CF0C260C3F0'     Row 24 Col 1
         DC     XL6'FFFFFFFFFFFF' TAB-END
*---------------------------------------------------------------------
*    DUMMY
*---------------------------------------------------------------------
DUMMY   EQU   *
  DC    XL1'C3'   WCC Reset MDT
       $SBA  (22,6,N)
  DC    C'*** DUMMY ***'
DUMMYL   EQU   *-DUMMY
*---------------------------------------------------------------------
*    Logoff-Screen            Zeile 22 -24
*---------------------------------------------------------------------
LOGOFMG EQU   *
  DC    XL1'C3'   WCC Reset MDT
       $SBA  (22,6,N)
  DC    C'Bye for now....'
LOGOFMGL EQU   *-LOGOFMG
*----------------------------------------------------------------------
*    WSF TEST                 Zeile 22 -24
*----------------------------------------------------------------------
WSFMSG   EQU   *
  DC    XL2'0020'  LEN
  DC    XL2'4000'
  DC    C'dies ist der WSF-Text'
WSFMSGL  EQU   *-WSFMSG                                                 00001441
*
BLANKS   DC    65CL1' '
SAVEAREA DS    18F
*---------------------------------------------------------------------
*        AREA FOR INBOUND-MSGS
*---------------------------------------------------------------------
BUFFER   DS    0CL2000
INAID    DS    CL1                 AID                                  00001658
INCURSOR DS    CL2                 CURSOR SBA                           00001659
INSBA    DS    CL3                 SBA                                  00001660
INCMD    DS    0CL8                COMMAND                              00001661
INUSR    DS    CL8                 USER-ID                              00001661
         DS    CL3                 SBA                                  00001662
INPWD    DS    CL8                 PASSWORD                             00001663
         ORG   BUFFER+2000                                              00001664
IOEND    EQU   *                                                        00001665
*---------------------------------------------------------------------
*        LOAD-AREA FOR OUTBOUND-MAPS
*---------------------------------------------------------------------
         DS    0F
MAPADDR  DS    0CL2008
MAPNAME  DS    CL8
MAPEYEC  DS    CL8
         DS    CL2
MAPLEN   DS    CL2                MAP-LEN
MAPVCNT  DS    AL1(0)             VAR-FIELD COUNT
MAPVLST  DS    AL3(0)             ADDR MAP-VAR-OUTPUT-LIST
MAPSTART DS    CL2000
         END
        BKEND
        CATALS   A.VSUTI,0.0
        BKEND   A.VSUTI
*------------------------------------------------------------------
*     NAME: VSUTI
*     TYPE: SUB-ROUTINE
* FUNCTION: Utility-Functions for SBA/RC and numbers
*
* INTERNAL
*
*
* EXTERNAL
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
VSUTI    CSECT                                                          00000016
         STM   R14,R12,12(R13)         STORE REGS IN SAVEAREA           00000018
         LR    R6,R15                 ESTABLISH ADDRESSABILITY
         USING VSUTI,R6                R11 NOW BASE REGISTER            00000020
         B     GOON
         DC    C'***** VSUTI *****'     EYECATCHER
GOON     DS    0H
         ST    R13,SAVEAREA+4          STORE MAIN PGM SAVEAREA          00000024
         LR    R10,R13                 SAVE R13                         00000025
         LA    R13,SAVEAREA            SAVE AREA FOR THIS TASK          00000026
         ST    R13,8(R10)              STORE EXIT SAVE ADDRESS          00000027
* --------------------------------------------------------------------* 00000028
*      STATEMENTS FOR SET UP FIELDS NEEDED FOR VSUTI
* --------------------------------------------------------------------* 00000030
         L     R2,0(R1)                PARMS ADDRESS                    00000031
         ST    R2,VISADR               SAVE                             00000032
         USING WORKA,R2
         L     R9,PBUFADR              LOAD DATATAB ADDRESS             00000034
         ST    R9,TABRECSA             SET  TABRECS ADDRESS             00000035
         USING TABRECS,R9              ESTABLISH ADDRESSABILITY         00000036
******   CNSLOUT MSG='PGM-START'
******   CNSLOUT ADR=PARMS
******   CNSLOUT ADR=PCCHHBI
         CLC   PFUNC,=CL3'SRC'         SBA TO ROW / COLUM
         BE    SRC000
         CLC   PFUNC,=CL3'RCS'         ROW / COLUM  TO SBA
         BE    SRC000
         CLC   PFUNC,=CL3'BRC'         BUFFER TO ROW / COLUM
         BE    SRC000
         CLC   PFUNC,=CL3'B2C'         CCHHRE TO CHAR 'CCC HH RR EE'
         BE    B2C000
         CLC   PFUNC,=CL3'C2B'         CCC HH RR EE TO CCHHRE
         BE    C2B000
         CLC   PFUNC,=CL3'BNC'         BINARY NUMBER TO CHAR
         BE    BNC000
         CLC   PFUNC,=CL3'CNB'         CHAR TO BINARY NUMBER
         BE    CNB000
         CLC   PFUNC,=CL3'B2H'         BINARY TO HEXADECIMAL CHARS
         BE    B2H000
         MVC   PRETCOD,=CL3'991'        ERROR: INVALID FUNCTION
* --------------------------------------------------------------------* 00000065
*     PGMEND Program-ENd                                              * 00000066
* --------------------------------------------------------------------* 00000067
PGMEND   DS    0H
******   CNSLOUT MSG='PGM-END'
******   CNSLOUT ADR=PARMS
******   CNSLOUT ADR=PCCHHBI
         L     R9,VISADR           LOAD PARAMETER-ADDR
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
* --------------------------------------------------------------------* 00000065
*     SRC - SBA TO ROW / COLUMN CALC                                  * 00000066
* --------------------------------------------------------------------* 00000067
SRC000   DS    0H
         MVC   SBAWORK,PSBAWORK
         TR    SBAWORK,EBCTOBIN    ECBDIC TO BIN
         NC    SBAWORK,=X'3F3F'    BIT 15 AND BIT 7 OFF
         SR    R4,R4
         SR    R5,R5
         IC    R4,SBAWORK
         IC    R5,SBAWORK+1
         SLL   R4,6
         AR    R5,R4
         SR    R4,R4
         D     R4,=F'80'
         LA    R4,1(R4)
         LA    R5,1(R5)
         STC   R5,PROW           1-43    1 => ROW 1
         STC   R4,PCOL           1-80    2 => COLUMN 2
         B     PGMEND              RETURN
* --------------------------------------------------------------------* 00000065
*     RCS - ROW / COLUMN TO SBA CALC                                  * 00000066
* --------------------------------------------------------------------* 00000067
RCS000   DS    0H
         XR    R0,R0
         XR    R1,R1
         IC    R0,PROW
         IC    R1,PCOL
         BCTR  R0,0
         BCTR  R1,0
         MH    R0,=H'80'
         AR    R1,R0
         SLL   R1,2
         STCM  R1,2,SBAWORK
         SRL   R1,2
         STCM  R1,2,SBAWORK+1
         NC    SBAWORK,=XL2'3F3F'
         TR    SBAWORK,BINTOEBC
         MVC   PSBAWORK,SBAWORK
         B     PGMEND              RETURN
* --------------------------------------------------------------------* 00000065
*     BRC - BUFFER TO ROW / COLUMN CALC                               * 00000066
* --------------------------------------------------------------------* 00000067
BRC000   DS    0H
         MVC   SBAWORK,PSBAWORK
         LH    R0,SBAWORK
         CH    R0,=H'4096'
         BH    BRC010            ERROR: BUFFER-ADDR TOO LARGE
*
         STC   R0,SBAWORK+1
         NI    SBAWORK+1,X'3F'
         SRL   R0,6
         STC   R0,SBAWORK
         TR    SBAWORK,BINTOEBC
         MVC   PROW(2),SBAWORK
         B     PGMEND              RETURN
BRC010   DS    0H
         MVC   PRETCOD,=CL3'990'  ERROR: BUFFER-ADDR TOO LARGE
         B     PGMEND
* --------------------------------------------------------------------* 00000065
*     B2C - CCHHRE TO 'CCC HH RR EE'                                  * 00000066
* --------------------------------------------------------------------* 00000067
*-----------------+-------------+---------------+---------------------* 00000067
*  Input allowed  :  C*R = (3)  ;   CCHHR = (5) ;   CCHHRE = (6)
*  Possible Output: 'CCC HH RR' ;   'CCC HH RR' ;  'CCC HH RR EE'
*-----------------+-------------+---------------+---------------------* 00000067
* * =  BIT 1 X'40' of Tracknumber means + 256 Cyls
* --------------------------------------------------------------------* 00000067
*-------
*  CC
*-------
B2C000   DS    0H
*******  CNSLOUT ADR=PCCHHBI
         XC    WORKF,WORKF
         MVC   PCCHHAR,BLANKS
         CLI   PCCHHBIF,X'03'      FORMAT 03 ?
         BNE   B2C020
         MVC   WORKF+3(1),PCCHHBI  MOVE C
         TM    PCCHHBI+1,X'40'     256 Indicator ON ?
         BNO   B2C030     RH B2C020 NO
         NI    PCCHHBI+1,X'BF'     SWITCH 40 OFF
B2C010   DS    0H
         LH    R11,WORKH
         AH    R11,=H'256'
         STH   R11,WORKH
         B     B2C030
B2C020   DS    0H                  NO FORMAT 03
         MVC   WORKH,PCCHHBI
B2C030   DS    0H
         BAL   R11,CALCSTBC
         MVC   PCCHHAR(3),RESULT+13
******   CNSLOUT ADR=PCCHHBI
*-------
*  HH
*-------
         XC    WORKF,WORKF
         CLI   PCCHHBIF,X'03'     FORMAT 03 ?
         BNE   B2C120
         MVC   WORKH+1(1),PCCHHBI+1
         B     B2C130
B2C120   DS    0H
         MVC   WORKH,PCCHHBI+2
B2C130   DS    0H
         BAL   R11,CALCSTBC
         MVC   PCCHHAR+4(2),RESULT+14
*****    CNSLOUT ADR=PCCHHBI
*
*-------
*  R
*-------
         XC    WORKF,WORKF
         CLI   PCCHHBIF,X'03'     FORMAT 03 ?
         BNE   B2C220
         MVC   WORKH+1(1),PCCHHBI+2
         B     B2C230
B2C220   DS    0H
         MVC   WORKH+1(1),PCCHHBI+4
B2C230   DS    0H
         BAL   R11,CALCSTBC
         MVC   PCCHHAR+7(2),RESULT+14
*****    CNSLOUT ADR=PCCHHBI
*
*-------
*  E
*-------
         XC    WORKF,WORKF
         CLI   PCCHHBIF,X'06'     FORMAT 06 ?
         BL    PGMEND
         MVC   WORKH+1(1),PCCHHBI+5
         BAL   R11,CALCSTBC
         MVC   PCCHHAR+10(2),RESULT+14
*
         B     PGMEND
* --------------------------------------------------------------------* 00000065
*     C2B - 'CCC HH RR EE' TO CCHHRE
* --------------------------------------------------------------------* 00000067
*-----------------+-------------+---------------+---------------------* 00000067
*  Input allowed: 'CCC HH RR'(9); 'CCC HH RR'(9); 'CCC HH RR EE' (12)
*  Output:      :  C*R = (3)    ;   CCHHR = (5) ;   CCHHRE = (6)
*-----------------+-------------+---------------+---------------------* 00000067
* * =  BIT 1 X'40' of Tracknumber means + 256 Cyls
* --------------------------------------------------------------------* 00000067
C2B000   DS    0H
*-------
* CCC
*-------
         MVI   HHFLAG,X'00'        INIT FLAG FOR OVERRUN
         XC    WORKD,WORKD
         XC    WORKF,WORKF
         MVC   WORKF+1(3),PCCHHAR  CCC
         BAL   R11,CALCSTCB        CHARACTER TO BINARY
         CLI   PCCHHBIF,X'03'
         BNE   C2B010
         CH    R4,=H'255'
         BNH   C2B015
         SH    R4,=H'256'
         MVI   HHFLAG,X'FF'        SET FLAG FOR OVERRUN
C2B015   DS    0H
         STC   R4,PCCHHBI          SAVE  AS C
         B     C2B020
C2B010   DS    0H
         MVC   PCCHHBI(2),WORKF+2  SAVE AS CC
*-------
* HH
*-------
C2B020   DS    0H
         XC    WORKD,WORKD
         XC    WORKF,WORKF
         MVC   WORKH,PCCHHAR+4      HH
         BAL   R11,CALCSTCB         CHARACTER TO BINARY
         CLI   PCCHHBIF,X'03'
         BNE   C2B110
         STC   R4,PCCHHBI+2         SAVE AS H
         CLI   HHFLAG,X'FF'
         BNE   C2B120
         OI    PCCHHBI+2,X'40'      SET OVERRUN BIT ON
         B     C2B030
C2B110   DS    0H
         MVC   PCCHHBI+2(2),WORKF+2 SAVE AS HH
C2B120   DS    0H
*-------
* R
*-------
C2B030   DS    0H
         XC    WORKD,WORKD
         XC    WORKF,WORKF
         MVC   WORKH,PCCHHAR+7      HH
         BAL   R11,CALCSTCB         CHARACTER TO BINARY
         CLI   PCCHHBIF,X'03'
         BNE   C2B210
         STC   R4,PCCHHBI+2         SAVE AS R
         CLI   HHFLAG,X'FF'
         BNE   PGMEND
         OI    PCCHHBI+2,X'40'      SET OVERRUN BIT ON
         B     PGMEND
C2B210   DS    0H
         MVC   PCCHHBI+4(1),WORKF+3 SAVE AS R
C2B220   DS    0H
*-------
* E
*-------
C2B040   DS    0H
         XC    WORKD,WORKD
         XC    WORKF,WORKF
         CLI   PCCHHBIF,X'06'
         BNE   C2B310
         MVC   WORKH,PCCHHAR+10     HH
         BAL   R11,CALCSTCB         CHARACTER TO BINARY
         MVC   PCCHHBI+5(1),WORKF+3 SAVE AS E
         B     PGMEND
C2B310   DS    0H
         MVI   PCCHHBI+5,X'00'   SAVE AS 00
C2B320   DS    0H
         B     PGMEND
*-------------------------------------
CALCSTBC DS    0H  Binary TO Character
*-------------------------------------
         MVC   RESULT,EDPAT
         L     R4,WORKF
         CVD   R4,WORKD
         OI    WORKD+7,X'0F'
         ED    RESULT,WORKD
         BR    R11                 RETURN TO CALLER
*------------------------------------------------
CALCSTCB DS    0H  Character to Binary CCC HH NN
*------------------------------------------------
         PACK  WORKD,WORKF
         CVB   R4,WORKD
         ST    R4,WORKF
         BR    R11                 RETURN TO CALLER
*--------------------------------------------------
CALCNMCB DS    0H  Character to Binary any Number
*--------------------------------------------------
         PACK  WORKD,PCCHHAR
******** CNSLOUT ADR=PCCHHAR,LEN=12
         CVB   R4,WORKD
         ST    R4,WORKF
         BR    R11                 RETURN TO CALLER
*---------------------------------------------------------------------* 00000065
*     BNC - Binary Number to CHAR
*-----------------+-------------+---------------+---------------------* 00000067
*  Input allowed: max 4 Bytes   ;               ;
*  Output:      :   ddddddddd   ;               ;
*-----------------+-------------+---------------+---------------------* 00000067
BNC000   DS    0H
*-------
         XC    WORKD,WORKD
         XC    WORKF,WORKF
         MVC   WORKF,PCCHHBI+2
         BAL   R11,CALCSTBC        BINARY TO CHARACTER
         MVC   PCCHHAR,RESULT+4
         B     PGMEND
*
*---------------------------------------------------------------------* 00000065
*     CNB - Character Number to Binary
*-----------------+-------------+---------------+---------------------* 00000067
*  Input allowed: max 9 DIGITS  ;               ;
*  Output:      :        bbbb   ;               ;
*-----------------+-------------+---------------+---------------------* 00000067
CNB000   DS    0H
*-------
         BAL   R11,CALCNMCB        CHARACTER TO BINARY
         XC    PCCHHBI,PCCHHBI
         MVC   PCCHHBI+2(4),WORKF
         B     PGMEND
*
*---------------------------------------------------------------------* 00000065
*     B2H - Binary to Hexadecimal Characters
*-----------------+-------------+---------------+---------------------* 00000067
*  Input allowed: max 6 BYTES   ;               ;
*  Output:      : max11 Bytes   ;               ;
*-----------------+-------------+---------------+---------------------* 00000067
B2H000   DS    0H
*-------
         MVC   PACK1,PCCHHBI
         UNPK  UNPACK,PACKED
         TR    UNPACK,TRTAB
         MVC   PCCHHAR,UNPACK
         B     PGMEND
*
TRTAB    EQU   *-240
         DC    CL16'0123456789ABCDEF'
*
UNPACK   DS    CL13
PACKED   DS    0CL7
PACK1    DS    CL6
PACK2    DS    CL1
BLANKS   DC    12CL1' '
WORKD    DS    D
SAVEST8  DS    5F
WORKF    DS    0F
         DS    H
WORKH    DS    H
EDPAT    DC    XL16'40202020202020202020202020212020'
RESULT   DS    CL16
X00      DC    XL1'00'
HHFLAG   DC    XL1'00'
* --------------------------------------------------------------------* 00000386
******   CNSLCCB
* --------------------------------------------------------------------* 00000386
*        WORKFIELDS                                                   * 00000387
* --------------------------------------------------------------------* 00000388
         DS    0D                                                       00000389
*                                                                       00000391
* --------------------------------------------------------------------* 00000404
*        MESSAGE TABLES                                               * 00000405
* --------------------------------------------------------------------* 00000406
         DS    0F                       ALIGNEMENT                      00000407
* --------------------------------------------------------------------* 00000369
*        SAVE AREA DECLARATIONS                                       * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F                                                       00000372
SAV8     DS    2F
SAVEAREA DC    XL72'00'                 MONITOR TASK SAVE AREA           0000037
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS          00000379
VISADR   DS    F                                                        00000458
SBAWORK  DS    CL2                                                      00000458
*                                                                       00000459
EBCTOBIN DC    256AL1(*-EBCTOBIN)                                       00001546
         ORG   EBCTOBIN+X'40'                                           00001547
         DC    X'C0'                                                    00001548
         ORG   EBCTOBIN+X'4A'                                           00001549
         DC    X'CACBCCCDCECF'                                          00001550
         ORG   EBCTOBIN+X'50'                                           00001551
         DC    X'D0'                                                    00001552
         ORG   EBCTOBIN+X'5A'                                           00001553
         DC    X'DADBDCDDDEDF'                                          00001554
         ORG   EBCTOBIN+X'60'                                           00001555
         DC    X'E0E1'                                                  00001556
         ORG   EBCTOBIN+X'6A'                                           00001557
         DC    X'EAEBECEDEEEF'                                          00001558
         ORG   EBCTOBIN+X'7A'                                           00001559
         DC    X'FAFBFCFDFEFF'                                          00001560
         ORG                                                            00001561
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        00001563
BINTOEBC DC    X'40C1C2C3C4C5C6C7C8C94A4B4C4D4E4F'  0                   00001564
         DC    X'50D1D2D3D4D5D6D7D8D95A5B5C5D5E5F'  1                   00001565
         DC    X'6061E2E3E4E5E6E7E8E96A6B6C6D6E6F'  2                   00001566
         DC    X'F0F1F2F3F4F5F6F7F8F97A7B7C7D7E7F'  3                   00001567
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                   * 00000456
* --------------------------------------------------------------------* 00000015
*        REGISTER EQUATES                                             * 00000351
* --------------------------------------------------------------------* 00000352
R0       EQU   0                                                        00000353
R1       EQU   1                                                        00000354
R2       EQU   2                                                        00000355
R3       EQU   3                                                        00000356
R4       EQU   4                                                        00000357
R5       EQU   5                                                        00000358
R6       EQU   6                                                        00000359
R7       EQU   7                                                        00000360
R8       EQU   8                                                        00000361
R9       EQU   9                                                        00000362
R10      EQU   10                                                       00000363
R11      EQU   11                                                       00000364
R12      EQU   12                                                       00000365
R13      EQU   13                                                       00000366
R14      EQU   14                                                       00000367
R15      EQU   15                                                       00000368
* --------------------------------------------------------------------* 00000424
         LTORG                                                          00000454
* --------------------------------------------------------------------* 00000426
         DS    0D                                                       00000427
* --------------------------------------------------------------------* 00000474
WORKA    DSECT
         COPY  LIBPARMS                                                 00000460
TABRECS  DSECT                                                          00000475
TABREC   DS    CL80                                                     00000476
*                                                                       00001545
*                                                                       00001562
*                                                                       00001562
* --------------------------------------------------------------------* 00000477
         END                                                            00000481
        BKEND
        CATALS   A.VSVTOC,0.0
        BKEND   A.VSVTOC
*------------------------------------------------------------------
*     NAME: VSVTOC
*     TYPE: SUB-ROUTINE
* FUNCTION: List a VTOC
*
* INTERNAL
*
*
* EXTERNAL
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
*        REGISTER EQUATES
* --------------------------------------------------------------------* 00000352
R0       EQU   0                                                        00000353
R1       EQU   1                                                        00000354
R2       EQU   2                                                        00000355
R3       EQU   3                                                        00000356
R4       EQU   4                                                        00000357
R5       EQU   5                                                        00000358
R6       EQU   6                                                        00000359
R7       EQU   7                                                        00000360
R8       EQU   8                                                        00000361
R9       EQU   9                                                        00000362
R10      EQU   10                                                       00000363
R11      EQU   11                                                       00000364
R12      EQU   12                                                       00000365
R13      EQU   13                                                       00000366
R14      EQU   14                                                       00000367
R15      EQU   15                                                       00000368
VSVTOC    CSECT                                                          0000001
         STM   R14,R12,12(R13)         STORE REGS IN SAVEAREA           00000018
         USING VSVTOC,R11,R12          R11 NOW BASE REGISTER            00000020
         LR    R11,R15                 ESTABLISH ADDRESSABILITY         00000021
         B     GOON
         DC    C'***** VSVTOC *****'   EYE-CATCHER
GOON     DS    0H
         LA    R12,4095(R11)                                            00000022
         LA    R12,1(R12)              SECOND BASE                      00000023
         ST    R13,SAVEAREA+4          STORE MAIN PGM SAVEAREA          00000024
         LR    R10,R13                 SAVE R13                         00000025
         LA    R13,SAVEAREA            SAVE AREA FOR THIS TASK          00000026
         ST    R13,8(R10)              STORE EXIT SAVE ADDRESS          00000027
* --------------------------------------------------------------------* 00000028
*        STATEMENTS FOR SET UP FIELDS NEEDED FOR LIBRM                * 00000029
* --------------------------------------------------------------------* 00000030
         L     R2,0(R1)                PARMS ADDRESS                    00000031
         ST    R2,PARMADDR             SAVE                             00000032
         USING WORKA,R2
         L     R9,PBUFADR              LOAD DATATAB ADDRESS             00000034
         ST    R9,TABRECSA             SET  TABRECS ADDRESS             00000035
         USING TABRECS,R9              ESTABLISH ADDRESSABILITY         00000036
* --------------------------------------------------------------------* 00000065
*     PGM-START                                                       * 00000066
* --------------------------------------------------------------------* 00000067
         CNSLOUT MSG='PGM-START'
         XC    PCCHHBI,PCCHHBI
         MVI   PCCHHBI+4,X'03'         VOLUME LABEL ADDR
         MVC   PNORL,=F'3'             NUMBER OF RECORDS TO READ
         BAL   R8,CVSRDSK
         LR    R10,R9
         LA    R10,240(R10)
******   PDUMP (R9),(R10)
         MVC   PCCHHBI(5),11(R9)       VTOC-ADDR
         BAL   R8,CVSRDSK
         LR    R10,R9
         LA    R10,240(R10)
*******  PDUMP (R9),(R10)
*
         LA    R10,20               X'14' for Partition COMREG ADDR
         L     R9,0(R10)           LOAD PUBTAB-ADDR
         CNSLOUT REG=9
         BAL   R8,PDUMP            PUBTAB
         LH    R9,76(R9)           DISPL X'4C' IS LUBTAB-ADDR
         CNSLOUT REG=9
         BAL   R8,PDUMP
         B     PGMEND
PDUMP    DS    0H
         LR    R10,R9
         LA    R10,500(R10)
         PDUMP (R9),(R10)
         BR    R8
*----------------------------------------------------------------
*  CVSRDSK CALL READ-DISK-PGM
*----------------------------------------------------------------
CVSRDSK  DS    0H
         CNSLOUT MSG='CALLVSRDSK'
         ST    R8,CRDSKR8          SAVE R8
*
         LA    R1,PARMS
         ST    R1,PARMLIST                                              00000735
         LA    R1,PARMLIST                                              00000736
*
         L     R15,VSRDSKAD
         BALR  R14,R15
*                                   ----------------------------------- 00000720
*        VSRDSK                     READ FROM DSK
*                                   ----------------------------------- 00000720
         L     R8,CRDSKR8           SAVE R8
         BR    R8                   RETURN TO CALLER
*
CRDSKR8  DS    F
VSRDSKAD DC    V(VSRDSK)
* --------------------------------------------------------------------* 00000065
*     PGMEND Program-ENd                                              * 00000066
* --------------------------------------------------------------------* 00000067
PGMEND   DS    0H
         CNSLOUT MSG='PGM-END'
         L     R9,PARMADDR         LOAD PARAMETER-ADDR
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
* --------------------------------------------------------------------* 00000386
*        WORKFIELDS                                                   * 00000387
* --------------------------------------------------------------------* 00000388
         DS    0D                                                       00000389
         CNSLCCB
*                                                                       00000391
* --------------------------------------------------------------------* 00000404
*        MESSAGE TABLES                                               * 00000405
* --------------------------------------------------------------------* 00000406
         DS    0F                       ALIGNEMENT                      00000407
* --------------------------------------------------------------------* 00000369
*        SAVE AREA DECLARATIONS                                       * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F                                                       00000372
SAVEAREA DC    XL72'00'                 MONITOR TASK SAVE AREA           0000037
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS          00000379
PARMADDR DS    F                                                        00000458
PARMLIST DS    F                                                        00000458
WORKAREA DC    148CL1' '
BLVTOC   DC    CL8'VSRVTOC'                                             00000458
SEARCH   DC    XL5'03'
*                                                                       00000459
* --------------------------------------------------------------------* 00000424
         LTORG                                                          00000454
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                   * 00000456
* --------------------------------------------------------------------* 00000457
WORKA    DSECT                                                          00000475
         COPY  LIBPARMS                                                 00000460
         DS    0D                                                       00000427
* --------------------------------------------------------------------* 00000474
TABRECS  DSECT                                                          00000475
TABREC   DS    CL80                                                     00000476
* --------------------------------------------------------------------* 00000477
MAPOUT   DSECT
         DS    0F
MAPADDR  DS    0CL2028
MAPNAME  DS    CL8
MAPEYEC  DS    CL8
         DS    CL2
MAPLEN   DS    CL2                MAP-LEN
MAPVCNT  DS    AL1(0)             VAR-FIELD COUNT
MAPVLST  DS    AL3(0)             ADDR MAP-VAR-OUTPUT-LIST
MAPSTART DS    CL2000
BUFFER   EQU   *                   MEMBER BUFFER BEGIN                  00001705
         END                                                            00000481
        BKEND
        CATALS   A.VSWAIT,0.0
        BKEND   A.VSWAIT
*------------------------------------------------------------------
*     NAME: VSWAIT
*     TYPE: SUB-ROUTINE
* FUNCTION: Wait for Input after displaying a message
*
* INTERNAL
*
*
* EXTERNAL
*
*   AUTHOR:  REINHARD_HANSEN@T-ONLINE.DE
*
*   NOTES:
*
*
*
*
*------------------------------------------------------------------
* --------------------------------------------------------------------* 00000015
*        REGISTER EQUATES                                             * 00000351
* --------------------------------------------------------------------* 00000352
R0       EQU   0                                                        00000353
R1       EQU   1                                                        00000354
R2       EQU   2                                                        00000355
R3       EQU   3                                                        00000356
R4       EQU   4                                                        00000357
R5       EQU   5                                                        00000358
R6       EQU   6                                                        00000359
R7       EQU   7                                                        00000360
R8       EQU   8                                                        00000361
R9       EQU   9                                                        00000362
R10      EQU   10                                                       00000363
R11      EQU   11                                                       00000364
R12      EQU   12                                                       00000365
R13      EQU   13                                                       00000366
R14      EQU   14                                                       00000367
R15      EQU   15                                                       00000368
VSWAIT    CSECT                                                          0000001
         STM   R14,R12,12(R13)         STORE REGS IN SAVEAREA           00000018
         USING VSWAIT,R11,R12          R11 NOW BASE REGISTER            00000020
         LR    R11,R15                 ESTABLISH ADDRESSABILITY         00000021
         B     GOON
         DC    C'***** VSWAIT *****'   EYE-CATCHER
GOON     DS    0H
         LA    R12,4095(R11)                                            00000022
         LA    R12,1(R12)              SECOND BASE                      00000023
         ST    R13,SAVEAREA+4          STORE MAIN PGM SAVEAREA          00000024
         LR    R10,R13                 SAVE R13                         00000025
         LA    R13,SAVEAREA            SAVE AREA FOR THIS TASK          00000026
         ST    R13,8(R10)              STORE EXIT SAVE ADDRESS          00000027
* --------------------------------------------------------------------* 00000028
*        STATEMENTS FOR SET UP FIELDS NEEDED FOR LIBRM                * 00000029
* --------------------------------------------------------------------* 00000030
         L     R2,0(R1)                PARMS ADDRESS                    00000031
         ST    R2,PARMADDR             SAVE                             00000032
         USING WORKA,R2
         L     R9,PBUFADR              LOAD DATATAB ADDRESS             00000034
         ST    R9,TABRECSA             SET  TABRECS ADDRESS             00000035
         USING TABRECS,R9              ESTABLISH ADDRESSABILITY         00000036
* --------------------------------------------------------------------* 00000065
*     PGM-START                                                       * 00000066
* --------------------------------------------------------------------* 00000067
******   CNSLOUT MSG='PGM-START'
         LA    R1,READMCCW
         BAL   R8,EXCPRD00          READ MODIFIED
         B     PGMEND
*----------------------------------------------------------------
*     EXCP - EXECUTE CHANNEL PROGRAM READ MOFIFIED
*----------------------------------------------------------------
EXCPRD00 DS    0H                Loop
         STCM  R1,7,EXCPCCB+9    STORE CCW-ADDR in CCB.
         LA    R1,EXCPCCB        Issue COMMAND
EXCPRD01 DS    0H                Loop
         EXCP  (1)
*
         STM     1,2,SAV1
         SETIME 1,EXCPTECB  INTERVAL SET 1 SECDS
         WAIT     EXCPTECB  WAIT
         LM      1,2,SAV1
*
         TM    2(1),X'80'    ECP POSTED ?
         BO    EXCPRD02      Yes, CHECK AID
         SVC   7             NO SVC 7
EXCPRD02 DS    0H
         CLI   IOAID,X'60'   NO DATA INPUT YET ?
         BE    EXCPRD01      YES, LOOP FOR READ
         OC    IOCMD,BLANKS  CAPS ON
*
         BR    R8            OK, FINISHED
EXCPTECB TECB                CREATE TIMER EVENT CONTROL BLOCK
* --------------------------------------------------------------------* 00000065
*     PGMEND Program-ENd                                              * 00000066
* --------------------------------------------------------------------* 00000067
PGMEND   DS    0H
******   CNSLOUT MSG='PGM-END'
         L     R9,PARMADDR         LOAD PARAMETER-ADDR
         L     R13,4(R13)
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14                 RETURN TO CALLER-PGM
* --------------------------------------------------------------------* 00000386
*        WORKFIELDS                                                   * 00000387
* --------------------------------------------------------------------* 00000388
         DS    0D                                                       00000389
EXCPCCB  CCB   SYS007,DUMMY,X'0000'
*                                                                       00000391
* --------------------------------------------------------------------* 00000404
*        MESSAGE TABLES                                               * 00000405
* --------------------------------------------------------------------* 00000406
         DS    0D                       ALIGNEMENT                      00000407
READMCCW CCW   X'06',IOAREA,X'20',L'IOAREA       READ MODIFIED.
* --------------------------------------------------------------------* 00000369
*        SAVE AREA DECLARATIONS                                       * 00000370
* --------------------------------------------------------------------* 00000371
         DS    0F                                                       00000372
SAVEAREA DC    XL72'00'                 MONITOR TASK SAVE AREA           0000037
TABRECSA DC    F'0'                     GETVIS TABRECS ADDRESS          00000379
PARMADDR DS    F                                                        00000458
SAV1     DS    2F
SAV8     DS    F
BLANKS   DC    80CL1' '
IOAREA   DS    0CL1840                                                   0000166
IOAID    DS    CL1                 AID                                  00001658
IOCURSBA DS    CL2                 CURSOR SBA                           00001659
IOCMD11  DS    CL1                 CONTROL SBA                          00001660
IOCMDSBA DS    CL2                 SBA                                  00001660
IOCMD    DS    CL40    8 RH        TOP-MAP-COMMAND OPTION===>           00001661
IOSBA11  DS    CL1                 1 IOSBA-CMD = X'11' or X'51'         00001662
IOSBA    DS    CL2                 SET BUFFER ADDRESS SBA               00001662
IODATA   EQU   *                   INPUT ADDR FOR ALL MAPS
         DS    CL240
****     CNSLCCB
*                                                                       00000459
*---------------------------------------------------------------------
*    DUMMY
*---------------------------------------------------------------------
DUMMY   EQU   *
  DC    XL1'C3'   WCC Reset MDT
       $SBA  (22,6,N)
  DC    C'*** DUMMY ***'
DUMMYL   EQU   *-DUMMY
* --------------------------------------------------------------------* 00000424
         LTORG                                                          00000454
* --------------------------------------------------------------------* 00000455
*        PARAMETERS                                                   * 00000456
* --------------------------------------------------------------------* 00000457
WORKA    DSECT                                                          00000475
         COPY  LIBPARMS                                                 00000460
         DS    0D                                                       00000427
* --------------------------------------------------------------------* 00000474
TABRECS  DSECT                                                          00000475
TABREC   DS    CL80                                                     00000476
* --------------------------------------------------------------------* 00000477
MAPOUT   DSECT
         DS    0F
MAPADDR  DS    0CL2028
MAPNAME  DS    CL8
MAPEYEC  DS    CL8
         DS    CL2
MAPLEN   DS    CL2                MAP-LEN
MAPVCNT  DS    AL1(0)             VAR-FIELD COUNT
MAPVLST  DS    AL3(0)             ADDR MAP-VAR-OUTPUT-LIST
MAPSTART DS    CL2000
BUFFER   EQU   *                   MEMBER BUFFER BEGIN                  00001705
         END                                                            00000481
        BKEND
        CATALS   A.$SBA,0.0
        BKEND   A.$SBA
         MACRO                                                          00000007
&NAME    $SBA                                                           00000008
         GBLB  &SET                                                     00000009
         GBLC  &SBA(64)                                                 00000010
         LCLA  &INDEX1,&INDEX2                                          00000011
         LCLA  &RC                                                      00000012
         LCLC  &R,&C,&ATRIB                                             00000013
         LCLC  &SBA1,&SBA2                                              00000014
         AIF   (&SET).GEN                                               00000015
&SBA(01) SETC  '40'                                                     00000016
&SBA(02) SETC  'C1'                                                     00000017
&SBA(03) SETC  'C2'                                                     00000018
&SBA(04) SETC  'C3'                                                     00000019
&SBA(05) SETC  'C4'                                                     00000020
&SBA(06) SETC  'C5'                                                     00000021
&SBA(07) SETC  'C6'                                                     00000022
&SBA(08) SETC  'C7'                                                     00000023
&SBA(09) SETC  'C8'                                                     00000024
&SBA(10) SETC  'C9'                                                     00000025
&SBA(11) SETC  '4A'                                                     00000026
&SBA(12) SETC  '4B'                                                     00000027
&SBA(13) SETC  '4C'                                                     00000028
&SBA(14) SETC  '4D'                                                     00000029
&SBA(15) SETC  '4E'                                                     00000030
&SBA(16) SETC  '4F'                                                     00000031
&SBA(17) SETC  '50'                                                     00000032
&SBA(18) SETC  'D1'                                                     00000033
&SBA(19) SETC  'D2'                                                     00000034
&SBA(20) SETC  'D3'                                                     00000035
&SBA(21) SETC  'D4'                                                     00000036
&SBA(22) SETC  'D5'                                                     00000037
&SBA(23) SETC  'D6'                                                     00000038
&SBA(24) SETC  'D7'                                                     00000039
&SBA(25) SETC  'D8'                                                     00000040
&SBA(26) SETC  'D9'                                                     00000041
&SBA(27) SETC  '5A'                                                     00000042
&SBA(28) SETC  '5B'                                                     00000043
&SBA(29) SETC  '5C'                                                     00000044
&SBA(30) SETC  '5D'                                                     00000045
&SBA(31) SETC  '5E'                                                     00000046
&SBA(32) SETC  '5F'                                                     00000047
&SBA(33) SETC  '60'                                                     00000048
&SBA(34) SETC  '61'                                                     00000049
&SBA(35) SETC  'E2'                                                     00000050
&SBA(36) SETC  'E3'                                                     00000051
&SBA(37) SETC  'E4'                                                     00000052
&SBA(38) SETC  'E5'                                                     00000053
&SBA(39) SETC  'E6'                                                     00000054
&SBA(40) SETC  'E7'                                                     00000055
&SBA(41) SETC  'E8'                                                     00000056
&SBA(42) SETC  'E9'                                                     00000057
&SBA(43) SETC  '6A'                                                     00000058
&SBA(44) SETC  '6B'                                                     00000059
&SBA(45) SETC  '6C'                                                     00000060
&SBA(46) SETC  '6D'                                                     00000061
&SBA(47) SETC  '6E'                                                     00000062
&SBA(48) SETC  '6F'                                                     00000063
&SBA(49) SETC  'F0'                                                     00000064
&SBA(50) SETC  'F1'                                                     00000065
&SBA(51) SETC  'F2'                                                     00000066
&SBA(52) SETC  'F3'                                                     00000067
&SBA(53) SETC  'F4'                                                     00000068
&SBA(54) SETC  'F5'                                                     00000069
&SBA(55) SETC  'F6'                                                     00000070
&SBA(56) SETC  'F7'                                                     00000071
&SBA(57) SETC  'F8'                                                     00000072
&SBA(58) SETC  'F9'                                                     00000073
&SBA(59) SETC  '7A'                                                     00000074
&SBA(60) SETC  '7B'                                                     00000075
&SBA(61) SETC  '7C'                                                     00000076
&SBA(62) SETC  '7D'                                                     00000077
&SBA(63) SETC  '7E'                                                     00000078
&SBA(64) SETC  '7F'                                                     00000079
&SET     SETB  (1)                                                      00000080
.GEN     ANOP  ,                                                        00000081
&R       SETC  '&SYSLIST(1,1)'                                          00000082
&C       SETC  '&SYSLIST(1,2)'                                          00000083
&ATRIB   SETC  '&SYSLIST(1,3)'                                          00000084
&RC      SETA  ((&R-1)*80)+(&C-1)                                       00000085
&INDEX1  SETA  (&RC/64+1)                                               00000086
&SBA1    SETC  '&SBA(&INDEX1)'                                          00000087
&INDEX2  SETA  (&RC-((&INDEX1-1)*64)+1)                                 00000088
&SBA2    SETC  '&SBA(&INDEX2)'                                          00000089
         AIF   ('&ATRIB' EQ 'B').BRIGHT                                 00000090
         AIF   ('&ATRIB' EQ 'N').NORMAL                                 00000091
         AIF   ('&ATRIB' EQ 'U').UNPROT                                 00000092
         AIF   ('&ATRIB' EQ 'UB').UNPBRI                                00000093
         DC    X'11&SBA1&SBA2'         SIN ATRIBUTO                     00000094
         MEXIT ,                                                        00000095
.NORMAL  ANOP                                                           00000096
         DC    X'11&SBA1&SBA2',X'1DF0' START FIELD,PROT SKIP NORMAL     00000097
         MEXIT ,                                                        00000098
.BRIGHT  ANOP                                                           00000099
         DC    X'11&SBA1&SBA2',X'1DF8' START FIELD,PROT SKIP BRIGHT     00000100
         MEXIT ,                                                        00000101
.UNPROT  ANOP                                                           00000102
         DC    X'11&SBA1&SBA2',X'1D40' START FIELD,UNPROT NORMAL        00000103
         MEXIT ,                                                        00000104
.UNPBRI  ANOP                                                           00000105
         DC    X'11&SBA1&SBA2',X'1DC8' START FIELD,UNPROT BRIGHT        00000106
         MEXIT ,                                                        00000107
         MEND                                                           00000108
        BKEND
        CATALS   A.CNSLCCB,0.0
        BKEND   A.CNSLCCB
         MACRO                                                          00000000
         CNSLCCB
         GBLC  &MSGID
         DS    0D
CNSLCCB  CCB   SYSLOG,CNSLCCW
CNSLCCW  CCW   X'09',CNSLMSG,X'20',L'CNSLMSG
CNSLMSG  DS    0CL60
CNSLMID  DC    CL9'&SYSECT:'                 CSECT
         DC    CL1' '
CNSLTXT  DC    CL50' '
CNSLOV1  DS    F
CNSLOV2  DS    F
CNSLOV3  DS    F
CNSLCV1  DS    F
CNSLOR   DS    F
CNSLWRK  DS    CL72
CNSLTRT  EQU   *-240
         DC    CL16'0123456789ABCDEF'
CNSLOUTW DS    0H
         ST    1,CNSLCV1
         LA    1,CNSLMSG           MESSAGE TEXT ADDRESS                 00001486
         STCM  1,7,CNSLCCW+1       TO ERROR CCW                         00001487
         LA    1,CNSLCCB
         EXCP  (1)
         WAIT  (1)
         L     1,CNSLCV1
         BR    1
         MEND                                                           92000025
        BKEND
        CATALS   A.CNSLOUT,0.0
        BKEND   A.CNSLOUT
         MACRO                                                          00000000
&NAME    CNSLOUT &REG=,&MSG=,&ADR=,&LEN=
         LCLC   &NDX
         LCLA   &MSGL,&REGL,&LENA
&NDX     SETC  '&SYSNDX'
&NAME    DS    0H
         MVI   CNSLTXT,X'40'
         MVC   CNSLTXT+1(49),CNSLTXT
         ST    1,CNSLOV1
         ST    2,CNSLOV2
         ST    3,CNSLOV3
*--------------------------------------------------------------
         AIF   (T'&REG EQ 'O').CHKMSG     OMITTED ?                     14000025
*--------------------------------------------------------------
         ST    &REG,CNSLOR                SAVE REGISTER                   160000
&REGL    SETA  K'&REG+9
         UNPK  CNSLTXT(9),CNSLOR(5)
         TR    CNSLTXT(8),CNSLTRT
         MVI   CNSLTXT+8,X'40'
         MVC   CNSLTXT+9(&REGL),=CL&REGL'REGISTER &REG'
         BAL   1,CNSLOUTW
         AGO   .CNSLEND                                                 92000025
*--------------------------------------------------------------
.CHKMSG  AIF   (T'&MSG EQ 'O').CHKADR     OMITTED ?                       140000
*--------------------------------------------------------------
&MSGL    SETA  K'&MSG-2                   2 * ''
         AIF   (&MSGL LE 50).MSGLOK
         MNOTE 4,'MSG TO LARGE, MSG TRUNCATED 50'
&MSGL    SETA  50
.MSGLOK  ANOP
         MVC   CNSLTXT(&MSGL),=CL&MSGL&MSG
         BAL   1,CNSLOUTW
         AGO   .CNSLEND                                                 92000025
*--------------------------------------------------------------
.CHKADR  ANOP
*--------------------------------------------------------------
         AIF   (T'&ADR NE 'O').CHKAOK     OMITTED ?                       140000
         MNOTE 8,'PARAMETER ''ADR'' MISSING - ABNORMAL-END'
         AGO   .CNSLEND
.CHKAOK  ANOP
         AIF   (T'&LEN NE 'O').CHKLEN     OMITTED ?                       140000
         MNOTE 3,'PARAMETER ''LEN'' MISSING - MAX = 35 ASSUMED'
&LENA    SETA  35
         AGO   .CHKLOK
.CHKLEN  ANOP
         AIF   (&LEN LE 35).CHKLZER                                     14000025
         MNOTE 4,'PARAMETER ''LEN'' TO LARGE - MAX = 35 ASSUMED'
&LENA    SETA  35
         AGO   .CHKLOK
.CHKLZER ANOP
         AIF   (&LEN GE 1).CHKLSET                                         14000
         MNOTE 4,'PARAMETER ''LEN'' TO SMALL - MIN = 1 ASSUMED'
&LENA    SETA  1
         AGO   .CHKLOK
.CHKLSET ANOP
&LENA    SETA  &LEN
         AGO   .CHKLOK
.CHKLOK  ANOP
         MVC   CNSLTXT(8),=CL8'&ADR'      PARAMETER IN CHAR
         MVI   CNSLTXT+8,C':'
         MVC   CNSLTXT+9(&LENA),&ADR
         BAL   1,CNSLOUTW
         B     CNHX&NDX
WF&NDX   DC    F'&LENA'                   VAR LEN
*--------------------------------------------------------------
CNHX&NDX DS    0H
         UNPK  CNSLWRK(15),&ADR.(8)
         UNPK  CNSLWRK+14(15),&ADR+7(8)
         UNPK  CNSLWRK+28(15),&ADR+14(8)
         UNPK  CNSLWRK+42(15),&ADR+21(8)
         UNPK  CNSLWRK+56(15),&ADR+28(8)    MAX 35 BYTES
         TR    CNSLWRK(70),CNSLTRT
*
         LA    1,CNSLWRK
         LA    2,CNSLTXT+9
         L     3,WF&NDX
CN1&NDX  DS    0H
         MVC   0(1,2),0(1)
         LA    1,2(1)
         LA    2,1(2)
         BCT   3,CN1&NDX
         BAL   1,CNSLOUTW
*
         LA    1,CNSLWRK+1
         LA    2,CNSLTXT+9
         L     3,WF&NDX
CN2&NDX  DS    0H
         MVC   0(1,2),0(1)
         LA    1,2(1)
         LA    2,1(2)
         BCT   3,CN2&NDX
         BAL   1,CNSLOUTW
*
.CNSLEND L     1,CNSLOV1
         L     2,CNSLOV2
         L     3,CNSLOV3
         MEND                                                           92000025
*
        BKEND
        CATALS   A.LIBPARMS,0.0
        BKEND   A.LIBPARMS
*---------------------------------------------------------------------
*        VSSPF  PARMS
*---------------------------------------------------------------------
PARMS    DS    0F
PFUNC    DS    CL3                 FUNCTION GET/PUT
PSYSIPT  DS    C                   DATA=Y/N
PBUFADR  DS    F                   BUFFER ADDRESS
PNORL    DS    F                   NO RECORDS
PENDBUF  DS    F                   END BUFFER ADDRESS
PLIB     DS    CL7                 LIBRARY NAME
PSLIB    DS    CL1                 SUBLIBRARY NAME
PMEMB    DS    CL8                 MEMBER NAME
PWORK    DS    CL8                 WORKFIELD
PRETCOD  DS    CL3                 R15
PMSG     DS    CL52
         ORG   PMSG
PSBAWORK DS    CL2                 3270 SBA ADR1 / ADR2
PROW     DS    CL1                 3270 ROW
PCOL     DS    CL1                 3270 COL
PSTACK   DS    F                   EDITOR STACK ADDR
PNXTSTCK DS    F                   ADDR OF NEXT STACK ENTRY
PWORK1   DS    F                   WORKSET (1) ADDR
PWORK2   DS    F                   WORKSET (2) ADDR
PHELP    DS    F                   WORKSET (HELP) ADDR
PLIBNAME DS    CL2                 ACTUAL LIBRARY NAME
PCCHHBI  DS    XL6                 BIN  FOR VSUTI TO CALC TO CHAR
PCCHHBIF DS    XL1                 FORMAT-LEN FOR PCCHHBI (3-6)
PCCHHAR  DS    CL12                CHAR FOR VSUTI TO CALC TO BIN
PCCHHARF DS    CL1                 FORMAT-LEN FOR PCCHHAR (9-12)
PSORTNUM DS    H                   NUMBER OF SORT RECORDS
PSORTLEN DS    CL1                 LENGTH OF SORTFIELD
PPFUNC   DS    CL3                 PREVIOUS PFUNC
         ORG
PWRQUEUE DS    CL3                 PWR QUEUE
PWRJOB   DS    CL14                PWR JOBNAME,JOBNUM
PEDBRSW  DS    CL1                 EDIT E/B BROWSE SWITCH
*---------------------------------------------------------------------
PMAPADDR DS    F                    ADDR OF OUTPUT-MAP
PINAREAD DS    F                    ADDR OF INPUT-AREA
PIOAREAD DS    F                    ADDR OF SECOND IO-AREA
*---------------------------------------------------------------------
SAVENDBA DS    F                   SAVE LAST RECORD ADDRESS
SAVCURR  DS    F                   SAVE CURRENT REC ADDRESS
SAVNEXTB DS    F                   SAVE NEXT REC ADDRESS
VIEW     DS    H                   VIEW MEMBER DISPLAY AT COLUMN NN
RECLEN   DS    H                   RECORD LENGTH IN PROCESS 80/132
MEMBER   DS    CL10                MEMBER NAME FOR SCALE
PSTATAB  DS    0CL49
PSCLB    DC    XL6'00'             SYSTEM-CLB START-ADDR BCCHH
PSCLBA   DC    X'40'               SYSTEM-CLB ARCHITECTURE CKD / FBA
PSRLB    DC    XL6'00'             SYSTEM-RLB START-ADDR CCHH
PSRLBA   DC    X'40'               SYSTEM-RLB ARCHITECTURE CKD / FBA
PSSLB    DC    XL6'00'             SYSTEM-SLB START-ADDR CCHH
PSSLBA   DC    X'40'               SYSTEM-SLB ARCHITECTURE CKD / FBA
PSPLB    DC    XL6'00'             SYSTEM-PLB START-ADDR CCHH
PSPLBA   DC    X'40'               SYSTEM-PLB ARCHITECTURE CKD / FBA
PPCLB    DC    XL6'00'             PRIVATE-CLB START-ADDR BCCHH
PPCLBA   DC    X'40'               PRIVATE-CLB ARCHITECTURE CKD / FBA
PPRLB    DC    XL6'00'             PRIVATE-RLB START-ADDR CCHH
PPRLBA   DC    X'40'               PRIVATE-RLB ARCHITECTURE CKD / FBA
PPSLB    DC    XL6'00'             PRIVATE-SLB START-ADDR CCHH
PPSLBA   DC    X'40'               PRIVATE-SLB ARCHITECTURE CKD / FBA
*
PDIRAREA DS    0CL80               DIRECTORY INFO AREA (RLB,SLB,PLB)
PDIRSA   DS    XL7                 START ADDR OF DIRECTORY BBCCHHR
PDIRNA   DS    XL8                 NEXT  ADDR OF DIRECTORY BBHHCCRE
PDIRLA   DS    XL8                 LAST  ADRR OF DIRECTORY BBCCHHRE
PLIBSA   DS    XL7                 START ADDR OF LIBRARY   BBCCHHR
PLIBNA   DS    XL7                 NEXT  ADDR OF LIBRARY   BBCCHHR
PLIBLA   DS    XL7                 LAST  ADRR OF LIBRARY   BBCCHHR
PDIRAE   DS    XL4                 ACTIVE ENTRIES DIRECTORY
PLIBAL   DS    XL4                 ALLOCATED BLOCKS  LIBRARY
PLIBAC   DS    XL4                 ACTIVE    BLOCKS  LIBRARY
PLIBDL   DS    XL4                 DELETED   BLOCKS  LIBRARY
PLIBAV   DS    XL4                 AVAILABLE BLOCKS  LIBRARY
PLIBACL  DS    XL2                 AUTOM CONDENSE LIMIT IN BLOCKS
PDLINCY  DS    XL2                 NUMBER CYLS FOR LIB AND DIR
PDIRTRKS DS    XL2                 NUMBER TRACKS FOR DIR
PDLIRES  DS    XL10                RESERVED
*
PSCRL    DC    H'0'                NUMBER OF SCROLL LINES
PFARG    DS    CL8                 FIND-ARGUMENT          FOR BR/ED
PFLEN    DC    H'0'                FIND-ARGUMENT-LEN
PCARG    DS    CL8                 CHANGE ARGUMENT        FOR BR/ED
PCLEN    DC    H'0'                CHANGE ARGUMENT-LEN
PCSCOPE  DC    H'0'                CHANGE ARGUMENT-SCOPE (#LINES)
PARMSL   EQU   *-PARMS             PARMS LENGTH
        BKEND
        CATALS   A.SPFM000,0.0
        BKEND   A.SPFM000
*---------------------------------------------------------------------
*    DOS/VS Welcome Screen Zeile 1 - 20  NO INPUT-FIELDS
*---------------------------------------------------------------------
SPFM000  CSECT
         DC C'SPFM000 ********'                       EYECATCHER
         DC A(MAPEND-MAPSTART) MAP-LEN
         DC AL1(0)             VAR-FIELD COUNT
         DC AL3(0)             ADDR MAP-VAR-OUTPUT-LIST
*---------------------------------------------------------------------
MAPSTART DS  0H
  DC XL1'C7'                   WCC Beep and Reset MDT
       $SBA (01,14)            ROW,COLUMN
  DC X'1DF02842F2'             SF PROT  SA EXTENDED ATTR RED
  DC C'      Welcome and Hello again to the Release 34 of the fabulous'
       $SBA  (03,12,B)
  DC C'       @@@@@@@@@@@       @@@@@@@@@@@      @@@@@@@@@@@@@    @@@@'
       $SBA  (04,12,B)
  DC C'      @@@@@@@@@@@@@    @@@@@@@@@@@@@@   @@@@@@@@@@@@@@    @@@@ '
       $SBA  (05,12,B)
  DC C'     @@@@      @@@@   @@@@       @@@@  @@@@              @@@@  '
       $SBA  (06,12,B)
  DC C'    @@@@       @@@@  @@@@       @@@@   @@@@@@@@@@@@     @@@@   '
       $SBA  (07,12,B)
  DC C'   @@@@       @@@@  @@@@       @@@@    @@@@@@@@@@@@@   @@@@    '
       $SBA  (08,12,B)
  DC C'  @@@@       @@@@  @@@@       @@@@             @@@@@  @@@@     '
       $SBA  (09,12,B)
  DC C' @@@@@@@@@@@@@@@   @@@@@@@@@@@@@@   @@@@@@@@@@@@@@@  @@@@      '
       $SBA  (10,12,B)
  DC C'@@@@@@@@@@@@@@      @@@@@@@@@@@    @@@@@@@@@@@@@@   @@@@       '
       $SBA  (13,12,B)
  DC C'             @@@@        @@@@   @@@@@@@@@@@@@                  '
       $SBA  (14,12,B)
  DC C'             @@@@       @@@@  @@@@@@@@@@@@@@                   '
       $SBA  (15,12,B)
  DC C'             @@@@      @@@@  @@@@                              '
       $SBA  (16,12,B)
  DC C'             @@@@     @@@@   @@@@@@@@@@@@                      '
       $SBA  (17,12,B)
  DC C'             @@@@    @@@@     @@@@@@@@@@@@@                    '
       $SBA  (18,12,B)
  DC C'             @@@@   @@@@                @@@@                   '
       $SBA  (19,12,B)
  DC C'             @@@@@@@@@@     @@@@@@@@@@@@@@@@                   '
       $SBA  (20,12,B)
  DC C'              @@@@@@@      @@@@@@@@@@@@@@@                     '
*---------------------------------------------------------------------
MAPEND EQU  *
     END
        BKEND
        CATALS   A.SPFM001,0.0
        BKEND   A.SPFM001
*---------------------------------------------------------------------
*    PF-Key Information    Row   23-24   NO INPUT-FIELDS
*---------------------------------------------------------------------
SPFM001  CSECT
         DC C'SPFM001 ********'                       EYECATCHER
         DC A(MAPEND-MAPSTART) MAP-LEN
         DC AL1(0)             VAR-FIELD COUNT
         DC AL3(0)             ADDR MAP-VAR-OUTPUT-LIST
*---------------------------------------------------------------------
MAPSTART DC    XL1'42'   WCC KBD RESET
         $SBA  (23,1,N)
         DC    C'F1=HELP     F2=Retriev     F3=Exit     F4=SWITCH   '
         DC    C' F5= BACKW    F6=REPEAT'
*---
         $SBA  (24,1,N)
         DC    C'F7=BACK     F8=FORWD       F9=1.Page  F10=VIEW-1   '
         DC    C'F11=VIEW-2   F12=LAST LINE'
*---
*---------------------------------------------------------------------
MAPEND   EQU  *
         END
        BKEND
        CATALS   A.SPFM002,0.0
        BKEND   A.SPFM002
*---------------------------------------------------------------------
*    Logon Information       Row   22-24   01 VAR-OUTPUT-FIELD
*    1.Error-MSG(50)
*---------------------------------------------------------------------
SPFM002  CSECT
         DC C'SPFM002 ********'                       EYECATCHER
         DC A(MAPEND-MAPSTART) MAP-LEN
         DC AL1(1)             VAR-FIELD COUNT
         DC AL3(MAPVLST)       ADDR MAP-VAR-OUTPUT-LIST
*---------------------------------------------------------------------
MAPSTART DC    XL1'42'   WCC KBD RESET
         $SBA  (22,6,N)
         DC    C'USER-ID........'
         $SBA  (22,22)
         DC    X'1DC513'           SF = UNPROT, IC
         DC    C'        '
         $SBA  (22,31,N)
         DC    C'   The name by which the system knows you.'
*---
         $SBA  (23,6,N)
         DC    C'PASSWORD.......'
         $SBA  (23,22)
         DC    X'1DCD'             UNPROT NO DISPLAY ?
         DC    C'        '
         $SBA  (23,31,N)
         DC    C'   Your personal access code.'
*---
         $SBA  (24,6,B)
MAPIN01  DC    CL50' '
MAPEND   EQU  *
MAPVLST  DS    0F
         DC    A(MAPIN01)
         END
        BKEND
        CATALS   A.SPFM003,0.0
        BKEND   A.SPFM003
*---------------------------------------------------------------------
* USER/DATE/TIME-Infos       R03-06                  3 VAR-OUT-FIELDS
*---------------------------------------------------------------------
SPFM003  CSECT
         DC C'SPFM003 ********'                       EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(3)             VAR-FIELD COUNT
         DC    AL3(MAPVLST)       ADDR MAP-VAR-OUTPUT-LIST
*---------------------------------------------------------------------
MAPSTART EQU   *
         DC XL1'42'                   WCC KEYBD RESET
*-------
         $SBA  (3,62,N)
         DC    C'User - '
         $SBA  (3,69,B)
SUSER    DC    CL8' '
*-------
         $SBA  (4,62,N)
         DC    C'Date - '
         $SBA  (4,69,B)
SDATE    DC    CL8' '
*-------
         $SBA  (5,62,N)
         DC     C'Time - '
         $SBA  (5,69,B)
STIME    DC    CL5' '
*---------------------------------------------------------------------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(SUSER)
         DC   A(SDATE)
         DC   A(STIME)
         END
        BKEND
        CATALS   A.SPFM004,0.0
        BKEND   A.SPFM004
*---------------------------------------------------------------------
* Top Map 2 Rows with Option      R01-02            1 VAR-OUT-Fields
*           1.MAPNAME(8)
*---------------------------------------------------------------------
SPFM004  CSECT
         DC C'SPFM004 ********'                       EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(1)             VAR-FIELD COUNT
         DC    AL3(MAPVLST)      ADDR MAP-VAR-OUTPUT-LIST
*---------------------------------------------------------------------
MAPSTART EQU   *
         DC XL1'42'                   WCC KBD RESET
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7-
         $SBA  (1,1,N)
         DC    C'VSSPF --------------------'   26
         DC    X'1DE8'
MAPNAME  DC    C'          '                   10
         DC    X'1DF0'
         DC    C'----------------------------------------'   40
         $SBA  (2,1,B)
         DC    C'OPTION ===>'
         $SBA  (2,13)
         DC    X'1D'                SF  : START FIELD
         DC    X'C1'                ATTR: UNPROT MDT  ON
         DC    XL1'13'                    INSERT CURSOR
         DC    CL40' '        8 RH
         $SBA  (2,76,N)      24 RH
         DC    CL2' '
*-------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(MAPNAME)
         END
        BKEND
        CATALS   A.SPFM005,0.0
        BKEND   A.SPFM005
*---------------------------------------------------------------------
*  SPFVS-Primary Map              R04-22              0 VAR-OUT-FIELDS
*---------------------------------------------------------------------
SPFM005  CSECT
         DC C'SPFM005 ********'                       EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(0)             VAR-FIELD COUNT
         DC    AL3(MAPVLST)       ADDR MAP-VAR-OUTPUT-LIST
MAPSTART EQU   *
         DC XL1'42'                   WCC KBD RESET
*---------------------------------------------------------------------
         $SBA  (4,3,B)
         DC    C'0'
         $SBA  (4,6,N)
         DC    C'Parameter   - Specify your own Parameter'
*-------
         $SBA  (5,3,B)
         DC    C'1'
         $SBA  (5,6,N)
         DC    C'Browse      - Browse anything you want'
*-------
         $SBA  (6,3,B)
         DC    C'2'
         $SBA  (6,6,N)
         DC    C'Edit        - Edit source members'
*-------
         $SBA  (7,3,B)
         DC    C'3'
         $SBA  (7,6,N)
         DC    C'Utilities   - Perform Utilities Functions'
*-------
         $SBA  (8,3,B)
         DC    C'4'
         $SBA  (8,6,N)
         DC    C'Foreground  - Invoke User applications in foreground'
*-------
         $SBA  (9,3,B)
         DC    C'5'
         $SBA  (9,6,N)
         DC    C'Environment - Information about configuration'
*-------
         $SBA  (10,3,B)
         DC    C'6'
         $SBA  (10,6,N)
         DC    C'Command     - Execute System Commands'
*-------
         $SBA  (11,3,B)
         DC    C'C'
         $SBA  (11,6,N)
         DC    C'Changes     - Changes from last Version'
*-------
         $SBA  (12,3,B)
         DC    C'T'
         $SBA  (12,6,N)
         DC    C'Tutorial    - This Program Information'
*-------
         $SBA  (13,3,B)
         DC    C'S'
         $SBA  (13,6,N)
         DC    C'Samples     - Sample Programs'
*-------
         $SBA  (14,3,B)
         DC    C'X'
         $SBA  (14,6,N)
         DC    C'Exit        - Terminate this Program'
*-------
         $SBA  (15,3,B)
         DC    C' '
         $SBA  (15,6,N)
         DC    C'     '
*-------
         $SBA  (16,3,B)
         DC    C' '
         $SBA  (16,6,N)
         DC    C'     '
*-------
         $SBA  (17,3,B)
         DC    C' '
         $SBA  (17,6,N)
         DC    C'     '
*-------
         $SBA  (18,3,B)
         DC    C' '
         $SBA  (18,6,N)
         DC    C'     '
*-------
         $SBA  (19,3,B)
         DC    C' '
         $SBA  (19,6,N)
         DC    C'     '
*-------
         $SBA  (20,3,B)
         DC    C' '
         $SBA  (20,6,N)
         DC    C'     '
*-------
         $SBA  (21,3,B)
         DC    C' '
         $SBA  (21,6,N)
         DC    C'     '
*-------
         $SBA  (22,1,B)
         DC    C'----+----1----+----2----+----3----+----4----+----5'
         DC    C'----+----6----+----7----+---'
*-------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(0)
         END
        BKEND
        CATALS   A.SPFM006,0.0
        BKEND   A.SPFM006
*---------------------------------------------------------------------
*  Browse/Edit-DSList Map         R04-22              0 VAR-OUT-FIELDS
*---------------------------------------------------------------------
SPFM006  CSECT
         DC C'SPFM006 ********'                       EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(0)             VAR-FIELD COUNT
         DC    AL3(MAPVLST)       ADDR MAP-VAR-OUTPUT-LIST
MAPSTART EQU   *
         DC XL1'42'                   WCC KBD RESET
*---------------------------------------------------------------------
         $SBA  (4,3,B)
         DC    C' '
         $SBA  (4,6,N)
         DC    C' '
*-------
         $SBA  (7,3,N)
         DC    C'Sub-Library: '
         $SBA  (7,16,U)
         DC    XL1'13'       INSERT CURSOR
         DC    CL1' '
         $SBA  (7,18,N)
         $SBA  (7,45,N)
         DC    C'(A,S,Z  1 Char)'
*-------
         $SBA  (8,3,N)
         DC    C'Member-Name: '
         $SBA  (8,16,U)
         DC    C'        '
         $SBA  (8,25,N)
         $SBA  (8,45,N)
         DC    C'(1-8 alphanumeric)'
*-------
         $SBA  (22,1,B)
         DC    C'----+----1----+----2----+----3----+----4----+----5'
         DC    C'----+----6----+----7----+---'
*-------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(0)
         END
        BKEND
        CATALS   A.SPFM007,0.0
        BKEND   A.SPFM007
*---------------------------------------------------------------------
*  Edit Member-Map                R03-22              5 VAR-OUT-FIELDS
*  1.SBAIC(Adr) 2.HDR1SZ(Adr) 3.LINE1(Adr)  4.LINE2(Adr)
*---------------------------------------------------------------------
SPFM007  CSECT
         DC C'SPFM007 ********'                       EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(4)             VAR-FIELD COUNT
         DC    AL3(MAPVLST)       ADDR MAP-VAR-OUTPUT-LIST
MAPSTART EQU   *
         DC XL1'42'                   WCC KBD RESET
*---------------------------------------------------------------------
SBAIC    DC    X'11C15D'         SBA, IC DEFAULT 2,14
HDR1SZ   EQU   *-MAPSTART
         $SBA  (3,1,UB)                                                  0000137
         DC    C'*====*'
         $SBA  (3,8,U)
LINE1    DC    CL72' '
*-------
         $SBA  (4,1,UB)                                                  0000137
         DC    C'*====*'
         $SBA  (4,8,U)
LINE2    DC    CL72' '
TWO1SZ   EQU   *-MAPSTART
*-------
         $SBA  (5,1,UB)                                                  0000137
         DC    C'*====*'
         $SBA  (5,8,U)
         DC    CL72' '
*-------
         $SBA  (6,1,UB)                                                  0000137
         DC    C'*====*'
         $SBA  (6,8,U)
         DC    CL72' '
*-------
         $SBA  (7,1,UB)                                                  0000137
         DC    C'*====*'
         $SBA  (7,8,U)
         DC    CL72' '
*-------
         $SBA  (8,1,UB)                                                  0000137
         DC    C'*====*'
         $SBA  (8,8,U)
         DC    CL72' '
*-------
         $SBA  (9,1,UB)                                                  0000137
         DC    C'*====*'
         $SBA  (9,8,U)
         DC    CL72' '
*-------
         $SBA  (10,1,UB)                                                 0000137
         DC    C'*====*'
         $SBA  (10,8,U)
         DC    CL72' '
*-------
         $SBA  (11,1,UB)                                                 0000137
         DC    C'*====*'
         $SBA  (11,8,U)
         DC    CL72' '
*-------
         $SBA  (12,1,UB)                                                 0000137
         DC    C'*====*'
         $SBA  (12,8,U)
         DC    CL72' '
*-------
         $SBA  (13,1,UB)                                                 0000137
         DC    C'*====*'
         $SBA  (13,8,U)
         DC    CL72' '
*-------
         $SBA  (14,1,UB)                                                 0000137
         DC    C'*====*'
         $SBA  (14,8,U)
         DC    CL72' '
*-------
         $SBA  (15,1,UB)                                                 0000137
         DC    C'*====*'
         $SBA  (15,8,U)
         DC    CL72' '
*-------
         $SBA  (16,1,UB)                                                 0000137
         DC    C'*====*'
         $SBA  (16,8,U)
         DC    CL72' '
*-------
         $SBA  (17,1,UB)                                                 0000137
         DC    C'*====*'
         $SBA  (17,8,U)
         DC    CL72' '
*-------
         $SBA  (18,1,UB)                                                 0000137
         DC    C'*====*'
         $SBA  (18,8,U)
         DC    CL72' '
*-------
         $SBA  (19,1,UB)                                                 0000137
         DC    C'*====*'
         $SBA  (19,8,U)
         DC    CL72' '
*-------
         $SBA  (20,1,UB)                                                 0000137
         DC    C'*====*'
         $SBA  (20,8,U)
         DC    CL72' '
*-------
         $SBA  (21,1,UB)                                                 0000137
         DC    C'*====*'
         $SBA  (21,8,U)
         DC    CL72' '
*-------
         $SBA  (22,1,UB)                                                 0000137
         DC    C'*====*'
         $SBA  (22,8,U)
         DC    CL72' '
*-------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(SBAIC)
         DC   A(HDR1SZ)
         DC   A(LINE1)
         DC   A(LINE2)
         END
        BKEND
        CATALS   A.SPFM008,0.0
        BKEND   A.SPFM008
*---------------------------------------------------------------------
*  SPFVS-Utilitiy Map              R04-22             0 VAR-OUT-FIELDS
*---------------------------------------------------------------------
SPFM008  CSECT
         DC C'SPFM008 ********'                       EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(0)             VAR-FIELD COUNT
         DC    AL3(MAPVLST)       ADDR MAP-VAR-OUTPUT-LIST
MAPSTART EQU   *
         DC XL1'42'                   WCC KBD RESET
*---------------------------------------------------------------------
         $SBA  (4,3,B)
         DC    C'3'
         $SBA  (4,6,N)
         DC    C'Move/Copy        - Move or copy Files'
*-------
         $SBA  (5,3,B)
         DC    C'4'
         $SBA  (5,6,N)
         DC    C'File List        - Display file names for Selection'
*-------
         $SBA  (6,3,B)
         DC    C' '
         $SBA  (6,6,N)
         DC    C'          '
*-------
         $SBA  (7,3,B)
         DC    C'V'
         $SBA  (7,6,N)
         DC    C'VTOC             - Volume Table of Contents'
*-------
         $SBA  (8,3,B)
         DC    C' '
         $SBA  (8,6,N)
         DC    C'           '
*-------
         $SBA  (9,3,B)
         DC    C' '
         $SBA  (9,6,N)
         DC    C'           '
*-------
         $SBA  (10,3,B)
         DC    C' '
         $SBA  (10,6,N)
         DC    C'           '
*-------
         $SBA  (11,3,B)
         DC    C' '
         $SBA  (11,6,N)
         DC    C'           '
*-------
         $SBA  (12,3,B)
         DC    C' '
         $SBA  (12,6,N)
         DC    C'           '
*-------
         $SBA  (13,3,B)
         DC    C' '
         $SBA  (13,6,N)
         DC    C'           '
*-------
         $SBA  (14,3,B)
         DC    C'X'
         $SBA  (14,6,N)
         DC    C'Exit             - Terminate this Program'
*-------
         $SBA  (15,3,B)
         DC    C' '
         $SBA  (15,6,N)
         DC    C'     '
*-------
         $SBA  (16,3,B)
         DC    C' '
         $SBA  (16,6,N)
         DC    C'     '
*-------
         $SBA  (17,3,B)
         DC    C' '
         $SBA  (17,6,N)
         DC    C'     '
*-------
         $SBA  (18,3,B)
         DC    C' '
         $SBA  (18,6,N)
         DC    C'     '
*-------
         $SBA  (19,3,B)
         DC    C' '
         $SBA  (19,6,N)
         DC    C'     '
*-------
         $SBA  (20,3,B)
         DC    C' '
         $SBA  (20,6,N)
         DC    C'     '
*-------
         $SBA  (21,3,B)
         DC    C' '
         $SBA  (21,6,N)
         DC    C'     '
*-------
         $SBA  (22,1,B)
         DC    C'----+----1----+----2----+----3----+----4----+----5'
         DC    C'----+----6----+----7----+---'
*-------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(0)
         END
        BKEND
        CATALS   A.SPFM009,0.0
        BKEND   A.SPFM009
*---------------------------------------------------------------------
*  SPFVS-System and Private Libraries 03-22           7 VAR-OUT-FIELDS
*---------------------------------------------------------------------
SPFM009  CSECT
         DC C'SPFM009 ********'                       EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(7)             VAR-FIELD COUNT
         DC    AL3(MAPVLST)       ADDR MAP-VAR-OUTPUT-LIST
MAPSTART EQU   *
         DC XL1'42'                   WCC KBD RESET
*---------------------------------------------------------------------
         $SBA  (6,3,B)
         DC    C'System Libraries'
*-------
         $SBA  (7,3,U)
SCLBS    DC    C'_'
         $SBA  (7,6,N)
         DC    C'Core Image Lib'
         $SBA  (7,36,N)
         DC    C'                '
*-------
         $SBA  (8,3,U)
SRLBS    DC    C'_'
         $SBA  (8,6,N)
         DC    C'Relocatable Lib'
         $SBA  (8,36,N)
         DC    C'i = info Lib'
*-------
         $SBA  (9,3,U)
SSLBS    DC    C'_'
         $SBA  (9,6,N)
         DC    C'Source Statement Lib'
         $SBA  (9,36,N)
         DC    C's = select Directory'
*-------
         $SBA  (10,3,U)
SPLBS    DC    C'_ '
         $SBA  (10,6,N)
         DC    C'Procedure Lib'
*-------
         $SBA  (11,3,B)
         DC    C' '
         $SBA  (11,6,N)
         DC    C'           '
*-------
         $SBA  (12,3,B)
         DC    C' '
         $SBA  (12,6,N)
         DC    C'           '
*-------
         $SBA  (13,3,B)
         DC    C' '
         $SBA  (13,6,N)
         DC    C'           '
*-------
         $SBA  (14,3,B)
         DC    C'Private Libraries'
*-------
         $SBA  (15,3,U)
PCLBS    DC    C'_'
         $SBA  (15,6,N)
         DC    C'Core Image Lib'
*-------
         $SBA  (16,3,U)
PRLBS    DC    C'_'
         $SBA  (16,6,N)
         DC    C'Relocatable Lib'
*-------
         $SBA  (17,3,U)
PSLBS    DC    C'_'
         $SBA  (17,6,N)
         DC    C'Source Statement Lib'
*-------
         $SBA  (18,3,N)
         DC    C'  '
         $SBA  (18,6,N)
         DC    C' '
*-------
         $SBA  (19,3,N)
         DC    C' '
         $SBA  (19,6,N)
         DC    C'           '
*-------
         $SBA  (20,3,N)
         DC    C' '
         $SBA  (20,6,N)
         DC    C'           '
*-------
         $SBA  (21,3,N)
         DC    C' '
         $SBA  (21,6,N)
         DC    C'           '
*-------
         $SBA  (22,1,B)
         DC    C'----+----1----+----2----+----3----+----4----+----5'
         DC    C'----+----6----+----7----+---'
*-------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(SCLBS)     SELECT SYSTEM  CLB
         DC   A(SRLBS)     SELECT SYSTEM  RLB
         DC   A(SSLBS)     SELECT SYSTEM  SLB
         DC   A(SPLBS)     SELECT SYSTEM  PLB
         DC   A(PCLBS)     SELECT PRIVATE CLB
         DC   A(PRLBS)     SELECT PRIVATE RLB
         DC   A(PSLBS)     SELECT PRIVATE SLB
         END
        BKEND
        CATALS   A.SPFM010,0.0
        BKEND   A.SPFM010
*---------------------------------------------------------------------
*  BROWSE Member-Map          R03-22        4 VAR-OUT-FIELDS
*  1.SBAIC(Adr) 2.HDR1SZ(Adr) 3.LINE1(Adr)  4.LINE2(Adr)
*---------------------------------------------------------------------
SPFM010  CSECT
         DC C'SPFM010 ********'                       EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(4)             VAR-FIELD COUNT
         DC    AL3(MAPVLST)       ADDR MAP-VAR-OUTPUT-LIST
MAPSTART EQU   *
         DC XL1'42'                   WCC KBD RESET
*---------------------------------------------------------------------
SBAIC    DC    X'11C15D'         SBA, IC DEFAULT 2,14
HDR1SZ   EQU   *-MAPSTART
         $SBA  (3,1,N)                                                  00001376
LINE1    DC    CL79' '
*-------
         $SBA  (4,1,N)                                                  00001376
LINE2    DC    CL79' '
*-------
         $SBA  (5,1,N)                                                  00001376
         DC    CL79' '
*-------
         $SBA  (6,1,N)                                                  00001376
         DC    CL79' '
*-------
         $SBA  (7,1,N)
         DC    CL79' '
*-------
         $SBA  (8,1,N)                                                  00001376
         DC    CL79' '
*-------
         $SBA  (9,1,N)                                                  00001376
         DC    CL79' '
*-------
         $SBA  (10,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (11,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (12,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (13,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (14,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (15,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (16,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (17,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (18,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (19,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (20,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (21,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (22,1,N)                                                 00001376
         DC    CL79' '
*-------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(SBAIC)
         DC   A(HDR1SZ)
         DC   A(LINE1)
         DC   A(LINE2)
         END
        BKEND
        CATALS   A.SPFM011,0.0
        BKEND   A.SPFM011
*---------------------------------------------------------------------
*  LIB/DIR-Info-MAP               R03-22              4 VAR-OUT-FIELDS
*  1.SBAIC(Adr) 2.HDR1SZ(Adr) 3.LINE1(Adr)  4.LINE2(Adr)
*---------------------------------------------------------------------
SPFM011  CSECT
         DC C'SPFM011 ********'                       EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(1)             VAR-FIELD COUNT
         DC    AL3(MAPVLST)       ADDR MAP-VAR-OUTPUT-LIST
MAPSTART EQU   *
         DC XL1'42'                   WCC KBD RESET
*---+----1----5----2----+----3----+----4----+----5----+----6----+---7
         $SBA  (3,30,B)                                                  0000137
         DC    CL79'Status-Information'
*-------
         $SBA  (4,1,B)                                                  00001376
         DC    CL37'Start-CHR  Next-CHR  Last-CHR  Act-E '
         DC    CL42' Allocat Active Deleted Avail Conds Trk/Cyl'
*-------
         $SBA  (5,1,N)                                                  00001376
LINE1    DC    CL36'nnn nn nn nnn nn nn nnn nn nn nnnnnn'
LINE1A   DC    CL43'  nnnnnn nnnnnn  nnnnnn nnnnn nnnn  nn/nnnn'
*-------
         $SBA  (6,1,N)                                                  00001376
         DC    CL79' '
*-------
         $SBA  (7,1,N)
         DC    CL79' '
*-------
         $SBA  (8,1,N)                                                  00001376
         DC    CL79' '
*-------
         $SBA  (9,1,N)                                                  00001376
         DC    CL79' '
*-------
         $SBA  (10,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (11,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (12,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (13,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (14,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (15,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (16,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (17,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (18,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (19,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (20,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (21,1,N)                                                 00001376
         DC    CL79' '
*-------
         $SBA  (22,1,N)                                                 00001376
         DC    CL79' '
*-------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(LINE1)
         END
        BKEND
        CATALS   A.SPFM012,0.0
        BKEND   A.SPFM012
*---------------------------------------------------------------------
*  LIB/DIR-Status-Info-Map        R03-22             0x VAR-OUT-FIELDS
*  1.
*---------------------------------------------------------------------
SPFM012  CSECT
         DC C'SPFM012 ********'                       EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(15)            VAR-FIELD COUNT
         DC    AL3(MAPVLST)       ADDR MAP-VAR-OUTPUT-LIST
MAPSTART EQU   *
         DC XL1'42'                   WCC KBD RESET
*---+----1----5----2----+----3----+----4----+----5----+----6----+---7
         $SBA  (3,33,B)
         DC    CL20'Status-Information'
*-------
         $SBA  (4,30,B)
         DC    CL25'Directory         Library'
*-------
         $SBA  (5,1,N)
M012LIB  DC    CL25'SYSRES SOURCE.STMT LIB'
*-------
         $SBA  (6,1,B)
         DC    CL25'Start-Addr CCC HH RR'
         $SBA  (6,30,N)
M012SAD  DC    CL9'nnn nn nn'
         $SBA  (6,46,N)
M012SAL  DC    CL9'nnn nn nn'
*-------
         $SBA  (7,1,B)
         DC    CL25'Next-Addr  CCC HH RR EE'
         $SBA  (7,30,N)
M012NAD  DC    CL15'nnn nn nn nn'
         $SBA  (7,46,N)
M012NAL  DC    CL12'nnn nn nn nn'
*-------
         $SBA  (8,1,B)
         DC    CL25'Last-Addr  CCC HH RR EE'
         $SBA  (8,30,N)
M012LAD  DC    CL15'nnn nn nn nn'
         $SBA  (8,46,N)
M012LAL  DC    CL12'nnn nn nn nn'
*-------
         $SBA  (9,1,N)
         DC    CL79' '
*-------
         $SBA  (10,1,B)
         DC    CL30'Active Dir-Entries'
         $SBA  (10,30,N)
M012AED  DC    CL9'nnnnnnnnn'
*-------
         $SBA  (11,1,B)
         DC    CL20'Allocated Lib-Blks'
         $SBA  (11,46,N)
M012ALB  DC    CL9'nnnnnnnnn'
*-------
         $SBA  (12,1,B)
         DC    CL20'Active Lib-Blks'
         $SBA  (12,46,N)
M012ACB  DC    CL9'nnnnnnnnn'
*-------
         $SBA  (13,1,B)
         DC    CL20'Deleted Lib-Blks'
         $SBA  (13,46,N)
M012DLB  DC    CL9'nnnnnnnnn'
*-------
         $SBA  (14,1,B)
         DC    CL20'Available Lib-Blks'
         $SBA  (14,46,N)
M012AVB  DC    CL9'nnnnnnnnn'
*-------
         $SBA  (15,1,B)
         DC    CL20'Codense Limit-Blks'
         $SBA  (15,46,N)
M012ACL  DC    CL9'nnnnnnnnn'
*-------
         $SBA  (16,1,B)
         DC    CL25'Allocated Cyls Lib+Dir'
         $SBA  (16,46,N)
M012ACLD DC    CL9'nnnnnnnnn'
*-------
         $SBA  (17,1,B)
         DC    CL25'Allocated Dir-Tracks'
         $SBA  (17,30,N)
M012ADT  DC    CL9'nnnnnnnnn'
*-------
         $SBA  (18,1,N)
         DC    CL79' '
*-------
         $SBA  (19,1,N)
         DC    CL79' '
*-------
         $SBA  (20,1,N)
         DC    CL79' '
*-------
         $SBA  (21,1,N)
         DC    CL79' '
*-------
         $SBA  (22,1,N)
         DC    CL79' '
*-------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(M012LIB)        LibrarY Name
         DC   A(M012SAD)        Start-Address-Directory
         DC   A(M012SAL)        Start-Address-Library
         DC   A(M012NAD)        Next-Address-Directory
         DC   A(M012NAL)        Next-Address-Library
         DC   A(M012LAD)        Last-Address-Directory
         DC   A(M012LAL)        Last-Address-Library
         DC   A(M012AED)        Start-Address-Library
         DC   A(M012ALB)        Start-Address-Library
         DC   A(M012ACB)        Start-Address-Library
         DC   A(M012DLB)        Start-Address-Library
         DC   A(M012AVB)        Start-Address-Library
         DC   A(M012ACL)        Start-Address-Library
         DC   A(M012ACLD)       Start-Address-Library
         DC   A(M012ADT)        Start-Address-Library
         END
        BKEND
        CATALS   A.SPFM013,0.0
        BKEND   A.SPFM013
*---------------------------------------------------------------------
*  DIRECTORY-DISPLAY MAP      R03-22        4 VAR-OUT-FIELDS
*  1.SBAIC(Adr) 2.HDR1SZ(Adr) 3.LINE1(Adr)  4.LINE2(Adr)
*---------------------------------------------------------------------
SPFM013  CSECT
         DC C'SPFM013 ********'                       EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(4)             VAR-FIELD COUNT
         DC    AL3(MAPVLST)       ADDR MAP-VAR-OUTPUT-LIST
MAPSTART EQU   *
         DC XL1'42'                   WCC KBD RESET
*---------------------------------------------------------------------
SBAIC    DC    X'11C15D'         SBA, IC DEFAULT 2,14
HDR1SZ   EQU   *-MAPSTART
         $SBA  (3,1,U)                                                  00001376
         DC    CL1'_'
         $SBA  (3,3,N)                                                  00001376
LINE1    DC    CL77' '
*-------
         $SBA  (4,1,U)                                                  00001376
         DC    CL1'_'
         $SBA  (4,3,N)                                                  00001376
LINE2    DC    CL77' '
*-------
         $SBA  (5,1,U)                                                  00001376
         DC    CL1'_'
         $SBA  (5,3,N)                                                  00001376
         DC    CL77' '
*-------
         $SBA  (6,1,U)                                                  00001376
         DC    CL1'_'
         $SBA  (6,3,N)                                                  00001376
         DC    CL77' '
*-------
         $SBA  (7,1,U)                                                  00001376
         DC    CL1'_'
         $SBA  (7,3,N)
         DC    CL77' '
*-------
         $SBA  (8,1,U)                                                  00001376
         DC    CL1'_'
         $SBA  (8,3,N)                                                  00001376
         DC    CL77' '
*-------
         $SBA  (9,1,U)                                                  00001376
         DC    CL1'_'
         $SBA  (9,3,N)                                                  00001376
         DC    CL77' '
*-------
         $SBA  (10,1,U)                                                  0000137
         DC    CL1'_'
         $SBA  (10,3,N)                                                 00001376
         DC    CL77' '
*-------
         $SBA  (11,1,U)                                                  0000137
         DC    CL1'_'
         $SBA  (11,3,N)                                                 00001376
         DC    CL77' '
*-------
         $SBA  (12,1,U)                                                  0000137
         DC    CL1'_'
         $SBA  (12,3,N)                                                 00001376
         DC    CL77' '
*-------
         $SBA  (13,1,U)                                                  0000137
         DC    CL1'_'
         $SBA  (13,3,N)                                                 00001376
         DC    CL77' '
*-------
         $SBA  (14,1,U)                                                  0000137
         DC    CL1'_'
         $SBA  (14,3,N)                                                 00001376
         DC    CL77' '
*-------
         $SBA  (15,1,U)                                                  0000137
         DC    CL1'_'
         $SBA  (15,3,N)                                                 00001376
         DC    CL77' '
*-------
         $SBA  (16,1,U)                                                  0000137
         DC    CL1'_'
         $SBA  (16,3,N)                                                 00001376
         DC    CL77' '
*-------
         $SBA  (17,1,U)                                                  0000137
         DC    CL1'_'
         $SBA  (17,3,N)                                                 00001376
         DC    CL77' '
*-------
         $SBA  (18,1,U)                                                  0000137
         DC    CL1'_'
         $SBA  (18,3,N)                                                 00001376
         DC    CL77' '
*-------
         $SBA  (19,1,U)                                                  0000137
         DC    CL1'_'
         $SBA  (19,3,N)                                                 00001376
         DC    CL77' '
*-------
         $SBA  (20,1,U)                                                  0000137
         DC    CL1'_'
         $SBA  (20,3,N)                                                 00001376
         DC    CL77' '
*-------
         $SBA  (21,1,U)                                                  0000137
         DC    CL1'_'
         $SBA  (21,3,N)                                                 00001376
         DC    CL77' '
*-------
         $SBA  (22,1,U)                                                  0000137
         DC    CL1'_'
         $SBA  (22,3,N)                                                 00001376
         DC    CL77' '
*-------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(SBAIC)
         DC   A(HDR1SZ)
         DC   A(LINE1)
         DC   A(LINE2)
         END
        BKEND
        CATALS   A.SPFM014,0.0
        BKEND   A.SPFM014
*---------------------------------------------------------------------
*  SPFVS-System and Private Libraries 03-22           0 VAR-OUT-FIELDS
*---------------------------------------------------------------------
SPFM014  CSECT
         DC C'SPFM014 ********'                       EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(60)            VAR-FIELD COUNT
         DC    AL3(MAPVLST)       ADDR MAP-VAR-OUTPUT-LIST
MAPSTART EQU   *
         DC XL1'42'                   WCC KBD RESET
*---------------------------------------------------------------------
         $SBA  (4,35,B)
         DC    C'System DASDs'
*-------
         $SBA  (07,03,N)
R07C03   DC    C'xxx - yyyy  '
*
         $SBA  (08,03,N)
R08C03   DC    C'            '
*
         $SBA  (09,03,N)
R09C03   DC    C'            '
*
         $SBA  (10,03,N)
R10C03   DC    C'            '
*
         $SBA  (11,03,N)
R11C03   DC    C'            '
*
         $SBA  (12,03,N)
R12C03   DC    C'            '
*
         $SBA  (13,03,N)
R13C03   DC    C'            '
*
         $SBA  (14,03,N)
R14C03   DC    C'            '
*
         $SBA  (15,03,N)
R15C03   DC    C'            '
*
         $SBA  (16,03,N)
R16C03   DC    C'            '
*
         $SBA  (17,03,N)
R17C03   DC    C'            '
*
         $SBA  (18,03,N)
R18C03   DC    C'            '
*
         $SBA  (07,19,N)
R07C19   DC    C'            '
*
         $SBA  (08,19,N)
R08C19   DC    C'            '
*
         $SBA  (09,19,N)
R09C19   DC    C'            '
*
         $SBA  (10,19,N)
R10C19   DC    C'            '
*
         $SBA  (11,19,N)
R11C19   DC    C'            '
*
         $SBA  (12,19,N)
R12C19   DC    C'            '
*
         $SBA  (13,19,N)
R13C19   DC    C'            '
*
         $SBA  (14,19,N)
R14C19   DC    C'            '
*
         $SBA  (15,19,N)
R15C19   DC    C'            '
*
         $SBA  (16,19,N)
R16C19   DC    C'            '
*
         $SBA  (17,19,N)
R17C19   DC    C'            '
*
         $SBA  (18,19,N)
R18C19   DC    C'            '
*
         $SBA  (07,35,N)
R07C35   DC    C'            '
*
         $SBA  (08,35,N)
R08C35   DC    C'            '
*
         $SBA  (09,35,N)
R09C35   DC    C'            '
*
         $SBA  (10,35,N)
R10C35   DC    C'            '
*
         $SBA  (11,35,N)
R11C35   DC    C'            '
*
         $SBA  (12,35,N)
R12C35   DC    C'            '
*
         $SBA  (13,35,N)
R13C35   DC    C'            '
*
         $SBA  (14,35,N)
R14C35   DC    C'            '
*
         $SBA  (15,35,N)
R15C35   DC    C'            '
*
         $SBA  (16,35,N)
R16C35   DC    C'            '
*
         $SBA  (17,35,N)
R17C35   DC    C'            '
*
         $SBA  (18,35,N)
R18C35   DC    C'            '
*
         $SBA  (07,51,N)
R07C51   DC    C'            '
*
         $SBA  (08,51,N)
R08C51   DC    C'            '
*
         $SBA  (09,51,N)
R09C51   DC    C'            '
*
         $SBA  (10,51,N)
R10C51   DC    C'            '
*
         $SBA  (11,51,N)
R11C51   DC    C'            '
*
         $SBA  (12,51,N)
R12C51   DC    C'            '
*
         $SBA  (13,51,N)
R13C51   DC    C'            '
*
         $SBA  (14,51,N)
R14C51   DC    C'            '
*
         $SBA  (15,51,N)
R15C51   DC    C'            '
*
         $SBA  (16,51,N)
R16C51   DC    C'            '
*
         $SBA  (17,51,N)
R17C51   DC    C'            '
*
         $SBA  (18,51,N)
R18C51   DC    C'            '
*
         $SBA  (07,67,N)
R07C67   DC    C'            '
*
         $SBA  (08,67,N)
R08C67   DC    C'            '
*
         $SBA  (09,67,N)
R09C67   DC    C'            '
*
         $SBA  (10,67,N)
R10C67   DC    C'            '
*
         $SBA  (11,67,N)
R11C67   DC    C'            '
*
         $SBA  (12,67,N)
R12C67   DC    C'            '
*
         $SBA  (13,67,N)
R13C67   DC    C'            '
*
         $SBA  (14,67,N)
R14C67   DC    C'            '
*
         $SBA  (15,67,N)
R15C67   DC    C'            '
*
         $SBA  (16,67,N)
R16C67   DC    C'            '
*
         $SBA  (17,67,N)
R17C67   DC    C'            '
*
         $SBA  (18,67,N)
R18C67   DC    C'            '
*
*-------
         $SBA  (22,1,B)
         DC    C'----+----1----+----2----+----3----+----4----+----5'
         DC    C'----+----6----+----7----+---'
*-------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(R07C03)
         DC   A(R08C03)
         DC   A(R09C03)
         DC   A(R10C03)
         DC   A(R11C03)
         DC   A(R12C03)
         DC   A(R13C03)
         DC   A(R14C03)
         DC   A(R15C03)
         DC   A(R16C03)
         DC   A(R17C03)
         DC   A(R18C03)
         DC   A(R07C19)
         DC   A(R08C19)
         DC   A(R09C19)
         DC   A(R10C19)
         DC   A(R11C19)
         DC   A(R12C19)
         DC   A(R13C19)
         DC   A(R14C19)
         DC   A(R15C19)
         DC   A(R16C19)
         DC   A(R17C19)
         DC   A(R18C19)
         DC   A(R07C35)
         DC   A(R08C35)
         DC   A(R09C35)
         DC   A(R10C35)
         DC   A(R11C35)
         DC   A(R12C35)
         DC   A(R13C35)
         DC   A(R14C35)
         DC   A(R15C35)
         DC   A(R16C35)
         DC   A(R17C35)
         DC   A(R18C35)
         DC   A(R07C51)
         DC   A(R08C51)
         DC   A(R09C51)
         DC   A(R10C51)
         DC   A(R11C51)
         DC   A(R12C51)
         DC   A(R13C51)
         DC   A(R14C51)
         DC   A(R15C51)
         DC   A(R16C51)
         DC   A(R17C51)
         DC   A(R18C51)
         DC   A(R07C67)
         DC   A(R08C67)
         DC   A(R09C67)
         DC   A(R10C67)
         DC   A(R11C67)
         DC   A(R12C67)
         DC   A(R13C67)
         DC   A(R14C67)
         DC   A(R15C67)
         DC   A(R16C67)
         DC   A(R17C67)
         DC   A(R18C67)
         END
        BKEND
        CATALS   A.SPFMERR,0.0
        BKEND   A.SPFMERR
*---------------------------------------------------------------------
* Top Map 2 Rows with Option      R01-02            1 VAR-OUT-Fields
* 1.MAPERROR (34)
*---------------------------------------------------------------------
SPFMERR  CSECT
         DC C'SPFMERR ********'                       EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(1)             VAR-FIELD COUNT
         DC    AL3(MAPVLST)      ADDR MAP-VAR-OUTPUT-LIST
*---------------------------------------------------------------------
MAPSTART EQU   *
         DC XL1'42'                   WCC KBD RESET
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7-
         $SBA  (1,46,B)
MAPERROR DC    34CL1' '
*-------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(MAPERROR)
         END
        BKEND
        CATALS   A.SPFMWARN,0.0
        BKEND   A.SPFMWARN
*---------------------------------------------------------------------
* Top Map 2 Rows with Option      R01-02            4 VAR-OUT-Fields
* 1.TEXT1(30) 2.TEXT2(30) 3.TEXT3(40) 4.TEXT4(30)
*---------------------------------------------------------------------
SPFMWARN CSECT
         DC C'SPFMWARN********'                      EYECATCHER
         DC    A(MAPEND-MAPSTART) MAP-LEN
         DC    AL1(4)             VAR-FIELD COUNT
         DC    AL3(MAPVLST)      ADDR MAP-VAR-OUTPUT-LIST
*---------------------------------------------------------------------
MAPSTART EQU   *
         DC XL1'42'                   WCC KBD RESET
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7-
         $SBA  (6,45,B)
         DC    CL30'+----------------------------+'
         $SBA  (7,45,B)
TEXT1    DC    CL30'I                            I'
         $SBA  (8,45,B)
TEXT2    DC    CL30'I                            I'
         $SBA  (9,45,B)
TEXT3    DC    CL30'I                            I'
         $SBA  (10,45,B)
TEXT4    DC    CL30'I                            I'
         $SBA  (11,45,B)
         DC    CL30'I ENTER=Continue   PF3=Exit  I'
         $SBA  (12,45,B)
         DC    CL30'+----------------------------+'
*-------
MAPEND   EQU  *
MAPVLST  DS   0F
         DC   A(TEXT1)
         DC   A(TEXT2)
         DC   A(TEXT3)
         DC   A(TEXT4)
         END
        BKEND
/*

