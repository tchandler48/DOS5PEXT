* $$ JOB JNM=ASMBLR,USER='BUDROW',CLASS=0,DISP=D
* $$ LST LST=00E,JSEP=1,CLASS=A,DISP=D
// JOB ASMBLR
ASSGN SYS001,X'362'
ASSGN SYS002,X'362'
ASSGN SYS003,X'362'
ASSGN SYS004,X'362'
// OPTION LINK
// EXEC ASSEMBLY
HELO     START 0
         BALR 12,0
         USING *,12
         LA 13,SAVE
         COMRG
         OPEN INPUT,OUTPUT
MORE     GET INPUT
         PUT OUTPUT
         B   MORE
ENDJOB   CLOSE INPUT,OUTPUT
         EOJ
INPUT    DTFCD DEVADDR=SYSIPT,IOAREA1=R1,EOFADDR=ENDJOB,BLKSIZE=80
OUTPUT   DTFPR DEVADDR=SYSLST,IOAREA1=P1
R1       DS 0CL80
P1       DC CL133' '
SAVE     DS 32F
         END HELO
/*
// EXEC LNKEDT
// EXEC
HELLO FROM DOS/VS ASSEMBLER!!
    HOPE YOU ENJOY YOUR PC MAINFRAME
/*
/&
* $$ EOJ
