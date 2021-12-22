* $$ JOB JNM=FORLNKGO,DISP=D,PRI=3,CLASS=0,USER='PROGRAMMER'
* $$ LST LST=00E,DISP=D,PRI=3,CLASS=A
// JOB FORTRAN
// ASSGN SYSIN,X'00C'
// ASSGN SYSLST,X'00E'
// ASSGN SYSLNK,X'362'
// ASSGN SYS001,X'362'
// ASSGN SYS002,X'362'
// ASSGN SYS003,X'362'
// ASSGN SYS004,X'362'
// OPTION LINK
// EXEC FFORTRAN
C HELLO WORLD, WE HOPE                                                  00070000
      WRITE(3,10)                                                       00080000
   10 FORMAT(12H HELLO WORLD)                                           00090000
      STOP                                                              00100000
      END                                                               00110000
/*
// EXEC LNKEDT
// EXEC
/*
/&
* $$ EOJ
