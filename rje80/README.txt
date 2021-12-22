                 RJE80 Beta Version 0.68
                 -----------------------

Version 0.68 of RJE80 features a full Windows GUi interface that's
easy and pleasant to use.  There are three windows: a reader, a
punch, and a printer.  Operation should be simple and intuitive.
If not, suggestions are appreciated!  See the Help button on the
reader control panel for more details.

The GUI version works best on a desktop of at least 1024x768, and
1280x1024 is even better -- you can see a whole 132 X 66 page at
a time.  800x600 is cramped but usable.  I've arranged the initial
layout to suit 1024x768 but you can arrange it how you like for 
your screen and it should remember your settings automatically.

This ZIP contains executables and support files for this BETA
release.  Here are the highlights:

    * RJE80.exe     - the Windows GUI executable.
    * RJE80C.exe    - the command-line Windows version
    * rje80         - the Linux command line version
    * rjeconfig.txt - Notes on how to configure IBM OSes for RJE

To make it easy for you to try RJE even if you haven't configured
your system for it, I'm maintaining a set of Hercules System/370
mainframes ready for you to connect into.  Once you try RJE80
you may want to use it at home on your system.

MVS

When RJE80 first comes up of of the box, it's ready to connect to
my MVS RJE system at port 3780.  Simply press 'Connect' and the
status window on the right of the reader should go to 'Connected'.
It's possible, however, that someone else might be online on that
port -- in case, try port 3781, 3782, or 3783.  If all of them
fail, possibly MVS itself has gone belly up.  I can't guarantee
any of these systems will be up and running, though I'll try and 
keep an eye on them -- none were written to run unattended by an
operator.

My JES2 RJE lines are not passworded.  There's no need to sign on,
although I've provided a sample mvs_logon.jcl file as an example.
You can simply send jobs to the mainframe.  I've provided a 
couple of sample JCL decks to send to MVS so you can see the 
output.  'mvs_vtoc.jcl' prints a vtoc, and so on.  To logoff MVS
RJE, simply press the Disconnect button.

OS/360

You can also connect to OS/360.  For this, you must change the
host to "diana.sysun.com 2780 OS" and press Connect.  The first
file you must send is the "os_logon.jcl" file, in order to 
signon to the line.  If line 1 is occupied, you can connect and
logon to another of the lines at 2781, 2782, or 2783 -- but you
must change the name of the TERM1 in the os_logon before you send
it.  For example to logon to line 2 it must be TERM2.  There is
one sample JCL deck, os_vtoc.jcl.  To signoff OS/360 RJE, send
the os_logoff.jcl deck.  OS/360 RJE is flaky and rather touchy,
don't be surprised if it crashes on you or isn't available.  In
the real world HASP was most often used.  I hope one day to get
HASP running with MVT.

VM/370

VM/370 is here for trial.  Connect to "diana.sysun.com 4780 VM"
and send the vm_logon.jcl deck to signon to line 1.  If line 1
is busy you can try the others at 4781, 4782, or 4783 -- but you
must change the vm_signon to match, for example, line 2 must
signon to DIALUP2.  There's a CMSBATCH machine running, and you 
can send vm_list.jcl to the line and see the results of its run,
or, I've provided four guest accounts on this VM/370 system so you
can logon and try sending and receiving files.  Logon as GUEST1
password GUEST1 (or 2 or 3 or 4).  To logoff the RSCS RJE line,
just press Disconnect.  Instructions on how to send files to and
from the VM account should be in the help.

DOS/VS

DOs/VS is also available for trial.  Change the text in the host 
window to "diana.sysun.com 5780 DOS" and Connect.  However, DOS
is not very robust when it comes to errors, so I ask you to be
careful for the sake of others ... most JECL and JCL errors will
cause a message to the DOS operator, and I guarantee the DOS/VS
console is probably unmanned.  To logon to DOS/VS, use dos_logon.jcl,
to logoff, press Disconnect. dos_vtoc.jcl is a sample VTOC listing
job.  If your job won't run, give the command "* .. D RDR" to see
the reader queue.  If your job is sitting there, probably a JCL
error or something else has brought the background to a halt and
the operator isn't there to fix it.  Remember too that DOS/VS is
rather slow to send printouts back, it may take 10 to 20 seconds
before you receive your results.  Be patient.

Please send bug reports/comments/suggestions to ceo1944@yahoo.com

