

This area contains copies and updates/variants of the FM32-40-2 program from Appendix F of the book. It is believed that the original files are accurate. The Ver 1.00 file has some formatting issues from being copied from the PDF version of the book. The QBasic version (1.03) is a mix of version 1.00 and 1.02 source code separated into subroutines and in some cases reworked to completely eliminate the need for line numbers and GOTO/GOSUB. At the time of this writing, the C version is a work in progress and it may also eventually get converted to C++. The C version (1.04) will have more changes and some improvements and rewrites to be more compatible witht he C programming language. All versions should remain compatible and mostly functionally identical. The calls to subs or functions with QB or C has slightly changed how some of the menu system works as they always return back through the call order unlike spaghetti code GW-BASIC.


There are 4 basic branches here:

Originals: The source code pretty much direct from the PDF book form and an updated version found online.

Qbasic: This is the Qbasic version. It builds under Quick Basic 4.0/4.5 and QB64 as well as runs under Qbasic. This file built with QB64 is likely to be the least problematic for those unfamiliar with QB.

C: A C version that uses older Borland style conio library functions. It must have conio2 or a similar MingW/Windows/GCC compatible version. It should build under Borland C++ 4.0 - 5.0 as well as Mingw with conio2. The conio2 files are included here but may not be the latest versions. Linux (GCC) users should be able to use a Linux version conio library that leverages curses to produce an equivalent result. Some other library functions are included to simplify the transition from QB to C.

Executables: Most likely a QB64 build and hopefully a leaner and faster C version build will be posted here. There should at least be 32 and 64 bit compabible builds available from QB64.