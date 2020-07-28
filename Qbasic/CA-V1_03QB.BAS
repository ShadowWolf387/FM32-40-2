Declare sub AnyKey ()
Declare function OneKey$ ()
DECLARE SUB RepeatLenCheck ()
DECLARE SUB PrintDigrStat ()
DECLARE SUB PrintMonoStat1 ()
DECLARE SUB PrintMonoStat2 ()
DECLARE SUB PrintMonoStat3 ()
DECLARE SUB AperiodKeyGen ()
DECLARE SUB AlphaEntryMenu ()
DECLARE SUB StripCipherTxt ()
DECLARE SUB CipherTxtPrep ()
DECLARE SUB EncipherSub ()
DECLARE SUB DecipherSub ()
DECLARE SUB PlainTxtDisk ()
DECLARE SUB CipherTxtDisk ()
DECLARE SUB PlainTxtKbd ()
DECLARE SUB CipherTxtKbd ()
DECLARE SUB MonoFreqCnt ()
DECLARE SUB ChiFreqCnt ()
DECLARE SUB PeriodFreqCnt ()
DECLARE SUB DigraphFreqCnt ()
DECLARE SUB FreqCntMenu ()
DECLARE SUB TextEntryMenu ()
DECLARE SUB ChoiceError ()
DECLARE SUB MonoAlphaUni ()
DECLARE SUB PeriodPolyAlpha ()
DECLARE SUB AperiodPolyAlpha ()
DECLARE SUB ReadComponent ()
DECLARE SUB PrintFreqCnt ()
DECLARE SUB SaveFreqCnt ()
DECLARE SUB PrintText ()
DECLARE SUB SaveText ()
DECLARE SUB FindRepeats ()
DECLARE SUB QBmain ()
'***************************************************************************
' CRYPTANALYSIS SUPPORT PROGRAM
' by Walter Howe
' Version 1.03 (1.02 converted to Qbasic with changes)
' 10 April 1992
'
' Developed in support of FM 34-40-2, Basic Cryptanalysis to provide
' accurate encryption, decryption, frequency counts, and statistics for use
' in the manual. It can be used for other applications.
'
' The program was written in Microsoft GW-BASIC on a Wang PC.  It has been
' run on other PCs running Microsoft QUICKBASIC and QBASIC without problems.
'
' As written, the program will print on a dot matrix printer with the name
' LPT1 that uses standard Epson control codes. If necessary, change the
' values in the *** Printer Setup *** section for the particular printer
' to be used.
'***************************************************************************

'***************************************************************************
'***************************************************************************
' Converted to Quick Basic
' Fixed some bugs (I hope no new ones)
' Program lines mostly limited to 80 chars and reformatted indents.
' Broke up most (all?) multi-statement lines.
' Unused lines either REMed or removed.
' Since all SUBs return, some menu functions may work slightly different.
' All line numbers removed. (mostly)
' All Goto's and Gosub's removed. (mostly)
' Subroutines moved to individual subs or functions.
' Some remarks changed to reflect program changes.
'
' Becuase this was a GW-BASIC program there are an insane number of global
' variables. Many were previously undeclared. Many previously global
' variables are now isolated. With more work some others may be removed.
'***************************************************************************
'***************************************************************************

CONST MAXLINES = 25
CONST MAXALPHBT = 20
CONST MAXCCOMP = 200

'***************************************************************************
' Declare global variables. Undeclared in GW-BASIC
'***************************************************************************
COMMON SHARED Aflag
COMMON SHARED CMIXflag
COMMON SHARED Dflag
COMMON SHARED FILEflag
COMMON SHARED Formfeed$
COMMON SHARED Mflag
COMMON SHARED NRlines
COMMON SHARED Outfile$
COMMON SHARED Period
COMMON SHARED Pflag
COMMON SHARED Printer$
COMMON SHARED RepeatKey$
COMMON SHARED Rkey$
DIM SHARED Status$(10)

' *** Previously declared variables
DIM SHARED PTEXTD$(MAXLINES), PTEXTI$(MAXLINES), CTEXTD$(MAXLINES)
DIM SHARED CTEXTI$(MAXLINES)
' Plain and ciphertext may be stored in two forms: display and internal.
' Display forms (PTEXTD$() and CTEXTD$()) are as typed with spaces.
' Internal forms (PTEXTI$() and CTEXTI$()) have spaces, and nonliteral
' characters stripped away. All frequency counts and ICs are performed on
' CTEXTI$() strings. Up to 25 lines of text are allowed, as written.
' Additional lines of text may be used if the CONST MAXLINES is increased.

DIM SHARED MFREQ(26), PFREQ(MAXALPHBT, 27), DIFREQ(26, 26), PHIMONO
DIM SHARED PHIPERI(MAXALPHBT), PHIDIG, PMIXFREQ(MAXALPHBT, 27)
DIM SHARED SET1(26), SET2(27), MATCH(27), PERPHISUM(MAXALPHBT)
DIM SHARED PERTOTLTR(MAXALPHBT)
' Sets up monographic, periodic, and digraphic frequency, IC tables. Up
' to 20 alphabets are allowed for periodic frequencies, as written. The
' number of alphabets can be increased by changing the CONST MAXALPHBT.

DIM SHARED PCOMP$, CCOMP$(MAXCCOMP) ' Variables for plain and cipher components with up
' to 200 cipher component sequences for long running key aperiodics. The
' length of the key may be increased by changing the CONST MAXCCOMP.

'***************************************************************************
' Start of program
'***************************************************************************

KEY OFF ' Turns off prompts on bottom of screen.

' *** Initialize Variables ***

' *** Printer Setup ***
Printer$ = "PRN1"
Formfeed$ = CHR$(12)
' CRLF$ = CHR$(13) + CHR$(10) ' (not used in 1.0)
' CONDENSED$ = CHR$(15) ' (not used in 1.0)
' DC2$ = CHR$(18) ' Cancels condensed mode (not used in 1.0)
' ELITE$ = CHR$(27) + "M" ' (not used in 1.0)
' PICA$ = CHR$(27) + "P" ' (not used in 1.0)

'***************************************************************************
' Run the program
'***************************************************************************

QBmain

END

SUB AlphaEntryMenu
    ' *** Alphabet Entry Subroutine ***
    PCOMP$ = "abcdefghijklmnopqrstuvwxyz"
    CCOMPO$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    Rkey$ = "AAAAAAAAAAAAAAAAAAAA"
    Period = 1
    CLS
    PRINT "Select type of system:"
    PRINT
    PRINT "     1. Monoalphabetic uniliteral"
    PRINT "     2. Periodic polyalphabetic"
    PRINT "     3. Aperiodic polyalphabetic"
    PRINT
    PRINT
    PRINT "Enter your choice: "
    Selection = ASC(OneKey$) - 48
    SELECT CASE Selection
        CASE 1
            MonoAlphaUni 'GOSUB 4240
        CASE 2
            PeriodPolyAlpha 'GOSUB 4860
        CASE 3
            AperiodPolyAlpha 'GOSUB 6020
        CASE ELSE
            ChoiceError
    END SELECT
END SUB

SUB AnyKey
    PRINT "PRESS ANY KEY TO CONTINUE."
    Pause$ = OneKey$
END SUB

SUB AperiodKeyGen
    ' *** Aperiodic Long-Running Key Generation Subroutine ***
    CLS
    RANDOMIZE
    INPUT "Enter the number of alphabets (up to 200): "; Period
    FOR N = 1 TO Period
        LRK$ = LRK$ + CHR$(INT(RND * 26) + 65)
    NEXT N
    RepeatKey$ = LRK$
    Rkey$ = LRK$
END SUB

SUB AperiodPolyAlpha
    ' *** Sets Flag Indicating Long-Running Key System ***
    Aflag = 1
    PeriodPolyAlpha
END SUB

SUB ChiFreqCnt
    ' *** Chi Test Subroutine ***
    DO
        PRINT "Do you want to print results or save to disk as text file?"
        INPUT "Enter P for printer, D for disk, or Q to quit.", S$
        S$ = UCASE$(S$)
        IF S$ = "P" THEN Outfile$ = Printer$
        IF S$ = "Q" THEN RETURN
    LOOP WHILE S$ <> "P" AND S$ <> "D"
    IF S$ = "D" THEN INPUT "Enter the complete disk filename. ", Outfile$
    OPEN Outfile$ FOR OUTPUT AS #1
    PRINT "Which of the "; Period; "alphabets do you want to match?"
    PRINT
    DO
        INPUT "    Enter number of 1st alphabet to be matched: ", ALF1
        INPUT "    Enter number of 2nd alphabet to be matched: ", ALF2
        PRINT "MATCHING ALPHABET"; ALF1; "AND ALPHABET"; ALF2
        PRINT #1, "MATCHING ALPHABET"; ALF1; "AND ALPHABET"; ALF2
        FOR N = 1 TO 26
            IF CMIXflag = 1 THEN SET1(N) = PMIXFREQ(ALF1, N) ELSE SET1(N) = PFREQ(ALF1, N)
            IF CMIXflag = 1 THEN SET2(N) = PMIXFREQ(ALF2, N) ELSE SET2(N) = PFREQ(ALF2, N)
        NEXT N
        FOR M = 1 TO 26
            FOR L = 1 TO 26
                PRINT #1, " "; MID$(CCOMPO$, L, 1); ' Print first sequence
            NEXT L
            PRINT #1,
            FOR L = 1 TO 26
                PRINT #1, USING "###"; SET1(L); ' Print first sequence frequencies
            NEXT L
            PRINT #1,
            FOR L = 0 TO 25
                LTRPOS = M + L: IF LTRPOS > 26 THEN LTRPOS = LTRPOS - 26
                PRINT #1, " "; MID$(CCOMPO$, LTRPOS, 1); ' Print second sequence
            NEXT L
            PRINT #1,
            MATCH(M) = 0
            FOR N = 1 TO 26
                MATCH(M) = MATCH(M) + (SET1(N) * SET(N))
                PRINT #1, USING "###"; SET2(N); ' Print second sequence frequencies
            NEXT N
            PRINT #1,
            IF M / 2 - INT(M / 2) <> 0 THEN PRINT TAB(1); "MATCH"; M; ":"; MATCH(M); ELSE PRINT TAB(40); "MATCH"; M; ":"; MATCH(M):
            PRINT #1, "    MATCH"; M; ":"; MATCH(M): PRINT #1,
            SET2(27) = SET2(1)
            FOR N = 1 TO 26
                SET2(N) = SET2(N + 1)
            NEXT N
        NEXT M
        IF Outfile$ = Printer$ THEN PRINT #1, Formfeed$
        INPUT "ANOTHER MATCH (Y/N)?", Q$
    LOOP WHILE UCASE$(Q$) = "Y"
    IF Outfile$ = Printer$ THEN PRINT #1, Formfeed$
    CLOSE #1
    RETURN
END SUB

SUB ChoiceError
    PRINT "Choice out of range. Hit a key.";
    AnyKey
END SUB

SUB CipherTxtDisk
    ' *** Ciphertext Disk Entry ***
    INPUT "Enter input filename, for example, A:SAMPLE.TXT ", INFILE$
    OPEN INFILE$ FOR INPUT AS #1
    NRlines = 0
    DO '2100
        NRlines = NRlines + 1
        LINE INPUT #1, CTEXTD$(NRlines)
    LOOP UNTIL EOF(1) OR NRlines = MAXLINES 'GOTO2100
    CLOSE #1
    Status$ = "    (CIPHERTEXT ENTERED)"
    CipherTxtPrep
END SUB

SUB CipherTxtKbd
    ' *** Ciphertext Keyboard Entry ***
    PRINT "Type a line of text. Use CAPITAL letters only."
    PRINT "When you are through, type END on a new line."
    NRlines = 0
    DO '2500
        INPUT T$
        NRlines = NRlines + 1
        CTEXTD$(NRlines) = T$
    LOOP UNTIL UCASE$(T$) = "END" OR NRlines = MAXLINES 'GOTO 2500
    Status$(1) = " (CIPHERTEXT ENTERED)"
    CipherTxtPrep
END SUB

SUB CipherTxtPrep
    ' *** Preps Ciphertext in Internal Format ***
    FOR TEXTLINE = 1 TO NRlines
        T$ = CTEXTD$(TEXTLINE)
        POSN = 0
        DO
            POSN = POSN + 1
            IF POSN > LEN(T$) THEN EXIT DO
            C$ = MID$(T$, POSN, 1)
            IF (C$ < "A" OR C$ > "Z") AND C$ <> "." THEN StripCipherTxt
        LOOP
        CTEXTI$(TEXTLINE) = T$
    NEXT TEXTLINE
END SUB

SUB DecipherSub
    ' *** Decipherment Subroutine ***
    ' There has to be a better way to do this and eliminate GOTO3780.
    ' Leaving it in for now.
    AlphaEntryMenu
    CYCLEPOS = 0
    FOR LNE = 1 TO NRlines
        PTEXTD$(LNE) = ""
    NEXT LNE
    FOR LNE = 1 TO NRlines
        FOR CHARPOS = 1 TO LEN(CTEXTD$(LNE))
            CCHAR$ = MID$(CTEXTD$(LNE), CHARPOS, 1)
            IF CCHAR$ = " " THEN
                PCHAR$ = " "
                GOTO 3780
            END IF
            CYCLEPOS = CYCLEPOS + 1
            IF CYCLEPOS > Period THEN CYCLEPOS = 1
            IF ASC(CCHAR$) > 96 AND ASC(CCHAR$) < 123 THEN
                CCHAR$ = CHR$(ASC(CCHAR$) - 32)
            END IF
            IF ASC(CCHAR$) < 65 OR ASC(CCHAR$) > 96 THEN CCHAR$ = "."
            IF CCHAR$ = "." THEN
                PCHAR$ = "."
                GOTO 3780
            END IF
            FOR ALPHCHAR = 1 TO 26
                IF CCHAR$ = MID$(CCOMP$(CYCLEPOS), ALPHCHAR, 1) THEN
                    PCHAR$ = MID$(PCOMP$, ALPHCHAR, 1)
                    GOTO 3780
                END IF
            NEXT ALPHCHAR
            PCHAR$ = "."
            3780 'fix me
            PTEXTD$(LNE) = PTEXTD$(LNE) + PCHAR$
        NEXT CHARPOS
    NEXT LNE
    CipherTxtPrep
    Status$(3) = "    (DECIPHERMENT COMPLETED)"
END SUB

SUB DigraphFreqCnt
    ' *** Digraphic Frequency and IC ***
    FOR LNE = 1 TO NRlines
        IF (LEN(CTEXTI$(LNE)) / 2 - INT(LEN(CTEXTI$(LNE)) / 2)) <> 0 THEN
            CARRY$ = RIGHT$(CTEXTI$(LNE), 1)
            CTEXTI$(LNE) = LEFT$(CTEXTI$(LNE), LEN(CTEXTI$(LNE)) - 1)
            CTEXTI$(LNE + 1) = CARRY$ + CTEXTI$(LNE + 1)
        END IF
    NEXT LNE
    FOR LNE = 1 TO NRlines
        FOR DIG = 1 TO INT(LEN(CTEXTI$(LNE)) / 2)
            LTR1 = ASC(MID$(CTEXTI$(LNE), DIG * 2 - 1, 1)) - 64
            LTR2 = ASC(MID$(CTEXTI$(LNE), DIG * 2, 1)) - 64
            IF LTR1 <> -18 AND LTR2 <> -18 THEN
                DIFREQ(LTR1, LTR2) = DIFREQ(LTR1, LTR2) + 1
            END IF
        NEXT DIG
    NEXT LNE
    FOR ROW = 1 TO 26
        FOR COLUMN = 1 TO 26
            TOTDIG = TOTDIG + DIFREQ(ROW, COLUMN)
            DIPHISUM = DIPHISUM + (DIFREQ(ROW, COLUMN) * (DIFREQ(ROW, COLUMN) - 1))
        NEXT COLUMN
    NEXT ROW
    PHIDIG = 676 * DIPHISUM / (TOTDIG * (TOTDIG - 1))
    Dflag = 1: STAT$(2) = " (COMPLETED)"
    Status$(6) = "  (COMPLETED)"
END SUB

SUB EncipherSub
    ' *** Encipherment Subroutine ***
    ' There has to be a better way to do this and eliminate GOTO 3320.
    ' Leaving it in for now.
    AlphaEntryMenu
    CYCLEPOS = 0
    FOR LNE = 1 TO NRlines
        CTEXTD$(LNE) = " "
        KTEXTD$(LNE) = " "
    NEXT LNE
    FOR LNE = 1 TO NRlines
        FOR CHARPOS = 1 TO LEN(PTEXTD$(LNE))
            PCHAR$ = MID$(PTEXTD$(LNE), CHARPOS, 1)
            IF PCHAR$ = " " THEN
                CCHAR$ = ""
                KCHAR$ = " "
                GOTO 3320
            END IF
            CYCLEPOS = CYCLEPOS + 1
            IF CYCLEPOS > Period THEN CYCLEPOS = 1
            KCHAR$ = MID$(RepeatKey$, CYCLEPOS, 1)
            IF ASC(PCHAR$) > 64 AND ASC(PCHAR$) < 91 THEN
                PCHAR$ = CHR$(ASC(PCHAR$) + 32)
            END IF
            IF ASC(PCHAR$) < 97 OR ASC(PCHAR$) > 122 THEN PCHAR$ = "."
            IF PCHAR$ = "." THEN
                CCHAR$ = "."
                GOTO 3320
            END IF
            FOR ALPHCHAR = 1 TO 26
                IF PCHAR$ = MID$(PCOMP$, ALPHCHAR, 1) THEN
                    CCHAR$ = MID$(CCOMP$(CYCLEPOS), ALPHCHAR, 1)
                    GOTO 3320
                END IF
            NEXT ALPHCHAR
            CCHAR$ = "."
            3320 'fix me
            CTEXTD$(LNE) = CTEXTD$(LNE) + CCHAR$
            KTEXTD$(LNE) = KTEXTD$(LNE) + KCHAR$
        NEXT CHARPOS
    NEXT LNE
    CipherTxtPrep
    Status$(2) = " (ENCIPHEREMENT COMPLETED)"
END SUB

SUB FindRepeats
    ' *** Subroutine to Find Repeats ***
    WHILE RptLen < 2
        INPUT "What is the shortest length repeat you want listed?", RptLen
    WEND
    Outfile$ = Printer$
    OPEN Outfile$ FOR OUTPUT AS #1
    FOR TLINE = 1 TO NRlines - 1
        FOR ALTR = 1 TO LEN(CTEXTI$(TLINE))
            IF TLINE <> NRlines THEN
                CT$ = CTEXTI$(TLINE) + CTEXTI$(TLINE + 1)
            ELSE
                CT$ = CTEXTI$(TLINE)
            END IF
            A$ = MID$(CT$, ALTR, RptLen)
            FOR BLTR = ALTR + 2 TO LEN(CTEXTI$(TLINE)) + 2
                BLINE = TLINE
                CTB$ = CT$
                IF BLTR <= LEN(CTEXTI$(TLINE)) THEN
                    B$ = MID$(CTB$, BLTR, RptLen)
                    IF A$ = B$ THEN RepeatLenCheck
                END IF
            NEXT BLTR
            IF TLINE <> NRlines THEN
                FOR BLINE = TLINE + 1 TO NRlines
                    IF BLINE <> NRlines THEN
                        CTB$ = CTEXTI$(BLINE) + CTEXTI$(BLINE + 1)
                    ELSE
                        CTB$ = CTEXTI$(BLINE)
                    END IF
                    FOR BLTR = 1 TO LEN(CTEXTI$(BLINE))
                        B$ = MID$(CTB$, BLTR, RptLen)
                        IF A$ = B$ THEN RepeatLenCheck
                    NEXT BLTR
                NEXT BLINE
            END IF
        NEXT ALTR
    NEXT TLINE
    PRINT #1, Formfeed$, Formfeed$
    CLOSE #1
END SUB

SUB FreqCntMenu
    ' *** Frequency Count, IC Subroutine ***
    DO
        CLS
        PRINT "Select the routine you want to run:"
        PRINT: PRINT
        PRINT "    1. Monographic frequencies and ICs" + STAT$(1)
        PRINT "    2. Digraphic frequencies and ICs" + STAT$(2)
        PRINT "    3. Periodic frequencies and ICs" + STAT$(3)
        PRINT "    4. Chi test" + STAT$(4)
        PRINT "    5. RETURN TO MAIN MENU"
        PRINT "        Your choice: "
        Choice = ASC(OneKey$) - 48
        SELECT CASE Choice
            CASE 1
                MonoFreqCnt 'GOSUB 7120
            CASE 2
                DigraphFreqCnt 'GOSUB 7440
            CASE 3
                PeriodFreqCnt 'GOSUB 7900
            CASE 4
                ChiFreqCnt 'GOSUB 11120
            CASE 5
                EXIT SUB
            CASE ELSE
                ChoiceError
        END SELECT
    LOOP UNTIL Choice = 5
END SUB

SUB MonoAlphaUni
    ' *** Monoalphabetic Alphabet Entry Subroutine ***
    DO
        CLS
        PLFAG = 0
        CIFLAG = 0
        DONEFLAG = 0
        PRINT TAB(5); "Present alphabet is--": PRINT
        PRINT TAB(10); "P: ";
        FOR N = 1 TO 26
            PRINT MID$(PCOMP$, N, 1); " ";
        NEXT N
        PRINT TAB(10); "C: ";
        FOR N = 1 TO 26
            PRINT MID$(CCOMPOS$, N, 1); " ";
        NEXT N
        PRINT
        PRINT
        PRINT TAB(20); "1. Change plain component"
        PRINT TAB(20); "2. Change cipher component"
        PRINT TAB(20); "3. Change specific key"
        PRINT TAB(20); "4. Accept alphabet as shown"
        PRINT
        PRINT TAB(18); "Enter your choice:  ";
        Choice = ASC(OneKey$) - 48
        SELECT CASE Choice
            CASE 1
                ROW = 3
                COLUMN = 11
                PLFAG = 1
                ReadComponent
                PCOMP$ = COMP$
            CASE 2
                ROW = 4
                COLUMN = 11
                CIFLAG = 1
                ReadComponent
                CCOMPO$ = COMP$
            CASE 3
                DO
                    LOCATE 11, 10: X = SCREEN(3, 13)
                    PRINT "Type the specific key: "; CHR$(X - 32); "    of plaintext = ? of ciphertext."
                    LOCATE 11, 50, 1
                    DO
                        X$ = INKEY$
                    LOOP UNTIL X$ <> ""
                    IF ASC(X$) > 96 AND ASC(X$) < 123 THEN X$ = CHR$(ASC(X$) - 32)
                    FOR N = 1 TO 26
                        IF X$ = MID$(CCOMPO$, N, 1) THEN
                            TCOMP$ = RIGHT$(CCOMPO$, 27 - N) + LEFT$(CCOMPO$, N - 1)
                            CCOMPO$ = TCOMP$
                            EXIT DO
                        END IF
                    NEXT N
                    PRINT "CHARACTER NOT FOUND IN CIPHER COMPONENT"
                LOOP
            CASE 4
                DONEFLAG = 1
            CASE ELSE
                ChoiceError
        END SELECT
    LOOP UNTIL DONEFLAG = 1
    CCOMP$(1) = CCOMPO$
END SUB

SUB MonoFreqCnt
    ' *** Monographic Frequency and IC Subroutine ***
    FOR LNE = 1 TO NRlines
        FOR CHARPOS = 1 TO LEN(CTEXTI$(LNE))
            NXTLTR$ = MID$(CTEXTI$(LNE), CHARPOS, 1)
            Z = ASC(NXTLTR$) - 64
            MFREQ(Z) = MFREQ(Z) + 1
        NEXT CHARPOS
    NEXT LNE
    FOR Z = 1 TO 26
        TOTLTRS = TOTLTRS + MFREQ(Z)
        PHISUM = PHISUM + (MFREQ(Z) * (MFREQ(Z) - 1))
    NEXT Z
    PHIMONO = 26 * PHISUM / (TOTLTRS * (TOTLTRS - 1))
    Mflag = 1
    STAT$(1) = " (COMPLETED)"
    Status$(6) = "  (COMPLETED)"
END SUB

FUNCTION OneKey$
    OneKey$ = ""
    DO
        OneKey$ = INKEY$
    LOOP WHILE OneKey$ = ""
END FUNCTION

SUB PeriodFreqCnt
    ' *** Periodic Frequency, IC Subroute ***
    CYCLEPOS = 0
    INPUT "What period do you want to use? ", Period
    FOR N = 1 TO Period
        FOR M = 1 TO 26
            PFREQ(N, M) = 0
        NEXT M
        PERPHISUM(N) = 0: PERTOTLTR(N) = 0
    NEXT N
    FOR N = 1 TO NRlines
        FOR M = 1 TO LEN(CTEXTI$(N))
            CYCLEPOS = CYCLEPOS + 1
            IF CYCLEPOS > Period THEN CYCLEPOS = 1
            NXTCHAR$ = MID$(CTEXTI$(N), M, 1)
            Z = ASC(NXTCHAR$) - 64
            IF Z = -18 THEN Z = 27
            PFREQ(CYCLEPOS, Z) = PFREQ(CYCLEPOS, Z) + 1
        NEXT M
    NEXT N
    FOR M = 1 TO Period
        FOR N = 1 TO 26
            PERTOTLTR(M) = PERTOTLTR(M) + PFREQ(M, N)
            PERPHISUM(M) = PERPHISUM(M) + (PFREQ(M, N) * (PFREQ(M, N) - 1))
        NEXT N
        PHIPERI(M) = 26 * PERPHISUM(M) / (PERTOTLTR(M) * (PERTOTLTR(M) - 1))
    NEXT M
    Pflag = 1
    STAT$(3) = " (COMPLETED)"
    Status$(6) = " (COMPLETED)"
    ' Skips mixed alphabet routine if std sequence
    IF CMIXflag = 0 THEN
        AnyKey
        EXIT SUB
    END IF
    FOR M = 1 TO Period
        FOR N = 1 TO 26
            PMIXFREQ(M, N) = PFREQ(M, ASC(MID$(CCOMPO$, N, 1)) - 64)
        NEXT N
    NEXT M
END SUB

SUB PeriodPolyAlpha
    ' *** Periodic and Aperiodic Alphabet Entry Subroutine ***
    DO
        CLS
        DONEFLAG = 0
        PLFLAG = 0
        CIFLAG = 0
        PRINT TAB(5); "Plain component is--"
        PRINT TAB(10); "P:  "
        FOR N = 1 TO 26
            PRINT MID$(PCOMP$, N, 1); " ";
        NEXT N
        PRINT
        PRINT TAB(5); "Cipher component is--"
        PRINT TAB(10); "C:  ";
        FOR N = 1 TO 26
            PRINT MID$(CCOMPO$, N, 1); "  ";
        NEXT N
        PRINT
        PRINT
        IF Aflag = 0 THEN
            PRINT TAB(5); "Length of pERIOD IS:  "; Period
        ELSE
            PRINT TAB(5); "Length of key is:  "; Period
        END IF
        X = SCREEN(2, 13)
        IF Aflag = 0 THEN RepeatKey$ = LEFT$(Rkey$, Period)
        IF Aflag = 0 THEN PRINT TAB(5); "Repeating key is  "; CHR$(X - 32); " of plaintext = "; RepeatKey$: PRINT ELSE PRINT TAB(5); "Long running key is: "; RepeatKey$: PRINT
        PRINT
        PRINT
        PRINT TAB(20); "1. Change plain component"
        PRINT TAB(20); "2. Change cipher component"
        IF Aflag = 0 THEN
            PRINT TAB(20); "3. Change repeating key"
        ELSE
            PRINT TAB(20); "3. Generate long running key"
        END IF
        IF Aflag = 0 THEN
            PRINT TAB(20); "4. Show complete matrix"
        ELSE
            PRINT TAB(20); "4. Accept alphabets"
        END IF
        PRINT
        PRINT TAB(18); "Enter your choice: ";
        INPUT Choice
        SELECT CASE Choice
            CASE 1
                ROW = 2
                COLUMN = 11
                PLFLAG = 1
                ReadComponent
                PCOMP$ = COMP$
            CASE 2
                ROW = 4
                COLUMN = 11
                CIFLAG = 1
                CMIXflag = 1
                ReadComponent
                CCOMPO$ = COMP$
            CASE 3
                IF Aflag = 1 THEN
                    AperiodKeyGen
                ELSE
                    LOCATE 7, 39
                    INPUT Rkey$
                    Period = LEN(Rkey$)
                END IF
                FOR N = 1 TO Period
                    FOR P = 1 TO 26
                        IF MID$(Rkey$, N, 1) = MID$(CCOMPO$, P, 1) THEN EXIT FOR
                    NEXT P
                    CCOMP$(N) = RIGHT$(CCOMPO$, 27 - P) + LEFT$(CCOMPO$, P - 1)
                NEXT N
            CASE 4
                CLS
                IF Aflag = 1 THEN
                    DONEFLAG = 1
                    EXIT DO
                END IF
                PRINT TAB(9); "P: ";
                FOR N = 1 TO 26
                    PRINT MID$(PCOMP$, N, 1); "  ";
                NEXT N
                PRINT
                PRINT TAB(13); "---------------------------------------------------"
                FOR P = 1 TO Period
                    PRINT TAB(9); "C"; CHR$(48 + P); ":  ";
                    FOR N = 1 TO 26
                        PRINT MID$(CCOMP$(P), N, 1); "  ";
                    NEXT N
                    PRINT
                NEXT P
                PRINT TAB(20); "1. Change matrix"
                PRINT TAB(20); "2. Accept matrix"
                INPUT "           Enter your choice:  "; Choice
                IF Choice <> 1 THEN
                    DONEFLAG = 1
                END IF
            CASE ELSE
                ChoiceError
        END SELECT
    LOOP UNTIL DONEFLAG = 1
    EXIT SUB
END SUB

SUB PlainTxtDisk
    ' *** Plaintext Disk Entry ***
    INPUT "Enter input filename, for example, P;:SAMPLE.TXT ", INFILE$
    OPEN INFILE$ FOR INPUT AS #1
    NRlines = 0
    DO '1920
        NRlines = NRlines + 1
        LINE INPUT #1, PTEXTD$(NRlines)
    LOOP UNTIL EOF(1) OR NRlines = MAXLINES 'GOTO 1920
    Status$(1) = " (PLAINTEXT ENTERED)"
    CLOSE #1
END SUB

SUB PlainTxtKbd
    ' *** Plaintext Keyboard Entry ***
    PRINT "Type a line of text. Use lower case letters only."
    PRINT "Use no commas in the text. When you are through,"
    PRINT "type END on a new line."
    NRlines = 0
    DO '2300
        LINE INPUT T$
        NRlines = NRlines + 1
        PTEXTD$(NRlines) = T$
    LOOP UNTIL UCASE$(T$) = "END" OR NRlines = MAXLINES 'GOTO 2300
    Status$(1) = "   (PLAINTEXT ENTERED)"
END SUB

SUB PrintDigrStat
    ' *** Print Digraphic Stats ***
    PRINT #1,
    PRINT #1,
    PRINT #1, " "; ALPH$
    FOR N = 1 TO 26
        PRINT #1, CHR$(N + 64);
        FOR M = 1 TO 26
            PRINT #1, USING "###"; DIFREQ(N, M);
        NEXT M
        PRINT #1,
    NEXT N
    PRINT #1,
    PRINT #1,
    PRINT #1, "TOTAL DIGRAPHS ="; TOTDIG; " DIGRAPHIC IC="; PHIDIG
END SUB

SUB PrintFreqCnt
    ' *** Mixed Alphabet Periodic Stat Print ***
    ALPH$ = " A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
    CLS
    Outfile$ = Printer$
    SaveText
    IF Mflag = 1 THEN
        PrintMonoStat1
    END IF
    IF Dflag = 1 THEN
        PRINT #1, Formfeed$
        PrintDigrStat
    END IF
    IF Pflag = 1 THEN
        PRINT #1, Formfeed$
        PrintMonoStat2
    END IF
    IF CMIXflag = 1 THEN
        PRINT #1, Formfeed$
        PrintMonoStat3
    END IF
    PRINT #1, Formfeed$
    PRINT #1, Formfeed$
    CLOSE #1
END SUB

SUB PrintMonoStat1
    ' *** Print Monographic Stats ***
    PRINT #1,
    PRINT #1,
    PRINT #1, ALPH$
    FOR N = 1 TO 26
        PRINT #1, USING "###"; MFREQ(N);
    NEXT N
    PRINT #1,
    PRINT #1,
    PRINT #1, "TOTAL LETTERS ="; TOTLTRS; " MONOGRAPHIC IC ="; PHIMONO
END SUB

SUB PrintMonoStat2
    ' *** Print Monographic Stats ***
    PRINT #1,
    PRINT #1,
    FOR N = 1 TO Period
        PRINT #1, ALPH$
        FOR M = 1 TO 26
            PRINT #1, USING "###"; PFREQ(N, M);
        NEXT M
        PRINT #1,
        PRINT #1, "TOTAL LETTERS ="; PERTOTLTR(N); "    IC="; PHIPERI(N)
        PRINT #1,
        PRINT #1,
    NEXT N
END SUB

SUB PrintMonoStat3
    PRINT #1,
    PRINT #1,
    FOR M = 1 TO Period
        ALPHMIX$(M) = " "
        FOR N = 1 TO 26
            ALPHMIX$(M) = ALPHMIX$(M) + " " + MID$(CCOMPO$, N, 1)
        NEXT N
    NEXT M
    FOR M = 1 TO Period
        PRINT #1, ALPHMIX$(M)
        FOR N = 1 TO 26
            PRINT #1, USING "###"; PMIXFREQ(M, N);
        NEXT N
        PRINT #1,
        PRINT #1, "TOTAL LETTERS ="; PERTOTLTR(M); "    IC ="; PHIPERI(M)
        PRINT #1,
        PRINT #1,
    NEXT M
END SUB

SUB PrintText
    ' *** Text Print Subroutine ***
    CLS
    PRINT "IS PRINTER READY (Y/N)? "
    X$ = OneKey$
    IF UCASE$(X$) = "N" THEN EXIT SUB
    Outfile$ = Printer$
    SaveText
    PRINT #1, Formfeed$; Formfeed$
    CLOSE #1
    Status$(4) = "   (TEXT PRINTED)"
    IF Printer$ <> "CON" THEN EXIT SUB
    PRINT "PRESS ANY KEY TO CONTINUE";
    AnyKey
END SUB

SUB QBmain
    ' *** Main Menu ***
    DO
        CLS
        PRINT "     CRYPTANALYSIS SUPPORT PROGRAM"
        PRINT
        PRINT
        PRINT "     1. Enter text "; Status$(1)
        PRINT "     2. Encipher text "; Status$(2)
        PRINT "     3. Decipher text "; Status$(3)
        PRINT "     4. Print text "; Status$(4)
        PRINT "     5. Save text to disk "; Status$(5)
        PRINT "     6. Calculate frequency counts, ICs "; Status$(6)
        PRINT "     7. Print frequency counts, ICs "; Status$(7)
        PRINT "     8. Save frequency counts, ICs to disk "; Status$(8)
        PRINT "     9. Find repeats "; Status$(9)
        PRINT "     0. Quit"
        PRINT
        PRINT

        ' *** Main Menu Control ***
        PRINT "Enter your choice: "
        Selection = ASC(OneKey$) - 48
        SELECT CASE Selection
            CASE 1
                TextEntryMenu 'GOSUB 1600
            CASE 2
                EncipherSub 'GOSUB 3000
            CASE 3
                DecipherSub 'GOSUB 3480
            CASE 4
                PrintText 'GOSUB 6080
            CASE 5
                SaveText 'GOSUB 6380
            CASE 6
                FreqCntMenu 'GOSUB 6840
            CASE 7
                PrintFreqCnt 'GOSUB 8600
            CASE 8
                SaveFreqCnt 'GOSUB 9960
            CASE 9
                FindRepeats 'GOSUB 10240
            CASE 0
                '*** Quit Subroutine ***
                DO
                    CLS
                    PRINT "Are you sure you want to quit (Y/N)? "
                    Choice$ = OneKey$
                    IF UCASE$(Choice$) <> "Y" THEN
                        Selection = -1
                    ELSE
                        Selection = 11
                        KEY ON ' restores bottom of screen prompts
                        EXIT DO
                    END IF
                LOOP WHILE UCASE$(Choice$) <> "N"
            CASE ELSE
                ChoiceError
        END SELECT
    LOOP UNTIL Selection = 11
END SUB

SUB ReadComponent
    ' *** Reads in Edited Plain or Cipher Component From Screen ***
    DO
        exitflag1 = 0
        LOCATE ROW, COLUMN
        INPUT DUMMY$ ' DUMMY$ is not used as text is read from screen
        COMP$ = " "
        FOR N = 13 TO 63 STEP 2
            X = SCREEN(ROW, N)
            COMP$ = COMP$ + CHR$(X)
            IF PLFLAG = 1 AND (X < 96 OR X > 122) AND X <> 46 THEN
                BEEP
                Repeatflag = True
                EXIT FOR
            END IF
            IF CIFLAG = 1 AND (X < 65 OR X > 90) THEN
                BEEP
                Repeatflag = True
                EXIT FOR
            END IF
        NEXT N
    LOOP UNTIL exitflag1 = False
END SUB

SUB RepeatLenCheck
    ' *** Subroutine to Check Length of Repeat and Print It ***
    LONGER = RptLen
    PRINT A$
    DO
        LONGER = LONGER + 1
    LOOP WHILE MID$(CT$, ALTR, LONGER) = MID$(CTB$, BLTR, LONGER) ' Try it longer
    LONGER = LONGER - 1 ' Nope, too long
    PRINT #1, MID$(CT$, ALTR, LONGER); " AT LINE"; TLINE; ", LETTER"; ALTR; " AND AT LINE"; BLINE; ", LETTER"; BLTR
END SUB

SUB SaveFreqCnt
    ' *** Statistics Save to Disk Subroutine ***
    ALPH$ = " A B C D E F G H I J K L M O P Q R S T U V W X Y Z"
    CLS
    PRINT "Enter the complete disk filename for the saved statistics, for example,"
    INPUT "A:MYSTAT.TXT "; Outfile$
    FILEflag = 1
    SaveText
    IF Mflag = 1 THEN PrintMonoStat1
    IF Dflag = 1 THEN PrintDigrStat
    IF Pflag = 1 THEN PrintMonoStat2
    IF CMIXflag = 1 THEN PrintMonoStat3
    CLOSE #1
END SUB

SUB SaveText
    ' *** Text Save to Disk Subroutine ***
    IF Outfile$ <> Printer$ THEN
        CLS
        PRINT "Enter complete disk filename for the save text, for example,"
        INPUT "A:MYSAVE.TXT "; Outfile$
    END IF
    OPEN Outfile$ FOR OUTPUT AS #1
    TEXTCOUNT = 0
    FOR N = 1 TO NRlines
        PRINT #1, PTEXTD$(N)
        PRINT #1, CTEXTD$(N)
        PRINT #1, KTEXTD$(N)
        TEXTCOUNT = TEXTCOUNT + LEN(CTEXTI$(N))
        PRINT #1,
    NEXT N
    IF Period <= 20 THEN
        PRINT #1, PCOMP$
        FOR N = 1 TO Period
            PRINT #1, CCOMP$(N)
        NEXT N
    END IF
    IF Outfile$ = Printer$ OR FILEflag = 1 THEN EXIT SUB
    CLOSE #1
    IF Outfile$ <> Printer$ THEN Status$(5) = "   (TEXT SAVED)"
END SUB

SUB StripCipherTxt
    ' *** Subroutine to Strip Nonliteral Characters From Ciphertext ***
    T$ = MID$(T$, 1, POSN - 1) + MID$(T$, POSN + 1, LEN(T$) - POSN)
    POSN = POSN - 1
END SUB

SUB TextEntryMenu
    ' *** Text Entry Subroutine ***
    DO
        CLS
        PRINT "     TEXT ENTRY MENU"
        PRINT
        PRINT
        PRINT
        PRINT " 1. Enter plaintext from disk "
        PRINT " 2. Enter ciphertext from disk "
        PRINT " 3. Enter plaintext from keyboard "
        PRINT " 4. Enter ciphertext from keyboard "
        PRINT " 5. Return to Main Menu "
        PRINT
        PRINT
        PRINT "Enter your choice: "
        Choice = ASC(OneKey$) - 48
        SELECT CASE Choice
            CASE 1
                PlainTxtDisk 'GOTO 1860
                EXIT SUB
            CASE 2
                CipherTxtDisk 'GOTO 2040
                EXIT SUB
            CASE 3
                PlainTxtKbd 'GOTO 2220
                EXIT SUB
            CASE 4
                CipherTxtKbd 'GOTO 2440
                EXIT SUB
            CASE 5
                EXIT SUB
            CASE ELSE
                ChoiceError
        END SELECT
    LOOP UNTIL Choice = 5
END SUB


