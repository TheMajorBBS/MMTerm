DECLARE SUB dialit (h%, v%)
DECLARE SUB findit (what$, found$, sw%, which%)
DECLARE SUB soundfx (sw%)
DECLARE SUB mousepointer (OnOff%)
DECLARE SUB mouseget (mouseb%)
DECLARE SUB emul (st$)
DECLARE SUB parse (temp$, LoopStr$, send$, look$, go$)
DECLARE SUB ermes (tp$, sw%)
DECLARE SUB dowindo (num%, cl1%, cl2%)
DECLARE SUB Nocary ()
DECLARE SUB PutText2 (text$, TextColor%, BackColor%)
DECLARE SUB edittext (rw%, clm%, which%, min%, max%)
DECLARE SUB tinyfont (hor%, ver%, inline$, CLR%)
DECLARE SUB cubeit (h%, v%, WD%, HT%, C1%, C2%, FILD%)
DECLARE SUB icon (h%, v%, WD%, HT%, C1%, C2%, FILD%, OUTIN%)
DECLARE SUB smallprint (spx%, spy%, sp$, spcolor%)
DECLARE FUNCTION DIR$ (FileSpec$)
DECLARE FUNCTION Exists% (filen$)
DECLARE FUNCTION BufIn$ (filen$, done%, reading%)
DECLARE FUNCTION DRAWSTR% (mode%, strg$, x0%, y0%, fg%, bg%, gap%)
DECLARE FUNCTION Iinkey$ (max%, row%, col%, fore%, sw%)
DECLARE SUB transmit CDECL ALIAS "_transmit_string" (addr$)
DECLARE FUNCTION DataWaiting% CDECL ALIAS "_data_waiting" ()
DECLARE FUNCTION ReadChar% CDECL ALIAS "_readchar" ()
DECLARE FUNCTION CarrierLost% CDECL ALIAS "_carrier_state" ()
DECLARE SUB DTRControl CDECL ALIAS "_dtr" (BYVAL OnOff%)
DECLARE SUB ClearInputBuffer CDECL ALIAS "_clear_input_buffer" ()
DECLARE FUNCTION ival% (number$)

DEFINT A-Z
'$DYNAMIC
COMMON SHARED nomouse%, mouseh%, mouseV%
COMMON SHARED uinput$(), macro$(), snd%, MAXLINE, line$, autobash, bash$
DIM SHARED keyin$, macon

DATA carrion,filthbug,acid,rat,kobold,lashworm
DATA thug,orc,cultist,zombie

REM $STATIC
SUB domacro (scrollback$(), keyin$, macro$(), recon, line$) STATIC
IF CarrierLost THEN EXIT SUB
b$ = keyin$
IF keyin$ <> "G" THEN dowindo 1, 8, 15
mousepointer 0
temp$ = INKEY$
temp$ = "0. " + CHR$(34) + "#$%&21G"
y = INSTR(temp$, RIGHT$(keyin$, 1))

IF y > 0 THEN                 'if huge or editable
  IF y = 19 THEN y = 37
  IF y = 20 THEN y = 38
  temp$ = macro$(y)
  IF temp$ <> "" THEN GOSUB execute
END IF
IF b$ <> "G" THEN dowindo 1, 15, 8 ELSE mousepointer 1
EXIT SUB
'----------------------------------------------
execute:
LoopStr$ = temp$
part2:
sw = 0: a$ = INKEY$
DO
  IF sw = 0 THEN
    parse temp$, LoopStr$, send$, look$, go$
    IF send$ <> "" THEN transmit send$: sw = 1
    IF look$ = "" THEN sw = 0
  END IF
  IF CarrierLost% THEN Nocary
  IF keyin$ = CHR$(27) THEN EXIT DO
  keyin$ = INKEY$
  DO WHILE DataWaiting
    modemin$ = CHR$(ReadChar)
    emul modemin$
    IF recon = 2 THEN       'if doing black box
      IF LEFT$(modemin$, 1) = CHR$(10) THEN
        IF w$ <> "" THEN PRINT #7, w$
        PRINT #7, modemin$
        w$ = ""
      ELSE
        w$ = w$ + modemin$
      END IF
    END IF
    IF RIGHT$(line$, LEN(look$)) = look$ THEN sw = 0
  LOOP
LOOP UNTIL sw = 0 OR keyin$ = CHR$(27)
IF keyin$ <> CHR$(27) AND temp$ <> "" THEN GOTO part2
RETURN
END SUB

SUB loadmac (monsize, regit)
IF regit = 0 THEN
  monsize = 19
  REDIM uinput$(0 TO monsize)
  FOR y = 10 TO monsize
    READ uinput$(y)
    uinput$(y) = UCASE$(uinput$(y))
  NEXT y
ELSE
  IF Exists("MMTmonst.dat") THEN
    filenum = FREEFILE
    OPEN "I", filenum, "MMTmonst.dat"
    LINE INPUT #filenum, inline$
    monsize = ival%(RIGHT$(inline$, 2)) + 9
    REDIM uinput$(0 TO monsize)
    FOR y = 10 TO monsize
      LINE INPUT #filenum, inline$
      uinput$(y) = UCASE$(inline$)
    NEXT y
    CLOSE filenum
  ELSE
    PRINT "Sorry, you are missing MMTMONST.DAT file. Program aborted": END
  END IF
END IF
REDIM macro$(1 TO 38)
y = 1
IF Exists("MMTmacro.fil") THEN
  filenum = FREEFILE
  OPEN "I", filenum, "MMTmacro.fil"
  IF LOF(filenum) <> 0 THEN
   DO
    LINE INPUT #filenum, inline$
    IF LEFT$(inline$, 1) = "'" THEN
      IF LEN(inline$) > 73 THEN inline$ = LEFT$(inline$, 73)
      macro$(y + 18) = MID$(inline$, 2)
      y = y - 1
    ELSE
      macro$(y) = inline$
      IF LEN(inline$) > 72 THEN macro$(y) = LEFT$(macro$(y), 72)
    END IF
    y = y + 1
    IF y > 18 THEN EXIT DO
   LOOP UNTIL EOF(filenum)
  END IF
  CLOSE filenum
END IF
END SUB

SUB MacroScreen (menusav())
row% = CSRLIN: col% = POS(0)
dowindo 1, 8, 15
mousepointer 0
GET (0, 0)-(639, 200), menusav
cubeit 0, 0, 640, 200, 15, 8, 7
icon 7, 6, 627, 188, 8, 15, 7, 0
GOSUB script
'
DO
  clm = 0
  DO
    choice$ = UCASE$(INKEY$)
    mouseget mouseb
    IF mouseb = 2 THEN choice$ = CHR$(27)
  LOOP UNTIL choice$ <> ""
  IF choice$ = CHR$(13) OR choice$ = CHR$(27) THEN EXIT DO
  '
  nz = INSTR("ABCDEGHIJKLMNOPRST", choice$): y = 18
  IF nz <> 0 AND sw = 0 THEN
    IF flip <> 0 THEN mc = nz ELSE mc = y + nz
    nz = nz + 2: clm = 6
    LOCATE nz, clm: PutText2 "Û", 2, 1
    IF flip = 0 THEN
      edittext nz, clm, mc, 6, 78
    ELSE
      edittext nz, clm, mc, 6, 78
    END IF
    file$ = "MMTmacro.fil"
    filenum = FREEFILE: OPEN "O", filenum, file$
    FOR x = 1 TO 18
      IF macro$(x + 18) <> "" THEN PRINT #filenum, "'" + macro$(x + 18)
      PRINT #filenum, macro$(x)
    NEXT
    CLOSE filenum
    clm = 1
  END IF
  '
  IF clm <> 1 THEN
    LINE (34, 13)-(627, 159), 1, BF
    z = 16
    FOR x = 1 TO 18
      IF flip = 0 THEN
        nz = DRAWSTR%(2, macro$(x), 40, z, 14, 1, 8)
      ELSE
        nz = DRAWSTR%(2, macro$(x + 18), 40, z, 14, 1, 8)
      END IF
      z = z + 8
    NEXT x
    IF flip = 0 THEN flip = 1 ELSE flip = 0
  END IF
LOOP
'
PUT (0, 0), menusav, PSET
dowindo 1, 15, 8
LOCATE row%, col%
EXIT SUB
'
script:
icon 33, 11, 598, 150, 8, 15, 1, 0     'editable box
nz = DRAWSTR%(2, "Edit Above Macros: Hit red letter on this menu.", 20, 165, 8, 7, 8)
nz = DRAWSTR%(2, "Run A Macro: Exit menu. Use <ALT> with letter of that macro,", 20, 175, 8, 7, 8)
nz = DRAWSTR%(2, "or click on blue letter at top of Main Screen.", 124, 185, 8, 7, 8)
cl1 = 4 + 128 XOR 7: COLOR cl1: z = 1: y = 3
FOR x = 65 TO 84
  IF x = 70 OR x = 81 THEN y = y - 1 ELSE LOCATE y, 3: PRINT CHR$(x)
  y = y + 1
NEXT x
clm = 16         'prints editable macros
FOR x = 19 TO 36
  nz = DRAWSTR%(2, macro$(x), 40, clm, 14, 1, 8)
  clm = clm + 8
NEXT x
'
smallprint 527, 172, "MACRO MENU", 0
smallprint 526, 171, "MACRO MENU", 14
x = 550: y = 177
icon x, y, 50, 12, 15, 8, 7, 0
smallprint x + 20, y + 9, "LIP", 0
smallprint x + 21, y + 9, "LIP", 0
smallprint x + 11, y + 9, "F", 4
smallprint x + 12, y + 9, "F", 4
RETURN
END SUB

SUB parse (temp$, LoopStr$, send$, look$, go$)
STATIC HOLD
temp$ = LTRIM$(temp$)
IF temp$ = "" THEN EXIT SUB
send$ = "": look$ = ""
x = INSTR(temp$, " ")        'looks for command
y = INSTR(temp$, "}")
IF x > y OR x = 0 THEN x = y
IF x = 0 THEN ermes "Macro Coded Incorrectly", 1: temp$ = "": EXIT SUB
IF HOLD > 0 THEN SLEEP HOLD: HOLD = 0
comm$ = UCASE$(LEFT$(temp$, x - 1))
temp$ = MID$(temp$, LEN(comm$) + 2)
'
SELECT CASE comm$
CASE "{SEND"                'looks for send     {send ##}{####}
  x = INSTR(temp$, "}")        'look for where transmit ends
  send$ = LEFT$(temp$, x - 1)
  IF LEFT$(send$, 1) = "%" THEN
    y = ival%(MID$(send$, 2))     'gets where in input array
    IF RIGHT$(send$, 1) = ";" THEN send$ = CHR$(13) ELSE send$ = ""
    IF uinput$(y) <> "" THEN
      send$ = uinput$(y) + send$
    ELSE                  'asking for input
      col = POS(0): row = CSRLIN: soundfx 6: LOCATE row, col - 1
      COLOR 14, 0: INPUT uinput$(y)
      send$ = uinput$(y) + send$
      LOCATE row, col + 1
    END IF
  ELSE
    IF RIGHT$(send$, 1) = ";" THEN send$ = LEFT$(send$, x - 2) + CHR$(13)
  END IF
  z = INSTR(temp$, "{")
  temp$ = MID$(temp$, z + 1)
  z = INSTR(temp$, "}")        'look where look ends
  look$ = LEFT$(temp$, z - 1)
  temp$ = MID$(temp$, z + 1)   'reset temp$
CASE "{BASH"
  findit "Obvious", bash$, sw, 0
  z = INSTR(16, UCASE$(bash$), "CLOSED")
  IF z <> 0 THEN
    bash$ = MID$(bash$, z + 12)
    z = INSTR(bash$, ",")
    IF z <> 0 THEN bash$ = LEFT$(bash$, z - 1)
    IF RIGHT$(bash$, 5) = "above" THEN
      bash$ = "U"
    ELSEIF RIGHT$(bash$, 5) = "below" THEN
      bash$ = "D"
    END IF
    bash$ = "bash " + bash$ + CHR$(13)
    transmit bash$
    autobash = 1
  END IF
CASE "{SET"
  z = INSTR(temp$, "}")        'look where reset ends
  IF z < 5 AND z > 0 THEN
    comm$ = LEFT$(temp$, z - 1)
    temp$ = MID$(temp$, z + 1)   'reset temp$
    IF LEFT$(comm$, 1) = "%" THEN
      z = ival%(MID$(comm$, 2))
      IF z > 3 AND z < 10 THEN uinput$(z) = ""
    END IF
  ELSE
    FOR y = 4 TO 9: uinput$(y) = "": NEXT y
  END IF
CASE "{GOTO"
  z = INSTR(temp$, "}")
  IF z <> 0 THEN
    comm$ = ":" + UCASE$(LEFT$(temp$, z - 1))
    z = INSTR(UCASE$(LoopStr$), comm$)
    IF z <> 0 THEN
      z = z + LEN(comm$) + 1
      temp$ = MID$(LoopStr$, z)
    ELSE
      ermes "Label Not Found!", 1: temp$ = ""
    END IF
  ELSE
    ermes "Wrong syntax for a label.", 1: temp$ = ""
  END IF
CASE "{BEEP"
  BEEP
CASE "{DROPC"
  LINE (4, 336)-(634, 344), 9, BF
  nz = DRAWSTR%(0, "Do you really want to drop carrier? y/N", 160, 337, 14, 9, 8)
  DO: keyin$ = UCASE$(INKEY$): LOOP UNTIL keyin$ <> ""
  IF keyin$ = "Y" THEN
    temp$ = ""
    DTRControl 0
  ELSE
    keyin$ = ""
  END IF
  LINE (4, 336)-(634, 344), 9, BF
CASE "{WAIT"
  x = INSTR(temp$, "}")
  IF x > 1 THEN HOLD = ival(LEFT$(temp$, x - 1)) ELSE HOLD = 1
  IF x = 0 THEN temp$ = "" ELSE temp$ = MID$(temp$, x + 1)
CASE ELSE    'does equal
  IF LEFT$(comm$, 2) = "{%" THEN
    rec = ival%(MID$(comm$, 3))
    y = INSTR("=", LEFT$(temp$, 1))
    temp$ = LTRIM$(MID$(temp$, 2))
    z = INSTR(temp$, "}")
    SELECT CASE y
      CASE 1    '=
        IF LEFT$(temp$, 1) = "%" THEN
          uinput$(rec) = uinput$(ival(MID$(temp$, 2, z - 2)))
        ELSE
          uinput$(rec) = LEFT$(temp$, z - 1)
          IF rec > 9 THEN uinput$(rec) = UCASE$(uinput$(rec))
        END IF
      CASE ELSE
        temp$ = "Incorrect Command: " + comm$ + " ? " + LEFT$(temp$, z - 1) + "}"
        ermes temp$, 1: temp$ = ""
    END SELECT
    temp$ = MID$(temp$, z + 1)
  ELSEIF LEFT$(comm$, 2) = "{:" THEN
  ELSE
    temp$ = "Incorrect Command: " + comm$ + "}": ermes temp$, 1: temp$ = ""
  END IF
END SELECT
END SUB

