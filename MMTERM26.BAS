DECLARE SUB reinit (sw%)
DECLARE SUB DoAltBar ()
DECLARE SUB DoHide ()
DECLARE SUB dialit (h%, v%)
DECLARE SUB init (dosfg%, dosbg%, regname$)
DECLARE SUB menubar2 ()
DECLARE FUNCTION inkey% (NoWait%, x%)
DECLARE SUB Response (swi%, what$)
DECLARE SUB hilite (temp$, fore%, back%, high%, many%)
DECLARE SUB soundfx (sw%)
DECLARE SUB domacro (scrollback$(), keyin$, macro$(), recon%, line$)
DECLARE SUB reg ()
DECLARE SUB PutText2 (text$, TextColor%, BackColor%)
DECLARE SUB edittext (rw%, clm%, which%, min%, max%)
DECLARE SUB loadspell ()
DECLARE SUB LOADhlp (hlp$())
DECLARE SUB mousefind (keyin$)
DECLARE SUB mouseget (mouseb%)
DECLARE SUB mousepointer (OnOff%)
DECLARE SUB MouseArea (y1%, y2%, min%, max%)
DECLARE SUB mouseinit ()
DECLARE SUB mousespeed (Speed%)
DECLARE SUB viewhelp ()
DECLARE SUB savecfg (sw%)
DECLARE SUB config ()
DECLARE SUB recorder ()
DECLARE SUB emul (st$)
DECLARE SUB alertchek ()
DECLARE SUB sheltodos ()
DECLARE SUB viewfile (ext$, choice$)
DECLARE SUB adjustscreen ()
DECLARE SUB ckey (keyin$)
DECLARE SUB dooptions (keyin$)
DECLARE SUB starlog (log$)
DECLARE SUB inbar (prompt$, sel$, lenofinput%, txtclr%, fg%, bg%, x%)
DECLARE SUB MacroScreen (menusav%())
DECLARE SUB loadmac (monsize%, regit%)
DECLARE SUB Credits ()
DECLARE SUB ermes (tp$, sw%)
DECLARE SUB scrollit (pntlow%, pnthigh%, size%, snd%)
DECLARE SUB readdir (ext$)
DECLARE SUB updateSBack (scrollcnt%, scrollback$(), line$)
DECLARE SUB doprint (ch0$, line$, fore0%, back0%, crai%, row%, col%)
DECLARE SUB ScrnScroll (ULRow%, ULColumn%, LRRow%, LRColumn%, NumOfLines%, sw%)
DECLARE SUB dowindo (num%, cl1%, cl2%)
DECLARE SUB Nocary ()
DECLARE SUB tinyfont (hor%, ver%, inline$, CLR%)
DECLARE SUB cubeit (h%, v%, WD%, HT%, C1%, C2%, FILD%)
DECLARE SUB icon (h%, v%, WD%, HT%, C1%, C2%, FILD%, OUTIN%)
DECLARE SUB Crypt (Buffer$, key$, start%)
DECLARE SUB smallprint (spx%, spy%, sp$, spcolor%)
DECLARE FUNCTION Exists% (filen$)
DECLARE FUNCTION Iinkey$ (max%, row%, col%, fore%, sw%)
DECLARE FUNCTION DIR$ (FileSpec$)
DECLARE FUNCTION CurDir$ ()
DECLARE FUNCTION BufIn$ (filen$, done%, reading%)
    DECLARE SUB OpenComm CDECL ALIAS "_open_comm" (BYVAL Port%, irq%, BYVAL Wlen%, BYVAL Parity%, BYVAL baud&, BYVAL HS%, BYVAL FOSSIL%)
    DECLARE SUB closecomm CDECL ALIAS "_close_comm" ()
    DECLARE FUNCTION ReadChar% CDECL ALIAS "_readchar" ()
    DECLARE SUB transmit CDECL ALIAS "_transmit_string" (addr$)
    DECLARE FUNCTION DataWaiting% CDECL ALIAS "_data_waiting" ()
    DECLARE SUB ClearInputBuffer CDECL ALIAS "_clear_input_buffer" ()
    DECLARE SUB carrierdetect CDECL ALIAS "_carrier_detect_flag" (BYVAL OnOff%)
    DECLARE FUNCTION CarrierLost% CDECL ALIAS "_carrier_state" ()
    DECLARE SUB DTRControl CDECL ALIAS "_dtr" (BYVAL OnOff%)
DECLARE FUNCTION ival% (number$)
DECLARE FUNCTION DRAWSTR% (mode%, strg$, x0%, y0%, fg%, bg%, gap%)
DECLARE FUNCTION EGAVIDEO% ()

TYPE RegTypeX
  ax    AS INTEGER
  bx    AS INTEGER
  cx    AS INTEGER
  dx    AS INTEGER
  bp    AS INTEGER
  si    AS INTEGER
  di    AS INTEGER
  flags AS INTEGER
  DS    AS INTEGER
  es    AS INTEGER
END TYPE
DIM SHARED InRegs AS RegTypeX, OutRegs AS RegTypeX
DECLARE SUB InterruptX (intnum AS INTEGER, InRegs AS RegTypeX, OutRegs AS RegTypeX)
CONST dos = &H21
CONST SetDTA = &H1A00, FindFirst = &H4E00, FindNext = &H4F00

DEFINT A-Z
'$DYNAMIC
COMMON SHARED nomouse%, mouseh%, mousev%
COMMON SHARED uinput$(), macro$(), snd%, maxline, line$, autobash, bash$
COMMON SHARED help()  AS STRING * 76, spells, spelsize
COMMON SHARED fore0%, back0%, linport$, bbs$, scrollcnt
COMMON SHARED atakon, caston, autograb, autosrch, itemalrt, autohide, autosnk
COMMON SHARED log$, CRLF$, regit, irq%, baud&, recon, Port%, HS, monsize
COMMON SHARED hidesw, grabover, alias$, hpt, autoheal, ver$
'
REDIM macro$(1 TO 38), uinput$(1), help(1) AS STRING * 76
REDIM SHARED menusav(1 TO 32767), scrollback$(140), itm$(1 TO 7)
REDIM SHARED hlp$(1 TO 112), spells$(1), atak$(1 TO 11)
'
RANDOMIZE TIMER
CLS
IF NOT EGAVIDEO% THEN PRINT "Sorry, EGA graphic card required to run MMTERM.": BEEP: END
IF NOT Exists("MMTscrn0.gfx") THEN PRINT "Sorry, It seems you are missing MMTERM's graphic file (MMTSCRN0.GFX).": BEEP: END
bbs$ = UCASE$(COMMAND$)                 'gets gamename, seperates major
z = INSTR(bbs$, " ")
IF z <> 0 THEN bbs$ = LEFT$(bbs$, z - 1): dial = 78
IF LEN(bbs$) > 8 THEN bbs$ = LEFT$(bbs$, 8)
IF bbs$ = "" THEN
  DO
    INPUT "You forgot to specify a database to access.  Please do so now: ", bbs$
  LOOP UNTIL bbs$ <> ""
  bbs$ = UCASE$(bbs$)
  z = INSTR(bbs$, " ")
  IF z <> 0 THEN bbs$ = LEFT$(bbs$, z - 1): dial = 78
END IF
IF NOT Exists(bbs$ + ".cfg") THEN
  IF Exists("setup.exe") THEN
    SHELL "setup " + bbs$
  ELSE
    PRINT "Sorry, you are missing SETUP.EXE. Now aborting!": BEEP: END
  END IF
END IF
'
mouseinit               'initialize for mouse
init dosfg, dosbg, regname$
MouseArea 194, 330, 2, 635
OpenComm Port, irq%, 8, 0, baud&, HS, 0
mousepointer 0
IF CarrierLost THEN
  nz = DRAWSTR%(0, "Do you wish to dial a BBS?  (Y/n)", 188, 337, 14, 9, 8)
  dial = inkey(0, 1)
ELSE
  dial = 78
END IF
IF dial <> 78 THEN 'yes dial
  dialit 1, 9
ELSE              'we either offline or already connected
  LOCATE 3, 1
  IF CarrierLost% <> 1 THEN transmit CHR$(13)
END IF
IF regit = 0 THEN regname$ = "UNREGISTERED"
hilite "Database Name:  " + bbs$ + STRING$(10, " ") + "Licensed to:  " + regname$, 14, 9, 0, 0
'
mousepointer 1
fore0 = 7
DO                              ' Main communications loop.
  mouseget mouseb
  keyin$ = INKEY$
  IF CarrierLost% THEN Nocary
  IF mouseb = 1 THEN
    mousefind keyin$
    WHILE mouseb%
      mouseget mouseb
    WEND
  ELSEIF mouseb = 2 THEN
    WHILE mouseb%
      mouseget mouseb
    WEND
  END IF
  IF keyin$ = CHR$(27) THEN            'escape
    IF snd = 1 THEN soundfx 6
    mousepointer 0
    LINE (4, 336)-(634, 344), 9, BF
    nz = DRAWSTR%(0, "Do you really want to exit MMTERM?  y/N", 160, 337, 14, 9, 8)
    DO: keyin$ = UCASE$(INKEY$): LOOP UNTIL keyin$ <> ""
    IF keyin$ = "Y" THEN
      EXIT DO
    ELSE
      keyin$ = ""
      LINE (4, 336)-(634, 344), 9, BF
      nz = DRAWSTR%(0, "MMTERM 2.6", 152, 337, 15, 9, 8)
      nz = DRAWSTR%(0, "Copyright 1995 by Will Boyett", 264, 337, 14, 9, 8)
      mousepointer 1
    END IF
  END IF
  IF LEN(keyin$) = 2 THEN                'F keys
    dooptions keyin$
  ELSEIF keyin$ <> "" THEN               'other keys
    ckey keyin$
  END IF
  IF recon = 1 THEN                     'if playing back recorder
    LINE INPUT #7, modemin$
    IF EOF(7) THEN recorder
    emul modemin$
  END IF
  DO WHILE DataWaiting
    modemin$ = CHR$(ReadChar)
    IF recon = 2 THEN       'if doing black box
      IF LEFT$(modemin$, 1) = CHR$(10) THEN
        IF w$ <> "" THEN PRINT #7, w$
        PRINT #7, modemin$
        w$ = ""
      ELSE
        w$ = w$ + modemin$
      END IF
    END IF
    emul modemin$          'determines if menu display
    IF autosrch = 2 THEN transmit "search" + CHR$(13): reinit 0: autosrch = 1
  LOOP
LOOP
xrror:
closecomm
savecfg 0
IF regit = 0 THEN reg
CLOSE
SCREEN 0
WIDTH 80, 25
Credits
COLOR dosfg, dosbg     'restore
IF regit = 0 THEN SLEEP 3
LOCATE 24, 1
END

REM $STATIC
SUB alertchek STATIC
IF line$ = "" THEN EXIT SUB
IF CarrierLost THEN EXIT SUB
'
IF spells = 1 THEN GOSUB explain
IF autobash = 1 THEN GOSUB bashit
'
IF RIGHT$(line$, 4) = "Off*" THEN
  IF atakon = 1 OR caston = 1 THEN
    IF did13 = 0 THEN transmit CHR$(13): did13 = 1
  END IF
  IF autograb <> 1 THEN reinit onemore
ELSEIF LEFT$(line$, 10) = "Also here:" THEN
  temp$ = UCASE$(RTRIM$(MID$(line$, 12)))   'contains lists of alsos
    DO
      GOSUB parsetemp
      h = 0
      FOR x = 10 TO monsize
        IF uinput$(1) = uinput$(x) THEN h = 1: x = monsize
      NEXT x
    LOOP UNTIL temp$ = "" OR h <> 0
    IF h <> 0 THEN              'if we found one
      IF atakon = 1 THEN
        transmit uinput$(0) + uinput$(1) + CHR$(13)
      ELSEIF caston = 1 THEN
        IF uinput$(2) = "" THEN
          ermes "Need to use F2 and specify a spell to use.", snd
        ELSE
          transmit uinput$(2) + uinput$(1) + CHR$(13)
        END IF
      END IF
    ELSE
      uinput$(1) = "*"         'didn't find one
    END IF
ELSEIF INSTR("orth.outh.east.west.", RIGHT$(line$, 5)) THEN
  IF INSTR(line$, "from the") THEN
    IF atakon = 1 OR caston = 1 THEN
      IF did13 = 0 THEN
        FOR x = 10 TO monsize
          z = INSTR(UCASE$(line$), UCASE$(uinput$(x)))
          IF z <> 0 THEN
            x = monsize: transmit CHR$(13): did13 = 1
          END IF
        NEXT x
      END IF
    END IF
  END IF
ELSEIF LEFT$(line$, 16) = "You make a sound" OR RIGHT$(line$, 16) = "you're sneaking." THEN
  IF autosnk = 1 THEN transmit "sneak" + CHR$(13)
ELSEIF RIGHT$(line$, 16) = "longer sneaking." THEN
  IF autosnk = 1 THEN transmit "sneak" + CHR$(13)
ELSEIF LEFT$(line$, 3) = "Obv" THEN
  didheal = 0: did13 = 0
ELSEIF MID$(line$, 2, 2) = "HP" THEN
  IF autoheal = 1 AND didheal = 0 THEN
    z = 0
    DO
      z = INSTR(5, line$, "HP=")
      IF z < 3 THEN z = 2 ELSE line$ = MID$(line$, z - 1)
    LOOP UNTIL z = 2
    z = INSTR(5, line$, "/MA")
    IF z = 0 THEN z = INSTR(5, line$, "/KAI")
    IF z = 0 THEN z = INSTR(5, line$, ":") - 1
    y = z - 5        'get illegal function here cause no MA or KAI
    y = ival(MID$(line$, 5, y))
    IF y <> hp THEN
      hp = y
      IF hp <= hpt AND hp > 0 THEN        'if needs healing
        z = z + 4
        IF MID$(line$, z, 1) = "=" THEN z = z + 1   'if it's KAI add one more
        y = INSTR(z, line$, ":") - 1
        y = y - z
        IF LEFT$(uinput$(3), 2) = "DR" THEN
          didheal = 1
          transmit uinput$(3) + CHR$(13)
          IF did13 = 0 THEN transmit CHR$(13): did13 = 1
        ELSE
          IF y < 0 THEN mana = 0 ELSE mana = ival(MID$(line$, z, y))
          IF mana > 1 THEN     'if enough mana cast
            didheal = 1
            transmit uinput$(3) + alias$ + CHR$(13)
            IF did13 = 0 THEN transmit CHR$(13): did13 = 1
          END IF
        END IF
      END IF
    END IF
    line$ = " " + line$
  END IF
ELSEIF LEFT$(line$, 8) = "There is" OR dl > 0 THEN
  IF ver$ = "A" THEN
    IF RIGHT$(line$, 5) <> "tion!" THEN
      GOSUB grabber
    END IF
  ELSE
    IF dl > 0 THEN GOSUB grabber
  END IF
ELSEIF LEFT$(line$, 8) = "You noti" OR dl > 0 THEN
  IF ver$ = "V" THEN GOSUB grabber
ELSEIF RIGHT$(line$, 10) = "Spell Name" THEN
  spells = 1
END IF
EXIT SUB
'-------------------------------
alert:
IF snd = 1 THEN soundfx 6
row0 = CSRLIN
cl = 15 + 128 XOR 12: COLOR cl
LINE (370, (row0 * 8) - 26)-(460, (row0 * 8) - 16), 12, BF
LOCATE row0 - 2, 48: PRINT "ITEM ALERT"
LOCATE row0 + 1, 1
RETURN
'
grabber:
IF RIGHT$(line$, 5) = "here." THEN
  IF itemalrt = 1 THEN GOSUB alert
  IF autograb = 1 THEN
    dl = dl + 1: itm$(dl) = LEFT$(line$, LEN(line$) - 5)
    GOSUB grab
    REDIM itm$(1 TO 7): dl = 0
  END IF
ELSE    'if it's more then one line long
  IF autograb = 1 THEN dl = dl + 1: itm$(dl) = line$
END IF
RETURN
'
grab:
itm$(1) = MID$(itm$(1), 10)
temp$ = " "
IF dl = 1 THEN
  temp$ = temp$ + itm$(1)
ELSE
  temp$ = temp$ + itm$(dl - 1) + itm$(dl)
END IF
temp$ = RTRIM$(temp$)
DO
  FOR z = LEN(temp$) TO 1 STEP -1
    IF MID$(temp$, z, 1) = " " THEN
      item$ = MID$(temp$, z + 1)
      temp$ = LEFT$(temp$, z - 1)
      FOR y = LEN(temp$) TO 1 STEP -1
        x = 0
        IF MID$(temp$, y, 1) = "," THEN
          temp$ = LEFT$(temp$, y - 1)
          y = 1: x = 1
        END IF
      NEXT y
      IF x = 0 THEN temp$ = ""
      z = 1
    END IF
  NEXT z
  SELECT CASE LEFT$(item$, 5)
    CASE "bulle", "fount", "stree"
    CASE ELSE
      IF grabover = 1 THEN
        grabit = 0
        DO
          inline$ = BufIn$("MMTGrab.dat", done, reading)
          IF item$ = inline$ THEN grabit = 1
        LOOP UNTIL done
        IF grabit = 1 THEN transmit "Get " + item$ + CHR$(13)
      ELSE
        grabit = 1: transmit "Get " + item$ + CHR$(13)
      END IF
  END SELECT
LOOP UNTIL temp$ = ""
IF grabit = 1 THEN
  SELECT CASE dl
    CASE IS < 3: reinit onemore
    CASE ELSE: IF did13 = 0 THEN transmit CHR$(13): did13 = 1
  END SELECT
END IF
RETURN
'
parsetemp:
z = INSTR(temp$, ",")                  'looks see if more then 1 present
y = INSTR(temp$, " ")
IF z = 0 THEN
  onemore = 0
  uinput$(1) = temp$
  IF y <> 0 THEN
    uinput$(1) = LEFT$(temp$, y - 1)
    temp$ = MID$(temp$, y + 1)
  ELSE
    temp$ = ""
  END IF
ELSE
  onemore = 1
  IF y < z THEN  'space comes first
    uinput$(1) = LEFT$(temp$, y - 1)
    temp$ = MID$(temp$, y + 1)
  ELSE      'comma coems first
    uinput$(1) = LEFT$(temp$, z - 1)
    temp$ = MID$(temp$, z + 1)
  END IF
END IF
IF RIGHT$(uinput$(1), 1) = "." THEN uinput$(1) = LEFT$(uinput$(1), LEN(uinput$(1)) - 1)
RETURN
'
explain:
IF INSTR(line$, ":") THEN spells = 0: RETURN
temp$ = RTRIM$(UCASE$(MID$(line$, 18)))
z = LEN(temp$)
IF z > 14 THEN temp$ = LEFT$(temp$, 14): z = 14
FOR x = 1 TO spelsize
  IF temp$ = LEFT$(UCASE$(spells$(x)), z) THEN
    nz = DRAWSTR%(0, " " + MID$(spells$(x), 16, 25) + " ", 284, (CSRLIN - 2) * 8, 8, 14, 8)
    x = spelsize
  END IF
NEXT x
RETURN
'
bashit:
IF RIGHT$(line$, 13) = "through fail!" THEN
  transmit bash$
ELSEIF RIGHT$(line$, 13) = "the door open." THEN
  autobash = 0
  IF autosnk = 1 THEN transmit "sneak" + CHR$(13)
END IF
bas = bas + 1
IF bas = 10 THEN autobash = 0: bas = 0
RETURN
END SUB

SUB ckey (keyin$)
SELECT CASE keyin$
CASE CHR$(3): starlog log$
CASE CHR$(4): sheltodos: transmit CHR$(13)
CASE CHR$(18): transmit "BRIEF" + CHR$(13)
CASE CHR$(16): viewhelp
CASE CHR$(15)
  IF regit <> 0 THEN
    macro$(37) = "{bash}"
    domacro scrollback$(), "G", macro$(), recon, line$
  ELSE
    ermes "Need to register in order to use this.", snd
  END IF
CASE CHR$(19): transmit "ST" + CHR$(13)
CASE CHR$(23): transmit "WEALTH" + CHR$(13)
CASE CHR$(2): viewfile "S", choice$
CASE CHR$(22): transmit "VERBOSE" + CHR$(13)
CASE CHR$(12): transmit "LOOK" + CHR$(13)
CASE CHR$(1): dialit POS(0), CSRLIN
CASE CHR$(9): transmit "I" + CHR$(13)
CASE CHR$(5): transmit "EXP" + CHR$(13)
CASE CHR$(11): recorder       'K
CASE ELSE: transmit keyin$
END SELECT
END SUB

SUB config
row% = CSRLIN: col% = POS(0)
mousepointer 0
GET (0, 0)-(639, 200), menusav
dowindo 2, 8, 15
mousepointer 0
GOSUB drawscrn
MouseArea 40, 190, 200, 350
mousepointer 1
DO
  DO
    choice$ = UCASE$(INKEY$)
    mouseget mouseb
  LOOP UNTIL choice$ <> "" OR mouseb <> 0
  IF mouseb = 1 THEN
    SELECT CASE mousev
      CASE 40 TO 54: choice$ = "1"
      CASE 55 TO 69: choice$ = "2"
      CASE 70 TO 85: choice$ = "3"
      CASE 86 TO 100: choice$ = "4"
      CASE 101 TO 115: choice$ = "5"
      CASE 116 TO 130: choice$ = "6"
      CASE 131 TO 145: choice$ = "7"
      CASE 146 TO 160: choice$ = "8"
      CASE 161 TO 175: choice$ = "9"
      CASE 176 TO 190: choice$ = "0"
    END SELECT
    WHILE mouseb%
      mouseget mouseb
    WEND
  ELSEIF mouseb = 2 THEN
    choice$ = CHR$(13)
  END IF
  x = ival%(choice$)
  IF x >= 0 AND x < 10 THEN
    SELECT CASE x
      CASE 1
        Port = Port + 1: IF Port = 5 THEN Port = 0
      CASE 2
        HS = HS + 1: IF HS = 4 THEN HS = 0
      CASE 3
        irq = irq + 1: IF irq = 8 THEN irq = 2
      CASE 4
        IF baud& = 2400 THEN
          baud& = 9600
        ELSEIF baud& = 9600 THEN
          baud& = 14400
        ELSEIF baud& = 14400 THEN
          baud& = 19200
        ELSEIF baud& = 19200 THEN
          baud& = 38400
        ELSEIF baud& = 38400 THEN
          baud& = 57600
        ELSEIF baud& = 57600 THEN
          baud& = 2400
        END IF
      CASE 5
        IF snd = 0 THEN snd = 1 ELSE snd = 0
      CASE 6
        IF itemalrt = 0 THEN itemalrt = 1 ELSE itemalrt = 0
      CASE 7
        IF grabover = 0 THEN grabover = 1 ELSE grabover = 0
      CASE 8
        inbar "Enter the spell command you wish to use against enemy: ", temp$, 9, 14, 0, 9, snd
        IF temp$ <> "" THEN uinput$(2) = RTRIM$(UCASE$(temp$)) + " "
      CASE 9
        inbar "Enter command you wish to use for healing self: ", temp$, 10, 14, 0, 9, snd
        IF temp$ <> "" THEN uinput$(3) = RTRIM$(UCASE$(temp$)) + " "
        inbar " Enter HP number to fall below in order for healing to start: ", temp$, 3, 14, 0, 9, snd
        IF temp$ <> "" THEN hpt = VAL(temp$): IF hpt = 0 THEN hpt = 10
      CASE 0
        inbar "Enter the attack command you wish to use against enemy: ", temp$, 4, 14, 0, 9, snd
        IF temp$ <> "" THEN uinput$(0) = RTRIM$(UCASE$(temp$)) + " "
    END SELECT
    GOSUB values
  END IF
LOOP UNTIL choice$ = CHR$(13)
mousepointer 0
PUT (0, 0), menusav, PSET
MouseArea 194, 330, 2, 635
savecfg 0
dowindo 2, 15, 8
LOCATE row%, col%
EXIT SUB
'
drawscrn:
cubeit 159, 0, 320, 199, 15, 8, 7
icon 164, 6, 310, 187, 8, 15, 7, 0
icon 219, 10, 200, 18, 8, 15, 7, 0
x = 187
nz = DRAWSTR%(0, ") ..........Comm Port:", x, 43, 0, 7, 8)
nz = DRAWSTR%(0, ") ........Handshaking:", x, 58, 0, 7, 8)
nz = DRAWSTR%(0, ") ................IRQ:", x, 73, 0, 7, 8)
nz = DRAWSTR%(0, ") ...............Baud:", x, 88, 0, 7, 8)
nz = DRAWSTR%(0, ") ..............Sound:", x, 103, 0, 7, 8)
nz = DRAWSTR%(0, ") .........Item Alert:", x, 118, 0, 7, 8)
nz = DRAWSTR%(0, ") ...Config Auto Grab:", x, 133, 0, 7, 8)
nz = DRAWSTR%(0, ") ..Auto Cast Command:", x, 148, 0, 7, 8)
nz = DRAWSTR%(0, ") ..Auto Heal Command:", x, 163, 0, 7, 8)
nz = DRAWSTR%(0, ") Auto Attack Command:", x, 178, 0, 7, 8)
'
cnt = 0: h = 169
FOR x = 43 TO 190 STEP 15
  cnt = cnt + 1
  IF cnt = 10 THEN cnt = 0
  IF cnt < 10 THEN nz = DRAWSTR%(0, STR$(cnt), h, x, 4, 7, 8)
NEXT x
smallprint 244, 23, "CONFIGURATION MENU", 0
smallprint 243, 22, "CONFIGURATION MENU", 14
values:
LINE (365, 40)-(452, 185), 7, BF
SELECT CASE HS
  CASE 0: temp$ = "NONE"
  CASE 1: temp$ = "XON/XOFF"
  CASE 2: temp$ = "CTS/RTS"
  CASE 3: temp$ = "ALL"
END SELECT
y = 9: x = 377
nz = DRAWSTR%(0, LTRIM$(STR$(Port%)), x, 43, y, 7, 8)
nz = DRAWSTR%(0, temp$, x, 58, y, 7, 8)
nz = DRAWSTR%(0, LTRIM$(STR$(irq%)), x, 73, y, 7, 8)
nz = DRAWSTR%(0, LTRIM$(STR$(baud&)), x, 88, y, 7, 8)
IF snd = 1 THEN temp$ = "ON" ELSE temp$ = "OFF"
nz = DRAWSTR%(0, temp$, x, 103, y, 7, 8)
IF itemalrt = 1 THEN temp$ = "ON" ELSE temp$ = "OFF"
nz = DRAWSTR%(0, temp$, x, 118, y, 7, 8)
nz = DRAWSTR%(0, uinput$(2), x, 148, y, 7, 8)
nz = DRAWSTR%(0, uinput$(3), x, 163, y, 7, 8)
nz = DRAWSTR%(0, uinput$(0), x, 178, y, 7, 8)
IF grabover = 1 THEN temp$ = "SELECT" ELSE temp$ = "ALL"
nz = DRAWSTR%(0, temp$, x, 133, y, 7, 8)
IF regit = 0 AND temp$ = "SELECT" THEN
  ermes "Sorry, only Registered Users can be selective when AUTO GRABing", 1
  grabover = 0: GOTO values
END IF
RETURN
END SUB

SUB dialit (h, v)
  dowindo 11, 8, 15
  ClearInputBuffer
  mousepointer 0
  LOCATE v, h
  file$ = "MMTdial.cfg"
  GOSUB getcfg
  carrierdetect 0     ' <-- NEEDED TO TALK TO MODEM W/NO CARRIER!!
  DO                   'make sure modem OK first
    transmit modem$ + CRLF$
    z = 0
    Response z, "OK"
    IF z = -2 THEN closecomm: OpenComm Port, irq%, 8, 0, baud&, HS, 0: EXIT DO
    choice$ = INKEY$
    IF z >= 0 THEN EXIT DO
    SLEEP 2
  LOOP UNTIL choice$ = CHR$(27)
  '
  IF z >= 0 THEN    'if we got an OK
    DO
      transmit dial$ + phone$ + CRLF$
      z = wate
      Response z, "CONNECT"
      IF z >= 0 THEN EXIT DO
      SLEEP 3
      choice$ = INKEY$
    LOOP UNTIL choice$ = CHR$(27)
    IF choice$ = CHR$(27) THEN
      ClearInputBuffer
      transmit OffHook$ + CRLF$
      z = 0
      Response z, "OK"
      carrierdetect 2
    END IF
    LINE (4, 336)-(634, 344), 9, BF
  ELSE
    carrierdetect 2
  END IF
  dowindo 11, 15, 8
EXIT SUB
getcfg:
  IF Exists(file$) THEN
    FOR x = 1 TO 4
      inline$ = BufIn$(file$, done, reading)
      SELECT CASE x
        CASE 1: modem$ = MID$(inline$, 22)
        CASE 2: OffHook$ = MID$(inline$, 22)
        CASE 3: dial$ = MID$(inline$, 22)
        CASE 4: wate = VAL(MID$(inline$, 22))
      END SELECT
    NEXT x
    inline$ = BufIn$(file$, done, reading)
    REDIM phone$(1 TO 5)
    FOR x = 1 TO 5
      phone$(x) = BufIn$(file$, done, reading)
      IF done THEN x = 5
    NEXT x
    y = 260: v = 10
    icon y - 3, v - 3, 346, 96, 15, 8, 7, 0
    icon y, v, 340, 90, 8, 15, 7, 0
    nz = DRAWSTR%(0, "Choose BBS To Dial", y + 100, v + 6, 4, 7, 8)
    v = v + 30
    FOR x = 1 TO 5
      a = INSTR(phone$(x), ":")
      IF a <> 0 THEN
        nz = DRAWSTR%(0, STR$(x) + ">", y + 10, v, 4, 7, 8)
        nz = DRAWSTR%(0, LEFT$(phone$(x), a - 1), y + 45, v, 1, 7, 8)
        nz = DRAWSTR%(0, MID$(phone$(x), a + 1), y + 181, v, 1, 7, 8)
        phone$(x) = MID$(phone$(x), a + 1)
      END IF
      v = v + 9
    NEXT x
    choice = inkey(0, 0)
    choice = choice - 48
    phone$ = ""
    IF choice > 0 AND choice < 6 THEN
      FOR x = 1 TO LEN(phone$(choice))
        IF MID$(phone$(choice), x, 1) <> "-" THEN phone$ = phone$ + MID$(phone$(choice), x, 1)
      NEXT x
    END IF
    IF phone$ = "" THEN dowindo 11, 15, 8: EXIT SUB
  ELSE
    ermes "You need to run SETUP.EXE first!", snd
    EXIT SUB
  END IF
RETURN
END SUB

SUB DoAltBar
mousepointer 0
cl = 1
LINE (0, 193)-(639, 201), 3, BF
LINE (0, 193)-(639, 193), 11
nz = DRAWSTR%(0, "A   B   C   D   E   G   H   I   J   K   L   M   N   O   P   R   S   T ", 40, 194, cl, 3, 8)
mousepointer 1
END SUB

SUB DoHide
IF autohide = 0 THEN dowindo 10, 8, 15
autohide = 1
IF CarrierLost THEN EXIT SUB
hidesw = hidesw + 1
transmit "hide" + CHR$(13)
transmit "l " + alias$ + CHR$(13)
END SUB

SUB dooptions (keyin$)
SELECT CASE RIGHT$(keyin$, 1)       '0 #$%&
CASE ";"                                  'f1
  MacroScreen menusav()
CASE "<"                                  'f2
  config
CASE "="                            'F3
  viewfile "M", choice$
CASE ">"                            'F4
  IF regit = 0 THEN
    ermes "Sorry, only registered users are allowed access to this feature.", snd
  ELSE
    IF autoheal = 0 THEN
      IF hpt < 1 THEN
        inbar " Enter number of HP's to fall below before Healing starts: ", temp$, 3, 14, 0, 9, snd
        IF temp$ <> "" THEN hpt = VAL(temp$): IF hpt = 0 THEN hpt = 10
      END IF
      autoheal = 1: dowindo 4, 8, 15: transmit CHR$(13)
    ELSE
      autoheal = 0: dowindo 4, 15, 8
    END IF
  END IF
CASE "?"                            'F5
  IF autograb = 0 THEN
    autograb = 1: dowindo 5, 8, 15: transmit CHR$(13)
  ELSE
    autograb = 0: dowindo 5, 15, 8
  END IF
CASE "@"                          'F6
  IF caston = 0 THEN
    IF atakon = 0 THEN
      atakon = 1
      dowindo 6, 8, 15
      transmit CHR$(13)
    ELSE
      transmit "break" + CHR$(13)
      atakon = 0
      dowindo 6, 15, 8
    END IF
  ELSE
    transmit "break" + CHR$(13)
    caston = 0
    dowindo 9, 15, 8
    atakon = 1
    dowindo 6, 8, 15
    transmit CHR$(13)
  END IF
CASE "A"                             'F7
  IF autosnk = 0 THEN
    IF autohide = 1 THEN dowindo 10, 15, 8: autohide = 0
    autosnk = 1: dowindo 7, 8, 15: transmit "sneak" + CHR$(13)
  ELSE
    autosnk = 0: dowindo 7, 15, 8
  END IF
CASE "B"                             'F8
  IF autosrch = 0 THEN
    autosrch = 1: dowindo 8, 8, 15: transmit "search" + CHR$(13)
    reinit 0
  ELSE
    autosrch = 0: dowindo 8, 15, 8
  END IF
CASE "C"                               'f9
  IF atakon = 0 THEN
    IF caston = 0 THEN
      caston = 1
      dowindo 9, 8, 15
      transmit CHR$(13)
    ELSE
      transmit "break" + CHR$(13)
      caston = 0
      dowindo 9, 15, 8
    END IF
  ELSE
    transmit "break" + CHR$(13)
    atakon = 0
    dowindo 6, 15, 8
    caston = 1
    dowindo 9, 8, 15
    transmit CHR$(13)
  END IF
CASE "D"                              'f10
  IF autohide = 0 THEN
    IF autosnk = 1 THEN dowindo 7, 15, 8: autosnk = 0
    DoHide
  ELSE
    autohide = 0: dowindo 10, 15, 8
  END IF
CASE "O", "P", "Q", "M", "I", "H", "G", "K", "R", "S"
  IF CarrierLost% <> 1 THEN
    SELECT CASE RIGHT$(keyin$, 1)
      CASE "O": transmit "SW" + CHR$(13)
      CASE "P": transmit "S" + CHR$(13)
      CASE "Q": transmit "SE" + CHR$(13)
      CASE "M": transmit "E" + CHR$(13)
      CASE "I": transmit "NE" + CHR$(13)
      CASE "H": transmit "N" + CHR$(13)
      CASE "G": transmit "NW" + CHR$(13)
      CASE "K": transmit "W" + CHR$(13)
      CASE "R": transmit "UP" + CHR$(13)
      CASE "S": transmit "DOWN" + CHR$(13)
    END SELECT
    IF autosrch = 1 THEN autosrch = 2
  END IF
CASE ELSE
  domacro scrollback$(), keyin$, macro$(), recon, line$
END SELECT
keyin$ = ""
END SUB

SUB dowindo (num, cl1, cl2)
REDIM button(512)
IF snd = 1 THEN soundfx 2
SELECT CASE num
 CASE IS < 8: x = 4: z = 204 + ((num - 1) * 18)
 CASE 8 TO 10: x = 107: z = 204 + ((num - 4) * 18)
 CASE 11 TO 17: x = 434: z = 204 + ((num - 11) * 18)
 CASE 18 TO 23: x = 537: z = 204 + ((num - 17) * 18)
END SELECT
mousepointer 0
GET (x + 2, z + 3)-(x + 97, z + 14), button
icon x, z, 99, 16, cl1, cl2, 7, 0
IF cl1 = 8 THEN PUT (x + 2, z + 4), button, PSET ELSE PUT (x + 2, z + 2), button, PSET
mousepointer 1
END SUB

SUB emul (st$) STATIC
IF LEN(st$) > 2 THEN
  z = INSTR(st$, CRLF$)
  st$ = MID$(st$, z + 1)
END IF
FOR disp0% = 1 TO LEN(st$)
  ch0$ = MID$(st$, disp0%, 1)
  IF ANSIcode0% THEN
    IF LEFT$(ANSIst0$, 2) = "[M" OR LEFT$(ANSIst0$, 2) = "[m" THEN
      IF ASC(ch0$) = 14 THEN
        ANSIst0$ = ""
        ANSIcode0% = 0
      ELSE
        ANSIst0$ = ANSIst0$ + ch0$
      END IF
    ELSEIF INSTR("ABCDEFGHIJKLMNOPQRSTUVWXYZ", UCASE$(ch0$)) THEN
         SELECT CASE ch0$
            CASE "A": GOSUB CursorUp0
            CASE "B": GOSUB CursorDown0
            CASE "C": GOSUB CursorRight0
            CASE "D": GOSUB CursorLeft0
            CASE "H", "f": GOSUB CursorLocate0
            CASE "s": GOSUB SaveCursorPosn0
            CASE "u": GOSUB RestCursorPosn0
            CASE "J": GOSUB BigErase0
            CASE "K": GOSUB SmallErase0
            CASE "h", "l": REM  set display mode... ignored
            CASE "m": GOSUB SetColors0
            CASE ELSE
               PRINT ANSIst0$;
               ANSIcode0% = 0: ANSIst0$ = ""
         END SELECT
         ANSIst0$ = ""
         ANSIcode0% = 0
      ELSEIF ASC(ch0$) <= 32 OR LEN(ANSIst0$) > 60 THEN   'special chr
         ANSIcode0% = 0: ANSIst0$ = ""
      ELSE
         ANSIst0$ = ANSIst0$ + ch0$
      END IF
   ELSEIF ASC(ch0$) = 27 THEN       'turn on ansi characters
      ANSIcode0% = -1
      ANSIst0$ = ""
   ELSE
     IF ch0$ = CHR$(10) THEN                    'end of line
       PRINT ch0$;
       alertchek
       IF log$ = "ON" THEN PRINT #6, line$
       updateSBack scrollcnt, scrollback$(), line$
       col = POS(0): row = CSRLIN
       IF row% > maxline THEN
         ScrnScroll 1, 1, maxline, 80, 1, 0
         LOCATE maxline, 1
       END IF
       line$ = ""
     ELSEIF ch0$ <> CHR$(13) THEN '                        if not end of line
       doprint ch0$, line$, fore0, back0, crai, row, col  'print to screen
       LOCATE row, col
     END IF
   END IF
NEXT
EXIT SUB
'
CursorUp0:
Tmp0% = ival%(MID$(ANSIst0$, 2))
IF Tmp0% < 1 THEN Tmp0% = 1
row0% = CSRLIN: col0% = POS(0)
row0% = row0% - Tmp0%
IF row0% < 1 THEN row0% = 1
LOCATE row0%, col0%
RETURN
'
CursorDown0:
Tmp0% = ival%(MID$(ANSIst0$, 2))
IF Tmp0% < 1 THEN Tmp0% = 1
row0% = CSRLIN: col0% = POS(0)
row0% = row0% + Tmp0%
IF row0% > 25 THEN row0% = 25
LOCATE row0%, col0%
RETURN
'
CursorLeft0:
Tmp0% = ival%(MID$(ANSIst0$, 2))
IF Tmp0% < 1 THEN Tmp0% = 1
row0% = CSRLIN: col0% = POS(0)
col0% = col0% - Tmp0%
IF col0% < 1 THEN col0% = 1
LOCATE row0%, col0%
RETURN
'
CursorRight0:
Tmp0% = ival%(MID$(ANSIst0$, 2))
IF Tmp0% < 1 THEN Tmp0% = 1
row0% = CSRLIN: col0% = POS(0)
col0% = col0% + Tmp0%
IF col0% > 80 THEN col0% = 80
LOCATE row0%, col0%
RETURN
'
CursorLocate0:
row0% = ival%(MID$(ANSIst0$, 2))
Tmp0% = INSTR(ANSIst0$, ";")
IF Tmp0% THEN
   col0% = ival%(MID$(ANSIst0$, Tmp0% + 1))
ELSE
   col0% = 1
END IF
IF row0% < 1 THEN
   row0% = 1
ELSEIF row0% > 25 THEN
   row0% = 25
END IF
IF col0% < 1 THEN
   col0% = 1
ELSEIF col0% > 80 THEN
   col0% = 80
END IF
LOCATE row0%, col0%
RETURN
'
SaveCursorPosn0:
SaveRow0% = CSRLIN: SaveCol0% = POS(0)
RETURN
'
RestCursorPosn0:
IF SaveRow0% > 0 THEN LOCATE SaveRow0%, SaveCol0%
RETURN
'
BigErase0:
LINE (0, 0)-(639, 192), 0, BF
LOCATE 1, 1
RETURN
'
SmallErase0:
row0% = CSRLIN: col0% = POS(0)
ver% = row0% * 8 - 8
remain = 80 - col0%
FOR x = 1 TO remain + 1
  hor% = col0% * 8 - 8
  nz = DRAWSTR%(0, " ", hor%, ver%, fore0%, back0%, 8)
  col0% = col0% + 1
NEXT
IF col0% > 80 THEN col0% = 1
LOCATE row0%, col0%
RETURN
'
SetColors0:
ANSIst0$ = MID$(ANSIst0$, 2)
DO WHILE LEN(ANSIst0$)
  Tmp0% = ival%(ANSIst0$)
  SELECT CASE Tmp0%
    CASE 0: fore0% = 7: back0% = 0             ' reset colors
    CASE 1: fore0% = (fore0% OR 8)             ' high intensity
    CASE 2: fore0% = (fore0% AND &H17)         ' normal intensity
    CASE 5: fore0% = (fore0% OR 16)            ' blink
    CASE 7: fore0% = 0: back0% = 7             ' reverse video
    CASE 8: fore0% = 0: back0% = 0            ' invisible
    CASE 30: fore0% = (fore0% AND &H18)        ' black foreground
    CASE 31: fore0% = (fore0% AND &H18) OR 4   ' red foreground
    CASE 32: fore0% = (fore0% AND &H18) OR 2   ' green foreground
    CASE 33: fore0% = (fore0% AND &H18) OR 6   ' yellow foreground
    CASE 34: fore0% = (fore0% AND &H18) OR 1   ' blue foreground
    CASE 35: fore0% = (fore0% AND &H18) OR 5   ' magenta foreground
    CASE 36: fore0% = (fore0% AND &H18) OR 3   ' cyan foreground
    CASE 37: fore0% = (fore0% OR 7)            ' white foreground
    CASE 40: back0% = 0                        ' black background
    CASE 41: back0% = 4                        ' red background
    CASE 42: back0% = 2                        ' green background
    CASE 43: back0% = 6                        ' yellow background
    CASE 44: back0% = 1                        ' blue background
    CASE 45: back0% = 5                        ' magenta background
    CASE 46: back0% = 3                        ' cyan background
    CASE 47: back0% = 7                        ' white background
    CASE ELSE                                  ' ignore anything weird
  END SELECT
  Tmp0% = INSTR(ANSIst0$, ";")
  IF Tmp0% THEN
    ANSIst0$ = MID$(ANSIst0$, Tmp0% + 1)
  ELSE
    ANSIst0$ = ""
  END IF
LOOP
RETURN
END SUB

SUB findit (what$, found$, sw, which)
found$ = ""
FOR x = 140 TO 1 STEP -1
  IF which = 1 THEN
    sw = INSTR(scrollback$(x), what$)
    IF sw <> 0 THEN offset = x: x = 1
  ELSE
    IF LEFT$(scrollback$(x), LEN(what$)) = what$ THEN
      found$ = scrollback$(x)
      x = 1
    END IF
  END IF
NEXT x
END SUB

SUB init (dosfg, dosbg, regname$)
'------- saves original colors
SCREEN 0
SHELL "CLS"
DEF SEG = &HB800
dosclr = PEEK(1)
dosfg = dosclr AND 15
dosbg = (dosclr \ 16) AND 7
DEF SEG
'---------  get regname if exists
regit = 0                            'checks if registered
IF Exists("MMTPower.PAK") THEN
  OPEN "i", 1, "MMTPOWER.PAK"
  LINE INPUT #1, regname$
  LINE INPUT #1, key1$
  key$ = "PatHalpin"
  CALL Crypt(regname$, key$, 10)
  IF key$ = key1$ THEN regit = 1
  CLOSE #1
END IF
FOR x = 1 TO LEN(regname$)
  IF MID$(regname$, x, 1) < CHR$(60) OR MID$(regname$, x, 1) > CHR$(125) THEN
    IF MID$(regname$, x, 1) = CHR$(46) THEN
    ELSEIF MID$(regname$, x, 1) = CHR$(32) THEN
    ELSE
      regit = 0: x = LEN(regname$)
    END IF
  END IF
NEXT x
'--------  loads in databases
loadmac monsize, regit
LOADhlp hlp$()              ' loads in the choices of help menu
loadspell                   ' loads in the spell descriptions
'------ loads settings/data -----------------
log$ = "OFF": maxline = 24: CRLF$ = CHR$(10) + CHR$(13)
savecfg 1
'--------  display screen
menubar2
scrollcnt = 140
IF ver$ = "" THEN
  inbar "Are you playing in version 1.0y or later of MajorMUD?  Y/n", ver$, 1, 14, 0, 9, snd
  IF UCASE$(ver$) = "N" THEN ver$ = "A" ELSE ver$ = "V"
END IF
END SUB

FUNCTION inkey% (NoWait%, x)
DO
  K$ = UCASE$(INKEY$)
LOOP UNTIL NoWait% OR LEN(K$)
SELECT CASE LEN(K$)
  CASE 1: inkey% = ASC(K$)
  CASE 2: inkey% = -ASC(RIGHT$(K$, 1))   'Extended key, return ASCII
END SELECT                                '  value negated
IF x = 1 THEN LINE (4, 336)-(634, 344), 9, BF
END FUNCTION

SUB loadspell
IF Exists("MMTspell.dat") THEN
  filenum = FREEFILE
  OPEN "I", filenum, "MMTspell.dat"
  LINE INPUT #filenum, inline$
  x = ival%(RIGHT$(inline$, 2))
  REDIM spells$(1 TO x): spelsize = x
  FOR y = 1 TO x
    LINE INPUT #filenum, spells$(y)
  NEXT y
  CLOSE filenum
ELSE
  ermes "Sorry, you are missing MMTSPELL.DAT file. Spells bypassed.", 1
END IF
END SUB

SUB menubar2
WIDTH 80, 43
SCREEN 9, 0, 0, 1
DEF SEG = VARSEG(menusav(1))
BLOAD "MMTscrn0.gfx", VARPTR(menusav(1))
DEF SEG
PUT (0, 201), menusav
SCREEN 9, 0, 0, 0
PRINT "MMTERM 2.6     Copyright 1995 by Will Boyett"
DoAltBar
x = snd: snd = 0
IF atakon = 1 THEN dowindo 6, 8, 15
IF autosrch = 1 THEN dowindo 8, 8, 15
IF caston = 1 THEN dowindo 9, 8, 15
IF autohide = 1 THEN dowindo 10, 8, 15
IF autoheal = 1 THEN dowindo 4, 8, 15
snd = x
END SUB

DEFSNG A-Z
SUB MouseArea (y1%, y2%, min%, max%)
 '** Define area for mouse movement, y1% thru y2%
 IF nomouse% THEN EXIT SUB
 InRegs.ax = 8
 InRegs.cx = y1%
 InRegs.dx = y2%
 CALL InterruptX(&H33, InRegs, OutRegs)
 InRegs.ax% = &H7
 InRegs.cx% = min%
 InRegs.dx% = max%
 CALL InterruptX(&H33, InRegs, OutRegs)
END SUB

DEFINT A-Z
SUB mousefind (keyin$)
IF mousev < 204 THEN
  SELECT CASE mouseh
    CASE 35 TO 50: keyin$ = CHR$(0) + ""
    CASE 68 TO 83: keyin$ = CHR$(0) + "0"
    CASE 100 TO 115: keyin$ = CHR$(0) + "."
    CASE 132 TO 147: keyin$ = CHR$(0) + " "
    CASE 164 TO 179: keyin$ = CHR$(0) + ""
    CASE 196 TO 211: keyin$ = CHR$(0) + CHR$(34)
    CASE 228 TO 243: keyin$ = CHR$(0) + "#"
    CASE 261 TO 276: keyin$ = CHR$(0) + ""
    CASE 293 TO 308: keyin$ = CHR$(0) + "$"
    CASE 325 TO 340: keyin$ = CHR$(0) + "%"
    CASE 357 TO 372: keyin$ = CHR$(0) + "&"
    CASE 389 TO 404: keyin$ = CHR$(0) + "2"
    CASE 421 TO 436: keyin$ = CHR$(0) + "1"
    CASE 453 TO 468: keyin$ = CHR$(0) + ""
    CASE 485 TO 500: keyin$ = CHR$(0) + ""
    CASE 517 TO 532: keyin$ = CHR$(0) + ""
    CASE 549 TO 564: keyin$ = CHR$(0) + ""
    CASE 580 TO 595: keyin$ = CHR$(0) + ""
  END SELECT
  EXIT SUB
END IF
SELECT CASE mouseh
  CASE 6 TO 104
    SELECT CASE mousev
      CASE 206 TO 221: keyin$ = CHR$(0) + ";" 'F1
      CASE 222 TO 239: keyin$ = CHR$(0) + "<" 'F2
      CASE 240 TO 257: keyin$ = CHR$(0) + "="   'F3
      CASE 258 TO 275: keyin$ = CHR$(0) + ">" 'F4
      CASE 276 TO 293: keyin$ = CHR$(0) + "?" 'F5
      CASE 294 TO 311: keyin$ = CHR$(0) + "@"'F6
      CASE IS > 311: keyin$ = CHR$(0) + "A" 'F7
    END SELECT
  CASE 107 TO 206
    SELECT CASE mousev
      CASE 276 TO 293: keyin$ = CHR$(0) + "B"   'F8
      CASE 294 TO 311: keyin$ = CHR$(0) + "C" 'F9
      CASE IS > 311: keyin$ = CHR$(0) + "D" 'F10
    END SELECT
  CASE 435 TO 532
    SELECT CASE mousev
      CASE 206 TO 221: keyin$ = CHR$(1)
      CASE 222 TO 239: keyin$ = CHR$(4)
      CASE 240 TO 257: keyin$ = CHR$(16)
      CASE 258 TO 275: keyin$ = CHR$(15)
      CASE 276 TO 293: keyin$ = CHR$(18)
      CASE 294 TO 311: keyin$ = CHR$(19)
      CASE IS > 311: keyin$ = CHR$(23)
    END SELECT
  CASE 538 TO 635
    SELECT CASE mousev
      CASE 222 TO 239: keyin$ = CHR$(2)
      CASE 240 TO 257: keyin$ = CHR$(3)
      CASE 258 TO 275: keyin$ = CHR$(12)
      CASE 276 TO 293: keyin$ = CHR$(22)
      CASE 294 TO 311: keyin$ = CHR$(9)
      CASE IS > 311: keyin$ = CHR$(5)
    END SELECT
  CASE 317 TO 352
    SELECT CASE mousev
      CASE 228 TO 241: temp$ = "U" + CHR$(13)
      CASE 251 TO 271: temp$ = "NW" + CHR$(13)
      CASE 275 TO 297: temp$ = "W" + CHR$(13)
      CASE 302 TO 322: temp$ = "SW" + CHR$(13)
    END SELECT
  CASE 353 TO 384
    SELECT CASE mousev
      CASE 251 TO 271: temp$ = "N" + CHR$(13)
      CASE 302 TO 322: temp$ = "S" + CHR$(13)
    END SELECT
  CASE 385 TO 423
    SELECT CASE mousev
      CASE 228 TO 241: temp$ = "D" + CHR$(13)
      CASE 251 TO 271: temp$ = "NE" + CHR$(13)
      CASE 275 TO 297: temp$ = "E" + CHR$(13)
      CASE 302 TO 322: temp$ = "SE" + CHR$(13)
    END SELECT
END SELECT
IF temp$ <> "" THEN
  IF CarrierLost% THEN
    ermes "Need to be on-line to move.", 1
  ELSE
    transmit temp$
    IF autosrch = 1 THEN autosrch = 2
  END IF
END IF
END SUB

SUB mouseget (mouseb)
  '*** Get mouse position and button status: mouseh%, mousev% and MouseB%
  IF nomouse% THEN EXIT SUB
  InRegs.ax = 3
  CALL InterruptX(&H33, InRegs, OutRegs)
  mouseb% = OutRegs.bx
  mouseh% = OutRegs.cx
  mousev% = OutRegs.dx
END SUB

DEFSNG A-Z
SUB mouseinit
 '*** Initialize mouse driver
 IF nomouse% THEN EXIT SUB
 InRegs.ax = 0                  '** Mouse initialization
 CALL InterruptX(&H33, InRegs, OutRegs)
 IF OutRegs.ax = 0 THEN nomouse% = 1: EXIT SUB   '*** no mouse driver
 InRegs.ax = 10                 '** Mouse should blink
 InRegs.bx = &H0
 InRegs.cx = &HFFFF
 InRegs.dx = &HFF00
 CALL InterruptX(&H33, InRegs, OutRegs)
 mousespeed 7            'set speed of mouse 0-9
END SUB

SUB mousepointer (OnOff%)
   '*** mouse pointer on/off
 IF nomouse% THEN EXIT SUB
 IF OnOff% = 0 THEN OnOff% = 2 ELSE OnOff% = 1
 InRegs.ax = OnOff%
 CALL InterruptX(&H33, InRegs, OutRegs)
 IF OnOff% = 1 THEN CALL InterruptX(&H33, InRegs, OutRegs) '** to be sure
END SUB

SUB mousespeed (Speed%)
   '*** Set mouse speed
 IF nomouse% THEN EXIT SUB
 InRegs.ax = &H15
 InRegs.cx = Speed%
 InRegs.dx = Speed% * 2
 CALL InterruptX(&H33, InRegs, OutRegs)
END SUB

DEFINT A-Z
SUB recorder STATIC
file$ = "MMTERM.rec"
IF recfirst = 0 THEN
  IF CarrierLost THEN
    IF Exists(file$) THEN
      OPEN "i", 7, file$: recon = 1
      recfirst = 1
      IF LOF(7) = 0 THEN
        sw = 2: GOSUB clserec
        sw = 1: GOSUB clserec
        KILL file$
      END IF
    ELSE
      sw = 2: GOSUB clserec
    END IF
  ELSE
    OPEN "o", 7, file$: recon = 2: recfirst = 1
  END IF
ELSE
  sw = 1: GOSUB clserec
END IF
EXIT SUB
'
clserec:
SELECT CASE sw
CASE 1
  CLOSE #7: recon = 0: recfirst = 0
  IF crai <> 0 THEN crai = 0: LINE (4, 336)-(634, 344), 9, BF
CASE 2
  ermes "No Recording Found!", 1
END SELECT
RETURN
END SUB

SUB reinit (sw)
IF sw = 1 THEN EXIT SUB
IF autohide = 1 THEN DoHide
IF autosnk = 1 THEN
  transmit "sneak" + CHR$(13)
  END IF
END SUB

SUB savecfg (sw)
file$ = bbs$ + ".cfg"
z = FREEFILE
IF sw = 0 THEN
  OPEN "O", #z, file$
  PRINT #z, Port%, HS, irq%, baud&, hpt
  PRINT #z, snd, grabover, autosrch, autoheal, extra
  PRINT #z, itemalrt, autohide, atakon, caston, extra
  WRITE #z, uinput$(0), uinput$(2), uinput$(3)
  WRITE #z, alias$, ver$, temp$
ELSE
  OPEN "I", z, file$
  INPUT #z, Port%, HS, irq%, baud&, hpt
  INPUT #z, snd, grabover, autosrch, autoheal, extra
  INPUT #z, itemalrt, autohide, atakon, caston, extra
  INPUT #z, uinput$(0), uinput$(2), uinput$(3)
  INPUT #z, alias$, ver$, temp$
END IF
IF regit = 0 THEN autoheal = 0: grabover = 0
CLOSE #z
END SUB

SUB scrollit (pntlow, pnthigh, size, snd)
IF pnthigh < 22 THEN pnthigh = 22: size = 22
DO
  DO
    choice$ = INKEY$
    mouseget mouseb
    IF mouseb = 2 THEN
      choice$ = "A"
    ELSEIF mouseb = 1 THEN
      SELECT CASE mouseh
        CASE 22 TO 47: choice$ = CHR$(0) + "H"
        CASE 48 TO 71: choice$ = CHR$(0) + "P"
        CASE 72 TO 134: choice$ = CHR$(0) + "I"
        CASE 135 TO 210: choice$ = CHR$(0) + "Q"
      END SELECT
    END IF
  LOOP UNTIL choice$ <> ""
  IF LEN(choice$) = 1 THEN EXIT DO
    SELECT CASE RIGHT$(choice$, 1)
      CASE "H"
        IF pntlow = 1 THEN
          IF snd = 1 THEN BEEP
          pnthigh = 22
        ELSE
          pntlow = pntlow - 1: pnthigh = pnthigh - 1
          ScrnScroll 3, 3, 24, 79, -1, 1
          nz = DRAWSTR%(0, help(pntlow), 17, 16, 8, 7, 8)
        END IF
      CASE "P"
        IF pnthigh >= size THEN
          IF snd = 1 THEN BEEP
        ELSE
          pntlow = pntlow + 1: pnthigh = pnthigh + 1
          ScrnScroll 3, 3, 24, 79, 1, 1
          nz = DRAWSTR%(0, help(pnthigh), 17, 184, 8, 7, 8)
        END IF
      CASE "Q"
        IF pnthigh >= size THEN
          IF snd = 1 THEN BEEP
        ELSE
          pntlow = pnthigh
          pnthigh = pntlow + 22
          IF pnthigh > size THEN pnthigh = size: pntlow = size - 21
          LINE (10, 7)-(632, 193), 7, BF
          FOR x = 2 TO 23
            nz = DRAWSTR%(0, help(pntlow + x - 2), 17, x * 8, 8, 7, 8)
          NEXT x
        END IF
      CASE "I"
        IF pntlow = 1 THEN
          IF snd = 1 THEN BEEP
        ELSE
          pnthigh = pntlow
          pntlow = pnthigh - 22
          IF pntlow < 1 THEN pntlow = 1: pnthigh = 22
          LINE (10, 7)-(632, 193), 7, BF
          FOR x = 2 TO 23
            nz = DRAWSTR%(0, help(pntlow + x - 2), 17, x * 8, 8, 7, 8)
          NEXT x
        END IF
      CASE ELSE
        z = 1
    END SELECT
LOOP UNTIL z = 1
END SUB

SUB sheltodos
row% = CSRLIN: col% = POS(0)
mousepointer 0
GET (0, 0)-(639, 201), menusav
WIDTH 80, 25
SCREEN 0
COLOR 14, 0: PRINT "Type EXIT to return to MMTERM"
SHELL
SCREEN 9
WIDTH 80, 43
PUT (0, 0), menusav
DEF SEG = VARSEG(menusav(1))
BLOAD "MMTscrn0.gfx", VARPTR(menusav(1))
DEF SEG
PUT (0, 201), menusav
MouseArea 194, 330, 2, 635
x = snd: snd = 0
IF autograb = 1 THEN dowindo 5, 8, 15
IF atakon = 1 THEN dowindo 6, 8, 15
IF autosnk = 1 THEN dowindo 7, 8, 15
IF autosrch = 1 THEN dowindo 8, 8, 15
IF caston = 1 THEN dowindo 9, 8, 15
IF autohide = 1 THEN dowindo 10, 8, 15
IF log$ = "ON" THEN dowindo 19, 8, 15
snd = x
mousepointer 1
LOCATE row%, col%
END SUB

SUB starlog (log$) STATIC
row% = CSRLIN: col% = POS(0)
IF log$ = "OFF" THEN
  log$ = "ON"
  dowindo 19, 8, 15
  inbar "Enter the filename (an ext of .CAP will be added) for your file: ", file$, 8, 14, 0, 9, snd
  IF file$ <> "" THEN
    file$ = LTRIM$(file$)
    IF LEN(file$) > 8 THEN file$ = LEFT$(file$, 8)
    a = INSTR(file$, ".")
    IF a <> 0 THEN file$ = LEFT$(file$, a - 1): a = 0
    file$ = file$ + ".CAP"
    IF Exists(file$) THEN
      IF snd = 1 THEN soundfx 6
      nz = DRAWSTR%(0, "Capture File already exists, Overwrite file (Y/n)?", 99, 337, 14, 9, 8)
      DO
        choice$ = UCASE$(INKEY$)
      LOOP UNTIL choice$ <> ""
      mousepointer 0
      LINE (4, 336)-(634, 344), 9, BF
      mousepointer 1
      IF choice$ = "N" THEN
        OPEN "A", 6, file$
      ELSE
        OPEN "o", 6, file$
      END IF
    ELSE
      OPEN "o", 6, file$
    END IF
    PRINT #6, DATE$ + "      *****************************"
  ELSE
    log$ = "OFF": file$ = ""
  END IF
ELSE
  log$ = "OFF": file$ = "": CLOSE #6
END IF
IF log$ = "OFF" THEN dowindo 19, 15, 8
LOCATE row%, col%
END SUB

SUB viewfile (ext$, choice$)
STATIC map$
row% = CSRLIN: col% = POS(0)
mousepointer 0
GET (0, 0)-(639, 201), menusav

SELECT CASE UCASE$(ext$)
  CASE "S"                             'scrollback
     dowindo 18, 8, 15
     REDIM help(1 TO 140) AS STRING * 76
     FOR x = 1 TO 140: help(x) = scrollback$(x): NEXT x
     size = 140: cnt = 16: pnthigh = size: pntlow = size - 21
     GOSUB display
     dowindo 18, 15, 8
  CASE "M"
     dowindo 3, 8, 15
     mousepointer 0
     cubeit 0, 0, 640, 201, 15, 8, 7
     icon 7, 6, 627, 189, 8, 15, 8, 0
     readdir "*.MAP"
     nz = DRAWSTR%(0, "List of Maps in current directory.", 176, 185, 15, 8, 8)
     inbar "Name of Map to view (press ENTER to display last map):", file$, 8, 14, 0, 9, snd
     file$ = file$ + ".MAP"
     IF Exists(file$) THEN
       map$ = file$
       GOSUB loadndisp
     ELSE
       IF file$ = ".MAP" THEN
         IF map$ = "" THEN
           ermes "You haven't chosen a map.", 1
         ELSE
           GOSUB loadndisp
         END IF
       ELSE
         ermes "You entered an incorrect Map name.", 1
       END IF
     END IF
     dowindo 3, 15, 8
END SELECT
REDIM help(1) AS STRING * 76
LINE (0, 0)-(639, 201), 0, BF
mousepointer 0
PUT (0, 0), menusav, PSET
LINE (4, 336)-(634, 344), 9, BF
mousepointer 1
LOCATE row%, col%
EXIT SUB
'
loadndisp:
       cnt = 0: pntlow = 1: pnthigh = 22
       DO
         inline$ = BufIn$(map$, done, reading)
         cnt = cnt + 1
       LOOP UNTIL done
       IF cnt < pnthigh THEN pnthigh = cnt
       IF cnt > 1700 THEN cnt = 1700
       REDIM help(1 TO cnt) AS STRING * 76
       x = 0
       DO
         inline$ = BufIn$(map$, done, reading)
         x = x + 1
         IF x <= cnt THEN help(x) = inline$
       LOOP UNTIL done
       LINE (8, 7)-(633, 194), 7, BF
       size = cnt: cnt = 16
       GOSUB display
RETURN
'
display:
mousepointer 0
cubeit 0, 0, 640, 201, 15, 8, 7
icon 7, 6, 627, 189, 8, 15, 7, 0
FOR x = pntlow TO pnthigh
  nz = DRAWSTR%(0, help(x), 17, cnt, 8, 7, 8)
  cnt = cnt + 8
NEXT
LINE (4, 336)-(634, 344), 9, BF
nz = DRAWSTR%(0, "[     PageUp  PageDown ]   Any other key or right mouse button to return", 15, 337, 14, 9, 8)
MouseArea 335, 345, 2, 570
mousepointer 1
scrollit pntlow, pnthigh, size, snd
MouseArea 194, 330, 2, 635
RETURN
END SUB

SUB viewhelp
STATIC pntr
file$ = "MMTscrn1.gfx"
IF Exists(file$) THEN
  mousepointer 0
  GET (0, 0)-(639, 200), menusav
  REDIM helpr(1 TO 32767)
  DEF SEG = VARSEG(helpr(1))
  BLOAD file$, VARPTR(helpr(1))
  DEF SEG                 ' Restore default BASIC segment.
  PUT (0, 0), helpr, PSET
  hilite "Select a topic, then press ENTER.   Press ESC or right mouse button to exit.", 14, 9, 0, 0
  IF pntr = 0 THEN pntr = 1
  MouseArea 2, 197, 4, 636
  dowindo 13, 8, 15
  sf = 0: sb = 2: GOSUB light
  DO
    choice$ = UCASE$(INKEY$)
    mouseget mouseb
    IF mouseb <> 0 THEN
      GOSUB mchoice
    END IF
    WHILE mouseb%
      mouseget mouseb
    WEND
    IF choice$ = CHR$(27) THEN EXIT DO
    IF choice$ <> "" THEN
      GOSUB kpress
    END IF
  LOOP UNTIL choice$ = CHR$(13)
  LINE (4, 336)-(634, 344), 9, BF
  ERASE helpr
  mousepointer 0
  PUT (0, 0), menusav, PSET
  MouseArea 194, 330, 2, 635
  dowindo 13, 15, 8
  IF temp$ <> "" AND temp$ <> STRING$(8, " ") THEN
    transmit "HELP " + temp$ + CHR$(13)
  END IF
ELSE
  ermes "You are missing the MMTSCRN1.GFX, graphic file!", 1
END IF
EXIT SUB
'
light:    'highlights last selection
SELECT CASE pntr
  CASE 1 TO 16: hr = 4
  CASE 17 TO 32: hr = 95
  CASE 33 TO 48: hr = 186
  CASE 49 TO 64: hr = 277
  CASE 65 TO 80: hr = 368
  CASE 81 TO 96: hr = 459
  CASE 97 TO 112: hr = 550
END SELECT
z = pntr
DO UNTIL z < 17
  z = z - 16
LOOP
vr = (z * 12) - 6
fi = 7
IF LEN(hlp$(pntr)) > 12 THEN fi = 6
nz = DRAWSTR%(0, hlp$(pntr), hr, vr, sf, sb, fi)
RETURN
'
mchoice:
IF mouseb = 2 THEN
  choice$ = CHR$(27)
ELSE
  mousepointer 0
  sf = 4: sb = 7: GOSUB light
  y = 1
  SELECT CASE mouseh
    CASE 92 TO 182: y = 17
    CASE 183 TO 273: y = 33
    CASE 274 TO 364: y = 49
    CASE 365 TO 455: y = 65
    CASE 456 TO 546: y = 81
    CASE 547 TO 637: y = 97
  END SELECT
  DO UNTIL mousev < 15
    y = y + 1
    mousev = mousev - 12
  LOOP
  IF y = pntr THEN temp$ = hlp$(pntr): choice$ = CHR$(27)
  pntr = y
  sf = 0: sb = 2: GOSUB light
  mousepointer 1
END IF
RETURN
'
kpress:
mousepointer 0
sf = 4: sb = 7: GOSUB light
SELECT CASE RIGHT$(choice$, 1)
  CASE CHR$(13)
   temp$ = hlp$(pntr)
  CASE "P"
    pntr = pntr + 1
    IF pntr > 112 THEN pntr = 1
    sf = 0: sb = 2: GOSUB light
  CASE "H"
    pntr = pntr - 1
    IF pntr = 0 THEN pntr = 112
    sf = 0: sb = 2: GOSUB light
  CASE "K"  'left
    IF pntr > 16 THEN pntr = pntr - 16
    sf = 0: sb = 2: GOSUB light
  CASE "M"  'left
    IF pntr < 97 THEN pntr = pntr + 16
    IF pntr > 112 THEN pntr = 112
    sf = 0: sb = 2: GOSUB light
END SELECT
mousepointer 1
RETURN
END SUB

