DECLARE FUNCTION grid$ ()
DECLARE SUB main ()
DECLARE SUB radar ()
DECLARE SUB loadgpxs ()
DECLARE SUB disp (title$, cl%, row%)
DECLARE SUB cubeit (h%, v%, WD%, HT%, C1%, C2%, FILD%)
DECLARE SUB icon (h%, v%, WD%, HT%, C1%, C2%, FILD%, OUTIN%)
DECLARE SUB smallprint (spx%, spy%, sp$, spcolor%)
DECLARE SUB tinyfont (hor%, ver%, inline$, clr%)
DECLARE FUNCTION Exists% (filen$)
DECLARE FUNCTION DRAWSTR% (mode%, strg$, x0%, y0%, fg%, bg%, gap%)
DECLARE SUB DRAWDOT (mode%, value%, x0%, y0%)

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
DIM SHARED inregs AS RegTypeX, outregs AS RegTypeX
DECLARE SUB InterruptX (intnum AS INTEGER, inregs AS RegTypeX, outregs AS RegTypeX)

DEFINT A-Z
COMMON SHARED pat$, spce
REDIM SHARED MENUSAV(1 TO 32767)
DATA Abilities,Actions,Agility,Aide,Ansi,Apprentice,Attack
DATA Backstab,Banking,Bards,Borrow,Broadcasting,Buy
DATA Chaos,Charm,Classes,Class Powers,Class Skills1,Class Skills2
DATA Clerics,Combat,Commands,Communications,Critical
DATA Dark-Elf,Dismiss,Drag,Drink,Druids,Dwarf
'#,Dying
DATA Elf,Encumbrance,Exit,Experience
DATA Feedback,Fire,Follow,Forgive
DATA Gnomes,Goblins,Gypsy
DATA Half-Elfs,Half-Ogres,Half-Orcs,Halflings,Healer,Health,Help,Hp,Humans
DATA Info,Intellect,Items,Join
'# jewellry
DATA Kai,Kang,Keys
DATA Laws,Leave,Levels,Lives,Lives/cps,Look
DATA Mages,Magicres,Misc1,Missionary,Movement,Mystic,Open
DATA Paladins,Party,Practice,Priests,Profile,Quests,Quiet
DATA Races,Rangers,Release,Reroll,Response,Rest,Rob,Rogues
DATA Set,Set2,Share,Shops,Silver,Spell,Spells,Stash
DATA Stats1,Stats2,Stealth,Strength
'shrine, street
DATA Thief,Tips,Title,Topics,Tracking,Training,Unkown
'#text
DATA Warlocks,Warriors,Wealth,Weapons,Wisdom,Witchunters
DATA "","","",""

spce = 1
pat$ = CHR$(191) + CHR$(191) + CHR$(191) + CHR$(72)
pat$ = pat$ + CHR$(223) + CHR$(223) + CHR$(223) + CHR$(36)
pat$ = pat$ + CHR$(239) + CHR$(239) + CHR$(239) + CHR$(18)
pat$ = pat$ + CHR$(253) + CHR$(253) + CHR$(253) + CHR$(18)
pat$ = pat$ + CHR$(251) + CHR$(251) + CHR$(251) + CHR$(36)
pat$ = pat$ + CHR$(247) + CHR$(247) + CHR$(247) + CHR$(72)

WIDTH 80, 43
SCREEN 9 '#, , 0, 1
COLOR 7, 0
main
END
  x = 2: z = 3
  FOR y = 1 TO 112
    icon x, z, 89, 11, 15, 8, 7, 0
    x = x + 91
    SELECT CASE y
      CASE 7: x = 2: z = 15
      CASE 14: x = 2: z = 27
      CASE 21: x = 2: z = 39
      CASE 28: x = 2: z = 51
      CASE 35: x = 2: z = 63
      CASE 42: x = 2: z = 75
      CASE 49: x = 2: z = 87
      CASE 56: x = 2: z = 99
      CASE 63: x = 2: z = 111
      CASE 70: x = 2: z = 123
      CASE 77: x = 2: z = 135
      CASE 84: x = 2: z = 147
      CASE 91: x = 2: z = 159
      CASE 98: x = 2: z = 171
      CASE 105: x = 2: z = 183
    END SELECT
  NEXT y
x = 2: z = 3
FOR y = 1 TO 112
  READ sel$
  IF LEN(sel$) > 12 THEN wid = 6 ELSE wid = 7
 
  NZ = DRAWSTR%(0, sel$, x + 3, z + 3, 4, 7, wid)
  z = z + 12
  SELECT CASE y
   CASE 16: x = 93: z = 3
   CASE 32: x = 184: z = 3
   CASE 48: x = 275: z = 3
   CASE 64: x = 366: z = 3
   CASE 80: x = 457: z = 3
   CASE 96: x = 548: z = 3
  END SELECT
NEXT y

GET (0, 0)-(639, 200), MENUSAV
DEF SEG = VARSEG(MENUSAV(1))
BSAVE "MMTSCRN1.GFX", VARPTR(MENUSAV(1)), 65534
DEF SEG                 ' Restore default BASIC segment.

CLS : REDIM MENUSAV(1 TO 32767)

DEF SEG = VARSEG(MENUSAV(1))
BLOAD "MMTSCRN1.GFX", VARPTR(MENUSAV(1))
DEF SEG                 ' Restore default BASIC segment.
PUT (0, 0), MENUSAV
END

SUB EXTRA
icon 120, 237, 158, 57, 8, 15, 7, 0   'EXPLOR
  icon 122, 239, 100, 25, 15, 8, 7, 0
  icon 125, 241, 94, 12, 8, 15, 3, 0
icon 176, 267, 100, 25, 15, 8, 7, 0
icon 179, 269, 94, 12, 8, 15, 3, 0
  icon 122, 267, 50, 25, 15, 8, 7, 0
  icon 125, 269, 44, 12, 8, 15, 3, 0
icon 226, 239, 50, 25, 15, 8, 7, 0
icon 229, 241, 44, 12, 8, 15, 3, 0
icon 186, 206, 90, 16, 15, 8, 7, 0
icon 189, 208, 84, 12, 8, 15, 3, 0
'
x = 130
tinyfont x - 1, 290, "KNOWN", 15
tinyfont x + 1, 290, "KNOWN", 8
tinyfont x, 290, "KNOWN", 5
tinyfont x + 67, 290, "LAST PATH", 15
tinyfont x + 69, 290, "LAST PATH", 8
tinyfont x + 68, 290, "LAST PATH", 5
tinyfont x + 10, 262, "GAME NAME", 15
tinyfont x + 12, 262, "GAME NAME", 8
tinyfont x + 11, 262, "GAME NAME", 5
tinyfont x + 113, 262, "SD", 15
tinyfont x + 115, 262, "SD", 8
tinyfont x + 114, 262, "SD", 5
icon 120, 208, 56, 23, 8, 15, 7, 0

END SUB

DEFSNG A-Z
FUNCTION grid$

'little metal pop up buttons
  pattern$ = CHR$(255) + CHR$(255) + CHR$(255) + CHR$(0)
  pattern$ = pattern$ + CHR$(255) + CHR$(255) + CHR$(255) + CHR$(124)
  pattern$ = pattern$ + CHR$(251) + CHR$(251) + CHR$(251) + CHR$(68)
  pattern$ = pattern$ + CHR$(251) + CHR$(251) + CHR$(251) + CHR$(68)
  pattern$ = pattern$ + CHR$(251) + CHR$(251) + CHR$(251) + CHR$(68)
  pattern$ = pattern$ + CHR$(251) + CHR$(251) + CHR$(251) + CHR$(68)
  pattern$ = pattern$ + CHR$(195) + CHR$(195) + CHR$(195) + CHR$(124)
  pattern$ = pattern$ + CHR$(255) + CHR$(255) + CHR$(255) + CHR$(0)

grid$ = pattern$
END FUNCTION

DEFINT A-Z
SUB main
CLS
clrb = 7: clrh = 15
icon 0, 331, 640, 18, clrh, 8, clrb, 0     'bottom bar
icon 2, 333, 636, 14, 8, clrh, clrb, 0
LINE (4, 335)-(634, 345), 9, BF
icon 0, 202, 640, 128, clrh, 8, clrb, 0     'upperbar
x = 4: z = 204
FOR y = 1 TO 10
  icon x, z, 99, 16, 15, 8, 7, 0
  tinyfont x + 4, z + 11, "F" + LTRIM$(STR$(y)), 8
  tinyfont x + 6, z + 11, "F" + LTRIM$(STR$(y)), 15
  tinyfont x + 5, z + 11, "F" + LTRIM$(STR$(y)), 4
  z = z + 18
  IF y = 7 THEN x = x + 103: z = 204 + (4 * 18)
NEXT y
x = 534: z = 205
tinyfont x + 20, z + 10, "CTRL KEYS", clrh
tinyfont x + 22, z + 10, "CTRL KEYS", 8
tinyfont x + 21, z + 10, "CTRL KEYS", 4
x = 434: z = 204
FOR y = 1 TO 13
  icon x, z, 99, 16, 15, 8, 7, 0
  z = z + 18
  IF y = 7 THEN x = x + 103: z = 222
NEXT y
LINE (106, 205)-(106, 273), 8
LINE (107, 273)-(208, 273), 15
LINE (209, 273)-(209, 327), 8
LINE (430, 205)-(430, 327), 15
LINE (107, 205)-(429, 205), 8
LINE (210, 327)-(429, 327), 15
x = 112: y = 212
NZ = DRAWSTR%(0, "To move: Use keypad or click on the direction", x, y, 0, clrb, 7)
NZ = DRAWSTR%(0, " Left mouse button: Select", x, y + 15, 0, clrb, 7)
NZ = DRAWSTR%(0, "Right mouse button: Deselect", x, y + 25, 0, clrb, 7)
NZ = DRAWSTR%(0, "<ESC> to EXIT MMTERM", x + 24, y + 43, 0, clrb, 7)
LINE (223, 280)-(296, 320), 1, BF
PAINT (230, 300), grid$, 7
x = 312: y = 247
icon x, y, 114, 76, 8, clrh, 7, 0
LINE (x + 31, y + 21)-(x + 84, y + 55), 6
LINE (x + 31, y + 55)-(x + 84, y + 21), 6
LINE (x + 25, y + 38)-(x + 90, y + 38), 6
LINE (x + 57, y + 18)-(x + 57, y + 58), 6
NZ = DRAWSTR%(0, "", x + 54, y + 34, 6, 7, 8)
tinyfont x + 13, y + 41, "W", 15
tinyfont x + 12, y + 41, "W", 1
tinyfont x + 100, y + 41, "E", 15
tinyfont x + 99, y + 41, "E", 1
tinyfont x + 56, y + 13, "N", 15
tinyfont x + 55, y + 13, "N", 1
tinyfont x + 56, y + 70, "S", 15
tinyfont x + 55, y + 70, "S", 1
tinyfont x + 18, y + 18, "NW", 15
tinyfont x + 17, y + 18, "NW", 1
tinyfont x + 86, y + 18, "NE", 15
tinyfont x + 85, y + 18, "NE", 1
tinyfont x + 18, y + 67, "SW", 15
tinyfont x + 17, y + 67, "SW", 1
tinyfont x + 86, y + 67, "SE", 15
tinyfont x + 85, y + 67, "SE", 1
FOR x = 317 TO 420 STEP 60
  icon x, 227, 46, 14, 15, 8, 7, 0
NEXT x
tinyfont 334, 237, "UP", 15
tinyfont 334, 237, "UP", 1
tinyfont 389, 237, "DOWN", 15
tinyfont 388, 237, "DOWN", 1
'
z = 215: x = 456
tinyfont x + 14, z, "A", 4
tinyfont x + 13, z, "A", 4
tinyfont x + 1, z + 1, "DI L BBS", 8
tinyfont x, z, "DI L BBS", 14
z = 233: x = 443
tinyfont x + 1, z + 1, "SHELL TO  OS", 8
tinyfont x, z, "SHELL TO  OS", 14
tinyfont x + 62, z, "D", 4
tinyfont x + 63, z, "D", 4
z = 233: x = 550
tinyfont x, z + 1, "SCROLL  ACK", 8
tinyfont x - 1, z, "SCROLL  ACK", 14
tinyfont x + 48, z, "B", 4
tinyfont x + 47, z, "B", 4
z = 269: x = 574
tinyfont x, z, "L", 4
tinyfont x + 1, z, "L", 4
tinyfont x + 10, z + 1, "OOK", 8
tinyfont x + 9, z, "OOK", 14
'
z = 215: x = 42
tinyfont x + 1, z + 1, "MACROS", 8
tinyfont x, z, "MACROS", 14
z = 233: x = 42
tinyfont x + 1, z + 1, "CONFIG", 8
tinyfont x, z, "CONFIG", 14
z = 251: x = 50
tinyfont x + 1, z + 1, "MAPS", 8
tinyfont x, z, "MAPS", 14
z = 269: x = 32
tinyfont x + 1, z + 1, "AUTO HEAL", 8
tinyfont x, z, "AUTO HEAL", 14
z = 287: x = 32
tinyfont x + 1, z + 1, "AUTO GRAB", 8
tinyfont x, z, "AUTO GRAB", 14
z = 287: x = 129
tinyfont x + 1, z + 1, "AUTO SEARCH", 8
tinyfont x, z, "AUTO SEARCH", 14
z = 305: x = 26
tinyfont x + 1, z + 1, "AUTO ATTACK", 8
tinyfont x, z, "AUTO ATTACK", 14
z = 305: x = 136
tinyfont x + 1, z + 1, "AUTO CAST", 8
tinyfont x, z, "AUTO CAST", 14
z = 323: x = 30
tinyfont x + 1, z + 1, "AUTO SNEAK", 8
tinyfont x, z, "AUTO SNEAK", 14
z = 323: x = 138
tinyfont x + 1, z + 1, "AUTO HIDE", 8
tinyfont x, z, "AUTO HIDE", 14
'
z = 287: x = 563
tinyfont x, z, "V", 4
tinyfont x + 1, z, "V", 4
tinyfont x + 10, z + 1, "ERBOSE", 8
tinyfont x + 9, z, "ERBOSE", 14
z = 287: x = 470
tinyfont x + 1, z + 1, "B IEF", 8
tinyfont x, z, "B IEF", 14
tinyfont x + 9, z, "R", 4
tinyfont x + 8, z, "R", 4
z = 269: x = 454
tinyfont x, z, "O", 4
tinyfont x + 1, z, "O", 4
tinyfont x + 10, z + 1, "PEN DOOR", 8
tinyfont x + 9, z, "PEN DOOR", 14
z = 251: x = 462
tinyfont x + 1, z + 1, "MM HEL", 8
tinyfont x, z, "MM HEL", 14
tinyfont x + 42, z, "P", 4
tinyfont x + 43, z, "P", 4
z = 251: x = 544
tinyfont x, z, "C", 4
tinyfont x + 1, z, "C", 4
tinyfont x + 10, z + 1, "APTURE FILE", 8
tinyfont x + 9, z, "APTURE FILE", 14
z = 305: x = 469
tinyfont x, z, "S", 4
tinyfont x + 1, z, "S", 4
tinyfont x + 10, z + 1, "TATS", 8
tinyfont x + 9, z, "TATS", 14
z = 305: x = 555
tinyfont x, z, "I", 4
tinyfont x + 1, z, "I", 4
tinyfont x + 10, z + 1, "NVENTORY", 8
tinyfont x + 9, z, "NVENTORY", 14
z = 323: x = 465
tinyfont x + 10, z + 1, "EALTH", 8
tinyfont x + 9, z, "EALTH", 14
tinyfont x, z, "W", 4
tinyfont x + 1, z, "W", 4
z = 323: x = 561
tinyfont x + 1, z + 1, "XPERIENCE", 8
tinyfont x, z, "XPERIENCE", 14
tinyfont x - 8, z, "E", 4
tinyfont x - 9, z, "E", 4

'tinyfont 591, 308, "MMTERM", 13
'tinyfont 590, 307, "MMTERM", 14
'tinyfont 601, 320, "1.0", 13
'tinyfont 600, 319, "1.0", 14
'#icon 284, 225, 354, 60, 8, 11, 3, 0
'#LINE (285, 226)-(636, 283), 8, B
'#PAINT (290, 231), pat$, 8
'icon 244, 266, 270, 17, clrh, 8, clrb, 0     'reg bar
'icon 246, 268, 266, 13, 8, clrh, 9, 0
'NZ = DRAWSTR%(0, "Licensed to:", 260, 271, 15, 9, 8)
GET (0, 201)-(639, 349), MENUSAV
DEF SEG = VARSEG(MENUSAV(1))
BSAVE "MMTSCRN0.GFX", VARPTR(MENUSAV(1)), 48127
DEF SEG                 ' Restore default BASIC segment.
END SUB

