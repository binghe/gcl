
(COMMON-LISP::IN-PACKAGE "COMMON-LISP-USER") 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION (COMMON-LISP::T) COMMON-LISP::*)
         XLIB::WINDOW-UNSET XLIB::WINDOW-GET-GEOMETRY
         XLIB::WINDOW-SET-INVERT XLIB::WINDOW-FONT-INFO
         XLIB::GET-ST-POINT XLIB::EDITMENU-YANK
         XLIB::WINDOW-INIT-MOUSE-POLL XLIB::WINDOW-SET-XOR
         XLIB::WINDOW-TOP-NEG-Y XLIB::WINDOW-LEFT
         XLIB::WINDOW-QUERY-POINTER XLIB::TEXTMENU-DRAW
         XLIB::EDITMENU-CARAT XLIB::EDITMENU-DRAW
         XLIB::WINDOW-STD-LINE-ATTR XLIB::WINDOW-UNMAP
         XLIB::WINDOW-QUERY-POINTER-B XLIB::WINDOW-BACKGROUND
         XLIB::EDITMENU-DELETE XLIB::WINDOW-MOVE XLIB::DOWINDOWCOM
         XLIB::WINDOW-SYNC XLIB::PICMENU-DRAW XLIB::WINDOW-MAP
         XLIB::WINDOW-RESET-COLOR XLIB::EDITMENU-KILL
         XLIB::BARMENU-DRAW XLIB::WINDOW-GET-GEOMETRY-B
         XLIB::MENU-CLEAR XLIB::WINDOW-RESET XLIB::WINDOW-WFUNCTION
         XLIB::MENU-DRAW XLIB::WINDOW-FOREGROUND XLIB::WINDOW-CLEAR
         XLIB::EDITMENU-BACKSPACE XLIB::WINDOW-DRAW-BORDER
         XLIB::LISP-STRING XLIB::WINDOW-SET-ERASE)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION (COMMON-LISP::*) COMMON-LISP::T)
         XLIB::OPEN-WINDOW)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION (COMMON-LISP::T COMMON-LISP::*)
             COMMON-LISP::T)
         XLIB::WINDOW-GET-ELLIPSE XLIB::EDITMENU-SELECT
         XLIB::WINDOW-SET-XCOLOR XLIB::TEXTMENU-SELECT
         XLIB::PICMENU-SELECT XLIB::MAKECONT XLIB::WINDOW-GET-CIRCLE
         XLIB::MENU XLIB::WINDOW-GET-REGION XLIB::TEXTMENU-SET-TEXT
         XLIB::MENU-SELECT XLIB::BARMENU-SELECT
         XLIB::PICMENU-CREATE-FROM-SPEC XLIB::PRINTINDEX
         XLIB::EDITMENU-EDIT XLIB::MENU-CREATE)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION (COMMON-LISP::T COMMON-LISP::T)
             COMMON-LISP::*)
         XLIB::BARMENU-UPDATE-VALUE XLIB::WINDOW-FONT-STRING-WIDTH
         XLIB::MENU-FIND-ITEM-WIDTH XLIB::WINDOW-STRING-WIDTH
         XLIB::PICMENU-BOX-ITEM XLIB::WINDOW-SET-FOREGROUND
         XLIB::WINDOW-INVERTAREA XLIB::PICMENU-UNBOX-ITEM
         XLIB::PICMENU-DRAW-NAMED-BUTTON XLIB::WINDOW-SET-CURSOR
         XLIB::WINDOW-SET-LINE-WIDTH XLIB::PICMENU-DELETE-NAMED-BUTTON
         XLIB::EDITMENU-ERASE XLIB::PICMENU-DRAW-BUTTON
         XLIB::WINDOW-SET-BACKGROUND)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION COMMON-LISP::NIL COMMON-LISP::*)
         XLIB::XINIT XLIB::WINDOW-SCREEN-HEIGHT)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T)
             (COMMON-LISP::VALUES COMMON-LISP::T COMMON-LISP::T))
         XLIB::WINDOW-CIRCLE-RADIUS)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::T COMMON-LISP::*)
             COMMON-LISP::*)
         XLIB::WINDOW-XOR-BOX-XY XLIB::WINDOW-DRAW-BOX-CORNERS
         XLIB::WINDOW-DRAW-LINE-XY XLIB::WINDOW-DRAW-ARROW2-XY
         XLIB::WINDOW-DRAW-ARROW-XY XLIB::WINDOW-DRAW-ELLIPSE-XY
         XLIB::WINDOW-ERASE-BOX-XY XLIB::WINDOW-DRAW-BOX-XY
         XLIB::WINDOW-DRAW-ARROWHEAD-XY)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T)
             COMMON-LISP::*)
         XLIB::WINDOW-COPY-AREA-XY)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T)
             COMMON-LISP::*)
         XLIB::WINDOW-PRETTYPRINTAT XLIB::MENU-UNBOX-ITEM
         XLIB::WINDOW-PRINTAT XLIB::WINDOW-DRAW-CROSSHAIRS-XY
         XLIB::WINDOW-MOVETO-XY XLIB::WINDOW-INVERT-AREA
         XLIB::WINDOW-DRAW-DOT-XY XLIB::WINDOW-DRAW-CARAT
         XLIB::WINDOW-ERASE-AREA XLIB::MENU-BOX-ITEM
         XLIB::WINDOW-DRAW-CROSS-XY)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::*)
             COMMON-LISP::*)
         XLIB::WINDOW-DRAW-CIRCLE-XY XLIB::WINDOW-PRINT-LINE)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T)
             COMMON-LISP::*)
         XLIB::WINDOW-PRETTYPRINTAT-XY XLIB::WINDOW-DRAW-CIRCLE-PT
         XLIB::EDITMENU-DISPLAY XLIB::WINDOW-PRINTAT-XY
         XLIB::WINDOW-PROCESS-CHAR-EVENT XLIB::MENU-DISPLAY-ITEM)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::T)
             COMMON-LISP::*)
         XLIB::WINDOW-ADJ-BOX-XY)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::*)
             COMMON-LISP::*)
         XLIB::WINDOW-DRAW-ARC-XY)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::T)
             COMMON-LISP::*)
         XLIB::WINDOW-DRAW-ELLIPSE-PT XLIB::WINDOW-ERASE-AREA-XY
         XLIB::WINDOW-INVERT-AREA-XY XLIB::WINDOW-DRAW-VECTOR-PT)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::*)
             COMMON-LISP::*)
         XLIB::WINDOW-DRAW-LINE XLIB::WINDOW-DRAW-BOX
         XLIB::WINDOW-DRAW-CIRCLE)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::*)
             COMMON-LISP::*)
         XLIB::WINDOW-DRAW-RCBOX-XY)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T)
             COMMON-LISP::*)
         XLIB::WINDOW-DRAW-LATEX-XY)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::*)
             COMMON-LISP::*)
         XLIB::WINDOW-SET-LINE-ATTR)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T)
             COMMON-LISP::*)
         XLIB::WINDOW-DRAW-BOX-LINE-XY)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION (COMMON-LISP::T COMMON-LISP::T)
             COMMON-LISP::T)
         XLIB::WINDOW-POSITIVE-Y XLIB::WINDOW-STRING-EXTENTS
         XLIB::MENU-CHOOSE XLIB::WINDOW-SET-FONT XLIB::PUSHFONT
         XLIB::WINDOW-STRING-HEIGHT XLIB::WORDLIST<
         XLIB::EDITMENU-LINE-Y XLIB::MENU-ITEM-Y
         XLIB::MENU-FIND-ITEM-HEIGHT XLIB::XFERCHARS
         XLIB::WINDOW-CENTEROFFSET XLIB::MENU-FIND-ITEM-Y
         XLIB::EDITMENU-CHAR XLIB::MENU-ITEM-VALUE
         XLIB::MENU-FIND-ITEM)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION (COMMON-LISP::T COMMON-LISP::*)
             COMMON-LISP::*)
         XLIB::WINDOW-FREE-COLOR)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION COMMON-LISP::NIL COMMON-LISP::T)
         XLIB::SEARCHFORALPHA XLIB::SAFE-CHAR XLIB::WINDOW-XINIT
         XLIB::WINDOW-MENU XLIB::WINDOW-INIT-KEYMAP XLIB::PARSE-INT
         XLIB::WINDOW-DESTROY-SELECTED-WINDOW
         XLIB::WINDOW-GET-MOUSE-POSITION)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION COMMON-LISP::NIL COMMON-LISP::FIXNUM)
         XLIB::FLUSHLINE)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T)
             COMMON-LISP::T)
         XLIB::PICMENU-BUTTON-CONTAINSXY? XLIB::MENU-MOVETO-XY
         XLIB::WINDOW-GET-BOX-SIZE XLIB::PRINTINDEXN
         XLIB::WINDOW-GET-LINE-POSITION
         XLIB::PICMENU-SET-NAMED-BUTTON-COLOR XLIB::EDITMENU-SETXY
         XLIB::MENU-SELECT-B XLIB::MENU-REPOSITION-LINE
         XLIB::WINDOW-GET-VECTOR-END)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::*)
             COMMON-LISP::T)
         XLIB::WINDOW-CREATE XLIB::WINDOW-TRACK-MOUSE
         XLIB::PICMENU-ITEM-POSITION XLIB::WINDOW-GET-CHARS
         XLIB::TEXTMENU-CREATE XLIB::EDITMENU-CREATE XLIB::TOHTML
         XLIB::WINDOW-SET-COLOR XLIB::MENU-ITEM-POSITION)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::*)
             COMMON-LISP::T)
         XLIB::WINDOW-INPUT-STRING XLIB::PICMENU-CREATE-SPEC
         XLIB::WINDOW-SET-COLOR-RGB XLIB::WINDOW-PRINT-LINES
         XLIB::PICMENU-CREATE)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::*)
             COMMON-LISP::T)
         XLIB::WINDOW-GET-ICON-POSITION XLIB::BARMENU-CREATE
         XLIB::WINDOW-GET-LATEX-POSITION XLIB::WINDOW-GET-BOX-POSITION)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::T COMMON-LISP::*)
             COMMON-LISP::T)
         XLIB::WINDOW-EDIT XLIB::WINDOW-TRACK-MOUSE-IN-REGION)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T)
             COMMON-LISP::T)
         XLIB::WINDOW-ADJUST-BOX-SIDE XLIB::EDITMENU-EDIT-FN)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION
             (COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::T COMMON-LISP::T
                 COMMON-LISP::T COMMON-LISP::*)
             COMMON-LISP::T)
         XLIB::WINDOW-GET-BOX-LINE-POSITION)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION (COMMON-LISP::T) COMMON-LISP::T)
         XLIB::WINDOW-DESTROY XLIB::EDITMENU-CALCULATE-SIZE
         XLIB::STRINGIFY XLIB::DOLINE XLIB::PUSHENV
         XLIB::WINDOW-POLL-MOUSE XLIB::WINDOW-FONT XLIB::WINDOW-SIZE
         XLIB::EDITMENU-END XLIB::WINDOW-PAINT XLIB::WINDOW-GEOMETRY
         XLIB::MENU-DESTROY XLIB::WINDOW-LABEL
         XLIB::PICMENU-CALCULATE-SIZE XLIB::POPENV XLIB::WINDOW-PARENT
         XLIB::WINDOW-WAIT-UNMAP XLIB::EDITMENU-INIT
         XLIB::WINDOW-GET-POINT XLIB::MENU-SELECT!
         XLIB::MENU-CALCULATE-SIZE XLIB::BARMENU-INIT XLIB::DOCOMMAND
         XLIB::MENU-INIT XLIB::WINDOW-OPEN XLIB::EDITMENU-META-B
         XLIB::WINDOW-GET-RAW-CHAR XLIB::WINDOW-DRAWABLE-HEIGHT
         XLIB::MENU-REPOSITION XLIB::WINDOW-YPOSITION
         XLIB::EDITMENU-ALPHANUMBERICP XLIB::EDITMENU-NEXT
         XLIB::MENU-SIZE XLIB::EDITMENU-PREVIOUS XLIB::EDITMENU-FORWARD
         XLIB::EDITMENU-BEGINNING XLIB::PICMENU-DESTROY
         XLIB::WINDOW-RESET-GEOMETRY XLIB::WINDOW-GCONTEXT
         XLIB::EDITMENU-BACKWARD XLIB::TERMLINE
         XLIB::WINDOW-DRAWABLE-WIDTH XLIB::WINDOW-GET-CROSSHAIRS
         XLIB::BARMENU-CALCULATE-SIZE XLIB::WINDOW-CHAR-DECODE
         XLIB::DOTABULAR XLIB::PICMENU-INIT XLIB::WINDOW-WAIT-EXPOSURE
         XLIB::PARSE-WORD XLIB::TEXTMENU-INIT XLIB::SEARCHFOR
         XLIB::MENU-OFFSET XLIB::MENU-ADJUST-OFFSET
         XLIB::WINDOW-SET-COPY XLIB::TEXTMENU-CALCULATE-SIZE
         XLIB::WINDOW-GET-CROSS XLIB::EDITMENU-META-F
         XLIB::WINDOW-GET-CLICK XLIB::EDITMENU-CURRENT-CHAR
         XLIB::DOHTML XLIB::WINDOW-CLOSE XLIB::EDITMENU-RETURN
         XLIB::WINDOW-CODE-CHAR)) 
(COMMON-LISP::PROCLAIM
    '(COMMON-LISP::FTYPE
         (COMMON-LISP::FUNCTION (COMMON-LISP::*) COMMON-LISP::*)
         XLIB::WINDOW-FORCE-OUTPUT)) 