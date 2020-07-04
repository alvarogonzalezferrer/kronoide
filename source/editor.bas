DECLARE SUB SalvarNivel (n AS INTEGER)
DECLARE SUB CargarNivel (n AS INTEGER)
DECLARE SUB SetearBloque (Bl AS ANY, B AS INTEGER)
DECLARE SUB PonerBloque (x AS INTEGER, y AS INTEGER, n AS INTEGER)
DECLARE SUB DrawBoard ()
DECLARE SUB dialogbox (Titulo$, ancho!, alto!, x!, y!)
DECLARE SUB box (x!, y!, XX!, YY!, tipolinea!)
DECLARE FUNCTION confirmar! (texto$, x!, y!)
DECLARE SUB llena (x!, y!, XX!, YY!, f$)
DECLARE FUNCTION CheckLevelEmpty! ()
DECLARE SUB CleanNivel ()
DECLARE SUB Presenta ()
'--------------------------------------------------
'Kronoide - Editor de niveles
'--------------------------------------------------
'Un Ark*n**d* en Qbasic
'Por Kronoman
'Copyright (c) 2002-2020
'Licencia GNU - Software libre
'En memoria de mi querido padre
'Hecho en Argentina
'--------------------------------------------------

'--------------------------------------------------
'Estructuras de datos
'--------------------------------------------------

'Estructura de bloque
TYPE BLOKE
    c1 AS INTEGER 'color del bloque
    c2 AS INTEGER 'color de sombra
    c3 AS INTEGER 'color de luz
    v AS INTEGER 'esta vivo? <> 0 = SI
END TYPE

'--------------------------------------------------
'Globales
'--------------------------------------------------

'Matriz de bloques de juego
DIM SHARED Tbl(0 TO 9, 0 TO 9) AS BLOKE 'bloques de 32x12 pixels,

DIM SHARED Bloque AS BLOKE 'Bloque actual
DIM SHARED Bloc AS INTEGER
Bloc = 1
DIM SHARED XC AS INTEGER
DIM SHARED YC AS INTEGER

DIM Salir AS INTEGER
Salir = 0

'--------------------------------------------------
'Implementacion
'--------------------------------------------------
RANDOMIZE TIMER

SCREEN 7
DO
    CALL Presenta
   
    CALL CleanNivel
    d$ = "DRAW!"
    DO
        CALL SetearBloque(Bloque, Bloc)

        IF d$ <> "" THEN
            CALL DrawBoard
            PCOPY 2, 0
        END IF
     
        d$ = UCASE$(INKEY$)

        IF d$ = CHR$(0) + "K" THEN XC = XC - 1
        IF d$ = CHR$(0) + "M" THEN XC = XC + 1
     
        IF d$ = CHR$(0) + "H" THEN YC = YC - 1
        IF d$ = CHR$(0) + "P" THEN YC = YC + 1
        IF d$ = CHR$(27) THEN Salir = -1

        IF XC < 0 THEN XC = 0
        IF YC < 0 THEN YC = 0
        IF XC > 9 THEN XC = 9
        IF YC > 9 THEN YC = 9
     
     
        IF d$ = " " THEN CALL PonerBloque(XC, YC, Bloc)
      
        IF d$ = CHR$(13) OR d$ = "T" THEN Bloc = Bloc + 1
        IF Bloc > 10 THEN Bloc = 1

        IF d$ = "C" THEN CALL CleanNivel 'Limpiar
        IF d$ = "R" THEN 'Random
            CALL CleanNivel
            z1 = INT(RND * 50) + 50
            FOR z2 = 0 TO z1
                z3 = INT(RND * 10) + 1
                CALL SetearBloque(Bloque, INT(z3))
                CALL PonerBloque(INT(RND * 10), INT(RND * 10), INT(z3))
            NEXT
        END IF

        IF d$ = "L" THEN 'Cargar
            SCREEN 7, 0, 0, 0
            CLS
            FILES
            PRINT "Numero de nivel a cargar? (archivo == nivel.n)"
            PRINT "COLOQUE SOLO EL NUMERO!"
            LINE INPUT a$
            a$ = LTRIM$(RTRIM$(a$))
            IF a$ <> "" THEN
                CALL CargarNivel(VAL(a$))
            END IF

        END IF
    
       
        IF d$ = "S" THEN 'Salvar
            SCREEN 7, 0, 0, 0
            CLS
            FILES
            PRINT "Numero de nivel a SALVAR? (archivo == nivel.n)"
            PRINT "COLOQUE SOLO EL NUMERO!"
            LINE INPUT a$
            a$ = LTRIM$(RTRIM$(a$))
            IF a$ <> "" THEN
                CALL SalvarNivel(VAL(a$))
            END IF

        END IF

        IF Salir THEN
            SCREEN 7, 0, 0, 0
            IF confirmar("Cancelar edicion?", 3, 10) = 0 THEN Salir = 0
        END IF


   
    LOOP UNTIL Salir
   
    IF Salir THEN
        SCREEN 7, 0, 0, 0
        IF confirmar("Realmente desea salir?", 3, 10) = 0 THEN Salir = 0
    END IF

LOOP UNTIL Salir


SCREEN 0
WIDTH 80, 25
CLS
COLOR 7, 0

SYSTEM


'--------------------------------------------------
'Manejos de errores
'--------------------------------------------------

'--------------------------------------------------
'Error cargando nivel
'--------------------------------------------------
nohaynivel:
SCREEN 0
WIDTH 80, 25
CLS
PRINT "ERROR: el nivel no existe!"
SYSTEM

fallosalvar:
SCREEN 0
WIDTH 80, 25
CLS
PRINT "ERROR: no se pudo salvar el nivel!"
SYSTEM

SUB box (x, y, XX, YY, tipolinea)
    '------------------------------------------------
    'dibuja una caja de dialogo
    'desde x,y a xx,yy con tipolinea:
    'tipolinea=0 comun
    'tipolinea=1 doble
    'tipolinea=2 gruesa
    '------------------------------------------------

    IF XX < x THEN SWAP XX, x
    IF YY < y THEN SWAP YY, y

    IF tipolinea < 0 OR tipolinea > 2 OR x < 1 OR x > 80 OR XX < x OR XX > 80 THEN EXIT SUB
    IF y < 1 OR y > 23 OR YY < y OR YY > 23 THEN EXIT SUB

    IF tipolinea = 0 THEN ray1$ = "≥": ray2$ = "ƒ": esq1$ = "⁄": esq2$ = "ø": esq3$ = "¿":: esq4$ = "Ÿ"
    IF tipolinea = 1 THEN ray1$ = "∫": ray2$ = "Õ": esq1$ = "…": esq2$ = "ª": esq3$ = "»":: esq4$ = "º"
    IF tipolinea = 2 THEN ray1$ = "€": ray2$ = "€": esq1$ = "€": esq2$ = "€": esq3$ = "€":: esq4$ = "€"


    FOR k = y TO YY
        LOCATE k, x: PRINT ray1$
        LOCATE k, XX: PRINT ray1$
    NEXT
    FOR k = x TO XX
        LOCATE y, k: PRINT ray2$
        LOCATE YY, k: PRINT ray2$
    NEXT
    LOCATE y, x: PRINT esq1$
    LOCATE y, XX: PRINT esq2$
    LOCATE YY, x: PRINT esq3$
    LOCATE YY, XX: PRINT esq4$


END SUB

SUB CargarNivel (n AS INTEGER)
    'Carga el nivel en archivo nivel.n
    ON ERROR GOTO nohaynivel

    CALL CleanNivel
    OPEN "NIVEL." + LTRIM$(RTRIM$(STR$(n))) FOR INPUT AS #1
    y = 0
    x = 0
    DO WHILE NOT EOF(1)
        LINE INPUT #1, a$
        a$ = LEFT$(a$, 10)
        x = 0
        FOR i = 1 TO LEN(a$)
            IF MID$(a$, i, 1) <> " " THEN
                Tbl(x, y).v = 1
           
                'elegir color
                SELECT CASE MID$(a$, i, 1)
                    CASE "1"
                        Tbl(x, y).c1 = 12
                        Tbl(x, y).c2 = 4
                        Tbl(x, y).c3 = 15
                
                    CASE "2"
                        Tbl(x, y).c1 = 10
                        Tbl(x, y).c2 = 2
                        Tbl(x, y).c3 = 15
                
                    CASE "3"
                        Tbl(x, y).c1 = 13
                        Tbl(x, y).c2 = 5
                        Tbl(x, y).c3 = 15
                
                    CASE "4"
                        Tbl(x, y).c1 = 9
                        Tbl(x, y).c2 = 8
                        Tbl(x, y).c3 = 11
                
                    CASE "5"
                        Tbl(x, y).c1 = 7
                        Tbl(x, y).c2 = 8
                        Tbl(x, y).c3 = 15
                    CASE "6"
                        Tbl(x, y).c1 = 6
                        Tbl(x, y).c2 = 8
                        Tbl(x, y).c3 = 14
                    CASE "7"
                        Tbl(x, y).c1 = 3
                        Tbl(x, y).c2 = 8
                        Tbl(x, y).c3 = 11
                    CASE "8"
                        Tbl(x, y).c1 = 8
                        Tbl(x, y).c2 = 8
                        Tbl(x, y).c3 = 7
                    CASE "9"
                        Tbl(x, y).c1 = 5
                        Tbl(x, y).c2 = 8
                        Tbl(x, y).c3 = 13
                
                    CASE "0"
                        Tbl(x, y).c1 = 1
                        Tbl(x, y).c2 = 8
                        Tbl(x, y).c3 = 9


                    CASE ELSE
                        Tbl(x, y).c1 = 7
                        Tbl(x, y).c2 = 8
                        Tbl(x, y).c3 = 15
                END SELECT
           
            END IF
            x = x + 1
            IF x > 9 THEN EXIT FOR
        NEXT
        y = y + 1
        IF y > 9 THEN EXIT DO

    LOOP
    CLOSE 1
    ON ERROR GOTO 0

    EXIT SUB

END SUB

FUNCTION CheckLevelEmpty

    'Devuelve 0 si el nivel tiene bloques, -1 si no hay bloques
    'Ideal para ver si hay que pasar de nivel

    FOR x = 0 TO 9
        FOR y = 0 TO 9
            IF Tbl(x, y).v <> 0 THEN
                CheckLevelEmpty = 0
                EXIT FUNCTION
            END IF

        NEXT
    NEXT

    CheckLevelEmpty = -1
END FUNCTION

SUB CleanNivel

    'Limpia un nivel
    FOR x = 0 TO 9
        FOR y = 0 TO 9
            Tbl(x, y).v = 0
        NEXT
    NEXT

END SUB

FUNCTION confirmar (texto$, x, y)
    '------------------------------------------------
    'Fncion que poone un cuadro de confirmacion en pantalla
    'en posicion x,y con texto$
    'permite responder <SI> o <NO>
    'Devuelve 1=SI, 0=NO
    '(c) 2001, Kronoman
    '------------------------------------------------
 
    'usa la rutina dialogbox para hacer un cuadro de dialogo
    IF LEN(texto$) < 20 THEN
        CALL llena(x, y, x + 21, y + 5, " ") 'limpiar
        CALL dialogbox(texto$, 20, 5, x, y) 'dibujar
    ELSE
        CALL llena(x, y, x + LEN(texto$) + 3, y + 5, " ") 'limpiar
        CALL dialogbox(texto$, LEN(texto$) + 3, 5, x, y) 'dibujar
    END IF

    'colocar los carteles SI, NO
    LOCATE y + 4, x + 1
    PRINT " S =  SI, N = NO "


    'esperar por tecla S/N

    DO
        k$ = UCASE$(INPUT$(1))
        IF k$ <> "N" AND k$ <> "S" THEN BEEP
    LOOP WHILE k$ <> "S" AND k$ <> "N"


    'devolver
    IF k$ = "S" THEN
        confirmar = 1
    ELSE
        confirmar = 0
    END IF

END FUNCTION

SUB dialogbox (Titulo$, ancho, alto, x, y)
    '------------------------------------------------
    'Dibuja una caja de dialogo (cool!)
    'Poner en titulo$ el titulo
    'En alto, el alto (y)  del cuadro, en ancho el ancho (x) del cuadro
    'en X,y la posicion del cuadro
    '------------------------------------------------
    CALL box(x, y, x + ancho, y + alto, 1)
    LOCATE y + 2, x + 2
    PRINT Titulo$

    'CALL box(X + 1, Y + 1, X + LEN(Titulo$) + 2, Y + 3, 0)'titulo

END SUB

SUB DrawBoard


    'Dibuja el tablero en la pagina de video 2
    SCREEN 7, 0, 2, 0
    LINE (0, 0)-(320, 200), 0, BF

    FOR x = 0 TO 9
        FOR y = 0 TO 9
            IF Tbl(x, y).v > 0 THEN
                LINE (x * 32, y * 12)-(x * 32 + 31, y * 12 + 11), Tbl(x, y).c1, BF
       
                LINE (x * 32, y * 12)-(x * 32 + 31, y * 12 + 11), Tbl(x, y).c2, B
       
                LINE (x * 32, y * 12)-(x * 32, y * 12 + 11), Tbl(x, y).c3
                LINE (x * 32, y * 12)-(x * 32 + 31, y * 12), Tbl(x, y).c3

            END IF
        NEXT
    NEXT

    x = XC
    y = YC
       
    LINE (x * 32, y * 12)-(x * 32 + 31, y * 12 + 11), 15, B
    LINE (x * 32, y * 12)-(x * 32 + 31, y * 12 + 11), 15
    LINE (x * 32, y * 12 + 11)-(x * 32 + 31, y * 12), 15
     
    'Bloque inferior
    x = 0
    y = 15
    LINE (x * 32, y * 12)-(x * 32 + 31, y * 12 + 11), Bloque.c1, BF
      
    LINE (x * 32, y * 12)-(x * 32 + 31, y * 12 + 11), Bloque.c2, B
      
    LINE (x * 32, y * 12)-(x * 32, y * 12 + 11), Bloque.c3
    LINE (x * 32, y * 12)-(x * 32 + 31, y * 12), Bloque.c3


    'Teclas
    LOCATE 20, 1
    PRINT CHR$(24); CHR$(25); CHR$(26); CHR$(27); " Mover - BARRA on/off - T tipo"
    PRINT "L cargar - S salvar - ESC salir -"
    PRINT "C limpiar - R random"

END SUB

SUB llena (x, y, XX, YY, f$)
    '------------------------------------------------
    'llena un area de texto
    'desde x,y a xx,yy con f$
    '------------------------------------------------
    IF XX < x THEN SWAP XX, x
    IF YY < y THEN SWAP YY, y

    IF x < 1 OR x > 80 OR XX < x OR XX > 80 THEN EXIT SUB
    IF y < 1 OR y > 23 OR YY < y OR YY > 23 THEN EXIT SUB

    FOR k = y TO YY
        LOCATE k, x: PRINT STRING$(XX - x, f$)
    NEXT


END SUB

SUB OkBox (texto$, x, y)
    '------------------------------------------------
    'Fncion que pone un cuadro de < Aceptar > en pantalla
    'en posicion x,y con texto$
    'Es solo para presentar mensajes en pantalla, y esperar por una tecla.
    '(c) 2001, Kronoman
    '------------------------------------------------

    'usa la rutina dialogbox para hacer un cuadro de dialogo
    IF LEN(texto$) < 18 THEN
        CALL llena(x, y, x + 21, y + 5, " ") 'limpiar
        CALL dialogbox(texto$, 20, 5, x, y) 'dibujar
    ELSE
        CALL llena(x, y, x + LEN(texto$) + 3, y + 5, " ") 'limpiar
        CALL dialogbox(texto$, LEN(texto$) + 3, 5, x, y) 'dibujar
    END IF

    'colocar el cartele < Aceptar >
    LOCATE y + 4, x + 1
    PRINT " < Aceptar > "


    'esperar por tecla
    DO
    LOOP UNTIL INKEY$ <> ""

END SUB

SUB PonerBloque (x AS INTEGER, y AS INTEGER, n AS INTEGER)
    'Si el bloque ya esta seteado, lo borra, sino, lo pone
    IF Tbl(x, y).v > 0 THEN
        Tbl(x, y).v = 0
    ELSE
        Tbl(x, y).c1 = Bloque.c1
        Tbl(x, y).c2 = Bloque.c2
        Tbl(x, y).c3 = Bloque.c3
        Tbl(x, y).v = n
    END IF
END SUB

SUB Presenta
    SCREEN 7, 0, 0, 0
    CLS
    COLOR 10
    PRINT " "
    PRINT " ‹€  €                       ‹      ‹ﬂ"
    PRINT "  € €                        €      ‹ﬂ"
    PRINT "  €€   ‹‹‹  ‹‹‹ ‹  ‹ ‹‹‹ ﬂ ‹‹€  ‹‹‹ ‹ﬂ"
    PRINT "  € €   € € € € €€ € € € € € €  €‹€ ‹ﬂ"
    PRINT " ‹€  €  €   €‹€ € ﬂ€ €‹€ € €‹€‹ €‹‹ ‹ﬂ"
    PRINT " ‹ ‹ ‹ ‹ ‹ ‹ ‹ ‹ ‹ ‹ ‹ ‹ ‹ ‹ ‹ ‹ ‹ ‹ ﬂ"
    PRINT "ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ ﬂ"
    PRINT
    COLOR 14
    PRINT "EDITOR DE NIVELES"
    COLOR 14
    PRINT "Por Kronoman"
    COLOR 11
    PRINT "Copyright (c) 2002-2020"
    PRINT "Licencia GNU - Software libre"
    PRINT "En memoria de mi querido padre"
    PRINT "Hecho en Argentina"
    COLOR 7
    LOCATE 23, 1
    PRINT "Presione una tecla..."
    DO: LOOP WHILE INKEY$ = ""

END SUB

SUB SalvarNivel (n AS INTEGER)
    'Salva el nivel en archivo nivel.n
    ON ERROR GOTO fallosalvar

    OPEN "NIVEL." + LTRIM$(RTRIM$(STR$(n))) FOR OUTPUT AS #1

    FOR y = 0 TO 9
        a$ = ""
        FOR x = 0 TO 9
            IF Tbl(x, y).v = 0 THEN
                a$ = a$ + " "
            ELSE
                IF Tbl(x, y).v < 9 THEN
                    a$ = a$ + LEFT$(LTRIM$(RTRIM$(STR$(Tbl(x, y).v))), 1)
                ELSE
                    a$ = a$ + "0"
                END IF
            END IF
        NEXT
        PRINT #1, a$
    NEXT

    CLOSE 1
    ON ERROR GOTO 0

    EXIT SUB

END SUB

SUB SetearBloque (Bl AS BLOKE, B AS INTEGER)
    'Setea el Bl al bloque Nß B

    a$ = LEFT$(LTRIM$(RTRIM$(STR$(B))), 1)
    IF B > 9 THEN a$ = "0" ' para evitar un bug con bloque tipo 10

    'elegir color
    SELECT CASE a$
        CASE "1"
            Bl.c1 = 12
            Bl.c2 = 4
            Bl.c3 = 15

        CASE "2"
            Bl.c1 = 10
            Bl.c2 = 2
            Bl.c3 = 15
               
        CASE "3"
            Bl.c1 = 13
            Bl.c2 = 5
            Bl.c3 = 15
               
        CASE "4"
            Bl.c1 = 9
            Bl.c2 = 8
            Bl.c3 = 11
               
        CASE "5"
            Bl.c1 = 7
            Bl.c2 = 8
            Bl.c3 = 15
        CASE "6"
            Bl.c1 = 6
            Bl.c2 = 8
            Bl.c3 = 14
        CASE "7"
            Bl.c1 = 3
            Bl.c2 = 8
            Bl.c3 = 11
        CASE "8"
            Bl.c1 = 8
            Bl.c2 = 8
            Bl.c3 = 7
        CASE "9"
            Bl.c1 = 5
            Bl.c2 = 8
            Bl.c3 = 13
               
        CASE "0"
            Bl.c1 = 1
            Bl.c2 = 8
            Bl.c3 = 9


        CASE ELSE
            Bl.c1 = 7
            Bl.c2 = 8
            Bl.c3 = 15
    END SELECT


END SUB

