DECLARE SUB IluminaBloque (X!, Y!)
DECLARE SUB OkBox (texto$, X!, Y!)
DECLARE SUB dialogbox (Titulo$, ancho!, alto!, X!, Y!)
DECLARE SUB box (X!, Y!, XX!, YY!, tipolinea!)
DECLARE FUNCTION confirmar! (texto$, X!, Y!)
DECLARE SUB llena (X!, Y!, XX!, YY!, f$)
DECLARE FUNCTION CheckLevelEmpty! ()
DECLARE SUB RebotarX ()
DECLARE SUB RebotarY ()
DECLARE SUB DrawGame ()
DECLARE SUB IniciarGame ()
DECLARE SUB DrawBoard ()
DECLARE SUB CargarNivel (n AS INTEGER)
DECLARE SUB CleanNivel ()
DECLARE SUB DoTempo ()
DECLARE SUB Presenta ()
'--------------------------------------------------
'Kronoide
'--------------------------------------------------
'Un Ark*n**d* en Qbasic
'Por Kronoman
'Copyright (c) 2002 - Licencia GNU - Software libre
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
   V AS INTEGER 'esta vivo? <> 0 = SI
END TYPE

TYPE JUGTIPE 'jugador(es)
  X AS INTEGER 'coordenadas
  Y AS INTEGER

  W AS INTEGER 'ancho paleta
  H AS INTEGER 'alto paleta
  c1 AS INTEGER 'color fondo
  c2 AS INTEGER 'color borde
  c3 AS INTEGER 'color sombra
  SP AS INTEGER 'velocidad X de paleta
  BALL AS INTEGER 'Tiene la bola pegada <> 0 = SI
  Vidas AS INTEGER ' cantidad de vidas
END TYPE

TYPE BALLTIPE 'bola(s)
  X AS INTEGER 'coordenadas, multiplicadas por 10; 320 = 3200; 320.1 = 3201
  Y AS INTEGER 'idem anterior, pero Y
  XD AS INTEGER 'Direccion X*10; 3.2 = 32
  YD AS INTEGER 'Direccion Y*10
  c1 AS INTEGER 'color bola
  c2 AS INTEGER 'color borde
END TYPE
'--------------------------------------------------
'Globales
'--------------------------------------------------

'Matriz de bloques de juego
DIM SHARED Tbl(0 TO 9, 0 TO 9) AS BLOKE 'bloques de 32x12 pixels,

'Jugador
DIM SHARED Jug AS JUGTIPE

'Pelota
DIM SHARED Bola AS BALLTIPE

'Temporizador de CPU x 1 segundo
DIM SHARED Temporizador AS LONG
'Relacion de temporizador (divide a Temporizador, se ajusta automaticamente)
DIM relacion AS LONG
relacion = 500 'default...
DIM looptempo AS LONG
DIM loopfor AS LONG
looptempo = 0

'Nivel actual
DIM SHARED Nivel AS INTEGER

DIM Salir  AS INTEGER
Salir = 0
DIM PasarNivel AS INTEGER
PasarNivel = 0
DIM Perdio AS INTEGER
Perdio = 0

'--------------------------------------------------
'Implementacion
'--------------------------------------------------
SCREEN 0
CLS
COLOR 7, 0
PRINT "Temporizando CPU... espere por favor..."
CALL DoTempo
PRINT Temporizador
PRINT "RANDOM: Semilla "; TIMER
RANDOMIZE TIMER

SCREEN 7

DO

    CALL Presenta
    
   
    Jug.Vidas = 3
    Nivel = 0
    PasarNivel = -1

    'Loop de juego pasando niveles
    DO
             CALL IniciarGame
             IF PasarNivel THEN CALL CargarNivel(Nivel)
             CALL DrawBoard
             Perdio = 0
             PasarNivel = 0

             
             'Loop de juego en un nivel
             DO
              'Teclado
              D$ = UCASE$(INKEY$)
              IF D$ = CHR$(0) + "K" OR D$ = "A" THEN Jug.X = Jug.X - Jug.SP
              IF D$ = CHR$(0) + "M" OR D$ = "D" THEN Jug.X = Jug.X + Jug.SP
              IF D$ = CHR$(27) THEN Salir = -1
              IF D$ = "P" THEN
                   SCREEN 7, 0, 0, 0
                   CALL OkBox("PAUSA - <ENTER> para continuar", 4, 10)
              END IF

              IF D$ = " " THEN Jug.BALL = 0 'Soltar la bola...

              'debug: TRUCO - ELIMINAR - F12
              IF D$ = CHR$(0) + "Ü" THEN CALL CleanNivel
                
              

              'Verificar limites paleta
              IF Jug.X - Jug.W / 2 < 0 THEN Jug.X = Jug.W / 2
              IF Jug.X + Jug.W / 2 > 320 THEN Jug.X = 320 - Jug.W / 2
             
              'Tiene la bola pegada?
              IF Jug.BALL THEN
                Bola.X = Jug.X * 10
                Bola.Y = (Jug.Y - Jug.H / 2 - 4) * 10
              ELSE
                 'Mover pelota
                 Bola.X = Bola.X + Bola.XD
                 Bola.Y = Bola.Y + Bola.YD

                 'Chequear si paso nivel
                 IF Bola.Y < 1250 OR Bola.Y > Jug.Y * 10 THEN
                      IF CheckLevelEmpty THEN
                            PasarNivel = -1
                            Nivel = Nivel + 1
                      END IF
                 END IF


                 'Ver si pego en paleta
                 IF Bola.YD > 0 THEN
                 IF Bola.X / 10 >= Jug.X - Jug.W / 2 AND Bola.X / 10 <= Jug.X + Jug.W / 2 THEN
                    IF Bola.Y / 10 >= Jug.Y - Jug.H / 2 - 4 AND Bola.Y / 10 <= Jug.Y + Jug.H THEN
                        Bola.Y = (Jug.Y - Jug.H / 2 - 4) * 10
                        Bola.YD = 1
                        CALL RebotarY

                        'Segun el punto de impacto y la direccion X, cambia de direccion
                        IF Bola.X / 10 > Jug.X + Jug.W * .75 THEN
                            IF Bola.XD > 0 THEN
                               CALL RebotarX
                            END IF
                        END IF
                        IF Bola.X / 10 < Jug.X - Jug.W * .75 THEN
                            IF Bola.XD < 0 THEN
                               CALL RebotarX
                            END IF
                        END IF
                    END IF
                 END IF
                END IF
                
                 'Ver bloques
                 IF Bola.Y < 1250 THEN
                     
                     XD = INT(Bola.X / 320)
                     YD = INT(Bola.Y / 120)
                    
                     IF XD < 0 THEN XD = 0
                     IF YD < 0 THEN YD = 0
                     IF XD > 9 THEN XD = 9
                     IF YD > 9 THEN YD = 9

                     IF Tbl(XD, YD).V > 0 THEN
                         
                          'Intento de evitar efecto 'doble rebote'
                          '   Bola.X = Bola.X - (Bola.XD * 1.7)
                          '   Bola.Y = Bola.Y - (Bola.YD * 1.7)
                        
                          'Tbl(XD, YD).V = Tbl(XD, YD).V - 1 'deteriorar bloque
                          'IF Tbl(XD, YD).V < 0 THEN Tbl(XD, YD).V = 0
                                                 
                          'Todos los bloques se rompen al contacto [me embola esperar...]
                          Tbl(XD, YD).V = 0
                         
                          CALL RebotarY
                          IF RND < .5 THEN CALL RebotarX

                          CALL IluminaBloque(XD, YD)
                        
                          'IF Tbl(XD, YD).V = 0 THEN ' murio bloque?
                          '  'Sumar puntos aqui, explotar bloque, etc
                          'ELSE
                          '  'No murio, iluminar
                          '  CALL IluminaBloque(XD, YD)
                          'END IF
                         
                          'Redibujar
                          CALL DrawBoard
                     END IF
                     

                END IF
                
                
                 'Ver limites mapa
                 IF Bola.X < 0 OR Bola.X > 3200 THEN
                     IF Bola.X < 0 THEN Bola.X = 4
                     IF Bola.X > 3200 THEN Bola.X = 3160
                     CALL RebotarX
                 END IF
                
                 IF Bola.Y < 0 THEN
                     Bola.Y = 4
                     CALL RebotarY
                     IF Bola.XD = 0 THEN CALL RebotarX
                 END IF

                 IF Bola.Y > 2040 THEN
                    'Perdio una vida
                    Jug.Vidas = Jug.Vidas - 1
                    Perdio = -1
                 END IF

              END IF 'fin bola pegada


              CALL DrawGame
             
              'Pausar
              idle2 = TIMER
              IF relacion = 0 THEN relacion = 1 'autoajuste de temporizador
              IF relacion > 5000 THEN relacion = 5000
              looptempo = Temporizador / relacion
              FOR loopfor = 0 TO looptempo
              'Hacer algo aqui, sino funciona muy rapido
               idle = idle + 1
               idle = 0
               idle = loopfor
              NEXT
              
             
              IF ABS(TIMER - idle2) < .02 THEN relacion = relacion - 1 'alentejar
              IF ABS(TIMER - idle2) > .02 THEN relacion = relacion + 1 'acelerar



                IF Salir THEN
                    SCREEN 7, 0, 0, 0
                    IF confirmar("Realmente desea finalizar el juego?", 2, 3) = 0 THEN Salir = 0
                END IF


             LOOP UNTIL Salir OR Perdio OR PasarNivel
            
             SCREEN 7, 0, 0, 0
             'Mostrar si perdio o paso de nivel aca!
             IF Perdio AND Jug.Vidas >= 0 THEN
                CALL OkBox("Perdiste una vida!", 7, 7)
             END IF

             IF PasarNivel THEN
                CALL OkBox("Pasaste de nivel!!", 7, 7)
             END IF

    LOOP UNTIL Salir OR Jug.Vidas < 0
SCREEN 7, 0, 0, 0

    
    IF Salir THEN
        IF confirmar("Realmente desea salir?", 3, 10) = 0 THEN Salir = 0
    ELSE
    'Mostrar GAME OVER aca!
        CALL OkBox("FIN DEL JUEGO!", 11, 5)
    END IF

LOOP UNTIL Salir


SCREEN 0
WIDTH 80, 25
CLS
COLOR 7, 0
COLOR 15, 0
PRINT "Kronoide(r)"
PRINT "Copyright (c) 2002, Kronoman"
PRINT "En memoria de mi querido padre"
COLOR 14, 0
PRINT "Programa Gratuito"
PRINT
COLOR 7
PRINT "Hecho en ";
COLOR 11
PRINT "Arg";
COLOR 15
PRINT "e";
COLOR 14
PRINT "n";
COLOR 15
PRINT "t";
COLOR 11
PRINT "ina "

COLOR 3, 0
PRINT "‹‹‹‹‹‹‹‹‹‹"
COLOR 15, 3
PRINT "ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ"
COLOR 7, 0
PRINT
PRINT "Factor de velocidad CPU: "; Temporizador; " /"; relacion; " = "; CINT(Temporizador / relacion)
PRINT
PRINT
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
PRINT "ERROR: el nivel "; Nivel; " no existe!"
PRINT "Posiblemente jugaste todos los niveles!"
PRINT "Felicitaciones..."
SYSTEM

SUB box (X, Y, XX, YY, tipolinea)
'------------------------------------------------
'dibuja una caja de dialogo
'desde x,y a xx,yy con tipolinea:
'tipolinea=0 comun
'tipolinea=1 doble
'tipolinea=2 gruesa
'------------------------------------------------

IF XX < X THEN SWAP XX, X
IF YY < Y THEN SWAP YY, Y

IF tipolinea < 0 OR tipolinea > 2 OR X < 1 OR X > 80 OR XX < X OR XX > 80 THEN EXIT SUB
IF Y < 1 OR Y > 23 OR YY < Y OR YY > 23 THEN EXIT SUB

IF tipolinea = 0 THEN ray1$ = "≥": ray2$ = "ƒ": esq1$ = "⁄": esq2$ = "ø": esq3$ = "¿": : esq4$ = "Ÿ"
IF tipolinea = 1 THEN ray1$ = "∫": ray2$ = "Õ": esq1$ = "…": esq2$ = "ª": esq3$ = "»": : esq4$ = "º"
IF tipolinea = 2 THEN ray1$ = "€": ray2$ = "€": esq1$ = "€": esq2$ = "€": esq3$ = "€": : esq4$ = "€"


FOR k = Y TO YY
LOCATE k, X: PRINT ray1$
LOCATE k, XX: PRINT ray1$
NEXT
FOR k = X TO XX
LOCATE Y, k: PRINT ray2$
LOCATE YY, k: PRINT ray2$
NEXT
LOCATE Y, X: PRINT esq1$
LOCATE Y, XX: PRINT esq2$
LOCATE YY, X: PRINT esq3$
LOCATE YY, XX: PRINT esq4$


END SUB

SUB CargarNivel (n AS INTEGER)
'Carga el nivel en archivo nivel.n
ON ERROR GOTO nohaynivel

CALL CleanNivel
OPEN "NIVEL." + LTRIM$(RTRIM$(STR$(n))) FOR INPUT AS #1
   Y = 0
   X = 0
   DO WHILE NOT EOF(1)
       LINE INPUT #1, a$
       a$ = LEFT$(a$, 10)
       X = 0
       FOR i = 1 TO LEN(a$)
           IF MID$(a$, i, 1) <> " " THEN
            Tbl(X, Y).V = 1 'default = 1
            
            'elegir color
                SELECT CASE MID$(a$, i, 1)
                 CASE "1"
                    Tbl(X, Y).c1 = 12
                    Tbl(X, Y).c2 = 4
                    Tbl(X, Y).c3 = 15
                    Tbl(X, Y).V = 1
                
                 CASE "2"
                    Tbl(X, Y).c1 = 10
                    Tbl(X, Y).c2 = 2
                    Tbl(X, Y).c3 = 15

                    Tbl(X, Y).V = 2

                 CASE "3"
                    Tbl(X, Y).c1 = 13
                    Tbl(X, Y).c2 = 5
                    Tbl(X, Y).c3 = 15
                   
                    Tbl(X, Y).V = 2
                
                 CASE "4"
                    Tbl(X, Y).c1 = 9
                    Tbl(X, Y).c2 = 8
                    Tbl(X, Y).c3 = 11
                   
                    Tbl(X, Y).V = 3
                
                 CASE "5"
                    Tbl(X, Y).c1 = 7
                    Tbl(X, Y).c2 = 8
                    Tbl(X, Y).c3 = 15
                   
                    Tbl(X, Y).V = 3

                 CASE "6"
                    Tbl(X, Y).c1 = 6
                    Tbl(X, Y).c2 = 8
                    Tbl(X, Y).c3 = 14

                    Tbl(X, Y).V = 3

                 CASE "7"
                    Tbl(X, Y).c1 = 3
                    Tbl(X, Y).c2 = 8
                    Tbl(X, Y).c3 = 11
                   
                    Tbl(X, Y).V = 4

                 CASE "8"
                    Tbl(X, Y).c1 = 8
                    Tbl(X, Y).c2 = 8
                    Tbl(X, Y).c3 = 7

                    Tbl(X, Y).V = 4

                 CASE "9"
                    Tbl(X, Y).c1 = 5
                    Tbl(X, Y).c2 = 8
                    Tbl(X, Y).c3 = 13
                   
                    Tbl(X, Y).V = 5

                 CASE "0"
                    Tbl(X, Y).c1 = 1
                    Tbl(X, Y).c2 = 8
                    Tbl(X, Y).c3 = 9
                   
                    Tbl(X, Y).V = 5

                 CASE ELSE
                    Tbl(X, Y).c1 = 7
                    Tbl(X, Y).c2 = 8
                    Tbl(X, Y).c3 = 15
                   
                    Tbl(X, Y).V = 1

                END SELECT
           
           END IF
           X = X + 1
           IF X > 9 THEN EXIT FOR
       NEXT
       Y = Y + 1
       IF Y > 9 THEN EXIT DO

   LOOP
CLOSE 1
ON ERROR GOTO 0

EXIT SUB

END SUB

FUNCTION CheckLevelEmpty

'Devuelve 0 si el nivel tiene bloques, -1 si no hay bloques
'Ideal para ver si hay que pasar de nivel

FOR X = 0 TO 9
FOR Y = 0 TO 9
    IF Tbl(X, Y).V <> 0 THEN
        CheckLevelEmpty = 0
        EXIT FUNCTION
    END IF

NEXT
NEXT

    CheckLevelEmpty = -1
END FUNCTION

SUB CleanNivel

'Limpia un nivel
FOR X = 0 TO 9
FOR Y = 0 TO 9
    Tbl(X, Y).V = 0
NEXT
NEXT

END SUB

FUNCTION confirmar (texto$, X, Y)
'------------------------------------------------
'Fncion que poone un cuadro de confirmacion en pantalla
'en posicion x,y con texto$
'permite responder <SI> o <NO>
'Devuelve 1=SI, 0=NO
'(c) 2001, Kronoman
'------------------------------------------------
 
'usa la rutina dialogbox para hacer un cuadro de dialogo
IF LEN(texto$) < 20 THEN
  CALL llena(X, Y, X + 21, Y + 5, " ") 'limpiar
  CALL dialogbox(texto$, 20, 5, X, Y) 'dibujar
ELSE
  CALL llena(X, Y, X + LEN(texto$) + 3, Y + 5, " ") 'limpiar
  CALL dialogbox(texto$, LEN(texto$) + 3, 5, X, Y) 'dibujar
END IF

  'colocar los carteles SI, NO
  LOCATE Y + 4, X + 1
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

SUB dialogbox (Titulo$, ancho, alto, X, Y)
'------------------------------------------------
'Dibuja una caja de dialogo (cool!)
'Poner en titulo$ el titulo
'En alto, el alto (y)  del cuadro, en ancho el ancho (x) del cuadro
'en X,y la posicion del cuadro
'------------------------------------------------
CALL box(X, Y, X + ancho, Y + alto, 1)
LOCATE Y + 2, X + 2
PRINT Titulo$

'CALL box(X + 1, Y + 1, X + LEN(Titulo$) + 2, Y + 3, 0)'titulo

END SUB

SUB DoTempo

'Temporiza el CPU para mantener una velocidad constante con un timer
'de alta resolucion
t = TIMER
Temporizador = 0
DO
    Temporizador = Temporizador + 1
    IF Temporizador > 2000000000 THEN Temporizador = 2000000000 'evita overflow en CPUs rapidos
LOOP UNTIL ABS(TIMER - t) > 1


END SUB

SUB DrawBoard


'Dibuja el tablero en la pagina de video 2
SCREEN 7, 0, 2, 0
LINE (0, 0)-(320, 200), 0, BF

FOR X = 0 TO 9
FOR Y = 0 TO 9
    IF Tbl(X, Y).V > 0 THEN
        LINE (X * 32, Y * 12)-(X * 32 + 31, Y * 12 + 11), Tbl(X, Y).c1, BF
       
        LINE (X * 32, Y * 12)-(X * 32 + 31, Y * 12 + 11), Tbl(X, Y).c2, B
       
        LINE (X * 32, Y * 12)-(X * 32, Y * 12 + 11), Tbl(X, Y).c3
        LINE (X * 32, Y * 12)-(X * 32 + 31, Y * 12), Tbl(X, Y).c3

    END IF
NEXT
NEXT


'Dibuja las vidas
Y = 5
FOR X = 0 TO Jug.Vidas
    CIRCLE (Y, 190), 4, 10
    PAINT (Y, 190), 10, 10
    Y = Y + 10
NEXT

END SUB

SUB DrawGame

'Dibuja el cuadro actual de juego en pagina 1 y lo envia a pantalla
'evita flickering

SCREEN 7, 0, 1, 0
PCOPY 2, 1

'Jugador
 LINE (Jug.X - Jug.W / 2, Jug.Y - Jug.H / 2)-(Jug.X + Jug.W / 2, Jug.Y + Jug.H / 2), Jug.c1, BF
 LINE (Jug.X - Jug.W / 2, Jug.Y - Jug.H / 2)-(Jug.X + Jug.W / 2, Jug.Y + Jug.H / 2), Jug.c2, B

 LINE (Jug.X - Jug.W / 2, Jug.Y + Jug.H / 2)-(Jug.X + Jug.W / 2, Jug.Y + Jug.H / 2), Jug.c3
 LINE (Jug.X + Jug.W / 2, Jug.Y - Jug.H / 2)-(Jug.X + Jug.W / 2, Jug.Y + Jug.H / 2), Jug.c3

'Bola
 'LINE (Bola.X / 10 - 1, Bola.Y / 10 - 2)-(Bola.X / 10 + 1, Bola.Y / 10 + 2), Bola.c1, BF
 CIRCLE (Bola.X / 10, Bola.Y / 10), 2, Bola.c2
 PAINT (Bola.X / 10, Bola.Y / 10), Bola.c1, Bola.c2


'LOCATE 23, 1
'PRINT INT(Bola.X / 320), INT(Bola.Y / 120)

PCOPY 1, 0


END SUB

SUB IluminaBloque (X, Y)
'Auxiliar, ilumina un bloque
SCREEN 7, 0, 0, 0
LINE (X * 32, Y * 12)-(X * 32 + 31, Y * 12 + 11), 15, BF

SOUND 37, .1 'snd inaudible,pausa

END SUB

SUB IniciarGame
'--------------------------------------------------
'Inicializa el juego por primera vez o despues de cada nivel
'--------------------------------------------------

 'Jugador
 Jug.X = 160
 Jug.Y = 180
 Jug.W = 60
 Jug.H = 5
 Jug.SP = INT(Jug.W * .75)
 Jug.BALL = -1
 Jug.c1 = 7
 Jug.c2 = 15
 Jug.c3 = 8

 'Bola
 Bola.X = 1600
 Bola.Y = (Jug.Y - Jug.H / 2 - 4) * 10
 Bola.XD = 0
 Bola.YD = -(20 * RND + 35)
 Bola.c1 = 15
 Bola.c2 = 7

END SUB

SUB llena (X, Y, XX, YY, f$)
'------------------------------------------------
'llena un area de texto
'desde x,y a xx,yy con f$
'------------------------------------------------
IF XX < X THEN SWAP XX, X
IF YY < Y THEN SWAP YY, Y

IF X < 1 OR X > 80 OR XX < X OR XX > 80 THEN EXIT SUB
IF Y < 1 OR Y > 23 OR YY < Y OR YY > 23 THEN EXIT SUB

FOR k = Y TO YY
LOCATE k, X: PRINT STRING$(XX - X, f$)
NEXT


END SUB

SUB OkBox (texto$, X, Y)
'------------------------------------------------
'Fncion que pone un cuadro de < Aceptar > en pantalla
'en posicion x,y con texto$
'Es solo para presentar mensajes en pantalla, y esperar por una tecla.
'(c) 2001, Kronoman
'------------------------------------------------

'usa la rutina dialogbox para hacer un cuadro de dialogo
IF LEN(texto$) < 18 THEN
  CALL llena(X, Y, X + 21, Y + 5, " ") 'limpiar
  CALL dialogbox(texto$, 20, 5, X, Y) 'dibujar
ELSE
  CALL llena(X, Y, X + LEN(texto$) + 3, Y + 5, " ") 'limpiar
  CALL dialogbox(texto$, LEN(texto$) + 3, 5, X, Y) 'dibujar
END IF

  'colocar el cartele < Aceptar >
  LOCATE Y + 4, X + 1
  PRINT " < Aceptar > "


'esperar por tecla
DO
LOOP UNTIL INKEY$ <> ""

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
PRINT "Por Kronoman"
COLOR 11
PRINT "Copyright (c) 2002"
PRINT "Licencia GNU - Software libre"
PRINT "En memoria de mi querido padre"
PRINT "Hecho en Argentina"
LINE (150, 104)-(180, 116), 3, BF
LINE (150, 108)-(180, 112), 15, BF

COLOR 7
LOCATE 23, 1
PRINT "Presione una tecla..."
DO: LOOP WHILE INKEY$ = ""

END SUB

SUB RebotarX
'Rebota en X, invierte

IF Bola.XD = 0 THEN
   Bola.XD = (40 * RND + 10)
   IF RND < .5 THEN Bola.XD = -Bola.XD
ELSE
   IF Bola.XD < 0 THEN
        Bola.XD = (40 * RND + 10)
   ELSE
        Bola.XD = -(40 * RND + 10)
   END IF
END IF

SOUND 450, .1: SOUND 600, .1
END SUB

SUB RebotarY
'Rebota en Y, invierte, y sonido

IF Bola.YD = 0 THEN
   Bola.YD = (20 * RND + 35)
   IF RND < .5 THEN Bola.YD = -Bola.YD
ELSE
   IF Bola.YD < 0 THEN
        Bola.YD = (20 * RND + 35)
   ELSE
        Bola.YD = -(20 * RND + 35)
   END IF
END IF

SOUND 450, .1: SOUND 600, .1

END SUB

