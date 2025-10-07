      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 807_PIZZARIA_RAMALHO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 NUMERO-PEDIDO            PIC 9(4) VALUE 1.
       77 NOME-CLIENTE             PIC X(30).
       77 CONTACTO-CLIENTE         PIC 9(09).
       77 TIPO-PIZZA               PIC 9.
           88 VALIDAR-TIPO-PIZZA   VALUES 1 THRU 3.
       77 PRECO-PIZZA              PIC ZZ9.99.
       77 PRECO-INGREDIENTES       PIC ZZ9.99.
       77 INGREDIENTES             PIC 9(02).
           88 VALIDAR-INGREDIENTES VALUES 1 THRU 10.
       77 TOTAL-LIQUIDO            PIC ZZ9.99.
       77 IVA                      PIC Z9.99.
       77 TOTAL-FINAL              PIC ZZ9.99.
       77 DATA-PEDIDO              PIC 9(08).
       77 DATA-FORMATADA           PIC X(10).
       77 DATA-SISTEMA             PIC X(10).
       77 HORA-SISTEMA             PIC X(08).
       77 HORA-FORMATADA           PIC X(08).
       77 TOTAL-PEDIDO             PIC 9(03)V9(02).
       77 TOTAL-INGREDIENTES       PIC 9(03)V9(02).
       77 NUM-INGREDIENTES         PIC 9(02).
       77 LINHA                    PIC 9(02) VALUE 19.
       77 TEMP                     PIC 9(03)V9(02).
       77 TEMP1                    PIC 9(03)V9(02).
       77 TEMP2                    PIC 9(02).
       77 REPETIR                  PIC A.
          88 VALIDAR-REPETIR      VALUES "s","S","n","N".
       01  TABELA-INGREDIENTES.
          05 INGREDIENTE-ANTERIOR PIC 9(02) OCCURS 5 TIMES INDEXED
          BY IDX.
       77 INGREDIENTE-REPETIDO     PIC X VALUE "N".

       SCREEN SECTION.
       01 CLS BLANK SCREEN.
       01 TITULO.
           05 COL 01 LINE 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "Pizzaria Ramalho, GestPedidosBeta1".
           05 COL 01 LINE 02 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "Pizzas e Derivados, Lda.".
           05 COL 01 LINE 03 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "+--------------------------------------------------------+".
       01 MOLDURA.
           05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
           "+--------------------------------------------------------+".
       01 CABECALHO.
           05 COL 01 LINE 05 HIGHLIGHT VALUE "No Pedido:".
           05 COL 25 LINE 05 HIGHLIGHT VALUE "Cliente:".
           05 COL 01 LINE 06 HIGHLIGHT VALUE "Data:".
           05 COL 25 LINE 06 VALUE "Contato:".
           05 COL 01 LINE 07 VALUE "Hora:".
           05 COL 01 LINE 10 VALUE "Tipo de Pizza Pretendido:".
           05 COL 01 LINE 12 VALUE "Ingredientes:".
           05 COL 01 LINE 15 VALUE
           "+--------------------------------------------------------+".
           05 COL 01 LINE 16 VALUE "Cod. Ingrediente".
           05 COL 25 LINE 16 VALUE "N. Ingrediente".
           05 COL 48 LINE 16 VALUE "Preco".
           05 COL 01 LINE 17 VALUE
           "+--------------------------------------------------------+".
           05 COL 01 LINE 25 VALUE
           "+--------------------------------------------------------+".
           05 COL 25 LINE 26 HIGHLIGHT VALUE "Total Ingredientes:".
           05 COL 25 LINE 27 HIGHLIGHT VALUE "Tipo de Pizza:".
           05 COL 25 LINE 28 HIGHLIGHT VALUE "A pagar:".
           05 COL 25 LINE 29 HIGHLIGHT VALUE "IVA:".
           05 COL 25 LINE 30 HIGHLIGHT VALUE "Final:".
       01 EMENTA.
           05 COL 1 VALUES "(1)Fiambre"          LINE 13.
           05 COL 1 VALUES "(2)Atum"             LINE 14.
           05 COL 12 VALUES "(3)Anchovas"        LINE 13.
           05 COL 12 VALUES "(4)Camarao"         LINE 14.
           05 COL 24 VALUES "(5)Bacon"           LINE 13.
           05 COL 24 VALUES "(6)Banana"          LINE 14.
           05 COL 35 VALUES "(7)Ananas"          LINE 13.
           05 COL 35 VALUES "(8)Azeitonas"       LINE 14.
           05 COL 48 VALUES "(9)Cogumelos"       LINE 13.
           05 COL 48 VALUES "(10)Milho"          LINE 14.
           05 COL 1  VALUES "[1]Pizza Pequena"   LINE 9
           FOREGROUND-COLOR 1 HIGHLIGHT.
           05 COL 22 VALUES "[2]Pizza Media"     LINE 9
           FOREGROUND-COLOR 1 HIGHLIGHT.
           05 COL 44 VALUES "[3]Pizza Grande"    LINE 9
           FOREGROUND-COLOR 1 HIGHLIGHT.
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY CLS.
           DISPLAY TITULO.
           DISPLAY CABECALHO.
           DISPLAY NUMERO-PEDIDO AT 0511.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
              MOVE 0 TO INGREDIENTE-ANTERIOR(IDX)
           END-PERFORM.
       PEGAR-DATA-HORA.
           ACCEPT DATA-PEDIDO FROM DATE YYYYMMDD
           STRING DATA-PEDIDO(7:2) "/"
                  DATA-PEDIDO(5:2) "/"
                  DATA-PEDIDO(1:4)
                  DELIMITED BY SIZE
                  INTO DATA-FORMATADA
           END-STRING.
           DISPLAY DATA-FORMATADA AT 0607.
           ACCEPT HORA-SISTEMA FROM TIME
           STRING HORA-SISTEMA(1:2) ":"
                  HORA-SISTEMA(3:2) ":"
                  HORA-SISTEMA(5:2)
                  DELIMITED BY SIZE
                  INTO HORA-FORMATADA
           END-STRING.
           DISPLAY HORA-FORMATADA AT 0707.
       ENTRADA-DADOS-PEDIDO.
           DISPLAY EMENTA.
           ACCEPT NOME-CLIENTE AT 0534.
           ACCEPT CONTACTO-CLIENTE AT 0634.
       ENTRADA-TIPO.
           ACCEPT TIPO-PIZZA AT 1028.
           DISPLAY "                      " LINE 10 POSITION 32
           EVALUATE TIPO-PIZZA
                WHEN 1
                   DISPLAY "Pizza Pequena" LINE 10 POSITION 30
                   DISPLAY "3.00" LINE 27 POSITION 48
                   ADD 3 TO TOTAL-PEDIDO
                WHEN 2
                   DISPLAY "Pizza Media" LINE 10 POSITION 30
                   DISPLAY "4.00" LINE 27 POSITION 48
                   ADD 4 TO TOTAL-PEDIDO
                WHEN 3
                   DISPLAY "Pizza Grande" LINE 10 POSITION 30
                   DISPLAY "5.00" LINE 27 POSITION 48
                   ADD 5 TO TOTAL-PEDIDO
                WHEN >3
                   DISPLAY "Tipo de pizza invalido" LINE 10
                   POSITION 32 FOREGROUND-COLOR 4
                   GO ENTRADA-TIPO
           END-EVALUATE.
           PERFORM UNTIL NUM-INGREDIENTES = 5
                ACCEPT INGREDIENTES LINE LINHA POSITION 3
                MOVE "N" TO INGREDIENTE-REPETIDO
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > NUM-INGREDIENTES
              IF INGREDIENTES = INGREDIENTE-ANTERIOR(IDX)
                MOVE "S" TO INGREDIENTE-REPETIDO
                EXIT PERFORM
              END-IF
           END-PERFORM
              IF INGREDIENTE-REPETIDO = "S"
                  DISPLAY "Ingrediente ja inserido!" LINE LINHA
                  POSITION 25
                  FOREGROUND-COLOR 4 HIGHLIGHT
              ELSE
                EVALUATE INGREDIENTES
                   WHEN 1
                       DISPLAY "                        " LINE LINHA
                       POSITION 25
                       DISPLAY "Fiambre" LINE LINHA POSITION 25
                       DISPLAY "0.50" LINE LINHA POSITION 48
                       ADD 0.5 TO TOTAL-INGREDIENTES
                       MOVE TOTAL-INGREDIENTES TO PRECO-INGREDIENTES
                       DISPLAY PRECO-INGREDIENTES AT 2646
                       ADD 1 TO NUM-INGREDIENTES
                       MOVE 1 TO INGREDIENTE-ANTERIOR(NUM-INGREDIENTES)
                   WHEN 2
                       DISPLAY "                        " LINE LINHA
                       POSITION 25
                       DISPLAY "Atum" LINE LINHA POSITION 25
                       DISPLAY "0.70" LINE LINHA POSITION 48
                       ADD 0.7 TO TOTAL-INGREDIENTES
                       MOVE TOTAL-INGREDIENTES TO PRECO-INGREDIENTES
                       DISPLAY PRECO-INGREDIENTES AT 2646
                       ADD 1 TO NUM-INGREDIENTES
                       MOVE 2 TO INGREDIENTE-ANTERIOR(NUM-INGREDIENTES)
                   WHEN 3
                       DISPLAY "                        " LINE LINHA
                       POSITION 25
                       DISPLAY "Anchovas" LINE LINHA POSITION 25
                       DISPLAY "0.40" LINE LINHA POSITION 48
                       ADD 0.4 TO TOTAL-INGREDIENTES
                       MOVE TOTAL-INGREDIENTES TO PRECO-INGREDIENTES
                       DISPLAY PRECO-INGREDIENTES AT 2646
                       ADD 1 TO NUM-INGREDIENTES
                       MOVE 3 TO INGREDIENTE-ANTERIOR(NUM-INGREDIENTES)
                   WHEN 4
                       DISPLAY "                        " LINE LINHA
                       POSITION 25
                       DISPLAY "Camarao" LINE LINHA POSITION 25
                       DISPLAY "0.80" LINE LINHA POSITION 48
                       ADD 0.8 TO TOTAL-INGREDIENTES
                       MOVE TOTAL-INGREDIENTES TO PRECO-INGREDIENTES
                       DISPLAY PRECO-INGREDIENTES AT 2646
                       ADD 1 TO NUM-INGREDIENTES
                       MOVE 4 TO INGREDIENTE-ANTERIOR(NUM-INGREDIENTES)
                   WHEN 5
                       DISPLAY "                        " LINE LINHA
                       POSITION 25
                       DISPLAY "Bacon" LINE LINHA POSITION 25
                       DISPLAY "0.90" LINE LINHA POSITION 48
                       ADD 0.9 TO TOTAL-INGREDIENTES
                       MOVE TOTAL-INGREDIENTES TO PRECO-INGREDIENTES
                       DISPLAY PRECO-INGREDIENTES AT 2646
                       ADD 1 TO NUM-INGREDIENTES
                       MOVE 5 TO INGREDIENTE-ANTERIOR(NUM-INGREDIENTES)
                   WHEN 6
                       DISPLAY "                        " LINE LINHA
                       POSITION 25
                       DISPLAY "Banana" LINE LINHA POSITION 25
                       DISPLAY "0.30" LINE LINHA POSITION 48
                       ADD 0.3 TO TOTAL-INGREDIENTES
                       MOVE TOTAL-INGREDIENTES TO PRECO-INGREDIENTES
                       DISPLAY PRECO-INGREDIENTES AT 2646
                       ADD 1 TO NUM-INGREDIENTES
                       MOVE 6 TO INGREDIENTE-ANTERIOR(NUM-INGREDIENTES)
                   WHEN 7
                       DISPLAY "                        " LINE LINHA
                       POSITION 25
                       DISPLAY "Ananas" LINE LINHA POSITION 25
                       DISPLAY "0.40" LINE LINHA POSITION 48
                       ADD 0.4 TO TOTAL-INGREDIENTES
                       MOVE TOTAL-INGREDIENTES TO PRECO-INGREDIENTES
                       DISPLAY PRECO-INGREDIENTES AT 2646
                       ADD 1 TO NUM-INGREDIENTES
                       MOVE 7 TO INGREDIENTE-ANTERIOR(NUM-INGREDIENTES)
                   WHEN 8
                       DISPLAY "                        " LINE LINHA
                       POSITION 25
                       DISPLAY "Azeitonas" LINE LINHA POSITION 25
                       DISPLAY "0.30" LINE LINHA POSITION 48
                       ADD 0.3 TO TOTAL-INGREDIENTES
                       MOVE TOTAL-INGREDIENTES TO PRECO-INGREDIENTES
                       DISPLAY PRECO-INGREDIENTES AT 2646
                       ADD 1 TO NUM-INGREDIENTES
                       MOVE 8 TO INGREDIENTE-ANTERIOR(NUM-INGREDIENTES)
                   WHEN 9
                       DISPLAY "                        " LINE LINHA
                       POSITION 25
                       DISPLAY "Cogumelos" LINE LINHA POSITION 25
                       DISPLAY "0.60" LINE LINHA POSITION 48
                       ADD 0.6 TO TOTAL-INGREDIENTES
                       MOVE TOTAL-INGREDIENTES TO PRECO-INGREDIENTES
                       DISPLAY PRECO-INGREDIENTES AT 2646
                       ADD 1 TO NUM-INGREDIENTES
                       MOVE 9 TO INGREDIENTE-ANTERIOR(NUM-INGREDIENTES)
                   WHEN 10
                       DISPLAY "                        " LINE LINHA
                       POSITION 25
                       DISPLAY "Milho" LINE LINHA POSITION 25
                       DISPLAY "0.50" LINE LINHA POSITION 48
                       ADD 0.5 TO TOTAL-INGREDIENTES
                       MOVE TOTAL-INGREDIENTES TO PRECO-INGREDIENTES
                       DISPLAY PRECO-INGREDIENTES AT 2646
                       ADD 1 TO NUM-INGREDIENTES
                       MOVE 10 TO INGREDIENTE-ANTERIOR(NUM-INGREDIENTES)
                   WHEN >10
                       DISPLAY "Ingrediente Invalido" LINE LINHA
                       POSITION 25 FOREGROUND-COLOR 4 HIGHLIGHT
                       DISPLAY "                    " LINE LINHA
                       POSITION 25
                   WHEN 00
                       DISPLAY "Finaliza ingredientes!" LINE LINHA
                       POSITION 15 FOREGROUND-COLOR 6 HIGHLIGHT
                       GO TO CALCULOS
                END-EVALUATE
             IF INGREDIENTE-REPETIDO = "N" AND INGREDIENTES <= 10
                 AND INGREDIENTES > 0
                 ADD 1 TO LINHA
             END-IF
            END-IF
           END-PERFORM.
           DISPLAY NUM-INGREDIENTES AT 2645.
       CALCULOS.
           COMPUTE TOTAL-LIQUIDO = (TOTAL-INGREDIENTES + TOTAL-PEDIDO).
           DISPLAY TOTAL-LIQUIDO AT 2846.
           MOVE TOTAL-LIQUIDO TO TEMP.
           COMPUTE IVA = TEMP * 0.23.
           DISPLAY IVA AT 2947.
           MOVE IVA TO TEMP1.
           COMPUTE TOTAL-FINAL = TEMP + TEMP1.
           DISPLAY TOTAL-FINAL AT 3046.
       LER-REPETIR.
           DISPLAY "FINALIZA PEDIDO? " FOREGROUND-COLOR 2
           HIGHLIGHT AT 2801.
           ACCEPT REPETIR AT 2819.
           IF NOT VALIDAR-REPETIR THEN
              DISPLAY "DIGITE S OU N!" FOREGROUND-COLOR 4
              HIGHLIGHT AT 2901
              GO LER-REPETIR
             ELSE
              IF REPETIR = "s" OR REPETIR = "S" THEN
                 ADD 1 TO NUMERO-PEDIDO
                 COMPUTE NUM-INGREDIENTES = 0
                 COMPUTE LINHA = 19
                 COMPUTE TOTAL-INGREDIENTES = 0
                 COMPUTE TOTAL-PEDIDO = 0
                 GO INICIO
               ELSE
                 DISPLAY "FIM DO PEDIDO!" FOREGROUND-COLOR 2
                 HIGHLIGHT AT 2902
                 ACCEPT OMITTED AT 3101
              END-IF
           END-IF.
           STOP RUN.
           END PROGRAM 807_PIZZARIA_RAMALHO.
