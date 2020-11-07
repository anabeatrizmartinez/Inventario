       IDENTIFICATION DIVISION.
       *>IDENTIFICACION DEL PROGRAMA.
       PROGRAM-ID. Menu. *>NOMBRE DEL PROGRAMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *>VARIABLES USADAS.
       77  WS-CONECTAR PIC X. *>SOLO ME SERVIRÁ PARA CONECTAR EL MENU CON LOS
                              *>DEMÁS PROGRAMAS EN SU LINKAGE SECTION.
                              *>CON NIVEL 77 PORQUE ES UNA VARIABLE NO COMPUESTA.

       01  WS-TITULO. *>VARIABLE PARA MOSTRAR EL TITULO CENTRADO.
           05 FILLER   PIC X(32) VALUE SPACES.
           05 WS-TIT   PIC X(16) VALUE "Centro Deportivo".
           05 FILLER   PIC X(32) VALUE SPACES.

       77  WS-OPCION PIC 9(2).

       PROCEDURE DIVISION.
       MAIN SECTION.
       PROGRAM-BEGIN.
       *>MOSTRAR MENU
       DISPLAY WS-TITULO.
       DISPLAY " ". *>PARA SALTAR UNA LÍNEA.
       DISPLAY "1) Instrumentos".
       DISPLAY "2) Tipos de Instrumentos".
       DISPLAY "3) Proveedores".
       DISPLAY "4) Usuarios".
       DISPLAY "5) Prestamos".
       DISPLAY "6) Consultas".
       DISPLAY "7) Salir".
       DISPLAY " ".
       DISPLAY "Ingrese numero de opcion deseada:".
       ACCEPT WS-OPCION.
       PERFORM VALIDACION.

       STOP RUN.

       *>RUTINA
       VALIDACION.
       EVALUATE WS-OPCION
       WHEN 1
           CALL "Instrumentos" USING WS-CONECTAR *>HAGO LA LLAMADA DEL PROGRAMA.
           *>EL NOMBRE DEL CALL DEBE SER IGUAL AL PROGRAM-ID DEL PROGRAMA
           *>QUE SE QUIERE LLAMAR.
       WHEN 2
           CALL "Tipos" USING WS-CONECTAR
       WHEN 3
           CALL "Proveedores" USING WS-CONECTAR
       WHEN 4
           CALL "Usuarios" USING WS-CONECTAR
       WHEN 5
           CALL "Prestamos" USING WS-CONECTAR
       WHEN 6
           CALL "Consultas" USING WS-CONECTAR
       WHEN 7
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION
       END-EVALUATE.


       END PROGRAM Menu. *>EL PROGRAMA QUE HACE LA LLAMADA CON CALL
                         *>TERMINA CON END PROGRAM.
