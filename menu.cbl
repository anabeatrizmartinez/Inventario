       IDENTIFICATION DIVISION.
       PROGRAM-ID. Menu.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CONECTAR PIC X. *>Solo me servirá para conectar el menu con los
                              *>demás programas en su LINKAGE SECTION.

       01  WS-TITULO.
           05 FILLER   PIC X(32) VALUE SPACES.
           05 WS-TIT   PIC X(16) VALUE "Centro Deportivo".
           05 FILLER   PIC X(32) VALUE SPACES.

       77  WS-OPTION PIC 9(1).

       PROCEDURE DIVISION.
       MAIN SECTION.
       PROGRAM-BEGIN.
       *>MOSTRAR MENU
       DISPLAY WS-TITULO.
       DISPLAY " ".
       DISPLAY "1) Instrumentos".
       DISPLAY "2) Tipos de Instrumentos".
       DISPLAY "3) Proveedores".
       DISPLAY "4) Usuarios".
       DISPLAY "5) Prestamos".
       DISPLAY "6) Consultas".
       DISPLAY "7) Salir".
       DISPLAY " ".
       DISPLAY "Ingrese numero de opcion deseada:".
       ACCEPT WS-OPTION.
       PERFORM VALIDACION.

       STOP RUN.

       *>RUTINA
       VALIDACION.
       EVALUATE WS-OPTION
       WHEN 1
           CALL "Instrumentos" USING WS-CONECTAR
           *>El Nombre del CALL debe ser igual al PROGRAM-ID del programa que
           *>se quiere llamar.
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
           ACCEPT WS-OPTION
           PERFORM VALIDACION
       END-EVALUATE.


       END PROGRAM Menu.
