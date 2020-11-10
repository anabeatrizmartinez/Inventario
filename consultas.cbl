       IDENTIFICATION DIVISION.
       PROGRAM-ID. Consultas.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       SELECT OPTIONAL INSTRUMENTOS-ARCHIVO
       ASSIGN TO "c:\instrumentos.dat"
       ORGANIZATION IS INDEXED
       RECORD KEY IS ID-INSTRUMENTO
       ACCESS MODE IS DYNAMIC.

       SELECT OPTIONAL PROVEEDORES-ARCHIVO
       ASSIGN TO "c:\proveedores.dat"
       ORGANIZATION IS INDEXED
       RECORD KEY IS ID-PROVEEDOR
       ACCESS MODE IS DYNAMIC.

       SELECT OPTIONAL USUARIOS-ARCHIVO
       ASSIGN TO "c:\usuarios.dat"
       ORGANIZATION IS INDEXED
       RECORD KEY IS CEDULA-USUARIO
       ACCESS MODE IS DYNAMIC.

       DATA DIVISION.
       FILE SECTION.
       FD INSTRUMENTOS-ARCHIVO.
           01 INSTRUMENTOS-REGISTRO.
               05 ID-INSTRUMENTO       PIC X(6).
               05 TIPO-INSTRUMENTO-IN  PIC X(40).
               05 FECHA-ADQUISICION    PIC 9(6).
               05 FORMA-ADQUISICION    PIC X(8).
               05 ID-PROVEEDOR-INST    PIC X(11).
               05 STATUS-INSTRUMENTO   PIC X(14).
               05 FECHA-STATUS         PIC 9(6).

       FD PROVEEDORES-ARCHIVO.
           01 PROVEEDORES-REGISTRO.
               05 ID-PROVEEDOR     PIC X(11).
               05 NOMBRE-PROVEEDOR PIC X(40).
               05 TIPO-PROVEEDOR   PIC X(8).
               05 DIRECCION        PIC X(100).
               05 TELEFONO         PIC 9(11).

       FD USUARIOS-ARCHIVO.
           01 USUARIOS-REGISTRO.
               05 CEDULA-USUARIO      PIC X(11).
               05 NOMBRE-USUARIO      PIC X(40).
               05 SEXO                PIC X(9).
               05 DIRECCION           PIC X(100).
               05 TELEFONO            PIC 9(11).
               05 TIPO-INSTRUMENTO-US PIC X(40).

       WORKING-STORAGE SECTION.
       *>VARIABLES USADAS
       01 WS-TITULO.
           05 FILLER   PIC X(34) VALUE SPACES.
           05 WS-TIT   PIC X(11) VALUE "Consultas".
           05 FILLER   PIC X(35) VALUE SPACES.

       01 WS-INSTRUMENTOS-REGISTRO.
               05 WS-ID-INSTRUMENTO       PIC X(6).
               05 WS-TIPO-INSTRUMENTO-IN  PIC X(40).
               05 WS-FECHA-ADQUISICION    PIC 9(6).
               05 WS-FORMA-ADQUISICION    PIC X(8).
               05 WS-ID-PROVEEDOR-INST    PIC X(11).
               05 WS-STATUS-INSTRUMENTO   PIC X(14).
               05 WS-FECHA-STATUS         PIC 9(6).

       01 WS-PROVEEDORES-REGISTRO.
               05 WS-ID-PROVEEDOR     PIC X(11).
               05 WS-NOMBRE-PROVEEDOR PIC X(40).
               05 WS-TIPO-PROVEEDOR   PIC X(8).
               05 WS-DIRECCION        PIC X(100).
               05 WS-TELEFONO         PIC 9(11).

       01 WS-USUARIOS-REGISTRO.
               05 WS-CEDULA-USUARIO      PIC X(11).
               05 WS-NOMBRE-USUARIO      PIC X(40).
               05 WS-SEXO                PIC X(9).
               05 WS-DIRECCION           PIC X(100).
               05 WS-TELEFONO            PIC 9(11).
               05 WS-TIPO-INSTRUMENTO-US PIC X(40).

       77 WS-OPCION PIC 9(2).
       77 LEE-TODO PIC 9(1).

       LINKAGE SECTION.
       01 LS-CONECTAR PIC X.

       PROCEDURE DIVISION USING LS-CONECTAR. *>CONECTO CON EL MENU.
       MAIN SECTION.
       *>PROGRAMA PRINCIPAL.
       PROGRAM-BEGIN.
       DISPLAY " ".
       DISPLAY WS-TITULO.
       DISPLAY " ".
       DISPLAY "1) Lista de donantes de instrumentos".
       DISPLAY "2) Lista de usuarios de un tipo de instrumento".
       DISPLAY "3) Lista de instrumentos deportivos ".
       DISPLAY "   por tipo de instrumento".
       DISPLAY "4) Lista de instrumentos deportivos dado un status".
       DISPLAY "5) Salir".
       ACCEPT WS-OPCION.
       PERFORM VALIDACION-MENU.

       STOP RUN.

       *>RUTINAS.
       VALIDACION-MENU.
       EVALUATE WS-OPCION
       WHEN 1
           PERFORM DONANTES
       WHEN 2
           PERFORM USUARIOS-TIPO
       WHEN 3
           PERFORM INSTRUMENTOS-TIPO
       WHEN 4
           PERFORM INSTRUMENTOS-STATUS
       WHEN 5
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-MENU
       END-EVALUATE.

       DONANTES.
       DISPLAY "---Lista de donantes de instrumentos---".
       DISPLAY " ".

       OPEN I-O PROVEEDORES-ARCHIVO.

       MOVE 0 TO LEE-TODO.
       PERFORM LEER-SIGUIENTE-DONANTE.
       IF LEE-TODO = 1
           DISPLAY "No se encontraron registros en el archivo."
       ELSE
           PERFORM MUESTRA-CAMPOS-DONANTE UNTIL LEE-TODO = 1.

       CLOSE PROVEEDORES-ARCHIVO.
       PERFORM VOLVER.

       LEER-SIGUIENTE-DONANTE.
       READ PROVEEDORES-ARCHIVO NEXT RECORD
       AT END
       MOVE 1 TO LEE-TODO.

       MUESTRA-CAMPOS-DONANTE.
       IF TIPO-PROVEEDOR = "DONANTE" OR = "donante"
           DISPLAY " "
           DISPLAY "Id de Proveedor: " ID-PROVEEDOR
           DISPLAY "Nombre de proveedor: " NOMBRE-PROVEEDOR
           DISPLAY "Tipo de proveedor: " TIPO-PROVEEDOR
           PERFORM LEER-SIGUIENTE-DONANTE
       ELSE
           PERFORM LEER-SIGUIENTE-DONANTE.

       VOLVER.
       DISPLAY "1.- Volver al menu".
       DISPLAY "2.- Salir".
       DISPLAY "Ingrese numero de opcion deseada:".
       ACCEPT WS-OPCION.
       PERFORM VALIDACION-VOLVER.

       VALIDACION-VOLVER.
       EVALUATE WS-OPCION
       WHEN 1
           PERFORM PROGRAM-BEGIN
       WHEN 2
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-VOLVER
       END-EVALUATE.

       USUARIOS-TIPO.
       DISPLAY "---Lista de usuarios de un tipo de instrumento---".
       DISPLAY " ".
       DISPLAY "Indique nombre de tipo de instrumento a buscar".
       ACCEPT WS-TIPO-INSTRUMENTO-US.

       OPEN I-O USUARIOS-ARCHIVO.

       MOVE 0 TO LEE-TODO.
       PERFORM LEER-SIGUIENTE-USUARIO.
       IF LEE-TODO = 1
           DISPLAY "No se encontraron registros en el archivo."
       ELSE
           PERFORM MUESTRA-CAMPOS-USUARIO UNTIL LEE-TODO = 1.

       CLOSE USUARIOS-ARCHIVO.
       PERFORM VOLVER.

       LEER-SIGUIENTE-USUARIO.
       READ USUARIOS-ARCHIVO NEXT RECORD
       AT END
       MOVE 1 TO LEE-TODO.

       MUESTRA-CAMPOS-USUARIO.
       IF TIPO-INSTRUMENTO-US = WS-TIPO-INSTRUMENTO-US
           DISPLAY " "
           DISPLAY "Cedula del usuario: " CEDULA-USUARIO
           DISPLAY "Nombre de usuario: " NOMBRE-USUARIO
           DISPLAY "Tipo de instrumento: " TIPO-INSTRUMENTO-US
           PERFORM LEER-SIGUIENTE-USUARIO
       ELSE
           PERFORM LEER-SIGUIENTE-USUARIO.

       INSTRUMENTOS-TIPO.
       DISPLAY "---Lista de instrumentos por tipo de instrumento---".
       DISPLAY " ".
       DISPLAY "Indique nombre de tipo de instrumento a buscar".
       ACCEPT WS-TIPO-INSTRUMENTO-IN.

       OPEN I-O INSTRUMENTOS-ARCHIVO.

       MOVE 0 TO LEE-TODO.
       PERFORM LEER-SIGUIENTE-TIPO.
       IF LEE-TODO = 1
           DISPLAY "No se encontraron registros en el archivo."
       ELSE
           PERFORM MUESTRA-CAMPOS-TIPO UNTIL LEE-TODO = 1.

       CLOSE INSTRUMENTOS-ARCHIVO.
       PERFORM VOLVER.

       LEER-SIGUIENTE-TIPO.
       READ INSTRUMENTOS-ARCHIVO NEXT RECORD
       AT END
       MOVE 1 TO LEE-TODO.

       MUESTRA-CAMPOS-TIPO.
       IF TIPO-INSTRUMENTO-IN = WS-TIPO-INSTRUMENTO-IN
           DISPLAY " "
           DISPLAY "Codigo del instrumento: " ID-INSTRUMENTO
           DISPLAY "Status del instrumento: " STATUS-INSTRUMENTO
           DISPLAY "Tipo de instrumento: " TIPO-INSTRUMENTO-IN
           PERFORM LEER-SIGUIENTE-TIPO
       ELSE
           PERFORM LEER-SIGUIENTE-TIPO.

       INSTRUMENTOS-STATUS.
       DISPLAY "---Lista de instrumentos deportivos dado un status---".
       DISPLAY " ".
       DISPLAY "Indique status del instrumento a buscar".
       ACCEPT WS-STATUS-INSTRUMENTO.

       OPEN I-O INSTRUMENTOS-ARCHIVO.

       MOVE 0 TO LEE-TODO.
       PERFORM LEER-SIGUIENTE-STATUS.
       IF LEE-TODO = 1
           DISPLAY "No se encontraron registros en el archivo."
       ELSE
           PERFORM MUESTRA-CAMPOS-STATUS UNTIL LEE-TODO = 1.

       CLOSE INSTRUMENTOS-ARCHIVO.
       PERFORM VOLVER.

       LEER-SIGUIENTE-STATUS.
       READ INSTRUMENTOS-ARCHIVO NEXT RECORD
       AT END
       MOVE 1 TO LEE-TODO.

       MUESTRA-CAMPOS-STATUS.
       IF STATUS-INSTRUMENTO = WS-STATUS-INSTRUMENTO
           DISPLAY " "
           DISPLAY "Codigo del instrumento: " ID-INSTRUMENTO
           DISPLAY "Tipo de instrumento: " TIPO-INSTRUMENTO-IN
           DISPLAY "Status del instrumento: " STATUS-INSTRUMENTO
           PERFORM LEER-SIGUIENTE-STATUS
       ELSE
           PERFORM LEER-SIGUIENTE-STATUS.


       EXIT PROGRAM.
