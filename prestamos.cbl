       IDENTIFICATION DIVISION.
       PROGRAM-ID. Prestamos.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       SELECT OPTIONAL PRESTAMOS-ARCHIVO
       ASSIGN TO "c:\prestamos.dat"
       ORGANIZATION IS INDEXED
       RECORD KEY IS CEDULA-USUARIO
       ALTERNATE RECORD KEY IS FECHA-PRESTAMO WITH DUPLICATES *>LLAVE ALTERNATIVA
       ALTERNATE RECORD KEY IS HORA-PRESTAMO WITH DUPLICATES
       ACCESS MODE IS DYNAMIC.

       DATA DIVISION.
       FILE SECTION.
       FD PRESTAMOS-ARCHIVO.
           01 PRESTAMOS-REGISTRO.
               05 CEDULA-USUARIO     PIC X(11).
               05 FECHA-PRESTAMO     PIC X(40).
               05 HORA-PRESTAMO      PIC X(9).
               05 ID-INSTRUMENTO     PIC X(100).
               05 FECHA-DEVOLUCION   PIC 9(11).
               05 HORA-DEVOLUCION    PIC X(40).
               05 STATUS-INSTRUMENTO PIC X(14).

       WORKING-STORAGE SECTION.
       *>VARIABLES USADAS
       01 WS-TITULO.
           05 FILLER   PIC X(36) VALUE SPACES.
           05 WS-TIT   PIC X(11) VALUE "Prestamos".
           05 FILLER   PIC X(36) VALUE SPACES.

       01 WS-PRESTAMOS-REGISTRO-1.
               05 WS-CEDULA-USUARIO-1     PIC X(11).
               05 WS-FECHA-PRESTAMO-1     PIC X(40).
               05 WS-HORA-PRESTAMO-1      PIC X(9).
               05 WS-ID-INSTRUMENTO-1     PIC X(100).
               05 WS-FECHA-DEVOLUCION-1   PIC 9(11).
               05 WS-HORA-DEVOLUCION-1    PIC X(40).
               05 WS-STATUS-INSTRUMENTO-1 PIC X(14).

       01 WS-PRESTAMOS-REGISTRO-2.
               05 WS-CEDULA-USUARIO-2     PIC X(11).
               05 WS-FECHA-PRESTAMO-2     PIC X(40).
               05 WS-HORA-PRESTAMO-2      PIC X(9).
               05 WS-ID-INSTRUMENTO-2     PIC X(100).
               05 WS-FECHA-DEVOLUCION-2   PIC 9(11).
               05 WS-HORA-DEVOLUCION-2    PIC X(40).
               05 WS-STATUS-INSTRUMENTO-2 PIC X(14).

       77 WS-OPCION PIC 9(2).
       77 WS-INDICADOR PIC 9(1).
       77 WS-SI-NO PIC X(1).

       LINKAGE SECTION.
       01 LS-CONECTAR PIC X.

       PROCEDURE DIVISION USING LS-CONECTAR. *>CONECTO CON EL MENU.
       MAIN SECTION.
       *>PROGRAMA PRINCIPAL.
       PROGRAM-BEGIN.
       DISPLAY " ".
       DISPLAY WS-TITULO.
       DISPLAY " ".
       DISPLAY "1) Registrar".
       DISPLAY "2) Actualizar".
       DISPLAY "3) Salir".
       ACCEPT WS-OPCION.
       PERFORM VALIDACION-MENU.

       STOP RUN.

       *>RUTINAS.
       VALIDACION-MENU.
       EVALUATE WS-OPCION
       WHEN 1
           PERFORM REGISTRAR
       WHEN 2
           PERFORM ACTUALIZAR
       WHEN 3
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-MENU
       END-EVALUATE.

       REGISTRAR.
       DISPLAY "---Registro de nuevo prestamo---".
       DISPLAY " ".
       DISPLAY "Indique numero de cedula del usuario".
       ACCEPT CEDULA-USUARIO.
       *>VERIFICAR SI LA CEDULA YA EXISTE EN LA BASE DE DATOS
       OPEN I-O PRESTAMOS-ARCHIVO.
       READ PRESTAMOS-ARCHIVO RECORD
           KEY CEDULA-USUARIO
               INVALID KEY     MOVE 0 TO WS-INDICADOR
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR.
       IF WS-INDICADOR = 0 *>SI NO EXISTE, GUARDO EL NUEVO REGISTRO
           DISPLAY " "
           DISPLAY "Ingrese fecha del prestamo (AA/MM/DD)"
           ACCEPT FECHA-PRESTAMO
           DISPLAY " "
           DISPLAY "Ingrese hora del prestamo"
           ACCEPT HORA-PRESTAMO
           DISPLAY " "
           DISPLAY "Ingrese codigo del instrumento"
           ACCEPT ID-INSTRUMENTO
           DISPLAY "Ingrese fecha de devolucion (AA/MM/DD)"
           ACCEPT FECHA-DEVOLUCION
           DISPLAY " "
           DISPLAY "Ingrese hora de devolucion"
           ACCEPT HORA-DEVOLUCION
           DISPLAY " "
           DISPLAY "Ingrese status del instrumento"
           ACCEPT STATUS-INSTRUMENTO
           WRITE PRESTAMOS-REGISTRO
           DISPLAY " "
           DISPLAY "Prestamo registrado correctamente."
       ELSE *>SI EXISTE, SALIR
           DISPLAY " "
           DISPLAY "El usuario ya tiene un prestamo.".

       CLOSE PRESTAMOS-ARCHIVO.
       PERFORM VOLVER-REGISTRAR.

       VOLVER-REGISTRAR.
       DISPLAY " ".
       DISPLAY "¿Que desea hacer?".
       DISPLAY "1.- Registrar otro prestamo".
       DISPLAY "2.- Volver al menu".
       DISPLAY "3.- Salir".
       DISPLAY "Ingrese numero de opcion deseada:".
       ACCEPT WS-OPCION.
       PERFORM VALIDACION-VOLVER-REGISTRAR.

       VALIDACION-VOLVER-REGISTRAR.
       EVALUATE WS-OPCION
       WHEN 1
           PERFORM REGISTRAR
       WHEN 2
           PERFORM PROGRAM-BEGIN
       WHEN 3
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-VOLVER-REGISTRAR
       END-EVALUATE.

       ACTUALIZAR.
       DISPLAY "---Actualizar prestamos---".
       DISPLAY " ".
       DISPLAY "Indique numero de cedula del usuario".
       ACCEPT CEDULA-USUARIO.
       *>VERIFICAR SI LA CEDULA YA EXISTE EN LA BASE DE DATOS
       OPEN I-O PRESTAMOS-ARCHIVO.
       READ PRESTAMOS-ARCHIVO RECORD
           KEY CEDULA-USUARIO
               INVALID KEY     MOVE 0 TO WS-INDICADOR
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR.
       IF WS-INDICADOR = 0 *>SI NO EXISTE REGRESO AL MENU.
           DISPLAY " "
           DISPLAY "El usuario ingresado no tiene prestamos."
           CLOSE PRESTAMOS-ARCHIVO
           PERFORM VOLVER-ACTUALIZAR
       ELSE *>SI EXISTE MUESTRO LOS CAMPOS ANTES DE ACTUALIZAR.
           DISPLAY " "
           DISPLAY "Numero de cedula del usuario: " CEDULA-USUARIO
           DISPLAY "Fecha de prestamo: " FECHA-PRESTAMO
           DISPLAY "Hora de prestamo: " HORA-PRESTAMO
           DISPLAY "Codigo del instrumento: " ID-INSTRUMENTO
           DISPLAY "Fecha de devolucion: " FECHA-DEVOLUCION
           DISPLAY "Hora de devolucion: " HORA-DEVOLUCION
           DISPLAY "Status del instrumento: " STATUS-INSTRUMENTO
           PERFORM CONFIRMAR-ACTUALIZAR.

       VOLVER-ACTUALIZAR.
       DISPLAY " ".
       DISPLAY "¿Que desea hacer?".
       DISPLAY "1.- Actualizar otro prestamo".
       DISPLAY "2.- Volver al menu".
       DISPLAY "3.- Salir".
       DISPLAY "Ingrese numero de opcion deseada:".
       ACCEPT WS-OPCION.
       PERFORM VALIDACION-VOLVER-ACTUALIZAR.

       VALIDACION-VOLVER-ACTUALIZAR.
       EVALUATE WS-OPCION
       WHEN 1
           PERFORM ACTUALIZAR
       WHEN 2
           PERFORM PROGRAM-BEGIN
       WHEN 3
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-VOLVER-ACTUALIZAR
       END-EVALUATE.

       CONFIRMAR-ACTUALIZAR.
       DISPLAY " ".
       DISPLAY "¿Que desea hacer con este prestamo?".
       DISPLAY "1.- Editar campos".
       DISPLAY "2.- Eliminar prestamo".
       DISPLAY "3.- Actualizar otro prestamo"
       DISPLAY "4.- Volver al menu".
       DISPLAY "5.- Salir"
       DISPLAY "Ingrese numero de opcion deseada:".
       ACCEPT WS-OPCION.
       PERFORM VALIDACION-CONFIRMAR-ACTUALIZAR.

       VALIDACION-CONFIRMAR-ACTUALIZAR.
       EVALUATE WS-OPCION
       WHEN 1
           PERFORM EDITAR-CAMPOS
       WHEN 2
           PERFORM ELIMINAR
       WHEN 3
           CLOSE PRESTAMOS-ARCHIVO
           PERFORM ACTUALIZAR
       WHEN 4
           CLOSE PRESTAMOS-ARCHIVO
           PERFORM PROGRAM-BEGIN
       WHEN 5
           CLOSE PRESTAMOS-ARCHIVO
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-CONFIRMAR-ACTUALIZAR
       END-EVALUATE.

       ELIMINAR. *>ELIMINAR EL PRESTAMO
       DISPLAY "¿Esta seguro de eliminar el "
       DISPLAY "prestamo del usuario " CEDULA-USUARIO "? (S/N)".
       ACCEPT WS-SI-NO.
       PERFORM VALIDACION-ELIMINAR.

       VALIDACION-ELIMINAR.
       EVALUATE WS-SI-NO
       WHEN = "S" OR = "s"
           DELETE PRESTAMOS-ARCHIVO RECORD
           DISPLAY " "
           DISPLAY "Prestamo eliminado."
           CLOSE PRESTAMOS-ARCHIVO
           PERFORM VOLVER-ACTUALIZAR
       WHEN = "N" OR = "n"
           PERFORM CONFIRMAR-ACTUALIZAR
       WHEN OTHER
           DISPLAY "Por favor ingrese S para si, o N para no."
           ACCEPT WS-SI-NO
           PERFORM VALIDACION-ELIMINAR
       END-EVALUATE.

       EDITAR-CAMPOS. *>ACTUALIZAR LOS CAMPOS
       DISPLAY " ".
       DISPLAY "Numero de cedula del usuario: " CEDULA-USUARIO.
       PERFORM CONFIRMAR. *>VERIFICAR SI SE QUIERE EDITAR ESTE CAMPO O NO.
       PERFORM CONFIRMAR-CEDULA.

       DISPLAY " ".
       DISPLAY "Fecha del prestamo: " FECHA-PRESTAMO.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-FECHA-PRE.

       DISPLAY " ".
       DISPLAY "Hora del prestamo: " HORA-PRESTAMO.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-HORA-PRE.

       DISPLAY " ".
       DISPLAY "Codigo del instrumento: " ID-INSTRUMENTO.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-CODIGO.

       DISPLAY " ".
       DISPLAY "Fecha de devolucion: " FECHA-DEVOLUCION.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-FECHA-DEV.

       DISPLAY " ".
       DISPLAY "Hora de devolucion: " HORA-DEVOLUCION.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-HORA-DEV.

       DISPLAY " ".
       DISPLAY "Status del instrumento: " STATUS-INSTRUMENTO.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-STATUS.

       PERFORM FINAL-EDITAR-CAMPOS.

       CONFIRMAR.
       DISPLAY "1.- Editar".
       DISPLAY "2.- Siguiente campo".
       DISPLAY "Ingrese numero de opcion deseada:".
       ACCEPT WS-OPCION.

       CONFIRMAR-CEDULA.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE CEDULA-USUARIO TO WS-CEDULA-USUARIO-1 *>GUARDAR EL REGISTRO INICIAL.
           DISPLAY " "
           DISPLAY "Ingrese nuevo numero de cedula"
           ACCEPT CEDULA-USUARIO
           MOVE CEDULA-USUARIO TO WS-CEDULA-USUARIO-2 *>GUARDAR EL REGISTRO NUEVO.
           NEXT SENTENCE
       WHEN 2
           MOVE CEDULA-USUARIO TO WS-CEDULA-USUARIO-1
           MOVE CEDULA-USUARIO TO WS-CEDULA-USUARIO-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-CEDULA
       END-EVALUATE.

       CONFIRMAR-FECHA-PRE.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE FECHA-PRESTAMO TO WS-FECHA-PRESTAMO-1
           DISPLAY " "
           DISPLAY "Ingrese nueva fecha de prestamo (AA/MM/DD)"
           ACCEPT FECHA-PRESTAMO
           MOVE FECHA-PRESTAMO TO WS-FECHA-PRESTAMO-2
           NEXT SENTENCE
       WHEN 2
           MOVE FECHA-PRESTAMO TO WS-FECHA-PRESTAMO-1
           MOVE FECHA-PRESTAMO TO WS-FECHA-PRESTAMO-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-FECHA-PRE
       END-EVALUATE.

       CONFIRMAR-HORA-PRE.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE HORA-PRESTAMO TO WS-HORA-PRESTAMO-1
           DISPLAY " "
           DISPLAY "Ingrese nueva hora de prestamo "
           ACCEPT HORA-PRESTAMO
           MOVE HORA-PRESTAMO TO WS-HORA-PRESTAMO-2
           NEXT SENTENCE
       WHEN 2
           MOVE HORA-PRESTAMO TO WS-HORA-PRESTAMO-1
           MOVE HORA-PRESTAMO TO WS-HORA-PRESTAMO-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-HORA-PRE
       END-EVALUATE.

       CONFIRMAR-CODIGO.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE ID-INSTRUMENTO TO WS-ID-INSTRUMENTO-1
           DISPLAY " "
           DISPLAY "Ingrese nuevo codigo del instrumento"
           ACCEPT ID-INSTRUMENTO
           MOVE ID-INSTRUMENTO TO WS-ID-INSTRUMENTO-2
           NEXT SENTENCE
       WHEN 2
           MOVE ID-INSTRUMENTO TO WS-ID-INSTRUMENTO-1
           MOVE ID-INSTRUMENTO TO WS-ID-INSTRUMENTO-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-CODIGO
       END-EVALUATE.

       CONFIRMAR-FECHA-DEV.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE FECHA-DEVOLUCION TO WS-FECHA-DEVOLUCION-1
           DISPLAY " "
           DISPLAY "Ingrese nueva fecha de devolucion (AA/MM/DD)"
           ACCEPT FECHA-DEVOLUCION
           MOVE FECHA-DEVOLUCION TO WS-FECHA-DEVOLUCION-2
           NEXT SENTENCE
       WHEN 2
           MOVE FECHA-DEVOLUCION TO WS-FECHA-DEVOLUCION-1
           MOVE FECHA-DEVOLUCION TO WS-FECHA-DEVOLUCION-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-FECHA-DEV
       END-EVALUATE.

       CONFIRMAR-HORA-DEV.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE HORA-DEVOLUCION TO WS-HORA-DEVOLUCION-1
           DISPLAY " "
           DISPLAY "Ingrese nueva hora de devolucion "
           ACCEPT HORA-PRESTAMO
           MOVE HORA-DEVOLUCION TO WS-HORA-DEVOLUCION-2
           NEXT SENTENCE
       WHEN 2
           MOVE HORA-DEVOLUCION TO WS-HORA-DEVOLUCION-1
           MOVE HORA-DEVOLUCION TO WS-HORA-DEVOLUCION-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-HORA-PRE
       END-EVALUATE.

       CONFIRMAR-STATUS.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE STATUS-INSTRUMENTO TO WS-STATUS-INSTRUMENTO-1
           DISPLAY " "
           DISPLAY "Ingrese nuevo status del instrumento"
           ACCEPT STATUS-INSTRUMENTO
           MOVE STATUS-INSTRUMENTO TO WS-STATUS-INSTRUMENTO-2
           NEXT SENTENCE
       WHEN 2
           MOVE STATUS-INSTRUMENTO TO WS-STATUS-INSTRUMENTO-1
           MOVE STATUS-INSTRUMENTO TO WS-STATUS-INSTRUMENTO-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-CODIGO
       END-EVALUATE.

       FINAL-EDITAR-CAMPOS.
       *>VERIFICAR SI LA LLAVE INGRESADA YA EXISTE
       READ PRESTAMOS-ARCHIVO RECORD
           KEY CEDULA-USUARIO
               INVALID KEY     MOVE 0 TO WS-INDICADOR *>NO SE ENCONTRÓ LA LLAVE.
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR. *>SI SE ENCONTRÓ LA LLAVE.
       *>VERIFICAR LLAVE INICIAL Y FINAL.
       IF WS-CEDULA-USUARIO-1 = WS-CEDULA-USUARIO-2 *>SOLO SE REESCRIBE
           MOVE 2 TO WS-INDICADOR.
       *>VERIFICAR INDICADOR.
       EVALUATE WS-INDICADOR
       WHEN 0 *>SI NO EXISTE, ELIMINO ANTERIOR Y SE ESCRIBE.
           MOVE WS-PRESTAMOS-REGISTRO-1
               TO PRESTAMOS-REGISTRO
           DELETE PRESTAMOS-ARCHIVO RECORD *>ELIMINO LLAVE ANTERIOR.
           WRITE PRESTAMOS-REGISTRO *>ESCRIBO NUEVA LLAVE.
               FROM WS-PRESTAMOS-REGISTRO-2
       WHEN 1 *>SI YA EXISTE EN LA BASE DE DATOS, INGRESAR OTRA CEDULA
           DISPLAY " "
           DISPLAY "El numero de cedula ingresado ya existe"
           DISPLAY " "
           DISPLAY "¿Que desea hacer?"
           DISPLAY "1.- Ingresar otro numero de cedula"
           DISPLAY "2.- Actualizar otro prestamo"
           DISPLAY "3.- Volver al menu"
           DISPLAY "4.- Salir"
           DISPLAY "Ingrese numero de opcion deseada:"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-CODIGO-IGUAL
       WHEN 2 *>SI LA LLAVE ES LA MISMA, SOLO SE REESCRIBE
           REWRITE PRESTAMOS-REGISTRO *>REESCRIBO LA LLAVE.
               FROM WS-PRESTAMOS-REGISTRO-2
       END-EVALUATE.

       DISPLAY " ".
       DISPLAY "Campo Actualizado correctamente.".

       CLOSE PRESTAMOS-ARCHIVO.
       PERFORM VOLVER-ACTUALIZAR.

       VALIDACION-CODIGO-IGUAL.
       EVALUATE WS-OPCION
       WHEN 1
           DISPLAY " "
           DISPLAY "Por favor ingrese otro numero de cedula"
           ACCEPT CEDULA-USUARIO
           MOVE CEDULA-USUARIO TO WS-CEDULA-USUARIO-2
           PERFORM FINAL-EDITAR-CAMPOS
       WHEN 2
           CLOSE PRESTAMOS-ARCHIVO
           PERFORM ACTUALIZAR
       WHEN 3
           CLOSE PRESTAMOS-ARCHIVO
           PERFORM PROGRAM-BEGIN
       WHEN 4
           CLOSE PRESTAMOS-ARCHIVO
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-CODIGO-IGUAL
       END-EVALUATE.


       EXIT PROGRAM.
