       IDENTIFICATION DIVISION.
       PROGRAM-ID. Instrumentos.

       ENVIRONMENT DIVISION.
       *>INFORMACIÓN DE LOS ARCHIVOS FÍSICOS.
       INPUT-OUTPUT SECTION. *>INFORMACIÓN DE ENTRADA-SALIDA.

       FILE-CONTROL.
       *>ARCHIVO FISICO.
       *>SE INDICA LA UBICACION DEL ARCHIVO Y SE CONECTA CON EL ARCHIVO LOGICO.
       SELECT OPTIONAL INSTRUMENTOS-ARCHIVO *>NOMBRE QUE SE LE DA AL ARCHIVO.
                                            *>CON OPTIONAL SE EVITAN ERRORES DE OPEN.
       ASSIGN TO "c:\instrumentos.dat" *>UBICACION AL ARCHIVO. ABRIR OPENCOBOLIDE
                                       *>COMO ADMINISTRADOR PARA QUE CREE EL ARCHIVO
                                       *>SIN PROBLEMA EN C.
       ORGANIZATION IS INDEXED *>TIPO DE ORGANIZACIÓN INDEXADA PARA USAR LLAVES.
       RECORD KEY IS ID-INSTRUMENTO *>LLAVE PRIMARIA.
       ACCESS MODE IS DYNAMIC *>TIPO DE ACCESO DINÁMICO PARA PODER ACCEDER A UN CAMPO
                               *>CUANDO SE NECESITE SIN QUE PASE PRIMERO POR TODOS.
       FILE STATUS FS-INSTRUMENTO.

       SELECT OPTIONAL TIPO-INSTRUMENTO-ARCHIVO
       ASSIGN TO "c:\tipo_instrumento.dat"
       ORGANIZATION IS INDEXED
       RECORD KEY IS CODIGO-TIPO
       ACCESS MODE IS DYNAMIC.

       DATA DIVISION.
       *>INFORMACIÓN DE LOS ARCHIVOS LÓGICOS, CON SU REGISTRO Y VARIABLES A USAR.
       FILE SECTION.
       *>ARCHIVO LOGICO.
       *>CON EL NOMBRE A CONECTAR CON EL ARCHIVO FISICO.
       FD INSTRUMENTOS-ARCHIVO. *>NOMBRE DEL ARCHIVO IGUAL AL FÍSICO.
           01 INSTRUMENTOS-REGISTRO. *>REGISTRO DEL ARCHIVO.
               05 ID-INSTRUMENTO       PIC X(6).
               05 TIPO-INSTRUMENTO     PIC X(40).
               05 FECHA-ADQUISICION    PIC 9(6).
               05 FORMA-ADQUISICION    PIC X(8).
               05 ID-PROVEEDOR         PIC X(11).
               05 STATUS-INSTRUMENTO   PIC X(14).
               05 FECHA-STATUS         PIC 9(6).

       FD TIPO-INSTRUMENTO-ARCHIVO.
           01 TIPO-INSTRUMENTO-REGISTRO.
               05 CODIGO-TIPO PIC X(3).
               05 NOMBRE-TIPO PIC X(40).
               05 CORRELATIVO PIC 9(3).

       WORKING-STORAGE SECTION.
       *>VARIABLES USADAS.
       01 WS-TITULO.
           05 FILLER   PIC X(34) VALUE SPACES.
           05 WS-TIT   PIC X(16) VALUE "Instrumentos".
           05 FILLER   PIC X(34) VALUE SPACES.

       01 WS-TIPO-INSTRUMENTO PIC X(40).
       01 WS-CODIGO-TIPO PIC X(3).

       77 WS-OPCION PIC 9(2).
       77 WS-INDICADOR PIC 9(1).
       77 WS-LEE-TIPO PIC 9(1).
       77 WS-LEE-INSTRUMENTO PIC 9(1).
       77 FS-INSTRUMENTO PIC 9(2).

       LINKAGE SECTION.
       *>VARIABLES PARA CONECTAR CON OTROS PROGRAMAS.
       77 LS-CONECTAR PIC X. *>VARIABLE PARA CONECTAR CON EL MENU.

       PROCEDURE DIVISION USING LS-CONECTAR. *>CONECTO CON EL MENU.
       MAIN SECTION.
       *>PROGRAMA PRINCIPAL.
       PROGRAM-BEGIN.
       PERFORM MENU-REGISTRO.

       STOP RUN.

       *>RUTINAS.
       MENU-REGISTRO.
       DISPLAY " ".
       DISPLAY WS-TITULO.
       DISPLAY " ".
       DISPLAY "1) Registrar".
       DISPLAY "2) Actualizar".
       DISPLAY "3) Salir".
       ACCEPT WS-OPCION.
       PERFORM VALIDACION-MENU.

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
       DISPLAY "Registro de nuevo instrumento:"
       DISPLAY " ".
       DISPLAY "Indique tipo de instrumento".
       ACCEPT WS-TIPO-INSTRUMENTO.
       DISPLAY "Indique codigo del tipo de instrumento (3 letras)".
       ACCEPT WS-CODIGO-TIPO.

       *>VERIFICAR SI EL CODIGO YA EXISTE EN LA BASE DE DATOS DE TIPO DE INSTRUMENTO.
       OPEN I-O TIPO-INSTRUMENTO-ARCHIVO.
       OPEN I-O INSTRUMENTOS-ARCHIVO.
       MOVE WS-CODIGO-TIPO TO CODIGO-TIPO.
       READ TIPO-INSTRUMENTO-ARCHIVO RECORD *>LEER UN REGISTRO DEL ARCHIVO
           KEY CODIGO-TIPO *>INDICO EN BASE A LA LLAVE QUE SE LEE.
               INVALID KEY     MOVE 0 TO WS-INDICADOR *>NO SE ENCONTRÓ LA LLAVE.
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR. *>SI SE ENCONTRÓ LA LLAVE.

       *>REGISTRAR DATOS PARA GENERAR EL CODIGO DEL INSTRUMENTO.
       IF WS-INDICADOR = 0 *>SI NO EXISTE, GUARDO EL NUEVO REGISTRO EN TIPO-INSTRUMENTO
           MOVE WS-TIPO-INSTRUMENTO TO NOMBRE-TIPO
           MOVE 1 TO CORRELATIVO
           WRITE TIPO-INSTRUMENTO-REGISTRO
       ELSE *>SI EXISTE, ACTUALIZO EL CORRELATIVO.
           ADD 1 TO CORRELATIVO
           REWRITE TIPO-INSTRUMENTO-REGISTRO.

       STRING CODIGO-TIPO, CORRELATIVO
           INTO ID-INSTRUMENTO. *>ESTE ES EL CODIGO DEL INSTRUMENTO.

       *>SOLICITUD DE LOS DEMÁS DATOS DEL ARCHIVO INSTRUMENTOS.
       MOVE WS-TIPO-INSTRUMENTO TO TIPO-INSTRUMENTO.
       DISPLAY "Ingrese Fecha de adquisicion (AA/MM/DD)".
       ACCEPT FECHA-ADQUISICION.
       DISPLAY "Ingrese forma de adquisicion (compra o donacion)".
       ACCEPT FORMA-ADQUISICION.
       DISPLAY "Ingrese Numero de cedula o RIF del Proveedor".
       ACCEPT ID-PROVEEDOR.
       DISPLAY "Ingrese Status del instrumento: ".
       DISPLAY "1.- En buen estado".
       DISPLAY "2.- Deteriorado".
       DISPLAY "3.- Extraviado".
       DISPLAY "4.- Prestado".
       DISPLAY "5.- Desincorporado".
       DISPLAY "Indique numero de opcion deseada:"
       ACCEPT WS-OPCION.
       PERFORM VALIDACION-STATUS.
       DISPLAY "Ingrese Fecha del status (AA/MM/DD)".
       ACCEPT FECHA-STATUS.

       WRITE INSTRUMENTOS-REGISTRO.

       CLOSE TIPO-INSTRUMENTO-ARCHIVO.
       CLOSE INSTRUMENTOS-ARCHIVO.

       PERFORM VOLVER.

       VOLVER.
       DISPLAY " ".
       DISPLAY "¿Que desea hacer?".
       DISPLAY "1.- Volver al menu".
       DISPLAY "2.- Salir".
       DISPLAY "Ingrese numero de opcion deseada:".
       ACCEPT WS-OPCION.
       PERFORM VALIDACION-VOLVER.

       VALIDACION-VOLVER.
       EVALUATE WS-OPCION
       WHEN 1
           PERFORM MENU-REGISTRO
       WHEN 2
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-VOLVER
       END-EVALUATE.

       VALIDACION-STATUS.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE "En buen estado" TO STATUS-INSTRUMENTO
       WHEN 2
           MOVE "Deteriorado" TO STATUS-INSTRUMENTO
       WHEN 3
           MOVE "Extraviado" TO STATUS-INSTRUMENTO
       WHEN 4
           MOVE "Prestado" TO STATUS-INSTRUMENTO
       WHEN 5
           MOVE "Desincorporado" TO STATUS-INSTRUMENTO
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-STATUS
       END-EVALUATE.

       ACTUALIZAR.
       DISPLAY "Actualizar instrumentos:".
       DISPLAY " ".
       OPEN I-O INSTRUMENTOS-ARCHIVO.
       OPEN I-O TIPO-INSTRUMENTO-ARCHIVO.

       *>MOSTRAR INSTRUMENTOS REGISTRADOS.
       MOVE 0 TO WS-LEE-INSTRUMENTO.
       PERFORM SIGUIENTE-REGISTRO-INSTRUMENTOS.

       IF WS-LEE-INSTRUMENTO = 1
           DISPLAY "No se encontraron instrumentos."
       ELSE
           PERFORM MUESTRA-INSTRUMENTOS UNTIL WS-LEE-INSTRUMENTO = 1.

       *>MOSTRAR TIPOS DE INSTRUMENTOS REGISTRADOS.
       MOVE 0 TO WS-LEE-TIPO.
       PERFORM SIGUIENTE-REGISTRO-TIPO.

       IF WS-LEE-TIPO = 1
           DISPLAY "No se encontraron tipos de instrumentos."
       ELSE
           PERFORM MUESTRA-CAMPOS-TIPO UNTIL WS-LEE-TIPO = 1.

       CLOSE INSTRUMENTOS-ARCHIVO.
       CLOSE TIPO-INSTRUMENTO-ARCHIVO.

       PERFORM VOLVER.

       SIGUIENTE-REGISTRO-TIPO.
       READ TIPO-INSTRUMENTO-ARCHIVO NEXT RECORD
       AT END
       MOVE 1 TO WS-LEE-TIPO.

       SIGUIENTE-REGISTRO-INSTRUMENTOS.
       READ INSTRUMENTOS-ARCHIVO NEXT RECORD
       AT END
       MOVE 1 TO WS-LEE-INSTRUMENTO.

       MUESTRA-CAMPOS-TIPO.
       DISPLAY " ".
       DISPLAY "Tipos de instrumentos:".
       DISPLAY " ".
       DISPLAY "Codigo de tipo de instrumento: " CODIGO-TIPO.
       DISPLAY "Nombre de tipo de instrumento: " NOMBRE-TIPO.
       DISPLAY "Numero Correlativo: " CORRELATIVO.
       PERFORM SIGUIENTE-REGISTRO-TIPO.

       MUESTRA-INSTRUMENTOS.
       DISPLAY " ".
       DISPLAY "Instrumentos:"
       DISPLAY " ".
       DISPLAY "Codigo de instrumento: " ID-INSTRUMENTO.
       DISPLAY "Tipo de instrumento: " TIPO-INSTRUMENTO.
       DISPLAY "Fecha de adquisicion: " FECHA-ADQUISICION.
       DISPLAY "Forma de adquisicion: " FORMA-ADQUISICION.
       DISPLAY "Cedula o RIF de Proveedor: " ID-PROVEEDOR.
       DISPLAY "Status del instrumento: " STATUS-INSTRUMENTO.
       DISPLAY "Fecha del status: " FECHA-STATUS.
       PERFORM SIGUIENTE-REGISTRO-INSTRUMENTOS.

       DISPLAY "ERROR: " FS-INSTRUMENTO.


       EXIT PROGRAM. *>LOS PROGRAMAS LLAMADOS CON CALL DEBEN TERMINAR CON
                     *>EXIT PROGRAM Y NO CON END PROGRAM.
