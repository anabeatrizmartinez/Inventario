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
       ACCESS MODE IS DYNAMIC. *>TIPO DE ACCESO DINÁMICO PARA PODER ACCEDER A UN CAMPO
                               *>CUANDO SE NECESITE SIN QUE PASE PRIMERO POR TODOS.

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

       01 WS-INSTRUMENTOS-REGISTRO.
               05 WS-ID-INSTRUMENTO       PIC X(6).
               05 WS-TIPO-INSTRUMENTO     PIC X(40).
               05 WS-FECHA-ADQUISICION    PIC 9(6).
               05 WS-FORMA-ADQUISICION    PIC X(8).
               05 WS-ID-PROVEEDOR         PIC X(11).
               05 WS-STATUS-INSTRUMENTO   PIC X(14).
               05 WS-FECHA-STATUS         PIC 9(6).

       01 WS-CODIGO-TIPO PIC X(3).

       77 WS-OPCION PIC 9(2).
       77 WS-INDICADOR PIC 9(1).
       77 WS-SI-NO PIC X(1).

       LINKAGE SECTION.
       *>VARIABLES PARA CONECTAR CON OTROS PROGRAMAS.
       77 LS-CONECTAR PIC X. *>VARIABLE PARA CONECTAR CON EL MENU.

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

       APERTURA.
       OPEN I-O TIPO-INSTRUMENTO-ARCHIVO.
       OPEN I-O INSTRUMENTOS-ARCHIVO.

       CIERRE.
       CLOSE TIPO-INSTRUMENTO-ARCHIVO.
       CLOSE INSTRUMENTOS-ARCHIVO.

       REGISTRAR.
       DISPLAY "---Registro de nuevo instrumento---".
       DISPLAY " ".
       DISPLAY "Indique tipo de instrumento".
       ACCEPT WS-TIPO-INSTRUMENTO.
       DISPLAY " ".
       DISPLAY "Indique codigo del tipo de instrumento (3 letras)".
       ACCEPT WS-CODIGO-TIPO.

       *>VERIFICAR SI EL CODIGO YA EXISTE EN LA BASE DE DATOS DE TIPO DE INSTRUMENTO.
       PERFORM APERTURA.
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
       DISPLAY " ".
       DISPLAY "Codigo de instrumento generado: " ID-INSTRUMENTO.

       *>SOLICITUD DE LOS DEMÁS DATOS DEL ARCHIVO INSTRUMENTOS.
       MOVE WS-TIPO-INSTRUMENTO TO TIPO-INSTRUMENTO.
       DISPLAY " ".
       DISPLAY "Ingrese Fecha de adquisicion (AA/MM/DD)".
       ACCEPT FECHA-ADQUISICION.
       DISPLAY " ".
       DISPLAY "Ingrese forma de adquisicion (compra o donacion)".
       ACCEPT FORMA-ADQUISICION.
       DISPLAY " ".
       DISPLAY "Ingrese Numero de cedula o RIF del Proveedor".
       ACCEPT ID-PROVEEDOR.
       DISPLAY " ".
       DISPLAY "Ingrese Status del instrumento: ".
       DISPLAY "1.- En buen estado".
       DISPLAY "2.- Deteriorado".
       DISPLAY "3.- Extraviado".
       DISPLAY "4.- Prestado".
       DISPLAY "5.- Desincorporado".
       DISPLAY "Indique numero de opcion deseada:"
       ACCEPT WS-OPCION.
       PERFORM VALIDACION-STATUS.
       DISPLAY " ".
       DISPLAY "Ingrese Fecha del status (AA/MM/DD)".
       ACCEPT FECHA-STATUS.

       WRITE INSTRUMENTOS-REGISTRO.

       PERFORM CIERRE.

       DISPLAY " ".
       DISPLAY "Instrumento deportivo registrado correctamente."

       PERFORM VOLVER-REGISTRAR.

       VOLVER-REGISTRAR.
       DISPLAY " ".
       DISPLAY "¿Que desea hacer?".
       DISPLAY "1.- Registrar otro instrumento deportivo".
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
       DISPLAY "---Actualizar instrumentos---".
       DISPLAY " ".
       DISPLAY "Indique codigo de instrumento ".
       DISPLAY "deportivo que desea actualizar".
       ACCEPT ID-INSTRUMENTO.

       *>VERIFICAR SI EL CODIGO YA EXISTE EN LA BASE DE DATOS DE INSTRUMENTOS.
       PERFORM APERTURA.
       READ INSTRUMENTOS-ARCHIVO RECORD
           KEY ID-INSTRUMENTO
               INVALID KEY     MOVE 0 TO WS-INDICADOR *>NO SE ENCONTRÓ LA LLAVE.
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR. *>SI SE ENCONTRÓ LA LLAVE.

       IF WS-INDICADOR = 0 *>SI NO EXISTE REGRESO AL MENU.
           DISPLAY " "
           DISPLAY "No existe el instrumento ingresado."
           PERFORM CIERRE
           PERFORM VOLVER-ACTUALIZAR
       ELSE *>SI EXISTE MUESTRO LOS CAMPOS ANTES DE ACTUALIZAR.
           DISPLAY " "
           DISPLAY "Codigo de instrumento: " ID-INSTRUMENTO
           DISPLAY "Tipo de instrumento: " TIPO-INSTRUMENTO
           DISPLAY "Fecha de adquisicion: " FECHA-ADQUISICION
           DISPLAY "Forma de adquisicion: " FORMA-ADQUISICION
           DISPLAY "Cedula o RIF de Proveedor: " ID-PROVEEDOR
           DISPLAY "Status del instrumento: " STATUS-INSTRUMENTO
           DISPLAY "Fecha del status: " FECHA-STATUS
           PERFORM CONFIRMAR-ACTUALIZAR.

       VOLVER-ACTUALIZAR.
       DISPLAY " ".
       DISPLAY "¿Que desea hacer?".
       DISPLAY "1.- Actualizar otro instrumento deportivo".
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
       DISPLAY "¿Que desea hacer con este instrumento?".
       DISPLAY "1.- Editar campos".
       DISPLAY "2.- Eliminar instrumento".
       DISPLAY "3.- Actualizar otro instrumento deportivo"
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
           PERFORM CIERRE
           PERFORM ACTUALIZAR
       WHEN 4
           PERFORM CIERRE
           PERFORM PROGRAM-BEGIN
       WHEN 5
           PERFORM CIERRE
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-CONFIRMAR-ACTUALIZAR
       END-EVALUATE.

       ELIMINAR. *>ELIMINAR EL INSTRUMENTO
       DISPLAY "¿Esta seguro de eliminar el "
       DISPLAY "instrumento " ID-INSTRUMENTO "? (S/N)".
       ACCEPT WS-SI-NO.
       PERFORM VALIDACION-ELIMINAR.

       VALIDACION-ELIMINAR.
       EVALUATE WS-SI-NO
       WHEN = "S" OR = "s"
           DELETE INSTRUMENTOS-ARCHIVO RECORD
           DISPLAY " "
           DISPLAY "Instrumento deportivo eliminado."
           PERFORM CIERRE
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
       DISPLAY "Codigo de instrumento: " ID-INSTRUMENTO.
       PERFORM CONFIRMAR. *>VERIFICAR SI SE QUIERE EDITAR ESTE CAMPO O NO.
       PERFORM CONFIRMAR-ID.

       DISPLAY " ".
       DISPLAY "Tipo de instrumento: " TIPO-INSTRUMENTO.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-TIPO.

       DISPLAY " ".
       DISPLAY "Fecha de adquisicion: " FECHA-ADQUISICION.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-FECHA-AD.

       DISPLAY " ".
       DISPLAY "Forma de adquisicion: " FORMA-ADQUISICION.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-FORMA-AD.

       DISPLAY " ".
       DISPLAY "Cedula o RIF de Proveedor: " ID-PROVEEDOR.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-PROVEEDOR.

       DISPLAY " ".
       DISPLAY "Status del instrumento: " STATUS-INSTRUMENTO.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-STATUS.

       DISPLAY " ".
       DISPLAY "Fecha del status: " FECHA-STATUS.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-FECHA-ST.

       PERFORM FINAL-EDITAR-CAMPOS.

       CONFIRMAR.
       DISPLAY "1.- Editar".
       DISPLAY "2.- Siguiente campo".
       DISPLAY "Ingrese numero de opcion deseada:".
       ACCEPT WS-OPCION.

       CONFIRMAR-ID. *>PARA ASEGURAR QUE SE INGRESÓ 1 O 2.
       EVALUATE WS-OPCION
       WHEN 1
           DISPLAY " "
           DISPLAY "Ingrese nuevo codigo de tipo "
           DISPLAY "de instrumento (3 letras)"
           ACCEPT CODIGO-TIPO
           *>VERIFICAR SI EL CODIGO YA EXISTE EN LA BASE DE DATOS DE TIPO DE INSTRUMENTO.
           PERFORM LEER-CODIGO-TIPO
           *>REGISTRAR DATOS PARA GENERAR EL CODIGO DEL INSTRUMENTO.
           PERFORM NUEVO-CORRELATIVO
           STRING CODIGO-TIPO, CORRELATIVO
               INTO WS-ID-INSTRUMENTO *>ESTE ES EL CODIGO DEL INSTRUMENTO.
           DISPLAY " "
           DISPLAY "Codigo de instrumento generado: " WS-ID-INSTRUMENTO
           NEXT SENTENCE *>YA NO SE USA, PERO SU OTRA OPCION, EXIT, NO CUMPLE
                         *>CON LA FUNCION DESEADA EN ESTE CASO.
       WHEN 2
           MOVE ID-INSTRUMENTO TO WS-ID-INSTRUMENTO
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-ID
       END-EVALUATE.

       LEER-CODIGO-TIPO.
       READ TIPO-INSTRUMENTO-ARCHIVO RECORD
           KEY CODIGO-TIPO
               INVALID KEY     MOVE 0 TO WS-INDICADOR *>NO SE ENCONTRÓ LA LLAVE.
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR. *>SI SE ENCONTRÓ LA LLAVE.

       NUEVO-CORRELATIVO.
       IF WS-INDICADOR = 0 *>SI NO EXISTE, GUARDO EL NUEVO REGISTRO EN TIPO-INSTRUMENTO
           DISPLAY " "
           DISPLAY "Ingrese nombre de tipo de instrumento"
           ACCEPT NOMBRE-TIPO
           MOVE 1 TO CORRELATIVO
           WRITE TIPO-INSTRUMENTO-REGISTRO
       ELSE *>SI EXISTE, ACTUALIZO EL CORRELATIVO.
           ADD 1 TO CORRELATIVO
           REWRITE TIPO-INSTRUMENTO-REGISTRO.

       CONFIRMAR-TIPO.
       EVALUATE WS-OPCION
       WHEN 1
           DISPLAY " "
           DISPLAY "Ingrese nuevo Tipo de instrumento "
           ACCEPT WS-TIPO-INSTRUMENTO
           NEXT SENTENCE
       WHEN 2
           MOVE TIPO-INSTRUMENTO TO WS-TIPO-INSTRUMENTO
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-TIPO
       END-EVALUATE.

       CONFIRMAR-FECHA-AD.
       EVALUATE WS-OPCION
       WHEN 1
           DISPLAY " "
           DISPLAY "Ingrese nueva fecha de adquisicion (AA/MM/DD)"
           ACCEPT WS-FECHA-ADQUISICION
           NEXT SENTENCE
       WHEN 2
           MOVE FECHA-ADQUISICION TO WS-FECHA-ADQUISICION
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-FECHA-AD
       END-EVALUATE.

       CONFIRMAR-FORMA-AD.
       EVALUATE WS-OPCION
       WHEN 1
           DISPLAY " "
        DISPLAY "Ingrese nueva forma de adquisicion (compra o donacion)"
           ACCEPT WS-FORMA-ADQUISICION
           NEXT SENTENCE
       WHEN 2
           MOVE FORMA-ADQUISICION TO WS-FORMA-ADQUISICION
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-FORMA-AD
       END-EVALUATE.

       CONFIRMAR-PROVEEDOR.
       EVALUATE WS-OPCION
       WHEN 1
           DISPLAY " "
           DISPLAY "Ingrese nuevo numero de cedula o RIF de Proveedor"
           ACCEPT WS-ID-PROVEEDOR
           NEXT SENTENCE
       WHEN 2
           MOVE ID-PROVEEDOR TO WS-ID-PROVEEDOR
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-PROVEEDOR
       END-EVALUATE.

       CONFIRMAR-STATUS.
       EVALUATE WS-OPCION
       WHEN 1
           DISPLAY " "
           DISPLAY "Ingrese nuevo status del instrumento"
           ACCEPT WS-STATUS-INSTRUMENTO
           NEXT SENTENCE
       WHEN 2
           MOVE STATUS-INSTRUMENTO TO WS-STATUS-INSTRUMENTO
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-STATUS
       END-EVALUATE.

       CONFIRMAR-FECHA-ST.
       EVALUATE WS-OPCION
       WHEN 1
           DISPLAY " "
           DISPLAY "Ingrese nueva fecha de status (AA/MM/DD)"
           ACCEPT WS-FECHA-STATUS
           NEXT SENTENCE
       WHEN 2
           MOVE FECHA-STATUS TO WS-FECHA-STATUS
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-FECHA-ST
       END-EVALUATE.

       FINAL-EDITAR-CAMPOS.
       IF ID-INSTRUMENTO = WS-ID-INSTRUMENTO *>LA LLAVE ID-INSTRUMENTO EXISTE.
           REWRITE INSTRUMENTOS-REGISTRO FROM WS-INSTRUMENTOS-REGISTRO
       ELSE *>LA LLAVE ID-INSTRUMENTO ES NUEVA.
           DELETE INSTRUMENTOS-ARCHIVO RECORD *>ELIMINO LLAVE ACTUAL.
           WRITE INSTRUMENTOS-REGISTRO FROM WS-INSTRUMENTOS-REGISTRO.*>ESCRIBO LA NUEVA
       DISPLAY " ".
       DISPLAY "Campo Actualizado correctamente.".

       PERFORM CIERRE.
       PERFORM VOLVER-ACTUALIZAR.


       EXIT PROGRAM. *>LOS PROGRAMAS LLAMADOS CON CALL DEBEN TERMINAR CON
                     *>EXIT PROGRAM Y NO CON END PROGRAM.
