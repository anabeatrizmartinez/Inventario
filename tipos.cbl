       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tipos.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       SELECT OPTIONAL TIPO-INSTRUMENTO-ARCHIVO
       ASSIGN TO "c:\tipo_instrumento.dat"
       ORGANIZATION IS INDEXED
       RECORD KEY IS CODIGO-TIPO
       ACCESS MODE IS DYNAMIC.

       DATA DIVISION.
       FILE SECTION.
       FD TIPO-INSTRUMENTO-ARCHIVO.
           01 TIPO-INSTRUMENTO-REGISTRO.
               05 CODIGO-TIPO PIC X(3).
               05 NOMBRE-TIPO PIC X(40).
               05 CORRELATIVO PIC 9(3).

       WORKING-STORAGE SECTION.
       *>VARIABLES USADAS
       01 WS-TITULO.
           05 FILLER   PIC X(30) VALUE SPACES.
           05 WS-TIT   PIC X(20) VALUE "Tipo de instrumentos".
           05 FILLER   PIC X(30) VALUE SPACES.

       01 WS-TIPO-INSTRUMENTO-REGISTRO-1.
           05 WS-CODIGO-TIPO-1 PIC X(3).
           05 WS-NOMBRE-TIPO-1 PIC X(40).
           05 WS-CORRELATIVO-1 PIC 9(3).

       01 WS-TIPO-INSTRUMENTO-REGISTRO-2.
           05 WS-CODIGO-TIPO-2 PIC X(3).
           05 WS-NOMBRE-TIPO-2 PIC X(40).
           05 WS-CORRELATIVO-2 PIC 9(3).

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
       DISPLAY "---Registro de nuevo tipo de instrumento---".
       DISPLAY " ".
       DISPLAY "Indique codigo de tipo de instrumento".
       ACCEPT CODIGO-TIPO.

       *>VERIFICAR SI EL CODIGO YA EXISTE EN LA BASE DE DATOS
       OPEN I-O TIPO-INSTRUMENTO-ARCHIVO.
       READ TIPO-INSTRUMENTO-ARCHIVO RECORD
           KEY CODIGO-TIPO
               INVALID KEY     MOVE 0 TO WS-INDICADOR
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR.
       IF WS-INDICADOR = 0 *>SI NO EXISTE, GUARDO EL NUEVO REGISTRO
           DISPLAY " "
           DISPLAY "Ingrese nombre de tipo de instrumento"
           ACCEPT NOMBRE-TIPO
           MOVE 0 TO CORRELATIVO
           DISPLAY " "
           DISPLAY "Por ser un codigo nuevo, el numero correlativo "
           DISPLAY "se inicializa en 0"
           WRITE TIPO-INSTRUMENTO-REGISTRO
           DISPLAY " "
           DISPLAY "Tipo de instrumento deportivo "
           DISPLAY "registrado correctamente."
       ELSE *>SI EXISTE, SALIR
           DISPLAY " "
           DISPLAY "El tipo de instrumento ya existe.".

       CLOSE TIPO-INSTRUMENTO-ARCHIVO.
       PERFORM VOLVER-REGISTRAR.

       VOLVER-REGISTRAR.
       DISPLAY " ".
       DISPLAY "¿Que desea hacer?".
       DISPLAY "1.- Registrar otro tipo de instrumento deportivo".
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
       DISPLAY "---Actualizar tipo de instrumentos---".
       DISPLAY " ".
       DISPLAY "Indique codigo de tipo de instrumento ".
       DISPLAY "deportivo que desea actualizar".
       ACCEPT CODIGO-TIPO.
       *>VERIFICAR SI EL CODIGO YA EXISTE EN LA BASE DE DATOS
       OPEN I-O TIPO-INSTRUMENTO-ARCHIVO.
       READ TIPO-INSTRUMENTO-ARCHIVO RECORD
           KEY CODIGO-TIPO
               INVALID KEY     MOVE 0 TO WS-INDICADOR
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR.
       IF WS-INDICADOR = 0 *>SI NO EXISTE REGRESO AL MENU.
           DISPLAY " "
           DISPLAY "No existe el tipo de instrumento ingresado."
           CLOSE TIPO-INSTRUMENTO-ARCHIVO
           PERFORM VOLVER-ACTUALIZAR
       ELSE *>SI EXISTE MUESTRO LOS CAMPOS ANTES DE ACTUALIZAR.
           DISPLAY " "
           DISPLAY "Codigo de tipo de instrumento: " CODIGO-TIPO
           DISPLAY "Nombre de tipo de instrumento: " NOMBRE-TIPO
           DISPLAY "Numero correlativo: " CORRELATIVO
           PERFORM CONFIRMAR-ACTUALIZAR.

       VOLVER-ACTUALIZAR.
       DISPLAY " ".
       DISPLAY "¿Que desea hacer?".
       DISPLAY "1.- Actualizar otro tipo de instrumento deportivo".
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
       DISPLAY "¿Que desea hacer con este tipo de instrumento?".
       DISPLAY "1.- Editar campos".
       DISPLAY "2.- Eliminar tipo de instrumento".
       DISPLAY "3.- Actualizar otro tipo de instrumento deportivo"
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
           CLOSE TIPO-INSTRUMENTO-ARCHIVO
           PERFORM ACTUALIZAR
       WHEN 4
           CLOSE TIPO-INSTRUMENTO-ARCHIVO
           PERFORM PROGRAM-BEGIN
       WHEN 5
           CLOSE TIPO-INSTRUMENTO-ARCHIVO
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-CONFIRMAR-ACTUALIZAR
       END-EVALUATE.

       ELIMINAR. *>ELIMINAR EL TIPO DE INSTRUMENTO
       DISPLAY "¿Esta seguro de eliminar el "
       DISPLAY "tipo de instrumento " CODIGO-TIPO "? (S/N)".
       ACCEPT WS-SI-NO.
       PERFORM VALIDACION-ELIMINAR.

       VALIDACION-ELIMINAR.
       EVALUATE WS-SI-NO
       WHEN = "S" OR = "s"
           DELETE TIPO-INSTRUMENTO-ARCHIVO RECORD
           DISPLAY " "
           DISPLAY "Tipo de instrumento deportivo eliminado."
           CLOSE TIPO-INSTRUMENTO-ARCHIVO
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
       DISPLAY "Codigo de tipo de instrumento: " CODIGO-TIPO.
       PERFORM CONFIRMAR. *>VERIFICAR SI SE QUIERE EDITAR ESTE CAMPO O NO.
       PERFORM CONFIRMAR-CODIGO.

       DISPLAY " ".
       DISPLAY "Nombre de tipo de instrumento: " NOMBRE-TIPO.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-NOMBRE.

       DISPLAY " ".
       DISPLAY "Numero correlativo: " CORRELATIVO.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-CORRELATIVO.

       PERFORM FINAL-EDITAR-CAMPOS.

       CONFIRMAR.
       DISPLAY "1.- Editar".
       DISPLAY "2.- Siguiente campo".
       DISPLAY "Ingrese numero de opcion deseada:".
       ACCEPT WS-OPCION.

       CONFIRMAR-CODIGO.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE CODIGO-TIPO TO WS-CODIGO-TIPO-1 *>GUARDAR EL REGISTRO INICIAL.
           DISPLAY " "
           DISPLAY "Ingrese nuevo codigo de tipo "
           DISPLAY "de instrumento (3 letras)"
           ACCEPT CODIGO-TIPO
           MOVE CODIGO-TIPO TO WS-CODIGO-TIPO-2 *>GUARDAR EL REGISTRO NUEVO.
           NEXT SENTENCE
       WHEN 2
           MOVE CODIGO-TIPO TO WS-CODIGO-TIPO-1
           MOVE CODIGO-TIPO TO WS-CODIGO-TIPO-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-CODIGO
       END-EVALUATE.

       CONFIRMAR-NOMBRE.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE NOMBRE-TIPO TO WS-NOMBRE-TIPO-1
           DISPLAY " "
           DISPLAY "Ingrese nuevo nombre de tipo de instrumento"
           ACCEPT NOMBRE-TIPO
           MOVE NOMBRE-TIPO TO WS-NOMBRE-TIPO-2
           NEXT SENTENCE
       WHEN 2
           MOVE NOMBRE-TIPO TO WS-NOMBRE-TIPO-1
           MOVE NOMBRE-TIPO TO WS-NOMBRE-TIPO-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-NOMBRE
       END-EVALUATE.

       CONFIRMAR-CORRELATIVO.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE CORRELATIVO TO WS-CORRELATIVO-1
           *>VERIFICAR SI EL CODIGO YA EXISTE EN LA BASE DE DATOS
           PERFORM LEER-CODIGO-TIPO
           PERFORM NUEVO-CORRELATIVO
           MOVE CORRELATIVO TO WS-CORRELATIVO-2
           NEXT SENTENCE
       WHEN 2
           MOVE CORRELATIVO TO WS-CORRELATIVO-1
           MOVE CORRELATIVO TO WS-CORRELATIVO-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-CORRELATIVO
       END-EVALUATE.

       LEER-CODIGO-TIPO.
       READ TIPO-INSTRUMENTO-ARCHIVO RECORD
           KEY CODIGO-TIPO
               INVALID KEY     MOVE 0 TO WS-INDICADOR *>NO SE ENCONTRÓ LA LLAVE.
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR. *>SI SE ENCONTRÓ LA LLAVE.

       NUEVO-CORRELATIVO.
       IF WS-INDICADOR = 0 *>SI NO EXISTE, SE INICIALIZA EN 0
           MOVE 0 TO CORRELATIVO
           DISPLAY " "
           DISPLAY "Por ser un codigo nuevo, el numero correlativo "
           DISPLAY "se inicializa en 0"
       ELSE *>SI EXISTE, RECIBO NUEVO CORRELATIVO
           DISPLAY " "
           DISPLAY "El numero correlativo para el codigo " CODIGO-TIPO
           DISPLAY "es: " CORRELATIVO
           DISPLAY "Ingrese nuevo numero correlativo"
           ACCEPT CORRELATIVO.

       FINAL-EDITAR-CAMPOS.
       *>VERIFICAR SI LA LLAVE INGRESADA YA EXISTE
       PERFORM LEER-CODIGO-TIPO.
       *>VERIFICAR LLAVE INICIAL Y FINAL.
       IF WS-CODIGO-TIPO-1 = WS-CODIGO-TIPO-2 *>SOLO SE REESCRIBE
           MOVE 2 TO WS-INDICADOR.
       *>VERIFICAR INDICADOR.
       EVALUATE WS-INDICADOR
       WHEN 0 *>SI NO EXISTE, ELIMINO ANTERIOR Y SE ESCRIBE.
           MOVE WS-TIPO-INSTRUMENTO-REGISTRO-1
               TO TIPO-INSTRUMENTO-REGISTRO
           DELETE TIPO-INSTRUMENTO-ARCHIVO RECORD *>ELIMINO LLAVE ANTERIOR.
           WRITE TIPO-INSTRUMENTO-REGISTRO *>ESCRIBO NUEVA LLAVE.
               FROM WS-TIPO-INSTRUMENTO-REGISTRO-2
       WHEN 1 *>SI YA EXISTE EN LA BASE DE DATOS, INGRESAR OTRO CODIGO
           DISPLAY " "
           DISPLAY "El codigo de tipo de instrumento ya existe "
           DISPLAY " "
           DISPLAY "¿Que desea hacer?"
           DISPLAY "1.- Ingresar otro codigo de tipo"
           DISPLAY "2.- Actualizar otro tipo de instrumento deportivo"
           DISPLAY "3.- Volver al menu"
           DISPLAY "4.- Salir"
           DISPLAY "Ingrese numero de opcion deseada:"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-CODIGO-IGUAL
       WHEN 2 *>SI LA LLAVE ES LA MISMA, SOLO SE REESCRIBE
           REWRITE TIPO-INSTRUMENTO-REGISTRO *>REESCRIBO LA LLAVE.
               FROM WS-TIPO-INSTRUMENTO-REGISTRO-2
       END-EVALUATE.

       DISPLAY " ".
       DISPLAY "Campo Actualizado correctamente.".

       CLOSE TIPO-INSTRUMENTO-ARCHIVO.
       PERFORM VOLVER-ACTUALIZAR.

       VALIDACION-CODIGO-IGUAL.
       EVALUATE WS-OPCION
       WHEN 1
           DISPLAY " "
           DISPLAY "Por favor ingrese otro codigo de tipo"
           ACCEPT CODIGO-TIPO
           MOVE CODIGO-TIPO TO WS-CODIGO-TIPO-2
           PERFORM FINAL-EDITAR-CAMPOS
       WHEN 2
           CLOSE TIPO-INSTRUMENTO-ARCHIVO
           PERFORM ACTUALIZAR
       WHEN 3
           CLOSE TIPO-INSTRUMENTO-ARCHIVO
           PERFORM PROGRAM-BEGIN
       WHEN 4
           CLOSE TIPO-INSTRUMENTO-ARCHIVO
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-CODIGO-IGUAL
       END-EVALUATE.


       EXIT PROGRAM.
