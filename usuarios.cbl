       IDENTIFICATION DIVISION.
       PROGRAM-ID. Usuarios.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       SELECT OPTIONAL USUARIOS-ARCHIVO
       ASSIGN TO "c:\usuarios.dat"
       ORGANIZATION IS INDEXED
       RECORD KEY IS CEDULA-USUARIO
       ACCESS MODE IS DYNAMIC.

       DATA DIVISION.
       FILE SECTION.
       FD USUARIOS-ARCHIVO.
           01 USUARIOS-REGISTRO.
               05 CEDULA-USUARIO   PIC X(11).
               05 NOMBRE-USUARIO   PIC X(40).
               05 SEXO             PIC X(9).
               05 DIRECCION        PIC X(100).
               05 TELEFONO         PIC 9(11).
               05 TIPO-INSTRUMENTO PIC X(40).

       WORKING-STORAGE SECTION.
       *>VARIABLES USADAS
       01 WS-TITULO.
           05 FILLER   PIC X(36) VALUE SPACES.
           05 WS-TIT   PIC X(11) VALUE "Usuarios".
           05 FILLER   PIC X(36) VALUE SPACES.

       01 WS-USUARIOS-REGISTRO-1.
               05 WS-CEDULA-USUARIO-1   PIC X(11).
               05 WS-NOMBRE-USUARIO-1   PIC X(40).
               05 WS-SEXO-1             PIC X(9).
               05 WS-DIRECCION-1        PIC X(100).
               05 WS-TELEFONO-1         PIC 9(11).
               05 WS-TIPO-INSTRUMENTO-1 PIC X(40).

       01 WS-USUARIOS-REGISTRO-2.
               05 WS-CEDULA-USUARIO-2   PIC X(11).
               05 WS-NOMBRE-USUARIO-2   PIC X(40).
               05 WS-SEXO-2             PIC X(9).
               05 WS-DIRECCION-2        PIC X(100).
               05 WS-TELEFONO-2         PIC 9(11).
               05 WS-TIPO-INSTRUMENTO-2 PIC X(40).

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
       DISPLAY "---Registro de nuevo usuario---".
       DISPLAY " ".
       DISPLAY "Indique numero de cedula del usuario".
       ACCEPT CEDULA-USUARIO.
       *>VERIFICAR SI LA CEDULA YA EXISTE EN LA BASE DE DATOS
       OPEN I-O USUARIOS-ARCHIVO.
       READ USUARIOS-ARCHIVO RECORD
           KEY CEDULA-USUARIO
               INVALID KEY     MOVE 0 TO WS-INDICADOR
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR.
       IF WS-INDICADOR = 0 *>SI NO EXISTE, GUARDO EL NUEVO REGISTRO
           DISPLAY " "
           DISPLAY "Ingrese nombre del usuario"
           ACCEPT NOMBRE-USUARIO
           DISPLAY " "
           DISPLAY "Ingrese sexo del usuario (femenino o masculino)"
           ACCEPT SEXO
           DISPLAY " "
           DISPLAY "Ingrese direccion del usuario"
           ACCEPT DIRECCION
           DISPLAY " "
           DISPLAY "Ingrese telefono del usuario"
           ACCEPT TELEFONO
           DISPLAY " "
           DISPLAY "Ingrese nombre de tipo de instrumento"
           ACCEPT TIPO-INSTRUMENTO
           WRITE USUARIOS-REGISTRO
           DISPLAY " "
           DISPLAY "Usuario registrado correctamente."
       ELSE *>SI EXISTE, SALIR
           DISPLAY " "
           DISPLAY "El usuario ya existe.".

       CLOSE USUARIOS-ARCHIVO.
       PERFORM VOLVER-REGISTRAR.

       VOLVER-REGISTRAR.
       DISPLAY " ".
       DISPLAY "¿Que desea hacer?".
       DISPLAY "1.- Registrar otro usuario".
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
       DISPLAY "---Actualizar usuarios---".
       DISPLAY " ".
       DISPLAY "Indique numero de cedula del usuario".
       ACCEPT CEDULA-USUARIO.
       *>VERIFICAR SI LA CEDULA YA EXISTE EN LA BASE DE DATOS
       OPEN I-O USUARIOS-ARCHIVO.
       READ USUARIOS-ARCHIVO RECORD
           KEY CEDULA-USUARIO
               INVALID KEY     MOVE 0 TO WS-INDICADOR
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR.
       IF WS-INDICADOR = 0 *>SI NO EXISTE REGRESO AL MENU.
           DISPLAY " "
           DISPLAY "No existe el usuario ingresado."
           CLOSE USUARIOS-ARCHIVO
           PERFORM VOLVER-ACTUALIZAR
       ELSE *>SI EXISTE MUESTRO LOS CAMPOS ANTES DE ACTUALIZAR.
           DISPLAY " "
           DISPLAY "Numero de cedula del usuario: " CEDULA-USUARIO
           DISPLAY "Nombre del usuario: " NOMBRE-USUARIO
           DISPLAY "Sexo del usuario: " SEXO
           DISPLAY "Direccion del usuario: " DIRECCION
           DISPLAY "Telefono del usuario: " TELEFONO
           DISPLAY "Nombre de tipo de instrumento: " TIPO-INSTRUMENTO
           PERFORM CONFIRMAR-ACTUALIZAR.

       VOLVER-ACTUALIZAR.
       DISPLAY " ".
       DISPLAY "¿Que desea hacer?".
       DISPLAY "1.- Actualizar otro usuario".
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
       DISPLAY "¿Que desea hacer con este usuario?".
       DISPLAY "1.- Editar campos".
       DISPLAY "2.- Eliminar usuario".
       DISPLAY "3.- Actualizar otro usuario"
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
           CLOSE USUARIOS-ARCHIVO
           PERFORM ACTUALIZAR
       WHEN 4
           CLOSE USUARIOS-ARCHIVO
           PERFORM PROGRAM-BEGIN
       WHEN 5
           CLOSE USUARIOS-ARCHIVO
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-CONFIRMAR-ACTUALIZAR
       END-EVALUATE.

       ELIMINAR. *>ELIMINAR EL USUARIO
       DISPLAY "¿Esta seguro de eliminar el "
       DISPLAY "usuario " CEDULA-USUARIO "? (S/N)".
       ACCEPT WS-SI-NO.
       PERFORM VALIDACION-ELIMINAR.

       VALIDACION-ELIMINAR.
       EVALUATE WS-SI-NO
       WHEN = "S" OR = "s"
           DELETE USUARIOS-ARCHIVO RECORD
           DISPLAY " "
           DISPLAY "Usuario eliminado."
           CLOSE USUARIOS-ARCHIVO
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
       DISPLAY "Nombre del usuario: " NOMBRE-USUARIO.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-NOMBRE.

       DISPLAY " ".
       DISPLAY "Sexo del usuario: " SEXO.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-SEXO.

       DISPLAY " ".
       DISPLAY "Direccion del usuario: " DIRECCION.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-DIRECCION.

       DISPLAY " ".
       DISPLAY "Telefono del usuario: " TELEFONO.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-TELEFONO.

       DISPLAY " ".
       DISPLAY "Tipo de instrumento: " TIPO-INSTRUMENTO.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-TIPO.

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

       CONFIRMAR-NOMBRE.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE NOMBRE-USUARIO TO WS-NOMBRE-USUARIO-1
           DISPLAY " "
           DISPLAY "Ingrese nuevo nombre de usuario"
           ACCEPT NOMBRE-USUARIO
           MOVE NOMBRE-USUARIO TO WS-NOMBRE-USUARIO-2
           NEXT SENTENCE
       WHEN 2
           MOVE NOMBRE-USUARIO TO WS-NOMBRE-USUARIO-1
           MOVE NOMBRE-USUARIO TO WS-NOMBRE-USUARIO-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-NOMBRE
       END-EVALUATE.

       CONFIRMAR-SEXO.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE SEXO TO WS-SEXO-1
           DISPLAY " "
           DISPLAY "Ingrese nuevo sexo del usuario "
           DISPLAY "(femenino o masculino)"
           ACCEPT SEXO
           MOVE SEXO TO WS-SEXO-2
           NEXT SENTENCE
       WHEN 2
           MOVE SEXO TO WS-SEXO-1
           MOVE SEXO TO WS-SEXO-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-SEXO
       END-EVALUATE.

       CONFIRMAR-DIRECCION.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE DIRECCION TO WS-DIRECCION-1
           DISPLAY " "
           DISPLAY "Ingrese nueva direccion del usuario"
           ACCEPT DIRECCION
           MOVE DIRECCION TO WS-DIRECCION-2
           NEXT SENTENCE
       WHEN 2
           MOVE DIRECCION TO WS-DIRECCION-1
           MOVE DIRECCION TO WS-DIRECCION-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-DIRECCION
       END-EVALUATE.

       CONFIRMAR-TELEFONO.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE TELEFONO TO WS-TELEFONO-1
           DISPLAY " "
           DISPLAY "Ingrese nuevo telefono del usuario"
           ACCEPT TELEFONO
           MOVE TELEFONO TO WS-TELEFONO-2
           NEXT SENTENCE
       WHEN 2
           MOVE TELEFONO TO WS-TELEFONO-1
           MOVE TELEFONO TO WS-TELEFONO-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-TELEFONO
       END-EVALUATE.

       CONFIRMAR-TIPO.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE TIPO-INSTRUMENTO TO WS-TIPO-INSTRUMENTO-1
           DISPLAY " "
           DISPLAY "Ingrese nuevo nombre de tipo de instrumento"
           ACCEPT TIPO-INSTRUMENTO
           MOVE TIPO-INSTRUMENTO TO WS-TIPO-INSTRUMENTO-2
           NEXT SENTENCE
       WHEN 2
           MOVE TIPO-INSTRUMENTO TO WS-TIPO-INSTRUMENTO-1
           MOVE TIPO-INSTRUMENTO TO WS-TIPO-INSTRUMENTO-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-TIPO
       END-EVALUATE.

       FINAL-EDITAR-CAMPOS.
       *>VERIFICAR SI LA LLAVE INGRESADA YA EXISTE
       READ USUARIOS-ARCHIVO RECORD
           KEY CEDULA-USUARIO
               INVALID KEY     MOVE 0 TO WS-INDICADOR *>NO SE ENCONTRÓ LA LLAVE.
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR. *>SI SE ENCONTRÓ LA LLAVE.
       *>VERIFICAR LLAVE INICIAL Y FINAL.
       IF WS-CEDULA-USUARIO-1 = WS-CEDULA-USUARIO-2 *>SOLO SE REESCRIBE
           MOVE 2 TO WS-INDICADOR.
       *>VERIFICAR INDICADOR.
       EVALUATE WS-INDICADOR
       WHEN 0 *>SI NO EXISTE, ELIMINO ANTERIOR Y SE ESCRIBE.
           MOVE WS-USUARIOS-REGISTRO-1
               TO USUARIOS-REGISTRO
           DELETE USUARIOS-ARCHIVO RECORD *>ELIMINO LLAVE ANTERIOR.
           WRITE USUARIOS-REGISTRO *>ESCRIBO NUEVA LLAVE.
               FROM WS-USUARIOS-REGISTRO-2
       WHEN 1 *>SI YA EXISTE EN LA BASE DE DATOS, INGRESAR OTRA CEDULA
           DISPLAY " "
           DISPLAY "El numero de cedula ingresado ya existe"
           DISPLAY " "
           DISPLAY "¿Que desea hacer?"
           DISPLAY "1.- Ingresar otro numero de cedula"
           DISPLAY "2.- Actualizar otro usuario"
           DISPLAY "3.- Volver al menu"
           DISPLAY "4.- Salir"
           DISPLAY "Ingrese numero de opcion deseada:"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-CODIGO-IGUAL
       WHEN 2 *>SI LA LLAVE ES LA MISMA, SOLO SE REESCRIBE
           REWRITE USUARIOS-REGISTRO *>REESCRIBO LA LLAVE.
               FROM WS-USUARIOS-REGISTRO-2
       END-EVALUATE.

       DISPLAY " ".
       DISPLAY "Campo Actualizado correctamente.".

       CLOSE USUARIOS-ARCHIVO.
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
           CLOSE USUARIOS-ARCHIVO
           PERFORM ACTUALIZAR
       WHEN 3
           CLOSE USUARIOS-ARCHIVO
           PERFORM PROGRAM-BEGIN
       WHEN 4
           CLOSE USUARIOS-ARCHIVO
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-CODIGO-IGUAL
       END-EVALUATE.


       EXIT PROGRAM.
