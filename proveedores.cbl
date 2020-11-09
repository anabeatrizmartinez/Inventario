       IDENTIFICATION DIVISION.
       PROGRAM-ID. Proveedores.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       SELECT OPTIONAL PROVEEDORES-ARCHIVO
       ASSIGN TO "c:\proveedores.dat"
       ORGANIZATION IS INDEXED
       RECORD KEY IS ID-PROVEEDOR
       ACCESS MODE IS DYNAMIC.

       DATA DIVISION.
       FILE SECTION.
       FD PROVEEDORES-ARCHIVO.
           01 PROVEEDORES-REGISTRO.
               05 ID-PROVEEDOR     PIC X(11).
               05 NOMBRE-PROVEEDOR PIC X(40).
               05 TIPO-PROVEEDOR   PIC X(8).
               05 DIRECCION        PIC X(100).
               05 TELEFONO         PIC 9(11).

       WORKING-STORAGE SECTION.
       *>VARIABLES USADAS
       01 WS-TITULO.
           05 FILLER   PIC X(34) VALUE SPACES.
           05 WS-TIT   PIC X(11) VALUE "Proveedores".
           05 FILLER   PIC X(35) VALUE SPACES.

       01 WS-PROVEEDORES-REGISTRO-1.
               05 WS-ID-PROVEEDOR-1     PIC X(11).
               05 WS-NOMBRE-PROVEEDOR-1 PIC X(40).
               05 WS-TIPO-PROVEEDOR-1   PIC X(8).
               05 WS-DIRECCION-1        PIC X(100).
               05 WS-TELEFONO-1         PIC 9(11).

       01 WS-PROVEEDORES-REGISTRO-2.
               05 WS-ID-PROVEEDOR-2     PIC X(11).
               05 WS-NOMBRE-PROVEEDOR-2 PIC X(40).
               05 WS-TIPO-PROVEEDOR-2   PIC X(8).
               05 WS-DIRECCION-2        PIC X(100).
               05 WS-TELEFONO-2         PIC 9(11).

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
       DISPLAY "---Registro de nuevo proveedor---".
       DISPLAY " ".
       DISPLAY "Indique numero de cedula o RIF del proveedor".
       ACCEPT ID-PROVEEDOR.
       *>VERIFICAR SI EL ID YA EXISTE EN LA BASE DE DATOS
       OPEN I-O PROVEEDORES-ARCHIVO.
       READ PROVEEDORES-ARCHIVO RECORD
           KEY ID-PROVEEDOR
               INVALID KEY     MOVE 0 TO WS-INDICADOR
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR.
       IF WS-INDICADOR = 0 *>SI NO EXISTE, GUARDO EL NUEVO REGISTRO
           DISPLAY " "
           DISPLAY "Ingrese nombre del proveedor"
           ACCEPT NOMBRE-PROVEEDOR
           DISPLAY " "
           DISPLAY "Ingrese tipo de proveedor (vendedor o donante)"
           ACCEPT TIPO-PROVEEDOR
           DISPLAY " "
           DISPLAY "Ingrese direccion del proveedor"
           ACCEPT DIRECCION
           DISPLAY " "
           DISPLAY "Ingrese telefono del proveedor"
           ACCEPT TELEFONO
           WRITE PROVEEDORES-REGISTRO
           DISPLAY " "
           DISPLAY "Proveedor registrado correctamente."
       ELSE *>SI EXISTE, SALIR
           DISPLAY " "
           DISPLAY "El proveedor ya existe.".

       CLOSE PROVEEDORES-ARCHIVO.
       PERFORM VOLVER-REGISTRAR.

       VOLVER-REGISTRAR.
       DISPLAY " ".
       DISPLAY "¿Que desea hacer?".
       DISPLAY "1.- Registrar otro proveedor".
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
       DISPLAY "---Actualizar proveedores---".
       DISPLAY " ".
       DISPLAY "Indique numero de cedula o RIF del proveedor "
       DISPLAY "que desea actualizar".
       ACCEPT ID-PROVEEDOR.
       *>VERIFICAR SI EL CODIGO YA EXISTE EN LA BASE DE DATOS
       OPEN I-O PROVEEDORES-ARCHIVO.
       READ PROVEEDORES-ARCHIVO RECORD
           KEY ID-PROVEEDOR
               INVALID KEY     MOVE 0 TO WS-INDICADOR
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR.
       IF WS-INDICADOR = 0 *>SI NO EXISTE REGRESO AL MENU.
           DISPLAY " "
           DISPLAY "No existe el proveedor ingresado."
           CLOSE PROVEEDORES-ARCHIVO
           PERFORM VOLVER-ACTUALIZAR
       ELSE *>SI EXISTE MUESTRO LOS CAMPOS ANTES DE ACTUALIZAR.
           DISPLAY " "
           DISPLAY "Numero de cedula o RIF del proveedor: " ID-PROVEEDOR
           DISPLAY "Nombre de proveedor: " NOMBRE-PROVEEDOR
           DISPLAY "Tipo de proveedor: " TIPO-PROVEEDOR
           DISPLAY "Direccion del proveedor: " DIRECCION
           DISPLAY "Telefono del proveedor: " TELEFONO
           PERFORM CONFIRMAR-ACTUALIZAR.

       VOLVER-ACTUALIZAR.
       DISPLAY " ".
       DISPLAY "¿Que desea hacer?".
       DISPLAY "1.- Actualizar otro proveedor".
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
       DISPLAY "¿Que desea hacer con este proveedor?".
       DISPLAY "1.- Editar campos".
       DISPLAY "2.- Eliminar proveedor".
       DISPLAY "3.- Actualizar otro proveedor"
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
           CLOSE PROVEEDORES-ARCHIVO
           PERFORM ACTUALIZAR
       WHEN 4
           CLOSE PROVEEDORES-ARCHIVO
           PERFORM PROGRAM-BEGIN
       WHEN 5
           CLOSE PROVEEDORES-ARCHIVO
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-CONFIRMAR-ACTUALIZAR
       END-EVALUATE.

       ELIMINAR. *>ELIMINAR EL PROVEEDOR
       DISPLAY "¿Esta seguro de eliminar el "
       DISPLAY "PROVEEDOR " ID-PROVEEDOR "? (S/N)".
       ACCEPT WS-SI-NO.
       PERFORM VALIDACION-ELIMINAR.

       VALIDACION-ELIMINAR.
       EVALUATE WS-SI-NO
       WHEN = "S" OR = "s"
           DELETE PROVEEDORES-ARCHIVO RECORD
           DISPLAY " "
           DISPLAY "Proveedor eliminado."
           CLOSE PROVEEDORES-ARCHIVO
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
       DISPLAY "Numero de cedula o RIF del proveedor: " ID-PROVEEDOR.
       PERFORM CONFIRMAR. *>VERIFICAR SI SE QUIERE EDITAR ESTE CAMPO O NO.
       PERFORM CONFIRMAR-ID.

       DISPLAY " ".
       DISPLAY "Nombre de proveedor: " NOMBRE-PROVEEDOR.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-NOMBRE.

       DISPLAY " ".
       DISPLAY "Tipo de proveedor: " TIPO-PROVEEDOR.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-TIPO.

       DISPLAY " ".
       DISPLAY "Direccion del proveedor: " DIRECCION.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-DIRECCION.

       DISPLAY " ".
       DISPLAY "Telefono del proveedor: " TELEFONO.
       PERFORM CONFIRMAR.
       PERFORM CONFIRMAR-TELEFONO.

       PERFORM FINAL-EDITAR-CAMPOS.

       CONFIRMAR.
       DISPLAY "1.- Editar".
       DISPLAY "2.- Siguiente campo".
       DISPLAY "Ingrese numero de opcion deseada:".
       ACCEPT WS-OPCION.

       CONFIRMAR-ID.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE ID-PROVEEDOR TO WS-ID-PROVEEDOR-1 *>GUARDAR EL REGISTRO INICIAL.
           DISPLAY " "
           DISPLAY "Ingrese nuevo numero de cedula o RIF"
           ACCEPT ID-PROVEEDOR
           MOVE ID-PROVEEDOR TO WS-ID-PROVEEDOR-2 *>GUARDAR EL REGISTRO NUEVO.
           NEXT SENTENCE
       WHEN 2
           MOVE ID-PROVEEDOR TO WS-ID-PROVEEDOR-1
           MOVE ID-PROVEEDOR TO WS-ID-PROVEEDOR-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-ID
       END-EVALUATE.

       CONFIRMAR-NOMBRE.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE NOMBRE-PROVEEDOR TO WS-NOMBRE-PROVEEDOR-1
           DISPLAY " "
           DISPLAY "Ingrese nuevo nombre de proveedor"
           ACCEPT NOMBRE-PROVEEDOR
           MOVE NOMBRE-PROVEEDOR TO WS-NOMBRE-PROVEEDOR-2
           NEXT SENTENCE
       WHEN 2
           MOVE NOMBRE-PROVEEDOR TO WS-NOMBRE-PROVEEDOR-1
           MOVE NOMBRE-PROVEEDOR TO WS-NOMBRE-PROVEEDOR-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-NOMBRE
       END-EVALUATE.

       CONFIRMAR-TIPO.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE TIPO-PROVEEDOR TO WS-TIPO-PROVEEDOR-1
           DISPLAY " "
           DISPLAY "Ingrese nuevo tipo de proveedor "
           DISPLAY "(vendedor o donante)"
           ACCEPT TIPO-PROVEEDOR
           MOVE TIPO-PROVEEDOR TO WS-TIPO-PROVEEDOR-2
           NEXT SENTENCE
       WHEN 2
           MOVE TIPO-PROVEEDOR TO WS-TIPO-PROVEEDOR-1
           MOVE TIPO-PROVEEDOR TO WS-TIPO-PROVEEDOR-2
           NEXT SENTENCE
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM CONFIRMAR-TIPO
       END-EVALUATE.

       CONFIRMAR-DIRECCION.
       EVALUATE WS-OPCION
       WHEN 1
           MOVE DIRECCION TO WS-DIRECCION-1
           DISPLAY " "
           DISPLAY "Ingrese nueva direccion del proveedor"
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
           DISPLAY "Ingrese nuevo telefono del proveedor"
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

       FINAL-EDITAR-CAMPOS.
       *>VERIFICAR SI LA LLAVE INGRESADA YA EXISTE
       READ PROVEEDORES-ARCHIVO RECORD
           KEY ID-PROVEEDOR
               INVALID KEY     MOVE 0 TO WS-INDICADOR *>NO SE ENCONTRÓ LA LLAVE.
               NOT INVALID KEY MOVE 1 TO WS-INDICADOR. *>SI SE ENCONTRÓ LA LLAVE.
       *>VERIFICAR LLAVE INICIAL Y FINAL.
       IF WS-ID-PROVEEDOR-1 = WS-ID-PROVEEDOR-2 *>SOLO SE REESCRIBE
           MOVE 2 TO WS-INDICADOR.
       *>VERIFICAR INDICADOR.
       EVALUATE WS-INDICADOR
       WHEN 0 *>SI NO EXISTE, ELIMINO ANTERIOR Y SE ESCRIBE.
           MOVE WS-PROVEEDORES-REGISTRO-1
               TO PROVEEDORES-REGISTRO
           DELETE PROVEEDORES-ARCHIVO RECORD *>ELIMINO LLAVE ANTERIOR.
           WRITE PROVEEDORES-REGISTRO *>ESCRIBO NUEVA LLAVE.
               FROM WS-PROVEEDORES-REGISTRO-2
       WHEN 1 *>SI YA EXISTE EN LA BASE DE DATOS, INGRESAR OTRO CODIGO
           DISPLAY " "
           DISPLAY "El numero de cedula o RIF ingresado ya existe "
           DISPLAY " "
           DISPLAY "¿Que desea hacer?"
           DISPLAY "1.- Ingresar otro numero de cedula o RIF"
           DISPLAY "2.- Actualizar otro proveedor"
           DISPLAY "3.- Volver al menu"
           DISPLAY "4.- Salir"
           DISPLAY "Ingrese numero de opcion deseada:"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-CODIGO-IGUAL
       WHEN 2 *>SI LA LLAVE ES LA MISMA, SOLO SE REESCRIBE
           REWRITE PROVEEDORES-REGISTRO *>REESCRIBO LA LLAVE.
               FROM WS-PROVEEDORES-REGISTRO-2
       END-EVALUATE.

       DISPLAY " ".
       DISPLAY "Campo Actualizado correctamente.".

       CLOSE PROVEEDORES-ARCHIVO.
       PERFORM VOLVER-ACTUALIZAR.

       VALIDACION-CODIGO-IGUAL.
       EVALUATE WS-OPCION
       WHEN 1
           DISPLAY " "
           DISPLAY "Por favor ingrese otro numero de cedula o RIF"
           ACCEPT ID-PROVEEDOR
           MOVE ID-PROVEEDOR TO WS-ID-PROVEEDOR-2
           PERFORM FINAL-EDITAR-CAMPOS
       WHEN 2
           CLOSE PROVEEDORES-ARCHIVO
           PERFORM ACTUALIZAR
       WHEN 3
           CLOSE PROVEEDORES-ARCHIVO
           PERFORM PROGRAM-BEGIN
       WHEN 4
           CLOSE PROVEEDORES-ARCHIVO
           STOP RUN
       WHEN OTHER
           DISPLAY "Por favor ingrese una opcion valida"
           ACCEPT WS-OPCION
           PERFORM VALIDACION-CODIGO-IGUAL
       END-EVALUATE.


       EXIT PROGRAM.
