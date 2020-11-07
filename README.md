# Inventario
Programa en COBOL para la asignatura Procesamiento de Datos.

Es un sistema de inventario para un centro deportivo, dicho inventario debe cumplir las siguientes funciones:

* Registro y actualización del inventario de todos los instrumentos deportivos que posee el centro.
*	Registro y actualización de los datos referentes a los instrumentos prestados. 
*	Registro y actualización de los usuarios de los instrumentos. 
*	Registro y actualización de los proveedores de los instrumentos, sean vendedores o donantes.
*	Consultas:
	* Lista de instrumentos deportivos por tipo de instrumento. Ej. Cantidad de balones de football.
	* Lista de instrumentos deportivos por status. Ej. Cantidad de raquetas extraviadas. 
	* Usuarios de un tipo de instrumento. (Ej. Listado de todos los usuarios de balones de basketball).
	* Lista de donantes de instrumentos.

Para la codificación de los instrumentos, se construye el código utilizando como prefijo el CODIGO DE TIPO, seguido de un número correlativo. Por ejemplo:
Para las pelotas de baseball se tendría:
PBB001, PBB002,…..
Para los balones de football se tendría:
BFB001, BFB002,…..

Programa aún en proceso.