PASOS PARA JUGAR

Para jugar debera tener 3 terminales abiertas.

SERVIDOR
En la primer terminal se debera correr 'erl -sname server'
Luego debemos compilar el archivo server con 'c(server).'
Ahora tocara llamar la funcion 'server:start_server().'
A partir de este momento ya podemos registrar nuevos jugadores
Habra que copiar el nombre del servidor que se nos muestra
Ejemplo nombre de servidor: 'player@DESKTOP-VEHNDPN'

JUGADOR
En la segunda y tercera terminal se debera correr 'erl -sname player#'
Al haber asignado al jugador 1 y 2, habra que compilar 'c(player).'
Luego tenemos que llamar la funcion 'player:init('nombre_del_servidor').'
Al registrarse ambos jugadores, se podra llamar la funcion 'player:move('nombre_del_servidor').'
Esta pedira una posicion X, Y, y el simbolo que se pondra
Al haber puesto todo esto se vera el movimiento realizado y el servidor registrara movimientos
El servidor tambien registrara la puntuacion

