xg <- data.table::fread('xg_hbl.csv')
data[,.N,posicion_tiro]


partidos <- data.table::fread('partidos_fase1.csv')
partidos <- partidos[,.(gol, posicion_tiro)]
partidos[is.na(gol), gol := 0]
tiros <- partidos[posicion_tiro != '']

esperados <- tiros[, .(xg = sum(gol)/.N), .(posicion_tiro)]
data

data[esperados, xg := i.xg, on = 'posicion_tiro']
data[is.na(gol), gol := 0]
pos <- data[!is.na(xg), .(xg, posesion, numero_posesion, posicion_tiro, gol, equipo)]

pos[, cantidad_tiros := .N, numero_posesion]
pos[cantidad_tiros > 1]


pos[, xg_ajustado := 1 - prod(1-xg), numero_posesion]
pos[, gol_en_posesion := any(gol), numero_posesion]

xg_final <- pos[, head(.SD, 1), numero_posesion]
xg_final[, sum(xg_ajustado), equipo]


data[,sum(gol), equipo]


# Hicieron tiro en esta cantidad de posesiones

data[, .(hubo_tiro = any(posicion_tiro != '')), .(numero_posesion, posesion)
     ][, .(cantidad_posesiones_con_tiro = sum(hubo_tiro)), posesion]

# Agregar cuántas se debieron a pérdidas o faltas técnicas.

data[, data.table::uniqueN(numero_posesion), posesion]


# Contar Turonvers y technicals

data[stringr::str_detect(accion, 'Technical'),.N,posesion]

data[, sum(!is.na(turnover)), posesion]

# En este caso hubo una posesion por cada equipo, donde se recuperó la bola tras rechazo, y se hizo un tiro malo.
# por eso al sumar las 3 cosas, nos da 57 posesiones por equipo.




