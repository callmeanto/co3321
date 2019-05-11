# Laboratorio numero 1

# Cargamos los datos y los almacenamos en una variable
table = read.table("desayuno.txt",header = T)

# Parte 1: Cantidad de mujeres y hombres

cant_m = length(table$sexo [table$sexo=='mujer'])
cant_h = length(table$sexo [table$sexo=='hombre'])

# Parte 2: Promedio de consumo general y por genero y desviacion estandar

# general
gasto = table$consumo
prom_g = mean(gasto)

# por genero
cons_h = gasto [ table$sexo == 'hombre' ]
cons_m = gasto [ table$sexo == 'mujer' ]

prom_h = mean(cons_h)
prom_m = mean(cons_m)

# desviacion estandar

de_h = sd(cons_h,na.rm=FALSE)
de_m = sd(cons_m,na.rm=FALSE)

# Parte 4: cantidad de eleccion de cafe por genero
cafe_m = length(table$Bebida [table$sexo=='mujer' & table$Bebida == 'cafe'])
cafe_h = length(table$Bebida [table$sexo=='hombre' & table$Bebida == 'cafe'])

# Parte 5: cantidad de eleccion de cafe y arepa por genero

cafeArepa_m = length(table$sexo [table$sexo=='mujer' & table$Bebida == 'cafe' & table$Consume_empanada == FALSE])
cafeArepa_h = length(table$sexo [table$sexo=='hombre' & table$Bebida == 'cafe' & table$Consume_empanada == FALSE])
