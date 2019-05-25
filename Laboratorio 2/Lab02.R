# Laboratorio 2
# Alumnos: Arturo Yepez 15-11551, Antonella Requena 15-11196

# LECTURA DE DATOS
# Lectura de Tabla
mercado = read.table("zonas_carne.txt", header = T)

# Se crea la función de intervalos de confianza
intervalo <- function(x, alfa) {
  n <- length(x)
  nu <- n - 1 #grados de libertad
  cuantil <- qt( 1 - alfa/2, df = nu )
  LS <- mean( x ) + cuantil*sqrt( var( x ) / n )
  LI <- mean( x ) - cuantil*sqrt( var( x ) / n )
  return( c( LI, mean( x ), LS ) )
}


### PREGUNTA 1

# Datos a extraer

# Extraemos vector con los precios solos de la carne de guisar de las zonas

# Zona 1
carne1 = as.vector(mercado$Zona1)
tipo_carne1 = as.vector(mercado$Tipo1)

carne_guisar1 = carne1 [tipo_carne1 == "Guisar"]

# Zona 2
carne2 = as.vector(mercado$Zona2)
tipo_carne2 = as.vector(mercado$Tipo2)

carne_guisar2 = carne2 [tipo_carne2 == "Guisar"]

# Zona 3
carne3 = as.vector(mercado$Zona3)
tipo_carne3 = as.vector(mercado$Tipo3)

carne_guisar3 = carne3 [tipo_carne3 == "Guisar"]

# Zona 4
carne4 = as.vector(mercado$Zona4)
tipo_carne4 = as.vector(mercado$Tipo4)

carne_guisar4 = carne4 [tipo_carne4 == "Guisar"]


# Histogramas
hist(carne_guisar1, main = "Precios de carne de guisar en Zona 1", ylab = "Abastecimientos", xlab = "Precios")
hist(carne_guisar2, main = "Precios de carne de guisar en Zona 2", ylab = "Abastecimientos", xlab = "Precios")
hist(carne_guisar3, main = "Precios de carne de guisar en Zona 3", ylab = "Abastecimientos", xlab = "Precios")
hist(carne_guisar4, main = "Precios de carne de guisar en Zona 4", ylab = "Abastecimientos", xlab = "Precios")

# Boxplots
boxplot(carne_guisar1, ylab="Precios", xlab="Zona 1", col=c("blue"))
boxplot(carne_guisar2, ylab="Precios", xlab="Zona 2", col=c("red"))
boxplot(carne_guisar3, ylab="Precios", xlab="Zona 3", col=c("yellow"))
boxplot(carne_guisar4, ylab="Precios", xlab="Zona 4", col=c("green"))

# Tabla

M = data.frame(stringsAsFactors = FALSE, Minimos =c(min(carne_guisar1),min(carne_guisar2),min(carne_guisar3),min(carne_guisar4)), Q1 =c(quantile(carne_guisar1,0.25),quantile(carne_guisar2,0.25),quantile(carne_guisar3,0.25),quantile(carne_guisar4,0.25)),
Q2=c(quantile(carne_guisar1,0.50),quantile(carne_guisar2,0.50),quantile(carne_guisar3,0.50),quantile(carne_guisar4,0.50)),Q3=c(quantile(carne_guisar1,0.75),quantile(carne_guisar2,0.75),quantile(carne_guisar3,0.75),quantile(carne_guisar4,0.75)),Media=c(mean(carne_guisar1),mean(carne_guisar2),mean(carne_guisar3),mean(carne_guisar4)),
DesviacionEstandar=c(sd(carne_guisar1),sd(carne_guisar2),sd(carne_guisar3),sd(carne_guisar4)))


### PREGUNTA 2

# Creamos una lista con los precios solos de la carne de guisar de la zona 1
carne1 = as.vector(mercado$Zona1)
tipo_carne1 = as.vector(mercado$Tipo1)

carne_guisar1 = carne1 [tipo_carne1 == "Guisar"]

# Creamos una lista con los precios solos de la carne de guisar de la zona 3
carne3 = as.vector(mercado$Zona3)
tipo_carne3 = as.vector(mercado$Tipo3)

carne_guisar3 = carne3 [tipo_carne3 == "Guisar"]

# Determinamos el intervalo de Confianza de la Zona 1
int_conf1 = intervalo(carne_guisar1, 0.01)

# Determinamos el intervalo de Confianza de la Zona 3
int_conf3 = intervalo(carne_guisar3, 0.01)


### PREGUNTA 3

# Creamos una lista con los precios solos de la carne de guisar de la zona 2 y xona 4
carne2 = as.vector(mercado$Zona2)
carne4 = as.vector(mercado$Zona4)

# Filtramos la columna con los tipos
tipo_carne2 = as.vector(mercado$Tipo2)
tipo_carne4 = as.vector(mercado$Tipo4)

# Filtramos los vectores de carne de zona 2 y 4 por Tipo Guisar
carne_guisar2 = carne2 [tipo_carne2 == "Guisar"]
carne_guisar4 = carne4 [tipo_carne4 == "Guisar"]


# Verificamos que podamos asumir que las varianzas sean iguales (i.e. que 1 este en el intervalo)
var.test(carne_guisar2, carne_guisar4,conf.level = 0.99)

# Como el resultado es, que 1 esta en el intervalo, podemos decir que las varianzas son iguales con un
# nivel de confianza de 99%

# Ahora calculamos el intervalo de confianza de las medias
t.test (carne_guisar2, carne_guisar4, var.equal = T, conf.level = 0.99,alternative = "greater" )


### PREGUNTA 4

# Creamos una lista con los precios de carne de la Zona 2
carne2 = as.vector(mercado$Zona2)

# Creamos una lista con los precios de carne de la Zona 4
carne4 = as.vector(mercado$Zona4)

# Ahora sacamos el intervalo de confianza 
int_conf_dif = t.test(carne2, carne4)


### PREGUNTA 5

# Tomamos carne3 como la lista de precios de la Zona 3 y sacamos su longitud
len_carne3 = length(carne3)

# Calculamos una lista con los precios de la Zona 3 mayores a 300 y sacamos su longitud
max_carne3 = carne3 [carne3 >= 300]
len_max_carne3 = length(max_carne3)

# Ahora, calculamos el intervalo de confianza de para proporciones
int_conf3_propor = binom.test(len_max_carne3, len_carne3, conf.level = 0.9)


### PREGUNTA 6

# Asumimos que el estadistico de prueba es la media

# Creamos una lista con los precios de la zona 1B (zona 1 ya lo tenemos)
carne1B = as.vector(mercado$Zona3B)

# Utilizamos prueba por hipotesis
# HN: media de carne 1B = media de carne 1
# HA: media de carne 1B > media de carne 1
t.test(carne1B,carne1,alternative = "greater",conf.level = 0.95)

