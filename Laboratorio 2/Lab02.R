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

### PREGUNTA 4


### PREGUNTA 5

# Tomamos carne3 como la lista de precios de la Zona 3 y sacamos su longitud
len_carne3 = length(carne3)

# Calculamos una lista con los precios de la Zona 3 mayores a 300 y sacamos su longitud
max_carne3 = carne3 [carne3 >= 300]
len_max_carne3 = length(max_carne3)

# Ahora, calculamos el intervalo de confianza de para proporciones
int_conf3_propor = binom.test(len_max_carne3, len_carne3, conf.level = 0.99)
