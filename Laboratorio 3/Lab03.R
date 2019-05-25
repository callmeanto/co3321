# Laboratorio 3
# Alumnos: Arturo Yepez 15-11551, Antonella Requena 15-11196

### PREGUNTA 1

# Como nos piden establacer una relación entre nivel socioeconomico y la zona donde vive
# utilizamos Tablas de Contingencia

c = 2
r = 3

rural = c(249, 80, 2)
urbano = c(139, 20, 15)

matriz = cbind(rural, urbano)

chisq.test(matriz)

### PREGUNTA 2

# Debemos realizar una prueba para proporciones

alpha = 0.05
n = 1000
k = 4
r = 0

p = c(1/4, 1/4, 1/4, 1/4)

fi = c(294, 276, 238, 192)

prop.test(fi, c(1000,1000,1000,1000), p)

