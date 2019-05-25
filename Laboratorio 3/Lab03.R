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

### PREGUNTA 3

# Como tenemos datos no agrupados, vamos a dividir las clases
# para obtener la frecuencia
n = 50
alpha = 0.05
r = 2

xi = c(32, 23, 64, 31, 74, 44, 61, 33, 66, 73,
       27, 65, 40, 54, 23, 43, 58, 87, 58, 62,
       68, 89, 93, 24, 73, 42, 33, 63, 36, 48,
       77, 75, 37, 59, 70, 61, 43, 68, 54, 29,
       48, 81, 57, 97, 35, 58, 56, 58, 57, 45)

hist(xi, main = "Histograma de Edades", ylab = "Frecuencia", xlab = "Edades", col = "blue")

# Ahora, agrupando los datos
k = 4
fi = c(12,19,14,5)
mi= c(30, 50, 70, 90)
lu = c(40, 60, 80, 100)  # limite superior de cada clase
ll = c(20, 40, 60, 80)   # limite inferior de cada clase


# Hacemos qq-plot
qqnorm(fi)
qqline(fi)

# Determinamos el promedio
prom = sum(fi*mi)/n
      
# Ahora la varianza
prom_n = rep(prom,n)

varianza = sum((xi-prom_n)^2 )/(n-1)

# Desviacion estandar
sd = sqrt(varianza)

# Una vez que tenemos varianza y promedio podemos calcular la probabilidad pi
pi = pnorm(lu, prom, sd) - pnorm (ll, prom, sd)

# Calculamos nuestro estadistico Chi-cuadrado
chi2_est = sum((fi-n*pi)^2/(n*pi))

# Como no se nos da el nivel de significancia alpha, calculamos el p-valor
p-valor = 1 - pchisq(chi2_est, k-1-r)
