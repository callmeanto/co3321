# Laboratorio 3
# Alumnos: Arturo Yepez 15-11551, Antonella Requena 15-11196

### PREGUNTA 1

# Como nos piden establacer una relación entre nivel socioeconomico y la zona donde vive
# utilizamos Tablas de Contingencia

# Número de columnas
c = 2

# Número de categorias
r = 3

# Datos de los ambientes que tenemos
rural = c(249, 80, 2)
urbano = c(139, 20, 15)

# Agrupación de esos datos
matriz = cbind(rural, urbano)

# Hacemos chisq.test para sacar los resultados de la tabla de contingencia
chisq.test(matriz)

### PREGUNTA 2

# Debemos realizar una prueba para proporciones

# Nivel de significancia
alpha = 0.05

# Cantidad total de datos
n = 1000

# Número de categorias

k = 4
r = 0

# Probabilidad con la que queremos usar H0
p = c(1/4, 1/4, 1/4, 1/4)

# Datos proporcionados según la categoria
fi = c(294, 276, 238, 192)

# Hacemos prop.test para verificar el resultado
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
