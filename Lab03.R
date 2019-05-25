# Ejercicio 2

# Como tenemos datos no agrupados, calculamos el promedio
n = 50
alpha = 0.05

xi = c(32, 23, 64, 31, 74, 44, 61, 33, 66, 73,
      27, 65, 40, 54, 23, 43, 58, 87, 58, 62,
      68, 89, 93, 24, 73, 42, 33, 63, 36, 48,
      77, 75, 37, 59, 70, 61, 43, 68, 54, 29,
      48, 81, 57, 97, 35, 58, 56, 58, 57, 45)

prom = sum(xi)/n

# Ahora la varianza
prom_n = rep(prom,n)

varianza = sum((xi-prom_n)^2 )/(n-1)

sd = sqrt(varianza)


# Una vez que tenemos varianza y promedio podemos calcular la probabilidad pi
pi = pnorm(xi,prom,varianza)


# Calculamos nuestro estadistico Chi-cuadrado
chi2_est = sum((xi-n*pi)^2/(n*pi))


# Determinamos la region de rechazo
chi2_rr = qchisq(1-alpha, n-1-2)

p_valor = 1 - pchisq(chi2_obs, n-1-2)
