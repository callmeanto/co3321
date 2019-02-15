mercado = read.table("Mercado.txt", header = T)
# Se extraen las secciones de interes de la tabla
genero = as.vector(mercado$Sexo)
carnico = as.vector(mercado$Carnico)
edad = as.vector(mercado$Edad)
fruta = as.vector(mercado$Fruta)
consumo = as.vector(mercado$Consumo)

# PREGUNTA 2

# Se filtran los datos según lo requerido (carnico y cambur como fruta)
F_consumo_carne = consumo [carnico == "Carne" & genero == "F" & fruta == "Cambur"]
# Vemos como se comportan los datos a nivel numerico
summary(F_consumo_carne)

# Se filtran los datos según lo requerido (carnico y cambur como fruta)
F_consumo_cerdo = consumo [carnico == "Cerdo" & genero == "F" & fruta == "Cambur"]
# Vemos como se comportan los datos a nivel numerico
summary(F_consumo_cerdo)

# Se filtran los datos según lo requerido (carnico y cambur como fruta)
F_consumo_pescado = consumo [carnico == "Pescado" & genero == "F" & fruta == "Cambur"]
# Vemos como se comportan los datos a nivel numerico
summary(F_consumo_pescado)

# Se filtran los datos según lo requerido (carnico y cambur como fruta)
F_consumo_pollo = consumo [carnico == "Pollo" & genero == "F" & fruta == "Cambur"]
# Vemos como se comportan los datos a nivel numerico
summary(F_consumo_pollo)

# Hacemos un boxplot triple para ver mejor como se comportan los datos
boxplot(F_consumo_carne, F_consumo_cerdo, F_consumo_pollo,
        
        main = "Dinero gastado por mujeres que compraron cambur según carnico",
        ylab = "Consumo",
        xlab = "Carnico",
        
        names = c("Carne", "Cerdo", "Pollo"),
        col = c("red", "blue", "green"))

# PREGUNTA 3

# Se filtran las variables para que quede una lista dado las comparaciones dadas
consumo_carne = carnico [genero == "M" & carnico == "Carne" & edad > 60]

# Se calcula la longitud del vector, que coincide con la cantidad buscada
cant_consumo_carne = length(consumo_carne)
