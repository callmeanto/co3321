mercado = read.table("Mercado.txt", header = T)
# Se extraen las secciones de interes de la tabla
genero = as.vector(mercado$Sexo)
carnico = as.vector(mercado$Carnico)
edad = as.vector(mercado$Edad)
fruta = as.vector(mercado$Fruta)
consumo = as.vector(mercado$Consumo)

# PREGUNTA 2

F_consumo_carne = consumo [carnico == "Carne" & genero == "F" & fruta == "Cambur"]
summary(F_consumo_carne)

F_consumo_cerdo = consumo [carnico == "Cerdo" & genero == "F" & fruta == "Cambur"]
summary(F_consumo_cerdo)

F_consumo_pescado = consumo [carnico == "Pescado" & genero == "F" & fruta == "Cambur"]
summary(F_consumo_pescado)

F_consumo_pollo = consumo [carnico == "Pollo" & genero == "F" & fruta == "Cambur"]
summary(F_consumo_pollo)

boxplot(F_consumo_carne, F_consumo_cerdo, F_consumo_pollo,
        names = c("Pollo", "Cerdo", "Carne"))

# PREGUNTA 3

# Se filtran las variables para que quede una lista dado las comparaciones dadas
consumo_carne = carnico [genero == "M" & carnico == "Carne" & edad > 60]

# Se calcula la longitud del vector, que coincide con la cantidad buscada
cant_consumo_carne = length(consumo_carne)
