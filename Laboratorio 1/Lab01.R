# Laboratorio 1
# Alumnos: Arturo Yepez 15-11551, Antonella Requena 15-11196

# LECTURA DE DATOS
# Lectura de Tabla
mercado = read.table("Mercado.txt", header = T)

# Se extraen las secciones de interes de la tabla
genero = as.vector(mercado$Sexo)
carnico = as.vector(mercado$Carnico)
edad = as.vector(mercado$Edad)
fruta = as.vector(mercado$Fruta)
consumo = as.vector(mercado$Consumo)
vegetales = as.vector(mercado$Vegetales)

# PREGUNTA 1

# Filtramos las variables de vegetales
consumo_cebolla = consumo [vegetales == "Cebolla"]
consumo_papa = consumo [vegetales == "Papa"]
consumo_lechuga = consumo [vegetales == "Lechuga"]
consumo_tomate = consumo [vegetales == "Tomate"]

# CALCULO DE LA MODA

# Funcion para calcular la moda dado un vector
moda <- function(datos){
  tab <- table(datos)
  return( as.numeric( names(tab)[tab == max(tab)] ) )
}

# Calculamos la moda de cada verdura
moda_cebolla = moda(consumo_cebolla)
moda_papa = moda(consumo_papas)
moda_lechuga = moda(consumo_lechuga)
moda_tomate = moda(consumo_tomate)

# Determinamos la varianza
var_cebolla = var(consumo_cebolla)
var_papa = var(consumo_papa)
var_lechuga = var(consumo_lechuga)
var_tomate = var(consumo_tomate)

# Determinamos la desviacion estandar
sd_cebolla = sd(consumo_cebolla)
sd_papa = sd(consumo_papa)
sd_lechuga = sd(consumo_lechuga)
sd_tomate = sd(consumo_tomate)

# Usamos la funcion summary para obtener los datos para el analisis descriptivo

summary_cebolla = summary(consumo_cebolla)
summary_papa = summary(consumo_papa)
summary_lechuga = summary(consumo_lechuga)
summary_tomate = summary(consumo_tomate)

# Graficamos con graficos de caja

boxplot(consumo_cebolla, consumo_papa, consumo_lechuga, consumo_tomate,
        
        main = "Vegetales según Consumo",
        ylab = "Consumo",
        xlab = "Vegetales",
        
        names = c("Cebolla", "Papa", "Lechuga", "Tomate"),
        col = c("white", "yellow", "green", "red"))


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
