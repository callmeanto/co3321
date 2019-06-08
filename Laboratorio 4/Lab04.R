# Laboratorio 4
# Alumnos: Arturo Yepez 15-11551, Antonella Requena 15-11196

# CARGAR LOS DATOS
# Cargamos el estado
data(state)

# Convertimos el estado en un data frame
edo.77 = as.data.frame(state.x77)

# Cambiamos algunos nombres para evitar problemas
names(edo.77)[4] = "Life.Exp"
names(edo.77)[6] = "Hs.Grad"


### PREGUNTA 1

# Resumenes de los datos (Cuartiles, Medianas, Mínimo, Máximo y Media)
summary( edo.77[1] )

summary( edo.77[2] )

summary( edo.77[3] )

summary( edo.77[4] )

summary( edo.77[5] )

summary( edo.77[6] )

summary( edo.77[7] )

summary( edo.77[8] )

# Desviación Estandar de los datos
sd( edo.77[1] )

sd( edo.77[2] )

sd( edo.77[3] )

sd( edo.77[4] )

sd( edo.77[5] )

sd( edo.77[6] )

sd( edo.77[7] )

sd( edo.77[8] )

# Diagramas de Caja de los 8 datos
boxplot(edo.77[1], 
        
        main = "Populación",
        ylab = "No. de Habitantes",
        
        col = "white")

boxplot(edo.77[2], 
        
        main = "Ingresos",
        ylab = "Renta per Capita",
        
        col = "green")

boxplot(edo.77[3], 
        
        main = "Analfabetismo",
        ylab = "Porcentaje de la Población",
        
        col = "orange")

boxplot(edo.77[4], 
        
        main = "Esperanza de Vida",
        ylab = "Años",
        
        col = "light blue")

boxplot(edo.77[5], 
        
        main = "Asesinato y Homicidio No Negligente",
        ylab = "No. de Personas",
        
        col = "red")

boxplot(edo.77[6], 
        
        main = "Graduados de Escuela Secundaria",
        ylab = "Porcentaje de la Población",
        
        col = "beige")

boxplot(edo.77[7], 
        
        main = "Temperatura por Debajo del Punto de Congelación",
        ylab = "No. de Días",
        
        col = "dark blue")

boxplot(edo.77[8], 
        
        main = "Area de Tierra",
        ylab = "Millas Cuadradas",
        
        col = "brown")


### PREGUNTA 2

graf_matriz_cor = pairs( edo.77, labels = c("Populación", "Ingresos", "Analfabetismo", 
                          "Esperanzas de Vida", "Asesinato", "Graduaos de High School", 
                          "Temperatura", "Area de Tierra") )

matriz_cor = cor(edo.77)


### PREGUNTA 3

model1 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + Hs.Grad 
            + Frost+ Area, data = edo.77)

model2 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + Hs.Grad 
            + Frost, data = edo.77)

model3 = lm(Life.Exp ~ Population + Income + Murder + Hs.Grad 
            + Frost, data = edo.77)

model4 = lm(Life.Exp ~ Income + Murder + Hs.Grad 
            + Frost, data = edo.77)

model5 = lm(Life.Exp ~ Murder + Hs.Grad + Frost, data = edo.77)

### PREGUNTA 4
