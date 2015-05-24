##-------------------------------------------------##
##------Modelos Lineales y Diseño de Expermientos -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre: Mateo Larco Álvarez


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.

data <- read.table("data.txt", header = TRUE, dec=",", sep="\t")
str(data)
names(data)

# 2.2 Calcular el mínimo, la media, el máximo de la variable Edad.

edad <- data[,1]
edad
min(edad,na.rm=TRUE)
mean(edad,na.rm=TRUE)
max(edad,na.rm=TRUE)

# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.

data_g <- subset(data,subset=data[,"Genero"]=="Femenino")
table(data_g[,"Genero"])

# 2.4 Encontrar la Edad mínima, media, máxima de los sujetos que Si son dependientes.

data_dependiente<-subset(data, subset=data[,"Dependiente"]=="Si")
table(data_dependiente[,"Dependiente"])
edad_dep<-data_dependiente[,"Edad"]
minimo_dep<-min(edad_dep,na.rm =TRUE )
maximo_dep<-max(edad_dep, na.rm =TRUE)
media_dep<-mean(edad_dep,na.rm = TRUE)
minimo_dep
maximo_dep
media_dep
# 2.5 Identificar el tipo de elementos que contiene cada variable.

tipos <- numeric(ncol(data))
for (i in 1:ncol(data)){
  tipos[i] <- typeof(data[,i])
}
tipos


# 2.6 Identificar la clase de cada variable (columna).

clase <- numeric(ncol(data))
for (i in 1:ncol(data)){
  clase[i] <- class(data[,i])
}
clase
# 2.7 Calcular la media de todas las variables numéricas (double, integer).

media <- numeric(ncol(data))
for (i in 1:ncol(data)){
  media[i] <- mean(data[,i],na.rm = TRUE)
}
media

# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.

vacios <- numeric(ncol(data))
for (i in 1:ncol(data)){
  vacios[i] <- sum(is.na(data[,i]))
}
vacios
prop.table(vacios)

# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.

data_e <- subset(data,subset=data[,"Edad"]>40)
table(data_e[,"Edad"])

# 3.2 Seleccione los sujetos que tienen Vivienda Propia.

data_v <- subset(data,subset=data[,"Vivienda"]=="Propia")
table(data_v[,"Vivienda"])

# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiatres.

data_c <- subset(data,subset=data[,"Cargas"]>2)
table(data_c[,"Cargas"])

# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.

data_da <- subset(data,(subset=data[,"Deuda"]>=500)&(subset=data[,"Dias_Atraso"]>8))
table(data_da[,"Deuda"],data_da[,"Dias_Atraso"])

# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC)
.
data_setc <- subset(data,(subset=data[,"Score"]>=900)&(subset=data[,"Edad"]<=35)&(subset=data[,"Numero_TC"]>3))
table(data_setc[,"Score"],data_setc[,"Edad"],data_setc[,"Numero_TC"])

# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red

hist(edad,col = "red",main="Edad")

# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green

boxplot(edad,col = "green",main="Edad")
