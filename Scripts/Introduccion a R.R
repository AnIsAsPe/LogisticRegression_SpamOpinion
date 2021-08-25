# INTRODUCCIÓN A R  ##########################################################

# 0. ANTES QUE NADA ###########################################################

# Operador de Asignación y principales tipos de variables

# (Alt + -) es un shortcut para escribir el operador <-

promedio <- 8.3     # numeric

grado <- 2          # numeric

nombre <- 'Mario'   # character

egresado <- FALSE   # logical


# Funciones predefinidas

class(promedio)

(sqrt(4))

round(2.35,1)

args(round)


# 1. VECTORES ================================================================

# Los vectores son la estructura básica en R.
# Un vector, siempre contiene elementos del mismo tipo

var1 <- c (6, 20, 4)

length(var1)  # longitud del vector

class(var1)   # clase de los elementos que contiene el vector

str(var1)     # estructura del objeto y sus elementos

var1 <- c(var1, 15) # agrega elemento al final de un vector

# qué pasa si añado un elemento de otra clase
(c(var1, 'gato'))
var2 <- 2:6

var.3 <- seq (2,3, by=.2)

4var <- rep(1:2, times=3)

var4 <- rep(1:2, times=3)

v5 <- rep(seq(1,5),times=2)

# v5 ¿es un vector?
is.vector(v5) 

#¿Qué tipo tienen los elementos de v5?
typeof(v5)

# ¿Qué pasa con los vectores a los que se les asigna distintos
# tipos de elementos? Véamos un ejemplo:
v6 <- c('a', 1, 'b')   

# v6 ¿es un vector?
is.vector(v6) 

#¿Qué tipo tienen los elementos de v6?
typeof(v6)

# 1.1 Seleccionar elementos de un vector por posición --------------------------

var2[3]       #selecciona el tercer elemento del vector

var2[-3]      #regresa todos los elementos menos el tercero

var2[c(1,5)]  # regresa el primer y quinto elemento


# 1.2 Seleccionar elementos de un vector por valor -----------------------------

var4[var4 == 2]     # regresa todos los elementos de var4 que sean igual a 2

var2[var2 > 4]      # regresa todos los elementos de var2 mayores que 4

v5[v5 %in% c(1,2,4)] # regresa todos los elementos de v5 contenidos 
                     # el vector 1, 2, 4

# 1.3 Funciones de vectores ----------------------------------------------------

#después de escribir el nombre de una función, se puede usar el tabulador para 
#ver la documentación

table(v5) # construye una tabla con el conteo de los elementos según su valor

unique(var4)  #regresa sin repetir los valores de los elementos en el vector

sort(var1)

sort(var1, decreasing = TRUE)


# 2 Listas ====================================================================

# Las listas pueden tener elementos de distintos tipos

l <- list('a', 1, 'b')   #¿Contiene elementos del mismo tipo??
l
typeof(l)

#Un objeto de una lista, puede ser otra lista

l1 <- list(x=1:5, y=c('a', 'b'))

# 2.1 Selección de objetos y elementos de una lista ----------------------------


l1[1]  #devuelve el primer objeto de la lista

l1[2]  #devuelve el segundo objeto de la lista

l1[[2]]  #elementos del segundo objeto

l1$y   # elementos del objeto y en la lista l1

l1$x   # elemento del objeto x en la lista l1

l1$y[1] # primer elemento del objeto y en l1


# 3 MATRICES ==================================================================

M <- matrix(var4, nrow=3, ncol=2)  #Convertir un vector en Matriz

M

# 3.1 Selección de elementos --------------------------------------------------

M[2, ] #Selecciona el segundo renglón

M[ ,1] # Selecciona la primera columna

M[2, 1] #selecciona el elemento del renglón 2 columna 1


# 4 DATA FRAMES ===============================================================

# Es un caso especial de una lista donde 
# todos los componentes son vectores del mismo tamaño

df <- data.frame(x = 1:3, y = c('a', 'b', 'c')) 

# 4.1 Selección de elementos en Data Frames ------------------------------------
# Igual que las matrices e igual que las listas.

df[,2] # Muestra los elementos de la columna 2
df$y

df[1, ] #Muestra los elementos del primer renglón

df <- cbind(df,tres=c(4,5,6))  #agregar columnas

df <- rbind(df, c(1,"d",7))  #agregar renglones

head(df)

# 4.2 Guardar DataFrame en un archivo -----------------------------------------

dir.create("Output")

write.csv(df, "Output/df.csv")

# 4.3 Leer un DataFrame de un archivo ----------------------------------------

Casas <- read.csv(url(
  "https://vincentarelbundock.github.io/Rdatasets/csv/MASS/Boston.csv"))

titanic <- read.csv(url(
  "https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/titanic.csv"
))

# 4.4 Explorar un DataFrame  -------------------------------------------------

View(titanic) # Ver el data frame en 1er cuadrante RStudio

head(titanic) # Muestra los primeros seis renglones

tail(titanic) # Muestra los últimos seis renglones

dim(titanic)  # Número de renglones y de columnas

nrow(titanic) # Número de renglones

ncol(titanic) # Número de columnas

summary(titanic)  #Resumen de varaibles numéricas de las bases de datos

# Convertir las variables 

titanic$Survived <- as.factor(titanic$Survived) #convertir una columna en factor

titanic$Sex <- as.factor(titanic$Sex)

summary(titanic)


# Crear una tabla cruzada 

table(titanic$Survived, titanic$Sex)


# 5 CICLOS FOR ===============================================================

for (col in titanic){
  print(col)
}

# 6 FUNCIONES  ===============================================================

cuadrado <- function(n){
  res <- n*n
  return(res)
}

cuadrado(7)

#7. INSTALAR Y CARGAR BIBLIOTECAS ============================================

# (por ejemplo ggplot2)


install.packages("ggplot2")   # Instalar

library(ggplot2)              # Cargar

# ejemplo utilizando funciones de (ggplot2)

xy <- data.frame(x=0:12)

ggplot(xy) +  geom_bar(aes(x = x, y = dbinom(x, 
                                         size=12, 
                                         prob=.5)),
                       stat="identity")

