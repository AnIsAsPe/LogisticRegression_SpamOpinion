# 1. Cargar librerías =========================================================

library(tidyverse) #collección de las ppales librerias para 
library(here)      #Para manerjar mejor rutas de direcciones

library(NLP)       #requerido por libreria tm
library(tm)   #text mining (más información en: 
              #https://miamioh.instructure.com/courses/38953/pages/text-mining)

library (RColorBrewer)  #requerido por wordcloud
library(wordcloud)     #Para visualizar nubes de palabras

library(lattice)  #requerido por caret
library(caret)    #Para dividir los datos en conjunto de prueba y entrenamiento

library(Matrix)     #Requerido por Glmnet
library(glmnet)     # Para entrenear modelos lineales generalizados (GLM) 
                    #con penalizacion 

# 2. Lectura de los datos  =====================================================

df <- read.csv(here("Datos","deceptive-opinion.csv"))

# 3. Exploración de datos  =====================================================

str (df)

# 3.1 Conversión de tipo de columnas----------------------------------------

df$deceptive <- as.factor(df$deceptive)
df$hotel <- as.factor(df$hotel)
df$polarity <- as.factor(df$polarity)
df$source <- as.factor (df$source)

summary(df)

table(df$deceptive,df$polarity)

# 4. Vectorización mediante Bolsa de Palabras ==================================

# 4.1 Crear Corpus -------------------------------------------------------

corpus <- VCorpus (VectorSource(df$text)) #crear el corpus de opiniones

inspect(corpus[100])  #ver características un elemento del corpus
                      
inspect(corpus[[100]]) #leer el documento del elemento 100 del corpus

# 4.2 Preprocesamiento de texto -------------------------------------------

#Transformar todas las letras a minúsculas
corpus <- tm_map(corpus, content_transformer(tolower))

#Remover "stopwords" (Palabras muy comunes sin significado importante)
corpus <- tm_map (corpus, removeWords, stopwords("english"))

corpus <- tm_map (corpus, removeWords, c("hotel", "hotels", 'room', 
                                         "chicago", "stay"))
#Remover signos de puntuación
corpus <- tm_map (corpus, removePunctuation)

#Obtener la raiz de las palabras (stemming)
corpus <- tm_map (corpus, stemDocument)

#Remover números
corpus <- tm_map (corpus, removeNumbers)

#Remover espacios en blanco de más
corpus <- tm_map (corpus, stripWhitespace) 


# 4.3 Tokenización  -------------------------------------------------------

#(crear matriz Termino-Documento donde los textos o documentos son columnas
# y cada palabra un renglón. las palabras (Terminos) y los renglones 
# cada texto (documentos))

tdm <- TermDocumentMatrix(corpus)

dim(tdm)

#4.4 Reducir la dimensionalidad --------------------------------------------

#La siguiente línea elimina términos que en el 90% de los docs no aparecen:
tdm_90 <- removeSparseTerms(tdm, 0.90) #113 términos restantes

#La siguiente línea elimina términos que en el 99% de los docs no aparecen
tdm_99 <- removeSparseTerms(tdm, 0.99) #957 términos restantes

#elimina términos que en el 99.9% de los docs no aparecen
tdm_999 <- removeSparseTerms(tdm, 0.999) #3528 términos restantes

# 4.5 Visualizar nube de palabras -------------------------------------------


plot_wordclouds <- function(tdm){
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing=TRUE)
  d <- data.frame(word = names(v), frec=v)  
  
  set.seed(1234)
  print(dim(tdm))
  return (wordcloud(words = d$word, freq = d$frec, min.freq = 1,
                    max.words = 50, random.order = FALSE, rot.per = 0.35, 
                    colors = brewer.pal(8, "Dark2")))
}

plot_wordclouds(tdm = tdm_999)  


# 4.6 Separar etiquetas(Z) de caracteristicas (Y) -----------------------------

dtm <- as.DocumentTermMatrix(tdm_999)

X <- as.matrix(dtm)

Z <- df$deceptive

# 5. Dividir datos en conjunto de entrenamiento y prueba ======================

set.seed(28)

test_index <- createDataPartition(df$deceptive, p=0.2, list=FALSE)

X_test <- X[test_index,]
Z_test <- Z[test_index]
X_train <- X[-test_index,]
Z_train <- Z[-test_index]

#5.1 Ver tamaño de conjunto de entrenamiento --------------------------------

dim(X_train)
length(Z_train)

#5.2 Ver tamaño de conjunto de prueba  --------------------------------------

dim(X_test)
length(Z_test)


#  6. Entrenamiento del modelo ===============================================

#6.1 Encontrar un valor de lambda óptimo -------------------------------------

#lambdas <- c( (10^seq(3,-2, by=-.1) ), 0 )

lambdas <- 10^seq(3, -2, by=-.1)

plot(lambdas)

cv_ridge <- cv.glmnet(X_train, Z_train, family="binomial", 
                      alpha=0, standarize=TRUE, lambda =lambdas)

plot(cv_ridge)  #El punto más bajo en la curva indica el valor óptimo para lambda

lambda_min <- cv_ridge$lambda.min
lambda_1se <- cv_ridge$lambda.1se

#guardamos todos los modelos de la validación cruzada
fit <- cv_ridge$glmnet.fit

# 7 Predecir Z en X_test con distintos valores de lambda y medir desempeño =====

ResultadosConjuntoValidacion <- function(lambda){
  Z_predicted <- predict(fit, s=lambda, newx= X_test, type="class")
  accuracy <- mean(Z_test == Z_predicted)
  confusion <- caret::confusionMatrix(data=as.factor(Z_predicted), 
                                      reference=Z_test)
  tmp_coeffs <- coef(fit, s=lambda)
  coeficientes <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1],
                             coefficient = tmp_coeffs@x)
  fourfoldplot(confusion$table, main = paste("SET: Validación. Lambda = ",
                                             lambda, "   Accuracy =", 
                                             accuracy*100,"%"))  
  print("Primeros 20 coeficientes")
  print(head(coeficientes,20))
  resultados < list(accuracy, coeficientes,Z_predicted,confusion)  
return(resultados)
}

ResultadosConjuntoEntrenamiento <- function(lambda){
  Z_predicted <- predict(fit, s=lambda, newx= X_train, type="class")
  accuracy <- mean(Z_train==Z_predicted)
  confusion <- caret::confusionMatrix(data = as.factor(Z_predicted), 
                                      reference = Z_train)
  tmp_coeffs <- coef(fit, s = lambda)
  coeficientes <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], 
                           coefficient = tmp_coeffs@x)
  fourfoldplot(confusion$table, main = paste("SET: Entrenamiento.  Lambda =",
                                           lambda, "   Accuracy =",
                                           accuracy*100, "%"))  
  print("Primeros 20 coeficientes")
  print(head(coeficientes, 20))
  resultados <- list(accuracy, coeficientes, Z_predicted, confusion)  
  return(resultados)
}

# Ejercicio ##################################################################
#Probar para distintas lambdas (0, lambda_min, lambda_1se, 10, 100,1000)

lambda_interes = lambda_1se

res_entrenamiento <- ResultadosConjuntoEntrenamiento(lambda = lambda_interes)

res_validacion < -ResultadosConjuntoValidacion(lambda = lambda_interes)

## Para guardar la predicción en términos de clase y probabilidad:
Z_pred_class <- predict(fit, s = lambda_interes, newx = X_test, type = "class")
Z_pred_prob <- predict(fit, s = lambda_interes, newx = X_test, type ="response")

Z_validacion <- data.frame(Z_test, Z_pred_class, Z_pred_prob)
names(Z_validacion)[2] <- "class"
names(Z_validacion)[3] <- "probabilidad"

head(Z_validacion, 20)


#8 Realizar una regresión logistica sin penalización ===========================

or <- glmnet(X_train, Z_train, family = "binomial", alpha = 0, 
             standarize = TRUE,lambda=0)

Z_predicted <- predict(or, newx= X_test, type="class")
accuracy <- mean(Z_test == Z_predicted)
confusion <- caret::confusionMatrix(data = as.factor(Z_predicted), 
                                  reference = Z_test)

temp_coeff_or <- coef(or)
coeff_or <- data.frame(name = temp_coeff_or@Dimnames[[1]][temp_coeff_or@i + 1], 
                     coefficient = temp_coeff_or@x)

fourfoldplot(confusion$table, main=paste("SET: Validación.    Lambda = 0", 
                                         "   Accuracy =", accuracy*100, "%"))  
print("Primeros 20 coeficientes")
print(head(coeff_or, 20))

mod <- glm( Z_train ~ X_train,family = "binomial")
