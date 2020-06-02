# 1. Cargar librerías -----
library(tidyverse) #collección de las ppales librerias para 
library(here)      #Para manerjar mejor rutas de direcciones

library(NLP)       #requerido por libreria tm
library(tm)       #text mining (más información en: https://miamioh.instructure.com/courses/38953/pages/text-mining)

library (RColorBrewer)  #requerido por wordcloud
library(wordcloud)  #Para visualizar nubes de palabras

library(lattice)  #requerido por lattice
library(caret)    #Para dividir los datos en conjunto de prueba y entrenamiento

library(glmnet)     # Para entrenear modelos lineales generalizados (GLM) con penalizacion 

# 2. Lectura de los datos    -----

df <- read.csv(here("Datos","deceptive-opinion.csv"))

# 3. Exploración de datos ----

str (df)

# 3.1 Conversión de tipo de columnas ----

df$deceptive <- as.factor(df$deceptive)
df$hotel <- as.factor(df$hotel)
df$polarity <- as.factor(df$polarity)
df$source <- as.factor (df$source)

summary(df)

table(df$deceptive,df$polarity)

# 4. Vectorización mediante Bolsa de Palabras ----

# 4.1 Crear Corpus ----
corpus <- VCorpus (VectorSource(df$text)) #crear el corpus de opiniones

inspect(corpus[100])  #ver características uno de los textos

inspect(corpus[[100]])


# 4.2 Preprocesamiento de texto ----

corpus <- tm_map(corpus, content_transformer(tolower))
inspect(corpus[[100]])  #ver los cambios en un elemento

corpus <- tm_map (corpus, removeWords, stopwords("english"))
inspect(corpus[[100]])  #ver los cambios en un elemento

corpus <- tm_map (corpus, removePunctuation)
inspect(corpus[[100]])  #ver los cambios en un elemento


corpus <- tm_map (corpus, stemDocument)
inspect(corpus[[100]])  #ver los cambios en un elemento

corpus <- tm_map (corpus, removeWords, c("hotel", "hotels", 'room', "chicago", "stay"))
inspect(corpus[[100]])  #ver los cambios en un elemento

corpus <- tm_map (corpus, removeNumbers)
inspect(corpus[[100]])  #ver los cambios en un elemento

corpus <- tm_map (corpus, stripWhitespace) #remover espacios en blanco
inspect(corpus[[100]])  #ver los cambios en un elemento


# 4.3 Tokenización ---- 
#(crear matriz Termino-Documento donde los textos o documentos son columnas y cada palabra un renglón. 
#las palabras (Terminos) y los renglones cada texto (documentos))

tdm<-TermDocumentMatrix(corpus)
dim(tdm)


#4.4 Reducir la dimensionalidad ----


tdm_90<-removeSparseTerms(tdm,0.90) #elimina términos que en el 90% de los docs no aparecen
#113 términos restantes

tdm_99<-removeSparseTerms(tdm,0.99) #elimina términos que en el 99% de los docs no aparecen
#957 términos restantes

tdm_999<-removeSparseTerms(tdm,0.999) #elimina términos que en el 99.9% de los docs no aparecen
#3528 términos restantes

# 4.5 Visualizar nube de palabras -----

plot_wordclouds <-function(tdm){
  
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing=TRUE)
  d<- data.frame(word = names(v), frec=v)  
  
  set.seed(1234)
  print(dim(tdm))
  return (wordcloud(words = d$word, freq = d$frec, min.freq = 1,
                    max.words=50, random.order=FALSE, rot.per=0.35, 
                    colors=brewer.pal(8, "Dark2")))
}


plot_wordclouds(tdm = tdm_90)  #Visualizar nube de palabras

# 4.6 Separar etiquetas(Z) de caracteristicas (Y) ----

dtm<-as.DocumentTermMatrix(tdm_999)

X <- as.matrix(dtm)

Z <- df$deceptive


# 5. Dividir datos en conjunto de entrenamiento y prueba ----

set.seed(28)
test_index <- createDataPartition(df$deceptive, p=0.2, list=FALSE)

X_test <- X[test_index,]
Z_test <- Z[test_index]
X_train <- X[-test_index,]
Z_train <- Z[-test_index]

#5.1 Ver tamaño de conjunto de entrenamiento ----
dim(X_train)
length(Z_train)

#5.2 Ver tamaño de conjunto de prueba----
dim(X_test)
length(Z_test)



#  6. Entrenamiento del modelo ----- 


#6.1 Encontrar un valor de lambda óptimo ----
lambdas <- 10^seq(3,-2, by=-.1)

cv_ridge<-cv.glmnet(X_train,Z_train,family="binomial", alpha=0, standarize=TRUE, lambda=lambdas)

plot(cv_ridge)  #El punto más vajo en la curva indica el valor optimo para lambda

lambda_opt<- cv_ridge$lambda.min

fit <- cv_ridge$glmnet.fit  #guardamos todos los modelos de la validación cruzada

# 7 Predecir Z en X_test con distintos valores de lambda y medir desempeño -----
# Probar con lambda_opt, 0

lambda=lambda_opt

Z_predicted<-predict(fit, s=lambda, newx= X_test, type="class")
accuracy<-mean(Z_test==Z_predicted)
confusion<-caret::confusionMatrix(data=as.factor(Z_predicted), reference=Z_test)

accuracy

coef(fit, s=lambda)[400:420]

fourfoldplot(confusion$table)