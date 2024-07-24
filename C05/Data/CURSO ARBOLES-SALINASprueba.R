################################
#                              #
#   ÁRBOLES DE CLASIFICACIÓN   #
#        EJEMPLO Valoración_credito         #
#   MG. Jesús Salinas Flores   #
#   jsalinas@lamolina.edu.pe   #
#                              #
################################


# 1. Librerías 

library(foreign)
library(gmodels)
library(vcd)
library(gplots)
library(partykit)
library(rpart)
library(rpart.plot)
library(iplots)
library(e1071)
library(caTools)
library(InformationValue)
library(caret)
library(randomForest)
library(adabag)


# 2. Descripción de los datos 
# Variable Dependiente:  
#           Valoración_credito   (0=Cliente Actual, 1=Fuga Voluntaria)
# Variables Independentes:
#           EDAD    (Edad del cliente en años)  
#           SEXO    (Sexo del cliente, 1=Fememino 2=Masculino)
#           CIVIL   (Estado civil del cliente, 1=Casado 2=Soltero) 
#           HIJOS   (Número de hijos del cliente)
#           INGRESO (Ingresos anuales del cliente)
#           AUTO    (Si el cliente es dueño de un auto, 1=Si 2=No)  
# Variable de identificación: 
#           ID      (Código del cliente)



str(datos)
attach(datos)
names(datos)

# 4. Análisis Exploratorio de los datos 

# Análisis descriptivo univariado

round(prop.table(table(Valoración_credito)),2)

boxplot(Edad,    ylab="Edad",            col="blue")





# 5. Árbol de Clasificación con el algoritmo RPART 
library(rpart)

# Ejemplo 1: Árbol con los parámetros por defecto

set.seed(123)
arbol1 <- rpart(Valoración_credito ~ . , 
                data=datos, 
                method="class")

# Si usa method="anova" es para Modelos de Regresión
str(arbol1)
arbol1
arbol1$variable.importance

# Graficando el arbol
library(rpart.plot)
rpart.plot(arbol1, digits=-1, type=0, extra=101,cex = .7, nn=TRUE)
rpart.plot(arbol1, digits=-1, type=1, extra=101,cex = .7, nn=TRUE)
rpart.plot(arbol1, digits=-1, type=2, extra=101,cex = .7, nn=TRUE)
rpart.plot(arbol1, digits=-1, type=3, extra=101,cex = .7, nn=TRUE)
rpart.plot(arbol1, digits=-1, type=4, extra=101,cex = .7, nn=TRUE)

prp(arbol1, digits=-1,  faclen = 0, type=0, cex = 0.8, extra = 1)
prp(arbol1, digits=-1,  faclen = 0, type=0, cex = 0.8, extra = 4)
prp(arbol1, digits=-1,  faclen = 0, type=2, cex = 0.8, extra = 1)

library(partykit)
plot(as.party(arbol1), tp_args = list(id = FALSE))

# Resumen del árbol 
summary(arbol1)


# Ejemplo 2: Árbol controlando parametros
# Parámetros 
# minsplit:   Indica el número mínimo de observaciones en un nodo para
#             que este sea dividido. Mínimo para que un nodo sea padre. 
#             Esta opción por defecto es 20.
# minbucket:  Indica el número mínimo de observaciones en cualquier
#             nodo terminal. Por defecto esta opción es el valor 
#             redondeado de minsplit/3.
# cp:         Parámetro de complejidad. Indica que si el criterio de 
#             impureza no es reducido en mas de cp*100% entonces se 
#             para. Por defecto cp=0.01. Es decir, la reducción en la 
#             impureza del nodo terminal debe ser de al menos 1% de la
#             impureza inicial.
# maxdepth:   condiciona la profundidad máxima del arbol. 
#             Por defecto está establecida como 30.

arbol1$control

set.seed(123)
arbol2 <- rpart(Valoración_credito ~ . , 
                data=datos,
                control=rpart.control(minsplit=90, minbucket=30),#control PARAMETROS
                method="class")

rpart.plot(arbol2, 
           digits=-1,
           type=2, 
           extra=101,
           cex = 0.7, 
           nn=TRUE)

printcp(arbol2)	   # Muestra la tabla cp
plotcp(arbol2)	   # Resultados del ploteo de validación cruzada  
print(arbol2)	     # Imprime resultados 
summary(arbol2)	   # Resultados detallados incluyendo sustitutos (surrogate)


# Ejemplo 3: Controlando el crecimiento del árbol
# con el parámetro de complejidad (cp=0.05)

set.seed(123)
arbol3 <-  rpart(Valoración_credito ~ . , 
             data=datos,
             control=rpart.control(minsplit=90, minbucket=30,cp=0.05),
             method="class")

rpart.plot(arbol3, 
           digits=-1,
           type=2, 
           extra=101,
           cex = 0.7, 
           nn=TRUE)

printcp(arbol3)
plotcp(arbol3)

# Ejemplo 4: Controlando el crecimiento del árbol
#           por número máximo de niveles (maxdepth=2)
set.seed(123)
arbol4 <- rpart(Valoración_credito~. ,
                data=datos,
                method="class",
                maxdepth=2)

rpart.plot(arbol4, 
           digits=-1,
           type=2, 
           extra=101,
           cex = 0.7, 
           nn=TRUE)

printcp(arbol4)
plotcp(arbol4)


# Ejemplo 5: cp=0.001 para obtener un árbol con más ramas

set.seed(123)
arbol5 <- rpart(Valoración_credito ~ . ,
             data=datos, 
             method="class",
             cp=0.001)

rpart.plot(arbol5, 
           digits=-1,
           type=2, 
           extra=101,
           cex = 0.7, 
           nn=TRUE)

printcp(arbol5)
plotcp(arbol5)

# Ejemplo 6: Recortar el árbol
arbol6 <- prune(arbol5,cp=0.1)

rpart.plot(arbol5, 
           digits=-1,
           type=2, 
           extra=101,cex = 0.7, 
           nn=TRUE)
printcp(arbol5)

arbol7 <- prune(arbol5,cp=0.01)
rpart.plot(arbol7, 
           digits=-1,
           type=2, 
           extra=101,
           cex = .7, nn=TRUE)

printcp(arbol7)
plotcp(arbol7)

# Ejemplo 7: Tuneando los parámetros
library(e1071)
# Controlando el minbucket
set.seed(123)
arbol7 <- tune.rpart(Valoración_credito ~ .,data=datos,minbucket=seq(10,50,by=1))
             
arbol7

plot(arbol7)
summary(arbol7)

# Controlando el parámetro de complejidad cp
set.seed(123)
arbol7 <- tune.rpart(Valoración_credito ~ .,data=datos,cp=seq(0,0.5,by=0.001))
      
arbol7

plot(arbol7)
summary(arbol7)

set.seed(123)
arbol8 <- rpart(Valoración_credito~. ,
                data=datos,
                method="class",
                cp=0.007)

rpart.plot(arbol8, 
           digits=-1,
           type=2, 
           extra=101,
           cex = 0.7, 
           nn=TRUE)

printcp(arbol8)
plotcp(arbol8)

# Ejemplo 8: Valor óptimo de CP
set.seed(123)
arbol.completo <- rpart(Valoración_credito ~ . ,
                        data=datos,
                        method="class",
                        cp=0, 
                        minbucket=0)
arbol.completo
printcp(arbol.completo)

plotcp(arbol.completo)

rpart.plot(arbol.completo, 
           digits=-1,
           type=2, 
           extra=101,
           cex = 0.7, 
           nn=TRUE)

str(arbol.completo)
arbol.completo$cptable  # Es similar a  printcp(arbol.completo)

xerr <- arbol.completo$cptable[,"xerror"]
xerr

minxerr <- which.min(xerr)
minxerr

mincp <- arbol.completo$cptable[minxerr, "CP"]
mincp

arbol.pruned <- prune(arbol.completo,cp=mincp)
arbol.pruned

printcp(arbol.pruned)
plotcp(arbol.pruned)

rpart.plot(arbol.pruned, 
           type=2, 
           extra=101,
           cex = 0.7, 
           nn=TRUE)

#------------------------------------
# Predicción usando el árbol podado

# Calcular los valores predichos
# PRED <-predict(arbol.pruned,datos[,c(1:6)],type="class")

CLASE.CART <- predict(arbol.pruned,
                datos,
                type="class")
CLASE.CART


# Calculando las probabilidades 
PROBA.CART <- predict(arbol.pruned, 
                      datos, 
                      type = "prob")
str(PROBA.CART)
head(PROBA.CART)

PROBA.CART <- PROBA.CART[,2]
head(PROBA.CART)
PROBA.CART

datoscart <- cbind(datos,CLASE.CART, PROBA.CART)
datoscart
str(datoscart)
write.csv(datoscart,"Valoración_credito con valor y probabilidad predicha de FUGA-CART_pruebA.csv")


#  6. Indicadores para Evaluación de Modelos
# Tabla de clasificación
library(gmodels)
CrossTable(x = Valoración_credito, y = CLASE.CART,
           prop.t=FALSE, prop.c=FALSE, prop.chisq = FALSE)

addmargins(table(Real=Valoración_credito,Clase_Predicha=CLASE.CART))
prop.table(table(Real=Valoración_credito,Clase_Predicha=CLASE.CART),1)

# Calcular el accuracy
accuracy <- mean(Valoración_credito==CLASE.CART) ; accuracy

# Calcular el error de mala clasificación
error <- mean(Valoración_credito!=CLASE.CART) ; error

# Curva ROC usando el paquete caTools
library(caTools)
colAUC(PROBA.CART,Valoración_credito,plotROC = TRUE)
abline(0, 1,col="red",lty=2)



# 8. Validación con Muestra de Entrenamiento y de Prueba 
# Selección de muestra de entrenamiento (70%) y de Validación (30%)

str(datos)                               # 1345 datos

library(caret)
set.seed(123) 

index      <- createDataPartition(datos$Valoración_credito, p=0.8, list=FALSE)
data.train <- datos[ index, ]            # 943 datos trainig             
data.test  <- datos[-index, ]            # 402 datos testing

round(prop.table(table(datos$Valoración_credito)),3)
round(prop.table(table(data.train$Valoración_credito)),3)
round(prop.table(table(data.test$Valoración_credito)),3)

# Árbol de Clasificación con valor óptimo de CP
set.seed(123)
arbol.completo <- rpart(Valoración_credito ~ . ,
                        data=data.train,
                        method="class",
                        cp=0, 
                        minbucket=0)
arbol.completo
printcp(arbol.completo)

rpart.plot(arbol.completo, type=2, extra=101,cex = .7, nn=TRUE)

str(arbol.completo)
xerr         <- arbol.completo$cptable[,"xerror"]
minxerr      <- which.min(xerr)
mincp        <- arbol.completo$cptable[minxerr, "CP"]
mincp
arbol.pruned <- prune(arbol.completo,cp=mincp)
arbol.pruned

rpart.plot(arbol.pruned, 
           digits=-1,
           type=2, 
           extra=101,
           cex = 0.7, 
           nn=TRUE)

# Prediciendo la Clase y Probabilidad en la muestra TEST
CLASE.CART <- predict(arbol.pruned,data.test[,c(2:6)],type="class")
CLASE.CART

PROBA.CART <- predict(arbol.pruned, 
                      data.test[,c(2:6)], 
                      type = "prob")
str(PROBA.CART)
head(PROBA.CART)

PROBA.CART <- PROBA.CART[,2]

ggplot(data.test, aes(x = PROBA.CART, fill = Valoración_credito)) + geom_density(alpha = 0.25)

# Evaluando la performance del modelo en la muestra TEST
# Tabla de clasificación
library(gmodels)
CrossTable(x = data.test$Valoración_credito, y = CLASE.CART,
           prop.t=FALSE, prop.c=FALSE, prop.chisq = FALSE)

addmargins(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.CART))
prop.table(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.CART),1)

# Calcular el accuracy
accuracy <- mean(data.test$Valoración_credito==CLASE.CART) ; accuracy
# Calcular el error de mala clasificación
error <- mean(data.test$Valoración_credito!=CLASE.CART) ; error

# Curva ROC usando el paquete caTools
library(caTools)
colAUC(PROBA.CART,data.test$Valoración_credito,plotROC = TRUE)
abline(0, 1,col="red",lty=3)


#  9. CART con el paquete caret y validación cruzada repetida  
# Relación de modelos 
library(caret)
names(getModelInfo())

#Relación de parámetros a ajustar de un modelo
modelLookup(model='rpart')

# Aplicando el modelo con Validación Cruzada Repetida 
set.seed(123)
ctrl <- trainControl(method="repeatedcv",
                     repeats = 3, number=10)
#                     classProbs=TRUE,summaryFunction = twoClassSummary)

modelo_cart <- train(Valoración_credito ~ ., 
                    data = data.train, 
                    method = "rpart", 
                    trControl = ctrl, 
                    tuneGrid = expand.grid(cp=seq(0,0.5,0.01)),
                    metric="Accuracy" )
modelo_cart

plot(modelo_cart)

CLASE.CART <- predict(modelo_cart,newdata = data.test )
head(CLASE.CART)

PROBA.CART <- predict(modelo_cart,newdata = data.test, type="prob")
PROBA.CART <- PROBA.CART[,2]
head(PROBA.CART)

# Evaluando la performance del modelo
# Tabla de clasificación
library(gmodels)
CrossTable(x = data.test$Valoración_credito, y = CLASE.CART,
           prop.t=FALSE, prop.c=FALSE, prop.chisq = FALSE)

addmargins(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.CART))
prop.table(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.CART),1)

# Calcular el accuracy
accuracy <- mean(data.test$Valoración_credito==CLASE.CART) ; accuracy
# Calcular el error de mala clasificación
error <- mean(data.test$Valoración_credito!=CLASE.CART) ; error

# Curva ROC usando el paquete caTools
library(caTools)
colAUC(PROBA.CART,data.test$Valoración_credito,plotROC = TRUE)
abline(0, 1,col="red")
varImp(modelo_cart)
#  10. Bagging para mejorar un modelo predictivo   
library(adabag)
library(rpart)

set.seed(123) 
library(adabag)
Modelo_AdaBag  <- adabag::bagging(Valoración_credito ~ ., 
                          data=data.train, 
                          mfinal=99)
Modelo_AdaBag$formula
Modelo_AdaBag$votes
Modelo_AdaBag$prob
Modelo_AdaBag$class
Modelo_AdaBag$samples
Modelo_AdaBag$importance

CLASE.BAG <- predict(Modelo_AdaBag,data.test,type="class")$class
CLASE.BAG
PROBA.BAG <- predict(Modelo_AdaBag,data.test,type="class")$prob
PROBA.BAG <- PROBA.BAG[,2] 

# Evaluando la performance del modelo
# Tabla de clasificación
library(gmodels)
CrossTable(x = data.test$Valoración_credito, y = CLASE.BAG,
           prop.t=FALSE, prop.c=FALSE, prop.chisq = FALSE)

addmargins(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.BAG))
prop.table(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.BAG),1)

# Calcular el accuracy
accuracy <- mean(data.test$Valoración_credito==CLASE.BAG) ; accuracy
# Calcular el error de mala clasificación
error <- mean(data.test$Valoración_credito!=CLASE.BAG) ; error

# Curva ROC usando el paquete caTools
library(caTools)
colAUC(PROBA.BAG,data.test$Valoración_credito,plotROC = TRUE)
abline(0, 1,col="red")

#  11. BAGGING con el paquete caret y validación cruzada repetida  
set.seed(123)
ctrl <- trainControl(method="repeatedcv",
                     repeats = 3, number=10)
modelo_bag <- train(Valoración_credito ~ ., 
                     data = data.train, 
                     method = "treebag",
                     trControl = ctrl, 
                     tuneLength = 5, 
                     metric="Accuracy")
modelo_bag

plot(modelo_bag)
# treebag no tiene parámetros para hacer tunning

CLASE.BAG <- predict(modelo_bag,newdata = data.test )
head(CLASE.BAG)

PROBA.BAG <- predict(modelo_bag,newdata = data.test, type="prob")
PROBA.BAG <- PROBA.BAG[,2]
head(PROBA.BAG)

------------------------------------------------------------
# Evaluando la performance del modelo
# Tabla de clasificación
library(gmodels)
CrossTable(x = data.test$Valoración_credito, y = CLASE.BAG,
           prop.t=FALSE, prop.c=FALSE, prop.chisq = FALSE)

addmargins(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.BAG))
prop.table(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.BAG),1)

# Calcular el accuracy
accuracy <- mean(data.test$Valoración_credito==CLASE.BAG) ; accuracy
# Calcular el error de mala clasificación
error <- mean(data.test$Valoración_credito!=CLASE.BAG) ; error

# Curva ROC usando el paquete caTools
library(caTools)
colAUC(PROBA.BAG,data.test$Valoración_credito,plotROC = TRUE)
abline(0, 1,col="red")

#  12. Random Forests   
set.seed(123)
library(randomForest)
MODELO.RF <- randomForest( Valoración_credito ~ ., data = data.train,   # Datos a entrenar 
                           ntree=100,                      # Número de árboles
                           mtry = 3,                       # Cantidad de variables
                           importance = TRUE,              # Determina la importancia de las variables
                           replace=T)                      # Muestras con reemplazo
                                      
# Mediciones del Random Forest

str(MODELO.RF)
head(MODELO.RF$votes)
head(MODELO.RF$predicted)

print(MODELO.RF)
# Graficar Error del Modelo
#
# En este gráfico se muestra un modelo que intenta predecir 
# la variable Valoración_credito={FUGA,ACTUAL}. 
# La linea negra representa el OOB, 
# la linea roja es el error al intentar predecir Valoración_credito={ACTUAL}, 
# la linea verde es el error en la prediccion Valoración_credito={FUGA}. 
# La linea negra siempre será el OOB, y las siguientes lineas
# se pueden identificar con la matriz de confusión 
# usando print(MODELO.RF) 

plot(MODELO.RF)

# Importancia de las variables
varImpPlot(MODELO.RF)
importance(MODELO.RF) 

# Calcular los valores predichos
CLASE.RF <-predict(MODELO.RF,data.test[,c(1:6)],type="class")
head(CLASE.RF)

# Calculando las probabilidades
PROBA.RF <-predict(MODELO.RF,data.test[,c(1:6)],type="prob")
head(PROBA.RF)

PROBA.RF <- PROBA.RF[,2]
head(PROBA.RF)

# Evaluando la performance del modelo
# Tabla de clasificación
library(gmodels)
CrossTable(x = data.test$Valoración_credito, y = CLASE.RF,
           prop.t=FALSE, prop.c=FALSE, prop.chisq = FALSE)

addmargins(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.RF))
prop.table(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.RF),1)

# Calcular el accuracy
accuracy <- mean(data.test$Valoración_credito==CLASE.RF) ; accuracy
# Calcular el error de mala clasificación
error <- mean(data.test$Valoración_credito!=CLASE.RF) ; error

# Curva ROC usando el paquete caTools
library(caTools)
colAUC(PROBA.RF,data.test$Valoración_credito,plotROC = TRUE)
abline(0, 1,col="red")

#  13. RANDOM FOREST con el paquete caret y validación cruzada repetida  
set.seed(123)
ctrl <- trainControl(method="repeatedcv",
                     repeats = 3, number=10)

modelo_rf <- train(Valoración_credito ~ ., 
                    data = data.train, 
                    method = "rf", 
                    trControl = ctrl, 
                    tuneLength = 5,
                    metric="Accuracy")

modelo_rf

plot(modelo_rf)

CLASE.RF <- predict(modelo_rf,newdata = data.test )
head(CLASE.RF)

PROBA.RF <- predict(modelo_rf,newdata = data.test, type="prob")
PROBA.RF <- PROBA.RF[,2]
head(PROBA.RF)

# Evaluando la performance del modelo
# Tabla de clasificación
library(gmodels)
CrossTable(x = data.test$Valoración_credito, y = CLASE.RF,
           prop.t=FALSE, prop.c=FALSE, prop.chisq = FALSE)

addmargins(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.RF))
prop.table(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.RF),1)

# Calcular el accuracy
accuracy <- mean(data.test$Valoración_credito==CLASE.RF) ; accuracy
# Calcular el error de mala clasificación
error <- mean(data.test$Valoración_credito!=CLASE.RF) ; error

# Curva ROC usando el paquete caTools
library(caTools)
colAUC(PROBA.RF,data.test$Valoración_credito,plotROC = TRUE)
abline(0, 1,col="red")

#  14. ADABOOSTING  
set.seed(123) 
library(adabag)
Modelo_AdaBoost  <- boosting(Valoración_credito ~ ., 
                            data=data.train,
                            boos = T,
                            coeflearn="Breiman",
                            mfinal=99)
Modelo_AdaBoost$formula
Modelo_AdaBoost$votes
Modelo_AdaBoost$prob
Modelo_AdaBoost$class
errorevol(Modelo_AdaBoost,data.train)
Modelo_AdaBoost$importance

t1<-Modelo_AdaBoost$trees[[1]]
library(rpart.plot)
rpart.plot(t1, 
           digits=-1,
           type=2, 
           extra=101,
           cex = 0.7, 
           nn=TRUE)


t99<-Modelo_AdaBoost$trees[[99]]
library(rpart.plot)
rpart.plot(t99, 
           digits=-1,
           type=2, 
           extra=101,
           cex = 0.7, 
           nn=TRUE)

CLASE.BOOS <- predict(Modelo_AdaBoost,data.test,type="class")$class
CLASE.BOOS
PROBA.BOOS <- predict(Modelo_AdaBoost,data.test,type="class")$prob
PROBA.BOOS <- PROBA.BOOS[,2] 

# Evaluando la performance del modelo
# Tabla de clasificación
library(gmodels)
CrossTable(x = data.test$Valoración_credito, y = CLASE.BOOS,
           prop.t=FALSE, prop.c=FALSE, prop.chisq = FALSE)

addmargins(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.BOOS))
prop.table(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.BOOS),1)

# Calcular el accuracy
accuracy <- mean(data.test$Valoración_credito==CLASE.BOOS) ; accuracy
# Calcular el error de mala clasificación
error <- mean(data.test$Valoración_credito!=CLASE.BOOS) ; error

# Curva ROC usando el paquete caTools
library(caTools)
colAUC(PROBA.BOOS,data.test$Valoración_credito,plotROC = TRUE)
abline(0, 1,col="red")

#  15. ADABOOSTING con el paquete caret  
set.seed(123)
ctrl <- trainControl(method="repeatedcv",
                     repeats = 3, number=10)

modelo_boos <- train(Valoración_credito ~ ., 
                     data = data.train, 
                     method = "AdaBoost.M1", tuneLength=1,
                     trControl=ctrl, metric="Accuracy")

modelo_boos

plot(modelo_boos)

CLASE.BOOS <- predict(modelo_boos,newdata = data.test )
head(CLASE.BOOST)

PROBA.BOOS <- predict(modelo_boos,newdata = data.test, type="prob")
PROBA.BOOS <- PROBA.BOOS[,2]
head(PROBA.BOOS)

# Evaluando la performance del modelo
# Tabla de clasificación
library(gmodels)
CrossTable(x = data.test$Valoración_credito, y = CLASE.BOOS,
           prop.t=FALSE, prop.c=FALSE, prop.chisq = FALSE)

addmargins(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.BOOS))
prop.table(table(Real=data.test$Valoración_credito,Clase_Predicha=CLASE.BOOS),1)

# Calcular el accuracy
accuracy <- mean(data.test$Valoración_credito==CLASE.BOOS) ; accuracy
# Calcular el error de mala clasificación
error <- mean(data.test$Valoración_credito!=CLASE.BOOS) ; error

# Curva ROC usando el paquete caTools
library(caTools)
colAUC(PROBA.BOOS,data.test$Valoración_credito,plotROC = TRUE)
abline(0, 1,col="red")

# 16. Comparando el entrenamiento en los modelos 
modelos  <- list(CART          = modelo_cart,
                 Bagging       = modelo_bag,
                 Random_Forest = modelo_rf,
                 AdaBoosting   = modelo_boos)

comparacion_modelos <- resamples(modelos) 
summary(comparacion_modelos)

dotplot(comparacion_modelos)
bwplot(comparacion_modelos)
densityplot(comparacion_modelos, metric = "Accuracy",auto.key=TRUE)
