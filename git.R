# ---------------------------------------------------------
# ----------------- Clasificación con SVM -----------------

# Se cargan las librerias ---------------------------------
  library("e1071")
  library(MASS)
  library(rpart)
  library(unbalanced)
  library(foreach)
  library(doParallel)
  library(iterators)
  library(parallel)
  library(mlr)
  library(ParamHelpers)
  library(caret)
  library(magrittr)
  library(readxl)

  set.seed(123) # Semilla
  
# Se importar el archivo -----------------------------------
  rattle <- read_excel("rattle.xlsx")
  rattle <- as.data.frame(rattle)
  View(rattle)

# Se comprueba que no hay NAs ------------------------------
  sum(is.na(rattle))
  numcols <- rattle %>%
    sapply(is.numeric) %>%    
    which() %>%               
    names()                     

# Se generan nuevos casos. SMOTE ---------------------------
  input <- subset(rattle, select = -status)
  output <- as.factor(rattle$status)
  SMOTE_data <- ubSMOTE(X = input,
                        Y = output, 
                        perc.over = 200, 
                        perc.under = 200, 
                        verbose = TRUE)

  rattle2 <- cbind(SMOTE_data$X, status = SMOTE_data$Y)
  summary(rattle2)
  dim(rattle2)
  View(rattle2)

# 1. Train-test split -------------------------------------

# Se divide la base en train (80%) y test (20%) -----------
  train_ind <- createDataPartition(rattle2$status, 
                                   p=0.8,
                                   list = F)
  train <- rattle2[train_ind, ]
  dim(train)
  test  <- rattle2[-train_ind, ]
  dim(test)
  
# Se entrena el algoritmo ---------------------------------
  svm_fit <- svm(status ~ .,
                 data=train,
                 kernel="radial", 
                 cost=1,
                 gamma=0.5)
  summary(svm_fit)

# Se realiza la prediccion en ambos subconjuntos ----------
  pred_train <- predict(svm_fit, train)
  pred_test  <- predict(svm_fit, test)

# Se adjunta a la base la predicción ----------------------
  train_final <- cbind(train, pred_train)
  test_final  <- cbind(test, pred_test)
  
# Matriz de confusión -------------------------------------
  confusionMatrix(train_final$pred_train, train_final$status)
  confusionMatrix(test_final$pred_test, test_final$status)
  
# 2. Cross-validation -------------------------------------
  
  train_control <- trainControl(method="cv", number=10)

# Se vuelve a generar el modelo
  model <- svm(status ~ .,
               data = train, 
               kernel="radial", 
               cost=1,
               gamma=0.5,
               trControl = train_control, 
               method = "rpart")

# Se realiza la prediccion
  predictions <- predict(model, test)

# Se adjunta a la base la predicción.
  test_final <- cbind(test, predictions)

# Matriz de confusión.
  confusionMatrix <- confusionMatrix(test_final$predictions, test_final$status)
  confusionMatrix

