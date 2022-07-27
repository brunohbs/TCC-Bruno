
medidas = function(bancod){
  library(rpart)
  library(rpart.plot)
  library(caret)
  library(pROC)
  library("glmnet")
  library(tidyverse)
  library(cowplot)
  library(randomForest)
  
  teste = bancod # Funcionar a função
  
  med.testes <- data.frame() # Criando um df vazio
  
  med.final <- data.frame() # Criando um df vazio
  
  for (k in 8:40){
    for (l in 5){
      
      banco_dados <- banco(k,18,l,8)
      
      
      banco_dados_teste <- banco_dados[481:nrow(banco_dados),]
      
      banco_dados_trei <- banco_dados[-(481:nrow(banco_dados)),]
      
      x.t <- as.matrix(banco_dados_trei[, -(1:5)])
      y.t <- as.numeric(banco_dados_trei[, 4])
      
      x.teste <- as.matrix(banco_dados_teste[, -(1:5)])
      y.teste <- as.numeric(banco_dados_teste[, 4])
  
  
  # Árvore binária
  
  modelo1 <- rpart(PLAYOFFS~., data = banco_dados_trei[,-(1:3)],control = rpart.control(maxdepth = 7, minsplit = 1), method = "class")
  
  roc.info <- roc(banco_dados_trei$PLAYOFFS, rpart.predict(modelo1)[,2], plot = F, legacy.axes=F, print.auc = F) # Obtendo as informações da curva roc
  
  AUC <- roc.info$auc[1]*100 # Salvando AUC
  
  roc.df <-  data.frame( # Gerando uma tabela com as informações da curva ROC
    mean = (roc.info$specificities*100 + roc.info$sensitivities*100)/2,
    dif = abs(roc.info$specificities*100 - roc.info$sensitivities),
    spec = roc.info$specificities*100,
    tpp = roc.info$sensitivities*100,
    fpp=(1 - roc.info$specificities)*100,
    thresholds = roc.info$thresholds
  )
  
  dif = 100
  
  for (i in 1:nrow(roc.df)){ # Definindo o ponto de corte
    modulo <- abs(roc.df[i,1] - AUC)
    if (modulo < dif){
      dif <- modulo
      corte <- roc.df[i,6]
    }
    
  }
  
  previsoes_modelo1 <- predict(modelo1, newdata=banco_dados_teste[,-(1:4)])[,2] > corte # Fazendo as previsões do modelo
  
  a <- confusionMatrix(table(as.numeric(previsoes_modelo1),banco_dados_teste[,4]),positive = "1",mode = "everything") # Gerando a matriz de confusão
  
  arvore <- c("Árvore Binaria",k ,l,a$table[1,2], a$table[2,1],
             a$table[2,2], a$table[1,1],
             round(a$byClass[[11]],3), round(a$byClass[[1]],3),
             round(a$byClass[[5]],3), round(a$byClass[[7]],3)) # Criando vetor para depois adicionar a tabela
  
  med.testes <- rbind(med.testes, arvore)
  
  #Random Forest
  
  set.seed(2022)
  
  rf <- randomForest(as.factor(PLAYOFFS)~., data = banco_dados_trei[,-(1:3)], ntree = 600, mtry = 9, importance=T, proximity=T ) 
  p1 <- predict(rf, data = banco_dados_trei[,-(1:3)],type = 'class')
  
  #Mtry - Número de variáveis amostradas aleatoriamente como candidatas em cada divisão.
  
  roc.info <- roc(banco_dados_trei$PLAYOFFS, as.integer(p1), plot = F, legacy.axes=F, print.auc = F) # Obtendo as informações da curva roc
  
  AUC <- roc.info$auc[1]*100 # Salvando AUC
  
  roc.df <-  data.frame( # Gerando uma tabela com as informações da curva ROC
    mean = (roc.info$specificities*100 + roc.info$sensitivities*100)/2,
    dif = abs(roc.info$specificities*100 - roc.info$sensitivities),
    spec = roc.info$specificities*100,
    tpp = roc.info$sensitivities*100,
    fpp=(1 - roc.info$specificities)*100,
    thresholds = roc.info$thresholds
  )
  
  dif = 100
  
  for (i in 1:nrow(roc.df)){ # Definindo o ponto de corte
    modulo <- abs(roc.df[i,1] - AUC)
    if (modulo < dif){
      dif <- modulo
      corte <- roc.df[i,6]
    }
    
  }
  
  previsoes_modelo1 <- as.numeric(predict(rf,banco_dados_teste[,-(1:4)])) > corte # Fazendo as previsões do modelo
  
  
  
  
  a <- confusionMatrix(table(as.numeric(previsoes_modelo1),banco_dados_teste[,4]),positive = "1",mode = "everything") # Gerando a matriz de confusão
  
  randomforest <- c("Random Forest",k ,l, a$table[1,2], a$table[2,1],
                    a$table[2,2], a$table[1,1],
                    round(a$byClass[[11]],3), round(a$byClass[[1]],3),
                    round(a$byClass[[5]],3), round(a$byClass[[7]],3)) # Criando vetor para depois adicionar a tabela
  
  med.testes <- rbind(med.testes, randomforest)
  
  # Ridge regression
  
  # Usa cv.glm eria melhor usar a validação cruzada para escolher o parâmetro de ajuste λ . Podemos fazer isso usando a função de validação cruzada integrada, cv.glmnet(). Por padrão, a função executa validação cruzada de 10 vezes, embora isso possa ser alterado usando o argumento folds.
  grid = 10^seq(10, -2, length = 100) # Definindo o grid
  ridge_mod = glmnet(x.t, y.t, alpha = 0, lambda = grid)
  
  RR <-  cv.glmnet(x.t,y.t, alpha = 0, family = binomial(link='logit') ) # Deviance?
  melhor_lambda <- RR$lambda.min
  
  RRP <- predict(ridge_mod, s=melhor_lambda, newx = x.t, type = 'response') # valores preditos
  
  roc.info <- roc(banco_dados_trei$PLAYOFFS, RRP, plot = F, legacy.axes=F, print.auc = F) # Obtendo as informações da curva roc
  
  AUC <- roc.info$auc[1]*100 # Salvando AUC
  
  roc.df <-  data.frame( # Gerando uma tabela com as informações da curva ROC
    mean = (roc.info$specificities*100 + roc.info$sensitivities*100)/2,
    dif = abs(roc.info$specificities*100 - roc.info$sensitivities),
    spec = roc.info$specificities*100,
    tpp = roc.info$sensitivities*100,
    fpp=(1 - roc.info$specificities)*100,
    thresholds = roc.info$thresholds
  )
  
  dif = 100
  
  for (i in 1:nrow(roc.df)){ # Definindo o ponto de corte
    modulo <- abs(roc.df[i,1] - AUC)
    if (modulo < dif){
      dif <- modulo
      corte <- roc.df[i,6]
    }
    
  }
  
  previsoes_modelo2 <- predict(ridge_mod, s = melhor_lambda, newx = x.teste) > corte # Fazendo as previsões do modelo
  
  
  a <-  confusionMatrix(table(as.numeric(previsoes_modelo2),banco_dados_teste[,4]), positive = "1", mode = "everything") # Gerando a matriz de confusão
  
  med_ridge <- c("Ridge",k ,l, a$table[1,2], a$table[2,1],
                    a$table[2,2], a$table[1,1],
                 round(a$byClass[[11]],3), round(a$byClass[[1]],3),
                 round(a$byClass[[5]],3), round(a$byClass[[7]],3)) # Criando vetor para depois adicionar a tabela
  
  med.testes <- rbind(med.testes, med_ridge)
  #Lasso
  
  
  lasso_mod = glmnet(x.t, y.t, alpha = 1, lambda = grid) # Definindo o grid
  
  
  lasso = cv.glmnet(x.t, y.t, alpha = 1) # Fit lasso model on training data
  melhor_lambda_lasso = lasso$lambda.min # Select lamda that minimizes training MSE
  
  lassoP = predict(lasso_mod, s = melhor_lambda_lasso, newx = x.t,type = 'response') # Use best lambda to predict test data
  
  roc.info <- roc(banco_dados_trei$PLAYOFFS, lassoP, plot = F, legacy.axes=F, print.auc = F) # Obtendo as informações da curva roc
  
  AUC <- roc.info$auc[1]*100 # Salvando AUC
  
  roc.df <-  data.frame( # Gerando uma tabela com as informações da curva ROC
    mean = (roc.info$specificities*100 + roc.info$sensitivities*100)/2,
    dif = abs(roc.info$specificities*100 - roc.info$sensitivities),
    spec = roc.info$specificities*100,
    tpp = roc.info$sensitivities*100,
    fpp=(1 - roc.info$specificities)*100,
    thresholds = roc.info$thresholds
  )
  
  dif = 100
  
  for (i in 1:nrow(roc.df)){ # Definindo o ponto de corte
    modulo <- abs(roc.df[i,1] - AUC)
    if (modulo < dif){
      dif <- modulo
      corte <- roc.df[i,6]
    }
    
  }
  
  previsoes_modelo2 <- predict(lasso_mod, s = melhor_lambda_lasso, newx = x.teste) > corte # Fazendo as previsões do modelo
  
  a <- confusionMatrix(table(as.numeric(previsoes_modelo2),banco_dados_teste[,4]), positive = "1", mode = "everything") # Gerando a matriz de confusão
  
  med_lasso <- c("Lasso", k ,l,a$table[1,2], a$table[2,1],
                 a$table[2,2], a$table[1,1],
                 round(a$byClass[[11]],3), round(a$byClass[[1]],3),
                 round(a$byClass[[5]],3), round(a$byClass[[7]],3)) # Criando vetor para depois adicionar a tabela
  
  
  med.testes <- rbind(med.testes, med_lasso) #Gerando tabela com os resultados dos modelos
  #med.final <- append(med.final, med.testes )
  
    }
  }
  
  colnames(med.testes) <- c("Técnica","Jogos","Média Móvel","Falso Negativo", "Falso Positivo", "Verdadeiro Positivo",
                            "Verdadeiro Negativo", "Acurácia Balanceada", "Sensitividade", "Precisão", "F1")
  
  return(med.testes)
}

mel <- medidas(1)
mel1 <- medidas(1)

mel_teste <- medidas(1)
