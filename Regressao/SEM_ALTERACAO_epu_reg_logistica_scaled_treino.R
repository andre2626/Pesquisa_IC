# EPU - Regressao Logistica com vari�veis normalizadas
# Separa��o treino/teste
# Vari�vel alvo: epu_mp10 (0/1)
# Vari�veis preditoras: m�s e vari�veis meteorol�gicas

# Refer�ncias:
# https://smolski.github.io/livroavancado/reglog.html#o-modelo
# https://www.rpubs.com/dudubiologico/545528
# https://www.scielo.br/j/rsocp/a/RWjPthhKDYbFQYydbDr3MgH/?lang=pt
# https://docs.ufpr.br/~taconeli/CE225/Aula14.pdf
# https://towardsdatascience.com/effective-feature-selection-recursive-feature-elimination-using-r-148ff998e4f7

# Bibliotecas utilizadas
library(dplyr)
library(DataExplorer)
library(caret)

##########################################################################
# L� e organiza os dados de entrada
epu <- read.csv('epu_mp10_classific.csv') 
met <- read.csv('meteo_IAG_PBL_2005-2021.csv')
rem <- which(is.na(met$VelocVento)) # h� 5 dados faltantes de velocidade do vento
met <- met[-rem,] # exclui as linhas com NA
epu <- epu[-rem,] # exclui as linhas com NA
dados <- met
dados[,1] <- epu$epu_mp10
colnames(dados)[1] <- 'epump'

# Cria uma coluna para a esta��o do ano
# DJF: ver�o, MAM: outuno, JJA: inverno, SON: primavera
dados$est <- as.factor(
  ifelse(dados$m==12| dados$m<=2,"ASUM",
         ifelse(dados$m>=3 & dados$m<=5, "FAL",
                ifelse(dados$m>=6 & dados$m<=8, "WIN", "SPR"))))

# Converte vari�veis categ�ricas para factor (pacote dplyr)
dados <- dados %>%  mutate_at(c("epump", "m", "DirVento"), as.factor)

# Normaliza as vari�veis num�ricas
dados <- dados %>%  mutate_if(is.numeric, scale)
dados <- dados %>%  mutate_if(is.numeric, as.numeric)

# Explora��o inicial dos dados (pacote DataExplorer)
# Vemos que epump_1 correlaciona com UR, VelocVento, Insolca��o, etc.
# Espera-se que essas vari�veis sejam importantes no modelo de regress�o que vamos construir.
plot_intro(dados)
plot_bar(dados)
plot_correlation(dados)

# Vari�veis preditoras:
x <- dados[,-1]
# Vari�vel alvo:
y <- dados$epump

# Separando conjuntos de treino (80%) e de teste (20%)
set.seed(2021)
inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]
x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]
y_train <- y[ inTrain]
y_test  <- y[-inTrain]
treino <- cbind(y_train,x_train)
colnames(treino)[1] <- 'epump'
teste <- cbind(y_test,x_test)
colnames(teste)[1] <- 'epump'

##########################################################################
# modelo1: regress�o log�stica com todas as vari�veis preditoras
# As seguintes vari�veis tiveram coeficientes significativos: 
# est, ustar, VV, DV-ENE-ESE-S, Irrad, Precip, Pressao, TempMED, TempMAX, UR 
# Vari�veis com coeficientes positivos contribuem para o aumento da chance de epump=1.
modelo1 <- glm(formula = epump ~
                 est +
                 pblh.m. +
                 ustar.m.s. +
                 VelocVento +
                 DirVento +
                 Insolacao +
                 Irradiacao +
                 Precipitacao +
                 PressaoMED +
                 TempMED +
                 TempMAX +
                 UR, 
               family = binomial(link = "logit"), 
               data = treino)
summary(modelo1) # AIC: 2548

# Raz�o de chances (odds ratio)
exp(modelo1$coefficients)
exp(cbind(OR = coef(modelo1), confint(modelo1)))

# Matriz de confus�o - dados de treinamento (pacote caret)
# https://topepo.github.io/caret/measuring-performance.html#measures-for-predicted-classes
# Avalia o desempenho do modelo em classificar os "eventos" e os "n�o eventos". 
# Vamos adotar o ponto de corte: se probab>0.50, vamos dizer que o modelo previu um evento.
probab1 <- predict(modelo1, newdata=x_train, type='response')
hist(probab1)
eventoprevisto <- as.factor(ifelse(probab1>=0.5,"1","0"))
confusionMatrix(eventoprevisto, y_train, positive="1")
# Acur�cia: 89%. Sensibilidade (acerto de eventos): 52%. Especificidade (acerto de n�o-eventos): 96%. 

# Curva ROC: AUC=0.903, melhorp=0.089
# https://docs.ufpr.br/~taconeli/CE225/Aula14.pdf
# https://rpubs.com/Wangzf/pROC
# Cada valor de p (ponto de corte) resulta em uma regra de classifica��o distinta. Para escolher
# o melhor valor de p, � usual estimar a sensibilidade e a especificidade do modelo para diferentes valores de p. 
# A curva ROC � um gr�fico de Especificidade X Sensibilidade para diferentes valores de p, cujos
# valores num�ricos s�o escritos no gr�fico. O melhor valor de p � aquele que resulta em alta
# sensibilidade e alta especificidade (canto superior esquerdo da curva ROC).
# A �rea sob a curva ROC (chamada de AUC) indica a qualidade preditiva do modelo (de 0 a 1).
library(pROC)
probab1 <- predict(modelo1, newdata=x_test, type='response')
roc1 <- roc(y_test,probab1)
plot.roc(y_test,probab1,
         print.auc=TRUE, # mostra o AUC no gr�fico
         ci=TRUE, of="thresholds", 
         thresholds="best", # seleciona o melhor threshold (p, ponto de corte)
         print.thres="best") # mostra o melhor threshold
melhorp <- coords(roc1, x="best", ret="threshold", transpose = FALSE)

# Matriz de confus�o - dados de teste, usando o melhor threshold (p) obtido pela curva ROC
# Teste com ponto de corte (threshold ou p) igual a 0.5. Se probab>0.5, o modelo previu um evento.
probab1 <- predict(modelo1, newdata=x_test, type='response')
eventoprevisto <- as.factor(ifelse(probab1>=0.5,"1","0"))
confusionMatrix(eventoprevisto, y_test, positive="1")
# Acur�cia: 88%. Sensibilidade (acerto de eventos): 50%. Especificidade (acerto de n�o-eventos): 95%.
# Teste com ponto de corte (threshold ou p) igual a 0.089 (obtido pela curva ROC)
eventoprevisto <- as.factor(ifelse(probab1>=0.089,"1","0"))
confusionMatrix(eventoprevisto, y_test, positive="1")
# Acur�cia: 80%. Sensibilidade (acerto de eventos): 92%. Especificidade (acerto de n�o-eventos): 75%.
# Testando e verificando, para entender:
testando<-data.frame(obs=as.numeric(as.character(y_test)),mod=probab1)
sum(testando[testando$mod<0.089,1]) # quantos eventos verdadeiros o modelo errou com probab<threshold?
sum(testando[testando$mod>=0.089,1]) # quantos eventos verdadeiros o modelo acertou com probab<threshold?

##########################################################################
# Stepwise feature selection
# A fun��o step ajusta o modelo de regress�o v�rias vezes, com diversas combina��es
# de vari�veis preditoras. Ele indica qual � a combina��o de vari�veis que resulta
# no melhor modelo (aquele que tem melhor AIC).
step(modelo1, direction="both")
# Resultado: o modelo melhora (AIC diminui) se as vari�veis DirVento, Insolacao e pblh forem removidas

##########################################################################
# modelo2: tirando DirVento, Insolacao e pblh
# est, ustar, VelocVento, Irradiacao, Precipitacao, TempMED, TempMAX, UR 
modelo2 <- glm(formula = epump ~
                 est +
                 #pblh.m. +
                 ustar.m.s. +
                 VelocVento +
                 #DirVento +
                 #Insolacao +
                 Irradiacao +
                 Precipitacao +
                 PressaoMED +
                 TempMED +
                 TempMAX +
                 UR, 
               family = binomial(link = "logit"), 
               data = dados)
summary(modelo2) # AIC: 3189.2 (o AIC aumentou do modelo1 para o modelo2 ??)

# Raz�o de chances (odds ratio)
exp(cbind(OR = coef(modelo2), confint(modelo2)))

# Curva ROC: AUC=0.910, melhorp=0.107
probab2 <- predict(modelo2, newdata=x_test, type='response')
roc2 <- roc(y_test,probab2)
plot.roc(y_test,probab2,
         print.auc=TRUE, # mostra o AUC no gr�fico
         ci=TRUE, of="thresholds", 
         thresholds="best", # seleciona o melhor threshold (p, ponto de corte)
         print.thres="best") # mostra o melhor threshold
melhorp <- coords(roc2, x="best", ret="threshold", transpose = FALSE)

# Matriz de confus�o - dados de teste, usando o melhor threshold (p=0.107) obtido pela curva ROC
eventoprevisto <- as.factor(ifelse(probab2>=0.107,"1","0"))
confusionMatrix(eventoprevisto, y_test, positive="1")
# Acur�cia: 80%. Sensibilidade (acerto de eventos): 90%. Especificidade (acerto de n�o-eventos): 78%.

########################################################################## 

