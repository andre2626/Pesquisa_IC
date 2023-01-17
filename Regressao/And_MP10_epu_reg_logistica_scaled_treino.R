# EPU - Regressao Logistica com vari?veis normalizadas
# Separa??o treino/teste
# Vari?vel alvo: epu_mp10 (0/1)
# Vari?veis preditoras: m?s e vari?veis meteorol?gicas

# Refer?ncias:
# https://smolski.github.io/livroavancado/reglog.html#o-modelo
# https://www.rpubs.com/dudubiologico/545528
# https://www.scielo.br/j/rsocp/a/RWjPthhKDYbFQYydbDr3MgH/?lang=pt
# https://docs.ufpr.br/~taconeli/CE225/Aula14.pdf
# https://towardsdatascience.com/effective-feature-selection-recursive-feature-elimination-using-r-148ff998e4f7

# Bibliotecas utilizadas
library(dplyr)
library(DataExplorer)
library(caret)

##########################################################################a

# STemptest=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/MedPol_CLP_VMet.CSV",header=T)
# EPUclasmp10test=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/epu_mp10_classific.CSV",header=T)


# STemptest <-STemptest[ ,-c(14:20)]

# STemptest$data<-EPUclasmp10test$EPUs.MP10
# colnames(STemptest)[1]="EPUs.MP10"
# colnames(EPUclasmp10test)[6]="epu_mp10"

 setwd("G:/Meu Drive/4 IC/1 Projeto/Reuniões/15Correlacao/Dados")
# write.csv(STemptest, file = 'meteo_IAG_PBL_2005-2021.csv', row.names = FALSE)
# write.csv(EPUclasmp10test, file = 'epu_mp10_classific.csv', row.names = FALSE)




# Le e organiza os dados de entrada
epu <- read.csv('epu_mp10_classific.csv') 
str(epu)
met <- read.csv('meteo_IAG_PBL_2005-2021.csv')
str(met)

rem <- which(is.na(met$VelocVento)) # ha 5 dados faltantes de velocidade do vento
met <- met[-rem,] # exclui as linhas com NA
epu <- epu[-rem,] # exclui as linhas com NA
dados <- met
dados[,1] <- epu$epu_mp10
colnames(dados)[1] <- 'epump'

# Cria uma coluna para a estacao do ano
# DJF: verao: Summer  | Dez,Jan e Fev
# MAM: outuno:Autumn  | Março, Abril e Maio
# JJA: inverno: Winter  | Junho, Julho e Agosto
# SON: primavera: Spring  |  Setembro, Outubro e Novembro

dados$est <- as.factor(
  ifelse(dados$m==12| dados$m<=2,"ASUM",
         ifelse(dados$m>=3 & dados$m<=5, "FAL",
                ifelse(dados$m>=6 & dados$m<=8, "WIN", "SPR"))))

# Converte variaveis categoricas para factor (pacote dplyr)
str(dados)
dados <- dados %>%  mutate_at(c("epump", "m", "DirVento"), as.factor)  #factor = v. categorica (tem classes)
str(dados)



# Normaliza as variaveis numericas ########
## Normalizar ou Não, não modificam a matrix de confusão (Acurácia não muda)

?mutate_if
?scale
dados <- dados %>%  mutate_if(is.numeric, scale) #escalonamento/ normalização / padronização
dados <- dados %>%  mutate_if(is.numeric, as.numeric)

# existem várias formas de Normalização (usamos a scale)
## célula da variavel - média da coluna da variavel / desvio padrão da variavel
## slide 94 10_Estatistica (ex do o quanto um hamburger é + calorico em relação a média)

plot(met$TempMAX) # TempMAX sem normalizar
plot(dados$TempMAX) # TempMAX COM normalizacao

hist(met$TempMAX)
hist(dados$TempMAX)




# Exploracao inicial dos dados (pacote DataExplorer)
# Vemos que epump_1 correlaciona com UR, VelocVento, Insolcacao, etc.
# Espera-se que essas vari?veis sejam importantes no modelo de regress?o que vamos construir.
?plot_intro
plot_intro(dados)
plot_bar(dados)
plot_correlation(dados)

# x variável independente/ preditoras: v. q "tenho controle"
# y variável DEpendente/ alvo: v. q " NÂO tenho controle"

# Variaveis preditoras:
x <- dados[,-1]
# Variavel alvo:
y <- dados$epump

# Separando conjuntos de treino (80%) e de teste (20%)
?createDataPartition
?set.seed # gera uma combinação aleatória, nesse caso foi a "combinação 2021" 
          # set.seed possibilita a reprodutibilidade dos dados

set.seed(2021)
inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]
head(inTrain) # mostra o inicio das linhas q serão usadas para o treinamento

x_train <- x[ inTrain, ] # x[ inTrain, ]: olhando p variavel x, todas as linhas correspondentes ao "inTrain", todas as colunas #80% 
x_test  <- x[-inTrain, ]# x[-inTrain,]: olhando p variavel x, todas as linhas MENOS as correspondentes ao "inTrain", todas as colunas # 20%
head(x_test)
head(x_train)

y_train <- y[ inTrain]
y_test  <- y[-inTrain]


treino <- cbind(y_train,x_train) # juntando as colunas de treino
colnames(treino)[1] <- 'epump' 

teste <- cbind(y_test,x_test) # juntando as colunas de TESTE
colnames(teste)[1] <- 'epump'



# modelagem #####

##########################################################################a

# modelo1: regressao logistica com todas as variaveis preditoras
# As seguintes variaveis tiveram coeficientes significativos: 
# est, ustar, VV, DV-ENE-ESE-S, Irrad, Precip, Pressao, TempMED, TempMAX, UR 
# Variaveis com coeficientes positivos contribuem para o aumento da chance de epump=1.
# coeficientes ####

# epump: col do data frame "teste"(20% dos dados) e "dados"(100% dos dados) 
str(teste)
## epump: factor (v. categórica)
## m: (v. categórica)
## est: (v. categórica)
## DirVento: (v. categórica)


### No modelo glm, quando se tem v. categóricas o valor do
### coeficiente de correlação é sempre em relação a PRIMEIRA CATEGORIA
?glm
modelo1 <- glm(formula = epump ~
                 #m +   # o coef. do mes 2 (ou 3,4...) é em relação ao mes 1
                 est +  # o coef. da est WIN (inver,) (ou outra) é em relação a est ASUM (ver.)
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
               data = treino) # dados #treino
summary(modelo1) # AIC: 2697.6 : quanto menor, melhor

#p > 0,05 : falha em rejeitar a H0 ("aceita H0")
# nosso caso # p < 0,05 : rejeitar a H0 ("aceita HA")

#H0: Não há significancia 
#HA: Há significancia 
# significancia ***: confiança de 95% (*90% 99%), de que um dado acontecimento (correlação) não se 
# tenha produzido devido ao acaso.


####################################################### a
# https://www.youtube.com/watch?v=bXroz0_Xpzc
# https://pt.quora.com/Qual-%C3%A9-a-diferen%C3%A7a-entre-probabilidade-e-chance
###############   Probabilidade

# Probabilidade: numero entre 0 e 1
# Probabilidade é a medida da chance de um evento ocorrer.

#Em uma urna há 100 bolas, onde 10 delas são azuis e as outras 90 são vermelhas.
#Desta forma, se sortearmos uma bola ao acaso desta urna, 
#temos que a probabilidade de sortearmos uma bola azul é dada por:

#p = N° de bolas azuis / Total de bolas
#p = 10/100
#p= 0,1

#Portanto, a probabilidade de se sortear uma bola azul é 0,1. 



###################   Chance

#chance é  a probabilidade de ocorrência de determinado evento sob
#a probabilidade da não ocorrência do evento.

#Considerando o exemplo exposto anteriormente, temos que a chance de se sortear uma bola vermelha é de:
  
#  Neste caso, p = 0,9
#c = p/(1-p)
#c = 0,9/(1–0,9)
#c = 0,9/0,1
#c = 9

# interpretamos este número como "As chances de obtermos uma bola vermelha é de 9 para cada bola azul"
#"As chances de obtermos uma bola vermelha é 9 vezes maior q 1 bola azul"

#######################################################a 




# Razao de chances (odds ratio) ####
# usado em modelo de regressão logística

## (odds ratio) OR: 3.4699 para a v.TempMAX (normalizada)
## signif. que a CHANCE de haver EPUmp são de 3.4699 VEZES

# ref: https://smolski.github.io/livroavancado/reglog.html#o-modelo  | 7.2.1 Estimando a Razão de Chances

exp(modelo1$coefficients[2]) 
exp(cbind(OR = coef(modelo1), confint(modelo1))) #Intervalo de Confiança

## se a v.TempMAX (normalizada) aumentar 1 (unidade), a CHANCE de haver EPUmp são de 2,46994% (3,46994 vezes) ((3,46994-1)*100)
## Nesse caso,  1 (unidade) é 1 DESVIO PADRÃO, pq as variaveis estao NORMALIZADAS

?sd # DESVIO PADRÃO: mede o quanto os dados variam em relação a media
#(o quao distante os dados estao do valor da media)

## o DES. P do resumo foi calculado a partir dos dados SEM NORMALIZAR !!!!!
# data frame  "met" deve ser SEM NORMALIZAR
sd(met$ustar.m.s.)  # 1 sd da v.ustar.m.s. é 0,105 m/s em relação a media
sd(met$TempMED)
sd(met$TempMAX) # 1 sd da v.TempMAX é 4,6 graus Celsius em relação a media
mean(met$TempMAX)

#View(met)
## se a v.TempMAX (normalizada) aumentar 1 (unidade), a CHANCE de haver EPUmp são de 246,994% (3,46994 vezes) ((3,46994-1)*100)
## Nesse caso,  1 (unidade) é 1 DESVIO PADRÃO, pq as variaveis estao NORMALIZADAS

##
###
#### resposta f: se a v.TempMAX aumentar 4,6 °C, a CHANCE de haver EPUmp aumenta em 246,994%
### resposta f: se a v.ustar.m.s. aumentar 0,105 m/s, a CHANCE de haver EPUmp CAI em 21,30% (0.78699640 vezes) ((0.78699640-1)*100)


## Dica: caso faça o odds com a v.TempMAX SEM NORMALIZAR, o OR vai variar em relação a 1 unidade, que será 1°C
plot(met$TempMAX)

## Dica: caso faça o odds com a v.ustar.m.s. SEM NORMALIZAR, o OR vai variar em relação a 1 unidade, que será 1 m/s
plot(met$ustar.m.s.) # para a v.ustar.m.s. isso não é Realista, pq a unidade dessa variável não varia tanto assim !!!



# Razao de chances outra forma (odds ratio)
#install.packages("mfx")
library("mfx")
logitor(epump~est +
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
          UR,data =treino)



###############################################################  Matriz de confusao

# Matriz de confusao - dados de treinamento (pacote caret) ####
## qualificar o ajuste do modelo de regressão logística

# https://topepo.github.io/caret/measuring-performance.html#measures-for-predicted-classes

# Avalia o desempenho do modelo em classificar os "eventos" e os "nao eventos". 
# Vamos adotar o PONTO de CORTE: se probab>0.50, vamos dizer que o modelo previu um evento.

probab1 <- predict(modelo1, newdata=x_train, type='response')
hist(probab1)
   #predict: 7.2.3 Predição de Probabilidades

eventoprevisto <- as.factor(ifelse(probab1>=0.5,"1","0"))
confusionMatrix(eventoprevisto, y_train, positive="1")
# Acuracia: 89%. Sensibilidade (acerto de eventos): 52%. Especificidade (acerto de nao-eventos): 96%. 


## Tudo junto
# eventoprevisto <- as.factor(ifelse(predict(modelo1, newdata=x_train, type='response')>=0.5,"1","0"))
# confusionMatrix(eventoprevisto, y_train, positive="1")



###########################################################a Curva ROC
# Curva ROC ####
# Curva ROC: graf classifica a qualidade do modelo

# AUC: indicador da qualidade do modelo (MAIOR = melhor), te da o maior valor de SENSIBILIDADE e ESPECIFICIDADE
#indica  qual é o melhor PONTO de CORTE para o modelo de regressão logística


# https://docs.ufpr.br/~taconeli/CE225/Aula14.pdf
# https://rpubs.com/Wangzf/pROC

# Cada valor de p (ponto de corte) resulta em uma regra de classificacao distinta. Para escolher
# o melhor valor de p, é usual estimar a sensibilidade e a especificidade do modelo para diferentes valores de p. 

# A curva ROC é um grafico de Especificidade X Sensibilidade para diferentes valores de p, cujos
# valores numericos sao escritos no grafico. O melhor valor de p é aquele que resulta em alta
# sensibilidade e alta especificidade (canto superior esquerdo da curva ROC).

# A area sob a curva ROC (chamada de AUC) indica a qualidade preditiva do modelo (de 0 a 1).

library(pROC)

probab1 <- predict(modelo1, newdata=x_test, type='response')

roc1 <- roc(y_test,probab1)
plot.roc(y_test,probab1,
         print.auc=TRUE, # mostra o AUC no grafico
         ci=TRUE, of="thresholds", 
         thresholds="best", # seleciona o melhor threshold (p, ponto de corte)
         print.thres="best") # mostra o melhor threshold

melhorp <- coords(roc1, x="best", ret="threshold", transpose = FALSE)

#oq a prof colocou: Curva ROC: AUC=0.903, melhor p (ponto de corte)= 0.089 
## tudo com propabilidade > 0.089 o modelo vai classificar como Evento

# Curva ROC: AUC=0.912, melhor p (ponto de corte)= 0.141 
## tudo com propabilidade > 0.141 o modelo vai classificar como Evento

plot(probab1)




## Matriz de confusao - dados de teste, usando o melhor threshold (p) obtido pela curva ROC

# Teste com ponto de corte (threshold ou p) igual a 0.5. Se probab>0.5, o modelo previu um evento.
probab1 <- predict(modelo1, newdata=x_test, type='response')
eventoprevisto <- as.factor(ifelse(probab1>=0.5,"1","0"))
confusionMatrix(eventoprevisto, y_test, positive="1")

# Acuracia: 88%. Sensibilidade (acerto de eventos): 50%. Especificidade (acerto de n?o-eventos): 95%.



## Teste com ponto de corte (threshold ou p) igual a 0.089 (obtido pela curva ROC)
eventoprevisto <- as.factor(ifelse(probab1>=0.089,"1","0"))
confusionMatrix(eventoprevisto, y_test, positive="1")

# Acuracia: 80%. Sensibilidade (acerto de eventos): 92%. Especificidade (acerto de n?o-eventos): 75%.


# Testando e verificando, para entender:
testando<-data.frame(obs=as.numeric(as.character(y_test)),mod=probab1)
sum(testando[testando$mod<0.089,1]) # quantos eventos verdadeiros o modelo errou com probab<threshold?
sum(testando[testando$mod>=0.089,1]) # quantos eventos verdadeiros o modelo acertou com probab<threshold?





# Stepwise ####
##########################################################################a

# Stepwise feature selection
# A funcao step ajusta o modelo de regressao varias vezes, com diversas combinaaces
# de variaveis preditoras. Ele indica qual a a combinacao de variaveis que resulta
# no melhor modelo (aquele que tem melhor AIC).
step(modelo1, direction="both")
# Resultado: o modelo melhora (AIC diminui) se as variaveis DirVento, Insolacao e pblh forem removidas

##########################################################################a




# modelo2 ####
# modelo2: tirando DirVento, Insolacao e pblh
# est, ustar, VelocVento, Irradiacao, Precipitacao, TempMED, TempMAX, UR 
modelo2 <- glm(formula = epump ~
                 #m +
                 est +
                 #pblh.m. +
                 ustar.m.s. +
                 VelocVento +
                 #DirVento +
                 #Insolacao +
                 Irradiacao +
                 Precipitacao +
                 PressaoMED +  ####
                 TempMED +
                 TempMAX +
                 UR, 
               family = binomial(link = "logit"), 
               data = treino) #treino #dados
summary(modelo2)  # AIC: 2699.2

# oq a prof colocou: AIC: 3189.2 (o AIC aumentou do modelo1 para o modelo2 ??)



### Razao de chances (odds ratio) modelo 2 ####
exp(cbind(OR = coef(modelo2), confint(modelo2)))




### Curva ROC modelo 2 #### 

# Curva ROC:mensura a capcidade de predição do modelo proposto,
#Especificidade(acerto de nao-eventos) X Sensibilidade(acerto de eventos)

# AUC: indicador representa aprobabilidade de que o classificador efetue predições 
#randômicas na instância positiva melhordo que na instância negativa.

probab2 <- predict(modelo2, newdata=x_test, type='response')
plot(probab2)

roc2 <- roc(y_test,probab2)

plot.roc(y_test,probab2,
         print.auc=TRUE, # mostra o AUC no grafico
         ci=TRUE, of="thresholds", 
         thresholds="best", # seleciona o melhor threshold (p, ponto de corte)
         print.thres="best") # mostra o melhor threshold

# Curva ROC: AUC=0.910, melhorp=0.107

melhorp <- coords(roc2, x="best", ret="threshold", transpose = FALSE)




### Matriz de confusao modelo 2 ####

# Matriz de confusao - dados de teste, usando o melhor threshold (p=0.107) obtido pela curva ROC
eventoprevisto <- as.factor(ifelse(probab2>=0.107,"1","0"))

confusionMatrix(eventoprevisto, y_test, positive="1")
# Acurucia: 80%. Sensibilidade (acerto de eventos): 90%. Especificidade (acerto de nao-eventos): 78%.


##########################################################################a 




# modelo3: tirando DirVento e pblh  ####
# est, ustar, VelocVento, Insolacao, Irradiacao, Precipitacao, TempMED, TempMAX, UR 
modelo3 <- glm(formula = epump ~
                 est +
                 #pblh.m. +
                 ustar.m.s. +
                 VelocVento +
                 #DirVento +
                 Insolacao +
                 Irradiacao +
                 Precipitacao +
                 PressaoMED +  ####
                 TempMED +
                 TempMAX +
                 UR, 
               family = binomial(link = "logit"), 
               data = treino) #treino #dados
summary(modelo3) # AIC: 2682.2


### Razao de chances (odds ratio) modelo 3 ####
exp(cbind(OR = coef(modelo3), confint(modelo3)))

## o DES. P do resumo foi calculado a partir dos dados SEM NORMALIZAR !!!!!
# data frame  "met" deve ser SEM NORMALIZAR
sd(met$ustar.m.s.)  # 1 sd da v.ustar.m.s. é 0,105 m/s em relação a media
sd(met$TempMED)
sd(met$TempMAX) # 1 sd da v.TempMAX é 4,6 graus Celsius em relação a media
mean(met$TempMAX)



### Curva ROC modelo 3 #### 

# Curva ROC:mensura a capcidade de predição do modelo proposto,
#Especificidade(acerto de nao-eventos) X Sensibilidade(acerto de eventos)

# AUC: indicador representa a probabilidade de que o classificador efetue predições 
#randômicas na instância positiva melhordo que na instância negativa.

probab3 <- predict(modelo3, newdata=x_test, type='response')
plot(probab3)

roc3 <- roc(y_test,probab3)

plot.roc(y_test,probab3,
         print.auc=TRUE, # mostra o AUC no grafico
         ci=TRUE, of="thresholds", 
         thresholds="best", # seleciona o melhor threshold (p, ponto de corte)
         print.thres="best") # mostra o melhor threshold

# Curva ROC: AUC=0.914, melhorp=0.140

melhorp <- coords(roc3, x="best", ret="threshold", transpose = FALSE)



### Matriz de confusao modelo 3 ####

# Matriz de confusao - dados de teste, usando o melhor threshold (p=0.140) obtido pela curva ROC
eventoprevisto <- as.factor(ifelse(probab3>=0.140,"1","0"))

confusionMatrix(eventoprevisto, y_test, positive="1")
# Acurucia: 0.8217 (82,17%)
# Sensibilidade (acerto de eventos): 0.8738 (87,38%)
# Especificidade (acerto de nao-eventos):0.8113 (81,13%)

##########################################################################a 







