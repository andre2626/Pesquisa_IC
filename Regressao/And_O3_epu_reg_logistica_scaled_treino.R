# EPU - Regressao Logistica com vari?veis normalizadas
# Separa??o treino/teste
# Vari?vel alvo: epu_o3 (0/1)
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
library(pROC)
library(car)
##########################################################################a

 STemptest=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/MedPol_CLP_VMet.CSV",header=T)
# EPUclaso3test=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/epu_o3_classific.CSV",header=T)


# STemptest <-STemptest[ ,-c(14:20)]

# STemptest$data<-EPUclaso3test$EPUs.O3
# colnames(STemptest)[1]="EPUs.O3"
# colnames(EPUclaso3test)[6]="epu_o3"

 setwd("G:/Meu Drive/4 IC/1 Projeto/Reuniões/15Correlacao/Dados")
# write.csv(STemptest, file = 'meteo_IAG_PBL_2005-2021.csv', row.names = FALSE)
# write.csv(EPUclaso3test, file = 'epu_o3_classific.csv', row.names = FALSE)




# Le e organiza os dados de entrada
epu <- read.csv('epu_o3_classific.csv') 
str(epu)
met <- read.csv('meteo_IAG_PBL_2005-2021.csv')
str(met)

rem <- which(is.na(met$VelocVento)) # ha 5 dados faltantes de velocidade do vento
rem1 <-which(is.na(met$Insolacao))
rem2 <-which(is.na(met$Irradiacao))

met <- met[-c(rem,rem1,rem2),] # exclui as linhas com NA
epu <- epu[-c(rem,rem1,rem2),] # exclui as linhas com NA

dados <- met
dados[,1] <- epu$epu_o3
colnames(dados)[1] <- 'epump'

# Cria uma coluna para a estacao do ano
# DJF: verao: Summer  | Dez,Jan e Fev
# MAM: outuno:Autumn  | Março, Abril e Maio
# JJA: inverno: Winter  | Junho, Julho e Agosto
# SON: primavera: Spring  |  Setembro, Outubro e Novembro

# dados$est<-NA

# dados$est[dados$m == 12 |dados$m <=2] <- "SUM"
# dados$est[dados$m >= 3 & dados$m <=5] <- "FAL"
# dados$est[dados$m >= 6 & dados$m <=8] <- "WIN"
# dados$est[dados$m >= 9 & dados$m <=11] <- "SPR"

# dados$est <- as.factor(dados$est)

dados$DirVento[dados$DirVento == "NNE"] <- "N"
dados$DirVento[dados$DirVento == "ENE"] <- "NE"
dados$DirVento[dados$DirVento == "ESE"] <- "E"
dados$DirVento[dados$DirVento == "SSE"] <- "SE"
dados$DirVento[dados$DirVento == "SSW"] <- "S"
dados$DirVento[dados$DirVento == "WSW"] <- "SW"
dados$DirVento[dados$DirVento == "WNW"] <- "W"
dados$DirVento[dados$DirVento == "NNW"] <- "NW"


 dados$est <- as.factor(
   ifelse(dados$m==12| dados$m<=2,"SUM_v",
          ifelse(dados$m>=3 & dados$m<=5, "FAL_o",
                 ifelse(dados$m>=6 & dados$m<=8, "aWIN_i", "SPR_p"))))

 
#Exclui os dados do INVERNO
 
 dados %>% select(est) %>% filter(est =="aWIN_i") %>% nrow() #para saber numero de linhas "aWIN_i" = 1555
 
 dados<-dados %>% filter(!est =="aWIN_i") # excluir linhas em que dados$est =="aWIN_i" (excluir linha com INVERNO)
 
 which(dados$m==6)  # conferir se as linhas "aWIN_i" foram excluidas, analisando os meses correspondentes 
 which(dados$m==7)
 which(dados$m==8)
 
 
 
 

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
plot_correlation(na.omit(dados))

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
modelo1 <- glm(formula = epump ~
                 #m +
                 est +
                 pblh.m. +
                 ustar.m.s. +
                 VelocVento +
                 DirVento +
                 Insolacao +
                 Irradiacao +
                 Precipitacao +
                 PressaoMED +  ####
                 TempMED +
                 TempMAX +
                 UR, 
               family = binomial(link = "logit"), 
               data = treino) #treino #dados
summary(modelo1) # AIC: 1126.7


vif(modelo1) # modelo 1 com todas as variaveis AIC:1126.7 

# modelo 1 com todas as variaveis AIC:1126.7 
exp(cbind(OR = coef(modelo1), confint(modelo1)))

# com todas as variaveis, recomenda tirar Precip e TempMED
step(modelo1, direction="both") 



# https://stacyderuiter.github.io/s245-notes-bookdown/collinearity-and-multicollinearity.html
# https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/Collinearity
# https://bookdown.org/sarahwerth2024/RegressionLabsBook/lab-7-r.html
# https://www.researchgate.net/post/Anyone_familiar_with_VIF_Variance_Inflation_Factor_and_categorical_variables

# https://www.researchgate.net/publication/236007265_Generalized_Collinearity_Diagnostics



# modelo2 ####
# modelo2:
modelo2 <- glm(formula = epump ~
                 # m +
                 est +
                 # pblh.m. + # Não significativa
                 ustar.m.s. +
                 VelocVento +
                 # DirVento +
                 #Insolacao +
                 Irradiacao +
                 # Precipitacao + # Não significativa
                 PressaoMED +  
                 # TempMED + # Não significativa
                 TempMAX +
                 UR, 
               family = binomial(link = "logit"), 
               data = treino) #treino #dados
summary(modelo2)  # AIC:1124.5



vif(modelo2) # modelo 2 sem pblh, Precipitacao e TempMED AIC:1124.5 

# modelo 2 sem pblh, Precipitacao e TempMED AIC:1124.5 
exp(cbind(OR = coef(modelo2), confint(modelo2)))

# Stepwise ####
##########################################################################a

# Stepwise feature selection
# A funcao step ajusta o modelo de regressao varias vezes, com diversas combinaaces
# de variaveis preditoras. Ele indica qual a a combinacao de variaveis que resulta
# no melhor modelo (aquele que tem melhor AIC).
step(modelo2, direction="both")

##########################################################################a





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

# Curva ROC: AUC=0.958, melhorp=0.072

melhorp <- coords(roc2, x="best", ret="threshold", transpose = FALSE)




### Matriz de confusao modelo 2 ####

# Matriz de confusao - dados de teste, usando o melhor threshold (p=0.107) obtido pela curva ROC
eventoprevisto <- as.factor(ifelse(probab2>=0.194,"1","0"))

confusionMatrix(eventoprevisto, y_test, positive="1")
# Acurucia: 89%. Sensibilidade (acerto de eventos): 89%. Especificidade (acerto de nao-eventos): 89%.


##########################################################################a 


















