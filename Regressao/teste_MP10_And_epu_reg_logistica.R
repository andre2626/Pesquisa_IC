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
library(pROC)
library(car)
##########################################################################a

# STemptest=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/MedPol_CLP_VMet.CSV",header=T)
# EPUclasmp10test=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/epu_mp10_classific.CSV",header=T)

#which(is.na(STemptest$VelocVento))


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

table(epu$epu_mp10)# com todos os dados(com os NA) 6209 = 100%, (não EPU 5176)0 = 83,4%, (sim EPU 1033)1 = 16,3%


rem <- which(is.na(met$VelocVento)) # ha 5 dados faltantes de velocidade do vento
rem1 <-which(is.na(met$Insolacao))
rem2 <-which(is.na(met$Irradiacao))

sum(is.na(met)) # vou perder 46 linhas de dados, 6209 - 49 = 6163

met <- met[-c(rem,rem1,rem2),] # exclui as linhas com NA
epu <- epu[-c(rem,rem1,rem2),] # exclui as linhas com NA

dados <- met
dados[,1] <- epu$epu_mp10
colnames(dados)[1] <- 'epump'


# Cria uma coluna para a estacao do ano
# DJF: verao: Summer  | Dez,Jan e Fev
# MAM: outuno: Autumn/ Fall  | Março, Abril e Maio
# JJA: inverno: Winter  | Junho, Julho e Agosto
# SON: primavera: Spring  |  Setembro, Outubro e Novembro

# dados$est<-NA

# dados$est[dados$m == 12 |dados$m <=2] <- "SUM"
# dados$est[dados$m >= 3 & dados$m <=5] <- "FAL"
# dados$est[dados$m >= 6 & dados$m <=8] <- "WIN"
# dados$est[dados$m >= 9 & dados$m <=11] <- "SPR"

# dados$est <- as.factor(dados$est)


dados$est <- as.factor(
  ifelse(dados$m==12| dados$m<=2,"ASUM_v",
         ifelse(dados$m>=3 & dados$m<=5, "FAL_o",
                ifelse(dados$m>=6 & dados$m<=8, "WIN_i", "aSPR_p"))))




dados %>% select(est) %>% filter(est =="ASUM_v") %>% nrow() #para saber numero de linhas "ASUM_v"

dados<-dados %>% filter(!est =="ASUM_v") # excluir linhas em que dados$est =="ASUM_v" (excluir linha com verao)
#dados<-dados %>% filter(!est =="FAL_o") # excluir linhas em que dados$est =="FAL_o" (excluir linha com verao)
#dados<-dados %>% filter(!est =="aSPR_p") # excluir linhas em que dados$est =="SPR_p" (excluir linha com verao)

# dados<-dados[,-14]



which(dados$m==1)  # conferir se as linhas "ASUM_v" foram excluidas, analisando os meses correspondentes 
which(dados$m==2)
which(dados$m==12)



str(dados$DirVento) # existe o level "C" na dir.vento 
table(dados$DirVento) # existe o level "C" na dir.vento 
which(dados$DirVento=="C")


dados %>% group_by(DirVento,VelocVento) %>% filter(DirVento =="C")
dados %>% select(VelocVento,DirVento) %>% arrange(VelocVento) %>% filter(DirVento =="C") # comparei "C" com a velo. Vento (ordem crescente)


# Calculo da proporção dos EPUs
table(epu$epu_mp10) # com todos os dados 6163 = 100%, (não EPU 5136)0 = 83,3%, (sim EPU 1027)1 = 16,7%  
table(dados$epump) 
# sem dados Verão: total dados 4646 = 100%, (não EPU 3661)0 = 78.8%, (sim EPU 985)1 = 21.2% 



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

hist(met$UR)


amostra1 <-met %>% 
  select(TempMAX) %>% 
  slice_sample(n=5000)

shapiro.test(amostra1$TempMAX)
hist(amostra1$TempMAX)




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
?createDataPartition # funsão utilizada na separação dos dados em treino/teste, garantindo aleatoriedade a separação.
# https://didatica.tech/o-pacote-caret-linguagem-r/

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



# interpretar GLM:
#https://anotherecoblog.wordpress.com/2020/07/02/interpretando-os-resultados-de-um-glm/
#https://anotherecoblog.wordpress.com/2020/07/02/interpretando-os-resultados-de-um-glm/

# interpretar valor p
# https://blog.minitab.com/pt/o-que-voce-pode-dizer-quando-seu-valor-p-e-maior-que-005#:~:text=Se%20o%20valor%2Dp%20for,que%20existe%20uma%20diferen%C3%A7a%20significativa.

## H0 : não há diferença significativa entre as médias (não há efeito)
## HA : há diferença significativa entre as médias (há efeito!)

# Nível de Confiança = 95% | Nível de Significância = 0,05
##  valor-p for menor que 0.05 = rejeitar a H0 (ficar com HA)
##  valor-p for MAIOR que 0.05 = falha em rejeitar a H0 (ficar com H0)

# modelagem #####
modelo3 <- glm(formula = epump ~
                 #m +
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
               data = treino) #treino #dados
summary(modelo3) 





# modelo3: tirando DirVento e pblh  ####
# est, ustar, VelocVento, Insolacao, Irradiacao, Precipitacao, TempMED, TempMAX, UR 
modelo3 <- glm(formula = epump ~
                  #est +
                 #pblh.m. + #Não significativa
                 ustar.m.s. +
                 VelocVento +
                 # DirVento + # Não significativa
                  #Insolacao +
                  Irradiacao +
                  Precipitacao +
                  PressaoMED +  ####
                  #TempMED +
                 TempMAX +
                 UR, 
               family = binomial(link = "logit"), 
               data = treino) #treino #dados
summary(modelo3) 

# todas AIC: 2704
# tirando DirVento e pblh AIC: 2689.3
# tirando DirVento, pblh, TempMED e IRRAD AIC: 2729.6



?vif
vif(modelo3) #tirando DirVento e pblh AIC: 2689.3



# Stepwise ####
##########################################################################a

# Stepwise feature selection
# A funcao step ajusta o modelo de regressao varias vezes, com diversas combinaaces
# de variaveis preditoras. Ele indica qual a a combinacao de variaveis que resulta
# no melhor modelo (aquele que tem melhor AIC).
step(modelo3, direction="both")

# Combinação com melhor AIC é:
# glm epump ~ est + ustar.m.s. + VelocVento + Irradiacao + Precipitacao + PressaoMED + TempMAX + UR
# com AIC = 2730

##########################################################################a


# http://sillasgonzaga.com/material/cdr/modelos.html
predict(modelo3)
?predict
saida<-predict(modelo3, newdata=x_test[,c(3,4,7,8,9,11,12,13)], type='response')

plot(saida)

predict(modelo3, newdata=x_test[1,c(3,4,11,12)], type='response')

plot(probab3)


### Razao de chances (odds ratio) modelo 3 ####
#OR estWIN 3.79087586
exp(cbind(OR = coef(modelo3), confint(modelo3)))

## o DES. P do resumo foi calculado a partir dos dados SEM NORMALIZAR !!!!!
# data frame  "met" deve ser SEM NORMALIZAR
sd(met$ustar.m.s.)  # 1 sd da v.ustar.m.s. é 0,105 m/s em relação a media
sd(met$TempMED)
sd(met$TempMAX) # 1 sd da v.TempMAX é 4,6 graus Celsius em relação a media
sd(met$Irradiacao) 
sd(met$UR) 
mean(met$TempMAX)





### Curva ROC modelo 3 #### 

# Curva ROC:mensura a capcidade de predição do modelo proposto,
#Especificidade(acerto de nao-eventos) X Sensibilidade(acerto de eventos)

# AUC: indicador representa a probabilidade de que o classificador efetue predições 
#randômicas na instância positiva melhor do que na instância negativa.

probab3 <- predict(modelo3, newdata=x_test, type='response')
plot(probab3)

roc3 <- roc(y_test,probab3)

plot.roc(y_test,probab3,
         print.auc=TRUE, # mostra o AUC no grafico
         ci=TRUE, of="thresholds", 
         thresholds="best", # seleciona o melhor threshold (p, ponto de corte)
         print.thres="best") # mostra o melhor threshold

# Curva ROC: AUC=0.911, melhorp=0.157

melhorp <- coords(roc3, x="best", ret="threshold", transpose = FALSE)



### Matriz de confusao modelo 3 ####

# Matriz de confusao - dados de teste, usando o melhor threshold (p=0.140) obtido pela curva ROC
eventoprevisto <- as.factor(ifelse(probab3>=0.195,"1","0"))

confusionMatrix(eventoprevisto, y_test, positive="1")
# Acurucia: 
# Sensibilidade (acerto de eventos):
# Especificidade (acerto de nao-eventos):

#install.packages("sjPlot")
library("sjPlot")
?sjPlot
tabel_mode(modelo2)
plot_model(modelo2)

#####################################


saida<-predict(modelo3, newdata=borabil[,c(4,5,8,9,10,12,13)], type='response')
plot(saida)

library(plotly)
fig <- plot_ly(
  y = saida,
  name="xxxxx",
  type = "scatter")

fig


# Le e organiza os dados de entrada
epu <- read.csv('epu_mp10_classific.csv') 
str(epu)
met <- read.csv('meteo_IAG_PBL_2005-2021.csv')
str(met)

table(epu$epu_mp10)# com todos os dados(com os NA) 6209 = 100%, (não EPU 5176)0 = 83,4%, (sim EPU 1033)1 = 16,3%


rem <- which(is.na(met$VelocVento)) # ha 5 dados faltantes de velocidade do vento
rem1 <-which(is.na(met$Insolacao))
rem2 <-which(is.na(met$Irradiacao))

sum(is.na(met)) # vou perder 46 linhas de dados, 6209 - 49 = 6163

met <- met[-c(rem,rem1,rem2),] # exclui as linhas com NA
epu <- epu[-c(rem,rem1,rem2),] # exclui as linhas com NA

dados <- met
dados[,1] <- epu$epu_mp10
colnames(dados)[1] <- 'epump'

dados[,14] <- epu$data
colnames(dados)[14] <- 'data'

borabil<-dados

# borabil<-STemptest %>% select(data ==2005) # excluir linhas em que dados$est =="ASUM_v" (excluir linha com verao)

borabil<-STemptest[1:365,]


fig1 <- plot_ly(data = borabil, x = ~data, y = ~epump, mode = 'scatter')
fig1

