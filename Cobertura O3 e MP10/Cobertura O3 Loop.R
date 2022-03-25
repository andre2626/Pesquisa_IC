#Atv: Calcular a média mensal de O3 em várias estações
#ATV: Fazer uma gráfico da COBERTURA de dados O3

library(dplyr)
library(lubridate)
medDia_O3=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/5 Dados completos MP10 e O3/Dados com edição nos nomes das estações  2005-20021/O3_media_diaria_2005-2021.CSV",header=T)

 
colnames(medDia_O3)[1]="data"
#renomei a coluna "matlab" para "data"

medDia_O3 <- medDia_O3[ ,-17] #["linha","coluna"] apaguei a coluna "X" 


medDia_O3$data <-paste(medDia_O3$y,medDia_O3$m,medDia_O3$d, sep = "_")
#mesclei os dados dos anos, meses e dias, na coluna "data", separando 
#cada dado por "_"
#https://www.ti-enxame.com/pt/r/como-mesclar-duas-colunas-em-r-com-um-simbolo-especifico/971257923/


library(lubridate) #Funções dmy, mdy, ymd, ymd_h, ymd_hms, etc: convertem 
#data escrita na forma de texto para um objeto tipo "date"

medDia_O3$data <-ymd(medDia_O3$data) 
#data organizada em (ano, mes, dia)

#data frame <- data frame ["linha","coluna"]
medDia_O3 <- medDia_O3[ ,-c(2,4)]
#Excluir as colnas "y"(ano) e "d"(dia), para não ter informacao 
#repetida, pois na coluna "data" ja tem esses dados



#### OBJ: quero a média de O3 de cada mes, para todas as Estacoes!!!

Med1_Mensal<-data.frame(mes=1:12, matrix(NA,12,12))

colnames(Med1_Mensal)[2:13]<- colnames(medDia_O3[3:14])

# loop For
#which: encontrar a posição de um elemento
for (iest in 3:14) {
  for (imes in 1:12) {
    lin <- which(medDia_O3$m==imes)
    Med1_Mensal[imes,iest-1] <- mean(medDia_O3[lin,iest], na.rm = TRUE)
  }
}



#### OBJ: saber a Cobertura de dados de cada mês, em cada uma da estações!!!

#1 coluna = Jan 2005, fev 2005... dez 2020
#2 coluna = % de dados validos em cada mes (ex: num. dados validos jan/31)

#### Cobertura de dados SÓ em DIADEMA ######
CobDados<- data.frame(mes=seq(ymd('2005-01-01'),ymd('2020-12-31'),by='months'))
CobDados[, 2:13]<-NA
colnames(CobDados)[2:13]<- colnames(medDia_O3[3:14])
medDia_O3$mesano<- ym (paste(year(medDia_O3[,1]),medDia_O3$m, sep="_"))

for (i in 1:nrow(CobDados)) {
  lin <- which(medDia_O3$mesano==CobDados$mes[i])
  CobDados[i,2]<-sum(!is.na(medDia_O3$Diadema[lin]))/length(medDia_O3$Diadema[lin])
}
###########################################


#### Cobertura de dados p/ todas as estações! ######

CobDados<- data.frame(mes=seq(ymd('2005-01-01'),ymd('2020-12-31'),by='months'))
CobDados[, 2:13]<-NA
colnames(CobDados)[2:13]<- colnames(medDia_O3[3:14])
medDia_O3$mesano<- ym (paste(year(medDia_O3[,1]),medDia_O3$m, sep="_"))

for (i in 1:nrow(CobDados)){
  for (im in 3:14) {
    lin <-which(medDia_O3$mesano==CobDados$mes[i])
    CobDados[i,im-1]<-sum(!is.na(medDia_O3[lin,im]))/length(medDia_O3[lin,im])
  } 
}
  


#### Heatmap da Cobertura de dados p/ todas as estações! ######
 
library(plotly)

cobert <- t(as.matrix(CobDados)) #t (matrix transposta - "linha vira coluna, viceversa")
fig1 <- plot_ly(x = CobDados$mes,
                y = colnames(CobDados)[2:13],
                z = cobert[2:13,], type = "heatmap")

#fig1 <- fig1 %>% layout(title = 'Cobertura de dados O3 (%)')

fig1<- fig1 %>% layout(title = list(text ='<b> O3 </b>', 
                                    font = list(size = 17)))

fig1<-fig1 %>% layout(font=list(size=16))

fig1



#Como formatar o gráfico? (ex: colocar título, mudar as cores...)
################################################################################

#na.rm = TRUE -> é uma foma de fazer a média ignorando os NaNs ou NAs

# !is.na -> ignora os NaNs ou NAs




