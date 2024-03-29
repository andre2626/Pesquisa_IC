#Atv: Calcular a m�dia mensal de MP10 em v�rias esta��es
#ATV: Fazer uma gr�fico da COBERTURA de dados MP10

library(dplyr)
library(lubridate)
medDia_MP10=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuni�es/5 Dados completos MP10 e O3/Dados com edi��o nos nomes das esta��es  2005-20021/MP10_media_diaria_2005-2021.CSV",header=T)

colnames(medDia_MP10)[1]="data"
#renomei a coluna "matlab" para "data"

medDia_MP10 <- medDia_MP10[ ,-17] #["linha","coluna"] apaguei a coluna "X" 


medDia_MP10$data <-paste(medDia_MP10$y,medDia_MP10$m,medDia_MP10$d, sep = "_")
#mesclei os dados dos anos, meses e dias, na coluna "data", separando 
#cada dado por "_"
#https://www.ti-enxame.com/pt/r/como-mesclar-duas-colunas-em-r-com-um-simbolo-especifico/971257923/


library(lubridate) #Fun��es dmy, mdy, ymd, ymd_h, ymd_hms, etc: convertem 
#data escrita na forma de texto para um objeto tipo "date"

medDia_MP10$data <-ymd(medDia_MP10$data) 
#data organizada em (ano, mes, dia)

#data frame <- data frame ["linha","coluna"]
medDia_MP10 <- medDia_MP10[ ,-c(2,4)]
#Excluir as colnas "y"(ano) e "d"(dia), para n�o ter informacao 
#repetida, pois na coluna "data" ja tem esses dados





#### OBJ: quero a m�dia de MP10 de cada mes, para todas as Estacoes!!!

Med_Mensal<-data.frame(mes=1:12, matrix(NA,12,12))

colnames(Med_Mensal)[2:13]<- colnames(medDia_MP10[3:14])

# loop For
#which: encontrar a posi��o de um elemento
for (iest in 3:14) {
  for (imes in 1:12) {
    lin <- which(medDia_MP10$m==imes)
    Med_Mensal[imes,iest-1] <- mean(medDia_MP10[lin,iest], na.rm = TRUE)
  }
}



#### OBJ: saber a Cobertura de dados de cada m�s, em cada uma da esta��es!!!

#1 coluna = Jan 2005, fev 2005... dez 2020
#2 coluna = % de dados validos em cada mes (ex: num. dados validos jan/31)

#### Cobertura de dados S� em DIADEMA ######
CobDados<- data.frame(mes=seq(ymd('2005-01-01'),ymd('2020-12-31'),by='months'))
CobDados[, 2:13]<-NA
colnames(CobDados)[2:13]<- colnames(medDia_MP10[3:14])
medDia_MP10$mesano<- ym (paste(year(medDia_MP10[,1]),medDia_MP10$m, sep="_"))

for (i in 1:nrow(CobDados)) {
  lin <- which(medDia_MP10$mesano==CobDados$mes[i])
  CobDados[i,2]<-sum(!is.na(medDia_MP10$Diadema[lin]))/length(medDia_MP10$Diadema[lin])
}
###########################################


#### Cobertura de dados p/ todas as esta��es! ######

CobDados<- data.frame(mes=seq(ymd('2005-01-01'),ymd('2020-12-31'),by='months'))
CobDados[, 2:13]<-NA
colnames(CobDados)[2:13]<- colnames(medDia_MP10[3:14])
medDia_MP10$mesano<- ym (paste(year(medDia_MP10[,1]),medDia_MP10$m, sep="_"))

for (i in 1:nrow(CobDados)){
  for (im in 3:14) {
    lin <-which(medDia_MP10$mesano==CobDados$mes[i])
    CobDados[i,im-1]<-sum(!is.na(medDia_MP10[lin,im]))/length(medDia_MP10[lin,im])
  } 
}



#### Heatmap da Cobertura de dados p/ todas as esta��es! ######

library(plotly)

cobert <- t(as.matrix(CobDados)) #t (matrix transposta - "linha vira coluna, viceversa")
fig2 <- plot_ly(x = CobDados$mes,
                y = colnames(CobDados)[2:13],
                z = cobert[2:13,], type = "heatmap")

#fig2 <- fig2 %>% layout(title = 'Cobertura de dados MP10 (%)',
#                        font=list(size=13))

fig2<- fig2 %>% layout(title = list(text ='<b> MP10 </b>', 
                                  font = list(size = 17)))

fig2<-fig2 %>% layout(font=list(size=16))

fig2

#fig2<-fig2 %>% layout(font=t)
#t <- list(
#  family = "Arial Black", #fontes do Word
#  size = 14,
#  color = 'Gray')

#fig <- fig %>% layout(legend=list(title=list(text='<b> Poluentes </b>')))
# <b> TEXTO </b> : p deixar o texto em NEGRITO



#Como formatar o gr�fico? (ex: colocar t�tulo, mudar as cores...)
################################################################################

#na.rm = TRUE -> � uma foma de fazer a m�dia ignorando os NaNs ou NAs

# !is.na -> ignora os NaNs ou NAs




