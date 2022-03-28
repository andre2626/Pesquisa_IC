
###### DURAÇÃO EPUs#######
#### MP10 e O3  ####
## 

#dados O3
EPUlistO3=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list EPUs O3.CSV",header=T)
library(dplyr)
DuracaoEPUo3 <-data.frame(matrix(NA,1,2))
DuracaoEPUo3<-EPUlistO3 %>% 
  count(Duração_do_EPU)

#dados MP10 
EPUlistmp10=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list EPUs MP10.CSV",header=T)
DuracaoEPUmp10 <-data.frame(matrix(NA,1,2))
DuracaoEPUmp10<-EPUlistmp10 %>% 
  count(Duração_do_EPU)


library(plotly)
fig <- plot_ly(
  x = DuracaoEPUo3$Duração_do_EPU,
  y = DuracaoEPUo3$n,
  name="O3",
  type = "bar")


fig<- fig %>% add_trace(x = DuracaoEPUmp10$Duração_do_EPU,
                        y = DuracaoEPUmp10$n,
                        name="MP10")

fig <- fig %>% layout(xaxis = list(title = "Duração (dias)"),
                      yaxis = list(title = "Número de Eventos"),
                      font=list(size=19))
                  
fig


#lista de cores:
# https://developer.mozilla.org/en-US/docs/Web/CSS/color_value



####### TOTAl Ultrapassagens em cada estação ######## 
##### MP10 e O3 ####
## 
# Numero de Ultrapassagens no periodo de 2005-2021 para cada uma das estações(12)

# dados mp10
w=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/UltrapasMP10.CSV",header=T)
library(dplyr)
Estacoes<-c("Diadema","Mauá","Osasco","PqDPedroII","S.Amaro","S.Caetano","Taboão","S.Bernardo-Paulic","Cerqueira","Congonhas","Grajau","NS do Ó")

o<-w %>% select(Diadema:NS.do.Ó) %>% 
  summarise(Total_Ultrapassagens=colSums(w[,3:14],na.rm=T))
x<-bind_cols(Estacoes,o) 
#bind_cols (junta tabelas de length iguais)
colnames(x)[1]="Estacoes"


# dados o3
o3w=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/UltrapasO3.CSV",header=T)

library(dplyr)
#Estacoes1<-c("Diadema","Mauá","Osasco","PqDPedroII","S.Amaro","S.Caetano","Taboão","S.Bernardo-Paulic","Cerqueira","Congonhas","Grajau","NS do Ó")
Estacoes1<-c("Diadema","Ibirapuera","Mauá","PqDPedroII","S.Amaro","Pinheiros","Santana","S.Caetano","Itaquera","Ipen","Grajau","NS.do.Ó")
o3o<-o3w %>% select(Diadema:NS.do.Ó) %>% 
  summarise(Total_Ultrapassagens=colSums(o3w[,3:14],na.rm=T))
o3x<-bind_cols(Estacoes1,o3o) 
colnames(o3x)[1]="Estacoes"


fig <- plot_ly(x = o3x$Estacoes,
               y = o3x$Total_Ultrapassagens,
               name="O3",
               type = "bar")
fig<- fig %>% add_trace(x = x$Estacoes,
                        y = x$Total_Ultrapassagens,
                        name="MP10")

#title = "Número de ultrapassagens dos poluentes MP10 (laranja) e O3 (Azul) na RMSP entre os anos 2005 e 2021"
fig <- fig %>% layout(yaxis = list(title = "Número de Eventos"),
                      font=list(size=17))

fig





####### OCORRENCIA EPUs p cada mes ######## 
##### MP10 e O3 ####
#num de EPUs para cada mes
library(dplyr)

#dados MP10
EPUlistmp10=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list EPUs MP10.CSV",header=T)
library(lubridate)
EPUlistmp10[,4] <-month(EPUlistmp10$Início_do_EPU)
mp10mes_Epu<-EPUlistmp10 %>% count(V4) 

#dados O3
EPUlistO3=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list EPUs O3.CSV",header=T)
library(lubridate)
EPUlistO3[,4] <-month(EPUlistO3$Início_do_EPU)
O3mes_Epu<-EPUlistO3 %>% count(V4)
#mes=month
#ano=year

library(plotly)
fig <- plot_ly(x = O3mes_Epu$V4,
               y = O3mes_Epu$n,
               name="O3",
               type = "bar")
fig<- fig %>% add_trace(x = mp10mes_Epu$V4,
                        y = mp10mes_Epu$n,
                        name="MP10")

#title = "Ocorrência de EPUS dos poluentes MP10 (laranja) e O3 (Azul) na RMSP entre os anos 2005 e 2021, em função dos meses",
fig <- fig %>% layout(xaxis = list(title = "Mês"),
                      yaxis = list(title = "Número de Eventos"),
                      font=list(size=19))

fig



##
### Como Juntar 2 bases de dados em um só grafico:
##
#fig <- plot_ly(x = mes_Epu$V4,
#               y = mes_Epu$n,
#               type = "bar")
#fig<- fig %>% add_trace(x = "x dos dados 2",
#                        y = "y dos dados 2",
#                        name="zzz")



#######OCORRENCIA EPUs para cada ano ######## 
####MP10 e O3 ####
#num de EPUs para cada mes

#dados MP10
EPUlistmp10=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list EPUs MP10.CSV",header=T)
library(lubridate)
EPUlistmp10[,4] <-year(EPUlistmp10$Início_do_EPU)
mp10mes_Epu<-EPUlistmp10 %>% count(V4) 

#dados O3
EPUlistO3=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list EPUs O3.CSV",header=T)
library(lubridate)
EPUlistO3[,4] <-year(EPUlistO3$Início_do_EPU)
O3mes_Epu<-EPUlistO3 %>% count(V4)
#mes=month
#ano=year

fig <- plot_ly(x = O3mes_Epu$V4,
               y = O3mes_Epu$n,
               name="O3",
               type = "bar")
fig<- fig %>% add_trace(x = mp10mes_Epu$V4,
                        y = mp10mes_Epu$n,
                        name="MP10")

# title = 'Ocorrência de EPUS dos poluentes MP10 (laranja) e O3 (Azul) na RMSP entre os anos 2005 e 2021, em função dos anos',
fig <- fig %>% layout(xaxis = list(title = ""),
                      yaxis = list(title = "Número de Eventos"),
                      font=list(size=19))

fig










