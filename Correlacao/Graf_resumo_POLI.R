#graf p resumo POLI 

####### OCORRENCIA EPUs p cada mes ######## 
##### MP10 e O3 ####
#num de EPUs para cada mes
library(dplyr)

#dados MP10
EPUlistmp10=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list_EPUs_MP10.CSV",header=T)
library(lubridate)
EPUlistmp10[,4] <-month(EPUlistmp10$Início_do_EPU)
mp10mes_Epu<-EPUlistmp10 %>% count(V4) 

#dados O3
#EPUlistO3=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list_EPUs_O3.CSV",header=T)
#library(lubridate)
#EPUlistO3[,4] <-month(EPUlistO3$Início_do_EPU)
#O3mes_Epu<-EPUlistO3 %>% count(V4)
#mes=month
#ano=year


mp10mes_Epu <- add_row(mp10mes_Epu,V4 = 12, n = 0)
#mp10mes_Epu %>% add_row(V4 = 12, n = 0)


library(plotly)
?plot_ly
fig <- plot_ly(x = mp10mes_Epu$V4,
               y = mp10mes_Epu$n,
               name="O3",
               type = "bar",
               color = "orange")


#title = "Ocorrência de EPUS dos poluentes MP10 (laranja) na RMSP entre os anos 2005 e 2021, em função dos meses",
fig <- fig %>% layout(xaxis = list(title = "Mês"),
                      yaxis = list(title = "Número de Eventos"),
                      font=list(size=25))

fig <- fig %>% layout(xaxis = list(
  dtick = 1, #"variar de 1 em 1"
  tickmode = "linear"))

fig









##
#### BOX PLOT MP10 ####
##

library(dplyr) 
library(plotly)

EPUlistmp10=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list_EPUs_MP10.CSV",header=T)
STemp=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/MedPol_CLP_VMet.CSV",header=T)


STemp[,21]<-NA
colnames(STemp)[21]="periodo"


for (i in 1:nrow(EPUlistmp10)) {
  inicio<-which(STemp$data==EPUlistmp10$Início_do_EPU[i])
  final<-which(STemp$data==EPUlistmp10$Término_do_EPU[i])
  STemp[(inicio-3):(inicio-1),21]<-"Antes"  # 3 dias antes dos EPUs: -3,-2,-1 (antes)
  STemp[(final-2):(final),21]<-"Pico"  # ultimos 3 dias nos EPUs, ex com EPU de 5 dias: 0,1,(2,3,4)
  STemp[(final+1):(final+3),21]<-"Depois"   # 3 dias depois dos EPUs: 5,6,7 (depois)
}


STempbox<-STemp[!is.na(STemp[,21]),]
STempbox$periodo<-factor(STempbox$periodo,levels =c("Antes","Pico","Depois")) 

#STempbox


hline <- function(y = 0, color = "red") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color)
  )
}

##### poluente MP10 #####

# fazer y de todas as coll (poluentes e VMet que est?o nos graf de Composi??o no relatorio p2)
# mudar title (poluentes e VMet que est?o nos graf de Composi??o no relatorio p2)
# mudar o numero do hline (padr?o da OMS)

STempbox$MP10..µg.m3..
padraoOMS<- 45
boxMP10 <-plot_ly(data=STempbox, y= ~MP10..µg.m3.., x = ~periodo, name = "MP10", type="box")
boxMP10 <-boxMP10 %>% layout(yaxis=list(title="MP10 (µg/m3)"),xaxis=list(label=c("Antes","Pico","Depois")))
boxMP10 <-boxMP10 %>% layout(xaxis=list(title=""))
boxMP10 <-boxMP10 %>% layout(shapes = list(hline(padraoOMS)),font=list(size=23))
boxMP10

#https://plotly.com/r/horizontal-vertical-shapes/  ## Colocar linha H ou V nos graf Plotly
#https://plotly.com/r/box-plots/
#https://statisticsglobe.com/plotly-boxplot-r



