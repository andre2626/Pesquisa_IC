library(dplyr) 

#### Precipitação acumulada durante cada EPU ####
##### MP10 #####
EPUlistmp10=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list EPUs MP10.CSV",header=T)
STemp=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/MedPol_CLP_VMet.CSV",header=T)


for (i in 1:nrow(EPUlistmp10)) {
  inicio<-which(STemp$data==EPUlistmp10$Início_do_EPU[i])
  final<-which(STemp$data==EPUlistmp10$Término_do_EPU[i])
  EPUlistmp10[i,4]<-sum(STemp[inicio:final,9],na.rm=TRUE)
  colnames(EPUlistmp10)[4]="Precip_Acumu"  #precipitação acumulada durante cada EPU
}


##### O3 #####
EPUlistO3=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list EPUs O3.CSV",header=T)
STemp=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/MedPol_CLP_VMet.CSV",header=T)


for (i in 1:nrow(EPUlistO3)) {
  inicio<-which(STemp$data==EPUlistO3$Início_do_EPU[i])
  final<-which(STemp$data==EPUlistO3$Término_do_EPU[i])
  EPUlistO3[i,4]<-sum(STemp[inicio:final,9],na.rm=TRUE)
  colnames(EPUlistO3)[4]="Precip_Acumu"  #precipitação acumulada durante cada EPU
}


#### Dias de chuvas durante TODO o periodo estudado (2005 - 2020) ####
##### MP10 #####

EPUclasmp10=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/epu_mp10_classific.CSV",header=T)
STemp=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/MedPol_CLP_VMet.CSV",header=T)

EPUclasmp10[,7]<-STemp %>% select(Precipitacao)


#SE o loop encontrar >= a 1 na coluna 7 (EPUclasmp10$Precipitacao), vai fazer tudo o q tiver no segundo while,
#SE não (else), vai preencher a col 8 com 0 e depois vai pra próxima linha

EPUclasmp10[,8]<-1
EPUclasmp10[EPUclasmp10[,7]>0,8]<-0
colnames(EPUclasmp10)[8]="sem_chuva" #chove=1 Não chove=0

#quantos dias sem chuva
EPUclasmp10[,9]<-0
colnames(EPUclasmp10)[9]="dias_sem_chuva" #num de dias sem chuvas

lin<-1
while(lin<nrow(EPUclasmp10)){
  if (EPUclasmp10[lin,8]==1){
    ndias<-0
    while (EPUclasmp10[lin,8]==1) {
      EPUclasmp10[lin,9]<-1+ndias
      ndias<-1+ndias
      lin<-lin+1
    }
  }else {
    EPUclasmp10[lin,9]<-0
    lin<-lin+1
  }
} 



#lin<-1
#while(lin<nrow(EPUclasmp10)){
#  if (EPUclasmp10[lin,7]>0){
#    EPUclasmp10[lin,8]<-1 
#    while (EPUclasmp10[lin,7]>0) {
#      EPUclasmp10[lin,8]<-1
#      lin<-lin+1
#    }
#  }else {
#    EPUclasmp10[lin,8]<-0
#    lin<-lin+1
#  }
#  colnames(EPUclasmp10)[8]="chuva" # dia com chuva
#} 


##### O3 #####

EPUclasO3=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/epu_o3_classific.CSV",header=T)
STemp=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/MedPol_CLP_VMet.CSV",header=T)

EPUclasO3[,7]<-STemp %>% select(Precipitacao)


#SE o loop encontrar >= a 1 na coluna 7 (EPUclasmp10$Precipitacao), vai fazer tudo o q tiver no segundo while,
#SE não (else), vai preencher a col 8 com 0 e depois vai pra próxima linha

EPUclasO3[,8]<-1
EPUclasO3[EPUclasO3[,7]>0,8]<-0
colnames(EPUclasO3)[8]="sem_chuva" #chove=1 Não chove=0

#quantos dias sem chuva
EPUclasO3[,9]<-0
colnames(EPUclasO3)[9]="dias_sem_chuva" #num de dias sem chuvas

lin<-1
while(lin<nrow(EPUclasO3)){
  if (EPUclasO3[lin,8]==1){
    ndias<-0
    while (EPUclasO3[lin,8]==1) {
      EPUclasO3[lin,9]<-1+ndias
      ndias<-1+ndias
      lin<-lin+1
    }
  }else {
    EPUclasO3[lin,9]<-0
    lin<-lin+1
  }
} 





#### Numero de dias sem chuva no início de cada EPU ####
#### sequencia de dias sem chuva até o início de cada EPU ####

##### MP10 #####


for (i in 1:nrow(EPUlistmp10)) {
  inicio<-which(EPUclasmp10$data==EPUlistmp10$Início_do_EPU[i])
  final<-which(EPUclasmp10$data==EPUlistmp10$Término_do_EPU[i])
  EPUlistmp10[i,5]<-(EPUclasmp10[inicio,9])
  colnames(EPUlistmp10)[5]="Num_dias_sem_chuva_inic" 
}

##### O3 #####

for (i in 1:nrow(EPUlistO3)) {
  inicio<-which(EPUclasO3$data==EPUlistO3$Início_do_EPU[i])
  final<-which(EPUclasO3$data==EPUlistO3$Término_do_EPU[i])
  EPUlistO3[i,5]<-(EPUclasO3[inicio,9])
  colnames(EPUlistO3)[5]="Num_dias_sem_chuva_inic" 
}




##
#### graf dias sem chuva no inicio dos EPUS ####
##

mp10<-EPUlistmp10 %>% count(Num_dias_sem_chuva_inic)
o3<-EPUlistO3 %>% count(Num_dias_sem_chuva_inic)

library(plotly)
fig <- plot_ly(
  x = o3$Num_dias_sem_chuva,
  y = o3$n,
  name="O3",
  type = "bar")


fig<- fig %>% add_trace(x = mp10$Num_dias_sem_chuva,
                        y = mp10$n,
                        name="MP10")

fig <- fig %>% layout(xaxis = list(title = "Número de dias sem chuvas"),
                      yaxis = list(title = "Número de eventos"),
                      font=list(size=19))

fig <- fig %>% layout(xaxis = list(
  dtick = 1, #"variar de 1 em 1"
  tickmode = "linear"))

fig

##### Não deve estar certo!!! #####
# duração_EPU no eixo y, dias sem chuva no eixo x).

library(plotly)
fig1 <- plot_ly(
  x = EPUlistO3$Num_dias_sem_chuva_inic,
  y = EPUlistO3$Duração_do_EPU,
  name="O3",
  type = "scatter")


fig1<- fig1 %>% add_trace(x = EPUlistmp10$Num_dias_sem_chuva_inic,
                        y = EPUlistmp10$Duração_do_EPU,
                        name="MP10")

fig1 <- fig1 %>% layout(xaxis = list(title = "dias sem chuva"),
                      yaxis = list(title = "duração epus"),
                      font=list(size=19))

fig1








#salvar a col "dias_sem_chuva" no data.frame "EPUclasmp10"

STemp[,20]<-EPUclasmp10 %>% select(dias_sem_chuva)

caminho <- "G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/"
#saveRDS(f,file=paste0(caminho,"epu_mp10_classific.Rds"))
write.csv(STemp, file = paste0(caminho,"MedPol_CLP_VMet.csv"), row.names = FALSE)

#agora da p fazer um graf de Composição p os "Dias sem chuvas" abrindo os scripts:
#"EvolucaoEPU_CLP_VMet_Polu_MP10" e "EvolucaoEPU_CLP_VMet_Polu_O3"



##
#### BOX PLOT MP10 (teste só para o poluente MP10) ####
##

EPUlistmp10=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list EPUs MP10.CSV",header=T)
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

# fazer y de todas as coll (poluentes e VMet que estão nos graf de Composição no relatorio p2)
# mudar title (poluentes e VMet que estão nos graf de Composição no relatorio p2)
# mudar o numero do hline (padrão da OMS)

STempbox$MP10..µg.m3..
padraoOMS<- 45
boxMP10 <-plot_ly(data=STempbox, y= ~MP10..µg.m3.., x = ~periodo, name = "MP10", type="box")
boxMP10 <-boxMP10 %>% layout(yaxis=list(title="MP10 (µg/m3)"),xaxis=list(label=c("Antes","Pico","Depois")))
boxMP10 <-boxMP10 %>% layout(xaxis=list(title=""))
boxMP10 <-boxMP10 %>% layout(shapes = list(hline(padraoOMS)))
boxMP10

#https://plotly.com/r/horizontal-vertical-shapes/  ## Colocar linha H ou V nos graf Plotly
#https://plotly.com/r/box-plots/
#https://statisticsglobe.com/plotly-boxplot-r





