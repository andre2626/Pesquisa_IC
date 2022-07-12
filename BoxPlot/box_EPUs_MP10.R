library(dplyr) 
library(plotly)


##
#### BOX PLOT MP10 ####
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
boxMP10 <-boxMP10 %>% layout(shapes = list(hline(padraoOMS)),font=list(size=23))
boxMP10

#https://plotly.com/r/horizontal-vertical-shapes/  ## Colocar linha H ou V nos graf Plotly
#https://plotly.com/r/box-plots/
#https://statisticsglobe.com/plotly-boxplot-r


##### poluente O3 #####
STempbox$O3..µg.m3.
padraoOMS<- 100
boxO3 <-plot_ly(data=STempbox, y= ~O3..µg.m3., x = ~periodo, name = "O3", type="box")
boxO3 <-boxO3 %>% layout(yaxis=list(title="O3 (µg/m3)"),xaxis=list(label=c("Antes","Pico","Depois")))
boxO3 <-boxO3 %>% layout(xaxis=list(title=""))
boxO3 <-boxO3 %>% layout(shapes = list(hline(padraoOMS)),font=list(size=23))
boxO3


##### poluente CO #####
STempbox$CO..ppm.
padraoOMS<-4 #ppm
boxCO <-plot_ly(data=STempbox, y= ~CO..ppm., x = ~periodo, name = "CO", type="box")
boxCO <-boxCO %>% layout(yaxis=list(title="CO (ppm)"),xaxis=list(label=c("Antes","Pico","Depois")))
boxCO <-boxCO %>% layout(xaxis=list(title=""))
boxCO <-boxCO %>% layout(shapes = list(hline(padraoOMS)),font=list(size=23))
boxCO

##### poluente MP2,5 #####
STempbox$MP2.5..µg.m3.
padraoOMS<-15
boxMP25 <-plot_ly(data=STempbox, y= ~MP2.5..µg.m3., x = ~periodo, name = "MP25", type="box")
boxMP25 <-boxMP25 %>% layout(yaxis=list(title="MP2.5 (µg/m3)"),xaxis=list(label=c("Antes","Pico","Depois")))
boxMP25 <-boxMP25 %>% layout(xaxis=list(title=""))
boxMP25 <-boxMP25 %>% layout(shapes = list(hline(padraoOMS)),font=list(size=23))
boxMP25

##### poluente NO2  #####
STempbox$NO2..µg.m3.
padraoOMS<- 25
boxNO2 <-plot_ly(data=STempbox, y= ~NO2..µg.m3., x = ~periodo, name = "NO2", type="box")
boxNO2 <-boxNO2 %>% layout(yaxis=list(title="NO2 (µg/m3)"),xaxis=list(label=c("Antes","Pico","Depois")))
boxNO2 <-boxNO2 %>% layout(xaxis=list(title=""))
boxNO2 <-boxNO2 %>% layout(shapes = list(hline(padraoOMS)),font=list(size=23))
boxNO2

##### poluente SO2  #####
STempbox$SO2..µg.m3.
padraoOMS<- 40
boxSO2 <-plot_ly(data=STempbox, y= ~SO2..µg.m3., x = ~periodo, name = "SO2", type="box")
boxSO2 <-boxSO2 %>% layout(yaxis=list(title="SO2 (µg/m3)"),xaxis=list(label=c("Antes","Pico","Depois")))
boxSO2 <-boxSO2 %>% layout(xaxis=list(title=""))
boxSO2 <-boxSO2 %>% layout(shapes = list(hline(padraoOMS)),font=list(size=23))
boxSO2


##### Altura da Camada Limite PBLH  #####
STempbox$pblh.m.
boxPBLH <-plot_ly(data=STempbox, y= ~pblh.m., x = ~periodo, name = "PBLH", type="box")
boxPBLH <-boxPBLH %>% layout(yaxis=list(title="PBLH (m)"),xaxis=list(label=c("Antes","Pico","Depois")))
boxPBLH <-boxPBLH %>% layout(xaxis=list(title=""),font=list(size=23))
boxPBLH

##### VelocVento  #####
STempbox$VelocVento
boxVV <-plot_ly(data=STempbox, y= ~VelocVento, x = ~periodo, name = "VelocVento", type="box")
boxVV <-boxVV %>% layout(yaxis=list(title="VV (km/h)"),xaxis=list(label=c("Antes","Pico","Depois")))
boxVV <-boxVV %>% layout(xaxis=list(title=""),font=list(size=23))
boxVV

##### Irradiação  #####
STempbox$Irradiacao
boxIrrad <-plot_ly(data=STempbox, y= ~Irradiacao, x = ~periodo, name = "Irrad", type="box")
boxIrrad <-boxIrrad %>% layout(yaxis=list(title="Irrad (MJ/m2)"),xaxis=list(label=c("Antes","Pico","Depois")))
boxIrrad <-boxIrrad %>% layout(xaxis=list(title=""),font=list(size=23))
boxIrrad

##### TempMax  #####
STempbox$TempMAX
boxTempMAX <-plot_ly(data=STempbox, y= ~TempMAX, x = ~periodo, name = "TempMAX", type="box")
boxTempMAX <-boxTempMAX %>% layout(yaxis=list(title="Tmax (°C)"),xaxis=list(label=c("Antes","Pico","Depois")))
boxTempMAX <-boxTempMAX %>% layout(xaxis=list(title=""),font=list(size=23))
boxTempMAX

##### Precipitação  #####
STempbox$Precipitacao
boxPrecip <-plot_ly(data=STempbox, y= ~Precipitacao, x = ~periodo, name = "Precipitação", type="box")
boxPrecip <-boxPrecip %>% layout(yaxis=list(title="Precip (mm/dia)"),xaxis=list(label=c("Antes","Pico","Depois")))
boxPrecip <-boxPrecip %>% layout(xaxis=list(title=""),font=list(size=23))
boxPrecip

##### UR  #####
STempbox$UR
boxUR <-plot_ly(data=STempbox, y= ~UR, x = ~periodo, name = "UR", type="box")
boxUR <-boxUR %>% layout(yaxis=list(title="UR (%)"),xaxis=list(label=c("Antes","Pico","Depois")))
boxUR <-boxUR %>% layout(xaxis=list(title=""),font=list(size=23))
boxUR
