#Evolucao EPU Poluentes
library(plotly)

##dados entrada

#serie temp: CLP_VMeteo_MMM
STemp=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/MedPol_CLP_VMet.CSV",header=T)
#STemp=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/[]média poluentes.CSV",header=T)


#lista de EPUs
#EPUlistO3=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list EPUs MP10.CSV",header=T)
EPUlistO3=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list EPUs O3.CSV",header=T)


#escolher Duracao (MP10 =5, O3 =3)
poluente <- "O3"
duracao<-3
lin<-which(EPUlistO3$Duração_do_EPU==duracao) #linhas com duracao selecionada

eixoX<-data.frame(matrix(NA,nrow(STemp),1))
plot(-3:8,matrix(0,1,length(-3:8)))


#colnames(STemp)[1:13]<- c( "data", "mes"," pblh (m) "," ustar (m/s) ", " Velocidade do Vento (km/h) ", " Direção do Vento", "Insolação (horas)", 
#                           " Irradiação (MJ/m2) ", "Precipitação (mm)", "Pressão (hPa)", 'Temperatura Média (°C)','Temperatura Máxima (°C)',
#                           "Umidade relativa do ar (%)") ##

colnames(STemp)[1:13]<- c( "data", "mes"," PBLH (m) "," USTAR (m/s) ", " VV (km/h) ", " Direção do Vento", "Insol (horas)", 
                           " Irrad (MJ/m2) ", "Precip (mm/dia)", "P (hPa)", 'Tmed (°C)','Tmax (°C)',
                           "UR (%)") ##

colnames(STemp)[14:20]<- c( "MP10 (µg/m3)","O3 (µg/m3)","CO (ppm)", "MP2,5 (µg/m3)", "NO2 (µg/m3)", "SO2 (µg/m3)","Dias sem chuvas")



for (j in c(3:5,7:20)) { # loop pelas variáveis de STemp
  
  selecao<-data.frame(x=seq(-3,duracao+2)) # selecao de dados dos eventos de interesse
  for (i in 1:length(lin)) { # loop pelos eventos de interesse
    inicio<-which(STemp$data==EPUlistO3$Início_do_EPU[lin[i]])
    fim<-which(STemp$data==EPUlistO3$Término_do_EPU[lin[i]])
    
    # Marca os instantes de tempo desde 3 dias antes até 3 dias depois dos eventos selecionados
    eixoX[(inicio-3):(fim+3),1]<-seq(-3,duracao+2)
    
    ## plot(eixoX[(inicio-3):(inicio+7),1], STemp$VelocVento[(inicio-3):(inicio+7)],"l") 
    
    selecao<-cbind(selecao,STemp[(inicio-3):(fim+3),j]) #(inicio+7) <- (0,1,2,3,4, + 3) duracao de 5 dias
    #selecao<-cbind(selecao,STemp[(inicio-3):(inicio+7),j]) #
    colnames(selecao)[i+1]<-paste("Evento",i)
  }
  
  # Calcula mediana e quartis para cada classe de dia (composição)
  
  mediana<-data.frame(matrix(NA,1,1))
  
  #low<-quantile(selecao[1,2:39],0.25)
  low<-data.frame(matrix(NA,1,1))
  
  #high<-quantile(selecao[1,2:39],0.75)
  high<-data.frame(matrix(NA,1,1))
  
  
  for (i in 1:nrow(selecao)) {
    low[i,]<-quantile(selecao[i,2:ncol(selecao)],0.25, na.rm=T)
    high[i,]<-quantile(selecao[i,2:ncol(selecao)],0.75, na.rm=T)
    colnames(low)[1]<-"low"
    colnames(high)[1]<-"high"
    mediana[i,]<-quantile(selecao[i,2:ncol(selecao)],0.50, na.rm=T)
    colnames(mediana)[1]<-"mediana"
  }
  ##
  
  
  #dataF <- data.frame(selecao$x, high, low)
  #colnames(dataF)[1]<-"eixox"
  
  dataF <- data.frame(selecao$x, high, low,mediana)
  colnames(dataF)[1]<-"eixox"
  
  #dataF$medias <- rowMeans(selecao[2:ncol(selecao)])
  
  
  #month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
  #           'August', 'September', 'October', 'November', 'December')
  #high_2014 <- c(28.8, 28.5, 37.0, 56.8, 69.7, 79.7, 78.5, 77.8, 74.1, 62.6, 45.3,12.2)
  #low_2014 <- c(12.7, 14.3, 18.6, 35.5, 49.9, 58.0, 60.0, 58.6, 51.7, 45.2, 32.2,12.2)
  #data <- data.frame(month, high_2014, low_2014)
  #data$average_2014 <- rowMeans(data[,c("high_2014", "low_2014")])
  
  #The default order will be alphabetized unless specified as below:
  #data$month <- factor(data$month, levels = data[["month"]])
  
  
  # Gráfico de composição 
  vline <- function(x = 0, color = "red") {
    list(type = "line", y0 = 0, y1 = 1, yref = "paper",
         x0 = x,  x1 = x,
         line = list(color = color, dash="dot"))}
  
  # Gráfico sombreado com plotly      
  
  fig <- plot_ly(dataF, x = ~eixox, y = ~high, type = 'scatter', mode = 'lines',
                 line = list(color = 'rgba(0,100,80,0.2)'),
                 showlegend = FALSE, name = 'Maiores valores')
  
  fig <- fig %>% add_trace(y = ~low, type = 'scatter', mode = 'lines',
                           fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,100,80,0.2)'),
                           showlegend = FALSE, name = 'Menores valores')
  
  fig <- fig %>% add_trace(x = ~eixox, y = ~mediana, type = 'scatter', mode = 'lines',
                           line = list(color='rgb(0,100,80)'),
                           name = 'Average') 
  fig
  ##
  # título: Velocidade média, máxima e mínima (eixo y), antes, durante e depois dos EPUs (eixo x)
  fig <- fig %>% layout(title = " ",
                        paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                        shapes = list(vline(0), vline(duracao-1)),
                        xaxis = list(title = "Dias",
                                     gridcolor = 'rgb(255,255,255)',
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     tickcolor = 'rgb(127,127,127)',
                                     ticks = 'outside',
                                     zeroline = F),
                        yaxis = list(title = "Velocidade do Vento (km/h)",
                                     gridcolor = 'rgb(255,255,255)',
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     tickcolor = 'rgb(127,127,127)',
                                     ticks = 'outside',
                                     zeroline = FALSE))
  
  
  fig <- fig %>% layout(xaxis = list(title = " Dias ",
                                     dtick = 1, #"variar de 1 em 1"
                                     tickmode = "linear"),
                        yaxis = list(title = colnames(STemp[j])), # nomes[j]
                        font=list(size=19))
  fig
  
  
  nomefig <- paste0("fig", j) #concatena (junta)
  assign(nomefig,fig)  
  
  
  
  
}

fig3
fig4
fig5
fig7
fig8
fig9
fig10
fig11
fig12
fig13
fig14
fig15
fig16
fig17
fig18
fig19
fig20




# eixoX = -3,-2,-1 (antes) 0,1,2,3,4 (durante) 5,6,7 (depois)
# dia 0 = inicio do EPU


#fig <- fig %>% layout(xaxis = list(
#  dtick = 1, #"variar de 1 em 1"
#  tickmode = "linear"))


# Mudar os eixos: https://plotly.com/r/tick-formatting/



#https://plotly.com/r/line-charts/
#https://plotly.com/r/filled-area-plots/  
#https://plotly.com/r/reference/#scatter-fill


#### Termina O3####
