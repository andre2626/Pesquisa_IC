#Juncao MedPol_CLP_VMet


### IAG ####
# Dados meteorologicos diários IAG 
Insolacao=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/9 IAG + Resumo/Base de dados trabalhados/back/Insolacao_2005-2021.CSV",header=T)
Irradiacao=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/9 IAG + Resumo/Base de dados trabalhados/back/Irradiacao_2005-2021.CSV",header=T)
Precipitacao=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/9 IAG + Resumo/Base de dados trabalhados/back/Precipitacao_2005-2021.CSV",header=T)
PressaoMED=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/9 IAG + Resumo/Base de dados trabalhados/back/PressaoMED_2005-2021.CSV",header=T)
TempMAX=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/9 IAG + Resumo/Base de dados trabalhados/back/TempMED_2005-2021.CSV",header=T)
TempMED=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/9 IAG + Resumo/Base de dados trabalhados/back/TempMAX_2005-2021.CSV",header=T)
UR=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/9 IAG + Resumo/Base de dados trabalhados/back/UR_2005-2021.CSV",header=T)
Ventos=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/9 IAG + Resumo/Base de dados trabalhados/back/Vento_2005-2021.CSV",header=T)



### CLP ####

# Dados da Camada Limite Planetária(CLP)
# ERA5 Reanalysis RMSP (-23.45 -23.70 -46.40 -46.80) 12:00LT
CLP=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/clp_2000-2021.CSV",header=T,skip=1)

CLP <- CLP[-c(1:1827),] #quero os dados de 2005 a 2021

colnames(CLP)[1]="data"
CLP$data <-paste(CLP$y,CLP$m,CLP$d, sep="_")

library(lubridate)
CLP$data <-ymd(CLP$data)
CLP$data<-as.character(CLP$data)

#CLP<-CLP[,-c(2:7)]
CLP<-CLP[,-c(2,4,5,6,7)]
CLP$m<-as.character(CLP$m)




### MP10 ####
MMM_MP10=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/6 Ultrapassagens R/Dados MMM/Todos poluentes/MP10_max_day_mavg_17estacoes.CSV",header=T)

mp10<- data.frame(MMM_MP10$data)
mp10[2] <- data.frame(rowMeans(MMM_MP10[2:18],na.rm=T))

#[] média do MP10 em relação a todas as estações
colnames(mp10)[2]= "MP10 (µg/m3) "
colnames(mp10)[1]= "data" 



### O3 ####
MMM_O3=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/6 Ultrapassagens R/Dados MMM/Todos poluentes/O3_max_day_mavg_17estacoes.CSV",header=T)

o3<- data.frame(MMM_O3$data)
o3[2] <- data.frame(rowMeans(MMM_O3[2:18],na.rm=T))

#[] média do MP10 em relação a todas as estações
colnames(o3)[2]= "O3 (µg/m3)" 
colnames(o3)[1]= "data" 


### CO ####
MMM_CO=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/6 Ultrapassagens R/Dados MMM/Todos poluentes/CO_max_day_mavg_17estacoes.CSV",header=T)

co<- data.frame(MMM_CO$data)
co[2] <- data.frame(rowMeans(MMM_CO[2:18],na.rm=T))

#[] média do MP10 em relação a todas as estações
colnames(co)[2]= "CO (ppm)" 
colnames(co)[1]= "data" 



### MP25 ####
MMM_MP25=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/6 Ultrapassagens R/Dados MMM/Todos poluentes/MP25_max_day_mavg_17estacoes.CSV",header=T)

mp25<- data.frame(MMM_MP25$MP25max_diaria_media_movel)
mp25[2] <- data.frame(rowMeans(MMM_MP25[2:18],na.rm=T))

#[] média do MP10 em relação a todas as estações
colnames(mp25)[2]= "MP2,5 (µg/m3)" 
colnames(mp25)[1]= "data" 


### NO2 ####
MMM_NO2=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/6 Ultrapassagens R/Dados MMM/Todos poluentes/NO2_max_day_mavg_17estacoes.CSV",header=T)

no2<- data.frame(MMM_NO2$NO2max_diaria_media_movel)
no2[2] <- data.frame(rowMeans(MMM_NO2[2:18],na.rm=T))

#[] média do MP10 em relação a todas as estações
colnames(no2)[2]= "NO2 (µg/m3)" 
colnames(no2)[1]= "data"



### SO2 ####
MMM_SO2=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/6 Ultrapassagens R/Dados MMM/Todos poluentes/SO2_max_day_mavg_17estacoes.CSV",header=T)

so2<- data.frame(MMM_MP25$MP25max_diaria_media_movel)
so2[2] <- data.frame(rowMeans(MMM_SO2[2:18],na.rm=T))

#[] média do MP10 em relação a todas as estações
colnames(so2)[2]= "SO2 (µg/m3)" 
colnames(so2)[1]= "data"



library(dplyr) 
tes<- CLP%>% full_join(Ventos) %>% full_join(Insolacao) %>% full_join(Irradiacao) %>%
  full_join(Precipitacao) %>% full_join(PressaoMED) %>% full_join(TempMAX) %>%
  full_join(TempMED) %>% full_join(UR) %>%  full_join(mp10) %>% full_join(o3) %>%
  full_join(co) %>% full_join(mp25) %>% full_join(no2) %>% full_join(so2) 
  

#[,5:16][MMM_MP10[,5:16] == 'NaN'] <- NA

# Quem vai receber w[,2:13]
# Filtro [MMM_MP10[,2:13] >= 45]
# O que vai receber <- 1

tes[,1:ncol(tes)][tes[,1:ncol(tes)] == 'NaN'] <- NA #os dados faltantes p os poluentes eram indicados por "NAN"

#which(tes$Insolacao<0)
tes$Insolacao[tes$Insolacao<0]<-NA #valores (-99,9) indicam falha no registro do heliógrafo (Insolacao)

#which(tes$Irradiacao==0)
tes$Irradiacao[tes$Irradiacao==0]<-NA #valores 0 indicam falha no registro do actinógrafo (Irradiacao)



caminho <- "G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/"
#saveRDS(f,file=paste0(caminho,"epu_mp10_classific.Rds"))
write.csv(tes, file = paste0(caminho,"MedPol_CLP_VMet.csv"), row.names = FALSE)



###### termina ##



