
#1 tabela que mostre a ocorrencia de ultrapassagens
#2 somar a quantidade de ultapassagens em uma das estações, para cada dia do ano

# Preparando a basse de dados principal

MMM_MP10=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/6 Ultrapassagens R/Dados MMM/Editados/MP10_max_diaria_media_movel_24h_2005-2021.CSV",header=T)

#para abrir o doc. tive que APAGAR a 1 linha abrindo os dados no Bloco de Notas

MMM_MP10<- MMM_MP10 [ , -17]

colnames(MMM_MP10)[1]="data"
MMM_MP10$data <-paste(MMM_MP10$y,MMM_MP10$m,MMM_MP10$d, sep = "_")

library(lubridate)
MMM_MP10$data <-ymd(MMM_MP10$data)

MMM_MP10 <- MMM_MP10[ ,-c(2,4)]

w<- data.frame(MMM_MP10)
w[ ,3:14]<-NA


#Tabela com os dias em que houveram ultrapassagens(1) ou não(0), em TODAS as Estações 
 # Quem vai receber w[,2:13]
 # Filtro [MMM_MP10[,2:13] >= 45]
 # O que vai receber <- 1

w[,3:14][MMM_MP10[,3:14] >= 45] <- 1
w[,3:14][MMM_MP10[,3:14] == 'NaN'] <- NA
w[,3:14][MMM_MP10[,3:14] < 45] <- 0

caminho <- "G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/"
#saveRDS(f,file=paste0(caminho,"epu_mp10_classific.Rds"))
write.csv(w, file = paste0(caminho,"UltrapasMP10.csv"), row.names = FALSE)


######################### HEATMAPs ############### #####################

#### OBJ: quero fazer um heatmap que mostre o
# num de total ultrapassagens em cada estação, para cada mes
#Número de ultrapassagens para cada mes identificadas entre 2005 e 2020


Soma_Mensal<-data.frame(mes=1:12, matrix(NA,12,12))
colnames(Soma_Mensal)[2:13]<- colnames(w[3:14])

#which: encontrar a posição de um elemento
for (iest in 3:14) {
  for (imes in 1:12) {
    linh <- which(w$m==imes)
    Soma_Mensal[imes,iest-1] <- sum(w[linh,iest], na.rm = TRUE)
  }
}

library(plotly)

somaMes <- t(as.matrix(Soma_Mensal)) 
fig <- plot_ly(x = Soma_Mensal$mes,
               y = colnames(Soma_Mensal)[2:13],
               z = somaMes[2:13,], type = "heatmap")

#title = 'Ultrapassagens MP10'
fig<- fig %>% layout(title = list(text ='<b> MP10 </b>', 
                                  font = list(size = 17)))

fig<-fig %>% layout(font=list(size=17))

fig

                        ############ 

#### OBJ: saber a quanti de ultrapassagens em cada mês,
#em cada uma da estações!!!

#### quanti de ultrapassagens em cada mês, para cada estação! ######

CobDados<- data.frame(mes=seq(ymd('2005-01-01'),ymd('2021-12-31'),by='months'))
CobDados[, 2:13]<-NA
colnames(CobDados)[2:13]<- colnames(w[3:14])
w$mesano<- ym (paste(year(w[,1]),w$m, sep="_"))

for (i in 1:nrow(CobDados)){
  for (im in 3:14) {
    lin <-which(w$mesano==CobDados$mes[i])
    CobDados[i,im-1]<-sum(w[lin,im],na.rm = TRUE)
  } 
}

#### Heatmap da quanti de ultrapassagens em cada mês, para cada estação! ######

ultrapassagens <- t(as.matrix(CobDados)) #t (matrix transposta - "linha vira coluna, viceversa")
fig2 <- plot_ly(x = CobDados$mes,
                y = colnames(CobDados)[2:13],
                z = ultrapassagens[2:13,], type = "heatmap")

#title = 'ultrapassagens MP10'
fig2<- fig2 %>% layout(title = list(text ='<b> MP10 </b>', 
                                  font = list(size = 17)))

fig2<-fig2 %>% layout(font=list(size=17))

fig2


#fig2 <- fig2 %>% layout(legend=list(title=list(text=' Numero de  ultrapassagens ')))


#################### ######################################## #################


#Numero de estações em que houveram ultrapassagens, para cada dia do ano 
 
f<-data.frame(MMM_MP10)
f<-f [ -c(3:14)]
f[,2]<-NA
   #(f)[2]="Num de estações com ultrapassagens naquele dia"
colnames(f)[2]="Num Est ultrapassagem"

 # Soma os valores linha por linha, ignorando os NA (valores ausentes) 
f[,2]<-rowSums(w[,3:14],na.rm = T)


 # soma o num de colunas, linha por linha, ignorando os NA (valores ausentes)
  #(f)[3]="Estações que estavão monitorando naquele dia"

f[,3]<-rowSums(!is.na(w[,3:14]))
colnames(f)[3]="Num Est Monitorando"


# (1) EPUs 50% das estacoes com ultrapassagens + (2) frequencia de 5 dias

# (1)
  #(f)[4]="Dias potencialmentes EPUs - Tinha METADADE
  #das estações com ultrapassagens"


  # Quem vai receber: f[,4]
  # Filtro: [f[,2]>=f[,3]/2]  
          # "SE a coluna 2 do f, for >= que a METADA da coluna 3 do f"
  # O que vai receber: <- 1

f[,4]<-0
colnames(f)[4]="50% esta ultrapassagens"
f[,4][f[,2]>=f[,3]/2]<-1


# (2)

#frequencia de 5 dias com Ultrapassagens
f[,5]<-0 #soma movel de 5 dias
colnames(f)[5]="soma movel de 5 dias"

for (i in 1:nrow(f)) {
  f[i,5]<-sum(f[i:(i+4),4])
}
f[6206:6209,5]<-0



f[,6]<-0 #dias com EPUs
colnames(f)[6]="EPUs MP10"


#SE o loop encontrar um 5 na coluna 5, vai fazer tudo o q tiver no segundo while,
#SE não (else), vai pra próxima linha

lin<-1
while(lin<nrow(f)){
  if (f[lin,5]==5){
    f[lin,6]<-1 #comecou 1 EPU 
    while (f[lin,4]==1) {
      f[lin,6]<-1
      lin<-lin+1
    }
  }else {lin<-lin+1}
} 


#epu_mp10_clf <- data.frame(mp10_ultrap[,1:4],f$`EPUs MP10`)
#colnames(epu_mp10_clf)[5] <- "epu_mp10"
caminho <- "G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/"
#saveRDS(f,file=paste0(caminho,"epu_mp10_classific.Rds"))
write.csv(f, file = paste0(caminho,"epu_mp10_classific.csv"), row.names = FALSE)




### lista de EPUs com inicio(col 1) e término(col 2) e duração(col 3)

#diff (derivada diferencial): Final - Inicial
difEPU <-diff(f[,6])


NumEPU <- length(which(difEPU==1))
NumEPU

EPUlist <-data.frame(matrix(0,122,3))

colnames(EPUlist)[1]="Início_do_EPU"
colnames(EPUlist)[2]="Término_do_EPU"
colnames(EPUlist)[3]="Duração_do_EPU"


lin<-(which(difEPU==1))+1
EPUlist[,1]<-f[lin,1] 

lin<-(which(difEPU==-1))
EPUlist[,2]<-f[lin,1] 

EPUlist[,3]<-(EPUlist[,2]-EPUlist[,1])+1 


EPUlist[,3]<-as.numeric(EPUlist[,3])

#ideal salvar separadamente esse ultimo código (EPUlist)

#write.table(f, file="ultrapassagensO3.csv", sep=",")

caminho <- "G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/"
#saveRDS(EPUlist,file=paste0(caminho,"list EPUs MP10.Rds"))
# saveRDS(EPUlist,file=paste0(caminho,"lista de EPUs.Rda")) # NÃO ABRE
write.csv(EPUlist, file = paste0(caminho,"list EPUs MP10.csv"), row.names = FALSE)



#install.packages("dplyr")
#library(dplyr)


#EPUlist %>% filter(Duração_do_EPU==6) %>% count
#EPUlist %>% filter(Duração_do_EPU==max) 

#EPUlist %>%
#  group_by(Duração_do_EPU) %>%
#  count



#A<-which(EPUlist$Duração_do_EPU==max(EPUlist$Duração_do_EPU))
#print(EPUlist$Duração_do_EPU[A])


