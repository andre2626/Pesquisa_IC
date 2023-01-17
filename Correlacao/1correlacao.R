# NÂO RODAR TUDO!

STemptest=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/MedPol_CLP_VMet.CSV",header=T)
#EPUclasmp10test=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/epu_mp10_classific.CSV",header=T)

STemptest <-STemptest[ ,-c(6,1,2,14:20)]


#### Matrizes de gráfico de dispersão - gráficos de base R ####
#http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs

# Sem pacote (olhar a img pq demora pra rodar)
#pairs(STemptest[,1:10],pch = 19)

# Pacote psych #teste de MULTICOLINEARIDADE
#install.packages("psych")
# https://www.rdocumentation.org/packages/psych/versions/2.2.5/topics/pairs.panels
library(psych)
pairs.panels(STemptest[,1:2], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE,
             stars = TRUE, #estrelinha quando haver significancia 
             alpha = 0.05,# qual o nível de significancia 
             lm = TRUE # show correlation ellipses # ajusta uma reta simples que se ajusta aos pontos
)

#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/cor.test
corr.test(STemptest[,1],STemptest[,2])

test<-corr.test(STemptest[,1],STemptest[,2])
print(corr.p(test))
p.adj
####



#### Testes ####
cor(STemptest$TempMAX,STemptest$TempMED)

STemptest=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/MedPol_CLP_VMet.CSV",header=T)
EPUclasmp10test=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/epu_mp10_classific.CSV",header=T)

STemptest <-STemptest[ ,-c(6,1,2,14:20)]

STemptest[,11] <-(EPUclasmp10test$EPUs.MP10)
colnames(STemptest)[11]="diasEPUmp10"

#STemptest <-STemptest[-c(100:6209),]


#### Matrizes de gráfico de dispersão - gráficos de base R ####
#http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs

# Sem pacote (olhar a img pq demora pra rodar)
#pairs(STemptest[,1:11],pch = 19)

# Pacote psych #teste de MULTICOLINEARIDADE
#install.packages("psych")
library(psych)
pairs.panels(STemptest[,1:11], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


#########

#Regressão linear múltipla no R
#https://www.youtube.com/watch?v=4YLOwyx_hxo&list=PLOw62cBQ5j9VE9X4cCCfFMjW_hhEAJUhU&index=29

#install.packages("readxl")
library(readxl)

#Para saber + sobre essa funsão
# https://www.datacamp.com/tutorial/linear-regression-R


#lm = regressão linear
#lm (variável DEPENDENTE) ~ + (variáveis INdependentes/ explicativas), nome da base de dados
mod<- lm(diasEPUmp10 ~ Insolacao + VelocVento+ Precipitacao , STemptest)


mod2<- lm(diasEPUmp10 ~ pblh.m. + ustar.m.s. + VelocVento + Insolacao + Irradiacao +Precipitacao + PressaoMED + TempMED + TempMAX + UR, STemptest)
#mod2<- lm(diasEPUmp10 ~ pblh.m. + VelocVento + Insolacao + Irradiacao + PressaoMED + TempMED + TempMAX + UR, STemptest)


summary(mod)
#Pr(>|t|) ou (valor de P) deve ser <0,05 para rejeitarmos a H0(hipótese nula) e
# afirmamos que há correlação!
# Pr(>|t|) <0,05 = o coeficiente tem correlação! (é importante/ tem significancia p o modelo)
# há evidência suficiente para apoiar a afirmativa da existência de uma correlação linear entre diasEPUmp10 e x (variavel Metorologica)



# Estimate: mostra o quão "forte" é essa correlação (seja NEG ou POSI)

# minha interpretação (VER SE TA CORRETO)
# VelocVento: a cada + 1 km, a chance de um dia de EPU de MP10 diminui(-) em 0.045 (4,5%)
# Insolacao: a cada + 1 H, a chance de um dia de EPU de MP10 AUMENTA(+) em 0.026 (2,6%)

summary(mod2)
# pq as estimativas mudaram?
# R-squared muitoo abaixo de 1 não é ruim?
