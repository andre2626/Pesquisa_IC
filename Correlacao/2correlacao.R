# NÂO RODAR TUDO!

STemptest=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/10 Camada Limite Planetária(CLP)/Dados Gerados/MedPol_CLP_VMet.CSV",header=T)

EPUclasmp10test=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/epu_mp10_classific.CSV",header=T)
EPUlistmp10test=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/7 Organização e Dados + importantes/Base de DADOS/list_EPUs_MP10.CSV",header=T)
MMM_MP10=read.csv("G:/Meu Drive/4 IC/1 Projeto/Reuniões/6 Ultrapassagens R/Dados MMM/MP10_max_diaria_media_movel_24h_2005-2021.CSV",header=T, skip=1)


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

# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/cor.test
corr.test(STemptest[,1],STemptest[,2])

test<-corr.test(STemptest[,1],STemptest[,2])
print(corr.p(test))
p.adj
####



# Correlações ####

#install.packages("ggcorrplot")
library(ggcorrplot)

# Compute a correlation matrix (Calcular uma matriz de correlação)
# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2


# na.omit: retira as linhas com NA
which(is.na(STemptest))
sum(is.na(STemptest))
sum(is.na(STemptest[3]))

corr <- round(cor(na.omit(STemptest)), 2)
head(corr[, 1:10])  #head: mostra o começo
corr[, 1:10]

# Compute a matrix of correlation p-values (Calcular uma matriz de valores p de correlação)
# p > 0,05: Não tem significancia
# p < 0,05: tem significancia
p.mat <- cor_pmat(na.omit(STemptest))
head(p.mat[, 1:10]) #head: mostra o começo
p.mat[, 1:10]


# Visualize the correlation matrix
# method = "square" (default)
ggcorrplot(corr)
#ggcorrplot(corr, method = "circle")


exemplo3 <- ggcorrplot(corr, 
                       p.mat = p.mat, # não significativos marcados com X
                       sig.level = 0.05, # escolha do nível de significância
                       hc.order = TRUE)
                       #insig = "blank")

exemplo3






############## outra opção

cor(na.omit(STemptest))
correlacao<-cor(na.omit(STemptest))

#install.packages("corrplot")
library(corrplot)

corrplot(correlacao)
corrplot(correlacao,method = "number")
corrplot(correlacao,method = "number", type = "upper") # type = lower, type = "upper"






############## outra opção

install.packages("Hmisc")
library(Hmisc)
m<-rcorr(as.matrix(na.omit(STemptest)))

m$r  # matriz com coef de CORRELAÇÃO
m$n  # numero de dados; ver se ha dados completos (n tao importante)
m$P  # matriz com valores P (significancia)

#graficos p melhor análise
corrplot(m$r,p.mat = m$P,sig.level = 0.005) #variáveis com velor P>0.005 estão com x
corrplot(m$r,p.mat = m$P,sig.level = 0.005,method = "number", type = "upper") #variáveis com velor P>0.005 estão com x




