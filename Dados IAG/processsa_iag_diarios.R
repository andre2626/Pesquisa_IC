# Processa planilhas do IAG (dados diários)
#
# Input:
# Planilha de dados diários com abas separadas para cada ano  
#
# Output:
# saida (data, valores)  

library("readxl") # ler planilhas do excel
library("lubridate") # lidar com datas

##### Insola��o #####
## Pasta que contém a planilha:
caminho <- "G:/Meu Drive/4 IC/1 Projeto/Reuni�es/9 IAG + Resumo/Base de dados brutos/Insol 2005 a 2021/"
## Nome do arquivo da planilha:
nome <- "Insola��o 2005 a 2021.xls"
## Células do Excel que devem ser lidas:
cel <- "C7:N38"
## Nome da variável que será processada:
nomevar <- "Insolacao"

##### Irradia��o #####
## Pasta que contém a planilha:
caminho <- "G:/Meu Drive/4 IC/1 Projeto/Reuni�es/9 IAG + Resumo/Base de dados brutos/Irrad 2005 a 2021/"
## Nome do arquivo da planilha:
nome <- "Irradia��o 2005 a 2021.xls"
## Células do Excel que devem ser lidas:
cel <- "C7:N38"
## Nome da variável que será processada:
nomevar <- "Irradiacao"

##### Temp MAX #####
## Pasta que contém a planilha:
caminho <- "G:/Meu Drive/4 IC/1 Projeto/Reuni�es/9 IAG + Resumo/Base de dados brutos/Temp 2005 a 2021/"
## Nome do arquivo da planilha:
nome <- "Temp_Di�ria 2005 a 2021.xls"
## Células do Excel que devem ser lidas:
cel <- "C37:N68"
## Nome da variável que será processada:
nomevar <- "TempMAX"

##### Temp MEDIA #####
## Pasta que contém a planilha:
caminho <- "G:/Meu Drive/4 IC/1 Projeto/Reuni�es/9 IAG + Resumo/Base de dados brutos/Temp 2005 a 2021/"
## Nome do arquivo da planilha:
nome <- "Temp_Di�ria 2005 a 2021.xls"
## Células do Excel que devem ser lidas:
cel <- "C2:N33"
## Nome da variável que será processada:
nomevar <- "TempMED"


##### Precipitacao #####
## Pasta que contém a planilha:
caminho <- "G:/Meu Drive/4 IC/1 Projeto/Reuni�es/9 IAG + Resumo/Base de dados brutos/Prec 2005 a 2021/"
## Nome do arquivo da planilha:
nome <- "Prec 2005 a 2021.xls"
## Células do Excel que devem ser lidas:
cel <- "C7:N38"
## Nome da variável que será processada:
nomevar <- "Precipitacao"


##### Pressao MED #####
## Pasta que contém a planilha:
caminho <- "G:/Meu Drive/4 IC/1 Projeto/Reuni�es/9 IAG + Resumo/Base de dados brutos/Press 2005 a 2021/"
## Nome do arquivo da planilha:
nome <- "Press�o_Di�ria 2005 a 2021.xls"
## Células do Excel que devem ser lidas:
cel <- "C2:N33"
## Nome da variável que será processada:
nomevar <- "PressaoMED"


##### Umidade R DIA #####
## Pasta que contém a planilha:
caminho <- "G:/Meu Drive/4 IC/1 Projeto/Reuni�es/9 IAG + Resumo/Base de dados brutos/UR 2005 a 2021/"
## Nome do arquivo da planilha:
nome <- "Umid_di�ria 2005 a 2021.xls"
## Células do Excel que devem ser lidas:
cel <- "C2:N33"
## Nome da variável que será processada:
nomevar <- "UR"





# Inicializando a matriz que vai guardar os dados processados
saida <- data.frame(data=seq(ymd("2005-01-01"),ymd("2021-12-31"),by="day"))
saida[,2] <- NA
colnames(saida)[2]<-nomevar

lin <- 1 # contador de linhas da variável saida
for (ano in 2005:2021){
  temp1 <- read_excel(paste0(caminho,nome), sheet = as.character(ano), range=cel) # lê dados da planilha
  temp2 <- data.frame(unlist(temp1)) # converte múltiplas colunas em uma só
  temp2 <- na.exclude(temp2) # exclui NA (ex: 30/fev) (aqui estou supondo que não há dados faltantes na base do IAG)
  saida[lin:(lin+nrow(temp2)-1),2] <- temp2 # guarda os dados na coluna 2 da variável saida
  # o loop if abaixo serve para verificar se o último dia corresponde a 31/dez:
  ultimodia <- ymd(paste0(as.character(ano),"-12-31"))
  if (saida[(lin+nrow(temp2)-1),1] != ultimodia){
    print("Atenção! O último dia de dado não corresponde a 31/dez.")
    print("Verifique se há dados faltantes na planilha para o ano de:")
    print(ano)
  }
  # atualiza o valor da linha para escrever o próximo ano:
  lin <- lin+nrow(temp2)
}

# Salva o arquivo de saida
#saveRDS(saida,file=paste0(caminho,nomevar,"_2005-2021.Rda"))

caminho_saida <- "G:/Meu Drive/4 IC/1 Projeto/Reuni�es/9 IAG + Resumo/Base de dados trabalhados/"
#saveRDS(f,file=paste0(caminho,"epu_mp10_classific.Rds"))
write.csv(saida, file = paste0(caminho_saida,nomevar,"_2005-2021.csv"), row.names = FALSE)


