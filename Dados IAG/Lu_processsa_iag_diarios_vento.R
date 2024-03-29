# Processa planilhas do IAG (dados di�rios de vento)
#
# Input:
# Planilha de dados di�rios com abas separadas para cada ano  
#
# Output:
# saida (data, valores)  

library("readxl") # ler planilhas do excel
library("lubridate") # lidar com datas

# Pasta que cont�m a planilha:
caminho <- "C:/Users/Luciana/Documents/R/"
# Nome do arquivo da planilha:
nome <- "Vent_di�ria 2005 a 2021.xls"
# C�lulas do Excel que devem ser lidas:
cel <- "B10:BF40"
# Nome da vari�vel que será processada:
nomevar <- "Vento"

# Inicializando a matriz que vai guardar os dados processados
saida <- data.frame(data=seq(ymd("2005-01-01"),ymd("2021-12-31"),by="day"),
                    VelocVento=NA, DirVento=NA)

lin <- 1 # contador de linhas da vari�vel saida
for (ano in 2005:2021){
  temp1 <- read_excel(paste0(caminho,nome), sheet = as.character(ano), range=cel, col_names=FALSE) # l� dados da planilha
  temp2 <- data.frame(unlist(temp1[,seq(1,56,5)])) # converte m�ltiplas colunas em uma s�
  temp2 <- na.exclude(temp2) # exclui NA (ex: 30/fev) (aqui estou supondo que n�o h� dados faltantes na base do IAG)
  saida[lin:(lin+nrow(temp2)-1),3] <- temp2 # guarda os dados na coluna 3 da vari�vel saida
  temp3 <- data.frame(unlist(temp1[,seq(2,57,5)])) 
  temp3 <- na.exclude(temp3) 
  saida[lin:(lin+nrow(temp3)-1),2] <- temp3 # guarda os dados na coluna 2 da vari�vel saida
  
  # o loop if abaixo serve para verificar se o �ltimo dia corresponde a 31/dez:
  ultimodia <- ymd(paste0(as.character(ano),"-12-31"))
  if (saida[(lin+nrow(temp2)-1),1] != ultimodia){
    print("Aten��o! O �ltimo dia de dado n�o corresponde a 31/dez.")
    print("Verifique se h� dados faltantes na planilha para o ano de:")
    print(ano)
  }
  
  # atualiza o valor da linha para escrever o pr�ximo ano:
  lin <- lin+nrow(temp2)
  rm(temp1,temp2,temp3)
}

# Salva o arquivo de saida
saida[,2] <- as.numeric(saida[,2])
saveRDS(saida,file=paste0(caminho,nomevar,"_2005-2021.Rda"))
write.csv(saida,file=paste0(caminho,nomevar,"_2005-2021.csv"),row.names=FALSE)


