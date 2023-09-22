
# Investment Fund Analysis Project of Banco do Brasil

library(readxl)
library(tseries)
library(Kendall)
library(dplyr)
library(lubridate)
library(ggplot2)
library(httr)
library(magrittr)
library(xml2)
library(rvest)

# Limpeza dos dados
rm(list = ls(all.names = TRUE)) # Limpar todos objetos do R
getwd() # Verificar local do diretorio


#--------------------------------------------------------------------------------------------------------------------------------------------
# ETAPA00 PASTA DO BANCO DE DADOS

diretorio_projeto <- getwd()

# Verifique se a pasta "Quote_History_FI" existe no diretório do projeto
if (!dir.exists(file.path(diretorio_projeto, "Quote_History_FI"))) {
  # Se não existe, crie a pasta
  dir.create(file.path(diretorio_projeto, "Quote_History_FI"))
  cat("Pasta 'Quote_History_FI' criada com sucesso!\n")
} else {
  cat("A pasta 'Quote_History_FI' já existe.\n")
}

#--------------------------------------------------------------------------------------------------------------------------------------------
# ETAPA01 DOWLOAD DADOS ATUALIZADOS

url_raw <- "https://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/"
output_file <- "informe_diario.zip"

zip_links <- httr::GET(url_raw) %>% 
  httr::content("text") 

zip_links2<-zip_links %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href")

zip_links <- zip_links2[which(grepl(x = zip_links2, pattern = "*.zip"))]
zip_links
if(length(zip_links)>1){
  print(" Dowload e Leitura")
  
  penultimo_Zip <- zip_links[length(zip_links)-1]
  ultimo_zip    <- zip_links[length(zip_links)]

  download_link00 <- paste0(url_raw, penultimo_Zip )
  download_link01 <- paste0(url_raw, ultimo_zip)
  
  download.file(download_link00, destfile = penultimo_Zip )
  download.file(download_link01, destfile = ultimo_zip )
  
  # Especifique o nome do arquivo CSV dentro do arquivo ZIP
  nome_arquivo_csv00 <- sub(".zip",".csv",penultimo_Zip)
  nome_arquivo_csv01 <- sub(".zip",".csv",ultimo_zip)
  
  # Descompacte o arquivo ZIP em um diretório temporário
  temp_dir <- tempdir()
  unzip(penultimo_Zip, exdir = temp_dir)
  unzip(ultimo_zip, exdir = temp_dir)
  
  # Leia o arquivo CSV descompactado
  caminho_arquivo_csv00 <- file.path(temp_dir, nome_arquivo_csv00)
  caminho_arquivo_csv01 <- file.path(temp_dir, nome_arquivo_csv01)
  
  dados00 <- read.csv2(caminho_arquivo_csv00)
  dados01 <- read.csv2(caminho_arquivo_csv01)
  dados<-rbind(dados00,dados01)
  
  # formatar
  dados$DT_COMPTC<-as.Date(dados$DT_COMPTC)
  dados$VL_TOTAL<-as.numeric(dados$VL_TOTAL)
  dados$VL_QUOTA<-as.numeric(dados$VL_QUOTA)
  dados$VL_PATRIM_LIQ<-as.numeric(dados$VL_PATRIM_LIQ)
  dados$CAPTC_DIA<-as.numeric(dados$CAPTC_DIA)
  dados$RESG_DIA<-as.numeric(dados$RESG_DIA)
  dados$NR_COTST<-as.numeric(dados$NR_COTST)
  
}

#--------------------------------------------------------------------------------------------------------------------------------------------
# ETAPA02 ATUALIZAR BANCO DE DADOS
Choose_List_RDS<-list.files("Quote_History_BB/",pattern = ".rds")
nnn<-length(Choose_List_RDS)
nnn

for (RR in 1:nnn) {
  #RR<-1
  print(paste(RR,":",nnn," |",Choose_List_RDS[RR]))
  Read_Link<-(paste0("Quote_History_BB/",Choose_List_RDS[RR]))
  tabela<-readRDS(Read_Link)
  
  date_filter<-max(tabela$DT_COMPTC)
  search_cnpj<-tabela$CNPJ_FUNDO[1]
  
  tab000<-dados[dados$CNPJ_FUNDO==search_cnpj,]
  tab001<-tab000[tab000$DT_COMPTC>date_filter,]
  tab001<-tab001[,2:9]
  if(nrow(tab001)>0){
    print(paste(search_cnpj," - Tabela atualizada a partir da data:",date_filter))
    tabela000<-rbind(tabela,tab001)
    # organizar
    tabela000<-tabela000[order(tabela000$DT_COMPTC),] 
    
    # formatar
    tabela000$DT_COMPTC<-as.Date(tabela000$DT_COMPTC)
    tabela000$VL_TOTAL<-as.numeric(tabela000$VL_TOTAL)
    tabela000$VL_QUOTA<-as.numeric(tabela000$VL_QUOTA)
    tabela000$VL_PATRIM_LIQ<-as.numeric(tabela000$VL_PATRIM_LIQ)
    tabela000$CAPTC_DIA<-as.numeric(tabela000$CAPTC_DIA)
    tabela000$RESG_DIA<-as.numeric(tabela000$RESG_DIA)
    tabela000$NR_COTST<-as.numeric(tabela000$NR_COTST)
    
    saveRDS(tabela000,Read_Link)
  }
}

#--------------------------------------------------------------------------------------------------------------------------------------------
# ETAPA03 PROCESSAR DADOS

# Limpeza dos dados
rm(list = ls(all.names = TRUE)) # Limpar todos objetos do R
getwd() # Verificar local do diretorio

source("Function_Simulation_Invest.R")

# planilha Resumo
List_funds<-read_xlsx("Quote_History_BB/#LISTA_FUNDOS.xlsx")
List_funds<-List_funds[!is.na(List_funds$CNPJ),]

# inicio Looping Análise
for (ff in 1:nrow(List_funds)) {
  if(!is.na(List_funds$CNPJ[ff])){
    #ff<-26
    ID00<-List_funds$ID[ff]
    INVESTMENT_RISK<-List_funds$INVESTMENT_RISK[ff]
    
    print(paste("Linha:",ff,List_funds$ID[ff],List_funds$Nome_Do_Fundo[ff]))
    #Link_Read<-paste0("Quote_History_BB/",List_funds$Nome_Arquivo[ff],".xlsx")
    Link_RDS<-paste0("Quote_History_BB/",sub("/",".",List_funds$CNPJ[ff]),".rds")
    
    
    
    # ler Arquivo
    #print(Link_Read)
    print(Link_RDS)
    #tab_price<-read_xlsx(Link_Read) %>% arrange((Data))
    tab_price<-readRDS(Link_RDS)  %>% arrange((DT_COMPTC))
    tab_price<-tab_price[tab_price$VL_QUOTA>0,]
    tab_price$VL_QUOTA<-as.numeric(tab_price$VL_QUOTA)
    tab_price$CAPTC_DIA<-as.numeric(tab_price$CAPTC_DIA)
    tab_price$RESG_DIA<-as.numeric(tab_price$RESG_DIA)
    
    tab_price$Data<-tab_price$DT_COMPTC
    tab_price$Cota<-tab_price$VL_QUOTA
 
    #tab_price<-read_xlsx("Quote_History_BB/BB Acoes Dividendos Midcaps FIC FI.xlsx") %>% arrange((Data))
    tab_price$Open<-(tab_price$VL_QUOTA)
    tab_price$Captação <- tab_price$CAPTC_DIA
    tab_price$Resgate <- tab_price$RESG_DIA
    
    diferenca_anos <- as.numeric(difftime(max(tab_price$Data), min(tab_price$Data), units = "weeks") / 52.25)
    diferenca_anos
    
    # Acumulado Dinheiro
    gfgf<- cumsum(tab_price$Captação) - cumsum(tab_price$Resgate)
    tab_price$gfgf<- gfgf
    
    gfgf<- cumsum(tab_price$Captação) - cumsum(tab_price$Resgate)
    #plot(gfgf)
    
    ddd  <-ifelse(!is.na(List_funds$Trend_Day[ff]),List_funds$Trend_Day[ff],26)
    eest <- ifelse(!is.na(List_funds$Best_Strategy[ff]),List_funds$Best_Strategy[ff],1)
    
    print(paste("Estratégia:",eest, "|", "Intervalo de Análise (dias):",ddd))
    
    
        tab_price$p_Value<-0
        tab_price$TAU<-0
        tab_price$status<-NA
        tab_price$estationary<-NA
        
        tab_price$p_Value_Money <-0
        tab_price$TAU_Money     <-0
        tab_price$Signal_Money  <-NA
        
        # Gerar analise da tendencia
        for (ii in 61: nrow(tab_price)) {
          
          result      <- MannKendall(tab_price$Open[(ii-ddd):(ii)])
          Trend_Money <- MannKendall(tab_price$gfgf[(ii-ddd):(ii)])
          
          # cota
          tab_price$TAU[ii]<-round(result$tau[1],5)
          tab_price$p_Value[ii]<-round(result$sl[1],5)
          
          # Money
          tab_price$TAU_Money[ii]     <-round(Trend_Money$tau[1],5)
          tab_price$p_Value_Money[ii] <-round(Trend_Money$sl[1],5)
          
          #print(ii)
          
          ##########################################################
          # Trend Quote
          if (result$sl[1] < 0.1) { # pvlue 00.1
            
            # Verifique se a tendência é de alta ou baixa
            if (result$tau > 0) {
              status <- "comprado"
            } else {
              status <- "neutro"
            }
            
          } else {
            status <- "neutro"
          }
          tab_price$status[ii] <- status
          
          ##########################################################
          # Trend Money 
          if (Trend_Money$sl[1] < 0.1) {
            # Verifique se a tendência é de alta ou baixa
            if (Trend_Money$tau > 0) {
              Signal_Money <- "Cash in"
            } else {
              Signal_Money <- "Cash out"
            }
          } else {
            Signal_Money <- "No Trend"
          }
          tab_price$Signal_Money[ii] <- Signal_Money
        }
        
        ############################################################################
        tab_price$status2<-NA
        tab_price$sum<-0
        
        tab_price$Sum_Trend_Money           <-0
        tab_price$Sum_Trend_Quote_and_Money <-0
        
        # Para as somas se ATENTAR as datas de cotização
        
        # Trend Quote
        for (dd in 81:nrow(tab_price)) {
          #dd
          if(!is.na(tab_price$status[dd])){
            if(tab_price$status[dd]=="comprado"){tab_price$sum[dd] <-   tab_price$Open[dd+1]-tab_price$Open[dd]}
            if(tab_price$status[dd]=="vendido"){tab_price$sum[dd]  <- -(tab_price$Open[dd+1]-tab_price$Open[dd])}
            
            if((tab_price$status[dd]=="neutro") & (tab_price$status[dd-1]=="vendido")){tab_price$status2[dd]<-"comprado"}
            if((tab_price$status[dd]=="neutro") & (tab_price$status[dd-1]=="comprado")){tab_price$status2[dd]<-"vendido"}
            if((tab_price$status[dd]=="neutro") & (tab_price$status[dd-1]=="neutro")){tab_price$status2[dd]<-tab_price$status2[dd-1]}
          }
        }
        
        
        # se o dinheiro sair pode significar que outro setor pode estar mais atrativo
        # Se dinheiro sair pode ser uma queda nos preço
        # Trend Money
        for (dd in 81:nrow(tab_price)) {
          #dd
          if(!is.na(tab_price$Signal_Money[dd])){
            if(tab_price$Signal_Money[dd]=="Cash in"){tab_price$Sum_Trend_Money[dd] <-    tab_price$Open[dd+1]-tab_price$Open[dd]}
          }
        }
        
        
        #Trend Quote and Money
        for (dd in 81:nrow(tab_price)) {
          #dd
          if(!is.na(tab_price$status[dd]) & !is.na(tab_price$Signal_Money[dd])){
            if(tab_price$Signal_Money[dd]=="Cash in" & tab_price$status[dd]=="comprado"){
              tab_price$Sum_Trend_Quote_and_Money[dd] <- tab_price$Open[dd+1]-tab_price$Open[dd]}
          }
        }
        
        if(eest==1){plot(cumsum(tab_price$sum[]))}
        if(eest==3){plot(cumsum(tab_price$Sum_Trend_Quote_and_Money[]))}

        
        # Grafico Geral
        #plot(tab_price$Open)
        EST_01<-ifelse(tab_price$status[nrow(tab_price)]=="comprado","comprado","neutro")
        EST_03<-ifelse(tab_price$status[nrow(tab_price)]=="comprado" & tab_price$Signal_Money[nrow(tab_price)] == "Cash in","comprado","neutro")
        
        
        List_funds$Nome_Do_Fundo[ff]
        
        
        print("concluido etapa 01")
        Tabela_Resumo00<-Result_Invest(INVESTMENT_TABLE=tab_price,
                                       FUND_NAME=List_funds$Nome_Do_Fundo[ff],
                                       STRATEGY=eest,
                                       TREND_TIME=ddd,
                                       IDENTIFICATION=ID00,
                                       ANALYSIS_TIME=NA,
                                       QUOTE_START=ifelse(is.na(List_funds$QUOTE_START[ff]),0,List_funds$QUOTE_START[ff]),
                                       QUOTE_END=ifelse(is.na(List_funds$QUOTE_END[ff]),0,List_funds$QUOTE_END[ff]),
                                       CASH_WITHDRAWAL=ifelse(is.na(List_funds$CASH_WITHDRAWAL[ff]),0,List_funds$CASH_WITHDRAWAL[ff]),
                                       INVESTMENT_RISK=INVESTMENT_RISK,
                                       INCOME_TAX=0)
        
        
        if(ff==1){Tabela_Resumo<-Tabela_Resumo00
        }else{
          Tabela_Resumo<-rbind(Tabela_Resumo,Tabela_Resumo00)
        }

  }
  
}

#--------------------------------------------------------------------------------------------------------------------------------------------
# ETAPA04 SALVAR OS DADOS

# periodo investido - média geral
sum((Tabela_Resumo$`Tempo de Investimento (anos)`/sum(Tabela_Resumo$`Tempo de Investimento (anos)`))*Tabela_Resumo$`Rendimento por Ano`)

# periodo do fundo - média geral
sum((Tabela_Resumo$`Tempo Do Fundo`/sum(Tabela_Resumo$`Tempo Do Fundo`))*Tabela_Resumo$`Redimento do Fundo (por ano)`)

saveRDS(Tabela_Resumo,paste0("PerformanceStrategies/",Sys.Date()," Investiment_Decision.rds"))


#--------------------------------------------------------------------------------------------------------------------------------------------
# ETAPA05 GERAR RELATÓRIO - OPORTUNIDADES
library(grid)
library(gridExtra)

names(Tabela_Resumo)
Colunas_select<-c("FUND_NAME","CNPJ","Estratégia","Decision","Buy_In","Investment Risk")
Comprados<-Tabela_Resumo[Tabela_Resumo$Decision!="Sell",][,Colunas_select]

{
# Abra um arquivo PDF para salvar a tabela
pdf( paste0("Results_Analyzes/",Sys.Date()," Resultados.pdf"),bg = "pink",width = 8, height = 12) #paper="a4"
  
  # Adicione texto ao PDF usando funções de gráficos regulares (adicionando um título)
  title <- "Relatório de Têndencia de Alta \n Fundos Selecionados Banco do Brasil"
  plot.new()
  text(0.5, 0.9, title, cex = 2, adj = 0.5)
  
  # Crie uma tabela a partir do dataframe
  grid.table(Comprados[])
  
  # Feche o dispositivo PDF
  dev.off()
}


#--------------------------------------------------------------------------------------------------------------------------------------------
# ETAPA05 ENVIAR PARA TELEGRAM



#--------------------------------------------------------------------------------------------------------------------------------------------
# RASCUNHO

FUND_NAME=List_funds$Nome_Do_Fundo[ff]
INVESTMENT_TABLE<-tab_price
STRATEGY<-eest
TREND_TIME<-ddd
IDENTIFICATION<-NA
ANALYSIS_TIME<-NA
QUOTE_START<-0
QUOTE_END<-0
CASH_WITHDRAWAL<-2
INVESTMENT_RISK<-"unknown"
INCOME_TAX<-"unknown"

