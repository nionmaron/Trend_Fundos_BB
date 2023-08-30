

library(readxl)
library(tseries)
library(Kendall)
library(dplyr)
library(lubridate)
library(ggplot2)

# Limpeza dos dados
rm(list = ls(all.names = TRUE)) # Limpar todos objetos do R
getwd() # Verificar local do diretorio

#--------------------------------------------------------------------------------------------------------------------------------------------
# 

# Funções
source("Function_Simulation_Invest.R")

# planilha Resumo
List_funds<-read_xlsx("Quote_History_BB/#LISTA_FUNDOS.xlsx")

# inicio Looping Análise
for (ff in 1:nrow(List_funds)) {
  if(!is.na(List_funds$Nome_Arquivo[ff])){
    #ff<-2
    ID00<-List_funds$ID[ff]
    print(paste("Linha:",ff,List_funds$ID[ff],List_funds$Nome_Do_Fundo[ff]))
    Link_Read<-paste0("Quote_History_BB/",List_funds$Nome_Arquivo[ff],".xlsx")
    
    # ler Arquivo
    print(Link_Read)
    tab_price<-read_xlsx(Link_Read) %>% arrange((Data))
    
    #tab_price<-read_xlsx("Quote_History_BB/BB Acoes Dividendos Midcaps FIC FI.xlsx") %>% arrange((Data))
    tab_price$Open<-tab_price$Cota
    
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
        
        print("concluido etapa 01")
        Tabela_Resumo00<-Result_Invest(tab_price,eest,ddd,ID=ID00)
        #
        
        if(ff==1){Tabela_Resumo<-Tabela_Resumo00
        }else{
          Tabela_Resumo<-rbind(Tabela_Resumo,Tabela_Resumo00)
        }

  }
  
}


sum((Tabela_Resumo$`Tempo de Investimento (anos)`/sum(Tabela_Resumo$`Tempo de Investimento (anos)`))*Tabela_Resumo$`Rendimento por Ano`)
sum((Tabela_Resumo$`Tempo Do Fundo`/sum(Tabela_Resumo$`Tempo Do Fundo`))*Tabela_Resumo$`Redimento do Fundo (por ano)`)

saveRDS(Tabela_Resumo,paste0("PerformanceStrategies/",Sys.Date(),"Investiment_Decisionrr.rds"))


