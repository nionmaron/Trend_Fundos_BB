

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
  #ff<-6
  if(!is.na(List_funds$Nome_Arquivo[ff])){
    ID00<-List_funds$ID[ff]
    print(paste("Linha:",ff,List_funds$ID[ff],List_funds$Nome_Do_Fundo[ff]))
    Link_Read<-paste0("Quote_History_BB/",List_funds$Nome_Arquivo[ff],".xlsx")
    
    # ler Arquivo
    print(Link_Read)
    tab_price<-read_xlsx(Link_Read) %>% arrange((Data))
    
    
    tab_price<-tab_price[tab_price$Data>(Sys.Date() -5*365 -90),]}
    
    #tab_price<-read_xlsx("Quote_History_BB/BB Acoes Dividendos Midcaps FIC FI.xlsx") %>% arrange((Data))
    tab_price$Open<-tab_price$Cota
    
    diferenca_anos <- as.numeric(difftime(max(tab_price$Data), min(tab_price$Data), units = "weeks") / 52.25)
    diferenca_anos
    
    # Acumulado Dinheiro
    gfgf<- cumsum(tab_price$Captação) - cumsum(tab_price$Resgate)
    tab_price$gfgf<- gfgf
    
    #gfgf<- cumsum(tab_price$Captação) - cumsum(tab_price$Resgate)
    # plot(gfgf)

    for (ddd in 6:40) {
      #ddd<-
    for (eest in c(1,3)) {
      
    print(paste("Linha:",ff," |Trend Time:",ddd,"|EST:",eest))
 
    tab_price$p_Value<-0
    tab_price$TAU<-0
    tab_price$status<-NA
    tab_price$estationary<-NA
    
    tab_price$p_Value_Money <-0
    tab_price$TAU_Money     <-0
    tab_price$Signal_Money  <-NA
    
    for (ii in 61: nrow(tab_price)) {
      
      result      <- MannKendall(tab_price$Open[(ii-ddd):(ii)])
      Trend_Money <- MannKendall(tab_price$gfgf[(ii-ddd):(ii)])
      
      #tste_est<-adf.test(tab_price$Open[(ii-500):(ii)])
      #tab_price$estationary[ii]<- tste_est$p.value[1]
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
      
      
      #plot(cumsum(tab_price$Sum_Trend_Money[]))
      
      #Trend Quote and Money
      for (dd in 81:nrow(tab_price)) {
        #dd
        if(!is.na(tab_price$status[dd]) & !is.na(tab_price$Signal_Money[dd])){
          if(tab_price$Signal_Money[dd]=="Cash in" & tab_price$status[dd]=="comprado"){
            tab_price$Sum_Trend_Quote_and_Money[dd] <- tab_price$Open[dd+1]-tab_price$Open[dd]}
        }
      }
      
      
      # Grafico Geral
      #plot(tab_price$Open)
      EST_01<-ifelse(tab_price$status[nrow(tab_price)]=="comprado","comprado","neutro")
      EST_03<-ifelse(tab_price$status[nrow(tab_price)]=="comprado" & tab_price$Signal_Money[nrow(tab_price)] == "Cash in","comprado","neutro")
      
      ###########################################################
      # INVESTMENT_TABLE=tab_price,STRATEGY=eest,TREND_TIME=ddd,IDENTIFICATION=ID00,ANALYSIS_TIME=NA,QUOTE_START=0,QUOTE_END=0,CASH_WITHDRAWAL=0,INCOME_TAX=0
      INVESTMENT_RISK<-List_funds$INVESTMENT_RISK[ff]
      Tabela_Resumo00<-Result_Invest(INVESTMENT_TABLE=tab_price,
                                     STRATEGY=eest,
                                     TREND_TIME=ddd,
                                     IDENTIFICATION=ID00,
                                     ANALYSIS_TIME=NA,
                                     QUOTE_START=ifelse(is.na(List_funds$QUOTE_START[ff]),0,List_funds$QUOTE_START[ff]),
                                     QUOTE_END=ifelse(is.na(List_funds$QUOTE_END[ff]),0,List_funds$QUOTE_END[ff]),
                                     CASH_WITHDRAWAL=ifelse(is.na(List_funds$CASH_WITHDRAWAL[ff]),0,List_funds$CASH_WITHDRAWAL[ff]),
                                     INVESTMENT_RISK=INVESTMENT_RISK,
                                     INCOME_TAX=0)
      
      if(ff==1 & ddd==6){Tabela_Resumo<-Tabela_Resumo00
      }else{
        Tabela_Resumo<-rbind(Tabela_Resumo,Tabela_Resumo00)
      }
    }
   }
  }




#Tabela_Resumo<-`2023-08-28 - Desempenho Fundos BB EST 01 e 03`

sum((Tabela_Resumo$`Tempo de Investimento (anos)`/sum(Tabela_Resumo$`Tempo de Investimento (anos)`))*Tabela_Resumo$`Rendimento por Ano`)
sum((Tabela_Resumo$`Tempo Do Fundo`/sum(Tabela_Resumo$`Tempo Do Fundo`))*Tabela_Resumo$`Redimento do Fundo (por ano)`)

saveRDS(Tabela_Resumo,paste0("PerformanceStrategies/",Sys.Date()," -D ALL YEARS Desempenho Fundos BB EST 01 e 03.rds"))


#--------------------------------------------------------------------------------------------------------------------------------------------
# Análise dos melhores parametros

Tabela_Resumo <-`2023-08-31 - ALL TIME Desempenho Fundos BB EST 01 e 03`
# Listar fundos
List_funds <- unique(Tabela_Resumo$Fundo)
List_funds

for (fl in 1:length(List_funds)) {
  
    print(List_funds[fl])
    peak_DF<-Tabela_Resumo[Tabela_Resumo$Fundo==List_funds[fl],]
    
  
    for (eest in c(1,3)) {
    peak_DF_03<-peak_DF[peak_DF$Estratégia==eest,]
    peak_DF_03<-peak_DF_03[,c("Tempo de Análise (Trend)","Rendimento por Ano")]
    names(peak_DF_03)<-c("Trend_Days","Redimento")
    
    # Ajustar o modelo de regressão polinomial 
    model <- lm(Redimento ~ poly(Trend_Days, 20, raw=TRUE), data=peak_DF_03)
    
    # Gerar uma sequência densa de valores de 'day'
    dense_days <- seq(7, 40, by=0.2)
    
    # Calcular rendimentos previstos para cada valor de 'day'
    predicted_rendimentos <- predict(model, data.frame(Trend_Days=dense_days))
    
    # Identificar o valor máximo e seu índice
    max_index <- which.max(predicted_rendimentos)
    max_day <- dense_days[max_index]
    max_rendimento <- predicted_rendimentos[max_index]
    
    cat("O 'day' que maximiza o rendimento é:", max_day, "com um rendimento estimado de:", max_rendimento, "\n")
    
    # Plotar
    #ggplot(peak_DF_03, aes(x=Trend_Days, y=Redimento)) + geom_point() + geom_smooth(method="lm", formula= y ~ poly(x, 20, raw=TRUE))
    
    
    Best_Parameters00<- data.frame("ID"=peak_DF$ID[1],
                                   "Fund"= List_funds[fl],
                                   "Strategy" = eest,
                                   "Time Days"= round(max_day,0),
                                   "Expected Yield Year"= round(max_rendimento,2))
    
    if(fl==1 & eest==1){Best_Parameters<-Best_Parameters00} else{Best_Parameters<-rbind(Best_Parameters,Best_Parameters00)}
    
    }
    
}

mean(Best_Parameters$Expected.Yield.Year)

# Filtrar pela linha com o maior rendimento para cada fundo
Chosen_Parameters<- Best_Parameters %>%
  group_by(Fund) %>%
  top_n(1, Expected.Yield.Year) %>%
  ungroup()

mean(Chosen_Parameters$Expected.Yield.Year)

saveRDS(Chosen_Parameters,paste0("Results_Analyzes/",Sys.Date()," Chosen_Parameters.rds"))
write.csv2(Chosen_Parameters,paste0("Results_Analyzes/",Sys.Date()," Chosen_Parameters.csv"))


