

library(readxl)
library(tseries)
library(Kendall)
library(dplyr)
library(lubridate)

source("Function_Simulation_Invest.R")



# planilha Resumo
List_funds<-read_xlsx("Quote_History_BB/#LISTA_FUNDOS.xlsx")

# inicio Looping Análise
for (ff in 1:nrow(List_funds)) {
  if(!is.na(List_funds$Nome_Arquivo[ff])){
    print(paste("Linha:",ff,List_funds$Nome_Do_Fundo[ff]))
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
    plot(gfgf)
    
    tab_price$p_Value<-0
    tab_price$TAU<-0
    tab_price$status<-NA
    tab_price$estationary<-NA
    
    tab_price$p_Value_Money <-0
    tab_price$TAU_Money     <-0
    tab_price$Signal_Money  <-NA
    
    for (ii in 61: nrow(tab_price)) {
      
      result      <- MannKendall(tab_price$Open[(ii-30):(ii)])
      Trend_Money <- MannKendall(tab_price$gfgf[(ii-30):(ii)])
      
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
      if (result$sl[1] < 120) { # pvlue 00.1
        
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
      if (Trend_Money$sl[1] < 120) {
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
      
      Time_Buy01<-round(min((sum(tab_price$status == "comprado",na.rm = TRUE)/nrow(tab_price))*100*1.1,100),0)
      Time_Buy01<-diferenca_anos*Time_Buy01/100
      Time_Buy01
      
      Soma01<-sum(tab_price$sum,na.rm = TRUE)
      Soma01
      
      Gain_By_Year01<-round(((Soma01/tab_price$Open[1]*100)/Time_Buy01)*0.77,2) # 0.77 23% de imposto estimativa Iof e IR
      Gain_By_Year01
      
      plot(cumsum(tab_price$sum[]))
    
      
      # se o dinheiro sair pode significar que outro setor pode estar mais atrativo
      # Se dinheiro sair pode ser uma queda nos preço
      # Trend Money
      for (dd in 81:nrow(tab_price)) {
        #dd
        if(!is.na(tab_price$Signal_Money[dd])){
          if(tab_price$Signal_Money[dd]=="Cash in"){tab_price$Sum_Trend_Money[dd] <-    tab_price$Open[dd+1]-tab_price$Open[dd]}
        }
      }
      
      Time_Buy02<-round(min((sum(tab_price$Signal_Money == "Cash in",na.rm = TRUE)/nrow(tab_price))*100*1.1,100),0)
      Time_Buy02<-diferenca_anos*Time_Buy02/100
      Time_Buy02
      
      Soma02<-sum(tab_price$Sum_Trend_Money,na.rm = TRUE)
      Soma02
      
      Gain_By_Year02<-round(((Soma02/tab_price$Open[1]*100)/Time_Buy02)*0.77,2) # 0.77 23% de imposto estimativa Iof e IR
      Gain_By_Year02
      
      #plot(cumsum(tab_price$Sum_Trend_Money[]))
      
      #Trend Quote and Money
      for (dd in 81:nrow(tab_price)) {
        #dd
        if(!is.na(tab_price$status[dd]) & !is.na(tab_price$Signal_Money[dd])){
          if(tab_price$Signal_Money[dd]=="Cash in" & tab_price$status[dd]=="comprado"){
            tab_price$Sum_Trend_Quote_and_Money[dd] <- tab_price$Open[dd+1]-tab_price$Open[dd]}
        }
      }
      
      Time_Buy03<-round(min((sum(tab_price$Signal_Money == "Cash in" & tab_price$status == "comprado",na.rm = TRUE)/nrow(tab_price))*100*1.1,100),0)
      Time_Buy03<-diferenca_anos*Time_Buy03/100
      Time_Buy03
      
      Soma03<-sum(tab_price$Sum_Trend_Quote_and_Money,na.rm = TRUE)
      Soma03<-Soma03*ifelse(Soma03>=0,0.77,1) # 0.77 23% de imposto estimativa IOF e IR
      Soma03

      Gain_By_Year03<-round(((Soma03/tab_price$Open[1]*100)/Time_Buy03),2)
      Gain_By_Year03
      
      plot(cumsum(tab_price$Sum_Trend_Quote_and_Money[]))
      
      # Grafico Geral
      #plot(tab_price$Open)
      EST_01<-ifelse(tab_price$status[nrow(tab_price)]=="comprado","comprado","neutro")
      EST_03<-ifelse(tab_price$status[nrow(tab_price)]=="comprado" & tab_price$Signal_Money[nrow(tab_price)] == "Cash in","comprado","neutro")
      
      Tabela_Resumo00<-Result_Invest(tab_price,3)
      #
      
      if(ff==1){Tabela_Resumo<-Tabela_Resumo00
      }else{
        Tabela_Resumo<-rbind(Tabela_Resumo,Tabela_Resumo00)
      }
  }

}


saveRDS(Tabela_Resumo,paste0("PerformanceStrategies/",Sys.Date()," - Desempenho Fundos BB EST01 sem pvlue.rds"))






# Criando um exemplo de data.frame
df <- tab_price

ESTRATEGIA<-3
if(ESTRATEGIA==1){df$status <-ifelse( df$status == 'comprado',"comprado","neutro")}
if(ESTRATEGIA==3){df$status <-ifelse(df$Signal_Money == "Cash in" & df$status == 'comprado',"comprado","neutro")}

transacoes <- data.frame(data_entrada=as.Date(character()), preco_entrada=numeric(), data_saida=as.Date(character()), preco_saida=numeric())
compra_ativa <- FALSE
data_entrada <- NA
preco_entrada <- NA


for (i in 1:nrow(df)) {
  if(!is.na(df$status[i])){
  if (df$status[i] == 'comprado' && !compra_ativa) {
    print(i)
    compra_ativa <- TRUE
    data_entrada <- df$Data[i]
    preco_entrada <- df$Open[i]
  } else if ((df$status[i] == 'neutro' || i == nrow(df)) && compra_ativa) {
    data_saida <- df$Data[i]
    preco_saida <- df$Open[i]
    
    transacao <- data.frame(data_entrada=data_entrada, preco_entrada=preco_entrada, data_saida=data_saida, preco_saida=preco_saida)
    transacoes <- rbind(transacoes, transacao)
    
    # Reset
    compra_ativa <- FALSE
    data_entrada <- NA
    preco_entrada <- NA
  }
}
}

# Calculando a valorização
transacoes$tempo_dias           <- as.numeric(difftime(transacoes$data_saida,transacoes$data_entrada, units = "days"))
transacoes$tempo_anos           <- as.numeric(difftime(transacoes$data_saida,transacoes$data_entrada, units = "weeks") / 52.25)
transacoes$valorizacao          <- (transacoes$preco_saida - transacoes$preco_entrada) / transacoes$preco_entrada * 100
transacoes$valorizacao_ajustada <-transacoes$valorizacao
#IOF e IR
for (kk in 1:nrow(transacoes)) {
  
  if(transacoes$valorizacao[kk]>0 & transacoes$tempo_dias[kk]>30){
    transacoes$valorizacao_ajustada[kk]<-transacoes$valorizacao_ajustada[kk]*0.85
  }
  
  if(transacoes$valorizacao[kk]>0 & transacoes$tempo_dias[kk]<=30){
    transacoes$valorizacao_ajustada[kk]<-transacoes$valorizacao_ajustada[kk]*0.85*transacoes$tempo_dias[kk]/30 # IR e IOF
  }
  
}
#transacoes$valorizacao_media <-transacoes$valorizacao/transacoes$tempo_anos

transacoes$acumulado <- cumprod(1+transacoes$valorizacao/100)
transacoes$acumulado_ajustado <- cumprod(1+transacoes$valorizacao_ajustada/100)


Time_Year<-sum(transacoes$tempo_anos)
Time_Year
Rendimento_Month<-round(((transacoes$acumulado_ajustado[nrow(transacoes)])^(1/(sum(transacoes$tempo_anos)*12))-1)*100,2)
Rendimento_Month
Rendimento_Year<-round(((transacoes$acumulado_ajustado[nrow(transacoes)])^(1/sum(transacoes$tempo_anos))-1)*100,2)
Rendimento_Year

Simulação_Invest<- data_frame(
  "Fundo"=df$Fundo[1],
  "Estratégia"=ESTRATEGIA,
  "Tempo de Investimento (anos)"=Time_Year,
  "Rendimento por Mês"=Rendimento_Month,
  "Rendimento por Ano" = Rendimento_Year)

Simulação_Invest
print((transacoes$acumulado_ajustado[nrow(transacoes)]-1)*100)




















