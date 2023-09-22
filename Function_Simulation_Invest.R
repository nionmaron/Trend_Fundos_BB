

# Função de classificação
classify_rendimento <- function(Rendimento,Rendimento_Referencia,Redimento_Fundo) {
  if (Rendimento < Rendimento_Referencia) {
    return("Bad")
  } else if (Rendimento > Rendimento_Referencia && Rendimento < Redimento_Fundo) {
    return("Neutral")
  } else {
    return("Good")
  }
}

# classify_rendimento(2.2,6,3)

Result_Invest<- function(INVESTMENT_TABLE,
                         FUND_NAME=NA,
                         STRATEGY,TREND_TIME,
                         IDENTIFICATION=NA,
                         ANALYSIS_TIME=NA,
                         QUOTE_START=0,
                         QUOTE_END=0,
                         CASH_WITHDRAWAL=0,
                         INVESTMENT_RISK="unknown",
                         INCOME_TAX="unknown"){
  
  # Criando um exemplo de data.frame
  
  df <- INVESTMENT_TABLE
  if(!is.na(ANALYSIS_TIME & ANALYSIS_TIME>365)){df<-df[df$Data>(Sys.Date() -ANALYSIS_TIME),]}
  
  #STRATEGY<-3
  if(STRATEGY==1){df$status <-ifelse( df$status == 'comprado',"comprado","neutro")}
  if(STRATEGY==3){df$status <-ifelse(df$Signal_Money == "Cash in" & df$status == 'comprado',"comprado","neutro")}
  
  transacoes <- data.frame(data_entrada=as.Date(character()), preco_entrada=numeric(), data_saida=as.Date(character()), preco_saida=numeric())
  compra_ativa <- FALSE
  data_entrada <- NA
  preco_entrada <- NA
  
  for (i in 1:(nrow(df)-1-QUOTE_END-CASH_WITHDRAWAL)) {
    if(!is.na(df$status[i])){
      if (df$status[i] == 'comprado' && !compra_ativa) {
        #print(i)
        compra_ativa <- TRUE
        data_entrada <- df$Data[i+1+QUOTE_START]
        preco_entrada <- df$Open[i+1+QUOTE_START]
      } else if ((df$status[i] == 'neutro' || i == (nrow(df)-1-QUOTE_END-CASH_WITHDRAWAL)) && compra_ativa) {
        data_saida <- df$Data[i+1+QUOTE_END+CASH_WITHDRAWAL]
        preco_saida <- df$Open[i+1+QUOTE_END]
        
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
  Rendimento_Month<-round(((transacoes$acumulado_ajustado[nrow(transacoes)])^(1/(sum(transacoes$tempo_anos)*12))-1)*100,2)
  Rendimento_Year<-round(((transacoes$acumulado_ajustado[nrow(transacoes)])^(1/sum(transacoes$tempo_anos))-1)*100,2)
  diferenca_anos <- as.numeric(difftime(max(df$Data), min(df$Data), units = "weeks") / 52.25)
  
  Rendimento_fund<- round((((df$Cota[nrow(df)] - df$Cota[2])/df$Cota[1]+1)^(1/diferenca_anos)-1)*100,2)
  
  if(Rendimento_fund>0){Rendimento_fund<-Rendimento_fund*0.85}
  #plot(df$Cota)
  yield_rating<-classify_rendimento(Rendimento_Year,6,Rendimento_fund)
  
  ss00<-df$status[nrow(df)]
  ss01<-df$status[nrow(df)-1]
  sm00<-df$Signal_Money[nrow(df)]
  sm01<-df$Signal_Money[nrow(df)-1]
  
  # parametros
  total_linhas<-nrow(transacoes)
  positivo<-nrow(transacoes[transacoes$valorizacao>0,])
  if(total_linhas>1){Acerto_perc<- round(positivo/total_linhas,2)*100}
  if(total_linhas<=1){Acerto_perc<- NA}
  
  # Start buying  / Buy  / Sell
  if(yield_rating=="Bad"){Decision<-"Sell"
  }else{
    if(STRATEGY==1){
      if(ss00=="comprado"){
        Decision<-ifelse(ss01=="comprado","Buy","Start buying")
      }else{
        Decision<-"Sell"
      }
    }
    if(STRATEGY==3){
      if(ss00=="comprado" &  sm00=="Cash in"){
        Decision<-ifelse(ss01=="comprado" & sm01=="Cash in","Buy","Start buying")
      }else{
        Decision<-"Sell"
      }
    }
  }
  
  if(Decision=="Sell"){Buy_In<-NA}
  if(Decision=="Buy"){Buy_In<-transacoes$data_entrada[total_linhas]}
  if(Decision=="Start buying"){Buy_In<-df$Data[nrow(df)]}

  
  Simulation_Invest<- data_frame(
    
    "ID"= IDENTIFICATION,
    "FUND_NAME"=FUND_NAME,
    "CNPJ"=df$CNPJ_FUNDO[1],
    "DAY_QUOTE"=paste0("D+",QUOTE_START," | D+",QUOTE_END," | D+",CASH_WITHDRAWAL),
    "Estratégia"=STRATEGY,
    "Tempo de Análise (Trend)"=TREND_TIME,
    "Número de Ordens por Ano"=round(nrow(transacoes)/diferenca_anos,2),
    "Tempo de Investimento (anos)"=Time_Year,
    "Rendimento por Mês"=Rendimento_Month,
    "Rendimento por Ano" = Rendimento_Year,
    "Tempo Do Fundo"=  diferenca_anos,
    "Redimento do Fundo (por ano)"=Rendimento_fund,
    "Classificação da Estratégia"= yield_rating,
    "Last Date"= df$Data[nrow(df)],
    "Last Trend Quote" = ss00,
    "Last Trend Equity" = sm00,
    "Decision"=Decision,
    "Buy_In"=as.Date(Buy_In),
    "Acerto_perc"=Acerto_perc,
    "Investment Risk"=INVESTMENT_RISK)
  
  return(Simulation_Invest)

}

