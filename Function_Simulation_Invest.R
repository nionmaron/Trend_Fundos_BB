


Result_Invest<- function(X,ESTRATEGIA){
  # Criando um exemplo de data.frame
  df <- X
  
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
  
  Simulation_Invest<- data_frame(
    "Fundo"=df$Fundo[1],
    "Estratégia"=ESTRATEGIA,
    "Tempo de Investimento (anos)"=Time_Year,
    "Rendimento por Mês"=Rendimento_Month,
    "Rendimento por Ano" = Rendimento_Year)
  
  return(Simulation_Invest)
  
}



