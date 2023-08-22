

library(readxl)
library(tseries)
library(Kendall)
library(dplyr)
library(lubridate)




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
    
    #tab_price<-read_xlsx("Quote_History_BB/BB Acoes Small Caps.xlsx") %>% arrange((Data))
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
      
      print(ii)
      
      ##########################################################
      # Trend Quote
      if (result$sl[1] < 0.05) {
        
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
      if (Trend_Money$sl[1] < 0.05) {
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
      
      plot(cumsum(tab_price$Sum_Trend_Money[]))
      
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
      Soma03
      
      Gain_By_Year03<-round(((Soma03/tab_price$Open[1]*100)/Time_Buy03)*0.77,2) # 0.77 23% de imposto estimativa Iof e IR
      Gain_By_Year03
      
      plot(cumsum(tab_price$Sum_Trend_Quote_and_Money[]))
      
      # Grafico Geral
      plot(tab_price$Open)
      
      Tabela_Resumo00<-data_frame("Nome Do Fundo"=tab_price$Fundo[ff],
                                  "Tempo do Histórico"=diferenca_anos,
                                  "Tempo de Investimento Est01"=Time_Buy01,
                                  "Desempenho por Ano Est01"=Gain_By_Year01,
                                  "Tempo de Investimento Est03"=Time_Buy03,
                                  "Desempenho por Ano Est03"=Gain_By_Year03)
      #
      
      if(ff==1){Tabela_Resumo<-Tabela_Resumo00
      }else{
        Tabela_Resumo<-rbind(Tabela_Resumo,Tabela_Resumo00)
      }
  }

}


saveRDS(Tabela_Resumo,"resumo.rds")

tab_price<-read_xlsx("Quote_History_BB/BB Multimercado Multigestor.xlsx") %>% arrange((Data))

tab_price$Open<-tab_price$Cota


gfgf<- cumsum(tab_price$Captação) - cumsum(tab_price$Resgate)

tab_price$gfgf<- gfgf

plot(gfgf)

tab_price$p_Value<-0
tab_price$TAU<-0
tab_price$status<-NA
tab_price$estationary<-NA

for (ii in 61: nrow(tab_price)) {
  
  result <- Kendall::MannKendall(tab_price$Open[(ii-30):(ii)])
  result <- Kendall::MannKendall(tab_price$Open[(ii-30):(ii)])
  
  #tste_est<-adf.test(tab_price$Open[(ii-500):(ii)])
  #tab_price$estationary[ii]<- tste_est$p.value[1]
  tab_price$TAU[ii]<-round(result$tau[1],5)
  tab_price$p_Value[ii]<-round(result$sl[1],5)
  print(ii)
  if (result$sl[1] < 0.05) {
    
    # Verifique se a tendência é de alta ou baixa
    if (result$tau > 0) {
      print("A série temporal está em tendência de alta significativa.")
      status <- "comprado"
    } else {
      print("A série temporal está em tendência de baixa significativa.")
      status <- "neutro"
    }
    
  } else {
    print("A série temporal não tem uma tendência significativa.")
    status <- "neutro"
  }
  
  tab_price$status[ii] <- status
  
}


############################################################################
tab_price$status2<-NA
tab_price$sum<-0

for (dd in 81:nrow(tab_price)) {
  dd
  if(!is.na(tab_price$status[dd])){
    if(tab_price$status[dd]=="comprado"){tab_price$sum[dd] <-   tab_price$Open[dd+1]-tab_price$Open[dd]}
    if(tab_price$status[dd]=="vendido"){tab_price$sum[dd]  <- -(tab_price$Open[dd+1]-tab_price$Open[dd])}
    
    if((tab_price$status[dd]=="neutro") & (tab_price$status[dd-1]=="vendido")){tab_price$status2[dd]<-"comprado"}
    if((tab_price$status[dd]=="neutro") & (tab_price$status[dd-1]=="comprado")){tab_price$status2[dd]<-"vendido"}
    if((tab_price$status[dd]=="neutro") & (tab_price$status[dd-1]=="neutro")){tab_price$status2[dd]<-tab_price$status2[dd-1]}
  }
  
}

sum(tab_price$sum,na.rm = TRUE)
plot(cumsum(tab_price$sum[]))

plot(tab_price$Open)


tab_price$sum2<-0

for (dd in 1:nrow(tab_price)) {
  if(!is.na(tab_price$status2[dd])){
    if(tab_price$status2[dd]=="comprado"){tab_price$sum2[dd] <-   tab_price$Open[dd+1]-tab_price$Open[dd]}
    if(tab_price$status2[dd]=="vendido"){tab_price$sum2[dd]  <- -(tab_price$Open[dd+1]-tab_price$Open[dd])}
    
  }
  
}


sum(tab_price$sum2, na.rm = TRUE)
plot(cumsum(tab_price$sum2[]))



####################################################
# Adicione uma coluna de tempo ao seu dataframe
tab_price$time <- 1:nrow(tab_price)
tab_price$slope<-"neutro"


for (dd in 61:nrow(tab_price)) {
  # Ajuste o modelo de regressão linear
  model <- lm(TAU ~ time, data = tab_price[(dd-7):(dd),])
  
  # Obtenha a inclinação da linha de regressão
  slope <- coef(model)[2]
  
  # Tomar decisão de compra ou venda com base na inclinação
  if (slope > 0) {
    tab_price$slope[dd]<-"comprado"
  } else {
    print("Vender!")
    tab_price$slope[dd]<-"vendido"
  }
}

tab_price$sum3<-0
for (dd in 1:nrow(tab_price)) {
  print(dd)
  if(!is.na(tab_price$slope[dd])){
    if(tab_price$slope[dd]=="comprado" & tab_price$p_Value[dd] < 0.01 & tab_price$TAU[dd]>0){tab_price$sum3[dd] <-   tab_price$Open[dd+1]-tab_price$Open[dd]}
    #if(tab_price$slope[dd]=="vendido" & tab_price$p_Value[dd]  < 0.01 & tab_price$TAU[dd]<0){tab_price$sum3[dd]  <- -(tab_price$Open[dd+1]-tab_price$Open[dd])}
  }
}

sum(tab_price$sum3, na.rm = TRUE)
plot(cumsum(tab_price$sum3[]))
plot(tab_price$Open)

