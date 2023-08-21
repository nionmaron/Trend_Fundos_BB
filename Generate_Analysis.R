
library(readxl)
library(tseries)
library(Kendall)
library(dplyr)


# planilha Resumo
List_funds<-read_xlsx("Quote_History_BB/#LISTA_FUNDOS.xlsx")

# inicio Looping Análise
for (ii in 1:nrow(List_funds)) {
  if(!is.na(List_funds$Nome_Arquivo[ii])){
    print(paste("Linha:",ii,List_funds$Nome_Do_Fundo[ii]))

  }

}


tab_price<-read_xlsx("Quote_History_BB/BB Acoes Construcao Civil FIC FI.xlsx") %>% arrange((Data))

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

