


library(rvest)

########################################################################################
#

# URL do site alvo
url <- "https://www37.bb.com.br/portalbb/tabelaRentabilidade/rentabilidade/gfi7,802,9085,9089,1.bbx"

# Ler o conteúdo da página
pagina <- read_html(url)

# Extrair a tabela
elementos <- pagina %>%
  html_text()  # Este comando extrai o texto desses elementos. Se você quer o HTML cru, use html_content()

elementos
# A página pode conter várias tabelas, então `html_table` retorna uma lista de data.frames
# Supondo que a primeira tabela é a que você quer:
dados <- tabela[[1]]

# Visualizar os primeiros registros da tabela raspada
head(dados)

tabela <- pagina %>%
  html_nodes(xpath = "//table[3]") %>%
  html_table()

dados <- tabela[[1]]
print(dados)


########################################################################################
# Dados CVM

# URL do site alvo
url02 <- "https://cvmweb.cvm.gov.br/SWB/Sistemas/SCW/CPublica/InfDiario/CPublicaInfDiario.aspx?PK_PARTIC=53395&COMPTC="

# Ler o conteúdo da página
pagina02 <- read_html(url02)

# Extrair a tabela
elementos <- pagina02 %>%
  html_text()  # Este comando extrai o texto desses elementos. Se você quer o HTML cru, use html_content()

elementos

tabela02 <- pagina02 %>%
  html_nodes(xpath = "//table[2]") %>%
  html_table()

dados02 <- tabela02[[1]]
print(dados02)









