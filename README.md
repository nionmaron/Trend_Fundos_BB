# Investment Fund Analysis Project of Banco do Brasil

## Description / Descrição
This project aims to analyze the share prices of investment funds (FI) from Banco do Brasil. We use the Kendall correlation trend to identify favorable times for investments in these funds. / Este projeto tem como objetivo analisar a cotação das cotas dos fundos de investimentos (FI) do Banco do Brasil. Utilizamos a tendência de correlação de Kendall para identificar momentos propícios para aportes nos referidos fundos.

## Repository Structure / Estrutura do Repositório

**BrazilianEconomicTimeSeries:** Folder containing the inflation history. This information is used to define an average attractiveness rate for the strategy, aiming to outperform inflation. In this project, we set an annual yield goal of 6%. / *Pasta que contém o histórico de inflação. Essa informação é usada para definir uma taxa de atratividade média da estratégia, buscando superar a inflação. No contexto deste projeto, adotamos uma meta de rendimento anual de 6%.*

**Quote_History_BB:** In this folder, you'll find the fund's share price history, organized by CNPJ. / *Nesta pasta, você encontrará o histórico das cotações dos fundos, organizado por CNPJ.*

**Results_Analyzes:** Here are the results of the analyses, indicating the best parameters to be adopted, as well as the suggested purchase and sale positions for each fund. / *Aqui estão os resultados das análises, indicando os melhores parâmetros a serem adotados, bem como as posições sugeridas de compra e venda para cada fundo.*

**Function_Simulation_Invest.r:** This file contains the functions that are processed and used in various parts of the project. / *Este arquivo contém as funções que são processadas e usadas em várias partes do projeto.*

**Investment_Decision.R:** Responsible for updating the data by scraping the CVM website and subsequently compiles the list of registered funds. / *Responsável pela atualização dos dados através de raspagem de dados do site da CVM, e posteriormente compila a lista de fundos cadastrados.*

## Success Criteria / Critério de Sucesso
A strategy is considered successful if it provides an annualized yield equal to or greater than 6% and outperforms the respective fund's performance over the analyzed period. / *Consideramos uma estratégia bem-sucedida aquela que proporciona um rendimento anual igual ou superior a 6% e que supere o desempenho do respectivo fundo no período analisado.*

## Usage / Uso
To start using the project, start with the Investment_Decision.R file, which will update the data and compile the list of funds. From there, you can process the functions from Function_Simulation_Invest.r as needed and review the results in the Results_Analyzes folder. / *Para começar a usar o projeto, comece pelo arquivo Investment_Decision.R, que atualizará os dados e compilará a lista de fundos. A partir daí, você pode processar as funções do Function_Simulation_Invest.r conforme sua necessidade e analisar os resultados na pasta Results_Analyzes.*

## Contributions / Contribuições
We are open to feedback and contributions. Feel free to open issues or pull requests. / *Estamos abertos a feedbacks e contribuições. Sinta-se à vontade para abrir issues ou pull requests.*

