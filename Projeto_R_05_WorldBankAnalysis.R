## Projeto 05 - WorldBankAnalysis

## Carregando os pacotes necessarios

library(ggplot2)
library(reshape2)
library(WDI)
library(dplyr)
library(shiny)
library(plotly)
library(tidyr)

## Obtencao da base de dados

# Criando dicionario com todos os indices disponiveis na API
db_search <- WDIsearch('continent')

# Obtendo os indices de interesse no ano de 2016
wb_debt <- WDI(indicator = c('SP.POP.TOTL', 'NY.GDP.PCAP.KD', 'NE.CON.GOVT.ZS'),
               start = 2016, end = 2016, extra = TRUE)

## Data Munging

# Transformacao em tbl, retirada de coluna iso2c, e nomeacao das colunas
wb_debt <- as.tbl(wb_debt) %>% 
  select(-iso2c) %>%
  select(-iso3c) %>%
  select(-capital) %>%
  select(-lending)
colnames(wb_debt) <- c('country', 'year', 'pop_total', 'GDP_per_cap',
                       'DEBT_perc_GDP', 'region', 'longitute', 'latitude', 'income')

# Analisando dataset e verificando valores
glimpse(wb_debt)
summary(wb_debt)

# Identificando as obs com NA
wb_debt_NA <- wb_debt %>% 
  filter(is.na(pop_total) | is.na(GDP_per_cap) | is.na(DEBT_perc_GDP)) %>% select(country)
table(wb_debt_NA)

#Identificando obs generalistas
wb_debt_trash <- wb_debt %>%
  filter(region == 'Aggregates') %>% select(country)
table(wb_debt_trash)

# Retirando valores NA 
wb_debt_clean <- wb_debt %>% 
  anti_join(wb_debt_NA) %>%
  anti_join(wb_debt_trash)

# Obs Brasil
br <- wb_debt_clean[wb_debt_clean$country == 'Brazil']

# ## Grafico 1 --> PIB per capita vs Gastos com Securidade (%PIB)

p <- plot_ly(wb_debt_clean, x = ~GDP_per_cap, y = ~DEBT_perc_GDP, 
             text = ~country,
             type = 'scatter',
             mode = 'markers', 
             color = ~income,
             colors = c('Green', 'Green', 'Red', 'Red', 'Orange'), 
             size = ~sqrt(pop_total),
             marker = list(sizemode = 'diameter')) %>%
  layout(title = 'Gastos do Governo como Percentual do PIB vs PIB per Capita',
         xaxis = list(showgrid = FALSE,
                      type = 'log',
                      title = 'PIB per Capita'),
         yaxis = list(showgrid = FALSE,
                      title = 'DÃ?vida como Percentual do PIB'))

#### RESULTADO --------
p


#Acessando a API e publicando
Sys.setenv("plotly_username"="Rix_Carboni")
Sys.setenv("plotly_api_key"="E1cCjrwRMJdsyF81fpB8")
chart_link = api_create(p, filename="Governement_Expenditure_vs_GDP_per_Capita")
chart_link


####################################
#Indicar o ponto do Brasil
#Fazer linhas separando os quadrantes