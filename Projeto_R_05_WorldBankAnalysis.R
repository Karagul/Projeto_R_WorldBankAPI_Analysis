## Projeto 05 - WorldBankAnalysis

# Objetivos da analise: 

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

## Data Cleaning

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

# Identificando informacoes regionais como paises
wb_debt_trash <- wb_debt %>%
  filter(region == 'Aggregates') %>% 
  select(country)
table(wb_debt_trash)

# Identificando as obs com NA
wb_debt_NA <- wb_debt %>% 
  filter(is.na(pop_total) | is.na(GDP_per_cap) | is.na(DEBT_perc_GDP) | is.na(income)) %>% 
  select(country)
table(wb_debt_NA)

# Retirando valores inutes a analise
wb_debt_clean <- wb_debt %>% 
  anti_join(wb_debt_NA) %>%
  anti_join(wb_debt_trash)

## EDA

# Plot 1: Gastos do Governo como Percentual do PIB vs PIB per Capita

# Criacao de um label para BR na feature income
wb_debt_br <- wb_debt_clean
levels(wb_debt_br$income) <- c(levels(wb_debt_br$income), 'Brazil')
wb_debt_br[wb_debt_br$country == 'Brazil', 'income'] = 'Brazil'

# Criacao de uma funcao para vline e hline no plot.ly
vline <- function(x = 0, color = "red") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color)
  )
}hline <- function(y = 0, color = "blue") {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color)
  )
}

plot_1 <- plot_ly(wb_debt_br, x = ~GDP_per_cap, y = ~DEBT_perc_GDP, 
             type = 'scatter',
             mode = 'markers', 
             color = ~income,
             colors = c('Green', 'Green', 'Red', 'Red', 'Orange', 'Blue'), 
             size = ~pop_total,
             alpha = 0.5,
             marker = list(sizemode = 'diameter'),
             hoverinfo = 'text',
             text = ~paste('Country:', country, '<br>PIB per Capita:', GDP_per_cap,
                           '<br>Divida como Percentual do PIB (%):', DEBT_perc_GDP)) %>%
  layout(title = 'Gastos do Governo como Percentual do PIB vs PIB per Capita',
         xaxis = list(showgrid = FALSE,
                      type = 'log',
                      title = 'PIB per Capita'),
         yaxis = list(showgrid = FALSE,
                      title = 'Divida como Percentual do PIB (%)'),
         shapes = list(
           list(type='line', x0= median(wb_debt_br$GDP_per_cap), x1= median(wb_debt_br$GDP_per_cap), 
                y0 = 0, y1=max(wb_debt_br$DEBT_perc_GDP), line = list(color = 'red', dash='dot')),
           
           list(type='line', x0= min(wb_debt_br$GDP_per_cap), x1 = max(wb_debt_br$GDP_per_cap), 
                y0 = median(wb_debt_br$DEBT_perc_GDP), y1 = median(wb_debt_br$DEBT_perc_GDP), 
                line = list(color = 'red', dash='dot'))))
plot_1



# #Acessando a API e publicando
# Sys.setenv("plotly_username"="Rix_Carboni")
# Sys.setenv("plotly_api_key"="E1cCjrwRMJdsyF81fpB8")
# chart_link = api_create(p, filename="Governement_Expenditure_vs_GDP_per_Capita")
# chart_link


####################################
#Indicar o ponto do Brasil
#Fazer linhas separando os quadrantes