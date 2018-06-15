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
db_search <- WDIsearch('inflation')

# Obtendo os indices de interesse no ano de 2016
wb_interest <- WDI(indicator = c('SP.POP.TOTL', 'NY.GDP.PCAP.KD', 'FR.INR.RINR'),
               start = 2013, end = 2013, extra = TRUE)

## Data Cleaning

# Transformacao em tbl, retirada de coluna iso2c, e nomeacao das colunas
wb_interest <- as.tbl(wb_interest) %>% 
  select(-iso2c) %>%
  select(-iso3c) %>%
  select(-capital) %>%
  select(-lending)
colnames(wb_interest) <- c('country', 'year', 'pop_total', 'GDP_per_cap',
                       'real_interest', 'region', 'longitute', 'latitude', 'income')

# Analisando dataset e verificando valores
glimpse(wb_interest)
summary(wb_interest)

# Identificando informacoes regionais como paises
wb_interest_trash <- wb_interest %>%
  filter(region == 'Aggregates') %>% 
  select(country)
table(wb_interest_trash)

# Identificando as obs com NA
wb_interest_NA <- wb_interest %>% 
  filter(is.na(pop_total) | is.na(GDP_per_cap) | is.na(real_interest) | is.na(income)) %>% 
  select(country)
table(wb_interest_NA)

# Retirando valores inutes a analise
wb_interest_clean <- wb_interest %>% 
  anti_join(wb_interest_NA) %>%
  anti_join(wb_interest_trash)

## EDA

# Plot 1: Gastos do Governo como Percentual do PIB vs PIB per Capita

# Criacao de um label para BR na feature income
wb_interest_br <- wb_interest_clean
levels(wb_interest_br$income) <- c(levels(wb_interest_br$income), 'Brazil')
wb_interest_br[wb_interest_br$country == 'Brazil', 'income'] = 'Brazil'

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
}
hline <- function(y = 0, color = "blue") {
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

plot_1 <- plot_ly(wb_interest_br, x = ~GDP_per_cap, y = ~real_interest, 
                  type = 'scatter',
                  mode = 'markers', 
                  color = ~income,
                  colors = c('Green', 'Green', 'Red', 'Red', 'Orange', 'Blue'), 
                  size = ~pop_total,
                  alpha = 0.5,
                  marker = list(sizemode = 'diameter'),
                  hoverinfo = 'text',
                  text = ~paste('Country:', country, '<br>PIB per Capita:', GDP_per_cap,
                                '<br>Juros Reais da Divida (%):', real_interest)) %>%
  layout(title = 'Juros Reais vs PIB per Capita',
         xaxis = list(showgrid = FALSE,
                      type = 'log',
                      title = 'PIB per Capita'),
         yaxis = list(showgrid = FALSE,
                      title = 'Juros Reais Anuais (%)'),
         shapes = list(
           list(type='line', x0= median(wb_interest_br$GDP_per_cap), x1= median(wb_interest_br$GDP_per_cap), 
                y0 = 0, y1=max(wb_interest_br$real_interest), line = list(color = 'red', dash='dot')),
           
           list(type='line', x0= min(wb_interest_br$GDP_per_cap), x1 = max(wb_interest_br$GDP_per_cap), 
                y0 = median(wb_interest_br$real_interest), y1 = median(wb_interest_br$real_interest), 
                line = list(color = 'red', dash='dot'))))
plot_1



# #Acessando a API e publicando
 Sys.setenv("plotly_username"="Rix_Carboni")
 Sys.setenv("plotly_api_key"="E1cCjrwRMJdsyF81fpB8")
 chart_link = api_create(plot_1, filename="Governement_Expenditure_vs_GDP_per_Capita")
 chart_link


