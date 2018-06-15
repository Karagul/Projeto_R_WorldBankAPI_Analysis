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
db_search <- WDIsearch('business')

# Obtendo os indices de interesse no ano de 2016
wb_inflation <- WDI(indicator = c('NY.GDP.DEFL.KD.ZG'),
               start = 2000, end = 2016, extra = TRUE)

## Data Cleaning

# Transformacao em tbl, retirada de coluna iso2c, e nomeacao das colunas
wb_inflation <- as.tbl(wb_inflation) %>% 
  select(-iso2c) %>%
  select(-iso3c) %>%
  select(-capital) %>%
  select(-lending)
colnames(wb_inflation) <- c('country', 'inflacao','year', 
                           'region', 'longitute', 'latitude', 'income')

# Analisando dataset e verificando valores
glimpse(wb_inflation)
summary(wb_inflation)

# Criacao de um label para BR na feature income
wb_inflation_br <- wb_inflation
levels(wb_inflation_br$income) <- c(levels(wb_inflation_br$income), 'Brazil')
wb_inflation_br[wb_inflation_br$country == 'Brazil', 'income'] = 'Brazil'

# Identificando informacoes regionais como paises
wb_inflation_trash <- wb_inflation_br %>%
  filter(income == 'Aggregates' | income == 'Upper middle income' | 
           income == 'Low income' | income == 'Lower middle income') %>%
  select(country)
table(wb_inflation_trash)

# Identificando as obs com NA
wb_inflation_NA <- wb_inflation_br %>% 
  filter(is.na(inflacao) | is.na(income) | is.na(region)) %>% 
  select(country)
table(wb_inflation_NA)

# Retirando valores inuteis a analise
wb_inflation_clean <- wb_inflation_br %>% 
  anti_join(wb_inflation_NA) %>%
  anti_join(wb_inflation_trash)

## EDA

# Plot 1: Inflacao Brasil comparada aos paises ricos

p0 <- ggplot(data = wb_inflation_clean, aes(x = year, y = inflacao, label = country))

p1 <- p0 + geom_smooth(method = "lm", se = FALSE, color = "Black") +
  geom_point(mapping = aes(colour = income)) +
  scale_color_manual(values = c('transparent', '#009933')) +
  ggtitle('Inflação Esperada Países Ricos vs Inflação Real Brasil') +
  xlab('Ano') +
  ylab('Inflação')

p1 <- p1 +
  theme(
    panel.background = element_rect(fill = "transparent"),
    legend.position = 'none'
  )

p1
