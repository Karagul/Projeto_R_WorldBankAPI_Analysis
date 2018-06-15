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
wb_time <- WDI(indicator = c('IC.REG.DURS'),
               start = 2013, end = 2017, extra = TRUE)

## Data Cleaning

# Transformacao em tbl, retirada de coluna iso2c, e nomeacao das colunas
wb_time <- as.tbl(wb_time) %>% 
  select(-iso2c) %>%
  select(-iso3c) %>%
  select(-capital) %>%
  select(-lending)
colnames(wb_time) <- c('country', 'time','year', 
                           'region', 'longitute', 'latitude', 'income')

# Analisando dataset e verificando valores
glimpse(wb_time)
summary(wb_time)

# Criacao de um label para BR na feature income
wb_time_br <- wb_time
levels(wb_time_br$income) <- c(levels(wb_time_br$income), 'Brazil')
wb_time_br[wb_time_br$country == 'Brazil', 'income'] = 'Brazil'

# Identificando informacoes regionais como paises
wb_time_trash <- wb_time_br %>%
  filter(income == 'Aggregates' | income == 'Upper middle income' | 
           income == 'Low income' | income == 'Lower middle income') %>%
  select(country)
table(wb_time_trash)

# Identificando as obs com NA
wb_time_NA <- wb_time_br %>% 
  filter(is.na(time) | is.na(income)) %>% 
  select(country)
table(wb_time_NA)

# Retirando valores inuteis a analise
wb_time_clean <- wb_time_br %>% 
  anti_join(wb_time_NA) %>%
  anti_join(wb_time_trash)

## EDA

# Plot 1: Inflacao Brasil comparada aos paises ricos

# Criacao de uma funcao para vline e hline no plot.ly

p0 <- ggplot(data = wb_time_clean, aes(x = year, y = time, label = country))

p1 <- p0 +
  geom_point(mapping = aes(colour = income)) +
  scale_color_manual(values = c('Black', '#009933')) +
  ggtitle('Tempo Médio para Abertura de uma Empresa') +
  xlab('Ano') +
  ylab('Tempo (Dias)')

p1 <- p1 +
  theme(
    panel.background = element_rect(fill = "transparent"),
    legend.position = 'none'
  )

p1




