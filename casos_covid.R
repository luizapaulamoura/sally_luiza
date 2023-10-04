#install.packages("ggplot2")
#install.packages("gganimate")
#install.packages("transformr")
install.packages("plotly")




library(tidyverse)
library(magrittr)
library(dplyr)
library(readr)
library(gganimate)
library(transformr)
library(plotly)

#dados de 2020
dados_gerais <- read_csv("dados/dadosGerais2020.csv")
#dados de 2021
dados_gerais2021 <- read_csv("dados/dadosGerais2021.csv")

#view(dados_gerais)
glimpse(dados_gerais2021)

#Todos os casos estão com status confirmado
dados_status <- table(dados_gerais$STATUS)
dados_status21 <- table(dados_gerais2021$STATUS)

#separar data da confirmação
#2020
dados_classificacao <- dados_gerais[,c("DATA_DA_NOTIFICACAO", "STATUS")]
dados_classificacao$mes <- month(dados_classificacao$DATA_DA_NOTIFICACAO)
#2021
dados_classificacao2021 <- dados_gerais2021[,c("DATA_DA_NOTIFICACAO", "STATUS")]
dados_classificacao2021$mes <- month(dados_classificacao2021$DATA_DA_NOTIFICACAO)
#view(dados_classificacao)

#Quantidade por mês
#2020
dados <- table(dados_classificacao$mes)
dados <- as.data.frame(dados)
names(dados) <- c("Mês", "Número de casos confirmados")
nomes <- names(dados)
#2021
dados2 <- table(dados_classificacao2021$mes)
dados2 <- as.data.frame(dados2)
names(dados2) <- c("Mês", "Número de casos confirmados")
#nomes <- names(dados)


#colocar os nomes dos meses e anos
#2020
glimpse(dados)
dados <- dados |>
  mutate(Mês = as.character(Mês))
dados <- dados |>
  mutate(Mês = recode(Mês, "1" = "Janeiro/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "2" = "Fevereiro/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "3" = "Março/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "4" = "Abril/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "5" = "Maio/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "6" = "Junho/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "7" = "Julho/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "8" = "Agosto/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "9" = "Setembro/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "10" = "Outubro/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "11" = "Novembro/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "12" = "Dezembro/2020"))
#2021
dados2 <- dados2 |>
  mutate(Mês = as.character(Mês))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "1" = "Janeiro/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "2" = "Fevereiro/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "3" = "Março/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "4" = "Abril/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "5" = "Maio/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "6" = "Junho/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "7" = "Julho/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "8" = "Agosto/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "9" = "Setembro/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "10" = "Outubro/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "11" = "Novembro/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "12" = "Dezembro/2021"))

#arredondar para visualização


#juntar dados de 2020 e 2021
dados <- rbind(dados, dados2)

view(dados)
glimpse(dados)

# Carregue a biblioteca plotly
library(plotly)

# Suponha que você tenha um conjunto de dados chamado "dados" com colunas "Mês" e "Número de casos confirmados"

# Defina a coluna "Mês" como um fator na ordem desejada
dados$Mês <- factor(dados$Mês, levels = unique(dados$Mês))

# Crie o scatter plot interativo
scatter_plot_interativo <- plot_ly(
  data = dados,
  x = ~Mês,
  y = ~`Número de casos confirmados`,
  type = "scatter",
  mode = "lines",
  marker = list(color = "blue"),
  text = ~paste("Mês: ", Mês, "<br>Casos: ", `Número de casos confirmados`),
  hoverinfo = "text"
) %>%
  layout(
    title = "Número de Casos de COVID-19",
    xaxis = list(title = "Mês"),
    yaxis = list(title = "Número de Casos")
  )

# Exiba o scatter plot interativo
scatter_plot_interativo


