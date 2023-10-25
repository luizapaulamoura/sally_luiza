######################################################DADOS DE 2020
# 1) Carregar bibliotecas
#install.packages("tidyverse")
#install.packages("dplyr")
#insstall.package("rjson")
#install.packages("RJSONIO")
library(tidyverse)
library(magrittr)
library(dplyr)
library(readr)
#library(rjson)
#library(RJSONIO)
library(lubridate)





#dados de 2020
dados_gerais <- read_csv("data/dadosGerais2020.csv")
#dados de 2021
dados_gerais2021 <- read_csv("data/dadosGerais2021.csv")

#view(dados_gerais)
glimpse(dados_gerais2021)

#Quais são as iinformações disponíveis
dados_status <- table(dados_gerais$SEXO)
dados_status21 <- table(dados_gerais2021$SEXO)

#separar data da confirmação
#2020
dados_classificacao <- dados_gerais[,c("DATA_DA_NOTIFICACAO", "SEXO", "STATUS")]
#dados_classificacao$idade <- difftime(dados_classificacao$DATA_DA_NOTIFICACAO, dados_classificacao$DATA_DE_NASCIMENTO, units = "years")
#dados_classificacao$IDADE <- as.numeric(interval(dados_classificacao$DATA_DE_NASCIMENTO, dados_classificacao$DATA_DA_NOTIFICACAO) / dyears(1))


#2021
dados_classificacao2021 <- dados_gerais2021[,c("DATA_DA_NOTIFICACAO", "SEXO", "STATUS")]
#dados_classificacao2021$IDADE <- as.numeric(interval(dados_classificacao2021$DATA_DE_NASCIMENTO, dados_classificacao2021$DATA_DA_NOTIFICACAO) / dyears(1))

#juntar dados de 2020 e 2021
dados <- rbind(dados_classificacao, dados_classificacao2021)

#separar mês e ano
dados$data_status <- paste(format(dados$DATA_DA_NOTIFICACAO, "%B"), format(dados$DATA_DA_NOTIFICACAO, "%Y"))
glimpse(dados)

view(dados)

#Fazer intervalos de faixa etária de 10 em 10 anos
#intervalos_idade <- c(0, 14, 29, 44, 59, 74, Inf)
#dados$faixa_etaria <- cut(dados$IDADE, breaks = intervalos_idade, labels = FALSE,  right = FALSE)
glimpse(dados)

#mudar nomes de faixa etária
dados <- dados |>
  mutate(SEXO = as.character(SEXO))
dados <- dados |>
  mutate(SEXO = recode(SEXO, "F" = "Mulheres"))
dados <- dados |>
  mutate(SEXO = recode(SEXO, "M" = "Homens"))
dados <- dados |>
  mutate(SEXO = recode(SEXO, "I" = "Ignorado"))

#Verificar valores disponíveis
dados_status_novo <- table(dados$SEXO)
dados_status_novo


#frequencia de data e faixa etaria
dados2 <- table(dados$data_status, dados$SEXO)
dados2 <- as.data.frame(dados2)

view(dados2)
dados3 <- dados2 %>%
  pivot_wider(names_from = Var2, values_from = Freq)
glimpse(dados3)
view(dados3)

meses <- seq(as.Date("2020-01-01"), as.Date("2021-12-01"), by = "month")
meses <- format(meses, format = "%B %Y")
meses <- paste(meses, sep = " ")
meses

dados3$Var1

#Organizar por ordem de meses certa :)
dados4 <- dados3 %>% 
  mutate(Var1  = factor(Var1, levels= meses)) %>% 
  arrange(Var1)

view(dados4)
names(dados4) <- c("Mês", "Homens", "Ignorado", "Mulheres")
nomes <- names(dados4)
glimpse(dados4)

dados4 <- dados4 |>
  mutate(Mês = recode(Mês, "dezembro 2021" = "dezembro 2021"))
#view(dados3)
#view(dados4)
glimpse(dados4)


view(dados4)

# Carregue a biblioteca plotly
library(plotly)

# Suponha que você tenha um conjunto de dados chamado "dados" com colunas "Mês" e "Número de casos confirmados"

# Defina a coluna "Mês" como um fator na ordem desejada
#dados4$Mês <- factor(dados4$Mês, levels = unique(dados4$Mês))


# Crie um scatter plot interativo vazio
scatter_plot_interativo <- plot_ly() %>%
  layout(
    title = "Número de Casos de COVID-19 por Gênero",
    xaxis = list(title = "Mês"),
    yaxis = list(title = "Número de Casos")
  )

# Adicione a primeira série
scatter_plot_interativo <- scatter_plot_interativo %>%
  add_trace(
    data = dados4,
    x = ~Mês,
    y = ~`Homens`,  # Substitua 'Série 1' pelo nome da sua primeira série
    name = "Homens",
    type = "scatter",
    mode = "lines",
    line = list(color = "blue"),
    text = ~paste("Mês: ", Mês, "<br>Casos: ", `Homens`),  # Substitua 'Série 1' conforme necessário
    hoverinfo = "text"
  )

# Adicione a segunda série
scatter_plot_interativo <- scatter_plot_interativo %>%
  add_trace(
    data = dados4,
    x = ~Mês,
    y = ~`Ignorado`,  # Substitua 'Série 2' pelo nome da sua segunda série
    name = "Ignorado",
    type = "scatter",
    mode = "lines",
    line = list(color = "yellow"),  # Cor da segunda série
    text = ~paste("Mês: ", Mês, "<br>Casos: ", `Ignorado`),  # Substitua 'Série 2' conforme necessário
    hoverinfo = "text"
  )

scatter_plot_interativo <- scatter_plot_interativo %>%
  add_trace(
    data = dados4,
    x = ~Mês,
    y = ~`Mulheres`,  # Substitua 'Série 2' pelo nome da sua segunda série
    name = "Mulheres",
    type = "scatter",
    mode = "lines",
    line = list(color = "pink"),  # Cor da segunda série
    text = ~paste("Mês: ", Mês, "<br>Casos: ", `Mulheres`),  # Substitua 'Série 2' conforme necessário
    hoverinfo = "text"
  )


# Exiba o scatter plot interativo
scatter_plot_interativo



