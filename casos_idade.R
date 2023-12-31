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

#Todos os casos estão com status confirmado
dados_status <- table(dados_gerais$STATUS)
dados_status21 <- table(dados_gerais2021$STATUS)

#separar data da confirmação
#2020
dados_classificacao <- dados_gerais[,c("DATA_DA_NOTIFICACAO","DATA_DE_NASCIMENTO", "STATUS")]
#dados_classificacao$idade <- difftime(dados_classificacao$DATA_DA_NOTIFICACAO, dados_classificacao$DATA_DE_NASCIMENTO, units = "years")
dados_classificacao$IDADE <- as.numeric(interval(dados_classificacao$DATA_DE_NASCIMENTO, dados_classificacao$DATA_DA_NOTIFICACAO) / dyears(1))


#2021
dados_classificacao2021 <- dados_gerais2021[,c("DATA_DA_NOTIFICACAO","DATA_DE_NASCIMENTO", "STATUS")]
dados_classificacao2021$IDADE <- as.numeric(interval(dados_classificacao2021$DATA_DE_NASCIMENTO, dados_classificacao2021$DATA_DA_NOTIFICACAO) / dyears(1))

#juntar dados de 2020 e 2021
dados <- rbind(dados_classificacao, dados_classificacao2021)

#separar mês e ano
dados$data_status <- paste(format(dados$DATA_DA_NOTIFICACAO, "%B"), format(dados$DATA_DA_NOTIFICACAO, "%Y"))
glimpse(dados)

view(dados)

#Fazer intervalos de faixa etária de 10 em 10 anos
intervalos_idade <- c(0, 14, 29, 44, 59, 74, Inf)
dados$faixa_etaria <- cut(dados$IDADE, breaks = intervalos_idade, labels = FALSE,  right = FALSE)
glimpse(dados)

#mudar nomes de faixa etária
dados <- dados |>
  mutate(faixa_etaria = as.character(faixa_etaria))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "1" = "0-14"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "2" = "15-29"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "3" = "30-44"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "4" = "45-59"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "5" = "60-74"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "6" = "75+"))



#frequencia de data e faixa etaria
dados2 <- table(dados$data_status, dados$faixa_etaria)
dados2 <- as.data.frame(dados2)

#view(dados3)
dados3 <- dados2 %>%
  pivot_wider(names_from = Var2, values_from = Freq)
glimpse(dados3)

meses <- seq(as.Date("2020-01-01"), as.Date("2021-12-01"), by = "month")
meses <- format(meses, format = "%B %Y")
meses <- paste(meses, sep = " ")
meses

dados3$Var1

dados4 <- dados3 %>% 
  mutate(Var1  = factor(Var1, levels= meses)) %>% 
  arrange(Var1)

view(dados4)
names(dados4) <- c("Mês", "0-14", "15-29", "30-44", "45-59", "60-74", "75+")
nomes <- names(dados4)
glimpse(dados4)

dados4 <- dados4 |>
  mutate(Mês = recode(Mês, "dezembro 2021" = "dezembro 2021"))
#view(dados3)
#view(dados4)
glimpse(dados4)

#dados4 %<>% mutate(`0-14`=round(`0-14`/1000,1))
#dados4 %<>% mutate(`15-29`=round(`15-29`/1000,1))
#dados4 %<>% mutate(`30-44`=round(`30-44`/1000,1))
#dados4 %<>% mutate(`45-59`=round(`45-59`/1000,1))
#dados4 %<>% mutate(`60-74`=round(`60-74`/1000,1))
#dados4 %<>% mutate(`75+`=round(`75+`/1000,1))


view(dados4)

# Carregue a biblioteca plotly
library(plotly)

# Suponha que você tenha um conjunto de dados chamado "dados" com colunas "Mês" e "Número de casos confirmados"

# Defina a coluna "Mês" como um fator na ordem desejada
#dados4$Mês <- factor(dados4$Mês, levels = unique(dados4$Mês))


# Crie um scatter plot interativo vazio
scatter_plot_interativo <- plot_ly() %>%
  layout(
    title = "Número de Casos de COVID-19 por idade",
    xaxis = list(title = "Mês"),
    yaxis = list(title = "Número de Casos")
  )

# Adicione a primeira série
scatter_plot_interativo <- scatter_plot_interativo %>%
  add_trace(
    data = dados4,
    x = ~Mês,
    y = ~`0-14`,  # Substitua 'Série 1' pelo nome da sua primeira série
    name = "0 a 14 anos",
    type = "scatter",
    mode = "lines",
    line = list(color = "blue"),
    text = ~paste("Mês: ", Mês, "<br>Casos: ", `0-14`),  # Substitua 'Série 1' conforme necessário
    hoverinfo = "text"
  )

# Adicione a segunda série
scatter_plot_interativo <- scatter_plot_interativo %>%
  add_trace(
    data = dados4,
    x = ~Mês,
    y = ~`15-29`,  # Substitua 'Série 2' pelo nome da sua segunda série
    name = "15 a 29 anos",
    type = "scatter",
    mode = "lines",
    line = list(color = "red"),  # Cor da segunda série
    text = ~paste("Mês: ", Mês, "<br>Casos: ", `15-29`),  # Substitua 'Série 2' conforme necessário
    hoverinfo = "text"
  )

scatter_plot_interativo <- scatter_plot_interativo %>%
  add_trace(
    data = dados4,
    x = ~Mês,
    y = ~`30-44`,  # Substitua 'Série 2' pelo nome da sua segunda série
    name = "30 a 44 anos",
    type = "scatter",
    mode = "lines",
    line = list(color = "yellow"),  # Cor da segunda série
    text = ~paste("Mês: ", Mês, "<br>Casos: ", `30-44`),  # Substitua 'Série 2' conforme necessário
    hoverinfo = "text"
  )



scatter_plot_interativo <- scatter_plot_interativo %>%
  add_trace(
    data = dados4,
    x = ~Mês,
    y = ~`45-59`,  # Substitua 'Série 2' pelo nome da sua segunda série
    name = "45 a 59 anos",
    type = "scatter",
    mode = "lines",
    line = list(color = "green"),  # Cor da segunda série
    text = ~paste("Mês: ", Mês, "<br>Casos: ", `45-59`),  # Substitua 'Série 2' conforme necessário
    hoverinfo = "text"
  )



scatter_plot_interativo <- scatter_plot_interativo %>%
  add_trace(
    data = dados4,
    x = ~Mês,
    y = ~`60-74`,  # Substitua 'Série 2' pelo nome da sua segunda série
    type = "scatter",
    name = "60 a 74 anos",
    mode = "lines",
    line = list(color = "brown"),  # Cor da segunda série
    text = ~paste("Mês: ", Mês, "<br>Casos: ", `60-74`),  # Substitua 'Série 2' conforme necessário
    hoverinfo = "text"
  )


scatter_plot_interativo <- scatter_plot_interativo %>%
  add_trace(
    data = dados4,
    x = ~Mês,
    y = ~`75+`,  # Substitua 'Série 2' pelo nome da sua segunda série
    name = "75 anos ou mais",
    type = "scatter",
    mode = "lines",
    line = list(color = "pink"),  # Cor da segunda série
    text = ~paste("Mês: ", Mês, "<br>Casos: ", `75+`),  # Substitua 'Série 2' conforme necessário
    hoverinfo = "text"
  )

# Você pode continuar adicionando mais séries da mesma maneira

# Exiba o scatter plot interativo
scatter_plot_interativo



