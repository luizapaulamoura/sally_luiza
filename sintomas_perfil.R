install.packages("tidyverse")
install.packages("magrittr")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("gridExtra")


library(gridExtra)
library(tidyverse)
library(magrittr)
library(dplyr)
library(readr)
library(ggthemes)
library(ggplot2)


#dados de 2020
dados_gerais <- read_csv("dados/dadosGerais2020.csv")
#dados de 2021
dados_gerais2021 <- read_csv("dados/dadosGerais2021.csv")

df <- rbind(dados_gerais, dados_gerais2021)

view(df)
glimpse(df)

unique(df$E_PROFISSIONAL_DE_SAUDE)
# [1] "Nao"  NA     "Sim"  "NA?o"
# Substituir "NA?o" por "Nao" na coluna E_PROFISSIONAL_DE_SAUDE
df$E_PROFISSIONAL_DE_SAUDE <- gsub("NA\\?o", "Nao", df$E_PROFISSIONAL_DE_SAUDE)
df$E_PROFISSIONAL_DE_SAUDE <- ifelse(df$E_PROFISSIONAL_DE_SAUDE %in% c("NA"), NA, df$E_PROFISSIONAL_DE_SAUDE)

unique(df$FEBRE)
# [1] "0"        "Nao"      "Sim"      "Ignorado"
df$FEBRE <- ifelse(df$FEBRE %in% c("0", "Ignorado","NA"), NA, df$FEBRE)

unique(df$TOSSE)
# [1] "0"        "Nao"      "Sim"      "Ignorado"
df$TOSSE <- ifelse(df$TOSSE %in% c("0", "Ignorado","NA"), NA, df$TOSSE)

unique(df$DOR_DE_GARGANTA)
# [1] "0"        "Sim"      "Nao"      "Ignorado"
df$DOR_DE_GARGANTA <- ifelse(df$DOR_DE_GARGANTA %in% c("0", "Ignorado","NA"), NA, df$DOR_DE_GARGANTA)

unique(df$DISPNEIA)
# [1] "0"        "Sim"      "Nao"      "Ignorado
df$DISPNEIA <- ifelse(df$DISPNEIA %in% c("0", "Ignorado","NA"), NA, df$DISPNEIA)

unique(df$DOENCAS_RESPIRATORIAS_CRONICAS_DESCOMPENSADAS)
#  [1] "0"        "Nao"      "Ignorado" "Sim" 
df$DOENCAS_RESPIRATORIAS_CRONICAS_DESCOMPENSADAS <- ifelse(df$DOENCAS_RESPIRATORIAS_CRONICAS_DESCOMPENSADAS %in% c("0", "Ignorado","NA"), NA, df$DOENCAS_RESPIRATORIAS_CRONICAS_DESCOMPENSADAS)

unique(df$DOENCAS_RENAIS_CRONICAS_EM_ESTAGIO_AVANCADO)
#  [1] "0"        "Nao"      "Ignorado" "Sim"
df$DOENCAS_RENAIS_CRONICAS_EM_ESTAGIO_AVANCADO <- ifelse(df$DOENCAS_RENAIS_CRONICAS_EM_ESTAGIO_AVANCADO %in% c("0", "Ignorado","NA"), NA, df$DOENCAS_RENAIS_CRONICAS_EM_ESTAGIO_AVANCADO)

unique(df$PORTADOR_DE_DOENCAS_CROMOSSOMICAS_OU_ESTADO_DE_FRAGILIDADE_IMUNOLOGICA)
# [1] "0"        "Nao"      "Ignorado" "Sim"
df$PORTADOR_DE_DOENCAS_CROMOSSOMICAS_OU_ESTADO_DE_FRAGILIDADE_IMUNOLOGICA <- ifelse(df$PORTADOR_DE_DOENCAS_CROMOSSOMICAS_OU_ESTADO_DE_FRAGILIDADE_IMUNOLOGICA %in% c("0", "Ignorado","NA"), NA, df$PORTADOR_DE_DOENCAS_CROMOSSOMICAS_OU_ESTADO_DE_FRAGILIDADE_IMUNOLOGICA)

unique(df$DIABETES)
# [1] "0"        "Nao"      "Ignorado" "Sim"
df$DIABETES <- ifelse(df$DIABETES %in% c("0", "Ignorado","NA"), NA, df$DIABETES)

unique(df$IMUNOSSUPRESSAO)
#[1] "0"        "Nao"      "Ignorado" "Sim"
df$IMUNOSSUPRESSAO <- ifelse(df$IMUNOSSUPRESSAO %in% c("0", "Ignorado","NA"), NA, df$IMUNOSSUPRESSAO)

unique(df$DOENCAS_CARDIACAS_CRONICAS)
# [1] "0"        "Nao"      "Ignorado" "Sim"
df$DOENCAS_CARDIACAS_CRONICAS <- ifelse(df$DOENCAS_CARDIACAS_CRONICAS %in% c("0", "Ignorado","NA"), NA, df$DOENCAS_CARDIACAS_CRONICAS)

outros_sintomas <- unique(df$OUTROS)

#view(outros_sintomas)

unique(df$CLASSIFICACAO_FINAL)
df$CLASSIFICACAO_FINAL <- ifelse(df$CLASSIFICACAO_FINAL %in% c("4","NA"), NA, df$CLASSIFICACAO_FINAL)
df$CLASSIFICACAO_FINAL <- ifelse(df$CLASSIFICACAO_FINAL %in% c("Confirmação Laboratorial", "Laboratorial"), "Confirmado Laboratorial", df$CLASSIFICACAO_FINAL)
df$CLASSIFICACAO_FINAL <- ifelse(df$CLASSIFICACAO_FINAL %in% c("Clínico"), "Confirmado por Critério Clínico", df$CLASSIFICACAO_FINAL)
df$CLASSIFICACAO_FINAL <- ifelse(df$CLASSIFICACAO_FINAL %in% c("Confirmação Clínico Epidemiológico"), "Confirmado Clínico-Epidemiológico", df$CLASSIFICACAO_FINAL)

frequencia <- table(df$CLASSIFICACAO_FINAL)
barplot(frequencia, 
        horiz = TRUE,  # Gira o gráfico para a posição horizontal
        main = "Classificação Final dos pacientes",
        xlab = "Frequência",  # Altera o rótulo do eixo x
        ylab = "",   # Remova o rótulo do eixo y
        col = "blue", # Cor das barras
        border = "black", # Cor das bordas das barras
        names.arg = names(frequencia), # Rótulos do eixo y (agora na posição horizontal)
        xlim = c(0, max(frequencia) + 1), # Define o limite x
        las = 1  # Gira o rótulo do eixo y em 90 graus
)
# Restaurar as margens padrão
par(mar = c(5, 15, 4, 4))

# Ajuste as margens esquerdas para afastar o eixo y da borda esquerdz


# Função para criar gráficos de barras para uma coluna específica
criar_grafico_de_barras <- function(coluna) {
  grafico <- ggplot(df, aes_string(x = coluna)) +
    geom_bar(fill = "blue", color = "black") +
    labs(
      title = paste(coluna),
      x = coluna,
      y = "Frequência"
    ) +
    theme_minimal()
  return(grafico)
}

# Crie uma matriz de gráficos de barras
grafico1 <- criar_grafico_de_barras("E_PROFISSIONAL_DE_SAUDE")
grafico2 <- criar_grafico_de_barras("FEBRE")
grafico3 <- criar_grafico_de_barras("TOSSE")
grafico4 <- criar_grafico_de_barras("DOR_DE_GARGANTA")
grafico5 <- criar_grafico_de_barras("DISPNEIA")
grafico6 <- criar_grafico_de_barras("DOENCAS_RESPIRATORIAS_CRONICAS_DESCOMPENSADAS")
grafico7 <- criar_grafico_de_barras("DOENCAS_RENAIS_CRONICAS_EM_ESTAGIO_AVANCADO")
grafico8 <- criar_grafico_de_barras("PORTADOR_DE_DOENCAS_CROMOSSOMICAS_OU_ESTADO_DE_FRAGILIDADE_IMUNOLOGICA")
grafico9 <- criar_grafico_de_barras("DIABETES")
grafico10 <- criar_grafico_de_barras("IMUNOSSUPRESSAO")
grafico11 <- criar_grafico_de_barras("DOENCAS_CARDIACAS_CRONICAS")


# Organize os gráficos em uma matriz
matriz_de_graficos <- grid.arrange(grafico2, grafico3, grafico4, grafico5, ncol = 2)

# Exiba a matriz de gráficos
print(matriz_de_graficos)

matriz_de_graficos <- grid.arrange(grafico6, grafico7, grafico8, grafico9, grafico10, grafico11, ncol = 2)

# Exiba a matriz de gráficos
print(matriz_de_graficos)



