library(agricolae)
library(ggplot2)
library(lmtest)
library(dplyr)

#-----------------------------------------------------------------------------

analise_dicc <- function(data, resposta, tratamento, 
                         cor = "blue", 
                         titulo_grafico = "Teste de Tukey", 
                         eixo_x = "Tratamento", 
                         eixo_y = "Variável resposta") {
  
  # Ajusta o modelo de ANOVA
  formula_modelo <- as.formula(paste(resposta, "~", tratamento))
  modelo <- aov(formula_modelo, data = data)
  
  # Resultados da ANOVA
  print(anova(modelo))
  
  # Teste de Normalidade (Shapiro-Wilk)
  print(shapiro.test(modelo$residuals))
  
  # Teste de Homogeneidade de variâncias (Bartlett)
  print(bartlett.test(as.formula(paste(resposta, "~", tratamento)), data = data))
  
  # Teste de Independência (Durbin-Watson)
  print(dwtest(modelo))
  
  # Teste de Tukey para comparação de médias
  tukey_result <- HSD.test(modelo, tratamento, group = TRUE)
  
  # Prepara dados para o gráfico
  new_data <- data.frame(Trat = rownames(tukey_result$groups), tukey_result$groups)
  
  # Gera o gráfico com ggplot2
  ggplot(new_data, aes(x = reorder(Trat, -get(resposta)), y = get(resposta))) + 
    geom_bar(stat = "identity", fill = cor) +  
    geom_text(aes(label = groups), vjust = -0.5) +
    labs(x = eixo_x, y = eixo_y) +  
    ggtitle(titulo_grafico) +  
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) 
}

# Exemplo de uso:

dic <- data.frame(
  TRAT = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4),
  PROD = c(90, 98, 95, 93, 56, 59, 63, 51, 75, 73, 71, 89, 64, 68, 65, 63)
)

dic$TRAT<-as.factor(dic$TRAT)

analise_dicc(dic, "PROD", "TRAT", 
             cor = "yellow", 
             titulo_grafico = "Teste de Tukey para Produtividade", 
             eixo_x = "Tratamento", 
             eixo_y = "Produtividade (kg/ha)")

#-------------------------------------------------------------------------------

analise_dbcc <- function(data, resposta, tratamento, bloco, 
                         cor = "blue", 
                         titulo_grafico = "Teste de Tukey", 
                         eixo_x = "Tratamento", 
                         eixo_y = "Variável resposta") {
  
  # Ajusta o modelo de ANOVA para DBC
  formula_modelo <- as.formula(paste(resposta, "~", tratamento, "+", bloco))
  modelo <- aov(formula_modelo, data = data)
  
  # Resultados da ANOVA
  print(anova(modelo))
  
  # Teste de Normalidade (Shapiro-Wilk)
  print(shapiro.test(modelo$residuals))
  
  # Teste de Homogeneidade de variâncias (Bartlett)
  print(bartlett.test(as.formula(paste(resposta, "~", tratamento)), data = data))
  
  # Teste de Independência (Durbin-Watson)
  print(dwtest(modelo))
  
  # Teste de Tukey para comparação de médias
  tukey_result <- HSD.test(modelo, tratamento, group = TRUE)
  
  # Prepara dados para o gráfico
  new_data <- data.frame(Trat = rownames(tukey_result$groups), tukey_result$groups)
  
  # Gera o gráfico com ggplot2
  ggplot(new_data, aes(x = reorder(Trat, -get(resposta)), y = get(resposta))) + 
    geom_bar(stat = "identity", fill = cor) +  
    geom_text(aes(label = groups), vjust = -0.5) +
    labs(x = eixo_x, y = eixo_y) +  
    ggtitle(titulo_grafico) +  
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Exemplo de uso 
dbc <- data.frame(
  TRAT = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5),
  BLOCO = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
  NUTRI = c(83, 63, 55, 86, 69, 61, 103, 79, 79, 116, 81, 79, 132, 98, 91)
)

dbc$TRAT <- as.factor(dbc$TRAT)
dbc$BLOCO <- as.factor(dbc$BLOCO)

analise_dbcc(dbc, "NUTRI", "TRAT", "BLOCO", 
             cor = 'purple',
             titulo_grafico = "Teste de Tukey para Nutrição", 
             eixo_x = "Tratamento", 
             eixo_y = "Nutrição")

#-------------------------------------------------------------------------------

analise_dicm <- function(data, resposta, tratamento, 
                         titulo_grafico = "Teste de Tukey", 
                         eixo_x = "Tratamento", 
                         eixo_y = "Variável resposta") {
  
  # Ajusta o modelo de ANOVA para DIC
  formula_modelo <- as.formula(paste(resposta, "~", tratamento))
  modelo <- aov(formula_modelo, data = data)
  
  # Resultados da ANOVA
  print(anova(modelo))
  
  # Teste de Normalidade (Shapiro-Wilk)
  print(shapiro.test(modelo$residuals))
  
  # Teste de Homogeneidade de variâncias (Bartlett)
  print(bartlett.test(as.formula(paste(resposta, "~", tratamento)), data = data))
  
  # Teste de Independência (Durbin-Watson)
  print(dwtest(modelo))
  
  # Teste de Tukey para comparação de médias
  tukey_result <- HSD.test(modelo, tratamento, group = TRUE)
  
  # Prepara dados para o gráfico
  new_data <- data.frame(Trat = rownames(tukey_result$groups), tukey_result$groups)
  
  # Define uma paleta de cores para as letras
  num_grupos <- length(unique(new_data$groups))
  cores_letras <- scales::hue_pal()(num_grupos)
  
  # Adiciona uma coluna de cores baseada nos grupos de letras
  new_data <- new_data %>%
    mutate(cor = cores_letras[as.numeric(as.factor(groups))])
  
  # Gera o gráfico com ggplot2
  ggplot(new_data, aes(x = reorder(Trat, -get(resposta)), y = get(resposta), fill = cor)) + 
    geom_bar(stat = "identity") +  
    geom_text(aes(label = groups), vjust = -0.5, size = 5) +  
    scale_fill_identity() +  
    labs(x = eixo_x, y = eixo_y) +  
    ggtitle(titulo_grafico) +  
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
}

# Exemplo de uso:

dic <- data.frame(
  TRAT = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4),
  PROD = c(90, 98, 95, 93, 56, 59, 63, 51, 75, 73, 71, 89, 64, 68, 65, 63)
)

dic$TRAT<-as.factor(dic$TRAT)

analise_dicm(dic, "PROD", "TRAT", 
             titulo_grafico = "Teste de Tukey para Produtividade", 
             eixo_x = "Tratamento", 
             eixo_y = "Produtividade (kg/ha)")

#-------------------------------------------------------------------------------

analise_dbcm <- function(data, resposta, tratamento, bloco, 
                         titulo_grafico = "Teste de Tukey", 
                         eixo_x = "Tratamento", 
                         eixo_y = "Variável resposta") {
  
  # Ajusta o modelo de ANOVA para DBC
  formula_modelo <- as.formula(paste(resposta, "~", tratamento, "+", bloco))
  modelo <- aov(formula_modelo, data = data)
  
  # Resultados da ANOVA
  print(anova(modelo))
  
  # Teste de Normalidade (Shapiro-Wilk)
  print(shapiro.test(modelo$residuals))
  
  # Teste de Homogeneidade de variâncias (Bartlett)
  print(bartlett.test(as.formula(paste(resposta, "~", tratamento)), data = data))
  
  # Teste de Independência (Durbin-Watson)
  print(dwtest(modelo))
  
  # Teste de Tukey para comparação de médias
  tukey_result <- HSD.test(modelo, tratamento, group = TRUE)
  
  # Prepara dados para o gráfico
  new_data <- data.frame(Trat = rownames(tukey_result$groups), tukey_result$groups)
  
  # Define uma paleta de cores para as letras
  num_grupos <- length(unique(new_data$groups))
  cores_letras <- scales::hue_pal()(num_grupos)
  
  # Adiciona uma coluna de cores ao dataframe baseado nos grupos de letras
  new_data <- new_data %>%
    mutate(cor = cores_letras[as.numeric(as.factor(groups))])
  
  # Gera o gráfico com ggplot2
  ggplot(new_data, aes(x = reorder(Trat, -get(resposta)), y = get(resposta), fill = cor)) + 
    geom_bar(stat = "identity") + 
    geom_text(aes(label = groups), vjust = -0.5, size = 5) +  
    scale_fill_identity() +  
    labs(x = eixo_x, y = eixo_y) +  
    ggtitle(titulo_grafico) +  
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
}

# Exemplo de uso 
dbc <- data.frame(
  TRAT = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5),
  BLOCO = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
  NUTRI = c(83, 63, 55, 86, 69, 61, 103, 79, 79, 116, 81, 79, 132, 98, 91)
)

dbc$TRAT <- as.factor(dbc$TRAT)
dbc$BLOCO <- as.factor(dbc$BLOCO)

analise_dbcm(dbc, "NUTRI", "TRAT", "BLOCO", 
             titulo_grafico = "Teste de Tukey para Nutrição", 
             eixo_x = "Tratamento", 
             eixo_y = "Nutrição")
