# ===============================================================
# 📊 Projeto: Análise de Dados de Acidentes de Trânsito 2024
# 📁 Script: src/analysis.R
# 🔗 Dataset: https://raw.githubusercontent.com/AndersonSalata/projeto-integrado-ciencia-de-dados/main/datatran2024.csv
# ===============================================================

# ===============================================================
# 1. Instalar pacotes necessários (se ainda não estiverem instalados)
# ===============================================================
# install.packages("dplyr")
# install.packages("ggplot2")

# ===============================================================
# 2. Carregar bibliotecas
# ===============================================================
library(dplyr)
library(ggplot2)

# ===============================================================
# 3. Carregar o conjunto de dados
# ===============================================================
dados <- read.csv(
  "https://raw.githubusercontent.com/AndersonSalata/projeto-integrado-ciencia-de-dados/main/datatran2024.csv",
  sep = ";",
  fill = TRUE,
  check.names = FALSE
)

# ===============================================================
# 4. Explorar o conjunto de dados
# ===============================================================
print(str(dados))     # Estrutura das variáveis
print(summary(dados)) # Resumo estatístico
print(head(dados))    # Primeiras linhas do dataset

# ===============================================================
# 5. Análise de dados
# ===============================================================

# 5.1 Filtrar acidentes sob condição de "Céu Claro"
dados_claros <- dados %>%
  filter(condicao_metereologica == "Céu Claro")

# 5.2 Contar acidentes por estado
acidentes_by_state <- dados %>%
  group_by(uf) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

# 5.3 Mostrar o estado com maior número de acidentes
print(head(acidentes_by_state, 1))

# 5.4 Calcular a probabilidade de acidente sob "Céu Claro"
probabilidade_ceu_claro <- nrow(dados_claros) / nrow(dados)
print(paste("Probabilidade de acidente em Céu Claro:", round(probabilidade_ceu_claro, 4)))

# ===============================================================
# 6. Visualização de dados
# ===============================================================

# 6.1 Criar gráfico: quantidade de acidentes por fase do dia
grafico_fase_dia <- ggplot(dados, aes(x = fase_dia)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Acidentes por Fase do Dia",
    x = "Fase do Dia",
    y = "Número de Acidentes"
  ) +
  theme_minimal()

# 6.2 Mostrar o gráfico no console
print(grafico_fase_dia)

# 6.3 Salvar o gráfico em arquivo PNG
ggsave(
  filename = "outputs/graphs/acidentes_fase_dia.png",
  plot = grafico_fase_dia,
  width = 10,
  height = 6
)
