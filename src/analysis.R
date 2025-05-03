# ===============================================================
# 📊 Projeto: Análise de Dados de Acidentes de Trânsito 2024
# ===============================================================

# 1. Carregar bibliotecas
library(dplyr)
library(ggplot2)

# 2. Criar pastas de saída se necessário
dir.create("outputs/results", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/graphs", showWarnings = FALSE, recursive = TRUE)

# 3. Redirecionar saída para arquivo TXT
sink("outputs/results/analise_saida.txt")

cat("=============================================================\n")
cat("📊 RELATÓRIO DE ANÁLISE DE DADOS DE ACIDENTES DE TRÂNSITO\n")
cat("=============================================================\n\n")

# 4. Carregar o dataset com codificação correta
cat("🔹 Carregando o dataset com encoding 'latin1'...\n\n")
dados <- read.csv(
  "https://raw.githubusercontent.com/AndersonSalata/projeto-integrado-ciencia-de-dados/main/datatran2024.csv",
  sep = ";",
  fill = TRUE,
  check.names = FALSE,
  fileEncoding = "latin1"
)

# 5. Explorar o dataset
cat("🔹 Estrutura do Dataset:\n")
str(dados)
cat("\n-------------------------------------------------------------\n\n")

cat("🔹 Resumo Estatístico:\n")
print(summary(dados))
cat("\n-------------------------------------------------------------\n\n")

cat("🔹 Primeiras Linhas do Dataset:\n")
print(head(dados, 5))
cat("\n=============================================================\n\n")

# 6. Análises Estatísticas
cat("📌 ANÁLISES ESTATÍSTICAS\n\n")

# 6.1 Estado com maior número de acidentes
cat("▶️ Estado com maior número de acidentes:\n")
acidentes_by_state <- dados %>%
  group_by(uf) %>%
  summarise(total = n()) %>%
  arrange(desc(total))
print(head(acidentes_by_state, 1))
cat("\n-------------------------------------------------------------\n\n")

# 6.2 Probabilidade de acidente em condição de 'Céu Claro'
cat("▶️ Probabilidade de acidente em condição de 'Céu Claro':\n")
dados_claros <- dados %>% filter(condicao_metereologica == "Céu Claro")
probabilidade_ceu_claro <- nrow(dados_claros) / nrow(dados)
cat(sprintf("Total de acidentes: %d\n", nrow(dados)))
cat(sprintf("Acidentes com 'Céu Claro': %d\n", nrow(dados_claros)))
cat(sprintf("Probabilidade: %.4f (%.2f%%)\n",
            probabilidade_ceu_claro,
            probabilidade_ceu_claro * 100))
cat("\n-------------------------------------------------------------\n\n")

# 6.3 Frequência de acidentes por fase do dia
cat("▶️ Frequência de Acidentes por Fase do Dia:\n")
print(table(dados$fase_dia))
cat("\n=============================================================\n\n")

# 7. Finalizar redirecionamento
sink()

# 8. Criar e salvar gráfico
grafico_fase_dia <- ggplot(dados, aes(x = fase_dia)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Acidentes por Fase do Dia",
    x = "Fase do Dia",
    y = "Número de Acidentes"
  ) +
  theme_minimal()

# Mostrar gráfico no console
print(grafico_fase_dia)

# Salvar gráfico
ggsave(
  filename = "outputs/graphs/acidentes_fase_dia.png",
  plot = grafico_fase_dia,
  width = 10,
  height = 6
)
