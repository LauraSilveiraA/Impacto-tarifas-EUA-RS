# ==============================================================
# 02_tabela_1.R
# Tabela 1: desvio das exportações do RS para os EUA em relação
# à tendência pré-tarifa, por período.
#
# Entrada: Dados_processados/exp_rs_eua.rds (gerado pelo 01_dados.R)
# Saídas:  Dados_processados/tabela_1.csv  (números da tabela)
#          Dados_processados/tabela_1.tex  (tabela pronta para o LaTeX)
#
# Passo a passo:
#   1. Parâmetros       -> datas e escolhas que podem mudar
#   2. Dados            -> lê a série e cria as colunas do modelo
#   3. Estimação        -> traça a reta com os meses antes das tarifas
#   4. Projeção         -> prolonga a reta de abr/2025 em diante
#   5. Desvio           -> compara o observado com a reta, mês a mês
#   6. Tabela           -> soma os meses de cada período
#   7. Salvar           -> grava em CSV e em LaTeX
# ==============================================================

library(tidyverse)   # carrega dplyr, readr, lubridate etc.


# --------------------------------------------------------------
# 1. PARÂMETROS
# --------------------------------------------------------------
pasta_proc <- "C:/TCC_2/Dados_processados"

# Janela de estimação: os meses usados para traçar a reta.
inicio_janela <- as.Date("2016-01-01")  # primeiro mês da base
fim_janela    <- as.Date("2025-03-01")  # último mês antes da tarifa de 10% (vigência 05/04/2025)

# Fim de cada período tarifário (1º dia do último mês do período).
fim_10 <- as.Date("2025-07-01")  # os 40% adicionais começam em 06/08/2025
fim_50 <- as.Date("2026-02-01")  # Suprema Corte decidiu em 20/02/2026: fevereiro ainda é 50%
# (use "2026-01-01" para reproduzir a versão antiga da tabela)

# Nomes dos períodos como vão aparecer na tabela.
# ATENÇÃO: se mudar as datas acima, atualize os nomes aqui.
nome_p1 <- "Abr-jul/2025 (10%)"
nome_p2 <- "Ago/2025-fev/2026 (50%)"
nome_p3 <- "Mar-set/2026 (pós-Suprema Corte)"

# Meses de choque, que ganham uma dummy.
meses_covid    <- seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month")
meses_enchente <- seq(as.Date("2024-05-01"), as.Date("2024-07-01"), by = "month")


# --------------------------------------------------------------
# 2. DADOS
# --------------------------------------------------------------
# Cada linha é um mês. Cria-se as colunas que o modelo usa:
#   valor    -> exportação do mês em US$ milhões
#   t        -> o "relógio": jan/2023 = 0, fev/2023 = 1, dez/2022 = -1...
#   covid    -> 1 nos meses da pandemia, 0 nos demais
#   enchente -> 1 nos meses da enchente, 0 nos demais

serie <- read_rds(file.path(pasta_proc, "exp_rs_eua.rds")) |>
  transmute(
    data,
    valor    = valor_usd / 1e6,                          # dólares -> milhões de dólares
    t        = (year(data) - 2023) * 12 + month(data) - 1,
    covid    = as.integer(data %in% meses_covid),        # TRUE/FALSE vira 1/0
    enchente = as.integer(data %in% meses_enchente)
  )

cat("Base vai de", format(min(serie$data), "%m/%Y"),
    "a", format(max(serie$data), "%m/%Y"), "(último mês é preliminar)\n")


# --------------------------------------------------------------
# 3. ESTIMAÇÃO DA RETA
# --------------------------------------------------------------
# Separa só os meses da janela (jan/2016 a mar/2025).
base <- filter(serie, data >= inicio_janela, data <= fim_janela)

# Confere que nenhum mês está faltando: devem ser 111.
meses_esperados <- length(seq(inicio_janela, fim_janela, by = "month"))
stopifnot("faltam meses na janela de estimação" = nrow(base) == meses_esperados)

# lm() encontra os números da equação
#   valor = α + β·t + γ₂·covid + γ₁·enchente
# que deixam a soma dos erros ao quadrado a menor possível.
modelo <- lm(valor ~ t + covid + enchente, data = base)

# Mostra os números encontrados. Na tabela "Coefficients", coluna "Estimate":
#   (Intercept) = α  -> valor da reta em jan/2023
#   t           = β  -> quanto a reta sobe por mês (US$ milhões)
#   covid       = γ₂ -> diferença média dos meses de Covid em relação à reta
#   enchente    = γ₁ -> diferença média dos meses de enchente em relação à reta
print(summary(modelo))


# --------------------------------------------------------------
# 4. PROJEÇÃO DA RETA
# --------------------------------------------------------------
# Pega os meses a partir de abr/2025 e calcula o valor da reta em
# cada um, com as dummies em zero ("como seria sem choques").
pos <- filter(serie, data > fim_janela)

pos$tend <- predict(modelo, newdata = mutate(pos, covid = 0L, enchente = 0L))


# --------------------------------------------------------------
# 5. DESVIO MÊS A MÊS
# --------------------------------------------------------------
pos <- pos |>
  mutate(
    desvio  = valor - tend,                   # quanto ficou acima (+) ou abaixo (-) da reta
    periodo = case_when(                      # em que período tarifário o mês cai
      data <= fim_10 ~ nome_p1,
      data <= fim_50 ~ nome_p2,
      TRUE           ~ nome_p3
    ),
    periodo = factor(periodo, levels = c(nome_p1, nome_p2, nome_p3))  # mantém a ordem certa
  )

# Mostra todos os meses, para conferência.
print(select(pos, data, periodo, valor, tend, desvio), n = Inf)


# --------------------------------------------------------------
# 6. TABELA: SOMA POR PERÍODO
# --------------------------------------------------------------
# Para cada grupo de meses: soma o observado, soma a reta e compara.
# O desvio em % compara as SOMAS (não é a média dos % de cada mês).
resumir <- function(df) {
  summarise(df,
            meses      = n(),
            observado  = sum(valor),
            tendencia  = sum(tend),
            desvio_usd = sum(desvio),
            desvio_pct = 100 * (sum(valor) / sum(tend) - 1))
}

tabela_1 <- bind_rows(
  pos |> group_by(periodo) |> resumir() |> mutate(periodo = as.character(periodo)),
  pos |> resumir() |> mutate(periodo = str_c("Abr/2025-", format(max(pos$data), "%b/%Y"), " (total)"))
)

print(tabela_1)


# --------------------------------------------------------------
# 7. SALVAR
# --------------------------------------------------------------
# 7.1 CSV com os números completos (sem arredondar).
write_csv(tabela_1, file.path(pasta_proc, "tabela_1.csv"))
message("Tabela 1 salva em: ", pasta_proc)
