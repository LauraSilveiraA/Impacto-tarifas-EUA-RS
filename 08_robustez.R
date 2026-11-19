#==============================================================
  # 08_robustez.R
  # Robustez do desvio da Tabela 1. Responde a três perguntas:
  #
  #   A) O resultado depende da janela de estimação ou da forma da
  #      tendência?                                   -> Tabela 2
  #   B) Por que a dummy da Covid cobre 2020 inteiro e a da enchente
  #      cobre mai-jul/2024?                          -> seção 5
  #   C) Quanto o método erra quando NÃO há tarifa?   -> seção 6 (placebo)
  #
  # Entrada: Dados_processados/exp_rs_eua.rds (gerado pelo 01_dados.R)
  # Saídas:  Dados_processados/tabela_2.csv e tabela_2.tex
  #          Dados_processados/robustez_dummies.csv
  #
  # Os parâmetros da seção 1 têm de ser IGUAIS aos do 02_tabela_1.R.
  # A primeira linha da Tabela 2 ("principal") tem de repetir o total
  # da Tabela 1; a seção 4 confere isso.
  #
  # Passo a passo:
  #   1. Parâmetros
  #   2. Dados
  #   3. Função que estima uma especificação e devolve o desvio
  #   4. Tabela 2: janelas e forma da tendência
  #   5. Escolha das dummies: resíduos e comparação de alternativas
  #   6. Placebo: o método aplicado a um ano sem tarifa
  #   7. Salvar
  # ==============================================================

library(tidyverse)


# --------------------------------------------------------------
# 1. PARÂMETROS (iguais aos do 02_tabela_1.R)
# --------------------------------------------------------------
pasta_proc <- "C:/TCC_2/Dados_processados"

inicio_janela <- as.Date("2016-01-01")
fim_janela    <- as.Date("2025-03-01")

# Função auxiliar: lista de meses entre duas datas.
meses <- function(de, ate) seq(as.Date(de), as.Date(ate), by = "month")

meses_covid    <- meses("2020-01-01", "2020-12-01")   # 2020 inteiro
meses_enchente <- meses("2024-05-01", "2024-07-01")   # mai-jul/2024

# Fim de cada período tarifário (como na Tabela 1).
fim_10 <- as.Date("2025-07-01")   # abr-jul/2025: tarifa de 10%
fim_50 <- as.Date("2026-02-01")   # ago/2025-fev/2026: tarifa de 50%; depois: pós-Suprema Corte


# --------------------------------------------------------------
# 2. DADOS
# --------------------------------------------------------------
serie <- read_rds(file.path(pasta_proc, "exp_rs_eua.rds")) |>
  transmute(
    data,
    valor = valor_usd / 1e6,                             # US$ milhões
    t     = (year(data) - 2023) * 12 + month(data) - 1,  # relógio: jan/2023 = 0
    mes   = factor(month(data))                          # mês do ano (1 a 12), para os efeitos mensais
  )


# --------------------------------------------------------------
# 3. FUNÇÃO: ESTIMA UMA ESPECIFICAÇÃO E DEVOLVE O DESVIO
# --------------------------------------------------------------
# É o mesmo cálculo da Tabela 1, mas com tudo que pode variar
# virando argumento:
#   inicio    -> primeiro mês da janela de estimação
#   fim       -> último mês da janela
#   covid     -> meses da dummy da Covid (NULL = sem dummy)
#   enchente  -> meses da dummy da enchente (NULL = sem dummy)
#   mensal    -> TRUE acrescenta um efeito para cada mês do ano
#   projetar  -> meses em que se compara o observado com a tendência
#                (padrão: tudo depois da janela)
#
# Devolve uma linha com o desvio e as medidas de ajuste do modelo.
estima <- function(nome, inicio = inicio_janela, fim = fim_janela,
                   covid = meses_covid, enchente = meses_enchente,
                   mensal = FALSE, projetar = NULL) {
  
  # 3.1 Cria as dummies desta especificação.
  d <- serie |>
    mutate(covid    = as.integer(data %in% covid),
           enchente = as.integer(data %in% enchente))
  
  base <- filter(d, data >= inicio, data <= fim)
  
  # 3.2 Monta a fórmula. Uma dummy só entra se tiver algum "1" dentro
  #     da janela: numa janela que começa em 2021, por exemplo, não há
  #     nenhum mês de 2020 e a dummy da Covid não tem o que estimar.
  termos <- "t"
  if (sum(base$covid)    > 0) termos <- c(termos, "covid")
  if (sum(base$enchente) > 0) termos <- c(termos, "enchente")
  if (mensal)                 termos <- c(termos, "mes")
  modelo <- lm(reformulate(termos, response = "valor"), data = base)
  
  # 3.3 Projeta com as dummies em zero e compara com o observado.
  pos <- if (is.null(projetar)) filter(d, data > fim) else filter(d, data %in% projetar)
  pos$tend <- predict(modelo, newdata = mutate(pos, covid = 0L, enchente = 0L))
  
  # 3.4 Desvio (%) de um pedaço da projeção: compara as somas dos meses
  #     entre duas datas. Nos placebos não há esses meses e o resultado é NA.
  pct <- function(de, ate) {
    p <- filter(pos, data >= de, data <= ate)
    if (nrow(p) == 0) NA_real_ else 100 * (sum(p$valor) / sum(p$tend) - 1)
  }
  
  tibble(
    especificacao = nome,
    n_meses       = nrow(base),                               # meses usados na estimação
    inclinacao    = unname(coef(modelo)["t"]),                # US$ mi por mês
    r2_ajustado   = summary(modelo)$adj.r.squared,
    aic           = AIC(modelo),                              # quanto MENOR, melhor
    bic           = BIC(modelo),                              # idem; pune mais quem tem mais variáveis
    observado     = sum(pos$valor),
    tendencia     = sum(pos$tend),
    desvio_usd    = sum(pos$valor) - sum(pos$tend),
    desvio_pct    = 100 * (sum(pos$valor) / sum(pos$tend) - 1),
    # desvio (%) em cada período tarifário
    pct_10        = pct(fim_janela %m+% months(1), fim_10),
    pct_50        = pct(fim_10     %m+% months(1), fim_50),
    pct_pos       = pct(fim_50     %m+% months(1), max(serie$data))
  )
}


# --------------------------------------------------------------
# 4. TABELA 2: JANELA E FORMA DA TENDÊNCIA
# --------------------------------------------------------------
# Cada linha muda UMA coisa em relação à especificação principal.
tabela_2 <- bind_rows(
  estima("Janela desde 2016 (principal)"),
  estima("Janela desde 2019",              inicio = as.Date("2019-01-01")),
  estima("Janela desde 2021",              inicio = as.Date("2021-01-01")),
  estima("Janela desde 2023",              inicio = as.Date("2023-01-01")),
  estima("Desde 2016, com efeitos mensais", mensal = TRUE)
)

message("TABELA 2 - sensibilidade à janela e à forma da tendência:")
print(mutate(tabela_2, across(where(is.numeric), \(x) round(x, 2))), width = Inf)

# O mesmo, aberto por período tarifário (desvio em %). É aqui que se
# vê se a queda do período de 50% resiste à troca da janela.
message("\nDesvio (%) por período tarifário, em cada especificação:")
tabela_2 |>
  select(especificacao, pct_10, pct_50, pct_pos, total = desvio_pct) |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |> print(width = Inf)

# Conferência: a linha principal é o total da Tabela 1?
arq_tab1 <- file.path(pasta_proc, "tabela_1.csv")
if (file.exists(arq_tab1)) {
  total_tab1 <- tail(read_csv(arq_tab1, show_col_types = FALSE)$desvio_usd, 1)
  if (abs(total_tab1 - tabela_2$desvio_usd[1]) > 0.01) {
    stop("A linha principal (", round(tabela_2$desvio_usd[1], 1), ") não bate com a Tabela 1 (",
         round(total_tab1, 1), "). Confira os parâmetros da seção 1.")
  }
  message("Conferência ok: a linha principal repete o total da Tabela 1.")
}


# --------------------------------------------------------------
# 5. ESCOLHA DAS DUMMIES
# --------------------------------------------------------------
# 5.1 Resíduos do modelo SEM nenhuma dummy.
#     Estima-se só a reta (valor ~ t) e olha-se o resíduo de cada mês:
#     observado menos reta. Dividido pelo desvio-padrão dos resíduos,
#     ele vira "quantos desvios-padrão o mês ficou fora da reta".
#     Um choque aparece como uma sequência de meses negativos.
base0 <- filter(serie, data >= inicio_janela, data <= fim_janela)
m0    <- lm(valor ~ t, data = base0)
base0 <- mutate(base0,
                residuo        = resid(m0),
                residuo_padrao = residuo / sd(residuo))

message("\nResíduos de 2020 (modelo sem dummies): quais meses ficaram abaixo da reta?")
base0 |> filter(year(data) == 2020) |>
  select(data, valor, residuo, residuo_padrao) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |> print(n = Inf)

message("\nResíduos de 2024 (modelo sem dummies): em que meses a enchente aparece?")
base0 |> filter(year(data) == 2024) |>
  select(data, valor, residuo, residuo_padrao) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |> print(n = Inf)

# Resumo de 2020: quantos meses abaixo da reta e a média do ano.
r2020 <- filter(base0, year(data) == 2020)
message("\n2020: ", sum(r2020$residuo < 0), " de 12 meses abaixo da reta; resíduo médio de ",
        round(mean(r2020$residuo), 1), " US$ mi (", round(mean(r2020$residuo_padrao), 2),
        " desvio-padrão).")

# 5.2 Comparação de alternativas.
#     Todas usam a mesma janela e os mesmos meses, então o R² ajustado,
#     o AIC e o BIC são comparáveis entre elas. As duas últimas colunas
#     mostram se o RESULTADO muda.
dummies <- bind_rows(
  estima("Principal: Covid 2020 inteiro + enchente mai-jul/2024"),
  # --- alternativas para a Covid (enchente igual à principal) ---
  estima("Covid: sem dummy",                covid = NULL),
  estima("Covid: mar-jun/2020",             covid = meses("2020-03-01", "2020-06-01")),
  estima("Covid: mar-dez/2020",             covid = meses("2020-03-01", "2020-12-01")),
  # --- alternativas para a enchente (Covid igual à principal) ---
  estima("Enchente: sem dummy",             enchente = NULL),
  estima("Enchente: só mai/2024",           enchente = meses("2024-05-01", "2024-05-01")),
  estima("Enchente: mai-jun/2024",          enchente = meses("2024-05-01", "2024-06-01")),
  estima("Enchente: mai-out/2024",          enchente = meses("2024-05-01", "2024-10-01")),
  # --- sem nenhuma dummy ---
  estima("Sem nenhuma dummy",               covid = NULL, enchente = NULL)
)

message("\nAlternativas para as dummies (R² ajustado maior e AIC/BIC menores = melhor ajuste):")
dummies |>
  select(especificacao, r2_ajustado, aic, bic, desvio_usd, desvio_pct) |>
  mutate(across(c(r2_ajustado), \(x) round(x, 3)),
         across(c(aic, bic, desvio_usd, desvio_pct), \(x) round(x, 1))) |>
  print(n = Inf, width = Inf)


# --------------------------------------------------------------
# 6. PLACEBO: O MÉTODO APLICADO A UM ANO SEM TARIFA
# --------------------------------------------------------------
# Pergunta: se a reta fosse estimada até mar/2024 e projetada para
# abr/2024-mar/2025 (12 meses SEM tarifa), que "desvio" apareceria?
# Esse número é o erro típico do método. Se o desvio depois das
# tarifas for muito maior que ele, não é um artefato da reta.
#
# São dois placebos:
#   - com a enchente: o desvio inclui o efeito da enchente de 2024;
#   - o ano anterior (abr/2023-mar/2024), sem enchente nem tarifa.
placebo <- bind_rows(
  estima("Placebo: reta até mar/2024, projeção abr/2024-mar/2025",
         fim = as.Date("2024-03-01"), enchente = NULL,
         projetar = meses("2024-04-01", "2025-03-01")),
  estima("Placebo: reta até mar/2023, projeção abr/2023-mar/2024",
         fim = as.Date("2023-03-01"), enchente = NULL,
         projetar = meses("2023-04-01", "2024-03-01"))
)

# Nomes curtos para a tabela do LaTeX.
placebo$rotulo <- c("Tendência até mar/2024, projeção abr/2024--mar/2025",
                    "Tendência até mar/2023, projeção abr/2023--mar/2024")

message("\nPlacebos (12 meses sem tarifa) e, para comparar, o resultado principal:")
bind_rows(placebo, tabela_2[1, ]) |>
  select(especificacao, observado, tendencia, desvio_usd, desvio_pct) |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  print(width = Inf)


# --------------------------------------------------------------
# 7. SALVAR
# --------------------------------------------------------------
write_csv(tabela_2, file.path(pasta_proc, "tabela_2.csv"))
write_csv(bind_rows(dummies, select(placebo, -rotulo)), file.path(pasta_proc, "robustez_dummies.csv"))

# --- Tabela 2 em LaTeX ---
# br(): número no padrão brasileiro, com sinal ($-$ ou +); NA vira "--".
br <- function(x, casas = 1) {
  txt <- format(round(abs(x), casas), nsmall = casas, big.mark = ".", decimal.mark = ",", trim = TRUE)
  if_else(is.na(x), "--", str_c(if_else(x < 0, "$-$", "+"), txt))
}
ultimo   <- max(serie$data)
meses_pt <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")

# Uma linha do LaTeX: nome + desvio em cada período + total.
linha <- function(nome, df) {
  str_c("    ", nome, " & ", br(df$pct_10), " & ", br(df$pct_50), " & ",
        br(df$pct_pos), " & ", br(df$desvio_pct), " \\\\")
}

tex <- c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\fontfamily{phv}\\fontsize{10}{12}\\selectfont",
  str_c("  \\caption{Sensibilidade do desvio (\\%) à especificação, por período (abr/2025--",
        meses_pt[month(ultimo)], "/", year(ultimo), ")}"),
  "  \\label{tab:robustez}",
  "  \\begin{tabular}{lrrrr}",
  "    \\toprule",
  "    Especificação & 10\\% & 50\\% & Pós-Suprema & Total \\\\",
  "     &  &  & Corte &  \\\\",
  "    \\midrule",
  linha(tabela_2$especificacao, tabela_2),
  "    \\midrule",
  "    \\multicolumn{5}{l}{\\textit{Placebo: doze meses sem tarifa}} \\\\",
  linha(placebo$rotulo, placebo),
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\par\\smallskip",
  "  \\begin{minipage}{\\linewidth}",
  str_c("    Fonte: elaboração própria com dados do Comex Stat. Desvio entre o exportado e a ",
        "tendência, em \\% da tendência. 10\\%: abr--jul/2025; 50\\%: ago/2025--fev/2026; ",
        "pós-Suprema Corte: mar--", meses_pt[month(ultimo)], "/", year(ultimo),
        ". Nos placebos, a tendência é estimada até o mês indicado e projetada para os doze meses seguintes."),
  "  \\end{minipage}",
  "\\end{table}"
)
write_lines(tex, file.path(pasta_proc, "tabela_2.tex"))
cat("\n", tex, sep = "\n")

message("\nRobustez salva em: ", pasta_proc)
