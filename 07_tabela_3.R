# ==============================================================
# 07_tabela_3.R
# Tabela 3: exportações para os EUA por grupo tarifário, no RS e
# no Brasil. Para cada grupo: peso na pauta de 2024, valor
# exportado antes e depois da tarifa de 50% e variação (%).
#
# Entradas: Dados_processados/exp_eua_ncm.rds (gerado pelo 01_dados.R)
#           Dados_processados/grupos_sh6.csv  (gerado pelo 05_grafico_grupos.R)
# Saídas:   Dados_processados/tabela_3.csv
#           Dados_processados/tabela_3.tex    (tabela pronta para o LaTeX)
#
# IMPORTANTE: rode o 05 antes. A classificação dos produtos vem do
# grupos_sh6.csv, a mesma dos dois gráficos de grupos.
#
# A comparação: os meses da tarifa de 50% ("pós") contra os MESMOS
# meses do ano anterior ("pré"). Comparar os mesmos meses tira o
# efeito da sazonalidade (safra, fim de ano etc.).
#
# Passo a passo:
#   1. Parâmetros  -> os dois períodos e a ordem dos grupos
#   2. Dados       -> junta o grupo às exportações
#   3. Tabela      -> peso, pré, pós e variação por grupo
#   4. Conferência -> o "pós" do RS bate com a Tabela 1?
#   5. Salvar      -> CSV e LaTeX
# ==============================================================

library(tidyverse)


# --------------------------------------------------------------
# 1. PARÂMETROS
# --------------------------------------------------------------
pasta_proc <- "C:/TCC_2/Dados_processados"

ano_base <- 2024                         # ano do peso na pauta

# "Pós": o período da tarifa de 50% -- o MESMO da Tabela 1
# (ago/2025 a fev/2026, 7 meses).
pos_inicio <- as.Date("2025-08-01")
pos_fim    <- as.Date("2026-02-01")

# "Pré": os mesmos meses, um ano antes (ago/2024 a fev/2025).
# %m-% months(12) = "volta 12 meses".
pre_inicio <- pos_inicio %m-% months(12)
pre_fim    <- pos_fim    %m-% months(12)

# Ordem das linhas na tabela (os mesmos nomes do script 05).
ordem_grupos <- c(
  "Tarifa de 50%",
  "Só os 40% (isento dos 10%)",
  "Isento a partir de nov/2025",
  "Só os 10% (isento dos 40%)",
  "Isento desde ago/2025",
  "Aeronaves civis",
  "Seção 232"
)

meses_pt <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")


# --------------------------------------------------------------
# 2. DADOS
# --------------------------------------------------------------
arq_grupos <- file.path(pasta_proc, "grupos_sh6.csv")
if (!file.exists(arq_grupos)) stop("Rode antes o 05_grafico_grupos.R: falta o grupos_sh6.csv.")
grupos_sh6 <- read_csv(arq_grupos, col_types = cols(sh6 = col_character(), grupo = col_character()))

exp_ncm <- read_rds(file.path(pasta_proc, "exp_eua_ncm.rds")) |>
  mutate(sh6  = str_sub(str_pad(as.character(NCM), 8, pad = "0"), 1, 6),
         data = make_date(ano, mes, 1))

# Duas regiões (RS e Brasil inteiro) e o grupo de cada produto.
# Produto fora de todas as listas paga a tarifa cheia.
dados <- bind_rows(
  exp_ncm |> filter(uf == "RS") |> mutate(regiao = "RS"),
  exp_ncm |>                       mutate(regiao = "Brasil")
) |>
  left_join(grupos_sh6, by = "sh6") |>
  mutate(grupo  = replace_na(grupo, "Tarifa de 50%"),
         regiao = factor(regiao, levels = c("RS", "Brasil")),
         grupo  = factor(grupo,  levels = ordem_grupos))

# Os dois períodos têm de estar completos na base.
stopifnot("faltam meses do período pós na base" = max(dados$data) >= pos_fim,
          "faltam meses do período pré na base" = min(dados$data) <= pre_inicio)


# --------------------------------------------------------------
# 3. TABELA
# --------------------------------------------------------------
# Para cada região e grupo, três somas, em US$ milhões:
#   v_base = exportado no ano de 2024      (para o peso)
#   pre    = exportado nos meses do "pré"
#   pos    = exportado nos meses do "pós"
# sum(valor_usd[condição]) soma só as linhas que atendem à condição.
tabela <- dados |>
  group_by(regiao, grupo) |>
  summarise(
    v_base = sum(valor_usd[year(data) == ano_base]) / 1e6,
    pre    = sum(valor_usd[data >= pre_inicio & data <= pre_fim]) / 1e6,
    pos    = sum(valor_usd[data >= pos_inicio & data <= pos_fim]) / 1e6,
    .groups = "drop"
  )

# Linha de total de cada região: soma de todos os grupos.
totais <- tabela |>
  group_by(regiao) |>
  summarise(grupo = "Total", across(c(v_base, pre, pos), sum))

# Junta os grupos e o total; calcula o peso e a variação.
#   peso = valor do grupo em 2024 ÷ total da região em 2024
#   var  = (pós − pré) ÷ pré
tabela <- bind_rows(mutate(tabela, grupo = as.character(grupo)), totais) |>
  group_by(regiao) |>
  mutate(peso = 100 * v_base / v_base[grupo == "Total"],
         var  = 100 * (pos - pre) / pre) |>
  ungroup() |>
  arrange(regiao, match(grupo, c(ordem_grupos, "Total"))) |>
  select(regiao, grupo, peso, pre, pos, var)

print(mutate(tabela, across(where(is.numeric), \(x) round(x, 1))), n = Inf)


# --------------------------------------------------------------
# 4. CONFERÊNCIA COM A TABELA 1
# --------------------------------------------------------------
# O total "pós" do RS é o valor observado do período de 50% da
# Tabela 1 (2ª linha). Se não bater, os períodos estão diferentes.
arq_tab1 <- file.path(pasta_proc, "tabela_1.csv")
if (file.exists(arq_tab1)) {
  obs_tab1 <- read_csv(arq_tab1, show_col_types = FALSE)$observado[2]
  pos_rs   <- tabela$pos[tabela$regiao == "RS" & tabela$grupo == "Total"]
  if (abs(obs_tab1 - pos_rs) > 0.5) {
    warning("O total pós do RS (", round(pos_rs), ") difere do observado na Tabela 1 (",
            round(obs_tab1), "). Confira os períodos da seção 1.")
  } else message("Conferência ok: o total pós do RS é o mesmo da Tabela 1.")
}


# --------------------------------------------------------------
# 5. SALVAR
# --------------------------------------------------------------
write_csv(tabela, file.path(pasta_proc, "tabela_3.csv"))

# --- LaTeX ---
# Funções de formatação no padrão brasileiro:
#   num(1234.5)    -> "1.235"   (sem casas, ponto de milhar)
#   dec(12.34)     -> "12,3"    (uma casa, vírgula decimal)
#   sinal(-12.34)  -> "$-$12,3" ; sinal(2.6) -> "+2,6"
num   <- function(x) format(round(x), big.mark = ".", decimal.mark = ",", trim = TRUE)
dec   <- function(x) format(round(x, 1), nsmall = 1, decimal.mark = ",", trim = TRUE)
sinal <- function(x) str_c(if_else(x < 0, "$-$", "+"), dec(abs(x)))

# Rótulo de um período: "ago/2024--fev/2025".
rot <- function(a, b) str_c(meses_pt[month(a)], "/", year(a), "--", meses_pt[month(b)], "/", year(b))

# Uma linha do LaTeX para cada linha da tabela. O total vai em negrito
# na variação; a região aparece só na primeira linha do bloco.
linhas <- tabela |>
  mutate(
    grupo_tex = str_replace_all(grupo, "%", "\\\\%"),          # % é comentário no LaTeX
    var_tex   = if_else(grupo == "Total", str_c("\\textbf{", sinal(var), "}"), sinal(var)),
    regiao_tex = if_else(row_number() == 1, as.character(regiao), ""), .by = regiao
  ) |>
  mutate(tex = str_c("    ", regiao_tex, " & ",
                     if_else(grupo == "Total", "\\textbf{Total}", grupo_tex), " & ",
                     dec(peso), " & ", num(pre), " & ", num(pos), " & ", var_tex, " \\\\"))

bloco <- function(r) linhas$tex[linhas$regiao == r]

tex <- c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\fontfamily{phv}\\fontsize{10}{12}\\selectfont",
  str_c("  \\caption{Exportações para os EUA por grupo tarifário: peso em ", ano_base,
        " e variação entre ", rot(pre_inicio, pre_fim), " e ", rot(pos_inicio, pos_fim), "}"),
  "  \\label{tab:grupos}",
  "  \\begin{tabular}{llrrrr}",
  "    \\toprule",
  str_c("     & Grupo & Peso ", ano_base, " & Pré & Pós & Var. \\\\"),
  "     &  & (\\%) & (US\\$ mi) & (US\\$ mi) & (\\%) \\\\",
  "    \\midrule",
  bloco("RS"),
  "    \\midrule",
  bloco("Brasil"),
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\par\\smallskip",
  "  \\begin{minipage}{\\linewidth}",
  str_c("    Fonte: elaboração própria com dados do Comex Stat, das Ordens Executivas 14257, 14323, ",
        "14360 e 14361 e do Budget Lab at Yale (Seção 232 e atualizações do Anexo II). ",
        "Correspondência por SH6. Pré: ", rot(pre_inicio, pre_fim), "; pós: ", rot(pos_inicio, pos_fim), "."),
  "  \\end{minipage}",
  "\\end{table}"
)

write_lines(tex, file.path(pasta_proc, "tabela_3.tex"))   # grava em UTF-8 (acentos corretos)
cat(tex, sep = "\n")                                       # mostra no console para copiar

message("\nTabela 3 salva em: ", pasta_proc)