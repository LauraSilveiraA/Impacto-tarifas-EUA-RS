# ==============================================================
# 09_tabela_4.R
# Tabela 4: os 15 principais produtos (SH6) exportados pelo RS
# para os EUA em 2024, com o grupo tarifário, o peso na pauta de
# 2024 e a variação entre ago/2024-fev/2025 e ago/2025-fev/2026.
#
# Usa os MESMOS dados, grupos e períodos da Tabela 3 (script 07),
# para que as duas tabelas fiquem coerentes.
#
# Entradas: Dados_processados/exp_eua_ncm.rds (gerado pelo 01)
#           Dados_processados/grupos_sh6.csv  (gerado pelo 05)
# Saídas:   Dados_processados/tabela_4.csv
#           Dados_processados/tabela_4.tex
#
# IMPORTANTE: rode o 05 (e o 07) antes.
# ==============================================================

library(tidyverse)


# --------------------------------------------------------------
# 1. PARÂMETROS (iguais aos do 07_tabela_3.R)
# --------------------------------------------------------------
pasta_proc <- "C:/TCC_2/Dados_processados"

ano_base   <- 2024
n_produtos <- 15

pos_inicio <- as.Date("2025-08-01")
pos_fim    <- as.Date("2026-02-01")
pre_inicio <- pos_inicio %m-% months(12)
pre_fim    <- pos_fim    %m-% months(12)

# Nome curto de cada grupo na tabela.
nome_curto <- c(
  "Tarifa de 50%"               = "50%",
  "Só os 40% (isento dos 10%)"  = "Só os 40%",
  "Isento a partir de nov/2025" = "Isento nov/2025",
  "Só os 10% (isento dos 40%)"  = "Só os 10%",
  "Isento desde ago/2025"       = "Isento",
  "Aeronaves civis"             = "Aeronaves",
  "Seção 232"                   = "Seção 232"
)

# Nomes dos produtos já usados no texto. Se aparecer um SH6 que não
# está aqui, a tabela mostra "(conferir nome)".
nomes <- c(
  "240120" = "Fumo total ou parcialmente destalado",
  "470329" = "Celulose branqueada de não coníferas",
  "930330" = "Carabinas e rifles de caça ou tiro esportivo",
  "850423" = "Transformadores de dielétrico líquido, $>$10 MVA",
  "440711" = "Madeira serrada de pinho",
  "640399" = "Calçados com parte superior de couro",
  "761510" = "Artefatos de alumínio de uso doméstico",
  "870899" = "Outras partes de veículos automóveis",
  "840991" = "Partes de motores de ignição por centelha",
  "681381" = "Guarnições de fricção (freios), sem amianto",
  "930320" = "Espingardas de caça ou tiro esportivo",
  "640359" = "Calçados com sola de couro",
  "270750" = "Misturas de hidrocarbonetos aromáticos",
  "870830" = "Freios e servofreios e suas partes",
  "870194" = "Tratores (75--130 kW)"
)

meses_pt <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
rot <- function(a, b) str_c(meses_pt[month(a)], "/", year(a), "--", meses_pt[month(b)], "/", year(b))


# --------------------------------------------------------------
# 2. DADOS: só o RS, com o grupo de cada SH6
# --------------------------------------------------------------
arq_grupos <- file.path(pasta_proc, "grupos_sh6.csv")
if (!file.exists(arq_grupos)) stop("Rode antes o 05: falta o grupos_sh6.csv.")
grupos_sh6 <- read_csv(arq_grupos, col_types = cols(sh6 = col_character(), grupo = col_character()))

rs <- read_rds(file.path(pasta_proc, "exp_eua_ncm.rds")) |>
  filter(uf == "RS") |>
  mutate(sh6  = str_sub(str_pad(as.character(NCM), 8, pad = "0"), 1, 6),
         data = make_date(ano, mes, 1)) |>
  left_join(grupos_sh6, by = "sh6") |>
  mutate(grupo = replace_na(grupo, "Tarifa de 50%"))


# --------------------------------------------------------------
# 3. TABELA: soma por SH6
# --------------------------------------------------------------
por_sh6 <- rs |>
  group_by(sh6, grupo) |>
  summarise(
    v_base = sum(valor_usd[year(data) == ano_base]) / 1e6,
    pre    = sum(valor_usd[data >= pre_inicio & data <= pre_fim]) / 1e6,
    pos    = sum(valor_usd[data >= pos_inicio & data <= pos_fim]) / 1e6,
    .groups = "drop"
  )

total_base <- sum(por_sh6$v_base)
queda_rs   <- sum(por_sh6$pos) - sum(por_sh6$pre)   # variação total do RS (US$ mi)

tabela <- por_sh6 |>
  mutate(peso   = 100 * v_base / total_base,
         var    = 100 * (pos - pre) / pre,
         # parte da variação total do RS que vem deste produto (%)
         parte  = 100 * (pos - pre) / queda_rs) |>
  arrange(desc(v_base)) |>
  slice_head(n = n_produtos) |>
  mutate(ordem   = row_number(),
         produto = coalesce(nomes[sh6], "(conferir nome)"),
         grupo_c = nome_curto[grupo]) |>
  select(ordem, sh6, produto, grupo = grupo_c, peso, pre, pos, var, parte)

print(mutate(tabela, across(where(is.numeric), \(x) round(x, 1))), n = Inf, width = Inf)


# --------------------------------------------------------------
# 4. NÚMEROS PARA O TEXTO DA SEÇÃO 4.5
# --------------------------------------------------------------
message("\nPeso dos ", n_produtos, " produtos na pauta de ", ano_base, ": ",
        round(sum(tabela$peso), 1), "%")
message("Variação total do RS no período: ", round(queda_rs, 1), " US$ mi")
message("Produtos do grupo de 50% ou da Seção 232 entre os ", n_produtos, ": ",
        sum(tabela$grupo %in% c("50%", "Seção 232")))

# Grupo inteiro x produto principal (a Tabela 3 tem de ser coerente com a 4).
message("\nVariação de cada grupo no RS (deve bater com a Tabela 3):")
por_sh6 |>
  group_by(grupo) |>
  summarise(pre = sum(pre), pos = sum(pos), .groups = "drop") |>
  mutate(var = round(100 * (pos - pre) / pre, 1)) |>
  print()


# --------------------------------------------------------------
# 5. SALVAR (CSV e LaTeX)
# --------------------------------------------------------------
write_csv(tabela, file.path(pasta_proc, "tabela_4.csv"))

dec   <- function(x) format(round(x, 1), nsmall = 1, decimal.mark = ",", trim = TRUE)
sinal <- function(x) str_c(if_else(x < 0, "$-$", "+"), dec(abs(x)))
sh6_p <- function(s) str_c(str_sub(s, 1, 4), ".", str_sub(s, 5, 6))   # 240120 -> 2401.20

linhas <- str_c("    ", tabela$ordem, " & ", sh6_p(tabela$sh6), " & ", tabela$produto, " & ",
                str_replace_all(tabela$grupo, "%", "\\\\%"), " & ",
                dec(tabela$peso), " & ", sinal(tabela$var), " \\\\")

tex <- c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\fontfamily{phv}\\fontsize{10}{12}\\selectfont",
  str_c("  \\caption{Os ", n_produtos, " principais produtos exportados pelo RS para os EUA em ", ano_base, "}"),
  "  \\label{tab:produtos}",
  "  \\begin{tabular}{rlp{5.2cm}lrr}",
  "    \\toprule",
  str_c("     & SH6 & Produto & Grupo & Peso ", ano_base, " (\\%) & Var. (\\%) \\\\"),
  "    \\midrule",
  linhas,
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\par\\smallskip",
  "  \\begin{minipage}{\\linewidth}",
  str_c("    Fonte: elaboração própria com dados do Comex Stat. Variação entre ",
        rot(pre_inicio, pre_fim), " e ", rot(pos_inicio, pos_fim), "."),
  "  \\end{minipage}",
  "\\end{table}"
)

write_lines(tex, file.path(pasta_proc, "tabela_4.tex"))
cat("\n", tex, sep = "\n")

message("\nTabela 4 salva em: ", pasta_proc)