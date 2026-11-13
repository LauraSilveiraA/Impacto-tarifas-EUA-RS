# ==============================================================
# 01_dados.R
# Prepara os dados de exportação usados no TCC.
#
# Organização:
#   Seções 1 a 4 -> parte comum: lê o Comex Stat uma vez, confere
#                   e monta as bases que todas as análises usam.
#   Seção 5 em diante -> uma seção por tabela ou gráfico. Cada uma
#                   só PREPARA os dados (filtra, cria colunas, marca
#                   períodos) e salva um arquivo próprio. As contas
#                   (estimar a reta, calcular desvios) ficam no
#                   script de cada tabela.
#
# Para acrescentar uma nova tabela ou gráfico: copie o modelo da
# seção 5, mude o nome e salve com um nome de arquivo novo.
# ==============================================================

library(tidyverse)   


# --------------------------------------------------------------
# 1. PARÂMETROS GERAIS
# --------------------------------------------------------------
# Tudo o que pode mudar fica aqui em cima, para não precisar
# procurar no meio do código.

pasta_projeto <- "C:/TCC_2"
pasta_comex   <- file.path(pasta_projeto, "Comex_data", "estadual")  # CSVs baixados do Comex Stat
pasta_saida   <- file.path(pasta_projeto, "Dados_processados")       # onde os resultados são salvos
dir.create(pasta_saida, showWarnings = FALSE, recursive = TRUE)      # cria a pasta se não existir

anos     <- 2016:2026   # um arquivo EXP_<ano>.csv para cada ano
uf_alvo  <- "RS"
PAIS_EUA <- 249L        # código dos Estados Unidos na coluna CO_PAIS do Comex Stat


# --------------------------------------------------------------
# 2. LEITURA DOS ARQUIVOS DO COMEX STAT
# --------------------------------------------------------------
# Os arquivos estaduais trazem uma linha por combinação de
# ano, mês, NCM, país de destino e UF. Lemos só as colunas usadas.

colunas_necessarias <- c("CO_ANO", "CO_MES", "CO_NCM", "CO_PAIS",
                         "SG_UF_NCM", "KG_LIQUIDO", "VL_FOB")

ler_comex_estadual <- function(arquivo) {
  message("Lendo: ", basename(arquivo))
  
  # Antes de ler o arquivo inteiro (que é grande), lê só o cabeçalho
  # e confere se as colunas esperadas estão lá.
  cab <- names(read_delim(arquivo, delim = ";", n_max = 0, show_col_types = FALSE))
  faltantes <- setdiff(colunas_necessarias, cab)
  if (length(faltantes) > 0) {
    stop("Faltam colunas em ", basename(arquivo), ": ",
         paste(faltantes, collapse = ", "))
  }
  
  read_delim(
    arquivo,
    delim   = ";",
    trim_ws = TRUE,
    col_types = cols_only(
      CO_ANO     = col_integer(),
      CO_MES     = col_integer(),
      CO_NCM     = col_character(),   # texto, para não perder zeros à esquerda (ex.: 02013000)
      CO_PAIS    = col_integer(),
      SG_UF_NCM  = col_character(),   # UF PRODUTORA da mercadoria (não a da empresa)
      KG_LIQUIDO = col_double(),
      VL_FOB     = col_double()       # valor FOB em dólares correntes
    )
  ) |>
    transmute(                          # renomeia para nomes mais curtos
      ano        = CO_ANO,
      mes        = CO_MES,
      uf         = SG_UF_NCM,
      NCM        = str_pad(CO_NCM, width = 8, side = "left", pad = "0"),  # garante 8 dígitos
      pais       = CO_PAIS,
      kg_liquido = KG_LIQUIDO,
      valor_fob  = VL_FOB
    )
}

# Monta o nome dos arquivos (EXP_2016.csv, EXP_2017.csv, ...),
# confere se todos existem e lê todos, empilhando um embaixo do outro.
arquivos <- file.path(pasta_comex, str_c("EXP_", anos, ".csv"))
ausentes <- arquivos[!file.exists(arquivos)]
if (length(ausentes) > 0) {
  stop("Faltam arquivos em ", pasta_comex, ": ",
       paste(basename(ausentes), collapse = ", "))
}

exp_bruto <- map_dfr(arquivos, ler_comex_estadual)


# --------------------------------------------------------------
# 3. CHECAGENS
# --------------------------------------------------------------
# Confere se a base está completa antes de usar. Olhe a saída no
# console: todo ano deve ter 12 meses (menos o último, que vai até
# o mês mais recente divulgado).

checa <- function(df, rotulo) {
  cat("\n==", rotulo, "==\n")
  ultimo_ano <- max(df$ano)
  cat("Último mês na base:", max(df$mes[df$ano == ultimo_ano]), "/", ultimo_ano, "\n")
  cat("NCMs distintas:", n_distinct(df$NCM), "| países:", n_distinct(df$pais), "\n")
  df |>
    group_by(ano) |>
    summarise(meses        = n_distinct(mes),
              total_usd_bi = sum(valor_fob, na.rm = TRUE) / 1e9,
              eua_usd_bi   = sum(valor_fob[pais == PAIS_EUA], na.rm = TRUE) / 1e9) |>
    print(n = Inf)
}

checa(exp_bruto, "Exportações do Brasil")
checa(filter(exp_bruto, uf == uf_alvo), str_c("Exportações do ", uf_alvo))


# --------------------------------------------------------------
# 4. BASES COMUNS (usadas por várias tabelas e gráficos)
# --------------------------------------------------------------

# 4.1 Painel das exportações para os EUA: uma linha por mês, UF e NCM.
#     É a base das análises por produto (grupos tarifários).
exp_eua_ncm <- exp_bruto |>
  filter(pais == PAIS_EUA) |>
  group_by(ano, mes, uf, NCM) |>
  summarise(valor_usd = sum(valor_fob,  na.rm = TRUE),
            kg_total  = sum(kg_liquido, na.rm = TRUE),
            .groups = "drop")

rm(exp_bruto); gc()   # a base bruta é grande; libera a memória

# 4.2 Séries mensais para os EUA: uma linha por mês, sem abrir por produto.
#     O Brasil soma todas as UFs, incluindo os códigos não regionalizados
#     (ND, EX), para bater com o total nacional do Comex Stat.
serie_mensal <- function(df) {
  df |>
    group_by(ano, mes) |>
    summarise(valor_usd = sum(valor_usd, na.rm = TRUE),
              kg_total  = sum(kg_total,  na.rm = TRUE),
              .groups = "drop") |>
    mutate(data = make_date(ano, mes, 1)) |>   # 1º dia do mês, para usar como data
    arrange(data)
}

exp_br_eua <- serie_mensal(exp_eua_ncm)
exp_rs_eua <- serie_mensal(filter(exp_eua_ncm, uf == uf_alvo))

# Conferência: participação do RS nas exportações brasileiras aos EUA.
cat("\nParticipação do", uf_alvo, "nas exportações do Brasil para os EUA (%):\n")
left_join(exp_br_eua, exp_rs_eua, by = c("ano", "mes", "data"), suffix = c("_br", "_rs")) |>
  group_by(ano) |>
  summarise(part_rs_pct = 100 * sum(valor_usd_rs, na.rm = TRUE) / sum(valor_usd_br)) |>
  print(n = Inf)

# Salva as bases comuns (.rds guarda os tipos, por exemplo NCM como texto).
write_rds(exp_eua_ncm, file.path(pasta_saida, "exp_eua_ncm.rds"))
write_rds(exp_br_eua,  file.path(pasta_saida, "exp_br_eua.rds"))
write_rds(exp_rs_eua,  file.path(pasta_saida, "exp_rs_eua.rds"))
