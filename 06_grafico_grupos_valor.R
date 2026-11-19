# ==============================================================
# 06_grafico_grupos_valor.R
# Gráfico de barras empilhadas: exportações mensais para os EUA
# em US$ milhões, divididas por grupo tarifário, com um painel
# para o RS e outro para o Brasil.
#
# Diferença para o 05_grafico_grupos.R: lá cada grupo é comparado
# consigo mesmo (índice, 2024 = 100); aqui se vê o TAMANHO de cada
# grupo em dólares e o total exportado (a altura da barra).
#
# Entradas: Dados_processados/exp_eua_ncm.rds (gerado pelo 01_dados.R)
#           Dados_processados/grupos_sh6.csv  (gerado pelo 05_grafico_grupos.R)
# Saídas:   Graficos/grafico_grupos_valor.pdf e .png
#
# IMPORTANTE: rode o 05 antes. A classificação dos produtos é feita
# lá e gravada no grupos_sh6.csv; este script só a lê. Assim os dois
# gráficos usam, com certeza, os mesmos grupos.
#
# Passo a passo:
#   1. Parâmetros  -> datas, nomes e cores
#   2. Dados       -> junta o grupo às exportações e soma por mês
#   3. Conferência -> a soma dos grupos é igual ao total exportado?
#   4. Gráfico     -> desenha camada por camada
#   5. Salvar
# ==============================================================

library(tidyverse)   # dplyr, ggplot2, lubridate etc.
library(scales)      # formatação dos números


# --------------------------------------------------------------
# 1. PARÂMETROS
# --------------------------------------------------------------
pasta_proc <- "C:/TCC_2/Dados_processados"
pasta_graf <- "C:/TCC_2/Graficos"
dir.create(pasta_graf, showWarnings = FALSE, recursive = TRUE)

inicio_grafico <- as.Date("2023-01-01")

# Grupos e cores: IGUAIS aos do 05_grafico_grupos.R, para que o
# mesmo grupo tenha a mesma cor nos dois gráficos.
# A ordem aqui é a ordem da legenda e do empilhamento: o primeiro
# fica na base da barra e o último no topo.
grupos <- c(
  "Seção 232"                       = "#d93b23",
  "Aeronaves civis"                 = "#f58607",
  "Isento desde ago/2025"           = "#54ba96",
  "Só os 10% (isento dos 40%)"      = "#f7f716",
  "Isento a partir de nov/2025"     = "#d457b9",
  "Só os 40% (isento dos 10%)"      = "#448a33",
  "Tarifa de 50%"                   = "#2a78d6"
)

# Medidas tarifárias, nas datas exatas (as mesmas dos outros gráficos).
eventos <- tribble(
  ~data,                 ~rotulo,
  as.Date("2025-04-05"), "Tarifa de 10%",
  as.Date("2025-08-06"), "Tarifa de 50%",
  as.Date("2025-11-13"), "Isenções agrícolas",
  as.Date("2026-02-20"), "Suprema Corte",
  as.Date("2026-07-22"), "Seção 301"
)

meses_pt <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")


# --------------------------------------------------------------
# 2. DADOS
# --------------------------------------------------------------
# 2.1 O grupo de cada SH6 (feito no script 05).
arq_grupos <- file.path(pasta_proc, "grupos_sh6.csv")
if (!file.exists(arq_grupos)) stop("Rode antes o 05_grafico_grupos.R: falta o grupos_sh6.csv.")
grupos_sh6 <- read_csv(arq_grupos, col_types = cols(sh6 = col_character(), grupo = col_character()))

# 2.2 Exportações por NCM, com o SH6 (6 primeiros dígitos da NCM).
exp_ncm <- read_rds(file.path(pasta_proc, "exp_eua_ncm.rds")) |>
  mutate(sh6  = str_sub(str_pad(as.character(NCM), 8, pad = "0"), 1, 6),
         data = make_date(ano, mes, 1)) |>
  filter(data >= inicio_grafico)

# 2.3 Duas regiões: o RS e o Brasil (todos os estados, RS incluído).
exp_regiao <- bind_rows(
  exp_ncm |> filter(uf == "RS") |> mutate(regiao = "Rio Grande do Sul"),
  exp_ncm |>                       mutate(regiao = "Brasil")
) |>
  mutate(regiao = factor(regiao, levels = c("Rio Grande do Sul", "Brasil")))

# 2.4 Junta o grupo e soma por região, grupo e mês.
#     Produto fora de todas as listas paga a tarifa cheia.
mensal <- exp_regiao |>
  left_join(grupos_sh6, by = "sh6") |>
  mutate(grupo = replace_na(grupo, "Tarifa de 50%")) |>
  group_by(regiao, grupo, data) |>
  summarise(valor = sum(valor_usd) / 1e6, .groups = "drop") |>      # US$ milhões
  mutate(
    # rev(): o ggplot empilha de cima para baixo na ordem do fator;
    # invertendo, o 1º grupo da lista fica na BASE da barra.
    grupo = factor(grupo, levels = rev(names(grupos))),
    x     = data + days(14)          # barra centrada no meio do mês (dia 15)
  )


# --------------------------------------------------------------
# 3. CONFERÊNCIA
# --------------------------------------------------------------
# Nenhum dólar pode sumir nem ser contado duas vezes: a soma das
# fatias de cada barra tem de ser igual ao total exportado no mês.
total_fatias <- sum(mensal$valor)
total_dados  <- sum(exp_regiao$valor_usd) / 1e6
stopifnot("a soma dos grupos não bate com o total exportado" =
            abs(total_fatias - total_dados) < 0.01)
message("Conferência ok: a soma dos grupos é igual ao total exportado.")


# --------------------------------------------------------------
# 4. GRÁFICO
# --------------------------------------------------------------
g <- ggplot(mensal) +
  # Camada 1: barras empilhadas. Cada barra tem 26 dias de largura
  # (sobra um espaço entre os meses); o contorno branco separa as fatias.
  geom_col(aes(x, valor, fill = grupo), width = 26, colour = "white", linewidth = 0.15) +
  # Camada 2: linhas verticais nas datas das medidas (por cima das barras).
  geom_vline(data = eventos, aes(xintercept = data),
             linetype = "dotted", linewidth = 0.4, colour = "grey20") +
  # Camada 3: rótulos das medidas, em pé, no topo.
  geom_text(data = eventos, aes(x = data, y = Inf, label = rotulo),
            angle = 90, hjust = 1.05, vjust = -0.5, size = 2.3, colour = "grey30") +
  # Um painel por região, um embaixo do outro; cada um com seu eixo y
  # (o Brasil exporta cerca de 20 vezes mais que o RS).
  facet_wrap(~ regiao, ncol = 1, scales = "free_y") +
  # breaks = names(grupos): a legenda segue a ordem da lista da seção 1.
  scale_fill_manual(values = grupos, breaks = names(grupos), name = NULL) +
  scale_x_date(breaks = seq(inicio_grafico, max(mensal$data) %m+% months(3), by = "3 months"),
               labels = function(d) str_c(meses_pt[month(d)], "-", format(d, "%y")),
               expand = expansion(add = 20)) +
  # Barras começam no zero; folga de 45% no topo para os rótulos.
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.45))) +
  labs(x = NULL, y = "US$ milhões (FOB)") +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_minimal(base_size = 9, base_family = "sans") +
  theme(legend.position    = "top",
        legend.key.size    = unit(0.35, "cm"),
        axis.text.x        = element_text(angle = 45, hjust = 1),
        strip.text         = element_text(face = "bold", hjust = 0),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank())

g   # mostra na tela do RStudio


# --------------------------------------------------------------
# 5. SALVAR
# --------------------------------------------------------------
ggsave(file.path(pasta_graf, "grafico_grupos_valor.pdf"), g,
       width = 16, height = 17, units = "cm", device = cairo_pdf)
ggsave(file.path(pasta_graf, "grafico_grupos_valor.png"), g,
       width = 16, height = 17, units = "cm", dpi = 300, bg = "white")

message("Gráfico de barras salvo em: ", pasta_graf)
