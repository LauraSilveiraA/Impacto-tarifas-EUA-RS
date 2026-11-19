# ==============================================================
# 03_grafico_1.R
# Gráfico 1: exportações mensais do RS para os EUA e tendência
# pré-tarifa, com as datas das medidas tarifárias.
#
# Entrada: Dados_processados/exp_rs_eua.rds (gerado pelo 01_dados.R)
#          Dados_processados/tabela_1.csv   (só para conferência)
# Saídas:  Graficos/grafico_1.pdf (para o LaTeX: não perde qualidade)
#          Graficos/grafico_1.png (para ver rápido ou usar fora do LaTeX)
#
# A reta é estimada exatamente como na Tabela 1. Os parâmetros da
# seção 1 precisam ser IGUAIS aos do 02_tabela_1.R; a seção 4
# confere isso automaticamente.
#
# Passo a passo:
#   1. Parâmetros  -> datas, rótulos e cores
#   2. Dados       -> lê a série e cria as colunas do modelo
#   3. Reta        -> estima e calcula a reta em todos os meses do gráfico
#   4. Conferência -> a reta bate com a da Tabela 1?
#   5. Gráfico     -> desenha camada por camada
#   6. Salvar      -> grava em PDF e PNG
# ==============================================================

library(tidyverse)   # dplyr, ggplot2, lubridate etc.
library(scales)      # formatação dos números do eixo


# --------------------------------------------------------------
# 1. PARÂMETROS
# --------------------------------------------------------------
pasta_proc <- "C:/TCC_2/Dados_processados"
pasta_graf <- "C:/TCC_2/Graficos"
dir.create(pasta_graf, showWarnings = FALSE, recursive = TRUE)

# --- Iguais aos do 02_tabela_1.R ---
inicio_janela  <- as.Date("2016-01-01")
fim_janela     <- as.Date("2025-03-01")
meses_covid    <- seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month")  # 2020 inteiro
meses_enchente <- seq(as.Date("2024-05-01"), as.Date("2024-07-01"), by = "month")

# --- Só do gráfico ---
inicio_grafico <- as.Date("2023-01-01")  # primeiro mês mostrado (a reta usa desde 2016)

# Medidas tarifárias, nas datas exatas de vigência.
eventos <- tribble(
  ~data,                 ~rotulo,
  as.Date("2025-04-05"), "Tarifa de 10%",           # EO 14257
  as.Date("2025-08-06"), "Tarifa de 50%",           # EO 14323 (+40%)
  as.Date("2025-11-13"), "Isenções agrícolas",      # EO 14361
  as.Date("2026-02-20"), "Suprema Corte",           # decisão em 20/02; Seção 122 (10%) vigora de 24/02 a 24/07
  as.Date("2026-07-22"), "Seção 301"                # 25% sobre o Brasil (22/07) e 12,5% de trabalho forçado (24/07); a Seção 122 expira em 24/07
)

meses_pt <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")

rot_obs  <- "Exportações do RS para os EUA"
rot_tend <- "Tendência pré-tarifa"
cores    <- setNames(c("#1677a8", "#fc8b00"), c(rot_obs, rot_tend))


# --------------------------------------------------------------
# 2. DADOS
# --------------------------------------------------------------
# Mesmas colunas da Tabela 1 (ver explicação no 02_tabela_1.R).
serie <- read_rds(file.path(pasta_proc, "exp_rs_eua.rds")) |>
  transmute(
    data,
    valor    = valor_usd / 1e6,                           # US$ milhões
    t        = (year(data) - 2023) * 12 + month(data) - 1, # relógio: jan/2023 = 0
    covid    = as.integer(data %in% meses_covid),
    enchente = as.integer(data %in% meses_enchente)
  )


# --------------------------------------------------------------
# 3. RETA
# --------------------------------------------------------------
# 3.1 Estima com os meses da janela (igual à Tabela 1).
base <- filter(serie, data >= inicio_janela, data <= fim_janela)
stopifnot("faltam meses na janela de estimação" =
            nrow(base) == length(seq(inicio_janela, fim_janela, by = "month")))
modelo <- lm(valor ~ t + covid + enchente, data = base)

# 3.2 Calcula a reta em TODOS os meses mostrados, inclusive os da
#     janela, com as dummies em zero. Por isso a reta passa reta pela
#     enchente de 2024: ela mostra a trajetória sem o choque.
dados <- filter(serie, data >= inicio_grafico)
dados$tend <- predict(modelo, newdata = mutate(dados, covid = 0L, enchente = 0L))

# 3.3 Posição de cada ponto no eixo: o meio do mês (dia 15).
#     Os dados são mensais, mas as medidas têm data exata. Com o ponto
#     no meio do mês, fica visível de que lado da linha cada mês cai:
#     fev/2026, por exemplo, fica antes da decisão de 20/02.
dados <- mutate(dados, x = data + days(14))


# --------------------------------------------------------------
# 4. CONFERÊNCIA COM A TABELA 1
# --------------------------------------------------------------
# A soma da reta de abr/2025 em diante tem de ser igual à coluna
# "tendencia" da linha de total da Tabela 1. Se não for, algum
# parâmetro da seção 1 está diferente do 02_tabela_1.R.
arq_tab1 <- file.path(pasta_proc, "tabela_1.csv")
if (file.exists(arq_tab1)) {
  total_tab1  <- tail(read_csv(arq_tab1, show_col_types = FALSE)$tendencia, 1)
  total_graf  <- sum(dados$tend[dados$data > fim_janela])
  if (abs(total_tab1 - total_graf) > 0.01) {
    stop("A reta do gráfico (", round(total_graf, 1), ") não bate com a da Tabela 1 (",
         round(total_tab1, 1), "). Confira os parâmetros da seção 1.")
  }
  message("Conferência ok: a reta do gráfico é a mesma da Tabela 1.")
}


# --------------------------------------------------------------
# 5. GRÁFICO
# --------------------------------------------------------------
# O ggplot desenha em camadas, na ordem em que aparecem: o que vem
# depois fica por cima.

# Formato "longo": uma linha por mês e por série, para o ggplot
# desenhar as duas linhas e montar a legenda sozinho.
longo <- bind_rows(
  transmute(dados, x, linha = rot_obs,  y = valor),
  transmute(dados, x, linha = rot_tend, y = tend)
) |>
  mutate(linha = factor(linha, levels = c(rot_obs, rot_tend)))  # ordem da legenda

pos <- filter(dados, data > fim_janela)   # meses depois das tarifas

g <- ggplot() +
  # Camada 1: área do desvio (entre o observado e a reta), só depois das tarifas.
  geom_ribbon(data = pos,
              aes(x, ymin = pmin(valor, tend), ymax = pmax(valor, tend)),
              fill = "#fce9d2") +
  # Camada 2: linhas verticais nas datas das medidas.
  geom_vline(data = eventos, aes(xintercept = data),
             linetype = "dotted", linewidth = 0.5, colour = "black") +
  # Camada 3: rótulos das linhas, em pé, no topo do gráfico.
  geom_text(data = eventos, aes(x = data, y = Inf, label = rotulo),
            angle = 90, hjust = 1.05, vjust = -0.5, size = 2.6, colour = "black") +
  # Camadas 4 e 5: as duas séries (linha + pontos).
  geom_line(data = longo, aes(x, y, colour = linha), linewidth = 0.7) +
  geom_point(data = longo, aes(x, y, colour = linha), size = 1) +
  # Cores e legenda.
  scale_colour_manual(values = cores, name = NULL) +
  # Eixo x: uma marca a cada 3 meses (jan, abr, jul, out), no formato "jan-23".
  # Os nomes dos meses são escritos aqui, para saírem em português em qualquer computador.
  scale_x_date(breaks = seq(inicio_grafico, max(dados$data) %m+% months(3), by = "3 months"),
               labels = function(d) str_c(meses_pt[month(d)], "-", format(d, "%y")),
               expand = expansion(add = 20)) +
  # Eixo y: "US$ 120 mi"; folga de 50% no topo para os rótulos não baterem nas linhas.
  scale_y_continuous(
    labels = label_number(prefix = "US$ ", suffix = " mi", big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0.05, 0.50))) +
  labs(x = NULL, y = NULL) +
  # Aparência: fundo limpo, letra de 9 pt (o "sans" do Windows é a Arial).
  theme_minimal(base_size = 9, base_family = "sans") +
  theme(legend.position  = "top",
        axis.text.x      = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

g   # mostra na tela do RStudio


# --------------------------------------------------------------
# 6. SALVAR
# --------------------------------------------------------------
# 16 cm de largura = a largura do texto numa página A4 com margens ABNT.
# Assim o gráfico entra no documento sem ser reduzido, e a letra fica
# do mesmo tamanho que você vê aqui.
ggsave(file.path(pasta_graf, "grafico_1.pdf"), g,
       width = 16, height = 9.5, units = "cm", device = cairo_pdf)  # cairo_pdf: acentos corretos
ggsave(file.path(pasta_graf, "grafico_1.png"), g,
       width = 16, height = 9.5, units = "cm", dpi = 300)

message("Gráfico 1 salvo em: ", pasta_graf)
