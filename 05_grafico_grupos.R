# ==============================================================
# 05_grafico_grupos.R
# Gráfico dos grupos tarifários: exportações para os EUA por
# tratamento tarifário, em índice (média de 2024 = 100), com um
# painel para o RS e outro para o Brasil.
#
# Entradas: Dados_processados/exp_eua_ncm.rds (gerado pelo 01_dados.R)
#           Dados_processados/listas_hs10.csv (gerado pelo 04_listas_tarifas.R corrigido)
# Saídas:   Graficos/grafico_grupos.pdf e .png
#           Dados_processados/grupos_sh6.csv   (o grupo de cada produto)
#           Dados_processados/pesos_grupos.csv (peso de cada grupo em 2024)
#
# A ideia: cada produto (SH6) vai para UM grupo, conforme o
# tratamento que recebeu nas ordens executivas. Depois soma-se a
# exportação de cada grupo, mês a mês, e compara-se com 2024.
#
# Passo a passo:
#   1. Parâmetros    -> datas, regra de classificação, nomes e cores
#   2. Listas        -> em que listas cada SH6 está, em cada data
#   3. Grupos        -> um grupo por SH6, por ordem de prioridade
#   4. Exportações   -> junta o grupo aos dados e soma por mês
#   5. Pesos         -> quanto cada grupo pesava em 2024
#   6. Robustez      -> os pesos mudam com a outra regra?
#   7. Índice        -> média de 2024 = 100 e média móvel de 3 meses
#   8. Gráfico       -> desenha camada por camada
#   9. Salvar
# ==============================================================

library(tidyverse)   # dplyr, ggplot2, lubridate etc.
library(scales)      # formatação dos números


# --------------------------------------------------------------
# 1. PARÂMETROS
# --------------------------------------------------------------
pasta_proc <- "C:/TCC_2/Dados_processados"
pasta_graf <- "C:/TCC_2/Graficos"
dir.create(pasta_graf, showWarnings = FALSE, recursive = TRUE)

# --- Datas em que se "fotografam" as listas ---
d_ago <- as.Date("2025-08-06")   # entrada em vigor dos 40% (EO 14323)
d_nov <- as.Date("2025-11-13")   # isenções agrícolas (EO 14360 e 14361)

# --- Regra de classificação ---
# As listas dos EUA estão em HTS (8 ou 10 dígitos) e o Comex Stat em
# NCM. Os dois sistemas só são iguais até o 6º dígito (SH6). Um SH6
# pode ter algumas linhas na lista e outras fora. A "cobertura" é a
# fração das linhas do SH6 que está na lista.
#   0.5 -> o SH6 entra na lista se METADE OU MAIS das linhas estiver nela
#   0   -> basta UMA linha (regra antiga)
regra_cobertura <- 0.5

# --- Aeronaves ---
# A isenção de "aeronaves civis" lista centenas de produtos comuns
# (tubos, parafusos, cabos), mas eles só são isentos SE forem usados
# em aeronave civil. O Comex Stat não informa o uso. Por isso, só os
# capítulos abaixo entram no grupo de aeronaves; os demais produtos
# dessa lista são tratados como se a isenção não valesse.
caps_aeronave <- c("88")         # capítulo 88: aeronaves e suas partes

# --- Gráfico ---
inicio_grafico <- as.Date("2024-01-01")
ano_base       <- 2024           # índice: média mensal deste ano = 100
peso_minimo    <- 1              # grupo com menos de 1% da pauta não é desenhado no painel

# Grupos, na ordem de prioridade (seção 3), e a cor de cada um.
# A cor é do grupo: ela não muda se algum grupo sair do gráfico.
grupos <- c(
  "Seção 232"                       = "#d93b23",
  "Aeronaves civis"                 = "#f58607",
  "Isento desde ago/2025"           = "#54ba96",
  "Só os 10% (isento dos 40%)"      = "#f7f716",
  "Isento a partir de nov/2025"     = "#d457b9",
  "Só os 40% (isento dos 10%)"      = "#448a33",
  "Tarifa de 50%"                   = "#2a78d6"
)

# Medidas tarifárias, nas datas exatas (as mesmas do Gráfico 1).
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
# 2. LISTAS: EM QUE LISTAS CADA SH6 ESTÁ, EM CADA DATA
# --------------------------------------------------------------
# Uma linha por lista e por linha tarifária americana (HTS10).
listas <- read_csv(file.path(pasta_proc, "listas_hs10.csv"),
                   col_types = cols(sh6 = col_character(), hs10 = col_character(),
                                    base = col_character(),
                                    inicio = col_date(), fim = col_date(),
                                    .default = col_guess()))

# Função: dado um pedaço das listas e uma data, devolve os SH6 que
# estão na lista naquela data, segundo a regra de cobertura.
#   - fica só com as linhas em vigor na data (inicio <= data <= fim;
#     fim vazio = ainda em vigor);
#   - conta as linhas HTS10 DISTINTAS do SH6 que estão na lista. Uma
#     linha que aparece em duas categorias (autopeças e peças de
#     caminhão, por exemplo) conta uma vez só;
#   - divide pelo total de linhas do SH6 e devolve os que passam na regra.
na_lista <- function(pedaco, data, regra) {
  pedaco |>
    filter(inicio <= data, is.na(fim) | fim >= data) |>
    distinct(sh6, hs10, n_linhas_sh6) |>
    count(sh6, n_linhas_sh6, name = "n_na_lista") |>
    mutate(cobertura = n_na_lista / n_linhas_sh6) |>
    filter(cobertura > 0, cobertura >= regra) |>
    pull(sh6)
}

# Pedaços das listas.
l_232   <- filter(listas, medida == "s232", base == "valor_total")   # tarifa sobre o valor todo do produto
l_10    <- filter(listas, medida == "reciproca_10")                  # Anexo II: isento dos 10%
l_40    <- filter(listas, medida == "brasil_40", categoria %in% c("isento", "isento_parcial"))
l_aero  <- filter(listas, medida == "brasil_40", categoria == "aeronave_civil")

# Obs.: os derivados de aço/alumínio e o cobre (base "conteudo_metalico")
# NÃO entram no grupo da Seção 232: neles a 232 incide só sobre o metal
# contido, e o resto do valor continuou sujeito às tarifas gerais.


# --------------------------------------------------------------
# 3. GRUPOS: UM GRUPO POR SH6
# --------------------------------------------------------------
# A função recebe a regra e devolve a tabela SH6 -> grupo. Está em
# função para poder ser repetida com a outra regra (seção 6).
#
# O case_when testa as condições DE CIMA PARA BAIXO e para na
# primeira verdadeira. Essa é a ordem de prioridade:
#   1º Seção 232: quem está na 232 ficou fora das tarifas gerais;
#   2º aeronaves civis;
#   depois, as combinações de isenção dos 10% e dos 40%.
classifica <- function(regra) {
  s232     <- na_lista(l_232,  d_nov, regra)
  aero     <- na_lista(l_aero, d_ago, regra)
  i10_ago  <- na_lista(l_10,   d_ago, regra)   # isento dos 10% em ago/2025
  i40_ago  <- na_lista(l_40,   d_ago, regra)   # isento dos 40% em ago/2025
  i40_nov  <- na_lista(l_40,   d_nov, regra)   # isento dos 40% em nov/2025
  
  tibble(sh6 = unique(listas$sh6)) |>
    mutate(grupo = case_when(
      sh6 %in% s232                                     ~ "Seção 232",
      sh6 %in% aero & str_sub(sh6, 1, 2) %in% caps_aeronave ~ "Aeronaves civis",
      sh6 %in% i40_ago &  sh6 %in% i10_ago              ~ "Isento desde ago/2025",
      sh6 %in% i40_ago                                  ~ "Só os 10% (isento dos 40%)",
      sh6 %in% i40_nov                                  ~ "Isento a partir de nov/2025",
      sh6 %in% i10_ago                                  ~ "Só os 40% (isento dos 10%)",
      TRUE                                              ~ "Tarifa de 50%"
    ))
}

grupos_sh6 <- classifica(regra_cobertura)
# Produto que não aparece em nenhuma lista paga a tarifa cheia (10% + 40%);
# isso é resolvido na seção 4, quando o grupo fica vazio depois da junção.

message("Número de SH6 por grupo (só os que aparecem em alguma lista):")
print(count(grupos_sh6, grupo))


# --------------------------------------------------------------
# 4. EXPORTAÇÕES POR GRUPO E POR MÊS
# --------------------------------------------------------------
# 4.1 Lê a base por NCM e cria o SH6 (6 primeiros dígitos da NCM).
#     str_pad repõe o zero à esquerda, caso a NCM tenha sido lida como número.
exp_ncm <- read_rds(file.path(pasta_proc, "exp_eua_ncm.rds")) |>
  mutate(sh6  = str_sub(str_pad(as.character(NCM), 8, pad = "0"), 1, 6),
         data = make_date(ano, mes, 1))

# 4.2 Duas regiões: o RS e o Brasil (todos os estados, RS incluído).
exp_regiao <- bind_rows(
  exp_ncm |> filter(uf == "RS") |> mutate(regiao = "Rio Grande do Sul"),
  exp_ncm |>                       mutate(regiao = "Brasil")
) |>
  mutate(regiao = factor(regiao, levels = c("Rio Grande do Sul", "Brasil")))

# 4.3 Função: junta os grupos e soma por região, grupo e mês.
soma_por_grupo <- function(tab_grupos) {
  exp_regiao |>
    left_join(tab_grupos, by = "sh6") |>
    mutate(grupo = replace_na(grupo, "Tarifa de 50%")) |>   # fora de todas as listas
    group_by(regiao, grupo, data) |>
    summarise(valor = sum(valor_usd) / 1e6, .groups = "drop") |>   # US$ milhões
    # garante que todo grupo tenha todos os meses (mês sem exportação = 0)
    complete(regiao, grupo, data, fill = list(valor = 0))
}

mensal <- soma_por_grupo(grupos_sh6)


# --------------------------------------------------------------
# 5. PESOS: QUANTO CADA GRUPO PESAVA NA PAUTA DE 2024
# --------------------------------------------------------------
calcula_pesos <- function(tab_mensal) {
  tab_mensal |>
    filter(year(data) == ano_base) |>
    group_by(regiao, grupo) |>
    summarise(valor_2024 = sum(valor), .groups = "drop_last") |>
    mutate(peso = 100 * valor_2024 / sum(valor_2024)) |>    # % da pauta da região
    ungroup()
}

pesos <- calcula_pesos(mensal)

message("\nPeso de cada grupo nas exportações para os EUA em ", ano_base, " (%):")
pesos |>
  select(regiao, grupo, peso) |>
  pivot_wider(names_from = regiao, values_from = peso) |>
  mutate(grupo = factor(grupo, levels = names(grupos))) |>
  arrange(grupo) |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  print()


# --------------------------------------------------------------
# 6. ROBUSTEZ: OS PESOS MUDAM COM A OUTRA REGRA?
# --------------------------------------------------------------
# Repete as seções 3 a 5 com a regra "basta uma linha" (regra = 0)
# e põe os dois resultados lado a lado. Se as diferenças forem
# pequenas, a escolha da regra não afeta as conclusões.
pesos_alt <- classifica(0) |> soma_por_grupo() |> calcula_pesos()

message("\nRobustez: peso (%) com a regra de ", 100 * regra_cobertura,
        "% e com a regra de uma linha:")
full_join(select(pesos,     regiao, grupo, regra_principal = peso),
          select(pesos_alt, regiao, grupo, uma_linha       = peso),
          by = c("regiao", "grupo")) |>
  mutate(across(where(is.numeric), \(x) round(replace_na(x, 0), 1)),
         diferenca = uma_linha - regra_principal) |>
  arrange(regiao, match(grupo, names(grupos))) |>
  print(n = Inf)


# --------------------------------------------------------------
# 7. ÍNDICE E MÉDIA MÓVEL
# --------------------------------------------------------------
# 7.1 Índice: valor do mês ÷ média mensal de 2024 × 100.
#     100 = o grupo exportou o mesmo que a média de 2024;
#      60 = exportou 40% menos.
# 7.2 Média móvel de 3 meses: média do mês com os dois anteriores.
#     Suaviza o sobe-e-desce mensal (embarques grandes e irregulares).
#     lag(x) = valor do mês anterior; lag(x, 2) = de dois meses antes.
indice <- mensal |>
  group_by(regiao, grupo) |>
  arrange(data, .by_group = TRUE) |>
  mutate(indice = 100 * valor / mean(valor[year(data) == ano_base]),
         mm3    = (indice + lag(indice) + lag(indice, 2)) / 3) |>
  ungroup() |>
  # só os grupos com peso relevante em cada painel
  left_join(select(pesos, regiao, grupo, peso), by = c("regiao", "grupo")) |>
  filter(peso >= peso_minimo, data >= inicio_grafico) |>
  mutate(grupo = fct_drop(factor(grupo, levels = names(grupos))),
         x     = data + days(14))                 # ponto no meio do mês, como no Gráfico 1

fora <- pesos |> filter(peso < peso_minimo)
if (nrow(fora) > 0) {
  message("\nGrupos não desenhados (peso menor que ", peso_minimo, "%):")
  print(select(fora, regiao, grupo, peso))
}


# --------------------------------------------------------------
# 8. GRÁFICO
# --------------------------------------------------------------
g <- ggplot(indice, aes(x, mm3, colour = grupo)) +
  # Camada 1: linha de referência em 100 (o nível de 2024).
  geom_hline(yintercept = 100, colour = "grey60", linewidth = 0.3) +
  # Camada 2: linhas verticais nas datas das medidas.
  geom_vline(data = eventos, aes(xintercept = data),
             linetype = "dotted", linewidth = 0.4, colour = "black") +
  # Camada 3: rótulos das medidas, em pé, no topo.
  geom_text(data = eventos, aes(x = data, y = Inf, label = rotulo), inherit.aes = FALSE,
            angle = 90, hjust = 1.05, vjust = -0.5, size = 2.3, colour = "black") +
  # Camada 4: uma linha por grupo.
  geom_line(linewidth = 0.7) +
  # Um painel por região, um embaixo do outro; cada um com seu eixo y.
  facet_wrap(~ regiao, ncol = 1, scales = "free_y") +
  # A cor vem do nome do grupo; a legenda mostra só os grupos desenhados.
  scale_colour_manual(values = grupos, name = NULL) +
  scale_x_date(breaks = seq(inicio_grafico, max(indice$data) %m+% months(3), by = "3 months"),
               labels = function(d) str_c(meses_pt[month(d)], "-", format(d, "%y")),
               expand = expansion(add = 20)) +
  # Folga de 45% no topo para os rótulos não baterem nas linhas.
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0.05, 0.45))) +
  labs(x = NULL, y = str_c("Índice (média de ", ano_base, " = 100)")) +
  guides(colour = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_minimal(base_size = 9, base_family = "sans") +
  theme(legend.position  = "top",
        legend.key.width = unit(0.8, "cm"),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        strip.text       = element_text(face = "bold", hjust = 0),
        panel.grid.minor = element_blank())

g   # mostra na tela do RStudio


# --------------------------------------------------------------
# 9. SALVAR
# --------------------------------------------------------------
write_csv(grupos_sh6, file.path(pasta_proc, "grupos_sh6.csv"))
write_csv(pesos,      file.path(pasta_proc, "pesos_grupos.csv"))

ggsave(file.path(pasta_graf, "grafico_grupos.pdf"), g,
       width = 16, height = 17, units = "cm", device = cairo_pdf)
ggsave(file.path(pasta_graf, "grafico_grupos.png"), g,
       width = 16, height = 17, units = "cm", dpi = 300, bg = "white")

message("\nGráfico dos grupos salvo em: ", pasta_graf)