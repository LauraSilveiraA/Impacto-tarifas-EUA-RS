# ==============================================================
# 04_listas_tarifas.R
# Monta as listas de produtos de cada regime tarifário dos EUA:
# quais códigos estavam isentos das tarifas da IEEPA e quais
# estavam sujeitos à Seção 232, e a partir de quando.
#
# Fontes:
#   A) PDFs das ordens executivas (EO 14257, 14323 e 14361),
#      lidos diretamente por este script.
#   B) Tariff Rate Tracker, do Budget Lab de Yale (repositório
#      público no GitHub), de onde vêm:
#        - as listas da Seção 232 (aço, alumínio, cobre, veículos,
#          autopeças, madeira e móveis, caminhões e ônibus);
#        - as alterações do Anexo II da EO 14257 depois de abril;
#        - a tabela tarifária americana completa (HTS10), usada para
#          medir quanto de cada SH6 é coberto por cada lista.
#
# Saídas (em Dados_processados):
#   listas_tarifas.csv -> uma linha por código de cada lista
#   listas_sh6.csv     -> o mesmo, resumido por SH6 (a chave que
#                         liga as listas americanas ao Comex Stat)
#
# Passo a passo:
#   1. Parâmetros
#   2. Funções auxiliares
#   3. Listas da IEEPA, lidas dos PDFs
#   4. Arquivos do Budget Lab (baixa uma vez e guarda)
#   5. Seção 232
#   6. Alterações do Anexo II (isentos dos 10%)
#   7. Tabela única com todas as listas
#   8. Resumo por SH6, com a cobertura de cada lista
#   9. Salvar
#
# Pacotes: install.packages(c("pdftools", "yaml")) na primeira vez.
# ==============================================================

library(tidyverse)
library(pdftools)   # lê o texto dos PDFs
library(yaml)       # lê o arquivo de parâmetros do Budget Lab


# --------------------------------------------------------------
# 1. PARÂMETROS
# --------------------------------------------------------------
dir_pdf    <- "C:/TCC_2/Bib/Decretos"            # PDFs das ordens executivas
pasta_bl   <- "C:/TCC_2/Bib/Budget_Lab"          # onde guardar os arquivos do Budget Lab
pasta_proc <- "C:/TCC_2/Dados_processados"
dir.create(pasta_bl,   showWarnings = FALSE, recursive = TRUE)
dir.create(pasta_proc, showWarnings = FALSE, recursive = TRUE)

# Versão exata do repositório do Budget Lab (de 09/11/2026). Fixar a
# versão garante que o script dê sempre o mesmo resultado, mesmo que
# o repositório seja atualizado depois. Cite esta versão no TCC.
versao_bl <- "a5ddf68dfa691c2c80fe5c4adc94e5fcae7dcb79"
url_bl    <- str_c("https://raw.githubusercontent.com/Budget-Lab-Yale/tariff-rate-tracker/", versao_bl, "/")

# Datas de vigência (conferidas no Federal Register; ver Quadro 1).
d_reciproca   <- as.Date("2025-04-05")  # EO 14257: tarifa recíproca de 10%
d_brasil_40   <- as.Date("2025-08-06")  # EO 14323: adicional de 40%
d_isencao_nov <- as.Date("2025-11-13")  # EO 14361: novas isenções dos 40%
d_fim_ieepa   <- as.Date("2026-02-23")  # último dia das tarifas da IEEPA (EO 14389)

# Seção 232: data em que cada setor passou a ser tarifado.
datas_232 <- c(
  automoveis        = "2025-04-03",   # Procl. 10908
  autopecas         = "2025-05-03",   # Procl. 10908
  cobre             = "2025-08-01",   # Procl. 10962
  madeira           = "2025-10-14",   # Procl. 10976
  moveis_de_madeira = "2025-10-14",   # Procl. 10976
  caminhoes         = "2025-11-01",   # Procl. 10984
  pecas_de_caminhao = "2025-11-01",   # Procl. 10984
  onibus            = "2025-11-01"    # Procl. 10984
)
d_antes_2025 <- as.Date("2018-03-23")  # aço e alumínio já tarifados desde 2018 (25% para o Brasil só em 12/03/2025)


# --------------------------------------------------------------
# 2. FUNÇÕES AUXILIARES
# --------------------------------------------------------------

# Lê um PDF e devolve o texto, uma linha por elemento.
ler_linhas <- function(arq) {
  pdf_text(file.path(dir_pdf, arq)) |>
    str_split("\n") |> unlist() |>
    # corrige um erro de leitura visto no PDF de agosto:
    # "9401.l 0.40" no lugar de "9401.10.40"
    str_replace_all("(\\d{4})\\.l\\s?(\\d)\\.(\\d{2})", "\\1.1\\2.\\3")
}

# Padrão de um código HTS de 8 dígitos escrito com pontos: 1234.56.78
re_hts <- "(?<![\\d.])\\d{4}\\.\\d{2}\\.\\d{2}(?![\\d.])"

# Extrai todos os códigos HTS de um texto (sem repetir, em ordem).
extrai_hts <- function(x) {
  x |> str_extract_all(re_hts) |> unlist() |>
    str_subset("^9903", negate = TRUE) |>   # descarta as rubricas do capítulo 99
    unique() |> sort()
}

# Tira os pontos: "1234.56.78" -> "12345678"
sem_pontos <- function(x) str_remove_all(x, "\\.")

# Acha a primeira linha que contém `chave` e fica depois da linha `depois`.
# exato = TRUE: a linha inteira é o título (ex.: "ANNEX I"), tolerando
# espaços a mais e maiúsculas/minúsculas.
acha <- function(linhas, chave, depois = 0L, exato = FALSE) {
  padrao <- str_c("^[\\s\\u00a0]*",
                  str_replace_all(chave, "\\s+", "[\\\\s\\\\u00a0]*"),
                  "[\\s\\u00a0]*$")
  hit <- if (exato) str_detect(linhas, regex(padrao, ignore_case = TRUE))
  else str_detect(linhas, fixed(chave))
  i <- which(hit & seq_along(linhas) > depois)[1]
  if (is.na(i)) stop("marcador não encontrado: ", chave)
  i
}

# Compara dois conjuntos de códigos e mostra as diferenças.
confere <- function(a, b, rotulo) {
  so_a <- setdiff(a, b); so_b <- setdiff(b, a)
  cat(rotulo, "| só no 1º:", length(so_a), "| só no 2º:", length(so_b), "\n")
  if (length(so_a) + length(so_b) > 0) cat("   ->", head(c(so_a, so_b), 10), "\n")
  invisible(list(so_a = so_a, so_b = so_b))
}

# Baixa um arquivo do Budget Lab, se ainda não estiver na pasta.
baixa_bl <- function(caminho) {
  destino <- file.path(pasta_bl, basename(caminho))
  if (!file.exists(destino)) {
    message("Baixando: ", caminho)
    download.file(str_c(url_bl, caminho), destino, mode = "wb", quiet = TRUE)
  }
  destino
}


# --------------------------------------------------------------
# 3. LISTAS DA IEEPA, LIDAS DOS PDFs
# --------------------------------------------------------------
# Cada ordem traz a mesma lista duas vezes: em um anexo (tabela) e no
# texto que altera a tabela tarifária (notas do capítulo 99). O script
# lê as duas e compara: se não baterem, há erro de leitura.

# ---- 3.1 EO 14257 (abr/2025): Anexo II, isentos da tarifa de 10% ----
linhas <- ler_linhas("eo-14257_10_1.pdf")

ini <- acha(linhas, "ANNEX II",  exato = TRUE)
fim <- acha(linhas, "ANNEX III", depois = ini, exato = TRUE)
m   <- str_match(linhas[ini:fim], "^\\s*(\\d{8})\\s+")   # linhas que começam com 8 dígitos
anexo2_abr <- unique(m[!is.na(m[, 2]), 2])

i_nota <- acha(linhas, "(iii) As provided in heading 9903.01.32")
i_fim  <- which(str_detect(linhas, "^\\s*\\(iv\\)") & seq_along(linhas) > i_nota)[1]
nota   <- sem_pontos(extrai_hts(linhas[i_nota:(i_fim - 1)]))

cat("EO 14257 | Anexo II:", length(anexo2_abr), "| nota (iii):", length(nota), "\n")
confere(anexo2_abr, nota, "EO 14257: Anexo II x nota (iii)")

# ---- 3.2 EO 14323 (ago/2025): isentos do adicional de 40% ----
linhas <- ler_linhas("eo-14323_40_1.pdf")

i3 <- acha(linhas, "(iii) As provided in heading 9903.01.81")
i4 <- acha(linhas, "(iv) As provided in heading 9903.01.82", depois = i3)
i5 <- acha(linhas, "(v) The additional duties imposed by heading 9903.01.77", depois = i4)

ago_isento   <- extrai_hts(linhas[i3:(i4 - 1)])   # isentos sem condição (9903.01.81)
ago_aeronave <- extrai_hts(linhas[i4:(i5 - 1)])   # isentos só para aeronave civil (9903.01.82)
stopifnot(length(ago_isento) == 129, length(ago_aeronave) == 565,
          length(intersect(ago_isento, ago_aeronave)) == 0)

ini    <- acha(linhas, "ANNEX I",  exato = TRUE)
fim    <- acha(linhas, "ANNEX II", depois = ini, exato = TRUE)
anexo1 <- extrai_hts(str_extract(linhas[ini:fim], "^\\s*\\d{4}\\.\\d{2}\\.\\d{2}"))
confere(anexo1, c(ago_isento, ago_aeronave), "EO 14323: Anexo I x notas (iii)+(iv)")

# ---- 3.3 EO 14361 (nov/2025): lista consolidada, que SUBSTITUI a de agosto ----
linhas <- ler_linhas("eo-14361_40_2.pdf")

i_a2   <- acha(linhas, "ANNEX II", exato = TRUE)
anexo1 <- linhas[seq_len(i_a2 - 1)]
anexo2 <- linhas[i_a2:length(linhas)]

# No Anexo I, cada linha traz o código e, no fim, uma marca:
#   (nada)        -> já estava na lista de agosto
#   Aircraft      -> isento só para aeronave civil
#   Addition      -> acrescentado em novembro
#   Addition (Ex) -> acrescentado em novembro, só para parte do código
m   <- str_match(anexo1, "^\\s*(\\d{4}\\.\\d{2}\\.\\d{2})(?=\\s)(.*)$")
nov <- tibble(hts8 = m[, 2], resto = m[, 3]) |> filter(!is.na(hts8))

dup <- nov |> count(hts8) |> filter(n > 1)   # avisa se algum código se repete
if (nrow(dup) > 0) { cat("Códigos repetidos no Anexo I:\n"); print(dup) }

nov <- nov |>
  mutate(marca = str_extract(str_trim(resto, "right"), "(Addition \\(Ex\\)|Addition|Aircraft|Ex)$"),
         marca = replace_na(marca, "anterior")) |>
  distinct(hts8, .keep_all = TRUE)

print(count(nov, marca))
stopifnot(nrow(nov) == 943,
          all(nov$marca %in% c("anterior", "Aircraft", "Addition", "Addition (Ex)")))

# Confere o Anexo I com o texto do Anexo II (as adições de novembro).
txt2 <- paste(anexo2, collapse = "\n")
i <- str_locate(txt2, fixed("Subdivision (x)(iii) of U.S. note 2"))[1, "start"]
j <- str_locate(txt2, fixed("ii. by inserting the following new subdivision (x)(iii)(b)"))[1, "start"]
stopifnot(setequal(extrai_hts(str_sub(txt2, i, j - 1)), nov$hts8[nov$marca == "Addition"]))        # 238
parciais <- str_match_all(str_sub(txt2, j), "subheading\\s+(\\d{4}\\.\\d{2}\\.\\d{2})\\)")[[1]][, 2]
stopifnot(setequal(parciais, nov$hts8[nov$marca == "Addition (Ex)"]))                              # 11

# A lista de novembro tem de conter a de agosto inteira (129 + 565).
confere(nov$hts8[nov$marca == "anterior"], ago_isento,   "Nov (anterior) x Ago (isento)")
confere(nov$hts8[nov$marca == "Aircraft"], ago_aeronave, "Nov (aeronave) x Ago (aeronave)")

# ---- 3.4 Junta as três listas em um formato único ----
# medida    -> de qual tarifa o código está isento
# categoria -> tipo de isenção
# inicio/fim -> período em que a lista valeu (fim = NA: até o fim da tarifa)
listas_pdf <- bind_rows(
  tibble(medida = "reciproca_10", categoria = "isento", codigo = anexo2_abr,
         inicio = d_reciproca, fim = as.Date(NA), fonte = "EO 14257, Anexo II (PDF)"),
  tibble(medida = "brasil_40", categoria = "isento", codigo = sem_pontos(ago_isento),
         inicio = d_brasil_40, fim = d_isencao_nov - 1, fonte = "EO 14323 (PDF)"),
  tibble(medida = "brasil_40", categoria = "aeronave_civil", codigo = sem_pontos(ago_aeronave),
         inicio = d_brasil_40, fim = d_isencao_nov - 1, fonte = "EO 14323 (PDF)"),
  nov |> transmute(
    medida = "brasil_40",
    categoria = case_match(marca,
                           "anterior"      ~ "isento",
                           "Addition"      ~ "isento",
                           "Aircraft"      ~ "aeronave_civil",
                           "Addition (Ex)" ~ "isento_parcial"),
    codigo = sem_pontos(hts8),
    inicio = d_isencao_nov, fim = d_fim_ieepa, fonte = "EO 14361 (PDF)")
)


# --------------------------------------------------------------
# 4. ARQUIVOS DO BUDGET LAB
# --------------------------------------------------------------
# Baixa só os arquivos necessários (na primeira vez) e guarda em pasta_bl.
arq_param   <- baixa_bl("config/policy_params.yaml")                # definições por posição (veículos, madeira...)
arq_metal   <- baixa_bl("resources/s232_metal_chapter_products.csv") # aço e alumínio: produtos primários e derivados antigos
arq_deriv   <- baixa_bl("resources/s232_derivative_products.csv")   # derivados de aço e alumínio
arq_cobre   <- baixa_bl("resources/s232_copper_products.csv")
arq_pecas   <- baixa_bl("resources/s232_auto_parts.txt")
arq_pecas_c <- baixa_bl("resources/s232_mhd_parts.txt")
arq_anexo2  <- baixa_bl("resources/annex_ii_first_appearance.csv")  # Anexo II da EO 14257, com datas
arq_brasil  <- baixa_bl("resources/country_eo_exempt_products.csv") # lista do Brasil (só para conferência)
arq_universo <- baixa_bl("resources/hs10_gtap_crosswalk.csv")      # todas as linhas da tabela tarifária (HTS10)


# --------------------------------------------------------------
# 5. SEÇÃO 232
# --------------------------------------------------------------
# Resultado: uma linha por código, com o setor, a data de início e a
# BASE da tarifa:
#   valor_total       -> a tarifa incide sobre o produto inteiro
#   conteudo_metalico -> só sobre o aço/alumínio/cobre contido no produto;
#                        o restante do valor continuava pagando a IEEPA.
# Essa diferença importa na classificação: um produto de base
# "conteudo_metalico" (uma máquina, por exemplo) não saiu da IEEPA.

le_prefixos <- function(arq) {                    # arquivos .txt: um código por linha
  read_lines(arq) |> str_trim() |> str_subset("^\\d+$")
}

# ---- 5.1 Aço e alumínio ----
metal <- read_csv(arq_metal, col_types = cols(.default = "c")) |>
  transmute(codigo = hts_prefix, inicio = effective_date)
deriv <- read_csv(arq_deriv, col_types = cols(.default = "c")) |>
  transmute(codigo = hts_prefix, inicio = effective_date)

s232_metal <- bind_rows(metal, deriv) |>
  mutate(inicio = as.Date(na_if(inicio, "NA")),
         inicio = coalesce(inicio, d_antes_2025)) |>   # sem data = já tarifado antes de 2025
  group_by(codigo) |>
  summarise(inicio = min(inicio), .groups = "drop") |> # um código pode aparecer como aço e como alumínio
  mutate(categoria = "aco_aluminio",
         # capítulos 72, 73 e 76 são os próprios metais e seus artigos: valor total.
         # Fora deles, são derivados: tarifa só sobre o conteúdo metálico.
         base = if_else(str_sub(codigo, 1, 2) %in% c("72", "73", "76"),
                        "valor_total", "conteudo_metalico"))

# ---- 5.2 Cobre ----
s232_cobre <- read_csv(arq_cobre, col_types = cols(.default = "c")) |>
  transmute(codigo = hts10, categoria = "cobre", base = "conteudo_metalico",
            inicio = as.Date(datas_232[["cobre"]])) |>
  distinct()

# ---- 5.3 Veículos, autopeças, madeira, móveis, caminhões e ônibus ----
# As posições estão no arquivo de parâmetros do Budget Lab.
p232 <- read_yaml(arq_param)$section_232_headings

s232_setores <- bind_rows(
  tibble(categoria = "automoveis",        codigo = c(p232$autos_passenger$prefixes, p232$autos_light_trucks$prefixes)),
  tibble(categoria = "autopecas",         codigo = le_prefixos(arq_pecas)),
  tibble(categoria = "madeira",           codigo = p232$softwood$prefixes),
  tibble(categoria = "moveis_de_madeira", codigo = c(p232$wood_furniture$prefixes, p232$kitchen_cabinets$prefixes)),
  tibble(categoria = "caminhoes",         codigo = p232$mhd_vehicles$prefixes),
  tibble(categoria = "pecas_de_caminhao", codigo = le_prefixos(arq_pecas_c)),
  tibble(categoria = "onibus",            codigo = p232$buses$prefixes)
) |>
  mutate(base = "valor_total", inicio = as.Date(datas_232[categoria]))

# ---- 5.4 Junta ----
listas_232 <- bind_rows(s232_metal, s232_cobre, s232_setores) |>
  mutate(medida = "s232", fim = as.Date(NA), fonte = "Budget Lab (Tariff Rate Tracker)")

cat("\nSeção 232: códigos por setor e base da tarifa\n")
print(count(listas_232, categoria, base, inicio), n = Inf)
# Observação: a reestruturação de abril de 2026 (Procl. 11021) não está aqui.
# Ela é posterior ao fim das tarifas da IEEPA, período que a classificação cobre.


# --------------------------------------------------------------
# 6. ALTERAÇÕES DO ANEXO II (isentos dos 10%)
# --------------------------------------------------------------
# O PDF de abril dá a lista original. Depois dela, o Anexo II mudou:
# entraram códigos (memorando de 11/04, EO 14346 e EO 14360) e saíram
# outros (os que passaram para a Seção 232). O Budget Lab registra a
# data de entrada e de saída de cada código.
anexo2_bl <- read_csv(arq_anexo2, col_types = cols(.default = "c")) |>
  transmute(codigo = prefix,
            entrada = as.Date(na_if(first_effective_date, "NA")),   # NA = estava na lista original
            saida   = as.Date(na_if(end_effective_date, "NA")))     # NA = nunca saiu

# 6.1 Conferência: a lista original do Budget Lab bate com a do PDF?
confere(anexo2_abr, anexo2_bl$codigo[is.na(anexo2_bl$entrada)],
        "Anexo II original: PDF x Budget Lab")

# 6.2 Lista original (do PDF), agora com a data de saída de quem saiu.
original <- listas_pdf |>
  filter(medida == "reciproca_10") |>
  select(-fim) |>
  left_join(select(anexo2_bl, codigo, fim = saida), by = "codigo")

# 6.3 Códigos acrescentados depois (do Budget Lab).
acrescimos <- anexo2_bl |>
  filter(!is.na(entrada), !codigo %in% anexo2_abr) |>
  transmute(medida = "reciproca_10", categoria = "isento", codigo,
            inicio = entrada, fim = saida,
            fonte = "Anexo II atualizado (Budget Lab)")

cat("\nAnexo II: acréscimos por data de entrada\n"); print(count(acrescimos, inicio))
cat("Anexo II: saídas por data\n");                print(count(filter(original, !is.na(fim)), fim))

listas_ieepa <- bind_rows(original, acrescimos, filter(listas_pdf, medida == "brasil_40")) |>
  mutate(fim = coalesce(fim, d_fim_ieepa))    # quem não saiu antes vale até o fim da IEEPA

# 6.4 Conferência extra: a lista do Brasil do Budget Lab x a dos PDFs.
#     Diferenças pequenas são esperadas (eles leem outra fonte, a tabela HTS).
brasil_bl <- read_csv(arq_brasil, comment = "#", col_types = cols(.default = "c")) |>
  mutate(codigo = sem_pontos(hts10))
confere(listas_pdf$codigo[listas_pdf$fonte == "EO 14323 (PDF)"],
        brasil_bl$codigo[brasil_bl$effective_date_start == "2025-08-06"],
        "Lista do Brasil de agosto: PDF x Budget Lab")


# --------------------------------------------------------------
# 7. TABELA ÚNICA
# --------------------------------------------------------------
listas_tarifas <- bind_rows(listas_ieepa, listas_232) |>
  select(medida, categoria, base, codigo, inicio, fim, fonte) |>
  arrange(medida, categoria, inicio, codigo)

cat("\nResumo das listas:\n")
print(count(listas_tarifas, medida, categoria, inicio, fim), n = Inf)


# --------------------------------------------------------------
# 8. RESUMO POR SH6, COM A COBERTURA DE CADA LISTA
# --------------------------------------------------------------
# As listas são definidas em códigos americanos (HTS, 4 a 10 dígitos);
# o Comex Stat usa a NCM. Os dois só coincidem nos 6 primeiros dígitos
# (SH6). Por isso a ligação é feita por SH6.
#
# Problema: às vezes só PARTE de um SH6 está na lista. Para medir essa
# parte, usamos a tabela tarifária americana completa (18.700 linhas
# de 10 dígitos):
#   cobertura = nº de linhas HTS10 do SH6 que estão na lista
#               ÷ nº total de linhas HTS10 do SH6
# cobertura = 1   -> o SH6 inteiro está na lista
# cobertura = 0,1 -> só 10% das linhas estão (classificar o SH6 inteiro
#                    seria exagero)
# É uma contagem de linhas, não de valor: uma linha pode pesar mais que
# outra no comércio. Mesmo assim, separa bem os SH6 inteiros dos parciais.

universo <- read_csv(arq_universo, col_types = cols(.default = "c")) |>
  transmute(hs10, sh6 = str_sub(hs10, 1, 6)) |>
  distinct()
linhas_sh6 <- count(universo, sh6, name = "n_linhas_sh6")   # total de linhas de cada SH6

# 8.1 Liga cada código das listas às linhas HTS10 que começam com ele.
#     (um código de 4 dígitos, como 8702, cobre todas as linhas "8702......")
liga <- map_dfr(sort(unique(nchar(listas_tarifas$codigo))), function(n) {
  listas_tarifas |>
    filter(nchar(codigo) == n) |>
    inner_join(mutate(universo, codigo = str_sub(hs10, 1, n)), by = "codigo",
               relationship = "many-to-many")
})

# 8.2 Conferência: códigos das listas que não existem na tabela usada.
#     (a tabela tarifária muda ao longo do ano; poucos casos são normais)
fora <- anti_join(distinct(listas_tarifas, medida, codigo), liga, by = c("medida", "codigo"))
cat("\nCódigos das listas não encontrados na tabela HTS:\n"); print(count(fora, medida))

# 8.3 Conta, por lista e SH6, as linhas cobertas (cada linha conta uma vez).
listas_sh6 <- liga |>
  distinct(medida, categoria, base, inicio, fim, sh6, hs10) |>
  count(medida, categoria, base, inicio, fim, sh6, name = "n_linhas_lista") |>
  left_join(linhas_sh6, by = "sh6") |>
  mutate(cobertura = n_linhas_lista / n_linhas_sh6) |>
  arrange(medida, categoria, inicio, sh6)

# 8.3b Linhas HTS10 de cada lista, uma a uma.
#      O listas_sh6 conta as linhas POR CATEGORIA. Quando a mesma linha
#      está em duas categorias (por exemplo, autopeças e peças de
#      caminhão), somar as categorias conta a linha duas vezes e exagera
#      a cobertura. Guardando as linhas uma a uma, o script 05 pode
#      contar cada linha uma única vez.
listas_hs10 <- liga |>
  distinct(medida, categoria, base, inicio, fim, sh6, hs10) |>
  left_join(linhas_sh6, by = "sh6") |>
  arrange(medida, categoria, inicio, hs10)



# 8.4 Quantos SH6 são cobertos só em parte?
cat("\nSH6 por faixa de cobertura:\n")
listas_sh6 |>
  mutate(faixa = case_when(cobertura >= 0.99 ~ "1. inteiro",
                           cobertura >= 0.50 ~ "2. maior parte (50% a 99%)",
                           TRUE              ~ "3. menor parte (menos de 50%)")) |>
  count(medida, categoria, faixa) |>
  pivot_wider(names_from = faixa, values_from = n, values_fill = 0, names_sort = TRUE) |>
  print(n = Inf)




# --------------------------------------------------------------
# 9. SALVAR
# --------------------------------------------------------------
write_csv(listas_tarifas, file.path(pasta_proc, "listas_tarifas.csv"))
write_csv(listas_hs10,    file.path(pasta_proc, "listas_hs10.csv"))
write_csv(listas_sh6,     file.path(pasta_proc, "listas_sh6.csv"))
message("Listas salvas em: ", pasta_proc)
