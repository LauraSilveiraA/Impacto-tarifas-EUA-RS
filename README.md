# Impacto das tarifas dos EUA nas exportações do Rio Grande do Sul

Scripts em R do trabalho de conclusão de curso **"Os efeitos das tarifas comerciais dos Estados Unidos sobre as exportações do Rio Grande do Sul"**, apresentado ao Curso de Ciências Econômicas da Universidade Federal do Rio Grande do Sul (UFRGS) em 2026.

Autora: Laura Silveira Alves
Orientador: prof. Flávio Tosi Feijó

## O que o trabalho faz

Acompanha as exportações do RS para os EUA de abril de 2025 a setembro de 2026 e compara com a tendência anterior às tarifas. Depois classifica cada produto (SH6) pelo tratamento que recebeu nas ordens executivas americanas, para ver se a queda veio dos produtos tarifados. A análise é descritiva.

## Scripts

Rode na ordem. Cada script lê o que os anteriores salvaram.

| Script | O que faz | Saída principal |
|---|---|---|
| `01_dados.R` | Lê os arquivos do Comex Stat e monta as bases comuns | `exp_eua_ncm.rds`, `exp_rs_eua.rds`, `exp_br_eua.rds` |
| `02_tabela_1.R` | Estima a tendência pré-tarifa e calcula o desvio por período | Tabela 1 |
| `03_grafico_1.R` | Exportações do RS para os EUA e a tendência pré-tarifa | Gráfico 1 |
| `04_listas_tarifas.R` | Monta as listas de produtos de cada regime tarifário (IEEPA e Seção 232) | `listas_hs10.csv`, `listas_sh6.csv` |
| `05_grafico_grupos.R` | Classifica os produtos por grupo tarifário; gráfico em índice | `grupos_sh6.csv`, gráfico dos grupos |
| `06_grafico_grupos_valor.R` | Exportações por grupo tarifário, em US$ | Gráfico de barras |
| `07_tabela_3.R` | Peso e variação de cada grupo tarifário, RS e Brasil | Tabela 3 |
| `08_robustez.R` | Sensibilidade à janela e à forma da tendência; placebos | Tabela 2 |
| `09_tabela_4.R` | Os 15 principais produtos do RS para os EUA | Tabela 4 |

As tabelas são salvas em `.csv` e em `.tex`, prontas para o LaTeX. Os gráficos são salvos em `.pdf` e `.png`.

## Dados

- **Comex Stat** (MDIC): arquivos anuais de exportação por NCM, `EXP_2016.csv` a `EXP_2026.csv`, da base de dados bruta em <https://comexstat.mdic.gov.br>. A UF usada é a produtora da mercadoria (`SG_UF_NCM`).
- **Ordens executivas** 14257, 14323 e 14361, em PDF, publicadas no Federal Register. O script 04 lê as listas de produtos diretamente desses PDFs.
- **Tariff Rate Tracker**, do Budget Lab at Yale (<https://github.com/Budget-Lab-Yale/tariff-rate-tracker>): listas da Seção 232 e alterações do Anexo II. O script 04 baixa os arquivos na versão `a5ddf68`, de 09/11/2026, para que os resultados sejam sempre os mesmos.

Dados Comex Stat não estão neste repositório. Baixe-os nas fontes acima.


PDFs lidos pelo script `04_listas_tarifas.R`, publicados no Federal Register.

| Arquivo | Ato | O que o script usa | Fonte oficial |
|---|---|---|---|
| [2025-04-02_EO14257_tarifa_reciproca.pdf](2025-04-02_EO14257_tarifa_reciproca.pdf) | EO 14257 | Anexo II: produtos isentos da tarifa de 10% | [Federal Register](https://www.govinfo.gov/content/pkg/FR-2025-04-07/html/2025-06063.htm) |
| [2025-07-30_EO14323_adicional_brasil.pdf](2025-07-30_EO14323_adicional_brasil.pdf) | EO 14323 | Anexo I: produtos isentos do adicional de 40% sobre o Brasil | [Federal Register](https://www.govinfo.gov/content/pkg/FR-2025-08-05/pdf/2025-14896.pdf) |
| [2025-11-20_EO14361_isencoes_brasil.pdf](2025-11-20_EO14361_isencoes_brasil.pdf) | EO 14361 | Anexo I consolidado, com as isenções de novembro de 2025 | [Casa Branca](https://www.whitehouse.gov/presidential-actions/2025/11/modifying-the-scope-of-tariffs-on-the-government-of-brazil/) |

## Como rodar

1. Instale os pacotes: `install.packages(c("tidyverse", "scales", "pdftools", "yaml"))`.
2. Os scripts usam a pasta `C:/TCC_2`. Para usar outra, troque o caminho na seção 1 de cada script.
3. Estrutura de pastas esperada:
   - `C:/TCC_2/Comex_data/estadual/` com os arquivos `EXP_<ano>.csv`
   - `C:/TCC_2/Bib/Decretos/` com os PDFs das ordens executivas
   - `Dados_processados/` e `Graficos/` são criadas pelos scripts
4. Rode de `01` a `09`, nessa ordem.
