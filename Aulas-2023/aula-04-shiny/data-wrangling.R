########################################################################################## #
#'  Script para manipulação de dados
#' 
#'  Autor: Mikael Marin Coletto
#'  Data: 
########################################################################################## #


## 0.1 - Bibliotecas e scripts fontes----
library(dplyr)
library(tidyr)

## 1 - Script/Função ----

## 1.0 - Lendo dados ----
df_sinasc <- data.table::fread(here::here("dados/dados.csv"))
## Adicionando a coluna de id
df_sinasc_ <- tibble::rowid_to_column(df_sinasc, "id")

# skimr::skim(df_sinasc)

## Usar localidade, ano de nascimento (dtnasc) e racacormae como filtros

## Exibir as variáveis
## escolaridade da mãe, consultas (verificar se é consultas prenatal), tprobson, qtdgestant

## 1.1 Filtros ----

### 1.1.1 - Raça/cor ----
## Ajustando Raça/cor (unificando categorias)
unique(df_sinasc_$RACACORMAE)

df_sinasc_ <- df_sinasc_ |> 
  # dplyr::select(RACACORMAE) |>
  dplyr::filter(RACACORMAE != "") |> 
  mutate(RACACORMAE = case_when(RACACORMAE == "1" ~ "Branca",
                                RACACORMAE == "2" ~ "Preta",
                                RACACORMAE == "3" ~ "Amarela",
                                RACACORMAE == "4" ~ "Parda",
                                RACACORMAE == "5" ~ "Indígena",
                              .default = RACACORMAE))

# skimr::skim(df_sinasc_)

unique(df_sinasc_$RACACORMAE)

### 1.1.2 Município ----

# df_sinasc__loc <- df_sinasc_ |> 
#   dplyr::select(CODMUNNASC, CODMUNRES) |> 
#   dplyr::mutate(equal = ifelse(CODMUNNASC == CODMUNRES, T, F)) |> 
#   dplyr::select(-CODMUNRES)

df_codmun <- read.csv(here::here("dados/codmun.csv")) |> 
  dplyr::select(-COD.UF, NOMEMUN = NOME) |> 
  dplyr::mutate(COD = as.integer(gsub("^(.{6}).*", "\\1", COD)))

df_sinasc_ <- dplyr::inner_join(df_sinasc_, df_codmun, by = join_by(CODMUNNASC == COD))

### 1.1.3 Ano de nascimento (data de nascimento) ----

df_dtnasc <- df_sinasc_ |> 
  dplyr::select(id, DTNASC)

## Separei os dois tipos pela presença do "-" e transformei cada parte em data
df_dtnasc_year_18_19 <- df_dtnasc |> 
  filter(stringr::str_detect(DTNASC, "-")) |> 
  mutate(data_nasc = lubridate::ymd(DTNASC))

df_dtnasc_year_20_22 <- df_dtnasc |> 
  filter(!stringr::str_detect(DTNASC, "-")) |> 
  mutate(data_nasc = lubridate::dmy(DTNASC))

## E então uni os dois bancos
df_dtnasc_year <- rbind(df_dtnasc_year_18_19, df_dtnasc_year_20_22)

rm(df_dtnasc_year_18_19, df_dtnasc_year_20_22)

df_dtnasc_year <- df_dtnasc_year |> 
  mutate(ano = lubridate::year(data_nasc)) |> 
  dplyr::select(id, ano, DTNASC = data_nasc)

df_sinasc_ <- dplyr::select(df_sinasc_, -DTNASC)

df_sinasc_ <- dplyr::inner_join(df_sinasc_, df_dtnasc_year, by = "id")


## 1.2. Variáveis ----
## escolaridade da mãe, consultas (consultas prenatal), tprobson, qtdgestant

df_sinasc_vars <- df_sinasc_ |> 
  dplyr::select(id, ESCMAE, ESCMAEAGR1, ESCMAE2010, CONSPRENAT, TPROBSON, QTDGESTANT)

# skimr::skim(df_sinasc_vars)
# min(df_sinasc_vars$ESCMAE2010, na.rm = T)

### 1.2.1 Escolaridade ----

(df_sinasc_vars$CONSPRENAT)

unique(df_sinasc_vars$ESCMAE)
unique(df_sinasc_vars$ESCMAE2010)

df_sinasc_vars |>
  dplyr::filter(ESCMAE2010 != "" & !is.na(ESCMAE2010)) |> 
  dplyr::count()

### 1.2.2 Consultas prenatal (consprenat) ----
df_sinasc_vars_cons <- df_sinasc_vars |> 
  dplyr::filter(CONSPRENAT <= 50) |> 
  dplyr::mutate(CONSPRENAT = case_when(CONSPRENAT < 4 ~ "0-3",
                                       CONSPRENAT < 6 ~ "4-6",
                                       CONSPRENAT < 11 ~ "7-10",
                                       CONSPRENAT < 16 ~ "11-15",
                                       CONSPRENAT < 21 ~ "16-20",
                                       CONSPRENAT < 31 ~ "21-30",
                                       CONSPRENAT < 41 ~ "31-40",
                                       CONSPRENAT < 51 ~ "41-50"))

skimr::skim(df_sinasc_vars_cons)

### 1.2.3 tprobson (Chance de parto vaginal/cesáreo) ----
df_sinasc_vars_tprobson <- df_sinasc_vars |> 
  dplyr::select(TPROBSON) |> 
  dplyr::filter(TPROBSON <= 10)

skimr::skim(df_sinasc_vars_tprobson)

### 1.2.4 qtdgest (partos anteriores) ----
df_sinasc_vars_qtdgest <- df_sinasc_vars |> 
  dplyr::select(QTDGESTANT) |> 
  dplyr::filter(QTDGESTANT <= 10)

df_sinasc_vars |>
  dplyr::filter(is.na(QTDGESTANT)) |> 
  dplyr::count()

skimr::skim(df_sinasc_vars_qtdgest)

### 1.2.X Alterando variáveis na base ----
df_sinasc_ <- df_sinasc_ |> 
  dplyr::filter(ESCMAE2010 != "" & !is.na(ESCMAE2010) &
                  CONSPRENAT <= 50 &
                  TPROBSON <= 10 &
                  QTDGESTANT <= 10)|> 
  dplyr::mutate(CONSPRENAT = case_when(CONSPRENAT < 4 ~ "1-3",
                                       CONSPRENAT < 6 ~ "4-6",
                                       CONSPRENAT < 11 ~ "7-10",
                                       CONSPRENAT < 16 ~ "11-15",
                                       CONSPRENAT < 21 ~ "16-20",
                                       CONSPRENAT < 31 ~ "21-30",
                                       CONSPRENAT < 41 ~ "31-40",
                                       CONSPRENAT < 51 ~ "41-50"))

## Escrevendo tabelas para shiny de demonstração

data.table::fwrite(df_sinasc_, here::here("dados/dados_manipulados.csv"))

data.table::fwrite(df_sinasc_ |> dplyr::sample_n(size = 10000), here::here("dados/dados_manipulados_sample.csv"))

## 2. Gerando tabelas agregadas ----
source("Aulas/fct_criar_tabela.R")
tabela_agregada <- func_criar_tabela(df_sinasc_, flag_uf_mun = 0, debug = T)

