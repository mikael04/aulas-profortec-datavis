########################################################################################## #
#'  Script/Função/módulo criado para pegar nome das colunas e organizar em um df
#' 
#'  Autor: Mikael Marin Coletto
#'  Data: 
########################################################################################## #


## 0.1 - Bibliotecas e scripts fontes----
library(dplyr)

## 1.0 - Script/Função ----

func_get_colnames_df <- function(df){
  col_names <- colnames(df)
  col_names %>%
    as.data.frame() %>%
    arrange(col_names)
}

func_get_colnames_df_unorder <- function(df){
  col_names <- colnames(df)
  col_names %>%
    as.data.frame()
}

func_get_colnames_tabela <- function(tabela){
  # tabela <- tabela_CADU_pt1
  unique(tabela$variavel)
}
