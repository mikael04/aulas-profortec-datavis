########################################################################################## #
#'  Função criada para gerar tabelas agregadas nascimentos Profortec
#'  Adaptada de função que gera tabelas da coorte 100m
#'   
#'  Autor: Mikael
#'  Data: 18/04/23
########################################################################################## #

# 0 - Scripts e bibliotecas ----
source("Aulas/fct_get_colnames_df.R")

# 1 - Funçao ----

func_criar_tabela <- function(tabelao, flag_uf_mun, debug){
  teste_interno <- F
  if(teste_interno){
    tabelao <- df_sinasc_
    flag_uf_mun <- 0
    debug <- T
  }
  tabelao <- tabelao |> 
    dplyr::select(-id) |> 
    dplyr::rename(ano_nasc = ano,
                  cod_mun = CODMUNNASC,
                  racacor_mae = RACACORMAE) |> 
    dplyr::relocate(c(ano_nasc, cod_mun, racacor_mae))
  
  if(flag_uf_mun == 1){
    if(debug){
      print("Criando tabela para UF")
    }
    # tabela <- func_criar_tabela_uf(tabelao, debug)
  }
  if(flag_uf_mun == 0){
    if(debug){
      print("Criando tabela para Municipios")
    }
    tabela <- func_criar_tabela_mun(tabelao, debug)
  }
  tabela
}

# ano, CODMUNNASC, RACACORMAE

## Usar localidade, ano de nascimento (dtnasc) e racacormae como filtros
func_criar_tabela_mun <- function(tabelao, debug){
  variaveis <- colnames(tabelao)[-c(1,2,3)]
  tabelas <- data.frame(nivel = integer(0), ano_nasc = character(0), cod_mun = character(0), racacor_mae = integer(0), n = numeric(0), variavel = character(0), stringsAsFactors = F)
  
  i <- 0
  system.time(for(variavel in variaveis){
    i <- i+1
    if(debug){
      print(paste0(round(100*i/length(variaveis)), "%"))
    }
    
    colnames_tabelao <- func_get_colnames_df_unorder(tabelao)
    tabela <- tabelao %>% dplyr::group_by_at(.vars = c(variavel, "cod_mun", "ano_nasc", "racacor_mae")) %>%  dplyr::tally() %>%  dplyr::collect()
    names(tabela)[1] <- "nivel"
    tabela$variavel <- variavel
    
    tabelas <-  plyr::rbind.fill(tabelas, tabela)
  } )
  tabelas
}
