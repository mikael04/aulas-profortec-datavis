## Shiny
library(shiny)
library(gridlayout)
library(bslib)
library(shinyWidgets)
## Gráficos
library(ggplot2)
library(plotly)
## Manipulação de dados
library(dplyr)
library(stringr)
# library()
## Bibliotecas para mapas
library(sf)
library(geobr)
options(timeout= 4000000)

## Dados
df_sinasc <- data.table::fread(here::here("dados/dados_manipulados_sample.csv"))
## Variáveis selecionadas
variaveis <- c("CONSPRENAT", "ESCMAE2010", "QTDGESTANT", "TPROBSON")
# list(str_sort(unique(df_sinasc$ano)))
ui <- navbarPage(
    title = "Exemplo aula 04",
    selected = "Gráficos",
    collapsible = TRUE,
    theme = bslib::bs_theme(),
    tabPanel(
        title = "Gráficos",
        grid_container(
            layout = c(
            "seletores graphs"
            ),
            row_sizes = c(
                "1fr"
        ),
            col_sizes = c(
                "250px",
                "1fr"
            ),
            gap_size = "10px",
            grid_card(
                area = "seletores",
                card_header("Filtros"),
                card_body_fill(
                    sliderTextInput(
                        inputId = "IDAnoNasc",
                        label = "Selecione o período:", 
                        choices = c("2018", "2019", "2020", "2021", "2022"),
                        selected = c("2018", "2022")
                    ),
                    selectInput(
                        inputId = "IDVarSel",
                        label = "Selecione a variável",
                        choices = as.list(variaveis),
                        selected = variaveis[1]
                    ),
                    selectInput(
                        inputId = "IDCategorias",
                        label = "Selecione a categoria da variável para visualizar no mapa",
                        choices = list("TODAS"),
                        selected = "TODAS",
                    ),
                    actionButton(inputId = "IDGenerateGraphs", label = "Gerar gráficos")
                )
            ),
            grid_card(
                area = "graphs",
                card_body_fill(
                    grid_container(
                        layout = c(
                            "card1 card1",
                            "card2 card3"
                        ),
                        row_sizes = c(
                            "1fr",
                            "1fr"
                        ),
                        col_sizes = c(
                            "1fr",
                            "1fr"
                        ),
                        gap_size = "10px",
                        grid_card(
                            area = "card1",
                            card_body_fill(plotlyOutput("graf_ano"))
                        ),
                        grid_card(
                            area = "card2",
                            card_body_fill(plotly::plotlyOutput("mapa"))
                        ),
                        grid_card(
                            area = "card3",
                            card_body_fill(plotly::plotlyOutput("graf_racacor"))
                        )
                    )
                )
            )
        )
    )
)


server <- function(input, output, session) {
    observeEvent(input$IDGenerateGraphs, {
        ano_sel <- input$IDAnoNasc
        var_sel <- input$IDVarSel
        start <- F
        df_filter <- df_sinasc |> 
            dplyr::filter(ano >= ano_sel[1] & ano <= ano_sel[2]) |> 
            dplyr::select(ano, NOMEMUN, CODMUNNASC, RACACORMAE, !!as.name(var_sel))
        
        # browser()
        df_filter_g_ano <- df_filter |> 
            dplyr::group_by(ano, !!as.name(var_sel)) |> 
            dplyr::mutate(count = n()) |> 
            dplyr::distinct(ano, !!as.name(var_sel), .keep_all = T) |>
            dplyr::select(ano, !!as.name(var_sel), count) |> 
            dplyr::ungroup()
        
        plot_ano <- ggplot(df_filter_g_ano, aes(x = ano, y = count, fill = !!as.name(var_sel))) +
            geom_bar(stat = "identity", position = position_stack(reverse = T)) +
            scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
            theme_minimal() + 
            ggplot2::coord_flip() +
            labs(title = paste0("Variável ", var_sel, " distribuída por anos selecionados"),
                 subtitle = "Distribuição por ano",
                 x = "",
                 y = "") +
            scale_colour_viridis_b()
        
        df_filter_g_racacor <- df_filter |> 
            dplyr::group_by(RACACORMAE) |> 
            dplyr::mutate(count = n()) |> 
            dplyr::distinct(RACACORMAE, .keep_all = T) |>
            dplyr::select(RACACORMAE, count) |> 
            dplyr::ungroup()
        
        plot_racacor <- ggplot(df_filter_g_racacor, aes(x = RACACORMAE, y = count, fill = RACACORMAE)) +
            geom_bar(stat = "identity", position = position_stack(reverse = T), show.legend = F) +
            scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
            theme_minimal() + 
            ggplot2::coord_flip() +
            labs(title = paste0("Variável ", var_sel, " distribuída raça/cor selecionados"),
                 subtitle = "Distribuição Geral por Raça/Cor",
                 x = "",
                 y = "") + 
            scale_fill_manual(values=c("#F5CEB9", "#F2E0D4", "#90562B", "#DAAE7F", "black"))
        
        # plot_racacor
        
        df_filter_g_mun <- df_filter |> 
            dplyr::group_by(NOMEMUN, !!as.name(var_sel)) |> 
            dplyr::mutate(count = n()) |> 
            dplyr::distinct(NOMEMUN, !!as.name(var_sel), .keep_all = T) |>
            dplyr::select(NOMEMUN, !!as.name(var_sel), count, CODMUNNASC) |> 
            dplyr::filter(!!as.name(var_sel) == input$IDCategorias) |> 
            dplyr::ungroup()
        
        dataset_final <- df_filter_g_mun |> 
            dplyr::mutate(CODMUNNASC = as.character(CODMUNNASC)) |> 
            right_join(mun_ba, by=c("CODMUNNASC"="code_muni")) |> 
            dplyr::mutate(count = ifelse(is.na(count), 0, count)) |> 
            dplyr::mutate(`Contagem` = count)
        
        categoria_sel <- unique(dataset_final$CONSPRENAT)[1]
        limit_map <- ceiling(max(dataset_final$`Contagem`) / 10) * 10
        # Plot map
        mapa <- ggplot() +
            geom_sf(data=dataset_final, aes(geometry = geom, fill=`Contagem`,
                                            text=paste0("Nome do município: ", name_muni, "<br>",
                                                        "Categoria: ", categoria_sel)),
                    color= NA, size=.15) + 
            labs(title= paste0("Distribuição de ", var_sel, " no estado da Bahia")) +
            scale_fill_distiller(palette = "Blues", limits=c(0, limit_map),
                                 direction = 1, name="Contagem") +
            theme_minimal()
        
        # mapa
        
        mapa <- plotly::ggplotly(mapa)
        
        output$graf_ano <- plotly::renderPlotly({
            ggplotly(plot_ano)
        })
        
        output$graf_racacor <- plotly::renderPlotly({
            ggplotly(plot_racacor)|> 
                plotly::hide_legend()
        })
        
        output$mapa <- plotly::renderPlotly({
            ggplotly(mapa)
        })
    })
observeEvent(input$IDVarSel, {
    var_sel_categ <- input$IDVarSel
    # browser()
    categorias <- df_sinasc |> 
        dplyr::select(!!as.name(var_sel_categ)) |> 
        dplyr::distinct(!!as.name(var_sel_categ)) |> 
        dplyr::arrange(nchar(!!as.name(var_sel_categ)), !!as.name(var_sel_categ)) |> 
        dplyr::pull()
    
    updateSelectInput(session, "IDCategorias", choices = as.list(categorias),
                      selected = categorias[1])
    })
    
    mun_ba<- geobr::read_municipality(code_muni="BA", year="2018") |> 
        dplyr::mutate(code_muni = substr(code_muni, 1, 6)) |> 
        dplyr::select(-code_state, abbrev_state)
    
}

shinyApp(ui, server)