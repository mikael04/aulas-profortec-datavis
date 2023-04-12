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
    selected = "Line Plots",
    collapsible = TRUE,
    theme = bslib::bs_theme(),
    tabPanel(
        title = "Line Plots",
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
                    # selectInput(
                    #     inputId = "anoNasc",
                    #     label = "Selecione o ano",
                    #     choices = as.list(str_sort(unique(df_sinasc$ano))),
                    #     selected = "2018"
                    # ),
                    sliderTextInput(
                        inputId = "anoNasc",
                        label = "Selecione o período:", 
                        choices = c("2018", "2019", "2020", "2021", "2022"),
                        selected = c("2018", "2022")
                    ),
                    selectInput(
                        inputId = "varSel",
                        label = "Selecione a variável",
                        choices = as.list(variaveis),
                        selected = variaveis[1]
                    ),
                    # selectInput(
                    #     inputId = "mySelectInput",
                    #     label = "Select Input",
                    #     choices = list("choice a" = "a", "choice b" = "b")
                    # ),
                    actionButton(inputId = "generate_graphs", label = "Gerar gráficos")
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
                            full_screen = TRUE,
                            card_header("Gráfico 1"),
                            plotly::plotlyOutput("graf_ano")
                        ),
                        grid_card(
                            area = "card2",
                            full_screen = TRUE,
                            card_header("Gráfico 2"),
                            plotly::plotlyOutput("mapa")
                        ),
                        grid_card(
                            area = "card3",
                            full_screen = TRUE,
                            card_header("Gráfico 3"),
                            plotly::plotlyOutput("graf_racacor")
                        )
                    )
                )
            )
        )
    )
)


server <- function(input, output) {
    observeEvent(input$generate_graphs, {
        ano_sel <- input$anoNasc
        var_sel <- input$varSel
        start <- F
        browser()
        df_filter <- df_sinasc |> 
            dplyr::filter(ano >= ano_sel[1] & ano <= ano_sel[2]) |> 
            dplyr::select(ano, NOMEMUN, CODMUNNASC, RACACORMAE, !!as.name(var_sel))
        
        df_filter_g_ano <- df_filter |> 
            dplyr::group_by(ano) |> 
            dplyr::mutate(count = n()) |> 
            dplyr::distinct(ano, .keep_all = T) |>
            dplyr::select(ano, count) |> 
            dplyr::ungroup()
        
        plot_ano <- ggplot(df_filter_g_ano, aes(x = ano, y = count, fill = ano)) +
            geom_bar(stat = "identity", position = position_stack(reverse = T), show.legend = F) +
            scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
            theme_minimal() + 
            ggplot2::coord_flip() +
            labs(title = paste0("Variável ", var_sel, " distribuída por anos selecionados"),
                 subtitle = "Distribuição por ano",
                 x = "",
                 y = "")
        
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
                 subtitle = "Distribuição por Raça/Cor",
                 x = "",
                 y = "") + 
            scale_fill_manual(values=c("#F5CEB9", "#F2E0D4", "#90562B", "#DAAE7F", "black"))
        
        ggplotly(plot_racacor) 
        
        df_filter_g_mun <- df_filter |> 
            dplyr::group_by(NOMEMUN) |> 
            dplyr::mutate(count = n()) |> 
            dplyr::distinct(NOMEMUN, .keep_all = T) |>
            dplyr::select(NOMEMUN, count, CODMUNNASC) |> 
            dplyr::ungroup()
        
        dataset_final <- df_filter_g_mun |> 
            dplyr::mutate(CODMUNNASC = as.character(CODMUNNASC)) |> 
            right_join(mun_ba, by=c("CODMUNNASC"="code_muni")) |> 
            dplyr::mutate(count = ifelse(is.na(count), 0, count)) |> 
            dplyr::mutate(`Contagem` = count)
        
        # Plot map
        mapa <- ggplot() +
            geom_sf(data=dataset_final, aes(geometry = geom, fill=`Contagem`,
                                            text=paste0("Nome do município: ", name_muni)),
                    color= NA, size=.15) + 
            labs(title= paste0("Distribuição de ", var_sel, " no estado da Bahia")) +
            scale_fill_distiller(palette = "Blues", limits=c(0, 1500),
                                 direction = 1, name="Contagem") +
            theme_minimal()
        
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
    
    
    mun_ba<- geobr::read_municipality(code_muni="BA", year="2018") |> 
        dplyr::mutate(code_muni = substr(code_muni, 1, 6)) |> 
        dplyr::select(-code_state, abbrev_state)
    
    
}

shinyApp(ui, server)