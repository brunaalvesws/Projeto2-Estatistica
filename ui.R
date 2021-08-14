

header <- dashboardHeader(title = "Projeto de Estatística")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Métricas", tabName = "m", icon = icon("chart-line")),
        menuItem('Comparando a Vacinação', tabName = 'comp', icon = icon('chart-bar'))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = 'm',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                        selectInput('column', 'Tipo de Vacinação', column_list, multiple=FALSE),
                        uiOutput("timedate"),
                        actionButton('go', 'Submeter')
                        )
                ),
                fluidRow(
                    box(title = "Informações sobre a vacinação", width = 12, solidHeader = TRUE,
                        DTOutput('info')
                    )
                ),
                fluidRow(
                    box(title = "Gráfico de linha", width = 12, solidHeader = TRUE,
                        plotOutput('sh')
                    )
                ),
                fluidRow(
                    box(title = "Histograma", width = 12, solidHeader = TRUE,
                        plotOutput('hist')
                    )
                ),
                fluidRow(
                    box(title = "BoxPlot", width = 12, solidHeader = TRUE,
                        plotOutput('box')
                    )
                ),
        ),
        tabItem(tabName = 'comp',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                        selectInput('column_comp', 'Vacinação', column_list, multiple=TRUE),
                        uiOutput("timedate_comp"),
                        actionButton('go_comp', 'Submeter')
                    )
                ),  
                fluidRow(
                    box(title = "Correlacao entre as colunas", width = 12, solidHeader = TRUE,
                        DTOutput('info_comp')
                    )
                ),
                fluidRow(
                    box(title = "Gráfico de linha", width = 12, solidHeader = TRUE,
                        plotOutput('comp_sh1'),
                        plotOutput('comp_sh2')
                    )
                ),
        )
    )
)

ui <- dashboardPage(
    skin = 'blue',
    header, sidebar, body)
