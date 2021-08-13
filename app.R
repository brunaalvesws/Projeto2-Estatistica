setwd('~/ProjetoVacina/Projeto2-Estatistica/')

source('global.R')
source('ui.R')
source('server.R')


shinyApp(
  ui = ui,
  server = server
)
