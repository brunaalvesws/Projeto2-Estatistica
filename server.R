
# Define server logic required to draw a histogram
server <- function(input, output) {
    ################### INPUT ####################
    select_stock <- eventReactive(input$go, {
        
        stock_name <- input$stock
        twin <- input$true_date
        
        # df_stock <- master_df %>% 
        #    filter(Index == stock_name) 
        ## FALTA -> FILTRAR O DF POR DATA!!
        df_stock <- master_df 
        return(df_stock)
    })
    
    output$timedate <- renderUI({
        
        stock_name <- input$stock
        
        df <- master_df
        
        min_time <- min(df$Date)
        max_time <- max(df$Date)
        dateRangeInput("true_date", "Período de análise",
                       end = max_time,
                       start = min_time,
                       min  = min_time,
                       max  = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    output$timedate_comp <- renderUI({
        
        stock_name <- input$stock_comp
        
        df <- master_df
        
        maxmin_time <- df %>% 
            summarise(MD = min(Date)) %>% 
            .$MD %>% 
            max()
        
        minmax_time <- df %>% 
            summarise(MD = max(Date)) %>% 
            .$MD %>% 
            min()
        
        min_time <- maxmin_time
        max_time <- minmax_time
        
        dateRangeInput("true_date_comp", "Período de análise",
                       end = max_time,
                       start = min_time,
                       min    = min_time,
                       max    = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    # Create the function.
    getmode <- function(v){
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    
    ################ OUTPUT #####################
    Info_DataTable <- eventReactive(input$go,{
        df <- select_stock()
        stock_name <- input$stock
 
        vetor <- df[, c(stock_name)]
        
        Media <- mean(vetor)
        
        Mediana <- median(vetor)

        DP <- sd(vetor)

        Moda <- getmode(vetor)

        
        Stock <- input$stock
        
        df_tb <-  data.frame(Stock, Media, Mediana, DP, Moda)
        
        df_tb <- as.data.frame(t(df_tb))
        
        # tb  <- as_tibble(cbind(nms = names(df_tb), t(df_tb)))
        # tb <- tb %>% 
        #     rename('Informações' = nms,
        #            'Valores' = V2)
        # 
        return(df_tb)
    })
    
    output$info <- renderDT({
        Info_DataTable() %>%
            as.data.frame() %>% 
            DT::datatable(options=list(
                language=list(
                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                )
            ))
    })
    
    output$sh <- renderPlot({
        # All the inputs
        df <- select_stock()
        stock_name <- input$stock
        if (stock_name == "Doses_Administered"){
            aux <- df$Doses_Administered %>% na.omit() %>% as.numeric()
            aux1 <- min(aux)
            aux2 <- max(aux)
            
            df$Date <- ymd(df$Date)
            a <- df %>% 
                ggplot(aes(Date, Doses_Administered, group=1)) +
                geom_path() +
                ylab('Preço da Ação em $') +
                coord_cartesian(ylim = c(aux1, aux2)) +
                theme_bw() +
                scale_x_date(date_labels = "%Y-%m-%d")
        }
        if (stock_name == "Doses_per_1000"){
            aux <- df$Doses_per_1000 %>% na.omit() %>% as.numeric()
            aux1 <- min(aux)
            aux2 <- max(aux)
            
            df$Date <- ymd(df$Date)
            a <- df %>% 
                ggplot(aes(Date, Doses_per_1000, group=1)) +
                geom_path() +
                ylab('Preço da Ação em $') +
                coord_cartesian(ylim = c(aux1, aux2)) +
                theme_bw() +
                scale_x_date(date_labels = "%Y-%m-%d")
        }
        if (stock_name == "Fully_Vaccinated_Population"){
            aux <- df$Fully_Vaccinated_Population %>% na.omit() %>% as.numeric()
            aux1 <- min(aux)
            aux2 <- max(aux)
            
            df$Date <- ymd(df$Date)
            a <- df %>% 
                ggplot(aes(Date, Fully_Vaccinated_Population, group=1)) +
                geom_path() +
                ylab('Preço da Ação em $') +
                coord_cartesian(ylim = c(aux1, aux2)) +
                theme_bw() +
                scale_x_date(date_labels = "%Y-%m-%d")
        }

        a
    })
}
