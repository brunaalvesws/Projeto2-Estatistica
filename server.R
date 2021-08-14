
# Define server logic required to draw a histogram
server <- function(input, output) {
    ################### INPUT ####################
    select_column <- eventReactive(input$go, {
        df_column <- master_df 
        return(df_column)
    })
    
    select_column_comp <- eventReactive(input$go_comp, {
        df_column <- master_df 
        return(df_column)
    })
    
    output$timedate <- renderUI({
        
        column_name <- input$column
        
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
        
        column_name <- input$column_comp
        
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
        df <- select_column()
        column_name <- input$column
        twin <- input$true_date
        
        datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
        
        vetor <- datacut[, c(column_name)]
        
        Media <- mean(vetor)
        Mediana <- median(vetor)
        DP <- sd(vetor)
        Moda <- getmode(vetor)
        
        Vacina <- input$column
        
        df_tb <-  data.frame(Vacina, Media, Mediana, DP, Moda)
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
    
    ################ OUTPUT COMP #####################
    Info_DataTable_COMP <- eventReactive(input$go_comp,{
        df <- select_column_comp()
        column_names <- input$column_comp
        twin <- input$true_date
        
        datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
        
        vetor_1 <- datacut[, c(column_names[1])]
        vetor_2 <- datacut[, c(column_names[2])]
        
        correlacao <- cor(vetor_1, vetor_2)
        
        colunas <- paste(column_names[1], column_names[2], sep = ' x ')
        
        df_tb <-  data.frame(colunas, correlacao)
        df_tb <- as.data.frame(t(df_tb))
        
        # tb  <- as_tibble(cbind(nms = names(df_tb), t(df_tb)))
        # tb <- tb %>% 
        #     rename('Informações' = nms,
        #            'Valores' = V2)
        # 
        return(df_tb)
    })
    
    output$info_comp <- renderDT({
        column_names <- input$column_comp
        if (length(column_names)>1){
            Info_DataTable_COMP() %>%
                as.data.frame() %>% 
                DT::datatable(options=list(
                    language=list(
                        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                    )
                ))
        }
    })
    
    output$sh <- renderPlot({
        # All the inputs
        df <- select_column()
        column_name <- input$column
        twin <- input$true_date
        
        datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
        
        aux <- datacut[, column_name] %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        datacut$Date <- ymd(datacut$Date)
        a <- datacut %>% 
            ggplot(aes_string("Date", toString(column_name), group='1')) +
            geom_path() +
            ylab(toString(column_name)) +
            coord_cartesian(ylim = c(aux1, aux2)) +
            theme_bw() +
            scale_x_date(date_labels = "%Y-%m-%d")
        
        a
        
    })
    output$hist <- renderPlot({
        # All the inputs
        df <- select_column()
        column_name <- input$column
        twin <- input$true_date
        
        datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
        
        aux <- datacut[, column_name] %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        datacut$Date <- ymd(datacut$Date)
        p <- datacut %>% 
            ggplot(aes_string(toString(column_name))) + 
            geom_histogram(color="black", fill="blue") +
            theme_bw() +
            labs(x="Frequência", toString(column_name))
        
        p
    })
    output$box <- renderPlot({
        # All the inputs
        df <- select_column()
        column_name <- input$column
        twin <- input$true_date
        
        df[, "week"] <- as.factor(week(df[,"Date"]))

        datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
        b <- datacut %>% 
            ggplot(aes_string(x="week", y=toString(column_name))) + 
            geom_boxplot()
        b
    })
    
    output$comp_sh1 <- renderPlot({
        # All the inputs
        column_names <- input$column_comp
        if (!is.null(column_names)){
            df <- select_column_comp()
            twin <- input$true_date_comp
            
            datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
            
            aux <- datacut[, column_names[1]] %>% na.omit() %>% as.numeric()
            aux1 <- min(aux)
            aux2 <- max(aux)
            datacut$Date <- ymd(datacut$Date)
            
            a <- datacut %>% 
                ggplot(aes_string("Date", toString(column_names[1]), group='1')) +
                geom_path() +
                ylab(toString(column_names[1])) +
                coord_cartesian(ylim = c(aux1, aux2)) +
                theme_bw() +
                scale_x_date(date_labels = "%Y-%m-%d")
            
            a
        }
    })
    
    output$comp_sh2 <- renderPlot({
        # All the inputs
        column_names <- input$column_comp
        if (length(column_names) > 1){
            df <- select_column_comp()
            twin <- input$true_date_comp
            
            datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
            
            aux <- datacut[, column_names[2]] %>% na.omit() %>% as.numeric()
            aux1 <- min(aux)
            aux2 <- max(aux)
            datacut$Date <- ymd(datacut$Date)
            
            a <- datacut %>% 
                ggplot(aes_string("Date", toString(column_names[2]), group='1')) +
                geom_path() +
                ylab(toString(column_names[2])) +
                coord_cartesian(ylim = c(aux1, aux2)) +
                theme_bw() +
                scale_x_date(date_labels = "%Y-%m-%d")
            
            a
        }
    })
}
