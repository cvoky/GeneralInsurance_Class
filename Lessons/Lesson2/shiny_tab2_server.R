
# Data for lesson 2 load
data_lesson2_react <- reactive({
  read.csv("data/lesson2_KPI.csv")
})

# Definition of basic KPIs
data_lesson2_KPI_prep_react <- reactive({
  dt <- data_lesson2_react()
  
  prepared <- 
    dt %>% # copy all data manipulation we have done in data preparation for KPIs definition
    filter(Premium > 0,
           Losses > 0, 
           Expenses > 0
    ) %>% 
    mutate(LR = Losses / Premium, 
           ER = Expenses / Premium, 
           CoR = (Losses + Expenses) / Premium
    ) 
  
  prepared
})

#### Sub-Tab Multi Dim (Focus Group) ####
# dynamically generate filter widgets if columns are categorical (character or factor)
source(file.path("Lessons", "Lesson2", "Support", "dynamic_filter.R"),
       local = TRUE)$value

######################################
output$lesson2_KPI_multidim_table <- renderDataTable({
  
  dt_prep <- data_lesson2_KPI_multidim_prep_filter_react()

  dt_prep <- 
    dt_prep %>%
    group_by_at(vars(input$lesson2_kpi_multidim_select_axis)) %>% 
    summarize(LR = mean(LR, na.rm =TRUE), 
              ER = mean(ER, na.rm = TRUE), 
              CoR = mean(CoR, na.rm = TRUE)
    )
    
  
  DT::datatable(dt_prep,
                rownames = FALSE,
                class = "hover") %>% 
    DT::formatPercentage(c("LR", "ER", "CoR"))
})

output$lesson2_KPI_multidim_ratio_graph <- renderPlot({

  data_lesson2_KPI_multidim_prep_filter_react() %>% 
    group_by_at(vars(input$lesson2_kpi_multidim_select_axis)) %>% 
    summarize(LR = mean(LR, na.rm =TRUE), 
              ER = mean(ER, na.rm = TRUE), 
              CoR = mean(CoR, na.rm = TRUE)
    ) %>% 
    ggplot() +
      geom_col(
        aes_string(x = input$lesson2_kpi_multidim_select_axis, y = "LR")
      ) +
      geom_hline(yintercept = 1) +
      ylab("Ratios") +
      scale_y_continuous(labels = scales::percent) +
      theme_bw() +
      coord_flip()
  
})

output$lesson2_KPI_multidim_UWR_graph <- renderPlot({
  # # Homework
  # data_lesson2_KPI_multidim_prep_filter_react() %>% 
  # 

})


