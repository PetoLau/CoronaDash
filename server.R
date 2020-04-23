## app.R ##
function(input, output, session) {
  
  # Load scripts/modules -----
  
  # reading data
  source("01_scripts/read_data_cssegis.R")
  source("01_scripts/aggregate_data.R")
  source("01_scripts/read_populations.R")
  
  # forecasting
  source("01_scripts/forecasting.R")
  
  # clustering
  source("01_scripts/clustering.R")
  source("01_scripts/clustering_trajectories.R")
 
  # menu -----
  output$Side_dash <- renderMenu({
    
    sidebarMenu(
      id = "sideBar_Menu",
      menuItem("COVID-19 Forecasting",
               icon = icon("chart-line"),
               tabName = "corTab",
               startExpanded = F,
               selected = T
               ),
      menuItem("Compare countries",
               icon = icon("chart-bar"),
               tabName = "compareTab",
               startExpanded = F,
               selected = F
               ),
      menuItem("Cluster trajectories",
               icon = icon("chart-line"),
               tabName = "trajectoryTab",
               startExpanded = F,
               selected = F
               ),
      menuItem("Cluster countries",
               icon = icon("object-group"),
               tabName = "analysisTab",
               startExpanded = F,
               selected = F
               ),
      menuItem("COVID-19 World agg.",
               icon = icon("globe"),
               tabName = "worldTab",
               startExpanded = F,
               selected = F
               )
      )
  })
  
  observe({
    
    query <- parseQueryString(session$clientData$url_search)
    
    query1 <- paste(names(query), query, sep = "=", collapse = ", ")
    
    # print(query1)
    
    if (query1 == "tab=compareTab") {
      
      updateTabItems(session, inputId = "sideBar_Menu", selected = "compareTab")
      
    } else if (query1 == "tab=worldTab") {
      
      updateTabItems(session, inputId = "sideBar_Menu", selected = "worldTab")
      
    } else if (query1 == "tab=analysisTab") {
      
      updateTabItems(session, inputId = "sideBar_Menu", selected = "analysisTab")
      
    } else if (query1 == "tab=trajectoryTab") {
      
      updateTabItems(session, inputId = "sideBar_Menu", selected = "trajectoryTab")
      
    }
    
  })
  
  # informative text for this app -----
  output$informative_text <- renderUI({
    
    tags$html(tags$p("This application is only for informative and analytics purposes,
                     how the COVID-19 virus can spread over time for a defined country and period of days (confirmed cases).
                     There isn't motivation to replace more sophisticated epidemiology models like SIR."),
              tags$p("Data are coming from",
                     tags$a(href = 'https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series',
                            target="_blank", "Johns Hopkins CSSE GitHub repository"),
                     ",",
                     tags$a(href = 'https://github.com/ulklc/covid19-timeseries',
                            target="_blank", "GitHub repository by ulklc"),
                     ", and",
                     tags$a(href = 'https://github.com/ChrisMichaelPerezSantiago/covid19',
                            target="_blank", "tests data are coming from COVID19 API.")),
              tags$p("The forecasting model is the ETS (Exponential smoothing) implemented in a smooth R package,
                      so only historical data of target time series are used (extrapolation) -
                      the multiplicative model with the possibility of a damped trend is used."
                     ),
              tags$p(
                tags$a("You can compare multiple countries' trajectories for various statistics in the", tags$b("Compare countries"), "tab,",
                       onclick = "openTab('compareTab')", href="#"),
                tags$a("and cluster them using time series data mining methods in the", tags$b("Cluster trajectories"), "tab.",
                       onclick = "openTab('trajectoryTab')", href="#"),
                tags$a("You can also cluster countries based on multiple last updated statistics and
                       view it on scatter plots and dendograms in the", tags$b("Cluster countries"), "tab.",
                       onclick = "openTab('analysisTab')", href="#"),
                tags$a("You can also check the aggregated World cases + forecasts in the", tags$b("COVID-19 World agg."), "tab.",
                            onclick = "openTab('worldTab')", href="#")
                       ),
              tags$p("The forecasting model applied on the Covid-19 use case was inspired by",
                     tags$a(href = 'https://twitter.com/fotpetr',
                            target="_blank", "Fotios Petropoulos tweets.")),
              tags$p("An author of this app currently works as a Data Scientist for start-up",
                      tags$a(href = 'https://powerex.io/',
                      target="_blank", "PowereX.")),
              tags$p("Take care of yourself!")
              )
    
  })
  
  # read the data ----
  data_corona <- reactive({
    
    data_res <- join_all_corona_data()
    
    data_pop <- read_populations()

    data_res[data_pop,
             on = .(Country),
             Population := i.Population]
    
    data_res
    
  })
  
  # date of update -----
  output$text_date_update <- renderUI({
    
    tags$html(tags$p(tags$b("Last data updated: "), data_corona()[, max(DateRep)]
                     )
              )
    
  })
  
  # Country selector -----
  output$selector_country <- renderUI({
    
    pickerInput(
      inputId = "country",
      label = "Pick a country:", 
      choices = data_corona()[, unique(Country)],
      selected = "US",
      options = list(
        `live-search` = TRUE,
         style = "btn-info",
         maxOptions = 7
        )
      )
    
  })
  
  # N days forecast slider ----
  output$slider_n_days_forec <- renderUI({
    
    sliderInput(
      inputId = "n_days_forec", 
      label = "Set how many days ahead to create a forecast for:",
      min = 1,
      max = 30,
      value = 7
    )
    
  })
  
  # Latest stats ----
  data_countries_stats <- reactive({
    
    data_res <- copy(data_corona())
    
    data_res_latest <- copy(data_res[,
                                     .SD[DateRep == max(DateRep)],
                                     by = .(Country)]
                            )
    
    setorder(data_res_latest, -Active_cases_cumsum)
    
    data_res_latest
    
  })
  
  # DT of most infected countries ----
  output$dt_countries_cases <- renderDataTable({
    
    data_res_latest <- copy(data_countries_stats())
    
    DT::datatable(data_res_latest[, .(Country,
                                      'Total Cases' = Cases_cumsum,
                                      'Total Deaths' = Deaths_cumsum,
                                      'Active Cases' = Active_cases_cumsum,
                                      'New cases' = Cases,
                                      'ActCases/ MilPop' = ceiling((Active_cases_cumsum / Population) * 1e6),
                                      'PositiveTests Rate' = round((Cases_cumsum / TotalTests) * 100, 2),
                                      'Tests/ MilPop' = Tests_1M_Pop
                                      )],
                  selection = "single",
                  class = "compact",
                  extensions = c('Buttons', 'Scroller'),
                  options = list(
                    pageLength = 10,
                    dom = 'Bfrtip',
                    deferRender = TRUE,
                    scrollY = 270,
                    scroller = TRUE,
                    buttons = c('csv', 'excel'),
                    scrollX = TRUE
                  ))
  })
  
  # Subset data by a country ----
  data_country <- reactive({
    
    shiny::req(input$country)
    
    data_res <- copy(data_corona()[.(input$country), on = .(Country)])
    
    data_res
    
  })
  
  # Value boxes of statistics -----
  
  output$valuebox_total_cases <- renderValueBox({
    
    valueBox(
      format(data_country()[.N, Cases_cumsum], nsmall=1, big.mark=","),
      "Total confirmed cases",
      icon = icon("ambulance"),
      color = "orange"
    )
    
  })
  
  output$valuebox_total_deaths <- renderValueBox({
    
    valueBox(
      format(data_country()[.N, Deaths_cumsum], nsmall=1, big.mark=","),
      "Total confirmed deaths",
      icon = icon("skull"),
      color = "red"
    )
    
  })
  
  output$valuebox_death_rate <- renderValueBox({
    
    valueBox(
      paste0(round(data_country()[.N, Deaths_cumsum]/data_country()[.N, Cases_cumsum], digits = 4)*100, "%"),
      "Death rate",
      icon = icon("exclamation-triangle"),
      color = "maroon"
    )
    
  })
  
  output$valuebox_total_recov <- renderValueBox({
    
    valueBox(
      format(data_country()[.N, Recovered_cumsum], nsmall=1, big.mark=","),
      "Total confirmed recovered cases",
      icon = icon("star-of-life"),
      color = "green"
    )
    
  })
  
  output$valuebox_total_active <- renderValueBox({
    
    valueBox(
      format(data_country()[.N, Active_cases_cumsum], nsmall=1, big.mark=","),
      "Total confirmed active cases",
      icon = icon("hospital-alt"),
      color = "yellow"
    )
    
  })
  
  output$valuebox_active_per_mil <- renderValueBox({
    
    valueBox(
      format(data_country()[.N,
                            as.integer(ceiling((Active_cases_cumsum / Population) * 1e6))], nsmall=1, big.mark=","),
      "Total confirmed active cases per 1 million population",
      icon = icon("male"),
      color = "purple"
    )
    
  })
  
  output$valuebox_positivetests_rate <- renderValueBox({
    
    valueBox(
      paste0(data_country()[.N,
                            round((Cases_cumsum / TotalTests)*100, 2)], "%"),
      "Positive tests rate",
      icon = icon("vial"),
      color = "maroon"
    )
    
  })
  
  output$valuebox_num_tests_pop <- renderValueBox({
    
    valueBox(
      format(data_country()[.N,
                             Tests_1M_Pop], nsmall=1, big.mark=","),
             "Number of tests per 1 million population",
             icon = icon("vials"),
             color = "olive"
      )
      
  })
  
  # informative text for cases -----
  output$cases_text <- renderUI({
    
    tags$html(tags$p("Cases = New confirmed cases at that day."),
              tags$p("Cases cumulative = Total accumulated confirmed cases at that day."),
              tags$p("Recovered cumulative = Total accumulated recovered cases at that day.")
              )
    
  })
  
  # Show cases of the selected country ----
  output$dygraph_country_cases <- renderDygraph({
    
    shiny::req(input$country)
    
    dygraph(data_country()[, .(DateRep, 'Cases cumulative' = Cases_cumsum, Cases, 'Recovered cumulative' = Recovered_cumsum)],
            main = input$country) %>%
      # dyAxis("y", label = "Cases") %>%
      dyRangeSelector(dateWindow = c(data_country()[, max(DateRep) - 10], data_country()[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = c("#5bc0de", "#FF6347", "#228b22")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # informative text for deaths -----
  output$death_text <- renderUI({
    
    tags$html(tags$p("Deaths = New confirmed death cases at that day."),
              tags$p("Deaths cumulative = Total accumulated confirmed deaths at that day.")
              )
    
  })
  
  # Show deaths of the selected country ----
  output$dygraph_country_deaths <- renderDygraph({
    
    shiny::req(input$country)
    
    dygraph(data_country()[, .(DateRep, 'Deaths cumulative' = Deaths_cumsum, Deaths)],
            main = input$country) %>%
      # dyAxis("y", label = "Deaths") %>%
      dyRangeSelector(dateWindow = c(data_country()[, max(DateRep) - 10], data_country()[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = c("#5bc0de", "#228b22")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  #### Compute forecasts --------
  
  # Forecasting Cases cumulative -----
  data_cases_cumsum_forec <- reactive({
    
    req(input$country, input$n_days_forec)
    
    data_res <- copy(data_country())
    
    data_forec <- forec_cases_cumsum(data_res, input$n_days_forec)
    
    data_res <- rbindlist(list(
      data_res,
      data.table(DateRep = seq.Date(data_res[, max(DateRep) + 1],
                                    data_res[, max(DateRep) + input$n_days_forec],
                                    by = 1),
                 Cases_cumsum_mean = round(data_forec$forecast, digits = 0),
                 Cases_cumsum_lwr = floor(data_forec$forecast),
                 Cases_cumsum_upr = data_forec$upper
                 )
      ), fill = TRUE, use.names = TRUE
      )
    
    data_res[, Model := data_forec$model]
    
    data_res
    
  })
  
  # informative text for forecasted cases -----
  output$cases_forec_text <- renderUI({
    
    tags$html(
              tags$p("Cases cumulative = Total accumulated confirmed cases at that day.")
              )
    
  })
  
  # Show forecasted cases of the selected country ----
  output$dygraph_country_cases_forecast <- renderDygraph({
    
    shiny::req(input$country, input$n_days_forec)
    
    data_res <- copy(data_cases_cumsum_forec())
    
    dygraph(data_res[, .(DateRep, 'Cases cumulative' = Cases_cumsum,
                         Cases_cumsum_mean, Cases_cumsum_lwr, Cases_cumsum_upr)],
            main = paste0(input$country,
                          ", model: ",
                          data_res[, unique(Model)])) %>%
      # dyAxis("y", label = "Cases - cumulative") %>%
      dySeries('Cases cumulative') %>%
      dySeries(c("Cases_cumsum_lwr", "Cases_cumsum_mean", "Cases_cumsum_upr"),
               label = "Cases cumulative - forecast") %>%
      dyRangeSelector(dateWindow = c(data_res[, max(DateRep) - input$n_days_forec - 7],
                                     data_res[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = c("#5bc0de", "#228b22")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyEvent(data_res[is.na(Cases_cumsum_mean), max(DateRep)],
              "Forecasting origin", labelLoc = "bottom") %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # Forecasting Deaths cumulative -----
  # data_deaths_cumsum_forec <- reactive({
  #   
  #   req(input$country, input$n_days_forec)
  #   
  #   data_res <- copy(data_country())
  #   
  #   data_forec <- forec_deaths_cumsum(data_res, input$n_days_forec)
  #   
  #   data_res <- rbindlist(list(
  #     data_res,
  #     data.table(DateRep = seq.Date(data_res[, max(DateRep) + 1],
  #                                   data_res[, max(DateRep) + input$n_days_forec],
  #                                   by = 1),
  #                Deaths_cumsum_mean = round(data_forec$forecast, digits = 0),
  #                Deaths_cumsum_lwr = floor(data_forec$forecast),
  #                Deaths_cumsum_upr = data_forec$upper
  #     )
  #   ), fill = TRUE, use.names = TRUE
  #   )
  #   
  #   data_res[, Model := data_forec$model]
  #   
  #   data_res
  #   
  # })
  # 
  # # Show forecasted deaths of the selected country ----
  # output$dygraph_country_deaths_forecast <- renderDygraph({
  #   
  #   shiny::req(input$country, input$n_days_forec)
  #   
  #   data_res <- copy(data_deaths_cumsum_forec())
  #   
  #   dygraph(data_res[, .(DateRep, 'Deaths cumulative' = Deaths_cumsum,
  #                        Deaths_cumsum_mean, Deaths_cumsum_lwr, Deaths_cumsum_upr)],
  #           main = paste0(input$country,
  #                         ", model: ",
  #                         data_res[, unique(Model)])) %>%
  #     # dyAxis("y", label = "Deaths - cumulative") %>%
  #     dySeries('Deaths cumulative') %>%
  #     dySeries(c("Deaths_cumsum_lwr", "Deaths_cumsum_mean", "Deaths_cumsum_upr"),
  #              label = "Deaths cumulative - forecast") %>%
  #     dyRangeSelector(dateWindow = c(data_res[, max(DateRep) - input$n_days_forec - 7],
  #                                    data_res[, max(DateRep) + 1]),
  #                     fillColor = "#5bc0de", strokeColor = "#222d32") %>%
  #     dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
  #               fillGraph = TRUE, fillAlpha = 0.4,
  #               drawPoints = TRUE, pointSize = 3,
  #               pointShape = "circle",
  #               colors = c("#5bc0de", "#228b22")) %>%
  #     dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
  #     dyEvent(data_res[is.na(Deaths_cumsum_mean), max(DateRep)],
  #             "Forecasting origin", labelLoc = "bottom") %>%
  #     dyLegend(width = 400, show = "always")
  #   
  # })
  
  #### World aggregated -----------
  
  data_world <- reactive({
    
    data_res <- aggregate_data(data_corona())
    
    data_res
    
  })
  
  # Value boxes of world statistics -----
  
  output$valuebox_total_cases_world <- renderValueBox({
    
    valueBox(
      format(data_world()[.N, Cases_cumsum], nsmall=1, big.mark=","),
      "Total confirmed cases",
      icon = icon("ambulance"),
      color = "orange"
    )
    
  })
  
  output$valuebox_total_deaths_world <- renderValueBox({
    
    valueBox(
      format(data_world()[.N, Deaths_cumsum], nsmall=1, big.mark=","),
      "Total confirmed deaths",
      icon = icon("skull"),
      color = "red"
    )
    
  })
  
  output$valuebox_death_rate_world <- renderValueBox({
    
    valueBox(
      paste0(round(data_world()[.N, Deaths_cumsum]/data_world()[.N, Cases_cumsum], digits = 4)*100, "%"),
      "Death rate",
      icon = icon("exclamation-triangle"),
      color = "maroon"
    )
    
  })

  output$valuebox_total_recov_world <- renderValueBox({
    
    valueBox(
      format(data_world()[.N, Recovered_cumsum], nsmall=1, big.mark=","),
      "Total confirmed recovered cases",
      icon = icon("star-of-life"),
      color = "green"
    )
    
  })
  
  output$valuebox_total_active_world <- renderValueBox({
    
    valueBox(
      format(data_world()[.N, Active_cases_cumsum], nsmall=1, big.mark=","),
      "Total confirmed active cases",
      icon = icon("hospital-alt"),
      color = "yellow"
      )
    
  })
  
  output$valuebox_active_per_mil_world <- renderValueBox({
    
    valueBox(
      format(data_world()[.N,
                          as.integer(ceiling((Active_cases_cumsum / Population) * 1e6))],
             nsmall=1, big.mark=","),
      "Total confirmed active cases per 1 million population",
      icon = icon("male"),
      color = "purple"
    )
    
  })
  
  # informative text for cases -world -----
  output$cases_text_world <- renderUI({
    
    tags$html(tags$p("Cases = New confirmed cases at that day."),
              tags$p("Cases cumulative = Total accumulated confirmed cases at that day."),
              tags$p("Recovered cumulative = Total accumulated recovered cases at that day.")
              )
    
  })
  
  # Show cases of the world ----
  output$dygraph_world_cases <- renderDygraph({
    
    dygraph(data_world()[, .(DateRep, 'Cases cumulative' = Cases_cumsum, Cases, 'Recovered cumulative' = Recovered_cumsum)],
            main = "World") %>%
      # dyAxis("y", label = "Cases") %>%
      dyRangeSelector(dateWindow = c(data_world()[, max(DateRep) - 60], data_country()[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = c("#5bc0de", "#FF6347", "#228b22")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # informative text for deaths - world -----
  output$death_text_world <- renderUI({
    
    tags$html(tags$p("Deaths = New confirmed death cases at that day."),
              tags$p("Deaths cumulative = Total accumulated confirmed deaths at that day.")
    )
    
  })
  
  # Show deaths of the world ----
  output$dygraph_world_deaths <- renderDygraph({

    dygraph(data_world()[, .(DateRep, 'Deaths cumulative' = Deaths_cumsum, Deaths)],
            main = "World") %>%
      # dyAxis("y", label = "Deaths") %>%
      dyRangeSelector(dateWindow = c(data_world()[, max(DateRep) - 60], data_country()[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = c("#5bc0de", "#228b22")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # Forecasting Cases cumulative world -----
  data_cases_cumsum_forec_world <- reactive({
    
    data_res <- copy(data_world())
    
    data_forec <- forec_cases_cumsum(data_res, 10)
    
    data_res <- rbindlist(list(
      data_res,
      data.table(DateRep = seq.Date(data_res[, max(DateRep) + 1],
                                    data_res[, max(DateRep) + 10],
                                    by = 1),
                 Cases_cumsum_mean = round(data_forec$forecast, digits = 0),
                 Cases_cumsum_lwr = floor(data_forec$forecast),
                 Cases_cumsum_upr = data_forec$upper
      )
    ), fill = TRUE, use.names = TRUE
    )
    
    data_res[, Model := data_forec$model]
    
    data_res
    
  })
  
  # Show forecasted cases of the world ----
  output$dygraph_world_cases_forecast <- renderDygraph({
    
    data_res <- copy(data_cases_cumsum_forec_world())
    
    dygraph(data_res[, .(DateRep, 'Cases cumulative' = Cases_cumsum,
                         Cases_cumsum_mean, Cases_cumsum_lwr, Cases_cumsum_upr)],
            main = paste0("World",
                          ", model: ",
                          data_res[, unique(Model)])) %>%
      # dyAxis("y", label = "Cases - cumulative") %>%
      dySeries('Cases cumulative') %>%
      dySeries(c("Cases_cumsum_lwr", "Cases_cumsum_mean", "Cases_cumsum_upr"),
               label = "Cases cumulative - forecast") %>%
      dyRangeSelector(dateWindow = c(data_res[, max(DateRep) - 10 - 7],
                                     data_res[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = c("#5bc0de", "#228b22")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyEvent(data_res[is.na(Cases_cumsum_mean), max(DateRep)],
              "Forecasting origin", labelLoc = "bottom") %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # Forecasting Deaths cumulative for world -----
  data_deaths_cumsum_forec_world <- reactive({
    
    data_res <- copy(data_world())
    
    data_forec <- forec_deaths_cumsum(data_res, 10)
    
    data_res <- rbindlist(list(
      data_res,
      data.table(DateRep = seq.Date(data_res[, max(DateRep) + 1],
                                    data_res[, max(DateRep) + 10],
                                    by = 1),
                 Deaths_cumsum_mean = round(data_forec$forecast, digits = 0),
                 Deaths_cumsum_lwr = floor(data_forec$forecast),
                 Deaths_cumsum_upr = data_forec$upper
      )
    ), fill = TRUE, use.names = TRUE
    )
    
    data_res[, Model := data_forec$model]
    
    data_res
    
  })
  
  # Show forecasted deaths of the world ----
  output$dygraph_world_deaths_forecast <- renderDygraph({

    data_res <- copy(data_deaths_cumsum_forec_world())
    
    dygraph(data_res[, .(DateRep, 'Deaths cumulative' = Deaths_cumsum,
                         Deaths_cumsum_mean, Deaths_cumsum_lwr, Deaths_cumsum_upr)],
            main = paste0("World",
                          ", model: ",
                          data_res[, unique(Model)])) %>%
      # dyAxis("y", label = "Deaths - cumulative") %>%
      dySeries('Deaths cumulative') %>%
      dySeries(c("Deaths_cumsum_lwr", "Deaths_cumsum_mean", "Deaths_cumsum_upr"),
               label = "Deaths cumulative - forecast") %>%
      dyRangeSelector(dateWindow = c(data_res[, max(DateRep) - 10 - 7],
                                     data_res[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = c("#5bc0de", "#228b22")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyEvent(data_res[is.na(Deaths_cumsum_mean), max(DateRep)],
              "Forecasting origin", labelLoc = "bottom") %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  #### Comparison of countries ------
  
  # output$checkboxgroup_countries_selector <- renderUI({
  #   
  #   shinyWidgets::checkboxGroupButtons(
  #     inputId = "countries_selector",
  #     label = "Pick countries for comparison:",
  #     choices = data_corona()[, unique(Country)],
  #     selected = c("US", "Italy", "France", "Spain", "Germany", "United Kingdom"), 
  #     checkIcon = list(
  #       yes = tags$i(class = "fa fa-check-square", 
  #                    style = "color: blue"),
  #       no = tags$i(class = "fa fa-square-o", 
  #                   style = "color: blue")
  #     )
  #   )
  #   
  # })
  
  output$picker_countries_selector <- renderUI({
    
    shinyWidgets::pickerInput(
    inputId = "countries_selector",
    label = NULL, 
    choices = data_corona()[, unique(Country)],
    selected = c("US", "Italy", "France", "Spain", "Germany", "United Kingdom",
                 "Belgium", "Netherlands", "Austria", "Slovakia"),
    multiple = TRUE,
    options = list(
      `actions-box` = TRUE,
       style = "btn-info",
      `live-search` = TRUE,
       size = 8),
    )
  
  })
  
  data_corona_all_new_stats <- reactive({
    
    data_res <- copy(data_corona())
    
    data_res[, ('Death rate (%)') := round((Deaths_cumsum / Cases_cumsum) * 100, 2)]

    data_res[, ('Total active cases per 1 million population') := ceiling((Active_cases_cumsum / Population) * 1e6)]
    data_res[, ('Total deaths per 1 million population') := ceiling((Deaths_cumsum / Population) * 1e6)]
    data_res[, ('Total cases per 1 million population') := ceiling((Cases_cumsum / Population) * 1e6)]
    data_res[, ('Total recovered cases per 1 million population') := ceiling((Recovered_cumsum / Population) * 1e6)]
        
    data_res[, ('New cases per 1 million population') := ceiling((Cases / Population) * 1e6)]
    data_res[, ('New deaths per 1 million population') := ceiling((Deaths / Population) * 1e6)]
    data_res[, ('New recovered cases per 1 million population') := ceiling((Recovered / Population) * 1e6)]

    data_res[, TotalTests := NULL]
    data_res[, Tests_1M_Pop := NULL]
    data_res[, Population := NULL]
    data_res[, lat := NULL]
    data_res[, lon := NULL]
    
    data_res
    
  })
  
  # output$checkboxgroup_stats_selector <- renderUI({
  #   
  #   data_res <- copy(data_corona_all_new_stats())
  # 
  #   shinyWidgets::checkboxGroupButtons(
  #     inputId = "stats_selector",
  #     label = "Pick one statistic for comparison:",
  #     choices = colnames(data_res)[-c(1:2)],
  #     selected = 'Total active cases per 1 million population', 
  #     checkIcon = list(
  #       yes = tags$i(class = "fa fa-check-square", 
  #                    style = "color: red"),
  #       no = tags$i(class = "fa fa-square-o", 
  #                   style = "color: red")
  #     )
  #   )
  #   
  # })
  
  # Statistic picker
  output$picker_stats_selector <- renderUI({
    
    data_res <- copy(data_corona_all_new_stats())
    
    shinyWidgets::pickerInput(
      inputId = "stats_selector",
      label = NULL, 
      choices = colnames(data_res)[-c(1:2)],
      selected = 'Total active cases per 1 million population',
      multiple = F,
      options = list(
        # `actions-box` = TRUE,
        style = "btn-danger",
        `live-search` = TRUE,
        size = 8),
    )
    
  })
  
  # Log-scale switch
  output$switch_log_scale_compareTab <- renderUI({
    
    materialSwitch(
      inputId = "log_scale_compareTab",
      label = "Use log scale on Y axis?",
      value = FALSE,
      status = "danger"
    )
    
  })
  
  # my_min <- 1
  # my_max <- 1
  # 
  # observe({
  # 
  #   if (length(input$stats_selector) > my_max) {
  # 
  #     shinyWidgets::updateCheckboxGroupButtons(session,
  #                                              "stats_selector",
  #                                              selected = tail(input$stats_selector, my_max))
  #   }
  # 
  #   # if (length(input$stats_selector) < my_min) {
  #   #
  #   #   shinyWidgets::updateCheckboxGroupButtons(session,
  #   #                                            "stats_selector",
  #   #                                            selected = "a1")
  #   #
  #   # }
  # 
  # })
  
  data_country_stat_selected <- reactive({
    
    shiny::req(input$countries_selector, input$stats_selector)
    
    data_res <- copy(data_corona_all_new_stats())
    
    data_picked <- copy(data_res[.(input$countries_selector),
                                 on = .(Country),
                                 .SD,
                                 .SDcols = c("Country",
                                             "DateRep",
                                             input$stats_selector)
                                 ])
    
    data_picked
    
  })
  
  # Show stats for the selected countries ----
  output$dygraph_countries_stats <- renderDygraph({
    
    shiny::req(input$countries_selector, input$stats_selector)
    
    data_res <- dcast(data_country_stat_selected(),
                      DateRep ~ Country,
                      value.var = input$stats_selector)
    
    dygraph(data_res,
            main = input$stats_selector) %>%
      dyRangeSelector(dateWindow = c(data_res[, max(DateRep) - 20],
                                     data_country()[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                logscale = if(input$log_scale_compareTab) {TRUE} else {NULL},
                colors = RColorBrewer::brewer.pal(ncol(data_res)-2, "Spectral")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5,
                                             pointSize = 4,
                                             fillAlpha = 0.5)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # Since first trajectory graph parameters -----
  
  output$selector_cases_since_first_n <- renderUI({
    
    numericInput(inputId = "cases_since_first_n",
                 label = "Select number of cases:",
                 value = 100,
                 min = 1,
                 max = 1e6,
                 step = 2
                 )
    
  })
  
  output$selector_deaths_since_first_n <- renderUI({
    
    numericInput(inputId = "deaths_since_first_n",
                 label = "Select number of deaths:",
                 value = 20,
                 min = 1,
                 max = 1e6,
                 step = 2
                 )
    
  })
  
  # stats by first 100th case or first 10th death by country -----
  # Cases
  data_country_stat_by_first_cases <- reactive({
    
    shiny::req(input$countries_selector, input$stats_selector, input$cases_since_first_n)
    
    data_res <- copy(data_corona_all_new_stats())
    
    # Cases greater than threshold
    data_res_cases <- copy(data_res[,
                                    .SD[DateRep >= .SD[Cases_cumsum >= input$cases_since_first_n,
                                                       min(DateRep,
                                                           na.rm = T)]],
                                    by = .(Country)])
    
    setorder(data_res_cases,
             Country,
             DateRep)
    
    # Create days column
    data_res_cases[, (paste0("Days_since_first_", input$cases_since_first_n, "_case")) := 1:.N,
                   by = .(Country)]
    
    # subset data based on selected parameters (Countries etc.)
    data_res_cases <- copy(data_res_cases[.(input$countries_selector),
                                          on = .(Country),
                                          .SD,
                                          .SDcols = c("Country",
                                                      paste0("Days_since_first_", input$cases_since_first_n, "_case"),
                                                      input$stats_selector)
                                          ])
    
    data_res_cases

  })
  
  # Deaths
  data_country_stat_by_first_deaths <- reactive({
    
    shiny::req(input$countries_selector, input$stats_selector, input$deaths_since_first_n)
    
    data_res <- copy(data_corona_all_new_stats())
    
    # Deaths greater than threshold
    data_res_deaths <- copy(data_res[,
                                     .SD[DateRep >= .SD[Deaths_cumsum >= input$deaths_since_first_n,
                                                        min(DateRep)]],
                                     by = .(Country)])
    
    setorder(data_res_deaths,
             Country,
             DateRep)
    
    # Create days column
    data_res_deaths[, (paste0("Days_since_first_", input$deaths_since_first_n, "_death")) := 1:.N,
                    by = .(Country)]
    
    # subset data based on selected parameters (Countries etc.)
    data_res_deaths <- copy(data_res_deaths[.(input$countries_selector),
                                            on = .(Country),
                                            .SD,
                                            .SDcols = c("Country",
                                                        paste0("Days_since_first_", input$deaths_since_first_n, "_death"),
                                                        input$stats_selector)
                                            ])
    
    data_res_deaths
    
  })

  # Show stats since first for the selected countries ----
  output$dygraph_countries_stats_since_first <- renderDygraph({
    
    shiny::req(input$countries_selector, input$stats_selector,
               input$cases_since_first_n, input$deaths_since_first_n)

    if (grepl(pattern = "case", x = input$stats_selector) |
        grepl(pattern = "Case", x = input$stats_selector) |
        grepl(pattern = "Recove", x = input$stats_selector)) {
      
      data_res <- dcast(data_country_stat_by_first_cases(),
                        get(paste0("Days_since_first_", input$cases_since_first_n, "_case")) ~ Country,
                        value.var = input$stats_selector)
      
      setnames(data_res, colnames(data_res)[1], paste0("Days_since_first_", input$cases_since_first_n, "_case"))

    } else if (grepl(pattern = "eath", x = input$stats_selector)) {
      
      data_res <- dcast(data_country_stat_by_first_deaths(),
                        get(paste0("Days_since_first_", input$deaths_since_first_n, "_death")) ~ Country,
                        value.var = input$stats_selector)
      
      setnames(data_res, colnames(data_res)[1], paste0("Days_since_first_", input$deaths_since_first_n, "_death"))
      
    }
    
    dygraph(data_res,
            main = paste(input$stats_selector)) %>%
      dyRangeSelector(fillColor = "#5bc0de", strokeColor = "#222d32") %>% # fillColor = "#5bc0de",
      dyAxis("x", label = colnames(data_res)[1]) %>%
      dyOptions(
                # useDataTimezone = TRUE,
                strokeWidth = 2,
                # fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                logscale = if(input$log_scale_compareTab) {TRUE} else {NULL},
                colors = RColorBrewer::brewer.pal(ncol(data_res)-2, "Spectral")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5,
                                             pointSize = 4)
                  ) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  ### Scatter plot + dendogram - clustering comparison of countries -----
  
  # Compute last available stats for countries
  data_countries_lastday_all_stats <- reactive({
    
    data_res_latest <- copy(data_countries_stats())
    
    data_res_latest_stats <- copy(data_res_latest[, .(Country,
                                                      'Total cases' = Cases_cumsum,
                                                      'Total deaths' = Deaths_cumsum,
                                                      'Active cases' = Active_cases_cumsum,
                                                      'Total tests' = TotalTests,
                                                      'New cases' = Cases,
                                                      'New deaths' = Deaths,
                                                      'New cases per 1 million population' = ceiling((Cases / Population) * 1e6),
                                                      'New deaths per 1 million population' = ceiling((Deaths / Population) * 1e6),
                                                      'New recovered cases per 1 million population' = ceiling((Recovered / Population) * 1e6),
                                                      'Death rate (%)' = round((Deaths_cumsum / Cases_cumsum) * 100, 2),
                                                      'Positive tests rate (%)' = round((Cases_cumsum / TotalTests) * 100, 2),
                                                      'Total active cases per 1 million population' = ceiling((Active_cases_cumsum / Population) * 1e6),
                                                      'Total deaths per 1 million population' = ceiling((Deaths_cumsum / Population) * 1e6),
                                                      'Total cases per 1 million population' = ceiling((Cases_cumsum / Population) * 1e6),
                                                      'Total recovered cases per 1 million population' = ceiling((Recovered_cumsum / Population) * 1e6),
                                                      'Total tests per 1 million population' = Tests_1M_Pop,
                                                      Population
                                                      )
                                                  ]
                                  )
    
    data_res_latest_stats

  })
  
  # multivariate analysis of stats and countries -----
  
  output$picker_multiple_stats_clust <- renderUI({
    
    data_res <- copy(data_countries_lastday_all_stats())
    
    shinyWidgets::pickerInput(
      inputId = "multiple_stats_clust",
      label = NULL, 
      choices = colnames(data_res)[-1],
      selected = c('Total active cases per 1 million population',
                   'New cases per 1 million population',
                   'New deaths per 1 million population',
                   'Positive tests rate (%)',
                   'Total deaths per 1 million population'
                   ),
      multiple = T,
      options = list(
        `actions-box` = TRUE,
        `multiple-separator` = " \n ",
        style = "btn-info",
        `live-search` = TRUE,
        size = 8),
      )
    
  })
  
  output$picker_sort_column <- renderUI({
    
    shiny::req(input$multiple_stats_clust)
    
    pickers <- input$multiple_stats_clust
    
    shinyWidgets::pickerInput(
      inputId = "sort_column",
      label = "Data will be sorted by this statistic:",
      choices = pickers,
      selected = pickers[1],
      multiple = F,
      options = list(
        # `actions-box` = TRUE,
        # `multiple-separator` = " \n ",
        style = "btn-info",
        `live-search` = TRUE,
        size = 5),
    )
    
  })
  
  # select top N countries from active cases
  
  output$selector_top_n_countries_multi <- renderUI({
    
    shiny::req(input$sort_column)
    
    data_res <- copy(data_countries_lastday_all_stats()[Population > 1e6])
    
    numericInput(inputId = "top_n_countries_multi",
                 label = paste0("Select number of top N countries from ", input$sort_column, ":"),
                 value = 48,
                 min = 1,
                 max = data_res[, .N],
                 step = 2
                 )
    
  })
  
  # number of clusters
  output$selector_n_clusters_multi <- renderUI({
    
    data_res <- copy(data_countries_lastday_all_stats()[Population > 1e6])
    
    numericInput(inputId = "n_clusters_multi",
                 label = "Select number of clusters:",
                 value = 8,
                 min = 2,
                 max = data_res[, .N] - 1,
                 step = 1
                 )
    
  })
  
  # Selected data for 2d/MDS scatter plot ----
  data_mds_scatterplot_selected <- reactive({
    
    shiny::req(input$multiple_stats_clust, input$sort_column, input$top_n_countries_multi)
    
    data_res <- copy(data_countries_lastday_all_stats()[Population > 1e6])
    
    setorderv(data_res, input$sort_column, -1)
    
    data_res <- copy(data_res[, .SD,
                              .SDcols = c("Country",
                                          input$multiple_stats_clust)
                              ])
    
    data_res_subset <- copy(na.omit(data_res[1:input$top_n_countries_multi]))
    
    if (sum(grepl(pattern = "Slovakia", x = data_res_subset$Country)) == 0) {
      
      data_res_subset <- rbindlist(list(data_res_subset,
                                        data_res[.("Slovakia"), on = .(Country)]
                                        )
                                   )
      
    }
    
    data_res_subset
    
  })
  
  # Clustering criterion
  output$dropdown_clustering_crit <- renderUI({
    
    shinyWidgets::dropdown(
      
      # tags$p("Choose criterion for hierarchical clustering:"),
      
      shinyWidgets::pickerInput(inputId = "clust_crit",
                                label = "Choose criterion for hierarchical clustering:",
                                choices = c("ward.D", "ward.D2", "single", "complete", "average"),
                                selected = "ward.D2",
                                multiple = F,
                                options = list(
                                  style = "btn-success",
                                  size = 3
                                  )
                                ),
      
      style = "unite", icon = icon("gear"),
      status = "success", width = "300px",
      animate = shinyWidgets::animateOptions(
        enter = animations$fading_entrances$fadeInLeftBig,
        exit = animations$fading_exits$fadeOutRightBig
      ),
      tooltip = tooltipOptions(title = "Click to choose other clustering criterion!")
    )
    
  })
  
  # clustering results ----
  clustering_results <- reactive({
    
    data_res <- copy(data_mds_scatterplot_selected())
    
    shiny::req(input$n_clusters_multi, input$clust_crit)
    
    k <- input$n_clusters_multi
    
    clust_res <- hie_clus(data = data_res,
                          k = k,
                          cols = input$multiple_stats_clust,
                          crit = input$clust_crit)
    
    clust_res
    
  })
  
  # Plot scatter plot 2D or 2D MDS ----
  
  output$plot_scatterplot_mds_country_stats <- renderPlot({
    
    data_res <- copy(data_mds_scatterplot_selected())
    
    clust_res <- clustering_results()
    
    data_res[, Cluster := clust_res$clustering$Cluster]
    
    # print(data_res)
    
    theme_my <- theme(panel.border = element_rect(fill = NA,
                                                  colour = "grey10"),
                      panel.background = element_blank(),
                      panel.grid.minor = element_line(colour = "grey85"),
                      panel.grid.major = element_line(colour = "grey85"),
                      panel.grid.major.x = element_line(colour = "grey85"),
                      axis.text = element_text(size = 13, face = "bold"),
                      axis.title = element_text(size = 14, face = "bold"),
                      plot.title = element_text(size = 16, face = "bold"),
                      strip.text = element_text(size = 16, face = "bold"),
                      strip.background = element_rect(colour = "black"),
                      legend.text = element_text(size = 15),
                      legend.title = element_text(size = 16, face = "bold"),
                      legend.background = element_rect(fill = "white"),
                      legend.key = element_rect(fill = "white"),
                      legend.position="bottom"
                      )
    
    if (length(input$multiple_stats_clust) == 2) {
      
      gg_scatter <- ggplot(data_res, aes(x = get(input$multiple_stats_clust[1]),
                                         y = get(input$multiple_stats_clust[2]),
                                         label = Country,
                                         color = as.factor(Cluster))) +
        geom_label_repel(alpha = 0.95,
                         segment.alpha = 0.35,
                         label.r = 0.1,
                         box.padding = 0.25,
                         label.padding = 0.3,
                         label.size = 0.35,
                         max.iter = 2500
                         ) +
        scale_color_manual(values = clust_res$clusters_colors$Color) +
        labs(x = input$multiple_stats_clust[1],
             y = input$multiple_stats_clust[2]) +
        guides(color = FALSE) +
        theme_my
      
      gg_scatter
      
    } else if (length(input$multiple_stats_clust) > 2) {
      
      d <- dist(scale(data.matrix(data_res[, .SD,
                                           .SDcols = c(input$multiple_stats_clust)
                                           ]),
                      center = T, scale = T)) #
      
      mds_classical <- cmdscale(d, eig = FALSE, k = 2) # very slow, be aware!
      # ds_nonmetric <- isoMDS(d, k = 2)$points
      
      data_plot <- data.table(mds_classical,
                              Country = data_res$Country,
                              Cluster = data_res$Cluster)
      
      gg_scatter <- ggplot(data_plot, aes(x = get("V1"),
                                          y = get("V2"),
                                          label = Country,
                                          color = as.factor(Cluster)
                                          )
                           ) +
        geom_label_repel(
                        alpha = 0.95,
                        segment.alpha = 0.35,
                        label.r = 0.1,
                        box.padding = 0.25,
                        label.padding = 0.3,
                        label.size = 0.35,
                        max.iter = 2500) +
        scale_color_manual(values = clust_res$clusters_colors$Color) +
        labs(x = NULL,
             y = NULL,
             color = NULL) +
        guides(color = FALSE) +
        theme_my
      
      gg_scatter
      
      
      # fig <- plot_ly(data_plot,
      #                x = ~get("V1"),
      #                y = ~get("V2"),
      #                type = 'scatter',
      #                mode = 'text',
      #                text = ~Country,
      #                alpha = 0.6,
      #                textposition = 'middle right',
      #                textfont = list(color = '#000000', size = 14)
      # )
      # 
      # fig <- fig %>% layout(
      #   xaxis = list(title = NA,
      #                zeroline = F,
      #                range = c(data_plot[, min(V1)*1.01],
      #                          data_plot[, max(V1)*1.8])
      #   ),
      #   yaxis = list(title = NA,
      #                zeroline = F,
      #                range = c(data_plot[, min(V2)*1.15],
      #                          data_plot[, max(V2)*1.15])
      #   )
      # )
      
      # fig
      
    }
    
  })
  
  # Plot clustering dendogram ----
  
  output$clust_res_multidim <- renderPlot({
    
    shiny::req(input$n_clusters_multi)
    
    data_res <- copy(data_mds_scatterplot_selected())
    
    k <- input$n_clusters_multi

    clust_res <- clustering_results()$clust_obj
    
    data_res[, Old_order := 1:.N]
    
    dt_order <- data.table(Old_order = clust_res$order,
                           New_order = 1:length(clust_res$order)
                           )
    
    dt_order[data_res, on = .(Old_order), Country := i.Country]
    setorder(dt_order, New_order)
    
    # print(data_res)
    
    dend <- as.dendrogram(clust_res)
    
    dend <- dend %>%
      color_branches(k = k) %>%
      color_labels(k = k) %>%
      set("branches_lwd", 1) %>% 
      set("labels", dt_order[, Country]) %>%
      set("labels_cex", 0.9)
    
    # always show top countries ot the top of dendogram
    where_is_first <- which(clust_res$order == 1)
    
    if (where_is_first >= length(clust_res$order)/2) {
      
      ggd1 <- as.ggdend(dend)
      
    } else {
      
      ggd1 <- as.ggdend(rev(dend))
      
    }
    
    gg_dendo <- ggplot(ggd1,
                       horiz = TRUE)
    
    gg_dendo
    
  })
  
  # DT of clusters averages -----
  
  # data preprocessing and merging
  data_clusters_averages <- reactive({
    
    data_res <- copy(data_mds_scatterplot_selected())
    
    clust_res <- clustering_results()
    
    data_res[, Cluster := clust_res$clustering$Cluster]
    
    data_res_ave <- copy(data_res[,
                                  as.vector(lapply(lapply(.SD, mean, na.rm = T), round, 2)),
                                  by = .(Cluster),
                                  .SDcols = input$multiple_stats_clust
                                  ]
                         ) # Compute averages of clusters

    data_res_ave[data_res[,
                          .(Countries = paste0(Country, collapse = ", ")),
                          by = .(Cluster)],
                 on = .(Cluster),
                 Countries := i.Countries] # List of countries in clusters
    
    data_res_ave[data_res[,
                          .(N_Countries = .N),
                          by = .(Cluster)],
                 on = .(Cluster),
                 N_Countries := i.N_Countries] # Number of countries in clusters
    
    data_res_ave <- copy(data_res_ave[, .SD,
                                      .SDcols = c("Cluster", "N_Countries", "Countries",
                                                  input$multiple_stats_clust)]) # Order columns
    
    setorder(data_res_ave, Cluster)
    
    data_res_ave
    
  })
  
  # DT
  output$dt_clusters_averages <- renderDataTable({
    
    data_res <- copy(data_clusters_averages())
    
    clust_res <- clustering_results()

    DT::datatable(data_res,
    selection = "none",
    class = "compact",
    # rownames = FALSE,
    extensions = c('Buttons', 'Scroller'),
    options = list(
      pageLength = nrow(data_res),
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 250,
      scroller = TRUE,
      buttons = c('csv', 'excel'),
      scrollX = TRUE,
      columnDefs = list(list(
        targets = 3,
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 15 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
          "}")
      ))
    ), callback = JS('table.page(1).draw(false);')) %>% formatStyle(
      'Cluster',
      backgroundColor = styleEqual(data_res[, Cluster], clust_res$clusters_colors$Color)
    )
  })
  
  ### Clustering countries' trajectories with DTW distance ------

  # data with statistics
  data_corona_all_time_series <- reactive({
    
    data_res <- copy(data_corona())
    
    data_res <- copy(data_res[Population > 8e5])
    
    data_res[, ('New cases per 1 million population') := ceiling((Cases / Population) * 1e6)]
    data_res[, ('New deaths per 1 million population') := ceiling((Deaths / Population) * 1e6)]
    data_res[, ('New recovered cases per 1 million population') := ceiling((Recovered / Population) * 1e6)]
    
    data_res[, ('Death rate (%)') := round((Deaths_cumsum / Cases_cumsum) * 100, 2)]
    
    data_res[, ('Total active cases per 1 million population') := ceiling((Active_cases_cumsum / Population) * 1e6)]
    data_res[, ('Total deaths per 1 million population') := ceiling((Deaths_cumsum / Population) * 1e6)]
    data_res[, ('Total cases per 1 million population') := ceiling((Cases_cumsum / Population) * 1e6)]
    data_res[, ('Total recovered cases per 1 million population') := ceiling((Recovered_cumsum / Population) * 1e6)]
    
    data_res[, Population := NULL]
    data_res[, TotalTests := NULL]
    data_res[, Tests_1M_Pop := NULL]
    data_res[, lat := NULL]
    data_res[, lon := NULL]
    
    data_res
    
  })
  
  
  # Select statistic for clustering
  output$picker_stat_selector_clust <- renderUI({
    
    data_res <- copy(data_corona_all_time_series())
    
    shinyWidgets::pickerInput(
      inputId = "stat_selector_clust",
      label = NULL, 
      choices = colnames(data_res)[-c(1:2)],
      selected = 'Total active cases per 1 million population',
      multiple = F,
      options = list(
        # `actions-box` = TRUE,
        style = "btn-info",
        `live-search` = TRUE,
        size = 8),
      )
    
  })
  
  # Since first trajectory clustering parameters -----
  output$selector_cases_since_first_n_clust <- renderUI({
    
    numericInput(inputId = "cases_since_first_n_clust",
                 label = "Select number of since first Nth case:",
                 value = 100,
                 min = 1,
                 max = 1e6,
                 step = 2
                )
    
  })
  
  output$selector_deaths_since_first_n_clust <- renderUI({
    
    numericInput(inputId = "deaths_since_first_n_clust",
                 label = "Select number of since first Nth death:",
                 value = 20,
                 min = 1,
                 max = 1e6,
                 step = 2
                )
    
  })
  
  # select top N countries from selected stat for clustering ----
  output$selector_top_n_countries_clust <- renderUI({
    
    shiny::req(input$stat_selector_clust)
    
    data_res <- copy(data_corona_all_time_series())
    
    numericInput(inputId = "top_n_countries_clust",
                 label = paste0("Select number of top N countries from ", input$stat_selector_clust, ":"),
                 value = 56,
                 min = 1,
                 max = data_res[, uniqueN(Country)],
                 step = 2
                 )
    
  })
  
  # number of clusters
  output$selector_n_clusters_dtw <- renderUI({
    
    # data_res <- copy(data_countries_lastday_all_stats()[Population > 1e6])
    
    numericInput(inputId = "n_clusters_dtw",
                 label = "Select number of clusters:",
                 value = 10,
                 min = 2,
                 max = 40,
                 step = 1
                )
    
  })
  
  # stats by first 100th case or first 10th death by country -----
  # Cases
  data_country_stat_by_first_cases_clust <- reactive({
    
    shiny::req(input$stat_selector_clust, input$top_n_countries_clust, input$cases_since_first_n_clust)
    
    data_res <- copy(data_corona_all_time_series())
    
    # Cases greater than threshold
    data_res_cases <- copy(data_res[,
                                    .SD[DateRep >= .SD[Cases_cumsum >= input$cases_since_first_n_clust,
                                                       min(DateRep,
                                                           na.rm = T)]],
                                    by = .(Country)])
    
    setorder(data_res_cases,
             Country,
             DateRep)
    
    # Create days column
    data_res_cases[, (paste0("Days_since_first_",
                             input$cases_since_first_n_clust, "_case")) := 1:.N,
                   by = .(Country)]
    
    # print(data_res_cases)
    
    # top N countries selection
    data_res_order <- copy(data_res_cases[,
                                          .SD[DateRep == max(DateRep),
                                              get(input$stat_selector_clust)],
                                          by = .(Country)
                                          ])
    
    setnames(data_res_order, "V1", input$stat_selector_clust)
    
    # print(data_res_order)
    
    setorderv(data_res_order, input$stat_selector_clust, -1)
    
    # subset data based on selected parameters (Countries etc.)
    data_res_cases_sub <- copy(data_res_cases[.(data_res_order[1:input$top_n_countries_clust][!is.na(Country), Country]),
                                          on = .(Country),
                                          .SD,
                                          .SDcols = c("Country",
                                                      paste0("Days_since_first_", input$cases_since_first_n_clust, "_case"),
                                                      input$stat_selector_clust)
                                          ])
    
    if (sum(grepl(pattern = "Slovakia", x = data_res_cases_sub[, unique(Country)])) == 0) {
      
      data_res_cases_sub <- rbindlist(list(data_res_cases_sub,
                                           data_res_cases[.("Slovakia"),
                                                          on = .(Country),
                                                          .SD,
                                                          .SDcols = c("Country",
                                                                      paste0("Days_since_first_", input$cases_since_first_n_clust, "_case"),
                                                                      input$stat_selector_clust)
                                                          ])
                                      )
      
    }
    
    # Make same length time series from countries data
    data_res_trajectories <- dcast(data_res_cases_sub,
                                   get(paste0("Days_since_first_", input$cases_since_first_n_clust, "_case")) ~ Country,
                                   value.var = input$stat_selector_clust)
    
    setnames(data_res_trajectories,
             colnames(data_res_trajectories)[1],
             paste0("Days_since_first_", input$cases_since_first_n_clust, "_case"))
    
    data_res_trajectories
    
  })
  
  # Deaths
  data_country_stat_by_first_deaths_clust <- reactive({
    
    shiny::req(input$stat_selector_clust, input$top_n_countries_clust, input$deaths_since_first_n_clust)
    
    data_res <- copy(data_corona_all_time_series())
    
    # Deaths greater than threshold
    data_res_deaths <- copy(data_res[,
                                     .SD[DateRep >= .SD[Deaths_cumsum >= input$deaths_since_first_n_clust,
                                                        min(DateRep)]],
                                     by = .(Country)])
    
    setorder(data_res_deaths,
             Country,
             DateRep)
    
    # Create days column
    data_res_deaths[, (paste0("Days_since_first_", input$deaths_since_first_n_clust, "_death")) := 1:.N,
                    by = .(Country)]
    
    # top N countries selection
    data_res_order <- copy(data_res_deaths[,
                                           .SD[DateRep == max(DateRep),
                                               get(input$stat_selector_clust)],
                                           by = .(Country)
                                           ])
    
    setnames(data_res_order, "V1", input$stat_selector_clust)
    
    setorderv(data_res_order, input$stat_selector_clust, -1)
    
    # subset data based on selected parameters (Countries etc.)
    data_res_deaths_sub <- copy(data_res_deaths[.(data_res_order[1:input$top_n_countries_clust][!is.na(Country), Country]),
                                            on = .(Country),
                                            .SD,
                                            .SDcols = c("Country",
                                                        paste0("Days_since_first_", input$deaths_since_first_n_clust, "_death"),
                                                        input$stat_selector_clust)
                                            ])
    
    if (sum(grepl(pattern = "Slovakia", x = data_res_deaths_sub[, unique(Country)])) == 0) {
      
      data_res_deaths_sub <- rbindlist(list(data_res_deaths_sub,
                                           data_res_deaths[.("Slovakia"),
                                                          on = .(Country),
                                                          .SD,
                                                          .SDcols = c("Country",
                                                                      paste0("Days_since_first_", input$deaths_since_first_n_clust, "_death"),
                                                                      input$stat_selector_clust)
                                                          ])
      )
      
    }
    
    # Make same length time series from countries data
    data_res_trajectories <- dcast(data_res_deaths_sub,
                                   get(paste0("Days_since_first_", input$deaths_since_first_n_clust, "_death")) ~ Country,
                                   value.var = input$stat_selector_clust)
    
    setnames(data_res_trajectories,
             colnames(data_res_trajectories)[1],
             paste0("Days_since_first_", input$deaths_since_first_n_clust, "_death")
             )
    
    data_res_trajectories
    
  })
  
  # SMA order input ----
  output$selector_sma_order <- renderUI({
    
    numericInput(inputId = "sma_order",
                 label = "Select order of Simple Moving Average (SMA):",
                 value = 3,
                 min = 1,
                 max = 7,
                 step = 1,
                 width = "50%"
                 )
    
  })
  
  # Normalization switch
  output$switch_normalization <- renderUI({
    
    materialSwitch(
      inputId = "normalization",
      label = "Normalize each country before clustering by z-score?", 
      value = FALSE,
      status = "info"
      )
    
  })
  
  # Log-scale switch
  output$switch_log_scale <- renderUI({
    
    materialSwitch(
      inputId = "log_scale",
      label = "Use log scale on Y axis?",
      value = FALSE,
      status = "info"
      )
    
  })
  
  # Prepare trajectories' data for clustering ----
  data_for_clustering_trajectories <- reactive({
    
    shiny::req(input$stat_selector_clust, input$sma_order)
    
    if (grepl(pattern = "case", x = input$stat_selector_clust) |
        grepl(pattern = "Case", x = input$stat_selector_clust) |
        grepl(pattern = "Recove", x = input$stat_selector_clust)) {
      
      data_res <- copy(data_country_stat_by_first_cases_clust())
      
      
    } else if (grepl(pattern = "eath", x = input$stat_selector_clust)) {
      
      data_res <- copy(data_country_stat_by_first_deaths_clust())
      
    }
    
    # save stats of dt
    n_col <- ncol(data_res)
    n_row <- nrow(data_res)
    
    n_row_na <- rowSums(data_res[, lapply(.SD, is.na)])
    n_col_na <- colSums(data_res[, lapply(.SD, is.na)])
    
    # remove all NA rows and cols
    if (length(which(n_row_na %in% n_col)) != 0) {
      
      data_res <- copy(data_res[-which(n_row_na == n_col)])
      
    }
    
    if (length(which(n_col_na %in% n_row)) != 0) {
      
      data_res <- copy(data_res[, -which(n_col_na %in% n_row), with = FALSE])
      
    }

    # Compute SMA for every Country
    data_res[,
             (colnames(data_res)[-1]) := lapply(.SD, function(i) c(rep(NA, input$sma_order - 1),
                                                                   repr_sma(i, input$sma_order)
                                                                   )
                                                ),
             .SDcols = colnames(data_res)[-1]]

    data_res
    
  })
  
  # Clustering trajectories ----
  clustering_result_trajectories <- reactive({
    
    shiny::req(input$n_clusters_dtw)
    
    data_res <- copy(data_for_clustering_trajectories())
    
    clust_res <- cluster_trajectories(data_res, input$n_clusters_dtw, input$normalization)
    
    clust_res
    
  })
  
  # Select country for info in which cluster is in it -----
  output$picker_country_clust <- renderUI({
    
    clust_res <- copy(clustering_result_trajectories())
    
    data_clust_id <- data.table(Cluster = clust_res@cluster,
                                Country = names(clust_res@cluster))
    
    shinyWidgets::pickerInput(
      inputId = "country_clust",
      label = "In which cluster is your prefered country? Pick one:",
      choices = data_clust_id[, Country],
      selected = 'Slovakia',
      multiple = F,
      options = list(
        # `actions-box` = TRUE,
        style = "btn-info",
        `live-search` = TRUE,
        size = 8),
    )
    
  })
  
  # Info text
  output$text_inwhich_cluster_country <- renderUI({
    
      clust_res <- copy(clustering_result_trajectories())

      data_clust_id <- data.table(Cluster = clust_res@cluster,
                                  Country = names(clust_res@cluster))

      shiny::req(input$country_clust)
    
      tags$html(tags$h3(tags$b(paste0("Cluster n.: ", data_clust_id[.(input$country_clust),
                                                                 on = .(Country),
                                                                 Cluster]))
                       )
               )
    
  })
  
  # output$infobox_inwhich_cluster_country <- renderInfoBox({
  #   
  #   clust_res <- copy(clustering_result_trajectories())
  #   
  #   data_clust_id <- data.table(Cluster = clust_res@cluster,
  #                               Country = names(clust_res@cluster))
  #   
  #   shiny::req(input$country_clust)
  #   
  #   infoBox(
  #     "Cluster: ", paste0(data_clust_id[.(input$country_clust),
  #                                       on = .(Country),
  #                                       Cluster]),
  #     icon = icon("list"),
  #     color = "light-blue"
  #     )
  #   
  # })
  
  # Data for plotting clustered trajectories
  data_plot_clusters_trajectories <- reactive({
    
    data_res <- copy(data_for_clustering_trajectories())
    
    clust_res <- clustering_result_trajectories()
    
    # prepare time series
    data_clust_id <- data.table(Cluster = clust_res@cluster,
                                Country = names(clust_res@cluster))
    
    data_plot <- melt(data_res,
                      id.vars = colnames(data_res)[1],
                      variable.name = "Country",
                      variable.factor = FALSE,
                      value.name = input$stat_selector_clust,
                      value.factor = FALSE
                      )
    
    data_plot <- copy(data_plot[.(data_clust_id$Country), on = .(Country)])
    
    data_plot[data_clust_id,
              on = .(Country),
              Cluster := i.Cluster]
    
    if (!input$normalization) {

      centroids <- lapply(1:length(clust_res@centroids), function(i) c(rep(NA, input$sma_order - 1),
                                                                       clust_res@centroids[[i]])
                          )
      
      # prepare centroids
      centers <- as.data.table(reshape2::melt(centroids))
      setnames(centers, "L1", "Cluster")
      centers[, (colnames(data_res)[1]) := 1:.N,
              by = .(Cluster)]
      setnames(centers, "value", input$stat_selector_clust)
      centers[, Country := as.character(Cluster)]
      
    }
    
    # Set colors for clusters
    data_clust_colors <- data.table(Cluster = 1:max(clust_res@cluster),
                                    # Color = RColorBrewer::brewer.pal(k, name = "Set2")
                                    Color = colorspace::rainbow_hcl(max(clust_res@cluster), c = 90, l = 50)
                                    )
    
    list(data = data_plot,
         centers = if (!input$normalization) { centers } else { NULL },
         colors = data_clust_colors
         )
    
  })
  
  # Plot of clusters members -----
  output$plot_clusters_trajectories <- renderPlot({
    
    data_clust_res <- data_plot_clusters_trajectories()
    
    theme_my <- theme(panel.border = element_rect(fill = NA,
                                                  colour = "grey10"),
                      panel.background = element_blank(),
                      panel.grid.minor = element_line(colour = "grey85"),
                      panel.grid.major = element_line(colour = "grey85"),
                      panel.grid.major.x = element_line(colour = "grey85"),
                      axis.text = element_text(size = 13, face = "bold"),
                      axis.title = element_text(size = 14, face = "bold"),
                      plot.title = element_text(size = 16, face = "bold"),
                      strip.text = element_text(size = 16, face = "bold"),
                      strip.background = element_rect(colour = "black"),
                      legend.text = element_text(size = 15),
                      legend.title = element_text(size = 16, face = "bold"),
                      legend.background = element_rect(fill = "white"),
                      legend.key = element_rect(fill = "white"),
                      legend.position="bottom")
    
    if (!input$normalization) {
      
      # plot the results
      gg <- ggplot(data_clust_res$data,
             aes(get(colnames(data_clust_res$data)[1]),
                 get(input$stat_selector_clust),
                 group = Country)) +
        facet_wrap(~Cluster,
                   ncol = ceiling(data_clust_res$data[, sqrt(uniqueN(Cluster))]),
                   scales = "free") +
        geom_line(color = "grey10", alpha = 0.75, size = 0.6) +
        geom_line(data = data_clust_res$centers,
                  aes(get(colnames(data_clust_res$data)[1]),
                      get(input$stat_selector_clust),
                      color = as.factor(Cluster)),
                  # color = "firebrick1",
                  alpha = 0.95, size = 1.4, linetype = "longdash") +
        scale_color_manual(values = data_clust_res$colors$Color) +
        labs(x = colnames(data_clust_res$data)[1],
             y = input$stat_selector_clust) +
        guides(color = FALSE) +
        theme_my
      
      if (input$log_scale) gg <- gg + scale_y_continuous(trans = 'log10')
      
    } else {
      
      # plot the results
      gg <- ggplot(data_clust_res$data,
             aes(get(colnames(data_clust_res$data)[1]),
                 get(input$stat_selector_clust),
                 group = Country)) +
        facet_wrap(~Cluster,
                   ncol = ceiling(data_clust_res$data[, sqrt(uniqueN(Cluster))]),
                   scales = "free") +
        geom_line(color = "grey10", alpha = 0.75, size = 0.6) +
        scale_color_manual(values = data_clust_res$colors$Color) +
        labs(x = colnames(data_clust_res$data)[1],
             y = input$stat_selector_clust) +
        guides(color = FALSE) +
        theme_my
      
      if (input$log_scale) gg <- gg + scale_y_continuous(trans = 'log10')
      
      
    }
    
    gg
    
  })
  
  # Focus plotly graph of defined cluster -----
  output$picker_cluster_focus <- renderUI({

    clust_res <- copy(clustering_result_trajectories())
    
    data_clust_id <- data.table(Cluster = clust_res@cluster,
                                Country = names(clust_res@cluster))
    
    shiny::req(input$n_clusters_dtw, input$country_clust)

    shinyWidgets::pickerInput(inputId = 'cluster_focus',
                              label = 'Choose cluster for visualisation:',
                              choices = c(1:input$n_clusters_dtw),
                              selected = data_clust_id[.(input$country_clust),
                                                       on = .(Country),
                                                       Cluster],
                              options = list(`style` = "btn-primary",
                                             `live-search` = TRUE,
                                              size = 5)
                              )
  })
  
  output$plotly_focus_cluster <- renderPlotly({
    
    shiny::req(input$cluster_focus)
    
    data_clust_res <- data_plot_clusters_trajectories()
    
    k <- input$cluster_focus
    
    theme_my <- theme(panel.border = element_rect(fill = NA,
                                                  colour = "grey10"),
                      panel.background = element_blank(),
                      panel.grid.minor = element_line(colour = "grey85"),
                      panel.grid.major = element_line(colour = "grey85"),
                      panel.grid.major.x = element_line(colour = "grey85"),
                      axis.text = element_text(size = 10, face = "bold"),
                      axis.title = element_text(size = 11, face = "bold"),
                      plot.title = element_text(size = 14, face = "bold")
                      )
    
    if (!input$normalization) {
      
      gg_clust <- ggplot(data_clust_res$data[Cluster == k],
                         aes(get(colnames(data_clust_res$data)[1]),
                             get(input$stat_selector_clust),
                             group = Country,
                             text = paste('</br>', colnames(data_clust_res$data)[1], ": ", get(colnames(data_clust_res$data)[1]),
                                          '</br>', input$stat_selector_clust, ": ", get(input$stat_selector_clust),
                                          '</br>Country: ', Country, sep = "")
                         )) +
        geom_line(data = data_clust_res$centers[Cluster == k],
                  aes(get(colnames(data_clust_res$data)[1]),
                      get(input$stat_selector_clust)
                  ),
                  linetype = "longdash", color = data_clust_res$colors[Cluster == k, Color],
                  alpha = 0.95, size = 1.2) +
        geom_line(color = "grey10", alpha = 0.75, size = 0.5) +
        labs(title = paste0("Cluster: ", k),
             x = colnames(data_clust_res$data)[1],
             y = input$stat_selector_clust) +
        theme_my
      
      if (input$log_scale) gg_clust <- gg_clust + scale_y_continuous(trans = 'log10')
      
    } else {
      
      gg_clust <- ggplot(data_clust_res$data[Cluster == k],
                         aes(get(colnames(data_clust_res$data)[1]),
                             get(input$stat_selector_clust),
                             group = Country,
                             text = paste('</br>', colnames(data_clust_res$data)[1], ": ", get(colnames(data_clust_res$data)[1]),
                                          '</br>', input$stat_selector_clust, ": ", get(input$stat_selector_clust),
                                          '</br>Country: ', Country, sep = "")
                         )) +
        geom_line(color = "grey10", alpha = 0.75, size = 0.5) +
        labs(title = paste0("Cluster: ", k),
             x = colnames(data_clust_res$data)[1],
             y = input$stat_selector_clust) +
        theme_my
      
      if (input$log_scale) gg_clust <- gg_clust + scale_y_continuous(trans = 'log10')
      
    }
    
    ggplotly(gg_clust, tooltip = "text")
    
  })
  
  # Dendogram of clustered trajectories ----
  output$plot_clusters_trajectories_dendogram <- renderPlot({
    
    clust_res <- clustering_result_trajectories()
    
    dend <- as.dendrogram(clust_res)

    dend <- dend %>%
      color_branches(k = input$n_clusters_dtw) %>%
      color_labels(k = input$n_clusters_dtw) %>%
      set("branches_lwd", 1) %>%
      # set("labels", dt_order[, Country]) %>%
      set("labels_cex", 0.9)

    ggd1 <- as.ggdend(dend)

    gg_dendo <- ggplot(ggd1,
                       horiz = T)

    gg_dendo
    
  })
  
  output$plot_scatter_mds_trajectories <- renderPlot({
    
    clust_res <- clustering_result_trajectories()

    mds_classical <- cmdscale(clust_res@distmat, eig = FALSE, k = 2) # very slow, be aware!
    # ds_nonmetric <- isoMDS(d, k = 2)$points

    data_plot <- data.table(mds_classical,
                            Country = row.names(mds_classical),
                            Cluster = clust_res@cluster
                            )

    gg_scatter <- ggplot(data_plot, aes(x = get("V1"),
                                        y = get("V2"),
                                        label = Country,
                                        color = as.factor(Cluster)
                                        )
                         ) +
      geom_label_repel(
        alpha = 0.95,
        segment.alpha = 0.35,
        label.r = 0.1,
        box.padding = 0.25,
        label.padding = 0.3,
        label.size = 0.35,
        max.iter = 2500) +
      scale_color_manual(values = colorspace::rainbow_hcl(input$n_clusters_dtw,
                                                          c = 90,
                                                          l = 50)) +
      labs(x = NULL,
           y = NULL,
           color = NULL) +
      guides(color = FALSE) +
      theme_bw()

    gg_scatter
    
  })
  
}
