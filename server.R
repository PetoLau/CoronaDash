## app.R ##
function(input, output, session) {
  
  # Load scripts/modules -----
  
  # reading data
  # source("01_scripts/read_data.R")
  source("01_scripts/read_data_cssegis.R")
  source("01_scripts/aggregate_data.R")
  source("01_scripts/read_populations.R")
  
  # forecasting
  source("01_scripts/forecasting.R")
 
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
    
    print(query1)
    
    if (query1 == "tab=compareTab") {
      
      updateTabItems(session, inputId = "sideBar_Menu", selected = "compareTab")
      
    } else if (query1 == "tab=worldTab") {
      
      updateTabItems(session, inputId = "sideBar_Menu", selected = "worldTab")
      
    } else if (query1 == "tab=analysisTab") {
      
      updateTabItems(session, inputId = "sideBar_Menu", selected = "analysisTab")
      
    }
    
  })
  
  # informative text for this app -----
  output$informative_text <- renderUI({
    
    tags$html(tags$p("This application is only for informative purposes,
                     how the COVID-19 virus can spread over time for a defined country and period of days (confirmed cases).
                     There isn't motivation to replace more sophisticated epidemiology models like SIR."),
              tags$p("Data are coming from",
                     tags$a(href = 'https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series',
                            target="_blank", "Johns Hopkins CSSE GitHub repository"),
                     "and",
                     tags$a(href = 'https://github.com/ulklc/covid19-timeseries',
                            target="_blank", "GitHub repository by ulklc.")),
              tags$p("The forecasting model is the ETS (Exponential smoothing) implemented in a smooth R package,
                      so only historical data of target time series are used (extrapolation).
                      For total cumulative confirmed cases, the multiplicative model is used.
                      For total cumulative death cases of the World, the fully multiplicative model is used
                     (it is the possibility of using a damped trend in both situations)."),
              tags$p(
                tags$a("You can compare multiple countries' trajectories for various statistics in the Compare countries tab,",
                       onclick = "openTab('compareTab')", href="#"),
                tags$a("and clustering of countries viewed on scatter plots and dendograms in the Countries analysis tab.",
                       onclick = "openTab('analysisTab')", href="#"),
                tags$a("You can also check the aggregated World cases + forecasts in the COVID-19 World agg. tab.",
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
      paste0(data_country()[.N,
                             Tests_1M_Pop]),
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
            main = paste(paste(input$stats_selector, collapse = ","),
                         " in ",
                         paste(input$countries_selector, collapse = ","),
                         collapse = " ")) %>%
      dyRangeSelector(dateWindow = c(data_res[, max(DateRep) - 20],
                                     data_country()[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = RColorBrewer::brewer.pal(ncol(data_res)-2, "Spectral")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5,
                                             pointSize = 4,
                                             fillAlpha = 0.5)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # 
  
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
  data_country_stat_by_first_cases <- reactive({
    
    shiny::req(input$countries_selector, input$stats_selector, input$cases_since_first_n)
    
    data_res <- copy(data_corona_all_new_stats())
    
    data_res_cases <- copy(data_res[,
                                    .SD[DateRep >= .SD[Cases_cumsum >= input$cases_since_first_n,
                                                       min(DateRep,
                                                           na.rm = T)]],
                                    by = .(Country)])
    
    setorder(data_res_cases,
             Country,
             DateRep)
    
    data_res_cases[, (paste0("Days_since_first_", input$cases_since_first_n, "_case")) := 1:.N,
                   by = .(Country)]
    
    data_res_cases <- copy(data_res_cases[.(input$countries_selector),
                                          on = .(Country),
                                          .SD,
                                          .SDcols = c("Country",
                                                      paste0("Days_since_first_", input$cases_since_first_n, "_case"),
                                                      input$stats_selector)
                                          ])
    
    data_res_cases

  })
  
  data_country_stat_by_first_deaths <- reactive({
    
    shiny::req(input$countries_selector, input$stats_selector, input$deaths_since_first_n)
    
    data_res <- copy(data_corona_all_new_stats())
    
    data_res_deaths <- copy(data_res[,
                                     .SD[DateRep >= .SD[Deaths_cumsum >= input$deaths_since_first_n,
                                                        min(DateRep)]],
                                     by = .(Country)])
    
    setorder(data_res_deaths,
             Country,
             DateRep)
    
    data_res_deaths[, (paste0("Days_since_first_", input$deaths_since_first_n, "_death")) := 1:.N,
                    by = .(Country)]
    
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
               input$deaths_since_first_n, input$deaths_since_first_n)

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
            main = paste(paste(input$stats_selector, collapse = ","),
                         " in ",
                         paste(input$countries_selector, collapse = ","),
                         collapse = " ")) %>%
      dyRangeSelector(fillColor = "#5bc0de", strokeColor = "#222d32") %>% # fillColor = "#5bc0de",
      dyAxis("x", label = colnames(data_res)[1]) %>%
      dyOptions(
                # useDataTimezone = TRUE,
                strokeWidth = 2,
                # fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = RColorBrewer::brewer.pal(ncol(data_res)-2, "Spectral")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5,
                                             pointSize = 4)
                  ) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  ### Scatter plot - clustering comparison of countries -----
  
  # Compute last available stats for countries
  data_countries_lastday_all_stats <- reactive({
    
    data_res_latest <- copy(data_countries_stats())
    
    data_res_latest_stats <- copy(data_res_latest[, .(Country,
                                                      'Total cases' = Cases_cumsum,
                                                      'Total deaths' = Deaths_cumsum,
                                                      'Active cases' = Active_cases_cumsum,
                                                      'Total tests' = TotalTests,
                                                      'New cases' = Cases,
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
  
  # columns selector - x and y ----
  
  # output$picker_stat_scatterplot_x <- renderUI({
  #   
  #   data_res <- copy(data_countries_lastday_all_stats())
  #   
  #   shinyWidgets::pickerInput(
  #     inputId = "stat_scatterplot_x",
  #     label = "x axis:", 
  #     choices = colnames(data_res)[-c(1)],
  #     selected = 'Total active cases per 1 million population',
  #     multiple = F,
  #     options = list(
  #       # `actions-box` = TRUE,
  #       style = "btn-info",
  #       `live-search` = TRUE,
  #       size = 8),
  #   )
  #   
  # })
  # 
  # output$picker_stat_scatterplot_y <- renderUI({
  #   
  #   shiny::req(input$stat_scatterplot_x)
  #   
  #   data_res <- copy(data_countries_lastday_all_stats())
  #   
  #   data_columns <- colnames(data_res)[-1]
  #   
  #   data_columns <- data_columns[!data_columns %in% input$stat_scatterplot_x]
  #   
  #   shinyWidgets::pickerInput(
  #     inputId = "stat_scatterplot_y",
  #     label = "y axis:", 
  #     choices = data_columns,
  #     selected = 'Positive tests rate (%)',
  #     multiple = F,
  #     options = list(
  #       # `actions-box` = TRUE,
  #       style = "btn-info",
  #       `live-search` = TRUE,
  #       size = 8),
  #   )
  #   
  # })
  # 
  # # select top N countries from selected x stat
  # 
  # output$selector_top_n_countries_x <- renderUI({
  #   
  #   data_res <- copy(data_countries_lastday_all_stats())
  #   
  #   numericInput(inputId = "top_n_countries_x",
  #                label = "Select number of top N countries from selected x statistic:",
  #                value = 48,
  #                min = 1,
  #                max = data_res[, .N],
  #                step = 2
  #                )
  #   
  # })
  # 
  # # Selected data for scatter plot ----
  # data_2d_scatterplot_selected <- reactive({
  #   
  #   shiny::req(input$stat_scatterplot_x, input$stat_scatterplot_y, input$top_n_countries_x)
  #   
  #   data_res <- copy(data_countries_lastday_all_stats()[Population > 1e6])
  #   
  #   data_res <- copy(data_res[, .SD,
  #                             .SDcols = c("Country",
  #                                         input$stat_scatterplot_x,
  #                                         input$stat_scatterplot_y)
  #                             ])
  # 
  #   setorderv(data_res, input$stat_scatterplot_x, -1)
  #   
  #   data_res_subset <- copy(na.omit(data_res)[1:input$top_n_countries_x])
  #   
  #   if (sum(grepl(pattern = "Slovakia", x = data_res_subset$Country)) == 0) {
  #     
  #     data_res_subset <- rbindlist(list(data_res_subset,
  #                                       data_res[.("Slovakia"), on = .(Country)]
  #                                       )
  #                                  )
  #     
  #   }
  #   
  #   data_res_subset
  #   
  # })
  
  # clustering 2d data ----
  
  # output$selector_n_clusters <- renderUI({
  #   
  #   data_res <- copy(data_countries_lastday_all_stats())
  #   
  #   numericInput(inputId = "n_clusters",
  #                label = "Select number of clusters:",
  #                value = 7,
  #                min = 2,
  #                max = data_res[, .N] - 1,
  #                step = 1
  #   )
  #   
  # })
  # 
  # output$clust_res_2d <- renderPlot({
  #   
  #   shiny::req(input$n_clusters)
  #   
  #   data_res <- copy(data_2d_scatterplot_selected())
  #   
  #   k <- input$n_clusters
  #   
  #   data_res_norm <- scale(data.matrix(data_res[, .SD,
  #                                               .SDcols = c(input$stat_scatterplot_x,
  #                                                           input$stat_scatterplot_y)
  #                                         ]),
  #                          center = T, scale = T)
  #   
  #   hie_complete <- hclust(dist(data_res_norm),
  #                          method = "ward.D2")
  #   
  #   dend <- as.dendrogram(hie_complete)
  #   
  #   # data_clust <- dendextend::cutree(hie_complete,
  #   #                                  k = k)
  #   
  #   # print(data_clust)
  #   
  #   # data_clust_colors <- data.table(Cluster = 1:k,
  #   #                                 Color = RColorBrewer::brewer.pal(k, name = "Set2"))
  #   
  #   # data_clust_merge <- data.table(Cluster = data_clust)
  #   # data_clust_merge[data_clust_colors,
  #   #                  on = .(Cluster),
  #   #                  Color := i.Color]
  #   
  #   # print(data_clust_merge)
  #   
  #   dend <- dend %>%
  #     color_branches(k = k) %>%
  #     color_labels(k = k) %>%
  #     set("branches_lwd", 1) %>% 
  #     set("labels", data_res[, Country]) %>%
  #     set("labels_cex", 0.9)
  #   
  #   ggd1 <- as.ggdend(rev(dend))
  #   
  #   gg_dendo <- ggplot(ggd1,
  #                      horiz = TRUE)
  # 
  #   gg_dendo
  #   
  # })
  
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
                   'Death rate (%)',
                   'Total tests per 1 million population'
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
    
    data_res <- copy(data_countries_lastday_all_stats())
    
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
    
    data_res <- copy(data_countries_lastday_all_stats())
    
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
    
    data_res_subset <- copy(na.omit(data_res)[1:input$top_n_countries_multi])
    
    if (sum(grepl(pattern = "Slovakia", x = data_res_subset$Country)) == 0) {
      
      data_res_subset <- rbindlist(list(data_res_subset,
                                        data_res[.("Slovakia"), on = .(Country)]
                                        )
                                   )
      
    }
    
    data_res_subset
    
  })
  
  # Plot scatter plot 2D or 2D MDS ----

  # output$plotly_scatterplot_2d_country_stat <- renderPlot({
  #   
  #   data_res <- copy(data_2d_scatterplot_selected())
  #   
  #   theme_my <- theme(panel.border = element_rect(fill = NA,
  #                                                 colour = "grey10"),
  #                     panel.background = element_blank(),
  #                     panel.grid.minor = element_line(colour = "grey85"),
  #                     panel.grid.major = element_line(colour = "grey85"),
  #                     panel.grid.major.x = element_line(colour = "grey85"),
  #                     axis.text = element_text(size = 13, face = "bold"),
  #                     axis.title = element_text(size = 14, face = "bold"),
  #                     plot.title = element_text(size = 16, face = "bold"),
  #                     strip.text = element_text(size = 16, face = "bold"),
  #                     strip.background = element_rect(colour = "black"),
  #                     legend.text = element_text(size = 15),
  #                     legend.title = element_text(size = 16, face = "bold"),
  #                     legend.background = element_rect(fill = "white"),
  #                     legend.key = element_rect(fill = "white"),
  #                     legend.position="bottom")
  #   
  #   gg_2d <- ggplot(data_res, aes(x = get(input$stat_scatterplot_x),
  #                                 y = get(input$stat_scatterplot_y),
  #                                 label = Country)) +
  #     geom_text_repel(alpha = 0.75) +
  #     labs(x = input$stat_scatterplot_x,
  #          y = input$stat_scatterplot_y) +
  #     theme_my
  #   
  #   gg_2d
  #   
  #   # fig <- plot_ly(data_res,
  #   #                x = ~get(input$stat_scatterplot_x),
  #   #                y = ~get(input$stat_scatterplot_y),
  #   #                type = 'scatter',
  #   #                mode = 'text',
  #   #                text = ~Country,
  #   #                alpha = 0.75,
  #   #                textposition = 'middle right',
  #   #                textfont = list(color = '#000000', size = 14))
  #   # fig <- fig %>% layout(
  #   #                       xaxis = list(title = input$stat_scatterplot_x,
  #   #                                    zeroline = TRUE),
  #   #                       yaxis = list(title = input$stat_scatterplot_y,
  #   #                                    zeroline = TRUE)
  #   #                       )
  #   # 
  #   # fig
  # 
  # })
  
  output$plot_scatterplot_mds_country_stats <- renderPlot({
    
    data_res <- copy(data_mds_scatterplot_selected())
    
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
                                         label = Country)) +
        geom_text_repel(alpha = 0.75) +
        labs(x = input$multiple_stats_clust[1],
             y = input$multiple_stats_clust[2]) +
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
                              Country = data_res$Country)
      
      gg_scatter <- ggplot(data_plot, aes(x = get("V1"),
                                          y = get("V2"),
                                          label = Country)) +
        geom_label_repel(
                        alpha = 0.95,
                        segment.alpha = 0.35,
                        label.r = 0.1,
                        box.padding = 0.25,
                        label.padding = 0.3,
                        label.size = 0.35,
                        max.iter = 2500) +
        labs(x = NULL,
             y = NULL) +
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
  
  # clustering multi-dimensional data ----
  
  output$clust_res_multidim <- renderPlot({
    
    shiny::req(input$n_clusters_multi)
    
    data_res <- copy(data_mds_scatterplot_selected())
    
    k <- input$n_clusters_multi
    
    data_res_norm <- scale(data.matrix(data_res[, .SD,
                                                .SDcols = c(input$multiple_stats_clust)
                                                ]),
                           center = T, scale = T)
    
    hie_complete <- hclust(dist(data_res_norm),
                           method = "ward.D2")
    
    dend <- as.dendrogram(hie_complete)
    
    dend <- dend %>%
      color_branches(k = k) %>%
      color_labels(k = k) %>%
      set("branches_lwd", 1) %>% 
      set("labels", data_res[, Country]) %>%
      set("labels_cex", 0.9)
    
    ggd1 <- as.ggdend(rev(dend))
    
    gg_dendo <- ggplot(ggd1,
                       horiz = TRUE)
    
    gg_dendo
    
  })
  
}
