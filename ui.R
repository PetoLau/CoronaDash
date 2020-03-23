## ui.R ##
header <- dashboardHeader(title = span(tagList(icon("diagnoses"), "CoronaDash")),
                          tags$li(a(href = 'https://petolau.github.io/',
                                    target="_blank", "Created by PetoLau",
                                    img(src = 'favicon_trans.png',
                                    title = "PetoLau", height = "25px"),
                                    style = "padding-top:13px; padding-bottom:9px;"),
                                  class = "dropdown")
                          )

sidebar <- dashboardSidebar(
  sidebarMenuOutput("Side_dash")
  )

css <- ".shiny-output-error { visibility: hidden; }
        .shiny-output-error:before { visibility: hidden; }
       "


body <- dashboardBody(
  
  useShinyjs(),
  
  tags$head(tags$link(rel = "shortcut icon", href = "favicon_trans.png")),

  tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
  
  shinyEffects::setShadow(class = "box"),
  shinyEffects::setShadow(id = "my-progress"),
  
  shinyWidgets::useSweetAlert(),

  setSliderColor(color = "DeepSkyBlue",
                 sliderId = 1),
  
  chooseSliderSkin("Flat"),
  
  tags$head(
    tags$style(
      HTML(
        ".checkboxgroup-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkboxgroup-inline+.checkboxgroup-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
      )
    )
  ),
  
  tags$style(type="text/css", css),
  
  tabItems(
    tabItem(tabName = "corTab",
            class = 'active',
            fluidRow(
              column(width = 4,
              box(title = span(icon("magic"), " Select your preferences - country and forecasting horizon"),
                  solidHeader = F,
                  collapsible = TRUE, width = NULL,
                  uiOutput("selector_country") %>% withSpinner(color = "#5bc0de"),
                  uiOutput("slider_n_days_forec") %>% withSpinner(color = "#5bc0de"),
                  htmlOutput("text_date_update")
              ),
              box(
                title = span(icon("share-alt"), " Connect with the author on:"),
                status = NULL,
                solidHeader = F,
                footer = "Links for my twitter, linkedin and github accounts.",
                collapsible = TRUE, width = NULL, collapsed = TRUE,
                # htmlOutput("text_accounts"),
                socialButton(
                  url = "https://twitter.com/petolauri",
                  type = "twitter"
                ),
                socialButton(
                  url = "https://www.linkedin.com/in/peter-laurinec-590a8a90",
                  type = "linkedin"
                ),
                socialButton(
                  url = "https://github.com/PetoLau/CoronaDash",
                  type = "github"
                )
              )
              ),
              box(title = span(icon("table"), " Table of countries"),
                  footer = "Table is sorted by total Active cases. New cases = cases in the past day (24h).
                  ActCases/MilPop = Active cases per 1 million population",
                  status = NULL,
                  solidHeader = F,
                  collapsible = TRUE, width = 4, collapsed = F,
                  DTOutput("dt_countries_cases")
                  ),
              box(
                title = span(icon("info-circle"), " Information about this app and forecasting COVID-19"),
                status = "info",
                solidHeader = T,
                collapsible = FALSE, width = 4, collapsed = F,
                htmlOutput("informative_text")
              )
            ),
            fluidRow(
              box(title = span(icon("table"), " Statistics"),
                  solidHeader = F,
                  collapsible = TRUE, width = 12,
                  valueBoxOutput("valuebox_total_cases") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_deaths") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_death_rate") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_active") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_recov") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_active_per_mil") %>% withSpinner(color = "#5bc0de")
                  )
              ),
            fluidRow(
              box(title = span(icon("chart-area"), " Cases for the selected country"),
                  solidHeader = F,
                  collapsible = TRUE, width = 6,
                  htmlOutput("cases_text"),
                  dygraphOutput("dygraph_country_cases") %>% withSpinner(color = "#5bc0de")
              ),
              box(title = span(icon("chart-line"), " Forecasted total cumulative cases for the selected country +
                               90% upper prediction interval"),
                  solidHeader = F,
                  collapsible = TRUE, width = 6,
                  htmlOutput("cases_forec_text"),
                  dygraphOutput("dygraph_country_cases_forecast") %>% withSpinner(color = "#5bc0de")
              )
            ),
            fluidRow(
              box(title = span(icon("chart-area"), " Deaths for the selected country"),
                  solidHeader = F,
                  collapsible = TRUE, width = 6,
                  htmlOutput("death_text"),
                  dygraphOutput("dygraph_country_deaths") %>% withSpinner(color = "#5bc0de")
              )
              # box(title = span(icon("chart-line"), " Forecasted cumulative deaths for the selected country +
              #                  90% upper prediction interval"),
              #     solidHeader = F,
              #     collapsible = TRUE, width = 6,
              #     dygraphOutput("dygraph_country_deaths_forecast") %>% withSpinner(color = "#5bc0de")
              # )
            )
            ),
    tabItem(tabName = "worldTab",
            fluidRow(
              box(title = span(icon("table"), " World statistics"),
                  solidHeader = F,
                  collapsible = TRUE, width = 12,
                  valueBoxOutput("valuebox_total_cases_world") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_deaths_world") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_death_rate_world") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_active_world") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_recov_world") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_active_per_mil_world") %>% withSpinner(color = "#5bc0de")
              )
            ),
            fluidRow(
              box(title = span(icon("chart-area"), " Cases for the World"),
                  solidHeader = F,
                  collapsible = TRUE, width = 6,
                  htmlOutput("cases_text_world"),
                  dygraphOutput("dygraph_world_cases") %>% withSpinner(color = "#5bc0de")
              ),
              box(title = span(icon("chart-line"), " Forecasted total cumulative cases for the World +
                               90% upper prediction interval"),
                  solidHeader = F,
                  collapsible = TRUE, width = 6,
                  dygraphOutput("dygraph_world_cases_forecast") %>% withSpinner(color = "#5bc0de")
              )
            ),
            fluidRow(
              box(title = span(icon("chart-area"), " Deaths for the World"),
                  solidHeader = F,
                  collapsible = TRUE, width = 6,
                  htmlOutput("death_text_world"),
                  dygraphOutput("dygraph_world_deaths") %>% withSpinner(color = "#5bc0de")
              ),
              box(title = span(icon("chart-line"), " Forecasted total cumulative deaths for the World +
                               90% upper prediction interval"),
                  solidHeader = F,
                  collapsible = TRUE, width = 6,
                  dygraphOutput("dygraph_world_deaths_forecast") %>% withSpinner(color = "#5bc0de")
              )
            )
    )
  )
  
)

ui <- function(req) {
  
  dashboardPagePlus(
    title = "CoronaDash",
    enable_preloader = TRUE,
    loading_duration = 2.1,
    header = header,
    sidebar = sidebar,
    body = body,
    skin = "blue-light" # -light
  )
  
}
