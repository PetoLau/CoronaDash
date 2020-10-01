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

  tags$style(make_css(list('.dygraph-legend', 
                           c('left', 'background-color'), 
                           c('70px !important', 'transparent !important')))),
  
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
  
  tags$style(type="text/css", css),
  
  tabItems(
    tabItem(tabName = "corTab",
            class = 'active',
            fluidRow(
              column(width = 4,
              box(title = span(icon("magic"), " Select your preferences - country and forecasting horizon"),
                  solidHeader = F, status = "info",
                  collapsible = TRUE, width = NULL,
                  uiOutput("selector_country"),
                  uiOutput("slider_n_days_forec"),
                  htmlOutput("text_date_update")
              ),
              box(
                title = span(icon("share-alt"), " Connect with the author on:"),
                status = "info",
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
                  status = "info",
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
                  solidHeader = F, status = "warning",
                  collapsible = TRUE, width = 12,
                  valueBoxOutput("valuebox_total_cases") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_deaths") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_death_rate") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_active") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_recov") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_active_per_mil") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_positivetests_rate") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_num_tests_pop") %>% withSpinner(color = "#5bc0de")
                  )
              ),
            fluidRow(
              tabBox(
                title = span(icon("chart-area"), " Cases for the selected country"),
                id = "tabset1",
                tabPanel("Cumulative cases",
                         htmlOutput("cases_text"),
                         dygraphOutput("dygraph_country_cases") %>% withSpinner(color = "#5bc0de")),
                tabPanel("New cases only",
                         # htmlOutput("cases_text"),
                         dygraphOutput("dygraph_country_new_cases") %>% withSpinner(color = "#5bc0de"))
              ),
              # box(title = span(icon("chart-area"), " Cases for the selected country"),
              #     solidHeader = F, status = "primary",
              #     collapsible = TRUE, width = 6,
              #     htmlOutput("cases_text"),
              #     dygraphOutput("dygraph_country_cases") %>% withSpinner(color = "#5bc0de")
              # ),
              box(title = span(icon("chart-line"), " Forecasted total cumulative cases for the selected country +
                               90% upper prediction interval"),
                  solidHeader = F, status = "primary",
                  collapsible = TRUE, width = 6,
                  htmlOutput("cases_forec_text"),
                  dygraphOutput("dygraph_country_cases_forecast") %>% withSpinner(color = "#5bc0de")
              )
            ),
            fluidRow(
              tabBox(
                title = span(icon("chart-area"), " Deaths for the selected country"),
                id = "tabset2",
                tabPanel("Cumulative deaths",
                         htmlOutput("death_text"),
                         dygraphOutput("dygraph_country_deaths") %>% withSpinner(color = "#5bc0de")
                         ),
                tabPanel("New deaths only",
                         # htmlOutput("death_text"),
                         dygraphOutput("dygraph_country_new_deaths") %>% withSpinner(color = "#5bc0de")
                         )
              )
              # box(title = span(icon("chart-area"), " Deaths for the selected country"),
              #     solidHeader = F, status = "primary",
              #     collapsible = TRUE, width = 6,
              #     htmlOutput("death_text"),
              #     dygraphOutput("dygraph_country_deaths") %>% withSpinner(color = "#5bc0de")
              # )
              # box(title = span(icon("chart-line"), " Forecasted cumulative deaths for the selected country +
              #                  90% upper prediction interval"),
              #     solidHeader = F,
              #     collapsible = TRUE, width = 6,
              #     dygraphOutput("dygraph_country_deaths_forecast") %>% withSpinner(color = "#5bc0de")
              # )
            )
            ),
    tabItem(tabName = "compareTab",
            fluidRow(
              box(title = span(icon("flag"), " Select multiple countries for comparison"),
                  solidHeader = F, status = "info",
                  collapsible = F, width = 6,
                  uiOutput("picker_countries_selector")
                  ),
              box(title = span(icon("table"), " Select one statistic for comparison"),
                  solidHeader = F, status = "danger",
                  collapsible = F, width = 3,
                  uiOutput("picker_stats_selector"),
                  uiOutput("switch_log_scale_compareTab")
                  ),
              box(title = span(icon("angle-double-up"), " Set parameters for comparison of \"since first\" trajectories graph"),
                  solidHeader = F, status = "info",
                  collapsible = F, width = 3,
                  uiOutput("selector_cases_since_first_n"),
                  uiOutput("selector_deaths_since_first_n")
                  )
              ),
            fluidRow(
              box(title = span(icon("chart-line"), " Comparison of countries' trajectories since their first total N-th case/ N-th death"),
                  solidHeader = F, status = "primary",
                  collapsible = TRUE, width = 12,
                  dygraphOutput("dygraph_countries_stats_since_first") %>% withSpinner(color = "#5bc0de")
              )
            ),
            fluidRow(
              box(title = span(icon("chart-area"), " Comparison of countries"),
                  solidHeader = F, status = "primary",
                  collapsible = TRUE, width = 12,
                  dygraphOutput("dygraph_countries_stats") %>% withSpinner(color = "#5bc0de")
                  )
              )
            ),
    tabItem(tabName = "trajectoryTab",
            fluidRow(
              box(title = span(icon("info-circle"), " Information about analysis capabilities in this dashboards tab"),
                  solidHeader = T, status = "info",
                  collapsible = F, width = 12,
                  htmlOutput("info_clustering_trajectories")
                  )
              ),
            fluidRow(
              box(title = span(icon("table"), " Select one statistic for clustering trajectories"),
                  solidHeader = F, status = "info",
                  collapsible = F, width = 4,
                  uiOutput("picker_stat_selector_clust"),
                  uiOutput("picker_country_clust"),
                  # infoBoxOutput("infobox_inwhich_cluster_country") %>% withSpinner(color = "#5bc0de", size = 0.1)
                  htmlOutput("text_inwhich_cluster_country")
              ),
              box(title = span(icon("angle-double-up"), " Set parameters for comparison of \"since first\" trajectories data"),
                  solidHeader = F, status = "info",
                  collapsible = F, width = 4,
                  uiOutput("selector_cases_since_first_n_clust"),
                  uiOutput("selector_deaths_since_first_n_clust")
              ),
              box(title = span(icon("sliders-h"), " Select parameters for analysis/ clustering"),
                  solidHeader = F, status = "info",
                  collapsible = F, width = 4,
                  uiOutput("selector_top_n_countries_clust"),
                  uiOutput("selector_n_clusters_dtw")
                  )
            ),
            fluidRow(
              box(title = span(icon("chart-line"), " Clustered countries' trajectories based on selected statistics and parameters (see above)"),
                  solidHeader = F, status = "info",
                  footer = "DTW distance measure and hierarchical clustering with Ward criterion are used.
                  Colored dashed lines are DTW barycenters.",
                  collapsible = TRUE, width = 8,
                  fluidRow(column(width = 5, uiOutput("selector_sma_order")),
                           column(width = 4, uiOutput("switch_normalization")),
                           column(width = 3, uiOutput("switch_log_scale"))
                           ),
                  plotOutput("plot_clusters_trajectories", height = "70vh") %>% withSpinner(color = "#5bc0de")
              ),
              box(title = span(icon("crosshairs"), " Focus plot for detailed analysis of countries in the selected cluster"),
                  solidHeader = F, status = "primary",
                  footer = "Interactive plot - zoom in/out etc.",
                  collapsible = TRUE, width = 4,
                  uiOutput("picker_cluster_focus"),
                  plotlyOutput("plotly_focus_cluster") %>% withSpinner(color = "#5bc0de")
                  )
            ),
            fluidRow(
              box(title = span(icon("tree"), " Dendrogram of clustered countries' trajectories based on similarities of selected statistic"),
                  footer = "DTW distance measure and hierarchical clustering with Ward criterion are used.",
                  solidHeader = F, status = "success",
                  collapsible = F, width = 7,
                  # uiOutput("dropdown_clustering_crit"),
                  plotOutput("plot_clusters_trajectories_dendogram", height = "75vh") %>% withSpinner(color = "#5bc0de")
                  ),
              box(title = span(icon("chart-area"), " 2D MDS Scatter plot of clustered countries' trajectories for selected statistic"),
                  footer = "MDS - Multidimensional scaling - parametric.",
                  solidHeader = F, status = "info",
                  collapsible = F, width = 5,
                  plotOutput("plot_scatter_mds_trajectories", height = "72vh") %>% withSpinner(color = "#5bc0de")
              )
            )
    ),
    tabItem(tabName = "analysisTab",
            fluidRow(
              box(title = span(icon("info-circle"), " Information about analysis capabilities in this dashboards tab"),
                  solidHeader = T, status = "info",
                  collapsible = F, width = 12,
                  htmlOutput("info_clustering_stats")
              )
            ),
            fluidRow(
              box(title = span(icon("balance-scale"), " Select multiple statistics for clustering countries based on these data"),
                  solidHeader = F, status = "info",
                  collapsible = F, width = 9,
                  uiOutput("picker_multiple_stats_clust"),
                  uiOutput("picker_sort_column")
              ),
              box(title = span(icon("sliders-h"), " Select parameters for analysis/ clustering"),
                  solidHeader = F, status = "info",
                  collapsible = F, width = 3,
                  uiOutput("selector_top_n_countries_multi"),
                  uiOutput("selector_n_clusters_multi")
                  )
            ),
            fluidRow(
              box(title = span(icon("chart-area"), " 2D/MDS Scatter plot of countries for selected statistics"),
                  footer = "If a number of statistics is equal to 2, then scatter plot is used without MDS. If n > 2 then MDS is always used.
                  MDS - Multidimensional scaling - parametric.",
                  solidHeader = F, status = "info",
                  collapsible = F, width = 6,
                  plotOutput("plot_scatterplot_mds_country_stats", height = "72vh") %>% withSpinner(color = "#5bc0de")
              ),
              box(title = span(icon("tree"), " Dendrogram of clustered countries based on similarities of selected statistics"),
                  footer = "Euclidean distance measure and hierarchical clustering with Ward criterion are used.",
                  solidHeader = F, status = "success",
                  collapsible = F, width = 6,
                  uiOutput("dropdown_clustering_crit"),
                  plotOutput("clust_res_multidim", height = "75vh") %>% withSpinner(color = "#5bc0de")
              )
            ),
            fluidRow(
              box(title = span(icon("table"), " Clusters' members averages"),
                  # footer = "If a number of statistics is equal to 2, then scatter plot is used without MDS. If n > 2 then MDS is always used.
                  # MDS - Multidimensional scaling - parametric.",
                  solidHeader = F, status = "info",
                  collapsible = F, width = 12,
                  DTOutput("dt_clusters_averages") %>% withSpinner(color = "#5bc0de")
              )
            )
    ),
    tabItem(tabName = "worldTab",
            fluidRow(
              box(title = span(icon("table"), " World statistics"),
                  solidHeader = F, status = "warning",
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
              tabBox(
                title = span(icon("chart-area"), " Cases for the World"),
                id = "tabset3",
                tabPanel("Cumulative cases",
                         htmlOutput("cases_text_world"),
                         dygraphOutput("dygraph_world_cases") %>% withSpinner(color = "#5bc0de")
                ),
                tabPanel("New cases only",
                         # htmlOutput("cases_text_world"),
                         dygraphOutput("dygraph_world_new_cases") %>% withSpinner(color = "#5bc0de")
                )
              ),
              # box(title = span(icon("chart-area"), " Cases for the World"),
              #     solidHeader = F, status = "primary",
              #     collapsible = TRUE, width = 6,
              #     htmlOutput("cases_text_world"),
              #     dygraphOutput("dygraph_world_cases") %>% withSpinner(color = "#5bc0de")
              # ),
              box(title = span(icon("chart-line"), " Forecasted total cumulative cases for the World +
                               90% upper prediction interval"),
                  solidHeader = F, status = "primary",
                  collapsible = TRUE, width = 6,
                  dygraphOutput("dygraph_world_cases_forecast") %>% withSpinner(color = "#5bc0de")
              )
            ),
            fluidRow(
              tabBox(
                title = span(icon("chart-area"), " Deaths for the World"),
                id = "tabset4",
                tabPanel("Cumulative deaths",
                         htmlOutput("death_text_world"),
                         dygraphOutput("dygraph_world_deaths") %>% withSpinner(color = "#5bc0de")
                ),
                tabPanel("New deaths only",
                         # htmlOutput("death_text_world"),
                         dygraphOutput("dygraph_world_new_deaths") %>% withSpinner(color = "#5bc0de")
                )
              ),
              # box(title = span(icon("chart-area"), " Deaths for the World"),
              #     solidHeader = F, status = "primary",
              #     collapsible = TRUE, width = 6,
              #     htmlOutput("death_text_world"),
              #     dygraphOutput("dygraph_world_deaths") %>% withSpinner(color = "#5bc0de")
              # ),
              box(title = span(icon("chart-line"), " Forecasted total cumulative deaths for the World +
                               90% upper prediction interval"),
                  solidHeader = F, status = "primary",
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
