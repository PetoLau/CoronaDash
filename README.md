# CoronaDash

[**Shinydashboard online application**](https://petolau.shinyapps.io/coronadash/)

This `shiny`/`shinydashboard` application is only for informative purposes, how the **COVID-19** virus can spread over time for a defined country (or whole world) and period of days (cases and deaths).

Data are coming from [European Centre for Disease Prevention and Control](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide).

The forecasting model is the ETS (**Exponential smoothing**) implemented in a [`smooth` R package](https://cran.r-project.org/package=smooth), so only historical data of target time series are used.
For total cumulative confirmed cases, the fully multiplicative model is used.
For total cumulative death cases, the fully additive/multiplicative model is used.

The forecasting model applied on the Covid-19 use case was inspired by [Fotios Petropoulos tweets](https://twitter.com/fotpetr).

You can read more about my work here: [petolau.github.io/about/](https://petolau.github.io/about/).

**Take care of yourself!**
