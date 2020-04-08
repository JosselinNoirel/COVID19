COVID19
================

[Online application](https://shiny.cnam.fr/CoVid19/) hosted by the
[Conservatoire national des arts et métiers](http://www.cnam.fr/)
<a href="http://www.cnam.fr/"><img src="./lecnamlogo.jpg" width="80px" /></a>.
This work is licensed under a [Attribution-NonCommercial-ShareAlike 4.0
International
License](http://creativecommons.org/licenses/by-nc-sa/4.0/) [![CC
BY 4.0](https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-nc-sa/4.0/)

## Data

  - This Shiny app depends on the [data provided by the JHU Center for
    Systems Science and
    Engineering](https://github.com/CSSEGISandData/COVID-19)

  - Other data of interest are given by the [European Centre for Disease
    Prevention and Control An agency of the European
    Union](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide)

<!-- https://data.europa.eu/euodp/en/data/dataset/covid-19-coronavirus-data/resource/55e8f966-d5c8-438e-85bc-c7a5a26f4863 -->

``` r
dat_URL = "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
dat = readr::read_csv(dat_URL)

dat_URL = "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx"
dat = rio::import(dat_URL)
```

  - Santé publique France provides a [weekly
    report](https://www.santepubliquefrance.fr/maladies-et-traumatismes/maladies-et-infections-respiratoires/infection-a-coronavirus/articles/infection-au-nouveau-coronavirus-sars-cov-2-covid-19-france-et-monde)
    of estimated COVID19 cases based on general practioners’ records,
    etc

## Visualisation

  - [Genomic epidemiology of novel
    coronaviruses](https://nextstrain.org/ncov)

  - John Hopkins University’s famous
    [dashboard](https://coronavirus.jhu.edu/map.html)

  - The well-known page by
    [Worldometer](https://www.worldometers.info/coronavirus/)

  - A [neat visualisation of the SEIR
    model](http://gabgoh.github.io/COVID/index.html)

  - SEIR+: [A SEIR model with social
    structure](https://github.com/ryansmcgee/seirsplus)

## Credits and resources

  - This app depends on the following packages:
    [DT](https://CRAN.R-project.org/package=DT) —
    [ggrepel](https://CRAN.R-project.org/package=ggrepel) —
    [lubridate](https://CRAN.R-project.org/package=lubridate) —
    [shiny](https://CRAN.R-project.org/package=shiny) —
    [shinyWidgets](https://CRAN.R-project.org/package=shinyWidgets) —
    [tidyverse](https://CRAN.R-project.org/package=tidyverse). The
    [shinyEventLogger](https://CRAN.R-project.org/package=shinyEventLogger)
    package has been useful during development.

  - RStudio’s articles on [Debugging Shiny
    applications](https://shiny.rstudio.com/articles/debugging.html) and
    [Shiny server error
    logs](https://support.rstudio.com/hc/en-us/articles/115003717168-Shiny-Server-Error-Logs)

  - [How to use SliderInput for
    dates](https://stackoverflow.com/questions/40908808/how-to-sliderinput-for-dates)
    on Stackoverflow

  - The [lead and
    lag](https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/lead-lag)
    functions from dplyr

  - I am grateful to Rafik Abdesselam for the help in getting this
    application online
