library("shiny")

log_level = 1 # 0: Print nothing
log_out = list(h3("Debug"))
log_add = function (str, level=1) {
    if (log_level >= level) {
        log_out[[length(log_out) + 1]] <<- p(paste(level, ":", str))
    }
}

log_add("Package 'shiny' loaded!")

library("shinyWidgets") # devtools::install_github("dreamRs/shinyWidgets")

log_add("Package 'shiny' loaded")

library("ggrepel")      # install.packages("ggrepel")

log_add("Package 'ggrepel' loaded")

library("tidyverse")

log_add("Package 'tidyverse' loaded")

library("lubridate")

log_add("Package 'lubridate' loaded")

library("DT")

log_add("Package 'DT' loaded")

theme_set(theme_bw() + theme(panel.border=element_blank()))

populations = read_rds("populations.rds")

log_add("Populations loaded")

today = function () "2020-04-06"

src = paste(today(), ".rds", sep="")

ma = function(x, n=5) as.numeric(stats::filter(x, rep(1/n, n), sides=2))

if (file.exists(src)) {
    dat = read_rds(src)
    log_add("Reading the RDS data")
}

if (FALSE && ! file.exists(src)) {
    fn = function (str)
        paste("time_series_covid19_", str, "_global.csv", sep="")

    preprocess = function (dat) {
        colnames(dat)[1:4] = c("Province", "Country", "Latitude", "Longitude")

        dat$Country[dat$Country == "United Kingdom"] =  "UK"
        dat$Country[dat$Country == "Korea, South"] =  "South Korea"
        dat$Country[dat$Country == "Taiwan*"] =  "Taiwan"
        dat = pivot_longer(dat, 5:last_col(), names_to="Date", values_to="Value")
        dat = dat %>% group_by(Country, Date) %>%
            summarise(Value=sum(Value)) %>%
            ungroup()
        dat$Date = mdy(dat$Date)
        dat
    }

    path = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

    file = list()
    file[["deceased"]]  = paste(path, fn("deaths"), sep="/")
    file[["confirmed"]] = paste(path, fn("confirmed"), sep="/")
    file[["recovered"]] = paste(path, fn("recovered"), sep="/")

    data = list()
    data[["deceased"]]  = read_csv(file[["deceased"]])
    data[["confirmed"]] = read_csv(file[["confirmed"]])
    data[["recovered"]] = read_csv(file[["recovered"]])

    dat = list()
    dat[["deceased"]] = preprocess(data[[1]])
    dat[["confirmed"]] = preprocess(data[[2]])
    dat[["recovered"]] = preprocess(data[[3]])

    write_rds(dat, src)
}

country_list = (dat[[2]] %>% filter(Date == dmy("15-03-2020") & Value > 40))$Country
country_list = setdiff(country_list, "Others")

log_add("Country list OK")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$log = renderUI(tagList(log_out))

    output$cumPlot = renderPlot({
        plot(mpg ~ qsec, data=mtcars)

        # if (str_starts(input$countries, "-")) {
        #     selfn = setdiff
        #     input_countries = substring(input$countries, 2)
        # } else {
        #     selfn = intersect
        #     input_countries = input$countries
        # }
        # cl = strsplit(input_countries, ',')[[1]]
        #
        # if (length(cl) == 0) {
        #     cl = country_list
        # } else {
        #     cl = selfn(country_list, cl)
        # }
        #
        # x = dat[[input$what]] %>% filter(Country %in% cl & Value > input$min)
        #
        # p = ggplot(x,
        #            aes(Date, Value, colour=Country)) +
        #     geom_point(size=1, alpha=(if (input$predict) 0.5 else 1)) +
        #     geom_line(size=1, alpha=(if (input$predict) 0.5 else 1)) +
        #     coord_cartesian(clip="off") +
        #     (if (input$log) scale_y_log10() else NULL) +
        #     (if (input$predict)
        #         geom_smooth(data=subset(x, Date > today() - 10),
        #                     method="lm", se=FALSE) else NULL) +
        #     geom_text_repel(data=subset(x, Date == max(x$Date)),
        #                     aes(label=Country, colour=Country, x=Date, y=Value),
        #                     xlim=c(min(x$Date), max(x$Date) + 14),
        #                     segment.size=0.1,
        #                     direction="y",
        #                     nudge_x = 1,
        #                     hjust = 0) +
        #     ylab(NULL) + xlab(NULL) +
        #     theme(plot.margin=unit(c(0,2,1,0), "in"),
        #           legend.position="none")
        #
        # p
    })

    output$peakPlot = renderPlot({
        plot(mpg ~ qsec, data=mtcars)

        # if (str_starts(input$countries, "-")) {
        #     selfn = setdiff
        #     input_countries = substring(input$countries, 2)
        # } else {
        #     selfn = intersect
        #     input_countries = input$countries
        # }
        # cl = strsplit(input_countries, ',')[[1]]
        #
        # if (length(cl) == 0) {
        #     cl = country_list
        # } else {
        #     cl = selfn(country_list, cl)
        # }
        #
        # x = dat[[input$what]] %>%
        #     filter(Country %in% cl &
        #                Value > input$min) %>%
        #     group_by(Country) %>%
        #     mutate(n=n()) %>%
        #     ungroup() %>%
        #     filter(n > 10)
        #
        # x= dat[[input$what]] %>% filter(Country %in% x$Country)
        #
        # x = x %>% left_join(populations, by="Country")
        #
        # TODAY = max(x$Date)
        # y = x %>% mutate(diff=as.integer(TODAY - Date)) %>%
        #     arrange(Date) %>%
        #     group_by(Country) %>%
        #     mutate(n=n(), Difference=c(0, diff(Value)),
        #            Smoothed=ma(Difference, n=4)) %>%
        #     ungroup() %>%
        #     mutate(NormalisedDifference=1000 * Difference/Population)
        #
        # p = ggplot(y,
        #            aes(Date, NormalisedDifference, colour=Country)) +
        #     geom_point() +
        #     geom_smooth(method="loess", formula=y~x,
        #                 span=0.5, se=FALSE) +
        #     geom_text_repel(data=subset(y, Date == max(x$Date)),
        #                     aes(label=Country, colour=Country,
        #                         x=Date, y=NormalisedDifference),
        #                     xlim=c(min(x$Date), max(x$Date) + 14),
        #                     segment.size=0.1,
        #                     direction="y",
        #                     nudge_x = 1,
        #                     hjust = 0) +
        #     ylab(NULL) + xlab(NULL) +
        #     coord_cartesian(clip="off") +
        #     theme(plot.margin=unit(c(0,2,1,0), "in"),
        #           legend.position="none")
        #
        # p
    })

    output$stats = renderDataTable({
        datatable(mtcars)

        # if (str_starts(input$countries, "-")) {
        #     selfn = setdiff
        #     input_countries = substring(input$countries, 2)
        # } else {
        #     selfn = intersect
        #     input_countries = input$countries
        # }
        # cl = strsplit(input_countries, ',')[[1]]
        #
        # if (length(cl) == 0) {
        #     cl = country_list
        # } else {
        #     cl = selfn(country_list, cl)
        # }
        #
        # dat[[input$what]] %>% filter(Country %in% cl & Value > input$min) %>%
        #     arrange(Country, Date) %>%
        #     datatable()
    })
})
