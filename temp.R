library("DT")

src = paste(today(), ".rds", sep="")

dat = read_rds(src)

country_list = (dat[[2]] %>% filter(Date == dmy("30-03-2020") & Value > 40))$Country
country_list = setdiff(country_list, "Others")

# write_csv(tibble(country=country_list), "populations.csv")
# # Run Mathematica Script
# populations = read_csv("populations.csv", col_names=c("Country", "Population"))
# write_rds(populations, "populations.rds")

input = list()
input$countries = "France,Italy,Germany,US,UK"
input$what = "confirmed"
input$log = TRUE
input$predict = FALSE
input$min = 5000

populations = read_rds("populations.rds")

ma = function(x, n=5) as.numeric(stats::filter(x, rep(1/n, n), sides=2))

if (str_starts(input$countries, "-")) {
    selfn = setdiff
    input_countries = substring(input$countries, 2)
} else {
    selfn = intersect
    input_countries = input$countries
}
cl = strsplit(input_countries, ',')[[1]]

if (length(cl) == 0) {
    cl = country_list
} else {
    cl = selfn(country_list, cl)
}

x = dat[[input$what]] %>% filter(Country %in% cl)

x = x %>% left_join(populations, by="Country")

TODAY = max(x$Date)
y = x %>% mutate(diff=as.integer(TODAY - Date)) %>%
    arrange(Date) %>%
    group_by(Country) %>%
    mutate(n=n(), Difference=c(0, diff(Value)),
           Smoothed=ma(Difference, n=4)) %>%
    ungroup() %>%
    mutate(NormalisedDifference=1000 * Difference/Population)

# ggplot(y %>% filter(Value > 0), aes(Date, Value, colour=Country)) +
#     geom_line() +
#     geom_point()

ggplot(y,
       aes(Date, NormalisedDifference, colour=Country)) +
    geom_point() +
    geom_smooth(method="loess", formula=y~x,
                span=0.5, se=FALSE) +
    geom_text_repel(data=subset(y, Date == max(x$Date)),
                    aes(label=Country, colour=Country,
                        x=Date, y=NormalisedDifference),
                    xlim=c(min(x$Date), max(x$Date) + 14),
                    segment.size=0.1,
                    direction="y",
                    nudge_x = 1,
                    hjust = 0) +
    ylab(NULL) +
    coord_cartesian(clip="off") +
    theme(plot.margin=unit(c(0,2,0,0), "in"),
          legend.position="none")

