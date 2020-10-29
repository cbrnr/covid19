library(readr)
library(dplyr)
library(magrittr)
library(purrr)
library(ggplot2)


#' Moving average
ma <- function(x, n) {
    as.vector(stats::filter(x, rep(1/n, n), sides=2))
}

plot_new_infections <- function(data) {
    if (n_distinct(data$Bundesland) > 1) {
        data %<>% group_by(Bundesland)
        legend <- TRUE
    } else {
        legend <- FALSE
    }
    
    data %>% 
        mutate(mean7=ma(AnzahlFaelle, 7)) %>%
        ggplot(aes(x=Time, y=mean7, color=Bundesland)) +
        geom_line() +
        scale_x_date(date_breaks="1 month", date_labels="%B") +
        scale_color_discrete() +
        labs(x=NULL) ->
    fig
    
    fig <- fig + labs(y="Neuinfektionen")
    if (!legend)
        fig <- fig + theme(legend.position="none") + geom_line(aes(x=Time, y=AnzahlFaelle, color=Bundesland), alpha=0.6, size=0.1)
    
    fig
}

plot_7day_incidence <- function(data) {
    if (n_distinct(data$Bundesland) > 1) {
        data %<>% group_by(Bundesland)
        legend <- TRUE
        geom <- geom_line
    } else {
        legend <- FALSE
        geom <- partial(geom_col, width=0.5)
    }
    
    data %<>% mutate(AnzahlFaelle=AnzahlFaelle / AnzEinwohner * 1e5,
                     incidence_7day=as.vector(stats::filter(AnzahlFaelle, rep(1, 7), sides=1)))
    
    data %>% 
        ggplot(aes(x=Time, y=incidence_7day, color=Bundesland, fill=Bundesland)) +
        geom() +
        scale_x_date(date_breaks="1 month", date_labels="%B") +
        scale_color_discrete() +
        labs(x=NULL, y="7-Tage-Inzidenz pro 100,000") ->
    fig
    
    if (!legend) fig <- fig + theme(legend.position="none")
    
    fig
}

plot_positive_tests <- function(data) {
    if (n_distinct(data$Bundesland) > 1) {
        data %<>% group_by(Bundesland)
        legend <- TRUE
        geom <- geom_line
    } else {
        legend <- FALSE
        geom <- partial(geom_col, width=0.5)
    }
    
    data %>% 
        mutate(faelle7=ma(AnzahlFaelle, 7),
               tests7=ma(tests, 7),
               positive=faelle7 / tests7) %>%
        ggplot(aes(x=Time, y=positive, color=Bundesland, fill=Bundesland)) +
        geom() +
        scale_x_date(date_breaks="1 month", date_labels="%B") +
        scale_y_continuous(labels=scales::percent) +
        coord_cartesian(ylim=c(0, 1)) +
        labs(x=NULL, y="Anteil positiver Tests") ->
    fig
    
    if (!legend) fig <- fig + theme(legend.position="none")
    
    fig
}

theme_set(theme_minimal())

covid <- read_delim("https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline.csv", ";",
                    locale=locale(decimal_mark=","),
                    col_types=cols(Time=col_date("%d.%m.%Y %H:%M:%S")))
tests <- read_delim("https://covid19-dashboard.ages.at/data/CovidFallzahlen.csv", ";",
                    locale=locale(decimal_mark=","),
                    col_types=cols(Meldedat=col_date("%d.%m.%Y")))

tests %<>% 
    group_by(Bundesland) %>% 
    mutate(tests=TestGesamt - lag(TestGesamt),
           Bundesland=recode(Bundesland, "Alle"="Österreich")) %>% 
    select(Meldedat, Bundesland, BundeslandID, tests)

covid <- left_join(covid, tests, by=c("Time"="Meldedat", "Bundesland"="Bundesland", "BundeslandID"="BundeslandID"))

plot_new_infections(covid %>% filter(Bundesland == "Österreich"))
plot_new_infections(covid %>% filter(Bundesland != "Österreich"))
plot_positive_tests(covid %>% filter(Bundesland == "Österreich")) + coord_cartesian(ylim=c(0, 0.15))
plot_7day_incidence(covid %>% filter(Bundesland != "Österreich"))
plot_7day_incidence(covid %>% filter(Bundesland == "Österreich"))
