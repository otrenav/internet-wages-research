##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##

setwd("~/Projects/itam/internet_wages/")
source("./analysis/functions/heat_map.R")

if (!require(png)) {
    install.packages("png")
    require(png)
}
if (!require(grid)) {
    install.packages("grid")
    require(grid)
}
if (!require(ggplot2)) {
    install.packages("ggplot2")
    require(ggplot2)
}
if (!require(ggmap)) {
    install.packages("ggmap")
    require(ggmap)
}

data <- read.csv(
    "./data/2010_2015_geocoded.csv",
    header=TRUE
)

geo_center <- c(-102, 25)
zoom_level <- 5

## Get map from Google
map <- get_map(
    zoom      = zoom_level,
    location  = geo_center,
    scale     = "auto",
    filename  = "./data/spatial/map",
    maptype   = "roadmap",
    source    = "google",
    messaging = FALSE
)

## Data setup
data_highs <- data[data$HIGH_ALL_2010 == 1, ]
data_highs <- data_highs[!is.na(data_highs$longitude), ]

data_non_highs <- data[data$HIGH_ALL_2010 == 0, ]
data_population_threshold <- data[data$NUMPERS_2010 > 15000, ]
data_non_highs <- data_non_highs[!is.na(data_non_highs$longitude), ]

threshold <- 1
data_non_highs_lost    <- data_non_highs[data_non_highs$DIFF_INGTRMEN < 0, ]
data_non_highs_won     <- data_non_highs[data_non_highs$DIFF_INGTRMEN > threshold, ]
data_non_highs_stable  <- data_non_highs[data_non_highs$DIFF_INGTRMEN > 0, ]
data_non_highs_stable  <- data_non_highs[data_non_highs$DIFF_INGTRMEN < threshold, ]

## Base map
map_plot <- ggmap(map) +
    labs(x="Longitude", y="Latitude") +
    coord_equal() +
    ## annotate(
    ##     "rect",
    ##     xmin=-Inf, xmax=Inf,
    ##     ymin=-Inf, ymax=Inf,
    ##     fill="black", alpha=0.3
    ## ) +
    geom_point(
        data=data_non_highs_stable,
        aes(x=as.numeric(longitude),
            y=as.numeric(latitude),
            ## size=INTERNET_2010),
            size=NUMPERS_2010),
        color="black",
        shape=16,  # Circle
        ## size=3,
        alpha=0.5
    ) +
    geom_point(
        data=data_non_highs_lost,
        aes(x=as.numeric(longitude),
            y=as.numeric(latitude),
            ## size=INTERNET_2010),
            size=NUMPERS_2010),
        color="red",
        shape=16,  # Circle
        ## size=3,
        alpha=0.5
    ) +
    geom_point(
        data=data_highs,
        aes(x=as.numeric(longitude),
            y=as.numeric(latitude),
            ## size=INTERNET_2010),
            size=NUMPERS_2010),
        color="blue",
        shape=16,  # Circle
        ## size=3,
        alpha=0.5
    ) +
    geom_point(
        data=data_population_threshold,
        aes(x=as.numeric(longitude),
            y=as.numeric(latitude),
            ## size=INTERNET_2010),
            size=NUMPERS_2010),
        color="orange",
        shape=16,  # Circle
        ## size=3,
        alpha=0.5
    ) +
    geom_point(
        data=data_non_highs_won,
        aes(x=as.numeric(longitude),
            y=as.numeric(latitude),
            ## size=INTERNET_2010),
            size=INTERNET_2010),
        color="green",
        shape=16,  # Circle
        ## size=3,
        alpha=0.5
    )
## geom_density2d(
##     data=data_highs,
##     aes(x=as.numeric(longitude),
##         y=as.numeric(latitude)),
##     color="red",
##     size=1
## ) +
## stat_binhex(
##     data=data_non_highs,
##     aes(x=as.numeric(longitude),
##         y=as.numeric(latitude)),
##     alpha=0.8,
##     bins=nrow(data_non_highs)/30
## )

print(map_plot)
