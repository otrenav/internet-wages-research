##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##

rm(list=ls(all=TRUE))
setwd("~/Projects/itam/internet_wages/")
require(stargazer)

original_data <- read.csv(
    "./data/2010_2015_aggregated.csv",
    header=TRUE
)

## Filter data to remove very high DIFF_INGTRMEN
data <- original_data[original_data$DIFF_INGTRMEN <= 2, ]
## data <- original_data

##
## Graph: distributions for internet 2010 and 2015 together
##
high_data <- data
high_data <- high_data[high_data$HIGH_INGTRMEN_2010 == 1, ]
high_data <- high_data[high_data$HIGH_INTERNET_2010 == 1, ]
high_data <- high_data[high_data$HIGH_NIVACAD_2010  == 1, ]
high_data <- high_data[high_data$HIGH_NUMPERS_2010  == 1, ]

plot(density(data$INTERNET_2010, na.rm=TRUE), col="red", lwd=2, main="")
lines(density(high_data$INTERNET_2010, na.rm=TRUE), col="blue", lwd=2)

high_data <- data
high_data <- high_data[high_data$HIGH_INGTRMEN_2015 == 1, ]
high_data <- high_data[high_data$HIGH_INTERNET_2015 == 1, ]
high_data <- high_data[high_data$HIGH_NIVACAD_2015  == 1, ]
high_data <- high_data[high_data$HIGH_NUMPERS_2015  == 1, ]

lines(density(data$INTERNET_2015, na.rm=TRUE), col="black", lwd=2)
lines(density(high_data$INTERNET_2015, na.rm=TRUE), col="orange", lwd=2)
legend(
    0.45, 8,
    legend=c(
        "All municipalities (2010)",
        "Privileged municipalities (2010)",
        "All municipalities (2015)",
        "Privileged municipalities (2015)"
    ),
    lty=c(1, 1, 1, 1), lwd=c(2, 2, 2, 2),
    col=c("blue", "red", "black", "orange")
)

##
## Graph: distributions for population 2010 and 2015 together
##
## high_data <- data
## high_data <- high_data[high_data$HIGH_INGTRMEN_2010 == 1, ]
## high_data <- high_data[high_data$HIGH_INTERNET_2010 == 1, ]
## high_data <- high_data[high_data$HIGH_NIVACAD_2010  == 1, ]
## high_data <- high_data[high_data$HIGH_NUMPERS_2010  == 1, ]

## plot(density(data$NUMPERS_2010), col="red", lwd=2,
##      main="Density of population")
## lines(density(high_data$NUMPERS_2010), col="blue", lwd=2)

## high_data <- data
## high_data <- high_data[high_data$HIGH_INGTRMEN_2015 == 1, ]
## high_data <- high_data[high_data$HIGH_INTERNET_2015 == 1, ]
## high_data <- high_data[high_data$HIGH_NIVACAD_2015  == 1, ]
## high_data <- high_data[high_data$HIGH_NUMPERS_2015  == 1, ]

## lines(density(data$NUMPERS_2015), col="black", lwd=2)
## lines(density(high_data$NUMPERS_2015), col="orange", lwd=2)
## legend(
##     6e04, 0.00025,
##     legend=c(
##         "All municipalities (2010)",
##         "Privileged municipalities (2010)",
##         "All municipalities (2015)",
##         "Privileged municipalities (2015)"
##     ),
##     lty=c(1, 1, 1, 1), lwd=c(2, 2, 2, 2),
##     col=c("blue", "red", "black", "orange")
## )

##
## Graph: distributions for income change percentage
##

## 2010
## high_data <- data
## high_data <- high_data[high_data$HIGH_INGTRMEN_2010 == 1, ]
## high_data <- high_data[high_data$HIGH_INTERNET_2010 == 1, ]
## high_data <- high_data[high_data$HIGH_NIVACAD_2010  == 1, ]
## high_data <- high_data[high_data$HIGH_NUMPERS_2010  == 1, ]
## plot(density(high_data$DIFF_INGTRMEN), col="blue", lwd=2,
##      main="Density of income change percentage 2010-2015")
## lines(density(data$DIFF_INGTRMEN, na.rm=TRUE), col="red", lwd=2)
## legend(
##     0.4, 3.5,
##     legend=c(
##         "All municipalities",
##         "Privileged municipalities"
##     ),
##     lty=c(1, 1), lwd=c(2, 2),
##     col=c("blue", "red")
## )

##
## Graph: diff_income vs internet_percentage
##

## 2010
high_data <- data
high_data <- high_data[high_data$HIGH_INGTRMEN_2010 == 1, ]
high_data <- high_data[high_data$HIGH_INTERNET_2010 == 1, ]
high_data <- high_data[high_data$HIGH_NIVACAD_2010  == 1, ]
high_data <- high_data[high_data$HIGH_NUMPERS_2010  == 1, ]

regression_basic <- lm(
    data$DIFF_INGTRMEN ~ data$INTERNET_2010
)
regression_basic_with_weights <- lm(
    data$DIFF_INGTRMEN ~ data$INTERNET_2010,
    weights=data$NUMPERS_2010
)
regression_high <- lm(
    high_data$DIFF_INGTRMEN ~ high_data$INTERNET_2010
)
regression_high_with_weights <- lm(
    high_data$DIFF_INGTRMEN ~ high_data$INTERNET_2010,
    weights=high_data$NUMPERS_2010
)

plot(data$INTERNET_2010, data$DIFF_INGTRMEN, col="black",
     main="",
     xlab="Penetración de internet en hogares", ylab="Crecimiento porcentual del ingreso de 2010 a 2015")
points(data[data$NUMPERS_2010 > 10000, "INTERNET_2010"],
       data[data$NUMPERS_2010 > 10000, "DIFF_INGTRMEN"], col="orange")
points(data[data$DIFF_INGTRMEN < 0, "INTERNET_2010"],
       data[data$DIFF_INGTRMEN < 0, "DIFF_INGTRMEN"], col="red")
points(data[data$DIFF_INGTRMEN > 1, "INTERNET_2010"],
       data[data$DIFF_INGTRMEN > 1, "DIFF_INGTRMEN"], col="green")
points(high_data$INTERNET_2010, high_data$DIFF_INGTRMEN, col="blue")

abline(regression_basic, col="black", lwd=2, lty=2)
abline(regression_basic_with_weights, col="black", lwd=2)
abline(regression_high, col="blue", lwd=2, lty=2)
abline(regression_high_with_weights, col="blue", lwd=2)
legend(
    0.3, 2,
    legend=c(
        "Todos los municipios",
        "Crecimiento porcentual < 0",
        "Crecimiento porcentual > 1",
        "Municipios privilegiados",
        "Con más de 10,000 personas",
        "Todos los municipios con ponderación uniforme",
        "Todos los municipios con ponderación por población",
        "Municipios privilegiados con ponderación uniforme",
        "Municipios privilegiados con ponderación por población"
    ),
    lty=c(NA, NA, NA, NA, NA, 2, 1, 2, 1),
    lwd=c(NA, NA, NA, NA, NA, 2, 2, 2, 2),
    pch=c(1, 1, 1, 1, 1, NA, NA, NA, NA),
    col=c("black", "red", "green", "blue", "orange", "black", "black", "blue", "blue")
)
summary(regression_high_with_weights)

##
## Regression: diff_income = everything
##

## data <- data[data$DIFF_INGTRMEN < Inf, ]
## data <- data[data$DIFF_NIVACAD < Inf, ]
## data <- data[data$DIFF_PRIMARIA < Inf, ]
## data <- data[data$DIFF_PREPARATORIA < Inf, ]
## data <- data[data$DIFF_LICENCIATURA < Inf, ]
## data <- data[data$DIFF_NUMPERS < Inf, ]
## data <- data[data$DIFF_CELULAR < Inf, ]
## data <- data[data$DIFF_INTERNET < Inf, ]
## ## data <- data[data$DIFF_INGTRMEN < 1, ]
## ## data <- data[data$NUMPERS_2010 > 5000, ]

## regression <- lm(
##     DIFF_INGTRMEN ~
##         ## NIVACAD_2010 +
##         ## ING_AYUGOB_2010 +
##         ## ING_OTROPAIS_2010 +
##         ## ING_DENTROPAIS_2010 +
##         ## COMPUTADORA_2010 +
##         ## TELEFONO_2010 +
##         ## AUTOPROPIO_2010 +
##         ## ELECTRICIDAD_2010 +
##         ## HLENGUA_2010 +
##         EDAD_2010 +
##         PRIMARIA_2010 +
##         PREPARATORIA_2010 +
##         LICENCIATURA_2010 +
##         NUMPERS_2010 +
##         CELULAR_2010 +
##         INTERNET_2010,
##   data=data,
##   weights=NUMPERS_2010)
## summary(regression)

##
## Regression: diff_income = diff_everything
##

## regression <- lm(
##     DIFF_INGTRMEN ~
##         DIFF_PRIMARIA +
##         DIFF_PREPARATORIA +
##         DIFF_LICENCIATURA +
##         DIFF_NUMPERS +
##         DIFF_CELULAR +
##         DIFF_INTERNET,
##     data=data,
##     weights=NUMPERS_2010
## )
## summary(regression)

##
## Regression: diff_income = everything + interactions
##
regression_highs <- lm(
    DIFF_INGTRMEN ~
        INTERNET_2010:HIGH_NIVACAD_2010 +
        ## INTERNET_2010:HIGH_LICENCIATURA_2010 +
        INTERNET_2010:HIGH_INTERNET_2010 +
        INTERNET_2010:HIGH_NUMPERS_2010 +
        INTERNET_2010:HIGH_ALL_2010 +
        INTERNET_2010,
  data=data,
  weights=NUMPERS_2010)
summary(regression_highs)

regression_all <- lm(
    DIFF_INGTRMEN ~
        ## NIVACAD_2010 +
        ING_AYUGOB_2010 +
        ING_OTROPAIS_2010 +
        ING_DENTROPAIS_2010 +
        COMPUTADORA_2010 +
        TELEFONO_2010 +
        AUTOPROPIO_2010 +
        ELECTRICIDAD_2010 +
        HLENGUA_2010 +
        EDAD_2010 +
        PRIMARIA_2010 +
        PREPARATORIA_2010 +
        LICENCIATURA_2010 +
        NUMPERS_2010 +
        CELULAR_2010 +
        INTERNET_2010:HIGH_NIVACAD_2010 +
        ## INTERNET_2010:HIGH_LICENCIATURA_2010 +
        INTERNET_2010:HIGH_INTERNET_2010 +
        INTERNET_2010:HIGH_NUMPERS_2010 +
        INTERNET_2010:HIGH_ALL_2010 +
        INTERNET_2010,
  data=data,
  weights=NUMPERS_2010)
summary(regression_all)

stargazer(regression_highs, regression_all, title="Resultados", align=TRUE)

##
## Print information tables for Latex
##
## summary(data[, c(
##     "INGTRMEN_2010", "INGTRMEN_2015",
##     "NIVACAD_2010", "NIVACAD_2015",
##     "PRIMARIA_2010", "PRIMARIA_2015",
##     "PREPARATORIA_2010", "PREPARATORIA_2015",
##     "LICENCIATURA_2010", "LICENCIATURA_2015",
##     "EDAD_2010", "EDAD_2015",
##     "HLENGUA_2010", "HLENGUA_2015",
##     "COMPUTADORA_2010", "COMPUTADORA_2015",
##     "INTERNET_2010", "INTERNET_2015",
##     "CELULAR_2010", "CELULAR_2015",
##     "NUMPERS_2010", "NUMPERS_2015"
## )])

stargazer(data[, c(
    "INGTRMEN_2010", "INGTRMEN_2015",
    "NIVACAD_2010", "NIVACAD_2015",
    "PRIMARIA_2010", "PRIMARIA_2015",
    "PREPARATORIA_2010", "PREPARATORIA_2015",
    "LICENCIATURA_2010", "LICENCIATURA_2015",
    "EDAD_2010", "EDAD_2015",
    "HLENGUA_2010", "HLENGUA_2015",
    "COMPUTADORA_2010", "COMPUTADORA_2015",
    "INTERNET_2010", "INTERNET_2015",
    "CELULAR_2010", "CELULAR_2015",
    "NUMPERS_2010", "NUMPERS_2015"
)], align=TRUE)

high_variables <- c(
    "HIGH_INGTRMEN_2010", "HIGH_INGTRMEN_2015",
    "HIGH_NIVACAD_2010", "HIGH_NIVACAD_2015",
    "HIGH_INTERNET_2010", "HIGH_INTERNET_2015",
    "HIGH_NUMPERS_2010", "HIGH_NUMPERS_2015",
    "HIGH_ALL_2010", "HIGH_ALL_2015"
)
for (variable in high_variables) {
    print(paste(variable, " SI: ", sum(data[, variable], na.rm=TRUE), sep=""))
    print(paste(variable, " NO: ", nrow(data) - sum(data[, variable], na.rm=TRUE), sep=""))
}
nrow(data)
