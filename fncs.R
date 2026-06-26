tawn <- data.frame(
    year = c(1991, 1994, 1995, 1997, 2000, 2003, 2005, 2006, 2007, 2009,
             2010, 2012, 2013, 2014, 2015, 2016, 2018, 2019, 2020, 2021, 2026),
    n_students = c(1, 1, 2, 3, 1, 4, 1, 1, 5, 1,
                   1, 1, 2, 1, 2, 5, 2, 4, 1, 3, 6)
)

years_full <- 1991:2026

tawn_full <- data.frame(
    year = years_full,
    n_students = sapply(years_full, function(y) {
        if (y %in% tawn$year) {
            tawn$n_students[tawn$year == y]
        } else {
            0
        }
    })
)

plot(tawn_full$year, tawn_full$n_students, type = "h",
     xlab = "Year", ylab = "Number of PhD students",
     main = "Annual Tawn PhD Output (including 2026)")


tawn <- data.frame(
    year = c(1991, 1994, 1995, 1997, 2000, 2003, 2005, 2006, 2007, 2009,
             2010, 2012, 2013, 2014, 2015, 2016, 2018, 2019, 2020, 2021),
    n_students = c(1, 1, 2, 3, 1, 4, 1, 1, 5, 1,
                   1, 1, 2, 1, 2, 5, 2, 4, 1, 3)
)

years_full <- 1991:2021

tawn_full <- data.frame(
    year = years_full,
    n_students = sapply(years_full, function(y) {
        if (y %in% tawn$year) {
            tawn$n_students[tawn$year == y]
        } else {
            0
        }
    })
)

X <- tawn_full$n_students

library(ismev)

gev_fit <- gev.fit(X)
gev_fit

gev.rl(gev_fit)

# Return periods

T <- seq(2, 50, by = 1)

mu <- gev_fit$mle[1]
sigma <- gev_fit$mle[2]
xi <- gev_fit$mle[3]

# Return level function
z_T <- if (abs(xi) > 1e-6) {
    mu + (sigma/xi) * ((-log(1 - 1/T))^(-xi) - 1)
} else {
    mu - sigma * log(-log(1 - 1/T))
}

plot(T, z_T, type = "l", lwd = 2,
     xlab = "Return period (years)",
     ylab = "Return level (students/year)",
     main = "GEV Return Level Plot for Tawn PhD Output")

abline(h = max(X), col = "red", lty = 2)
points(rep(1, length(X)), X, pch = 16, cex = 0.6)

# Create a data frame with student names
students <- c(
    "Stuart Coles", "Saralees Nadarajah", "Mark Dixon", "Anthony Ledford",
    "Paola Bortot", "Louise Harper", "Mike Robinson", "Miguel Ancona-Navarrete",
    "Carl Scarrott", "Fabrizio Laurini", "Christopher Ferro", "Alec Stephenson",
    "Adam Butler", "Mark Latham", "Tilman Payer", "Caroline Keef", "Bakri Adam",
    "Paul Smith", "Emma Eastoe", "David Wyncoll", "Sawsan Abbas", "Jennifer Wadsworth",
    "Ioannis Papastathopoulos", "Ye Liu", "Darmesah Gabda", "Ross Towe", "Hugo Winter",
    "Tom Flowerdew", "George Foulds", "Christian Rohrbeck", "Monika Leng", "Paul Sharkey",
    "Thomas Lugrin", "Emma Simpson", "Christina Wright", "Robert Shooter", "Oliver Hatfield",
    "Toby Kingsman", "Anna Barlow", "Jordan Richards", "Yanyun Wu", "Harry Spearing",
    "Stan Tendijck", "Eleanor D’Arcy", "Daire Healy", "Conor Murphy"
)

# Initialize an empty column for publication counts
publication_count <- rep(NA, length(students))

c(72, 200, 147, 22, )

# Combine into a data frame
student_data <- data.frame(
    Name = students,
    Publications = publication_count,
    stringsAsFactors = FALSE
)

# Print to check
print(student_data)


# Load required library
library(extRemes)

# Original annual counts
tawn <- data.frame(
    year = c(1991, 1994, 1995, 1997, 2000, 2003, 2005, 2006, 2007, 2009,
             2010, 2012, 2013, 2014, 2015, 2016, 2018, 2019, 2020, 2021),
    n_students = c(1, 1, 2, 3, 1, 4, 1, 1, 5, 1,
                   1, 1, 2, 1, 2, 5, 2, 4, 1, 3)
)

# Define threshold
threshold <- 1.5

# Extract exceedances
exceedances <- tawn$n_students[tawn$n_students > threshold] - threshold
exceedance_years <- tawn$year[tawn$n_students > threshold]

# Create data frame for POT
exceedance_data <- data.frame(
    year = exceedance_years,
    exceedance = exceedances
)

# Fit GPD (POT) model with linear trend in location
fit <- fevd(exceedance, data = exceedance_data,
            threshold = threshold,
            type = "GP",
            location.fun = ~ year)  # linear trend in location

# View model summary
summary(fit)

# Define years for return-level prediction
pred_years <- seq(min(tawn$year), max(tawn$year), by = 1)

# Compute return levels for 5-year and 10-year events (example)
return_levels <- return.level(fit, return.period = c(5, 10),
                              newdata = data.frame(year = pred_years))

# Plot return levels
plot(pred_years, return_levels[,1], type = "l", col="blue", lwd=2,
     xlab="Year", ylab="Return Level",
     main="Return Level Plot (POT with Linear Trend in Time)")
lines(pred_years, return_levels[,2], col="red", lwd=2)
legend("topleft", legend=c("5-year", "10-year"), col=c("blue","red"), lwd=2)

# Optional: diagnostic plots
plot(fit)



======

    dat <- data.frame(
        year = c(1991,1994,1995,1997,2000,2003,2005,2006,2007,2009,
                 2010,2012,2013,2014,2015,2016,2018,2019,2020,2021),
        n_students = c(1,1,2,3,1,4,1,1,5,1,
                       1,1,2,1,2,5,2,4,1,3)
    )

dat$year_c <- dat$year - mean(dat$year)

fit <- glm(
    n_students ~ year_c,
    family = poisson(link = "log"),
    data = dat
)

summary(fit)

library(ggplot2)

dat$fit <- predict(fit, type = "response")

ggplot(dat, aes(year, n_students)) +
    geom_point(size = 2.5) +
    geom_line(aes(y = fit), colour = "steelblue", linewidth = 1.2) +
    labs(
        x = "Year",
        y = "Number of students"
    ) +
    theme_minimal(base_size = 14)

newdat <- data.frame(
    year = seq(min(dat$year), max(dat$year), length.out = 200)
)

newdat$year_c <- newdat$year - mean(dat$year)

pred <- predict(fit, newdat, type = "link", se.fit = TRUE)

newdat$fit <- exp(pred$fit)
newdat$lwr <- exp(pred$fit - 1.96 * pred$se.fit)
newdat$upr <- exp(pred$fit + 1.96 * pred$se.fit)

ggplot(dat, aes(year, n_students)) +
    geom_point(size = 2.5) +
    geom_ribbon(
        data = newdat,
        aes(x = year, ymin = lwr, ymax = upr),
        inherit.aes = FALSE,
        fill = "steelblue",
        alpha = 0.2
    ) +
    geom_line(
        data = newdat,
        aes(x = year, y = fit),
        inherit.aes = FALSE,
        colour = "steelblue",
        linewidth = 1.2
    ) +
    labs(
        x = "Year",
        y = "Number of students"
    ) +
    theme_minimal(base_size = 14)

fit_pois = fit_pois

dispersion <- sum(residuals(fit, type = "pearson")^2) / fit$df.residual
dispersion

library(survival)
library(tidyr)
library(dplyr)
library(ggplot2)

## Annual counts
dat <- data.frame(
    year = c(1991,1994,1995,1997,2000,2003,2005,2006,2007,2009,
             2010,2012,2013,2014,2015,2016,2018,2019,2020,2021),
    n_students = c(1,1,2,3,1,4,1,1,5,1,
                   1,1,2,1,2,5,2,4,1,3)
)

## Expand to one row per graduate
graduates <- dat |>
    uncount(n_students) |>
    mutate(status = 1)

## Add six right-censored students
censored <- data.frame(
    year = rep(2021, 6),
    status = 0
)

surv_dat <- bind_rows(graduates, censored)

## Fit Weibull survival model
fit <- survreg(
    Surv(year, status) ~ 1,
    data = surv_dat,
    dist = "weibull"
)

summary(fit)

library(ggplot2)

## ------------------------------
## Poisson regression predictions
## ------------------------------

newdat <- data.frame(
    year = seq(min(dat$year), 2030, by = 0.1)
)

newdat$year_c <- newdat$year - mean(dat$year)

newdat$poisson <-
    predict(fit_pois,
            newdata = newdat,
            type = "response")

## ------------------------------
## Survival model predictions
## ------------------------------

N <- sum(dat$n_students) + 6

shape <- 1 / fit_surv$scale
scale <- exp(coef(fit_surv))

F0 <- pweibull(newdat$year,
               shape = shape,
               scale = scale)

F1 <- pweibull(newdat$year + 1,
               shape = shape,
               scale = scale)

newdat$survival <- N * (F1 - F0)

## ------------------------------
## Plot
## ------------------------------

ggplot(dat, aes(year, n_students)) +

    geom_point(size = 2.8) +

    geom_line(
        data = newdat,
        aes(y = poisson, colour = "Poisson regression"),
        linewidth = 1.2
    ) +

    geom_line(
        data = newdat,
        aes(y = survival, colour = "Survival model"),
        linewidth = 1.2
    ) +

    scale_colour_manual(
        values = c(
            "Poisson regression" = "#0072B2",
            "Survival model" = "#D55E00"
        ),
        name = NULL
    ) +

    labs(
        x = "Year",
        y = "Graduates per year"
    ) +

    theme_minimal(base_size = 15)

