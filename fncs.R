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
threshold <- 2.5

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
