# read the data, change the address if you need to reproduce
bixi4_part1 <- read.csv("C:/Users/MossY/OneDrive/Desktop/Statistic Modelling/project 1/bixi4_part1.csv")
head(bixi4_part1)

# package
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(gridExtra)) install.packages("gridExtra")
if (!require(broom)) install.packages("broom")
if (!require(dplyr)) install.packages("dplyr")
if (!require(stargazer)) install.packages("stargazer")
if (!require(modelsummary))  install.packages("modelsummary")

library(ggplot2)
library(ggExtra)
library(broom)
library(dplyr)
library(stargazer)
library(modelsummary)

# add variables
bixi4_part1$is_weekend <- factor(ifelse(bixi4_part1$jj >= 6, "Yes", "No"), levels = c("No","Yes"))
head(bixi4_part1$is_weekend)

bixi4_part1$winter_idx <- bixi4_part1$mm %in% c(12,1,2)
bixi4_part1$spring_idx <- bixi4_part1$mm %in% c(3,4,5)
bixi4_part1$summer_idx <- bixi4_part1$mm %in% c(6,7,8)
bixi4_part1$fall_idx <- bixi4_part1$mm %in% c(9,10,11)
bixi4_part1$season[bixi4_part1$winter_idx] <- "Winter"
bixi4_part1$season[bixi4_part1$spring_idx] <- "Spring"
bixi4_part1$season[bixi4_part1$summer_idx] <- "Summer"
bixi4_part1$season[bixi4_part1$fall_idx] <- "Fall"
bixi4_part1$season <- factor(bixi4_part1$season, levels = c("Winter", "Spring", "Summer", "Fall"))
head(bixi4_part1$season)

bixi4_part1$raining_type <- factor(ifelse(bixi4_part1$precip > 0, "Yes", "No"), levels = c("No","Yes"))
head(bixi4_part1$raining_type)

bixi4_part1$mean_lat <- mean(bixi4_part1$lat, na.rm = TRUE)
bixi4_part1$lat_centered <- bixi4_part1$lat - bixi4_part1$mean_lat

bixi4_part1$mean_long <- mean(bixi4_part1$long, na.rm = TRUE)
bixi4_part1$long_centered <- bixi4_part1$long - bixi4_part1$mean_long

# check the relationship by plots
x_vars <- c("temp", "raining_type", "is_weekend", "season", "arrondissement", "lat_centered", "long_centered")

create_plot <- function(x_var) {
  ggplot(bixi4_part1, aes(x = .data[[x_var]], y = .data[["dur"]]))  +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = paste("dur vs", x_var),
         x = x_var, y = "Trip Duration") }
plot_list <- lapply(x_vars, create_plot)
grid.arrange(grobs = plot_list, ncol = 2)

################################################################

# fit the model
mod <- lm(dur ~ temp + raining_type + is_weekend + as.factor(season) +
            lat_centered + I(lat_centered^2) + long_centered + I(long_centered^2) +
            as.factor(arrondissement) * lat_centered + as.factor(arrondissement) * long_centered +
            is_weekend*temp + is_weekend*raining_type
            , data=bixi4_part1)
summary(mod)

# test for global effect

# full vs null
null <- lm(dur ~ 1,data=bixi4_part1)
summary(null)

anova(null)
anova(mod,null)

# full vs full_without_is_weekend # Pr(>F) 0.04637 reject
mod.no.is_weekend <- lm(dur ~ temp + raining_type + as.factor(season) +
                          lat_centered + I(lat_centered^2) + long_centered + I(long_centered^2) +
                          as.factor(arrondissement) * lat_centered + as.factor(arrondissement) * long_centered
                        , data=bixi4_part1)
summary(mod.no.is_weekend)
anova(mod.no.is_weekend)
anova(mod,mod.no.is_weekend)

# full vs full_without_raining_type # Pr(>F) 0.3242 not reject
mod.no.raining_type <- lm(dur ~ temp + is_weekend + as.factor(season) +
                            lat_centered + I(lat_centered^2) + long_centered + I(long_centered^2) +
                            as.factor(arrondissement) * lat_centered + as.factor(arrondissement) * long_centered +
                            is_weekend*temp
                          , data=bixi4_part1)
summary(mod.no.raining_type)
anova(mod.no.raining_type)
anova(mod,mod.no.raining_type)

# full vs full_without_week_weather_interaction 0.9418 not reject
mod.no.week_weather_interaction <- lm(dur ~ temp + raining_type + is_weekend + as.factor(season) +
                                        lat_centered + I(lat_centered^2) + long_centered + I(long_centered^2) +
                                        as.factor(arrondissement) * lat_centered + as.factor(arrondissement) * long_centered
                                      , data=bixi4_part1)
summary(mod.no.week_weather_interaction)
anova(mod.no.week_weather_interaction)
anova(mod,mod.no.week_weather_interaction)

# fit the model
mod1 <- lm(dur ~ temp + is_weekend + as.factor(season) +
            lat_centered + I(lat_centered^2) + long_centered + I(long_centered^2) +
            as.factor(arrondissement) * lat_centered + as.factor(arrondissement) * long_centered
          , data=bixi4_part1)
summary(mod1)

################################################################

# outlier
boxplot(bixi4_part1$dur)
boxplot(bixi4_part1$temp)

boxplot_stats <- boxplot.stats(bixi4_part1$dur)
outliers <- boxplot_stats$out
Q1 <- quantile(bixi4_part1$dur, 0.25, na.rm = TRUE)
Q3 <- quantile(bixi4_part1$dur, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
bixi4_part1$is_outlier <- ifelse(bixi4_part1$dur < lower_bound | bixi4_part1$dur > upper_bound, 1, 0)
bixi4_part1$dur_winsorized <- bixi4_part1$dur
bixi4_part1$dur_winsorized[bixi4_part1$dur_winsorized < lower_bound] <- lower_bound
bixi4_part1$dur_winsorized[bixi4_part1$dur_winsorized > upper_bound] <- upper_bound

boxplot_stats <- boxplot.stats(bixi4_part1$temp)
outliers <- boxplot_stats$out
Q1 <- quantile(bixi4_part1$temp, 0.25, na.rm = TRUE)
Q3 <- quantile(bixi4_part1$temp, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
bixi4_part1$is_outlier <- ifelse(bixi4_part1$temp < lower_bound | bixi4_part1$temp > upper_bound, 1, 0)
bixi4_part1$temp_winsorized <- bixi4_part1$temp
bixi4_part1$temp_winsorized[bixi4_part1$temp_winsorized < lower_bound] <- lower_bound
bixi4_part1$temp_winsorized[bixi4_part1$temp_winsorized > upper_bound] <- upper_bound


mod2 <- lm(dur_winsorized ~ temp_winsorized + is_weekend + as.factor(season) +
             lat_centered + I(lat_centered^2) + long_centered + I(long_centered^2) +
             as.factor(arrondissement) * lat_centered + as.factor(arrondissement) * long_centered
           , data=bixi4_part1)
summary(mod2)

################################################################

summary(mod)$adj.r.squared #0.04840765
summary(mod1)$adj.r.squared #0.04853967
summary(mod2)$adj.r.squared #0.04984073