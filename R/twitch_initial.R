sapply(c('readr', 'dplyr', 'ggplot2'), FUN = require, character.only = T)
library(ggridges)

# new_wd <- "C:/Users/madou/OneDrive - UCLA IT Services/Stat_Crunch_Competition/"
# setwd(new_wd)

Twitch23 <- read.csv("Twitch_Streamer_Data_2023.csv")

# Twitch23$`Mean.weekly.stream.hours^0.15` <-  transformTukey(Twitch23$Mean.weekly.stream.hours, plotit = TRUE)

# Include <-  Twitch23$Mean.weekly.stream.hours <= max(Twitch23$Mean.weekly.stream.hours)

max_stream_hours <- 58

# ################################## #
# Logistic Regression ####
# ################################## #

twitch_logit <- 
  Twitch23 |> glm(
    family = 'binomial',
    formula = Partnered ~ Mean.weekly.stream.hours
  )

summary(twitch_logit)

autoplot(twitch_logit)

twitch_logit_no_outliers <- 
  Twitch23 |> filter(Mean.weekly.stream.hours <= max_stream_hours) |>  glm(
    family = 'binomial',
    formula = Partnered ~ Mean.weekly.stream.hours
  )

summary(twitch_logit_no_outliers)


# Trying with all variables

twitch_logit_best_model <-
  Twitch23 |> filter(Followers.gained < 320000) |> glm(
    family = 'binomial',
    formula = Partnered ~ 
      Followers.gained  # AIC: 246.53
      # Followers # AIC: 237.57
      # Peak.viewers # AIC: 245.79
      # Average.viewers # AIC: 246.42
      # Mature # AIC: 246.22
      # Mean.weekly.watch.hours # AIC: 245.77
      # Mean.weekly.stream.hours # AIC: 246.36
  )

summary(twitch_logit_best_model)

# Call:
#   glm(formula = Partnered ~ Followers.gained + Followers, family = "binomial", 
#       data = filter(Twitch23, Followers < 1000000))
# 
# Coefficients:
#   Estimate   Std. Error z value Pr(>|z|)    
# (Intercept)       2.985888702  0.381125013   7.834 4.71e-15 ***
#   Followers.gained -0.000007139  0.000001786  -3.997 6.41e-05 ***
#   Followers         0.000002561  0.000001170   2.189   0.0286 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 219.7  on 666  degrees of freedom
# Residual deviance: 203.4  on 664  degrees of freedom
# AIC: 209.4
# 
# Number of Fisher Scoring iterations: 6



ggplot(Twitch23 |> filter(Followers.gained < 320000), aes(x = Followers.gained, y = Partnered)) +
  geom_density_ridges(aes(group = Partnered, scale = ifelse(Partnered, 0.25, -0.25)), fill = 'gray') +
  geom_hline(yintercept = 1) + geom_hline(yintercept = 0) +
  geom_point(aes(y = ifelse(Partnered, Partnered + 0.03, Partnered - 0.03)), position = position_jitter(height = 0.03, seed = 12), color = 'red', alpha = 0.3) +
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial"))


# ################################## #
# Logistic Regression Plots ####
# ################################## #


ggplot(Twitch23, aes(x = Mean.weekly.stream.hours, y = Partnered)) + 
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial")) + geom_density_ridges()

ggplot(Twitch23 |> filter(Mean.weekly.stream.hours <= max_stream_hours), aes(x = Mean.weekly.stream.hours, y = Partnered)) +
  geom_point(position = position_jitter(height = 0.05, seed = 12)) + 
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial"))
  # labs(x = "Predictor Variable 1", y = "Predicted Probability") +
  # ggtitle("Logistic Regression Visualization")


ggplot(Twitch23, aes(x = Mean.weekly.stream.hours, y = Partnered)) +
  geom_density_ridges(aes(group = Partnered, scale = ifelse(Partnered, 0.25, -0.25)), fill = 'gray') +
  geom_hline(yintercept = 1) + geom_hline(yintercept = 0) +
  # geom_point(position = position_jitter(height = 0.05, seed = 12), color = 'gray25') +
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial"))
# ################################## #
# Poisson Regression ####
# ################################## #


poisson_stream_viewers <- Twitch23 |>  glm(
  family = 'poisson',
  data = _,
  `Average viewers` ~ `Stream time (hrs)`
)




# ################################## #
# Linear Regression ####
# ################################## #
formula <- `Average viewers` ~ `Stream time`

lm_stream_avg_viewers <-
  lm(formula,
     data = Twitch23 #[
       # Twitch23$`Stream time (hrs)` <= 7500,
     # ]
     )

plot(formula, data = Twitch23[
  Twitch23$`Stream time (hrs)` <= 7500,
], pch = 19, cex = 0.5);abline(lm_stream_avg_viewers, col = 'red', lwd = 2)

outliers <- function(x, bound) {
  iqr <- IQR(x)
  if (bound == 'lower') {
    q1 <- quantile(x, 0.25)
    lower_limit <- q1 - 1.5 * iqr
    return(lower_limit)
  } else if (bound == 'upper') {
    q3 <- quantile(x, 0.75)
    upper_limit <- q3 + 1.5 * iqr
    return(upper_limit)
  } 
}



pairs(
  Twitch23 |> select(
    -ID, -Channel, -Language, -`Watch time`, -`Stream time`,
    -`Followers Prev Yr`, -Partnered, -Mature
  ) |> 
    filter(
      `Peak viewers` <= outliers(`Peak viewers`, 'upper'),
      `Average viewers` <= outliers(`Average viewers`, 'upper'),
      Followers <= outliers(Followers, 'upper'),
      `Followers gained` <= outliers(`Followers gained`, 'upper'),
      `Followers gained percent` <= outliers(`Followers gained percent`, 'upper'),
      `Mean weekly watch hours` < 150000,
      `Mean weekly stream hours` <= outliers(`Mean weekly stream hours`, 'upper')
      )
)


transformed_model <- car::powerTransform(lm_stream_avg_viewers)













# Extract studentized residuals from the model
student_resid <- rstandard(lm_stream_avg_viewers)

# Sort the studentized residuals
sorted_resid <- sort(student_resid)

# Generate theoretical quantiles for a standard normal distribution
theoretical_quantiles <- qnorm(ppoints(length(sorted_resid)))

lm_1 <- lm(sorted_resid ~ theoretical_quantiles)

sorted_resid_flat <-
  sorted_resid - (
    theoretical_quantiles * lm_1$coefficients['theoretical_quantiles'])
z_scores <- sorted_resid_flat
# z_scores <- scale(sorted_resid_flat)
# z_scores <- (sorted_resid- mean(sorted_resid)) / sd(sorted_resid)

# Create a residual Q-Q plot
plot(
  theoretical_quantiles,
  z_scores,
  main = "Residual Plot of the Q-Q Model\nStandardized residuals from the standard normal distribution",
  xlab = "Theoretical Quantiles",
  ylab = "Standardized residuals of the Q-Q Model",
  ylim = c(-1, 4)
)
abline(h = 0, col = 'red')

plot(
  lm_stream_avg_viewers,
  # which = 2,
  pch = 19,
  # cex = 0.75,
  lty = 8
)


plot(
  theoretical_quantiles,
  sorted_resid,
  # main = "Residual Plot of the Q-Q Model\nStandardized residuals from the standard normal distribution",
  xlab = "Theoretical Quantiles",
  ylab = "Standardized residuals",
  ylim = c(-4, 4)
)
abline(lm_1, col = 'red')


plot(formula = `Average viewers` ~ (`Stream time`),
     data = Twitch23)

