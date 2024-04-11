library(readr)
library(dplyr)
library(ggplot2)

# new_wd <- "C:/Users/madou/OneDrive - UCLA IT Services/Stat_Crunch_Competition/"
# setwd(new_wd)

Twitch23 <-
  read.csv(
    "Twitch_Streamer_Data_2023.csv"
  )

Outliers <-  Twitch23$Mean.weekly.stream.hours <= 60

Twitch_partnered <- Twitch23[as.logical(Twitch23$Partnered) & Outliers,]
 
Twitch_not_partnered <- Twitch23[!Twitch23$Partnered & Outliers,]

Twitch23_sample <-
  rbind(
    Twitch_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered,
    Twitch_not_partnered
    )

ggplot(Twitch23_sample, aes(x = Mean.weekly.stream.hours, y = Partnered)) +
  geom_point(position = position_jitter(height = 0.05, seed = 12)) + 
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial")) +
  geom_hline(yintercept = 0.5)
  # labs(x = "Predictor Variable 1", y = "Predicted Probability") +
  ggtitle("Logistic Regression Visualization")

# ################################## #
# Logistic Regression ####
# ################################## #

twitch_sample_logit <- 
  Twitch23_sample |>  glm(
    family = 'binomial',
    formula = Partnered ~ sqrt(Mean.weekly.stream.hours)
  )


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

