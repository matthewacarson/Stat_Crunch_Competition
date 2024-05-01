sapply(c('readr', 'dplyr', 'ggplot2'), FUN = require, character.only = T)
# library(ggridges)

new_wd <- "C:/Users/madou/OneDrive - UCLA IT Services/Stat_Crunch_Competition/"
setwd(new_wd)

Twitch23 <- read.csv("Twitch_Streamer_Data_2023.csv")

Twitch23 |> 
  plot(Average.viewers ~ Mean.weekly.stream.hours, data = _)

viewers_stream_lm <- Twitch23 |> 
  lm(1/Average.viewers ~ Mean.weekly.stream.hours, data = _)

median_stream_hours <- median(Twitch23$Mean.weekly.stream.hours)

viewers_stream_predict <- data.frame(
  Mean.weekly.stream.hours = seq(median_stream_hours - 30, median_stream_hours + 100, 5)
)

viewers_stream_predict$Prediction <- 1/predict(viewers_stream_lm, viewers_stream_predict)


for (i in c(1,2,3,5)) {
  followers_stream_lm <- Twitch23 |> 
    lm(log(Followers) ~ Mean.weekly.stream.hours, data = _)
  
  plot(followers_stream_lm, which = i)
  
  followers_stream_lm <- Twitch23 |> 
    lm(log(Followers) ~ log(Mean.weekly.stream.hours), data = _)
  
  plot(followers_stream_lm, which = i)
}


followers_avg_viewers_mature_lm <- Twitch23 |> 
  lm(
    log(Followers) ~ log(Average.viewers) + Mature, data = _
  )

followers_watch_hrs_mature_lm <- Twitch23 |> 
  lm(
    log(Followers) ~ Mean.weekly.watch.hours + Mature, data = _
  )

followers_watch_hrs_avg_viewers_mature_lm <- Twitch23 |> 
  lm(
    log(Followers) ~ log(Mean.weekly.watch.hours) + log(Average.viewers) + Mature, data = _
  )

# ################################## #
# Logistic Regression ####
# ################################## #

twitch_logit_best_model <-
  Twitch23 |> glm(
    family = binomial,
    formula = Partnered ~ Followers...100000  # AIC: 237.57

      # Peak.viewers # AIC: 245.79 -- BIC: 255.3945
      # Average.viewers # AIC: 246.42 -- BIC: 256.0215
      # Mature # AIC: 246.22 -- BIC: 255.8209
      # Mean.weekly.watch.hours # AIC: 245.77 -- BIC: 255.3718
      # Mean.weekly.stream.hours # AIC: 246.36 -- BIC: 255.9618
)
AIC(twitch_logit_best_model)
summary(twitch_logit_best_model)

predict(twitch_logit_best_model, newdata = data.frame(Followers = 100000), type = 'response')



####
twitch_logit_best_model <-
  Twitch23 |> glm(
    family = binomial,
    formula = Partnered ~ Followers + Peak.viewers)
AIC(twitch_logit_best_model) # 238.8016

twitch_logit_best_model <-
  Twitch23 |> glm(
    family = binomial,
    formula = Partnered ~ Followers + Average.viewers)
AIC(twitch_logit_best_model) # 237.6662

twitch_logit_best_model <-
  Twitch23 |> glm(
    family = binomial,
    formula = Partnered ~ Followers + Mature)
AIC(twitch_logit_best_model) # 239.0717

twitch_logit_best_model <-
  Twitch23 |> glm(
    family = binomial,
    formula = Partnered ~ Followers + Mean.weekly.watch.hours)
AIC(twitch_logit_best_model) # 239.0692

twitch_logit_best_model <-
  Twitch23 |> glm(
    family = binomial,
    formula = Partnered ~ Followers + Mean.weekly.stream.hours)
AIC(twitch_logit_best_model) # 238.3238

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


poisson_viewers_stream <- Twitch23 |>  glm(
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

