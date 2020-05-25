library(tidyverse)
library(splines)
library(patchwork)
library(ggridges)
x <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")

pre_covid <- filter(x, date < "2020-05-16", league == "German Bundesliga")
post_covid <- filter(x, league == "German Bundesliga", date <= Sys.Date(), date >= "2020-05-16")

### Fit Model
n <- nrow(pre_covid)
df <- tibble(
  "goals" = c(pre_covid$score1, pre_covid$score2),
  "spi" = c(pre_covid$spi1, pre_covid$spi2),
  "opp_spi" = c(pre_covid$spi2, pre_covid$spi1),
  "importance" = c(pre_covid$importance1, pre_covid$importance2),
  "opp_importance" = c(pre_covid$importance2, pre_covid$importance1),
  "home" = rep(1:0, each = n)
)

bund_model <- glm(goals ~ ns(spi, 5) + ns(opp_spi, 5) + home, data = df, family = "poisson")

predict_lambda <- function(df_row, model) {
  df <- tibble(
    "goals" = c(df_row$score1, df_row$score2),
    "spi" = c(df_row$spi1, df_row$spi2),
    "opp_spi" = c(df_row$spi2, df_row$spi1),
    "importance" = c(df_row$importance1, df_row$importance2),
    "opp_importance" = c(df_row$importance2, df_row$importance1),
    "home" = 1:0
  )
  lambdas <- predict(model, newdata = df, type = "response")
  lambdas <- tibble("lambda1" = lambdas[1], "lambda2" = lambdas[2])
  return(lambdas)
}

match_probs <- function(lambda_1, lambda_2) {
  max_goals <- 10
  score_matrix <- dpois(0:max_goals, lambda_1) %o% dpois(0:max_goals, lambda_2)
  score_matrix <- score_matrix/sum(score_matrix)
  tie_prob <- sum(diag(score_matrix))
  win_prob <- sum(score_matrix[lower.tri(score_matrix)])
  loss_prob <- sum(score_matrix[upper.tri(score_matrix)])
  return(tibble("win_prob" = win_prob, "tie_prob" = tie_prob, "loss_prob" = loss_prob))
}

get_predictions <- function(df, hfa_reduction = 0, model) {
  model$coefficients['home'] <- (1 - hfa_reduction) * model$coefficients['home']
  df <- 
    df %>%
    bind_cols(map_dfr(1:nrow(df), ~predict_lambda(df[.x,], model))) %>%
    bind_cols(map2_dfr(.$lambda1, .$lambda2, match_probs))
  df$hfa_reduction <- hfa_reduction
  return(df)
}

pre_covid <- filter(x, date < "2020-05-16", league == "German Bundesliga")
pre_covid <- get_predictions(pre_covid, 0, bund_model)

p1 <- ggplot(pre_covid, aes(x = prob1, y = win_prob)) +
  geom_point(alpha = 0.2) +
  geom_abline(lty = 2, size = 1, color = "red") +
  theme_bw() +
  theme(axis.title = element_text(size = 16, hjust = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        legend.position = "bottom") +
  labs(x = "FiveThirtyEight Probability",
       y = "Model Estimated Probability", 
       subtitle = "P(Home Team Win)") +
  scale_x_continuous(limits = c(0,1), labels = function(x) paste0(100 * x, "%")) +
  scale_y_continuous(limits = c(0,1), labels = function(x) paste0(100 * x, "%")) +
  annotate("label", x = 0.25, y = 0.75, size = 4,
           label = paste0("Correlation: ", round(cor(pre_covid$prob1, pre_covid$win_prob, use = "pairwise.complete.obs"), 2)))


p2 <- ggplot(pre_covid, aes(x = prob2, y = loss_prob)) +
  geom_point(alpha = 0.2) +
  geom_abline(lty = 2, size = 1, color = "red") +
  theme_bw() +
  theme(axis.title = element_text(size = 16, hjust = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        legend.position = "bottom") +
  labs(x = "FiveThirtyEight Probability",
       y = "Model Estimated Probability", 
       subtitle = "P(Away Team Win)") +
  scale_x_continuous(limits = c(0,1), labels = function(x) paste0(100 * x, "%")) +
  scale_y_continuous(limits = c(0,1), labels = function(x) paste0(100 * x, "%")) +
  annotate("label", x = 0.25, y = 0.75, size = 4,
           label = paste0("Correlation: ", round(cor(pre_covid$prob2, pre_covid$loss_prob, use = "pairwise.complete.obs"), 2)))


p3 <- ggplot(pre_covid, aes(x = probtie, y = tie_prob)) +
  geom_point(alpha = 0.2) +
  geom_abline(lty = 2, size = 1, color = "red") +
  theme_bw() +
  theme(axis.title = element_text(size = 16, hjust = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        legend.position = "bottom") +
  labs(x = "FiveThirtyEight Probability",
       y = "Model Estimated Probability", 
       subtitle = "P(Draw)") +
  scale_x_continuous(limits = c(0,1), labels = function(x) paste0(100 * x, "%")) +
  scale_y_continuous(limits = c(0,1), labels = function(x) paste0(100 * x, "%")) +
  annotate("label", x = 0.25, y = 0.75, size = 4,
           label = paste0("Correlation: ", round(cor(pre_covid$probtie, pre_covid$tie_prob, use = "pairwise.complete.obs"), 2)))


p1 + p2 + p3 +
  plot_annotation(title = 'Calibration of Model Estimated Probabilities',
                  theme = theme(plot.title = element_text(size = 24, hjust = 0.5))) 


ggsave("calibration.png", height = 9/1.2, width = 16/1.2)

y <- map_dfr(seq(0, 1, 0.05), ~get_predictions(post_covid, .x, bund_model))

get_exp_home_points <- function(df) {
  case_when(runif(nrow(df)) <= df$win_prob ~ 3,
            runif(nrow(df)) <= df$win_prob  + df$tie_prob ~ 1,
            T ~ 0) %>%
    sum()
}


set.seed(123)
nsims <- 10000
sim <- function(x) {
  hfa <- seq(0,1,0.05)
  exp_pts <- map_dbl(hfa, ~ get_exp_home_points(filter(y, hfa_reduction == .x)))
  return(tibble("hfa_reduction" = hfa,
                "exp_pts" = exp_pts))
}

df_sims <- map_dfr(1:nsims, sim)
post_covid <- post_covid %>%
  mutate("exp_home_points" = 3 * prob1 + probtie,
         "home_points" = 3 * (score1 > score2) + 1 * (score1 == score2),
         "home_wp" =  1 * (score1 > score2) + 0.5 * (score1 == score2),
         "exp_home_wp" =   1 * prob1 + 0.5 * probtie)
home_pts <- sum(post_covid$home_points)
ggplot(df_sims, aes(x = exp_pts, y = as.factor(hfa_reduction))) +
  geom_density_ridges(scale = 1) +
  geom_vline(xintercept = home_pts, lty = 2, size = 1.2) +
  theme_bw() +
  theme(axis.title = element_text(size = 16, hjust = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        legend.position = "bottom") +
  labs(x = "Points Accrued by Home Team",
       y = "Reduction in Home Field Advantage", 
       title = "Distribution of Expected Home Team Points w/ Varying HFA",
       subtitle = "German Bundesliga: 2020-05-16 to Present") +
  scale_y_discrete(labels = function(x) paste0(100 * as.numeric(x), "%"))

# tibble("spi" = c(post_covid$spi1, post_covid$spi2),
#        "team" = rep(c("Home", "Away"), each = 18)) %>%
#   ggplot(aes(x = spi, y = team)) +
#   geom_density_ridges(aes(fill = team), scale = 1, quantile_lines = T) + 
#   theme_bw() +
#   theme(axis.title = element_text(size = 16, hjust = 0.5),
#         plot.title = element_text(size = 24, hjust = 0.5),
#         plot.subtitle = element_text(size = 20, hjust = 0.5),
#         legend.position = "none") +
#   labs(x = "FiveThirtyEight Soccer Power Index",
#        title = "Distribution of Home/Away Power Ratings",
#        subtitle = "German Bundesliga: 2020-05-16 to Present")
# ggsave("Desktop/dist.png", height = 9/1.2, width = 16/1.2)
#   
# 
# 
# tibble("xg" = c(post_covid$xg1, post_covid$xg2),
#        "goals" = c(post_covid$score1, post_covid$score2),
#        "loc" = rep(c("Home", "Away"), each = 18),
#        "week" = paste0("Week: ", rep(rep(c(1, 2), each = 9), 2))) %>%
#   ggplot(aes(x = xg, y = goals)) +
#   geom_point(aes(color = loc)) +
#   facet_wrap(~week) +
#   geom_abline() +
#   theme_bw() +
#   theme(axis.title = element_text(size = 16, hjust = 0.5),
#         plot.title = element_text(size = 24, hjust = 0.5),
#         plot.subtitle = element_text(size = 20, hjust = 0.5),
#         legend.position = "bottom") +
#   labs(x = "Shot Based xG",
#        y = "Goals Scored",
#        color = "",
#        title = "xG vs. Goals Scored",
#        subtitle = "German Bundesliga: 2020-05-16 to Present")
# ggsave("Desktop/shot_based_xg.png", height = 9/1.2, width = 16/1.2)
# 
# tibble("xg" = c(post_covid$nsxg1, post_covid$nsxg2),
#        "goals" = c(post_covid$score1, post_covid$score2),
#        "loc" = rep(c("Home", "Away"), each = 18),
#        "week" = paste0("Week: ", rep(rep(c(1, 2), each = 9), 2))) %>%
#   ggplot(aes(x = xg, y = goals)) +
#   geom_point(aes(color = loc)) +
#   facet_wrap(~week) +
#   geom_abline() +
#   theme_bw() +
#   theme(axis.title = element_text(size = 16, hjust = 0.5),
#         plot.title = element_text(size = 24, hjust = 0.5),
#         plot.subtitle = element_text(size = 20, hjust = 0.5),
#         legend.position = "bottom") +
#   labs(x = "Non-Shot Based xG",
#        y = "Goals Scored",
#        color = "",
#        title = "xG vs. Goals Scored",
#        subtitle = "German Bundesliga: 2020-05-16 to Present")
# ggsave("Desktop/non_shot_based_xg.png", height = 9/1.2, width = 16/1.2)
#        