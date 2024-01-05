library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(stargazer)  
library(FactoMineR)
library(factoextra)
library(caret)
library(mgcv)
library(np)
library(lmtest)

set.seed(450)

# read / convert data
data <- read_csv('clean-data.csv')
data <- data %>%
  mutate(
    sex = as.numeric(as.factor(sex)), # male = 2, female = 1
    DRK_YN = as.numeric(as.factor(DRK_YN)), # 1 = no, 2 = yrs
    SMK_Y = (SMK_stat_type_cd == 3) * 1,
    SMK_N = (SMK_stat_type_cd == 1) * 1
  ) %>%
  select(
    -SMK_stat_type_cd
  )
data_pca <- data %>% select(age, height, weight, waistline, DBP, SBP, BLDS,
                            tot_chole, LDL_chole, triglyceride,
                            SGOT_AST, SGOT_ALT, gamma_GTP, hemoglobin)

# pca
pca <- PCA(data_pca, graph = FALSE)
print(summary(pca))
scr_plot <- fviz_screeplot(pca, addlabels = TRUE)
ggsave("./output/Lab4/scr_plot.png", plot = scr_plot, limitsize = FALSE, scale = 2)
var_plot_12 <- fviz_pca_var(pca, col.var = "black", axes = c(1, 2))
ggsave("./output/Lab4/var_plot_12.png", plot = var_plot_12, limitsize = FALSE, scale = 2)
bi_plot_12 <- fviz_pca_biplot(pca, label = "var", axes = c(1, 2))
ggsave("./output/Lab4/bi_plot_12.png", plot = bi_plot_12, limitsize = FALSE, scale = 2)
var_plot_34 <- fviz_pca_var(pca, col.var = "black", axes = c(3, 4))
ggsave("./output/Lab4/var_plot_34.png", plot = var_plot_34, limitsize = FALSE, scale = 2)
bi_plot_34 <- fviz_pca_biplot(pca, label = "var", axes = c(3, 4))
ggsave("./output/Lab4/bi_plot_34.png", plot = bi_plot_34, limitsize = FALSE, scale = 2)
var_plot_45 <- fviz_pca_var(pca, col.var = "black", axes = c(4, 5))
ggsave("./output/Lab4/var_plot_45.png", plot = var_plot_45, limitsize = FALSE, scale = 2)
bi_plot_45 <- fviz_pca_biplot(pca, label = "var", axes = c(4, 5))
ggsave("./output/Lab4/bi_plot_45.png", plot = bi_plot_45, limitsize = FALSE, scale = 2)

data_train_raw <- data %>% sample_n(nrow(data) * 0.6)
data_test_raw <- setdiff(data, data_train_raw)
data_train_raw <- data_train_raw %>% select(age, height, weight, waistline, DBP, SBP, BLDS,
                                            tot_chole, LDL_chole, triglyceride,
                                            SGOT_AST, SGOT_ALT, gamma_GTP, hemoglobin)
data_train_raw_nh <- data_train_raw %>% select(-hemoglobin)
data_test_raw <- data_test_raw %>% select(age, height, weight, waistline, DBP, SBP, BLDS,
                                          tot_chole, LDL_chole, triglyceride,
                                          SGOT_AST, SGOT_ALT, gamma_GTP, hemoglobin)
data_test_raw_nh <- data_test_raw %>% select(-hemoglobin)
data_train_pca <- PCA(data_train_raw_nh, graph = FALSE)
data_test_pca <- PCA(data_test_raw_nh, graph = FALSE)
data_train_pca_15 <- as_tibble(data_train_pca$ind$coord[, 1:5])
data_test_pca_15 <- as_tibble(data_test_pca$ind$coord[, 1:5])
data_train_pca_15 <- data_train_pca_15 %>% mutate(hemoglobin = data_train_raw$hemoglobin)
data_test_pca_15 <- data_test_pca_15 %>% mutate(hemoglobin = data_test_raw$hemoglobin)

# gam
gam_pca <- gam(hemoglobin ~ s(Dim.1) + s(Dim.2) + s(Dim.3) + s(Dim.4) + s(Dim.5),
               data = data_train_pca_15)
prediction_result <- predict.gam(gam_pca, newdata = data_test_pca_15, se.fit = TRUE)
compare_prediction_true <- tibble(
  index = seq(1, nrow(data_test_pca_15)),
  expected = data_test_pca_15$hemoglobin,
  actual = prediction_result$fit
)
score = mean(prediction_result$se.fit)
hist_ea <- ggplot(compare_prediction_true, aes(x = index))
hist_ea <- hist_ea +
  labs(x = "index", y = "expected(blue) | actual(orange)",
       title = sprintf("gam model: expected vs actual, mean se = %f", score)) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hist_ea <- hist_ea +
  geom_point(aes(y = expected), color = "lightblue", alpha = 0.75) +
  geom_point(aes(y = actual), color = "orange", alpha = 0.75)
ggsave("./output/Lab4/gam_expected_actual.png", plot = hist_ea,
       limitsize = FALSE, dpi = "retina", width = 5000, height = 2500, units = "px")
data_test_pca_15_nh <- data_test_pca_15 %>% select(-hemoglobin)
data_test_pca_15_names <- names(data_test_pca_15_nh)
grid_size = 1000
for (i in 1:ncol(data_test_pca_15_nh)) {
  name_prediction_data <- as_tibble(data.frame(matrix(nrow=0,ncol=length(data_test_pca_15_names))))
  colnames(name_prediction_data) <- data_test_pca_15_names
  free_pc_values = seq(min(data_test_pca_15_nh[[i]]), max(data_test_pca_15_nh[[i]]), length.out = grid_size)
  for (j in 1:grid_size) {
    prediction_data_row <- rep(0, ncol(name_prediction_data))
    prediction_data_row[i] <- free_pc_values[j]
    kdeces <- seq(1,ncol(name_prediction_data))
    for (k in kdeces[kdeces != i]) {
      prediction_data_row[k] <- mean(data_test_pca_15_nh[[k]])
    }
    named_prediction_data_row <- set_names(prediction_data_row, data_test_pca_15_names)
    name_prediction_data <- bind_rows(name_prediction_data, named_prediction_data_row)
  }
  prediction_result <- predict.gam(gam_pca, newdata = name_prediction_data, se.fit = TRUE)
  pc_plot_data <- tibble(
    pc = name_prediction_data[[i]],
    actual = prediction_result$fit
  )
  pc_plot <- ggplot(pc_plot_data, aes(x = pc)) +
    labs(x = sprintf("PC %i", i), y = "hemoglobin",
         title = sprintf("gam model: hemoglobin from PC %f", i)) +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 8),
          plot.title = element_text(size = 8)) +
    geom_ribbon(aes(ymin = actual + qnorm(0.025) * prediction_result$se.fit,
                    ymax = actual + qnorm(0.975) * prediction_result$se.fit),
                fill = "steelblue2") +
    geom_line(aes(y = actual), color = "firebrick", linewidth = 1)
    #geom_point(aes(y = (data_test_pca_15 %>% sample_n(grid_size))$hemoglobin), alpha = 0.2)
  ggsave(sprintf("./output/Lab4/gam_pc_%i_hemoglobin.png", i), plot = pc_plot, limitsize = FALSE, scale = 2)
}

# linear
lm_pca <- lm(hemoglobin ~ Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5,
             data = data_train_pca_15)
prediction_result <- predict.lm(lm_pca, newdata = data_test_pca_15, se.fit = TRUE)
compare_prediction_true <- tibble(
  index = seq(1, nrow(data_test_pca_15)),
  expected = data_test_pca_15$hemoglobin,
  actual = prediction_result$fit
)
score = mean(prediction_result$se.fit)
hist_ea <- ggplot(compare_prediction_true, aes(x = index))
hist_ea <- hist_ea +
  labs(x = "index", y = "expected(blue) | actual(orange)",
       title = sprintf("lm model: expected vs actual, mean se = %f", score)) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hist_ea <- hist_ea +
  geom_point(aes(y = expected), color = "lightblue", alpha = 0.75) +
  geom_point(aes(y = actual), color = "orange", alpha = 0.75)
ggsave("./output/Lab4/lm_expected_actual.png", plot = hist_ea,
       limitsize = FALSE, dpi = "retina", width = 5000, height = 2500, units = "px")
data_test_pca_15_nh <- data_test_pca_15 %>% select(-hemoglobin)
data_test_pca_15_names <- names(data_test_pca_15_nh)
grid_size = 1000
for (i in 1:ncol(data_test_pca_15_nh)) {
  name_prediction_data <- as_tibble(data.frame(matrix(nrow=0,ncol=length(data_test_pca_15_names))))
  colnames(name_prediction_data) <- data_test_pca_15_names
  free_pc_values = seq(min(data_test_pca_15_nh[[i]]), max(data_test_pca_15_nh[[i]]), length.out = grid_size)
  for (j in 1:grid_size) {
    prediction_data_row <- rep(0, ncol(name_prediction_data))
    prediction_data_row[i] <- free_pc_values[j]
    kdeces <- seq(1,ncol(name_prediction_data))
    for (k in kdeces[kdeces != i]) {
      prediction_data_row[k] <- mean(data_test_pca_15_nh[[k]])
    }
    named_prediction_data_row <- set_names(prediction_data_row, data_test_pca_15_names)
    name_prediction_data <- bind_rows(name_prediction_data, named_prediction_data_row)
  }
  prediction_result <- predict.lm(lm_pca, newdata = name_prediction_data, se.fit = TRUE)
  pc_plot_data <- tibble(
    pc = name_prediction_data[[i]],
    actual = prediction_result$fit
  )
  pc_plot <- ggplot(pc_plot_data, aes(x = pc)) +
    labs(x = sprintf("PC %i", i), y = "hemoglobin",
         title = sprintf("lm model: hemoglobin from PC %f", i)) +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 8),
          plot.title = element_text(size = 8)) +
    geom_ribbon(aes(ymin = actual + qnorm(0.025) * prediction_result$se.fit,
                    ymax = actual + qnorm(0.975) * prediction_result$se.fit),
                fill = "steelblue2") +
    geom_line(aes(y = actual), color = "firebrick", linewidth = 1)
  #geom_point(aes(y = (data_test_pca_15 %>% sample_n(grid_size))$hemoglobin), alpha = 0.2)
  ggsave(sprintf("./output/Lab4/lm_pc_%i_hemoglobin.png", i), plot = pc_plot, limitsize = FALSE, scale = 2)
}
lm_pca_hc0 <- coeftest(lm_pca, vcov. = vcovHC(lm_pca, type = "HC0"))
stargazer(lm_pca, out = "./output/Lab4/lm_stargazer.html", type = "html",
          label = "table:evals-reg",
          dep.var.labels = c("hemoglobin"),
          dep.var.caption = "",
          se = list(lm_pca_hc0[, 2]),
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"))

# kernel data
kernel_data <- data_train_pca_15 %>% sample_n(5000)
X <- kernel_data$Dim.1
Y <- kernel_data$hemoglobin
x_grid <- seq(min(X), max(X), length.out = 1000)

# linear
linear <- lm(Y ~ X, data = kernel_data)
l <- function(x) {
  x * 0.4621459 + 14.2088393
}
linear_plot <- ggplot(kernel_data, aes(x = Dim.1, y = hemoglobin)) +
  geom_point(size = 1) +
  geom_function(fun = l, size = 1, color = "red")
ggsave("./output/Lab4/linear_plot.png", plot = linear_plot, limitsize = FALSE, scale = 2)

# kernel Nadaraya-Watson
h <- npregbw(Y ~ X, data = kernel_data, regtype = "lc")
nw_LOOCV <- npreg(h, newdata = data.frame(X = x_grid))
h_plot <- c(0.01, round(h$bw, 3), 2)
nw_under <- npreg(bws = h_plot[1], xdat = X, ydat = Y, exdat = x_grid)
nw_over <- npreg(bws = h_plot[3], xdat = X, ydat = Y, exdat = x_grid)
nw_df <- tibble(
  x = rep(x_grid, 3),
  y_hat = c(nw_under$mean, nw_LOOCV$mean, nw_over$mean),
  h = rep(h_plot, each = length(x_grid))
)
k_nw_plot <- ggplot(kernel_data, aes(x = Dim.1, y = hemoglobin)) +
  geom_point(size = 1) +
  geom_line(data = nw_df, aes(x = x, y = y_hat),
            size = 1, color = "red") +
  geom_function(fun = l, size = 1) +
  labs(x = "X", y = expression(hat(Y))) +
  facet_wrap(~ factor(h)) +
  theme_classic() +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))
ggsave("./output/Lab4/k_nw_plot.png", plot = k_nw_plot, limitsize = FALSE,
       dpi = "retina", width = 9000, height = 2500, units = "px")

# kernel local-linear
h <- npregbw(Y ~ X, data = kernel_data, regtype = 'll')
ll_LOOCV <- npreg(h, newdata = data.frame(X = x_grid))
h_plot <- c(1, round(h$bw, 3),30)
ll_under <- npreg(bws = h_plot[1], xdat = X, ydat = Y,
                  exdat = x_grid)
ll_over <- npreg(bws = h_plot[3], xdat = X, ydat = Y,
                 exdat = x_grid)
ll_df <- tibble(x = rep(x_grid, 3),
                y_hat = c(ll_under$mean,
                          ll_LOOCV$mean,
                          ll_over$mean),
                h = rep(h_plot, each = length(x_grid)))
k_ll_plot <- ggplot(kernel_data, aes(x = Dim.1, y = hemoglobin)) +
  geom_point(size = 1) +
  geom_line(data = ll_df, aes(x = x, y = y_hat),
            linewidth = 1, color = "red") +
  geom_function(fun = l, linewidth = 1) +
  facet_wrap(~ factor(h)) +
  theme_classic() +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))
ggsave("./output/Lab4/k_ll_plot.png", plot = k_ll_plot, limitsize = FALSE,
       dpi = "retina", width = 9000, height = 2500, units = "px")

# compare linear, nw, ll
cmp_nw_data <- tibble(
  x = x_grid,
  y = nw_LOOCV$mean
)
cmp_ll_data <- tibble(
  x = x_grid,
  y = ll_LOOCV$mean
)
cmp_plot <- ggplot(kernel_data, aes(x = Dim.1, y = hemoglobin)) +
  geom_point(size = 1) +
  geom_function(fun = l, size = 1, color = "red") +
  geom_line(data = cmp_nw_data, aes(x = x, y = y), color = "blue") +
  geom_line(data = cmp_ll_data, aes(x = x, y = y), color = "green")
ggsave("./output/Lab4/compare.png", plot = cmp_plot, limitsize = FALSE, scale = 2)

ll_eval_plot <- ggplot(data = data.frame(x = x_grid,
                       y_hat = ll_LOOCV$mean,
                       lower = ll_LOOCV$mean - qnorm(0.975)*ll_LOOCV$merr,
                       upper = ll_LOOCV$mean + qnorm(0.975)*ll_LOOCV$merr),
       aes(x = x, y = y_hat)) +
  geom_point(data = kernel_data, aes(x = X, y = Y), size = 1) +
  geom_line(linewidth = 1, color = "red") +
  geom_function(fun = l, linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "gray") +
  theme_classic() +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))
ggsave("./output/Lab4/ll_eval_plot.png", plot = ll_eval_plot, limitsize = FALSE, scale = 2)