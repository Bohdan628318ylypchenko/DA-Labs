library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stargazer)
library(car)
library(lmtest)
library(sandwich)
library(ggcorrplot)
library(ggpubr)

ggplotRegression <- function (fit) {
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

data <- read_csv('clean-data.csv')
# data <- data %>% mutate(
#   sex = as.numeric(as.factor(sex)), # male = 2, female = 1
#   DRK_YN = as.numeric(as.factor(DRK_YN)) # 1 = no, 2 = yrs
# )

# ================= simple regression

data_age_20_60_weight_60_75 <- data %>% filter(age >= 20, age <= 60, weight >= 60, weight <= 75)

lm_in_age_out_hemoglobin <- lm(hemoglobin ~ age, data = data_age_20_60_weight_60_75)
plot_lm_in_age_out_hemoglobin <- ggplotRegression(lm_in_age_out_hemoglobin)
ggsave("./output/Lab3_Regression/plot_lm_in_age_out_hemoglobin.png",
       plot = plot_lm_in_age_out_hemoglobin)

lm_in_weight_out_hemoglobin <- lm(hemoglobin ~ weight, data = data_age_20_60_weight_60_75)
plot_lm_in_weight_out_hemoglobin <- ggplotRegression(lm_in_weight_out_hemoglobin)
ggsave("./output/Lab3_Regression/plot_lm_in_weight_out_hemoglobin.png",
       plot = plot_lm_in_weight_out_hemoglobin)

model <- lm(hemoglobin ~ age + weight, data = data)
model_hc0 <- coeftest(model, vcov. = vcovHC(model, type = "HC0"))
stargazer(model, out = "./output/Lab3_Regression/alldata_age_weight.html", type = "html",
          label = "table:evals-reg",
          dep.var.labels = c("hemoglobin"),
          dep.var.caption = "",
          se = list(model_hc0[, 2]),
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"))

file <- "./output/Lab3_Regression/alldata_age_weight.html"

# ================= 12 models data

# data <- data %>%
#   mutate(mbi = data$weight / ((0.01 * data$height)^2)) %>%
#   select(-weight, -height)
data <- data %>% filter(age >= 20)
data_male_y_1 <- data %>%
  filter(sex == "Male", DRK_YN == "Y", SMK_stat_type_cd == 1) %>% 
  select(-sex, -DRK_YN, -SMK_stat_type_cd)
data_male_y_2 <- data %>%
  filter(sex == "Male", DRK_YN == "Y", SMK_stat_type_cd == 2) %>% 
  select(-sex, -DRK_YN, -SMK_stat_type_cd)
data_male_y_3 <- data %>%
  filter(sex == "Male", DRK_YN == "Y", SMK_stat_type_cd == 3) %>%
  select(-sex, -DRK_YN, -SMK_stat_type_cd)
data_male_n_1 <- data %>%
  filter(sex == "Male", DRK_YN == "N", SMK_stat_type_cd == 1) %>% 
  select(-sex, -DRK_YN, -SMK_stat_type_cd)
data_male_n_2 <- data %>%
  filter(sex == "Male", DRK_YN == "N", SMK_stat_type_cd == 2) %>% 
  select(-sex, -DRK_YN, -SMK_stat_type_cd)
data_male_n_3 <- data %>%
  filter(sex == "Male", DRK_YN == "N", SMK_stat_type_cd == 3) %>% 
  select(-sex, -DRK_YN, -SMK_stat_type_cd)
data_female_y_1 <- data %>%
  filter(sex == "Female", DRK_YN == "Y", SMK_stat_type_cd == 1) %>% 
  select(-sex, -DRK_YN, -SMK_stat_type_cd)
data_female_y_2 <- data %>%
  filter(sex == "Female", DRK_YN == "Y", SMK_stat_type_cd == 2) %>% 
  select(-sex, -DRK_YN, -SMK_stat_type_cd)
data_female_y_3 <- data %>%
  filter(sex == "Female", DRK_YN == "Y", SMK_stat_type_cd == 3) %>% 
  select(-sex, -DRK_YN, -SMK_stat_type_cd)
data_female_n_1 <- data %>%
  filter(sex == "Female", DRK_YN == "N", SMK_stat_type_cd == 1) %>% 
  select(-sex, -DRK_YN, -SMK_stat_type_cd)
data_female_n_2 <- data %>%
  filter(sex == "Female", DRK_YN == "N", SMK_stat_type_cd == 2) %>% 
  select(-sex, -DRK_YN, -SMK_stat_type_cd)
data_female_n_3 <- data %>%
  filter(sex == "Female", DRK_YN == "N", SMK_stat_type_cd == 3) %>% 
  select(-sex, -DRK_YN, -SMK_stat_type_cd)

data_12models = list(data_male_y_1, data_male_y_2, data_male_y_3,
                     data_male_n_1, data_male_n_2, data_male_n_3,
                     data_female_y_1, data_female_y_2, data_female_y_3,
                     data_female_n_1, data_female_n_1, data_female_n_3)

# ================= 12 models

# age weight height waistline for each from 12
models12 <- list()
for (i in 1:length(data_12models)) {
  model <- lm(hemoglobin ~ age + weight + height + waistline, data = data_12models[[i]])
  model_hc0 <- coeftest(model, vcov. = vcovHC(model, type = "HC0"))
  models12[[i]] = model
  print(sprintf("coeftest for model %d", i))
  print(model_hc0)
}
stargazer(models12, out = "./output/Lab3_Regression/model12_age_weight.html", type = "html",
          label = "table:evals-reg",
          dep.var.labels = c("hemoglobin"),
          dep.var.caption = "",
          se = list(model_hc0[, 2]),
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"))

# best auto model for each from 12
models12 <- list()
for (i in 1:length(data_12models)) {
  min_model <- lm(hemoglobin ~ 1, data = data_12models[[i]])
  model <- step(min_model, direction = "forward",
                scope = (~ age + height + weight + waistline +
                           sight_left + sight_right + hear_left + hear_right + 
                           SBP + DBP + BLDS + 
                           tot_chole + HDL_chole + LDL_chole + triglyceride + 
                           urine_protein + serum_creatinine +
                           SGOT_AST + SGOT_ALT + gamma_GTP))
  model_hc0 <- coeftest(model, vcov. = vcovHC(model, type = "HC0"))
  models12[[i]] <- model
  print(sprintf("coeftest for model %d", i))
  print(model_hc0)
}
stargazer(models12, out = "./output/Lab3_Regression/model12_autobest.html", type = "html",
          label = "table:evals-reg",
          dep.var.labels = c("hemoglobin"),
          dep.var.caption = "",
          se = list(model_hc0[, 2]),
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"))

# don't know what to do - time to build corr matrices
corr_matrices_plots <- list()
for (i in 1:length(data_12models)) {
  corr <- cor(data_12models[[i]])
  corr_plot <- ggcorrplot(corr, lab = TRUE)
  corr_plot + theme(text = element_text(size = 30, face = "bold"),
                    legend.title=element_text(size=15), 
                    legend.text=element_text(size=15),
                    axis.text.y = element_text(size=30, face = "bold"),
                    axis.text.x = element_text(size=30, face= "bold"))
  corr_matrices_plots[[i]] <- corr_plot
}
corr_matrices_plot <- ggarrange(corr_matrices_plots[[1]], corr_matrices_plots[[2]], corr_matrices_plots[[3]],
                                corr_matrices_plots[[4]], corr_matrices_plots[[5]], corr_matrices_plots[[6]],
                                corr_matrices_plots[[7]], corr_matrices_plots[[8]], corr_matrices_plots[[9]],
                                corr_matrices_plots[[10]], corr_matrices_plots[[11]], corr_matrices_plots[[12]])
ggsave("./output/Lab3_Regression/corr_matrices.svg", plot = corr_matrices_plot, limitsize = FALSE, dpi = 1000, scale = 1024)

# Manual 12 models
models12 <- list()
models12[[1]] <- lm(hemoglobin ~ age + height + weight + waistline +
                    DBP + tot_chole + LDL_chole + triglyceride + SGOT_ALT,
                    data = data_12models[[1]])
models12[[2]] <- lm(hemoglobin ~ age + height + weight + waistline +
                    DBP + tot_chole + LDL_chole + triglyceride + SGOT_ALT,
                    data = data_12models[[2]])
models12[[3]] <- lm(hemoglobin ~ age + height + weight + waistline +
                    DBP + tot_chole + LDL_chole + triglyceride + SGOT_ALT,
                    data = data_12models[[3]])
models12[[4]] <- lm(hemoglobin ~ age + height + weight + waistline + 
                    hear_left + hear_right +
                    DBP + tot_chole + LDL_chole + triglyceride + SGOT_AST,
                    data = data_12models[[4]])
models12[[5]] <- lm(hemoglobin ~ age + height + weight + waistline +
                    hear_left + hear_right + DBP + tot_chole + LDL_chole + triglyceride +
                    urine_protein + serum_creatinine + SGOT_ALT,
                    data = data_12models[[5]])
models12[[6]] <- lm(hemoglobin ~ age + height + weight + waistline +
                    DBP + tot_chole + LDL_chole + triglyceride + SGOT_ALT,
                    data = data_12models[[6]])
models12[[7]] <- lm(hemoglobin ~ DBP + SBP + BLDS + tot_chole + LDL_chole + triglyceride + SGOT_ALT,
                    data = data_12models[[7]])
models12[[8]] <- lm(hemoglobin ~ weight + waistline + SBP + DBP + BLDS +
                    tot_chole + LDL_chole + triglyceride + SGOT_ALT,
                    data = data_12models[[8]])
models12[[9]] <- lm(hemoglobin ~ weight + waistline + SBP + DBP + BLDS +
                    tot_chole + LDL_chole + triglyceride + SGOT_ALT,
                    data = data_12models[[9]])
models12[[10]] <- lm(hemoglobin ~ weight + SBP + DBP + BLDS +
                     tot_chole + LDL_chole + triglyceride + SGOT_AST + SGOT_ALT + gamma_GTP,
                     data = data_12models[[10]])
models12[[11]] <- lm(hemoglobin ~ weight + SBP + DBP + BLDS +
                     tot_chole + LDL_chole + triglyceride + SGOT_AST + SGOT_ALT + gamma_GTP,
                     data = data_12models[[11]])
models12[[12]] <- lm(hemoglobin ~ weight + waistline + SBP + DBP + BLDS +
                     tot_chole + LDL_chole + triglyceride + SGOT_AST + SGOT_ALT,
                     data = data_12models[[12]])
stargazer(models12, out = "./output/Lab3_Regression/model12_manual.html", type = "html",
          label = "table:evals-reg",
          dep.var.labels = c("hemoglobin"),
          dep.var.caption = "",
          se = list(model_hc0[, 2]),
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"))

# ================= all data
data <- data %>%
  mutate(
    sex = as.numeric(as.factor(sex)), # male = 2, female = 1
    DRK_YN = as.numeric(as.factor(DRK_YN)), # 1 = no, 2 = yrs
    SMK_Y = (SMK_stat_type_cd == 3),
    SMK_N = (SMK_stat_type_cd == 1)
  ) %>%
  select(
    -SMK_stat_type_cd
  )
min_model <- lm(hemoglobin ~ 1, data = data)
model_all_nocat <- step(min_model, direction = "forward", 
                        scope = (~ age + height + weight + waistline + 
                                   sight_left + sight_right + hear_left + hear_right + 
                                   SBP + DBP + BLDS + 
                                   tot_chole + HDL_chole + LDL_chole + triglyceride + 
                                   urine_protein + serum_creatinine +
                                   SGOT_AST + SGOT_ALT + gamma_GTP))
model_all_sexcat <- step(min_model, direction = "forward", 
                         scope = (~ age + height + weight + waistline + 
                                    sight_left + sight_right + hear_left + hear_right + 
                                    SBP + DBP + BLDS + 
                                    tot_chole + HDL_chole + LDL_chole + triglyceride + 
                                    urine_protein + serum_creatinine +
                                    SGOT_AST + SGOT_ALT + gamma_GTP +
                                   sex))
model_all_allcat <- step(min_model, direction = "forward", 
                         scope = (~ age + height + weight + waistline + 
                                    sight_left + sight_right + hear_left + hear_right + 
                                    SBP + DBP + BLDS + 
                                    tot_chole + HDL_chole + LDL_chole + triglyceride + 
                                    urine_protein + serum_creatinine +
                                    SGOT_AST + SGOT_ALT + gamma_GTP +
                                    sex + DRK_YN + SMK_Y + SMK_N))
stargazer(model_all_nocat, model_all_sexcat, model_all_allcat,
          out = "./output/Lab3_Regression/alldata.html", type = "html",
          label = "table:evals-reg",
          dep.var.labels = c("hemoglobin"),
          dep.var.caption = "",
          se = list(model_hc0[, 2]),
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"))
vif(model_all_allcat)

model_all_allcat_no_tot_chole <- lm(hemoglobin ~ age + height + weight + waistline + 
                                    hear_left + hear_right + 
                                    SBP + DBP + BLDS + 
                                    HDL_chole + LDL_chole + triglyceride + 
                                    urine_protein + serum_creatinine +
                                    SGOT_AST + SGOT_ALT + gamma_GTP +
                                    sex + DRK_YN + SMK_Y + SMK_N,
                                    data = data)
stargazer(model_all_allcat, model_all_allcat_no_tot_chole,
          out = "./output/Lab3_Regression/alldata_no_tot_chole.html", type = "html",
          label = "table:evals-reg",
          dep.var.labels = c("hemoglobin"),
          dep.var.caption = "",
          se = list(model_hc0[, 2]),
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"))
vif(model_all_allcat_no_tot_chole)

hypos <- c('age=0', 'height=0', 'weight=0', 'waistline=0',
           'hear_left=0', 'hear_right',
           'SBP=0', 'DBP=0', 'BLDS=0',
           'HDL_chole=0', 'LDL_chole=0', 'triglyceride=0',
           'urine_protein=0', 'serum_creatinine=0',
           'SGOT_AST=0', 'SGOT_ALT=0', 'gamma_GTP=0',
           'sex=0', 'DRK_YN=0', 'SMK_YTRUE=0', 'SMK_NTRUE=0')
for (hypo in hypos) {
  current_hypos <- hypos[!hypos %in% c(hypo)]
  print(linearHypothesis(model_all_allcat_no_tot_chole,
                   current_hypos,
                   vcov. = hccm(model_all_allcat_no_tot_chole, type='hc1')))
}

coeftest(model_all_allcat_no_tot_chole, vcoc. = hccm(model_all_allcat_no_tot_chole, type = 'hc0'))