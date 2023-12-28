library(tidyverse)
library(dplyr)
library(boot)

data <- read_csv('clean-data.csv')
data_all_numeric <- data %>%
  select(-sex, -SMK_stat_type_cd, -DRK_YN)

if (!dir.exists("./output/Lab2_HI")) {
  dir.create("./output/Lab2_HI")  
}

# confidence interval mean (by normal distribution)
mean_calculate_stats <- function(column, column_name) {
  n <- length(column)
  mean_val <- mean(column)
  sd_val <- sd(column)
  
  ci_norm_a <- mean_val + qnorm(0.025) * sd_val / sqrt(n)
  ci_norm_b <- mean_val + qnorm(0.975) * sd_val / sqrt(n)
  
  tibble(name = column_name,
         mean = mean_val,
         standard_deviation = sd_val,
         confidence_interval_norm_a = ci_norm_a,
         confidence_interval_norm_b = ci_norm_b)
}
mean_confidence_intervals_norm <- bind_rows(
  lapply(names(data_all_numeric), function(column_name) {
    mean_calculate_stats(data_all_numeric[[column_name]], column_name)
  })
)
write_csv(mean_confidence_intervals_norm, file = "./output/Lab2_HI/mean_confidence_intervals_norm.csv")

# boot median confidence interval fail
calc_median <- function(x, i) {
  return(median(x[i]))
}
median_confidence_intervals_boot <- bind_rows(lapply(names(data_all_numeric), function(column_name) {
  boot_result <- boot(data_all_numeric[[column_name]], statistic = calc_median, R = 500)
  boot_ci_result <- boot.ci(boot_result, type = "basic", conf = 0.95)
  tibble(name = column_name,
         median_t0 = boot_result$t0,
         median_ci = boot_ci_result$t0)
  })
)
write_csv(median_confidence_intervals_boot, file = "./output/Lab2_HI/median_confidence_intervals_boot.csv")

# age
boot_samples <- boot(data_all_numeric$age, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# height
boot_samples <- boot(data_all_numeric$height, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# weight
boot_samples <- boot(data_all_numeric$weight, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# waistline
boot_samples <- boot(data_all_numeric$waistline, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# sight_left
boot_samples <- boot(data_all_numeric$sight_left, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# sight_right
boot_samples <- boot(data_all_numeric$sight_right, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# hear_left
boot_samples <- boot(data_all_numeric$hear_left, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# hear_right
boot_samples <- boot(data_all_numeric$hear_right, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# sbp
boot_samples <- boot(data_all_numeric$SBP, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# dbp
boot_samples <- boot(data_all_numeric$DBP, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# blds
boot_samples <- boot(data_all_numeric$BLDS, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# tot_chole
boot_samples <- boot(data_all_numeric$tot_chole, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# hdl_chole
boot_samples <- boot(data_all_numeric$HDL_chole, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# ldl_chole
boot_samples <- boot(data_all_numeric$LDL_chole, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# triglyceride
boot_samples <- boot(data_all_numeric$triglyceride, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# hemoglobin
boot_samples <- boot(data_all_numeric$hemoglobin, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# urine_protein
boot_samples <- boot(data_all_numeric$urine_protein, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# serum_creatinine
boot_samples <- boot(data_all_numeric$serum_creatinine, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# SGOT_AST
boot_samples <- boot(data_all_numeric$SGOT_AST, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# SGOT_ALT
boot_samples <- boot(data_all_numeric$SGOT_ALT, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# gamma_GTP
boot_samples <- boot(data_all_numeric$gamma_GTP, statistic = calc_median, R = 100)
boot_ci <- boot.ci(boot_samples, type = "basic", conf = 0.95)
boot_ci

# confidence interval mean (by normal distribution)
median_calculate_stats <- function(column, column_name) {
  n <- length(column)
  median_val <- median(column)
  sd_val <- sd(column)
  
  ci_norm_a <- median_val + qnorm(0.025) * sd_val / sqrt(n)
  ci_norm_b <- median_val + qnorm(0.975) * sd_val / sqrt(n)
  
  tibble(name = column_name,
         median = median_val,
         standard_deviation = sd_val,
         confidence_interval_norm_a = ci_norm_a,
         confidence_interval_norm_b = ci_norm_b)
}
median_confidence_intervals_norm <- bind_rows(
  lapply(names(data_all_numeric), function(column_name) {
    median_calculate_stats(data_all_numeric[[column_name]], column_name)
  })
)
write_csv(median_confidence_intervals_norm, file = "./output/Lab2_HI/median_confidence_intervals_norm.csv")

# hypos
calc_statistics_criteria <- function(x, y) {
  mean_x <- mean(x)
  mean_y <- mean(y)
  mean_d <- mean_x - mean_y
  sd_x <- sd(x)
  sd_y <- sd(y)
  nx <- length(x)
  ny <- length(y)
  se = sqrt(((sd_x * sd_x)/nx) + ((sd_y * sd_y)/ny))
  sc = mean_d / se
  ci_low = mean_d - se * 1.64
  return(c(statistics_criteria = sc, standard_error = se, confidence_interval_low = ci_low))
}

# hypo: weight (body mass index) influence on hemoglobin
# h0: hemoglobin(obese) = hemoglobin(not_obese)
# ha: hemoglobin(obese) > hemoglobin(not_obese)
data <- data %>% mutate(mbi = data$weight / ((0.01 * data$height)^2))
x <- (data %>% filter(mbi > 25))$hemoglobin
y <- (data %>% filter(mbi <= 25))$hemoglobin
calc_statistics_criteria(x, y)
c(mean_hemoglobin_not_obese = mean(y), mean_hemoglobin_obese = mean(x))


# hypo: Alcohol influences on SBP
# h0: SBP(Alc) = SBP(NoAlc)
# ha: SBP(Alc) > SBP(NoAlc)
x <- (data %>% filter(DRK_YN == "Y"))$SBP
y <- (data %>% filter(DRK_YN == "N"))$SBP
calc_statistics_criteria(x, y)

# hypo: Alcohol influence on DBP
# h0: DBP(Alc) = DBP(NoAlc)
# ha: DBP(Alc) > DBP(NoAlc)
x <- (data %>% filter(DRK_YN == "Y"))$DBP
y <- (data %>% filter(DRK_YN == "N"))$DBP
calc_statistics_criteria(x, y)

# hypo: Alcohol influence on tot_chole
# h0: tot_chole(Alc) = tot_chole(NoAlc)
# ha: tot_chole(Alc) > tot_chole(NoAlc)
x <- (data %>% filter(DRK_YN == "Y"))$tot_chole
y <- (data %>% filter(DRK_YN == "N"))$tot_chole
calc_statistics_criteria(x, y)

# hypo: Alcohol influence on hemoglobin
# h0: hemoglobin(Alc) = hemoglobin(NoAlc)
# ha: hemoglobin(Alc) > hemoglobin(NoAlc)
x <- (data %>% filter(DRK_YN == "Y"))$hemoglobin
y <- (data %>% filter(DRK_YN == "N"))$hemoglobin
calc_statistics_criteria(x, y)

# hypo: Smoking influence on SBP
# h0: SBP(2) = SBP(1|3)
# ha: SBP(2) < SBP(1|3)
x <- (data %>% filter(SMK_stat_type_cd == 2))$SBP
y <- (data %>% filter(SMK_stat_type_cd != 2))$SBP
calc_statistics_criteria(x, y)

# hypo: Smoking influence on DBP
# h0: DBP(2) = DBP(1|3)
# ha: DBP(2) < DBP(1|3)
x <- (data %>% filter(SMK_stat_type_cd == 2))$DBP
y <- (data %>% filter(SMK_stat_type_cd != 2))$DBP
calc_statistics_criteria(x, y)

# hypo: Smoking influence on hemoglobin 1
# h0: hemoglobin(3) = hemoglobin(1)
# ha: hemoglobin(3) > hemoglobin(1)
x <- (data %>% filter(SMK_stat_type_cd == 3))$hemoglobin
y <- (data %>% filter(SMK_stat_type_cd == 1))$hemoglobin
calc_statistics_criteria(x, y)

# hypo: Smoking influence on hemoglobin 2
# h0: hemoglobin(3) = hemoglobin(2)
# ha: hemoglobin(3) > hemoglobin(2)
x <- (data %>% filter(SMK_stat_type_cd == 3))$hemoglobin
y <- (data %>% filter(SMK_stat_type_cd == 2))$hemoglobin
calc_statistics_criteria(x, y)