library(tidyverse)
library(dplyr)
library(qqplotr)
library(ggpubr)

# load and display data
data <- read_csv('original-data.csv')
glimpse(data)

# check if there are any NaN values in whole data
nan_check <- data %>% summarise_all(~ any(is.nan(.)))
print(nan_check)


# >>> age >>>

# age frequency histogram
age_fhist <- ggplot(data, aes(x = age)) +
  geom_bar(fill="skyblue") +
  scale_x_continuous(breaks = seq(min(data$age), max(data$age), 5)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "age", y = "age frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("./output/Lab1_EDA/data_review/age_fhist.png", plot = age_fhist)

# age frequency
age_frequency = data %>% count(age) %>% group_by(age)

# age qqplot
age_qq <- ggplot(age_frequency, aes(sample = log(n))) +
  stat_qq_point() + stat_qq_line() + stat_qq_band(fill="skyblue") +
  labs(x = "Quntiles of normal distribution", y = "Age") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))
ggsave("./output/Lab1_EDA/data_review/age_qq.png", plot = age_qq)


# >>> height >>>

# height frequency histogram
height_fhist <- ggplot(data, aes(x = height)) +
  geom_bar(fill="skyblue") +
  scale_x_continuous(breaks = seq(min(data$height), max(data$height), 5)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "height", y = "height frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("./output/Lab1_EDA/data_review/height_fhist.png", plot = height_fhist)

# height frequency
height_frequency = data %>% count(height) %>% group_by(height)

# height qqplot
height_qq <- ggplot(height_frequency, aes(sample = log(n))) +
  stat_qq_point() + stat_qq_line() + stat_qq_band(fill="skyblue") +
  labs(x = "Quntiles of normal distribution", y = "Height") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))
ggsave("./output/Lab1_EDA/data_review/height_qq.png", plot = height_qq)


# >>> weight >>>

# weight frequency histogram
weight_fhist <- ggplot(data, aes(x = weight)) +
  geom_bar(fill="skyblue") +
  scale_x_continuous(breaks = seq(min(data$weight), max(data$weight), 5)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "weight", y = "weight frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("./output/Lab1_EDA/data_review/weight_fhist.png", plot = weight_fhist)

# weight frequency
weight_frequency = data %>% count(weight) %>% group_by(weight)

# weight qqplot
ggplot(height_frequency, aes(sample = log(n))) +
  stat_qq_point() + stat_qq_line() + stat_qq_band(fill="skyblue") +
  labs(x = "Quntiles of normal distribution", y = "Weight") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))


# >>> sbp >>>

# sbp frequency histogram
sbp_fhist <- ggplot(data, aes(x = SBP)) +
  geom_bar(fill="skyblue") +
  scale_x_continuous(breaks = seq(min(data$SBP), max(data$SBP), 10)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "sbp", y = "sbp frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("./output/Lab1_EDA/data_review/sbp_fhist.png", plot = sbp_fhist)


# sbp boxplot
sbp_box <- ggplot(data, aes(x = SBP)) + 
  geom_boxplot() +
  labs(title = "SBP distribution") +
  scale_x_continuous(breaks = seq(min(data$SBP), max(data$SBP) + 10, 10)) +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 30))
ggsave("./output/Lab1_EDA/data_review/sbp_box.png", plot = sbp_box)

# sbp frequency
sbp_frequency = data %>% 
  count(SBP) %>%
  group_by(SBP) %>% 
  mutate(n = log10(n), SBP = log10(SBP))

# sbp qqplot
sbp_qq <- ggplot(height_frequency, aes(sample = log(n))) +
  stat_qq_point() + stat_qq_line() + stat_qq_band(fill="skyblue") +
  labs(x = "Quntiles of normal distribution", y = "Sbp") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))
ggsave("./output/Lab1_EDA/data_review/sbp_qq.png", plot = sbp_qq)


# >>> dbp

# dbp frequency histogram
dbp_fhist <- ggplot(data, aes(x = DBP)) +
  geom_bar(fill="skyblue") +
  scale_x_continuous(breaks = seq(min(data$DBP), max(data$DBP), 10)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "dbp", y = "dbp frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("./output/Lab1_EDA/data_review/dbp_fhist.png", plot = dbp_fhist)

# dbp boxplot
dbp_box <- ggplot(data, aes(x = DBP)) + 
  geom_boxplot() +
  labs(title = "dbp distribution") +
  scale_x_continuous(breaks = seq(min(data$DBP), max(data$DBP) + 10, 10)) +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 30))
ggsave("./output/Lab1_EDA/data_review/dbp_box.png", plot = dbp_box)

# dbp frequency
dbp_frequency = data %>% 
  count(DBP) %>%
  group_by(DBP) %>% 
  mutate(n = log10(n), DBP = log10(DBP))

# dbp qqplot
dbp_qq <- ggplot(height_frequency, aes(sample = log(n))) +
  stat_qq_point() + stat_qq_line() + stat_qq_band(fill="skyblue") +
  labs(x = "Quntiles of normal distribution", y = "dbp") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))
ggsave("./output/Lab1_EDA/data_review/dbp_qq.png", plot = dbp_qq)


# >>> tot_chole >>>

# tot_chole frequency histogram
tot_chole_fhist <- ggplot(data, aes(x = tot_chole)) +
  geom_bar(fill="skyblue") +
  scale_x_continuous(breaks = seq(min(data$tot_chole), max(data$tot_chole), 100)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "tot_chole", y = "tot_chole frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("./output/Lab1_EDA/data_review/tot_chole_fhist.png", plot = tot_chole_fhist)

# tot_chole boxplot
tot_chole_box <- ggplot(data, aes(x = tot_chole)) + 
  geom_boxplot() +
  labs(title = "tot_chole distribution") +
  scale_x_continuous(breaks = seq(min(data$tot_chole), max(data$tot_chole) + 5, 150)) +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 30))
ggsave("./output/Lab1_EDA/data_review/tot_chole_box.png", plot = tot_chole_box)

# tot_chole frequency
tot_chole_frequency = data %>% count(tot_chole) %>% group_by(tot_chole)

# tot_chole qqplot
tot_chole_qq <- ggplot(height_frequency, aes(sample = n)) +
  stat_qq_point() + stat_qq_line() + stat_qq_band(fill="skyblue") +
  labs(x = "Quntiles of normal distribution", y = "tot_chole") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))
ggsave("./output/Lab1_EDA/data_review/tot_chole_qq.png", plot = tot_chole_qq)


# >>> hemoglobin >>>

# hemoglobin boxplot
hemoglobin_box <- ggplot(data, aes(x = hemoglobin)) + 
  geom_boxplot() +
  labs(title = "hemoglobin distribution") +
  scale_x_continuous(breaks = seq(min(data$hemoglobin), max(data$hemoglobin) + 5, 150)) +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 30))
ggsave("./output/Lab1_EDA/data_review/hemoglobin_box.png", plot = hemoglobin_box)

# hemoglobin frequency histogram
hemoglobin_fhist <- ggplot(data, aes(x = hemoglobin)) +
  geom_bar(fill="skyblue") +
  scale_x_continuous(breaks = seq(min(data$hemoglobin), max(data$hemoglobin), 0.5)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "hemoglobin", y = "hemoglobin frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("./output/Lab1_EDA/data_review/hemoglobin_fhist.png", plot = hemoglobin_fhist)

# hemoglobin frequency
hemoglobin_frequency = data %>% count(hemoglobin) %>% group_by(hemoglobin)

# hemoglobin qqplot
hemoglobin_qq <- ggplot(height_frequency, aes(sample = n)) +
  stat_qq_point() + stat_qq_line() + stat_qq_band(fill="skyblue") +
  labs(x = "Quntiles of normal distribution", y = "hemoglobin") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))
ggsave("./output/Lab1_EDA/data_review/hemoglobin_qq.png", plot = hemoglobin_qq)


# >>> xy plots >>>

# split data into 6 subsets
nodrink_1smoke <- data %>% filter(DRK_YN == "N" & SMK_stat_type_cd == 1)
nodrink_2smoke <- data %>% filter(DRK_YN == "N" & SMK_stat_type_cd == 2)
nodrink_3smoke <- data %>% filter(DRK_YN == "N" & SMK_stat_type_cd == 3)
yodrink_1smoke <- data %>% filter(DRK_YN == "Y" & SMK_stat_type_cd == 1)
yodrink_2smoke <- data %>% filter(DRK_YN == "Y" & SMK_stat_type_cd == 2)
yodrink_3smoke <- data %>% filter(DRK_YN == "Y" & SMK_stat_type_cd == 3)

# tot_chole -> sbp
tot_chole_sbp_n1_clusters <- ggplot(nodrink_1smoke, aes(x = tot_chole, y = SBP)) +
  geom_point() +
  labs(x = "tot_chole", y = "SBP", title = "tot_chole and SBP of no drink 1 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
tot_chole_sbp_n2_clusters <- ggplot(nodrink_2smoke, aes(x = tot_chole, y = SBP)) +
  geom_point() +
  labs(x = "tot_chole", y = "SBP", title = "tot_chole and SBP of no drink 2 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
tot_chole_sbp_n3_clusters <- ggplot(nodrink_3smoke, aes(x = tot_chole, y = SBP)) +
  geom_point() +
  labs(x = "tot_chole", y = "SBP", title = "tot_chole and SBP of no drink 3 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
tot_chole_sbp_y1_clusters <- ggplot(yodrink_1smoke, aes(x = tot_chole, y = SBP)) +
  geom_point() +
  labs(x = "tot_chole", y = "SBP", title = "tot_chole and SBP of yes drink 1 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
tot_chole_sbp_y2_clusters <- ggplot(yodrink_2smoke, aes(x = tot_chole, y = SBP)) +
  geom_point() +
  labs(x = "tot_chole", y = "SBP", title = "tot_chole and SBP of yes drink 2 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
tot_chole_sbp_y3_clusters <- ggplot(yodrink_3smoke, aes(x = tot_chole, y = SBP)) +
  geom_point() +
  labs(x = "tot_chole", y = "SBP", title = "tot_chole and SBP of yes drink 3 smoke") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
tot_chole_sbp <- ggarrange(tot_chole_sbp_n1_clusters,
                           tot_chole_sbp_n2_clusters,
                           tot_chole_sbp_n3_clusters,
                           tot_chole_sbp_y1_clusters,
                           tot_chole_sbp_y2_clusters,
                           tot_chole_sbp_y3_clusters,
                           ncol = 3, nrow = 2)
ggsave("./output/Lab1_EDA/data_review/tot_chole_sbp_cluster.png", plot = tot_chole_sbp)

# tot_chole - dbp
tot_chole_dbp_n1_clusters <- ggplot(nodrink_1smoke, aes(x = tot_chole, y = DBP)) +
  geom_point() +
  labs(x = "tot_chole", y = "DBP", title = "tot_chole and DBP of no drink 1 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
tot_chole_dbp_n2_clusters <- ggplot(nodrink_2smoke, aes(x = tot_chole, y = DBP)) +
  geom_point() +
  labs(x = "tot_chole", y = "DBP", title = "tot_chole and DBP of no drink 2 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
tot_chole_dbp_n3_clusters <- ggplot(nodrink_3smoke, aes(x = tot_chole, y = DBP)) +
  geom_point() +
  labs(x = "tot_chole", y = "DBP", title = "tot_chole and DBP of no drink 3 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
tot_chole_dbp_y1_clusters <- ggplot(yodrink_1smoke, aes(x = tot_chole, y = DBP)) +
  geom_point() +
  labs(x = "tot_chole", y = "DBP", title = "tot_chole and DBP of yes drink 1 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
tot_chole_dbp_y2_clusters <- ggplot(yodrink_2smoke, aes(x = tot_chole, y = DBP)) +
  geom_point() +
  labs(x = "tot_chole", y = "DBP", title = "tot_chole and DBP of yes drink 2 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
tot_chole_dbp_y3_clusters <- ggplot(yodrink_3smoke, aes(x = tot_chole, y = DBP)) +
  geom_point() +
  labs(x = "tot_chole", y = "DBP", title = "tot_chole and DBP of yes drink 3 smoke") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
tot_chole_dbp <- ggarrange(tot_chole_dbp_n1_clusters,
                           tot_chole_dbp_n2_clusters,
                           tot_chole_dbp_n3_clusters,
                           tot_chole_dbp_y1_clusters,
                           tot_chole_dbp_y2_clusters,
                           tot_chole_dbp_y3_clusters,
                           ncol = 3, nrow = 2)
ggsave("./output/Lab1_EDA/data_review/tot_chole_dbp_cluster.png")

# hemoglobin -> sbp
hemoglobin_sbp_n1_clusters <- ggplot(nodrink_1smoke, aes(x = hemoglobin, y = SBP)) +
  geom_point() +
  labs(x = "hemoglobin", y = "SBP", title = "hemoglobin and SBP of no drink 1 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hemoglobin_sbp_n2_clusters <- ggplot(nodrink_2smoke, aes(x = hemoglobin, y = SBP)) +
  geom_point() +
  labs(x = "hemoglobin", y = "SBP", title = "hemoglobin and SBP of no drink 2 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hemoglobin_sbp_n3_clusters <- ggplot(nodrink_3smoke, aes(x = hemoglobin, y = SBP)) +
  geom_point() +
  labs(x = "hemoglobin", y = "SBP", title = "hemoglobin and SBP of no drink 3 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hemoglobin_sbp_y1_clusters <- ggplot(yodrink_1smoke, aes(x = hemoglobin, y = SBP)) +
  geom_point() +
  labs(x = "hemoglobin", y = "SBP", title = "hemoglobin and SBP of yes drink 1 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hemoglobin_sbp_y2_clusters <- ggplot(yodrink_2smoke, aes(x = hemoglobin, y = SBP)) +
  geom_point() +
  labs(x = "hemoglobin", y = "SBP", title = "hemoglobin and SBP of yes drink 2 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hemoglobin_sbp_y3_clusters <- ggplot(yodrink_3smoke, aes(x = hemoglobin, y = SBP)) +
  geom_point() +
  labs(x = "hemoglobin", y = "SBP", title = "hemoglobin and SBP of yes drink 3 smoke") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hemoglobin_sbp <- ggarrange(hemoglobin_sbp_n1_clusters,
                            hemoglobin_sbp_n2_clusters,
                            hemoglobin_sbp_n3_clusters,
                            hemoglobin_sbp_y1_clusters,
                            hemoglobin_sbp_y2_clusters,
                            hemoglobin_sbp_y3_clusters,
                            ncol = 3, nrow = 2)
ggsave("./output/Lab1_EDA/data_review/hemoglobin_sbp_cluster.png")

# hemoglobin -> dbp
hemoglobin_dbp_n1_clusters <- ggplot(nodrink_1smoke, aes(x = hemoglobin, y = DBP)) +
  geom_point() +
  labs(x = "hemoglobin", y = "DBP", title = "hemoglobin and DBP of no drink 1 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hemoglobin_dbp_n2_clusters <- ggplot(nodrink_2smoke, aes(x = hemoglobin, y = DBP)) +
  geom_point() +
  labs(x = "hemoglobin", y = "DBP", title = "hemoglobin and DBP of no drink 2 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hemoglobin_dbp_n3_clusters <- ggplot(nodrink_3smoke, aes(x = hemoglobin, y = DBP)) +
  geom_point() +
  labs(x = "hemoglobin", y = "DBP", title = "hemoglobin and DBP of no drink 3 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hemoglobin_dbp_y1_clusters <- ggplot(yodrink_1smoke, aes(x = hemoglobin, y = DBP)) +
  geom_point() +
  labs(x = "hemoglobin", y = "DBP", title = "hemoglobin and DBP of yes drink 1 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hemoglobin_dbp_y2_clusters <- ggplot(yodrink_2smoke, aes(x = hemoglobin, y = DBP)) +
  geom_point() +
  labs(x = "hemoglobin", y = "DBP", title = "hemoglobin and DBP of yes drink 2 smoke") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hemoglobin_dbp_y3_clusters <- ggplot(yodrink_3smoke, aes(x = hemoglobin, y = DBP)) +
  geom_point() +
  labs(x = "hemoglobin", y = "DBP", title = "hemoglobin and DBP of yes drink 3 smoke") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
hemoglobin_dbp <- ggarrange(hemoglobin_dbp_n1_clusters,
                            hemoglobin_dbp_n2_clusters,
                            hemoglobin_dbp_n3_clusters,
                            hemoglobin_dbp_y1_clusters,
                            hemoglobin_dbp_y2_clusters,
                            hemoglobin_dbp_y3_clusters,
                            ncol = 3, nrow = 2)
ggsave("./output/Lab1_EDA/data_review/hemoglobin_dbp_cluster.png")