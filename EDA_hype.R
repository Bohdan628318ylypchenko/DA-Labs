library(tidyverse)
library(dplyr)
library(qqplotr)
library(ggpubr)

# load and display data
data <- read_csv('clean-data.csv')


# >>> drink >>>

# split into drinker / not drinker
data_ndrinker <- data %>% filter(DRK_YN == "N")
data_ydrinker <- data %>% filter(DRK_YN == "Y")

# sbp
sbp_fhist_nd <- ggplot(data_ndrinker, aes(x = SBP)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_ydrinker$SBP), col = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(data_ndrinker$SBP), max(data_ndrinker$SBP),54)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "sbp (no drinker)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
sbp_fhist_yd <- ggplot(data_ydrinker, aes(x = SBP)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_ydrinker$SBP), col = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(data_ydrinker$SBP), max(data_ydrinker$SBP), 5)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "sbp (drinker)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
drink_sbp_fhist <- ggarrange(sbp_fhist_nd, sbp_fhist_yd, ncol = 1, nrow = 2)
ggsave("./output/Lab1_EDA/hype/drink_sbp_fhist.png", plot = drink_sbp_fhist)

# dbp
dbp_fhist_nd <- ggplot(data_ndrinker, aes(x = DBP)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_ndrinker$DBP), col = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(data_ndrinker$DBP), max(data_ndrinker$DBP), 5)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "dbp (no drinker)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dbp_fhist_yd <- ggplot(data_ydrinker, aes(x = DBP)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_ydrinker$DBP), col = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(data_ydrinker$DBP), max(data_ydrinker$DBP), 5)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "dbp (drinker)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
drink_dbp_fhist <- ggarrange(dbp_fhist_nd, dbp_fhist_yd, ncol = 1, nrow = 2)
ggsave("./output/Lab1_EDA/hype/drink_dbp_fhist.png", plot = drink_dbp_fhist)

# tot_chole
tot_chole_fhist_nd <- ggplot(data_ndrinker, aes(x = tot_chole)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_ndrinker$tot_chole), col = "blue", linewidth = 1) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "tot_chole (no drinker)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
tot_chole_fhist_yd <- ggplot(data_ydrinker, aes(x = tot_chole)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_ydrinker$tot_chole), col = "blue", linewidth = 1) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "tot_chole (drinker)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
drink_tot_chole_fhist <- ggarrange(tot_chole_fhist_nd, tot_chole_fhist_yd, ncol = 1, nrow = 2)
ggsave("./output/Lab1_EDA/hype/drink_tot_chole_fhist.png", plot = drink_tot_chole_fhist)

# hemoglobin
hemoglobin_fhist_nd <- ggplot(data_ndrinker, aes(x = hemoglobin)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_ndrinker$hemoglobin), col = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(data_ndrinker$hemoglobin), max(data_ndrinker$hemoglobin), 10)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "hemoglobin (no drinker)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
hemoglobin_fhist_yd <- ggplot(data_ydrinker, aes(x = hemoglobin)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_ydrinker$hemoglobin), col = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(data_ydrinker$hemoglobin), max(data_ydrinker$hemoglobin), 10)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "hemoglobin (drinker)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
drink_hemoglobin_fhist <- ggarrange(hemoglobin_fhist_nd, hemoglobin_fhist_yd, ncol = 1, nrow = 2)
ggsave("./output/Lab1_EDA/hype/hemoglobin_fhist.png", plot = drink_hemoglobin_fhist)


# >>> smoking >>>

# split by smoking category
data_1s = data %>% filter(SMK_stat_type_cd == 1)
data_2s = data %>% filter(SMK_stat_type_cd == 2)
data_3s = data %>% filter(SMK_stat_type_cd == 3)

# sbp
sbp_fhist_1s <- ggplot(data_1s, aes(x = SBP)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_1s$SBP), col = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(data_1s$SBP), max(data_1s$SBP), 5)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "sbp (1s)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
sbp_fhist_2s <- ggplot(data_2s, aes(x = SBP)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_2s$SBP), col = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(data_2s$SBP), max(data_2s$SBP), 5)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "sbp (2s)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
sbp_fhist_3s <- ggplot(data_3s, aes(x = SBP)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_3s$SBP), col = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(data_3s$SBP), max(data_3s$SBP), 5)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "sbp (3s)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
smoke_sbp_fhist <- ggarrange(sbp_fhist_1s, sbp_fhist_2s, sbp_fhist_3s, ncol = 1, nrow = 3)
ggsave("./output/Lab1_EDA/hype/smoke_sbp_fhist.png", plot = smoke_sbp_fhist)

# dbp
dbp_fhist_1s <- ggplot(data_1s, aes(x = DBP)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_1s$DBP), col = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(data_1s$DBP), max(data_1s$DBP), 5)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "dbp (1s)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dbp_fhist_2s <- ggplot(data_2s, aes(x = DBP)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_2s$DBP), col = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(data_2s$DBP), max(data_2s$DBP), 5)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "dbp (2s)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dbp_fhist_3s <- ggplot(data_3s, aes(x = DBP)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_3s$DBP), col = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(data_3s$DBP), max(data_3s$DBP), 5)) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "dbp (3s)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
smoke_dbp_fhist <- ggarrange(dbp_fhist_1s, dbp_fhist_2s, dbp_fhist_3s, ncol = 1, nrow = 3)
ggsave("./output/Lab1_EDA/hype/smoke_dbp_fhist.png", plot = smoke_dbp_fhist)

# tot_chole
tot_chole_fhist_1s <- ggplot(data_1s, aes(x = tot_chole)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_1s$tot_chole), col = "blue", linewidth = 1) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "tot_chole (1s)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
tot_chole_fhist_2s <- ggplot(data_2s, aes(x = tot_chole)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_2s$tot_chole), col = "blue", linewidth = 1) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "tot_chole (2s)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
tot_chole_fhist_3s <- ggplot(data_3s, aes(x = tot_chole)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_3s$tot_chole), col = "blue", linewidth = 1) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "tot_chole (3s)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
smoke_tot_chole_fhist <- ggarrange(tot_chole_fhist_1s, tot_chole_fhist_2s, tot_chole_fhist_3s, ncol = 1, nrow = 3)
ggsave("./output/Lab1_EDA/hype/smoke_tot_chole_fhist.png", plot = smoke_tot_chole_fhist)

# hemoglobin
hemoglobin_fhist_1s <- ggplot(data_1s, aes(x = hemoglobin)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_1s$hemoglobin), col = "blue", linewidth = 1) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "hemoglobin (1s)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
hemoglobin_fhist_2s <- ggplot(data_2s, aes(x = hemoglobin)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_2s$hemoglobin), col = "blue", linewidth = 1) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "hemoglobin (2s)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
hemoglobin_fhist_3s <- ggplot(data_3s, aes(x = hemoglobin)) +
  geom_boxplot() +
  geom_vline(xintercept=mean(data_3s$hemoglobin), col = "blue", linewidth = 1) +
  scale_y_continuous(n.breaks = 20, labels = scales::comma) +
  labs(x = "hemoglobin (3s)", y = "frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
smoke_hemoglobin_fhist <- ggarrange(hemoglobin_fhist_1s, hemoglobin_fhist_2s, hemoglobin_fhist_3s, ncol = 1, nrow = 3)
ggsave("./output/Lab1_EDA/hype/smoke_hemoglobin_fhist.png", plot = smoke_hemoglobin_fhist)


# >>> mean >>>
print(paste("mean SBP (no drinker): ", as.character(mean(data_ndrinker$SBP))))
print(paste("mean SBP (drinker): ", as.character(mean(data_ydrinker$SBP))))
print("\n")
print(paste("mean DBP (no drinker): ", as.character(mean(data_ndrinker$DBP))))
print(paste("mean DBP (drinker): ", as.character(mean(data_ydrinker$DBP))))
print("\n")
print(paste("mean tot_chole (no drinker): ", as.character(mean(data_ndrinker$tot_chole))))
print(paste("mean tot_chole (drinker): ", as.character(mean(data_ydrinker$tot_chole))))
print("\n")
print(paste("mean hemoglobin (no drinker): ", as.character(mean(data_ndrinker$hemoglobin))))
print(paste("mean hemoglobin (drinker): ", as.character(mean(data_ydrinker$hemoglobin))))
print("\n")
print(paste("mean sbp (1s): ", as.character(mean(data_1s$SBP))))
print(paste("mean sbp (2s): ", as.character(mean(data_2s$SBP))))
print(paste("mean sbp (3s): ", as.character(mean(data_3s$SBP))))
print("\n")
print(paste("mean dbp (1s): ", as.character(mean(data_1s$DBP))))
print(paste("mean dbp (2s): ", as.character(mean(data_2s$DBP))))
print(paste("mean dbp (3s): ", as.character(mean(data_3s$DBP))))
print("\n")
print(paste("mean tot_chole (1s): ", as.character(mean(data_1s$tot_chole))))
print(paste("mean tot_chole (2s): ", as.character(mean(data_2s$tot_chole))))
print(paste("mean tot_chole (3s): ", as.character(mean(data_3s$tot_chole))))
print("\n")
print(paste("mean hemoglobin (1s): ", as.character(mean(data_1s$hemoglobin))))
print(paste("mean hemoglobin (2s): ", as.character(mean(data_2s$hemoglobin))))
print(paste("mean hemoglobin (3s): ", as.character(mean(data_3s$hemoglobin))))
print("\n")