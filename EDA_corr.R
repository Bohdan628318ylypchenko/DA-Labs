library(tidyverse)
library(ggplot2)
library(corrplot)

# load, numerize, display data
data <- read_csv('original-data.csv')
data <- data %>% mutate(
  sex = as.numeric(as.factor(sex)), # male = 2, female = 1
  DRK_YN = as.numeric(as.factor(DRK_YN)) # 1 = no, 2 = yrs
)
glimpse(data)

# visualize correlation matrix
file_path= "./output/Lab1_EDA/corr/corr_all.png"
png(height=1200, width=1200, file=file_path, type = "cairo")
corrplot(cor(data), method = "ellipse")
dev.off()

# Scatter Point plot: hemoglobin | weight
sp_hemoglobin_weight <- ggplot(data, aes(x = weight, y = hemoglobin)) +
  geom_point(alpha = 0.2) +
  labs(x = "weight", y = "hemoglobin", title = "Scatter weight | hemoglobin") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
ggsave("./output/Lab1_EDA/corr/sp_hemoglobin_weight.png", plot = sp_hemoglobin_weight)

# Scatter Point plot: HDL_chole | weight
sp_weight_hdl_chole <- ggplot(data, aes(x = weight, y = log(HDL_chole))) +
  geom_point(alpha = 0.2) +
  labs(x = "weight", y = "HDL_chole", title = "Scatter weight | log(HDL_chole)") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
ggsave("./output/Lab1_EDA/corr/sp_hdl_chole_weight.png", plot = sp_weight_hdl_chole)

# Scatter Point plot: triglyceride | HDL_chole
sp_triglyceride_hdl_chole <- ggplot(data, aes(x = triglyceride, y = log(HDL_chole))) +
  geom_point(alpha = 0.2) +
  labs(x = "tridlyceride", y = "log(HDL_chole)", title = "Scatter triglyceride | log(HDL_chole)") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
ggsave("./output/Lab1_EDA/corr/sp_triglyceride_hdl_chole.png", plot = sp_triglyceride_hdl_chole)

# Scatter Point plot: hemoglobin | HDL_chole
sp_hemoglobin_hdl_chole <- ggplot(data, aes(x = hemoglobin, y = log(HDL_chole))) +
  geom_point(alpha = 0.2) +
  labs(x = "hemoglobin", y = "log(HDL_chole)", title = "Scatter hemoglobin | log(HDL_chole)") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8))
ggsave("./output/Lab1_EDA/corr/sp_hemoglobin_hdl_chole.png", plot = sp_hemoglobin_hdl_chole)
