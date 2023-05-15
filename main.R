##############################################################
# Anova using R
##############################################################
install.packages(c("ggpubr", "rstatix", "tidyverse", "ggboxplot"))
# Loading package
library(rstatix)
library(tidyverse)
library(ggpubr)
library(ggboxplot)

# clearing Workspace
rm(list=ls())
#############################################################
#importing the data from local disk
data <- read.csv("E:/R_Project_finished/2022_9_04_R_one-way Anova/Data2WayAnova.csv")
ID <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
data$ID <- ID
summary(data)
data <- data %>%
  gather(key = "drug", value = "reaction_time", Congruent, Incongruent, Uninformative) %>%
  convert_as_factor(ID, drug)
head(data, 3)

summary<-data %>%
  group_by(drug) %>%
  get_summary_stats(reaction_time, type = "mean_sd")
summary

bxp <- ggboxplot(data, x = "drug", y = "reaction_time", add = "point")
bxp

# Outlier Detection
outlier<-data %>%
  group_by(drug) %>%
  identify_outliers(reaction_time)
outlier

# Normality Checking
normality<-data %>%
  group_by(drug) %>%
  shapiro_test(reaction_time)
normality
ggqqplot(data, "reaction_time", facet.by = "drug")

# ANOVA calculation(Sphericity Assumption)
sphericity<-anova_test(data=data,dv=reaction_time,wid=ID, within=drug)
result <- data.frame(get_anova_table(sphericity))
result
write.csv(result,"E:\\result(one-way repeated measure of ANOVA).csv", row.names = FALSE)

# Post-hoc tests
pair<-data %>% 
  pairwise_t_test( reaction_time ~ drug,paired=TRUE, p.adjust.method = "bonferroni" ) 
data.frame(pair)
write.csv(pair,"E:\\result_2(Post-hoc test).csv", row.names = FALSE)
