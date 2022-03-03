# 13089020400
candler <- subset(atlas,  state == 13 & county == 089 & tract == 020400)
dekalb <- subset(atlas,  state == 13 & county == 089)
georgia <- subset(atlas,  state == 13)


# Histogram of kfr_pooled_pooled_p25
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse) 
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)

ggplot(atlas) + geom_histogram(aes(x=kfr_pooled_pooled_p25, y=..density..)) 

# Summary stats for kfr_pooled_pooled_p25
summary(atlas$kfr_pooled_pooled_p25)
sd(atlas$kfr_pooled_pooled_p25, na.rm=TRUE) 

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  26.57   31.26   34.75   36.79   40.59   65.04       1 
#  SD: 7.4198

# Candler Park and Dekalb kfr_pooled_pooled_p25 = 
mean(candler$kfr_pooled_pooled_p25, na.rm = TRUE)
sd(dekalb$kfr_pooled_pooled_p25, na.rm=TRUE)
sd(georgia$kfr_pooled_pooled_p25, na.rm=TRUE)

# Subsets based on HOLC Grade
grade_a <- subset(atlas, HOLC_A > 0.5 & !is.na(HOLC_A))
grade_b <- subset(atlas, HOLC_B > 0.5 & !is.na(HOLC_B))
grade_c <- subset(atlas, HOLC_C > 0.5 & !is.na(HOLC_C))
grade_d <- subset(atlas, HOLC_D > 0.5 & !is.na(HOLC_D))

# Mean kfr_pooled_pooled_p25 by HOLC Grade
mean(grade_a$kfr_pooled_pooled_p25, na.rm = TRUE) # 44.01404
mean(grade_b$kfr_pooled_pooled_p25, na.rm = TRUE) # 42.46768
mean(grade_c$kfr_pooled_pooled_p25, na.rm = TRUE) # 39.87268
mean(grade_d$kfr_pooled_pooled_p25, na.rm = TRUE) # 36.15839

# Mean kfr_black_pooled_p25 by HOLC Grade
mean(grade_a$kfr_black_pooled_p25, na.rm = TRUE) # 34.43756
mean(grade_b$kfr_black_pooled_p25, na.rm = TRUE) # 34.50712
mean(grade_c$kfr_black_pooled_p25, na.rm = TRUE) # 33.23932
mean(grade_d$kfr_black_pooled_p25, na.rm = TRUE) # 31.61861

# Mean kfr_white_pooled_p25 by HOLC Grade
mean(grade_a$kfr_white_pooled_p25, na.rm = TRUE) # 50.36435
mean(grade_b$kfr_white_pooled_p25, na.rm = TRUE) # 48.75348
mean(grade_c$kfr_white_pooled_p25, na.rm = TRUE) # 46.32723
mean(grade_d$kfr_white_pooled_p25, na.rm = TRUE) # 44.11593




