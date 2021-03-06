# Load database
library(haven)
atlas <- read_dta("~/ec50/EP1/atlas.dta")

# Create Data frames
candler <- subset(atlas,  state == 13 & county == 089 & tract == 020400)
dekalb <- subset(atlas,  state == 13 & county == 089)
georgia <- subset(atlas,  state == 13)
comm_zone <- subset(atlas, cz == 9100)
single_not_NA <- subset(comm_zone, !is.na(singleparent_share1990) & !is.na(kfr_pooled_pooled_p25))
popdensity_not_NA <- subset(comm_zone, !is.na(popdensity2000) & !is.na(kfr_pooled_pooled_p25))

# Install required packages
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse) 
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(statar)) install.packages("statar"); library(statar)

# Histogram of kfr_pooled_pooled_p25
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

# Examining for covariates with kfr_pooled_pooled_p25 within commuting zone

# Measuring against foreign share - correlation, but not linear
ggplot(comm_zone, aes(x = foreign_share2010, y = kfr_pooled_pooled_p25)) + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE)

# Measuring against single parent share - Good correlation
ggplot(comm_zone, aes(x = singleparent_share1990, y = kfr_pooled_pooled_p25)) + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE)
# Correlation Coefficient
cor(single_not_NA$kfr_pooled_pooled_p25, single_not_NA$singleparent_share1990)
# r = -0.6666442

# Measuring against job density - Decent correlation
ggplot(comm_zone, aes(x = jobs_total_5mi_2015, y = kfr_pooled_pooled_p25)) + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE)

# Measuring against high paying job density - Very similar to previous
ggplot(comm_zone, aes(x = jobs_highpay_5mi_2015, y = kfr_pooled_pooled_p25)) + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE)

# Measuring against population density - Decent correlation
ggplot(comm_zone, aes(x = popdensity2000, y = kfr_pooled_pooled_p25)) + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE)
# Correlation Coefficient
cor(popdensity_not_NA$kfr_pooled_pooled_p25, popdensity_not_NA$popdensity2000)
# r = -0.2596039


# Now, we consider the two relationships that stuck out by racial group
# Starting with single parent measures
    # kfr_black_pooled_p25 not null - similar pattern, lower intercept flatter slope
    black_not_NA <- subset(comm_zone, !is.na(kfr_black_pooled_p25) & !is.na(singleparent_share1990))
    ggplot(comm_zone, aes(x = singleparent_share1990, y = kfr_black_pooled_p25)) + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE)
    cor(black_not_NA$kfr_black_pooled_p25, black_not_NA$popdensity2000)
    # r = -0.149883

    # kfr_white_pooled_p25 - similar pattern, higher intercept, steeper slope
    white_not_NA <- subset(comm_zone, !is.na(kfr_white_pooled_p25) & !is.na(singleparent_share1990))
    ggplot(comm_zone, aes(x = singleparent_share1990, y = kfr_white_pooled_p25)) + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE)
    cor(white_not_NA$kfr_white_pooled_p25, white_not_NA$popdensity2000)
    # r = 0.02174442?
    
# Now considering versus population density
    # kfr_black_pooled_p25
    ggplot(comm_zone, aes(x = popdensity2000, y = kfr_black_pooled_p25)) + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE)
    
    # kfr_white_pooled_p25
    ggplot(comm_zone, aes(x = popdensity2000, y = kfr_white_pooled_p25)) + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE)
    
ggplot(comm_zone, aes(x = kfr_pooled_pooled_p25, y = ann_avg_job_growth_2004_2013)) + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE)


# Create Bar Graph Representing upward mobility of certain regions near Candler Park
neighborhoods <- c(29.12689, 38.52973, 49.98878)
name <- c("Edgewood", "Candler Park", "Druid Hills")

barplot(neighborhoods, names.arg=name,xlab="Neighborhood",ylab="Upward Mobility (Percentile)",col=c("red", "yellow", "green"), main="Movility vs Neighborhood")

# Subsets of Dekalb County by HOLC
dekalb_a <- subset(comm_zone, HOLC_A > 0.5 & !is.na(HOLC_A))
dekalb_b <- subset(comm_zone, HOLC_B > 0.5 & !is.na(HOLC_B))
dekalb_c <- subset(comm_zone, HOLC_C > 0.5 & !is.na(HOLC_C))
dekalb_d <- subset(comm_zone, HOLC_D > 0.5 & !is.na(HOLC_D))

# Create Bar Graph to show HOLC vs p25
columns <- c(mean(dekalb_d$kfr_pooled_pooled_p25, na.rm = TRUE), mean(dekalb_c$kfr_pooled_pooled_p25, na.rm = TRUE), mean(dekalb_b$kfr_pooled_pooled_p25, na.rm = TRUE), mean(dekalb_a$kfr_pooled_pooled_p25, na.rm = TRUE))
name_list <- c("Grade D", "Grade C", "Grade B", "Grade A")

barplot(columns, names.arg=name_list,xlab="HOLC Grade",ylab="Upward Mobility (Percentile)",col=c("red", "yellow", "blue", "green"), main="Movility vs Neighborhood")

# Create Bar Graph to show same data for black residents
black_names <- c("Grade D", "Grade C", "Grade B")
columns_black <- c(mean(dekalb_d$kfr_black_pooled_p25, na.rm = TRUE), mean(dekalb_c$kfr_black_pooled_p25, na.rm = TRUE), mean(dekalb_b$kfr_black_pooled_p25, na.rm = TRUE))
barplot(columns_black, names.arg=black_names,xlab="HOLC Grade",ylab="Upward Mobility (Percentile)",col=c("red", "yellow", "blue"), main="Movility vs Neighborhood (Black Residents)")

# Create Bar Graph to show same data for white residents
columns_white <- c(mean(dekalb_d$kfr_white_pooled_p25, na.rm = TRUE), mean(dekalb_c$kfr_white_pooled_p25, na.rm = TRUE), mean(dekalb_b$kfr_white_pooled_p25, na.rm = TRUE), mean(dekalb_a$kfr_white_pooled_p25, na.rm = TRUE))
barplot(columns_white, names.arg=name_list,xlab="HOLC Grade",ylab="Upward Mobility (Percentile)",col=c("red", "yellow", "blue", "green"), main="Movility vs Neighborhood (White Residents)")

# Create Bar Graph to show same data for educated residents
a_edu <- subset(dekalb_a, frac_coll_plus2010 > 0.5)
b_edu <- subset(dekalb_b, frac_coll_plus2010 > 0.5)
c_edu <- subset(dekalb_c, frac_coll_plus2010 > 0.5)
d_edu <- subset(dekalb_d, frac_coll_plus2010 > 0.5)

columns_edu <- c(mean(d_edu$kfr_pooled_pooled_p25, na.rm = TRUE), mean(c_edu$kfr_pooled_pooled_p25, na.rm = TRUE), mean(b_edu$kfr_pooled_pooled_p25, na.rm = TRUE), mean(a_edu$kfr_pooled_pooled_p25, na.rm = TRUE))
barplot(columns_edu, names.arg=name_list,xlab="HOLC Grade",ylab="Upward Mobility (Percentile)",col=c("red", "yellow", "blue", "green"), main="Movility vs Neighborhood (Educated Communities)")

# Create Bar Graph to show same data for uneducated residents
a_nedu <- subset(dekalb_a, frac_coll_plus2010 < 0.5)
b_nedu <- subset(dekalb_b, frac_coll_plus2010 < 0.5)
c_nedu <- subset(dekalb_c, frac_coll_plus2010 < 0.5)
d_nedu <- subset(dekalb_d, frac_coll_plus2010 < 0.5)

nedu_names <- c("Grade D", "Grade C")

columns_nedu <- c(mean(d_nedu$kfr_pooled_pooled_p25, na.rm = TRUE), mean(c_nedu$kfr_pooled_pooled_p25, na.rm = TRUE))
barplot(columns_nedu, names.arg=nedu_names,xlab="HOLC Grade",ylab="Upward Mobility (Percentile)",col=c("red", "yellow", "blue", "green"), main="Movility vs Neighborhood (Uneducated Communities)")



