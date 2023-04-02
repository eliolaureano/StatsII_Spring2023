
# load the required libraries
library(rstudioapi)
library(readstata13)
library(pmd)
library(Amelia)
library(randomizr)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(qualtRics)
library(Hmisc)
library(MASS)
library(foreign)
library(tidyverse)
library(reporttools)
library(haven)
library(lmtest)
library(sandwich)



# Load Survey Data


covid19_survey2 <- read.dta13("replication_data_final.dta", convert.factors=T)

covid19_survey2$date <- substr(as.character(covid19_survey2$EndDate), start = 9, stop = 10)

colnames(covid19_survey2)[(colnames(covid19_survey2) == 'region')] <- 'region_code'

region_infect <- read.csv2("region_infect_rate.csv", sep = ",")[, c('region_code', "reg_infect_date", 'reg_cases_pc', 'reg_death_pc')]
region_infect$region_code <- as.numeric(as.character(region_infect$region_code) )
region_infect$date <- substr(str_pad(region_infect$reg_infect_date, 9, pad = "0"), start = 1, stop = 2)
region_infect$reg_cases_pc <- as.numeric(as.character(region_infect$reg_cases_pc))
region_infect$reg_deaths_pc <- as.numeric(as.character(region_infect$reg_death_pc))

covid.tmp <- merge(covid19_survey2, region_infect, by=c("region_code", 'date'), all.x = TRUE, all.y = FALSE)

prov_infect <- read.csv2("prov_infect_rate.csv", sep = ",")[, c('province_code', "prov_infect_date", 'prov_cases_pc', 'prov_death_pc')]
prov_infect$province_code <- as.character(str_pad(prov_infect$province_code, 2, pad = "0")) 
prov_infect$date <- substr(str_pad(prov_infect$prov_infect_date, 9, pad = "0"), start = 1, stop = 2)
prov_infect$prov_cases_pc <- as.numeric(as.character(prov_infect$prov_cases_pc))
prov_infect$prov_deaths_pc <- as.numeric(as.character(prov_infect$prov_death_pc))

covid19 <- merge(covid.tmp, prov_infect, by=c("province_code", "date"), all.x = TRUE)

(covid19 <- covid19 %>% #gender
    mutate(gender = case_when(
      covid19$Q2 == 1 ~ NA_real_,
      covid19$Q2 == 3 ~ 0, #male
      covid19$Q2 == 4 ~ 1  #female
    )))

(covid19 <- covid19 %>% #age_category
    mutate(age_cat = case_when(
      covid19$Q3 == 1 ~ 0,
      covid19$Q3 == 2 ~ 1,
      covid19$Q3 == 3 ~ 2,
      covid19$Q3 == 4 ~ 3,
      covid19$Q3 == 5 ~ 4,
      covid19$Q3 == 6 ~ 5
    )))

(covid19 <- covid19 %>% #age_category
    mutate(age_cat2 = case_when(
      covid19$Q3 == 1 ~ NA_real_,
      covid19$Q3 == 2 ~ 0,
      covid19$Q3 == 3 ~ 1,
      covid19$Q3 == 4 ~ 2,
      covid19$Q3 == 5 ~ 3,
      covid19$Q3 == 6 ~ 4
    )))

(covid19 <- covid19 %>% #education
    mutate(educ_cat = case_when(
      covid19$Q162 == 1 ~ 0,#secondary or less
      covid19$Q162 == 2 ~ 1,#high school
      covid19$Q162 == 3 ~ 2,#some college
      covid19$Q162 == 4 ~ 3,#college
      covid19$Q162 == 5 ~ 4,#graduate (MA)
      covid19$Q162 == 6 ~ 4#graduate (PhD)
    )))

(covid19 <- covid19 %>% #town size
    mutate(town_size = case_when(
      covid19$Q6 == 1 ~ 5,#capital
      covid19$Q6 == 2 ~ 4,#more100k
      covid19$Q6 == 3 ~ 3,#50to100k
      covid19$Q6 == 4 ~ 2,#20to50k
      covid19$Q6 == 5 ~ 1,#5to20k
      covid19$Q6 == 6 ~ 0#less than 5k
    )))

(covid19 <- covid19 %>% #occupation_category
    mutate(occup_cat = case_when(
      covid19$Q158 == 1 ~ 1,#Big business manager -> Small/Big business manager
      covid19$Q158 == 2 ~ 2,#Engineer
      covid19$Q158 == 3 ~ 7,#Doctor -> Medical Worker
      covid19$Q158 == 4 ~ 4,#Teacher/professor
      covid19$Q158 == 5 ~ 5,#Other qualified
      covid19$Q158 == 6 ~ 1,#Small business manager -> Small/Big business manager
      covid19$Q158 == 7 ~ 7,#Nurse -> Medical Worker
      covid19$Q158 == 8 ~ 8,#Middle skilled worker
      covid19$Q158 == 9 ~ 16,#Self-employed -> Others
      covid19$Q158 == 10 ~ 10,#Middle manager -> Supervisor/Middle Manager
      covid19$Q158 == 11 ~ 10,#Supervisor -> Supervisor/Middle Manager
      covid19$Q158 == 12 ~ 12,#Technician
      covid19$Q158 == 13 ~ 13,#Industrial worker -> Agriculture/Industrial Worker
      covid19$Q158 == 14 ~ 13,#Agriculture worker -> Agriculture/Industrial Worker
      covid19$Q158 == 15 ~ 15,#Unskilled worker
      covid19$Q158 == 16 ~ 16,#Unclassified -> Others
      covid19$Q158 == "" & covid19$age_cat == 5 ~ 17,#retired
      covid19$Q158 == "" & covid19$age_cat == 1 ~ 18,#student
      covid19$Q158 == "" ~ 16,#NAs -Others
    )))

(covid19 <- covid19 %>% #time_at_home
    mutate(time_home = case_when(
      covid19$Q24 == 1 ~ 3,#much more
      covid19$Q24 == 2 ~ 2,#more
      covid19$Q24 == 3 ~ 1,#a bit more
      covid19$Q24 == 4 ~ 0,#usual
    )))

(covid19 <- covid19 %>% #work_affect_covid
    mutate(work_affect = case_when(
      covid19$Q60 == 1 ~ 2,#erte
      covid19$Q60 == 2 ~ 3,#dismissed
      covid19$Q60 == 3 ~ 4,#telework
      covid19$Q60 == 4 & (covid19$occup_cat == 17 | covid19$occup_cat == 18) ~ 1,#inactive
      covid19$Q60 == 4 ~ 5,#unaffected
    )))


#Figure 3: Histogram, mask-wearing behavior

h = hist(covid19$facemask,plot=FALSE)
h$density = round(h$counts[h$counts!=0]/sum(h$counts)*100, 2)
temp_dat <- as.data.frame(cbind(c('Never', 'Rarely', 'Occasionally', 'Very frequently'), as.numeric(h$density)))

pdf("descr_facemask.pdf")
ggplot(temp_dat, aes(x = temp_dat[,1], y = as.numeric(as.character(temp_dat[,2])))) +
  geom_bar(stat='identity') +
  scale_x_discrete(limits=c('Never', 'Rarely', 'Occasionally', 'Very frequently')) +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 20, r = 0, b = 2, l = 0))) +
  theme(axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 2, l = 0))) +
  labs(title="", x="Mask-wearing behavior", y = "Percentage of respondents") +
  scale_y_continuous(breaks=c(seq(0,50,10)), limits = c(0,50)) +
  theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white", colour = NA)) +
  annotate(geom="text", x=1, y=45, label="41%",
             color="black") +
  annotate(geom="text", x=2, y=14, label="10%",
             color="black") +
  annotate(geom="text", x=3, y=30, label="25.8%",
             color="black") +
  annotate(geom="text", x=4, y=27, label="23.1%",
             color="black")
dev.off()

#Table 1: Ordinal Logistic Regressions Investigating the Association Between Demographic Characteristics and Mask Use

summary(demo_ordinal <- polr(as.factor(facemask) ~ as.factor(gender) + as.factor(age_cat) + as.factor(educ_cat) + as.factor(occup_cat) + time_home + as.factor(work_affect), data = covid19, Hess=TRUE))

(sumtable <- coef(summary(demo_ordinal)))
p <- pnorm(abs(sumtable[, "t value"]), lower.tail = FALSE) * 2

(ci <- confint(demo_ordinal))

TABLE_1 <- cbind(round(exp(cbind(OR = coef(demo_ordinal), ci)), 2), p = round(p[1:length(coef(demo_ordinal))], 3))

print(TABLE_1)





# Replication



# Re-estimation using OLS model
model2 <- polr(as.factor(facemask) ~ gender + age_cat + educ_cat + occup_cat + time_home + work_affect, data = covid19, Hess=TRUE)

#summary of the model
(sumtable <- coef(summary(model2)))
p <- pnorm(abs(sumtable[, "t value"]), lower.tail = FALSE) * 2

(ci <- confint(model2))

TABLE_2 <- cbind(round(exp(cbind(OR = coef(model2), ci)), 2), p = round(p[1:length(coef(model2))], 3))

print(TABLE_2)



# F-test



# Check the important model
library(lmtest)
# perform f-test
lrtest(demo_ordinal, model2)



