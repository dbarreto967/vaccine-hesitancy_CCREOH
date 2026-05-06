# vaccine hesitancy CCREOH

#################### Clean the Data ####################

library(tidyverse)

# read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

# select necessary columns and factor variables
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(child_vaccine, covid_pos, pos_household, mother_vaccine, age_mother, 
         age_child, ethnicity3, education2, employment2, 
         region_residence, num_house, gender_child
  ) %>%
  mutate(child_vaccine = as.factor(child_vaccine),
         covid_pos = as.factor(covid_pos),
         pos_household = as.factor(pos_household),
         mother_vaccine = as.factor(mother_vaccine),
         ethnicity3 = as.factor(ethnicity3),
         education2 = as.factor(education2),
         employment2 = as.factor(employment2),
         region_residence = as.factor(region_residence),
         num_house = as.factor(num_house),
         gender_child = as.factor(gender_child))

# remove rows with any NA
ImpactsOfCOVID19_clean <- na.omit(ImpactsOfCOVID19_Ranalysis)

# Frequency of child vaccine
table(ImpactsOfCOVID19_clean$child_vaccine)

# Recode child_vaccine to combine "no" and "not sure" levels into a "vaccine hesitancy" level

ImpactsOfCOVID19_clean$child_vaccinere <- ifelse(ImpactsOfCOVID19_clean$child_vaccine == 1, 1, 2)

ImpactsOfCOVID19_clean$child_vaccinere <- factor(ImpactsOfCOVID19_clean$child_vaccinere,
                                                 levels = c(1, 2),
                                                 labels = c("vaccine acceptance", "vaccine hesitance"))


#################### Label variables ####################

library(Hmisc)

# Adding labels for child_vaccine and its levels
ImpactsOfCOVID19_clean$child_vaccine <- factor(ImpactsOfCOVID19_clean$child_vaccine, levels = c(1, 2, 3),
                                               labels = c("Yes", "No", "Not sure"))

label(ImpactsOfCOVID19_clean$child_vaccine) <- "Intentions of COVID-19 vaccination for children"

# Adding labels for Age of mother (years)
label(ImpactsOfCOVID19_clean$age_mother) <- "Age of mother (years)"
label(ImpactsOfCOVID19_clean$age_mother)



# Adding labels for covid_pos and its levels
ImpactsOfCOVID19_clean$covid_pos <- factor(ImpactsOfCOVID19_clean$covid_pos, levels = c(0, 1),
                                           labels = c("No previous COVID-19 positive test results", "Previous COVID-19 positive test results"))

label(ImpactsOfCOVID19_clean$covid_pos) <- "Children with previous COVID-19 positive test results"

# Adding labels for pos_household and its levels
ImpactsOfCOVID19_clean$pos_household <- factor(ImpactsOfCOVID19_clean$pos_household, levels = c(0, 1),
                                               labels = c("No previous COVID-19 positive test results", "Previous COVID-19 positive test results"))

label(ImpactsOfCOVID19_clean$pos_household) <- "Household members with previous COVID-19 positive test results"

# Adding labels for mother_vaccine and its levels
ImpactsOfCOVID19_clean$mother_vaccine <- factor(ImpactsOfCOVID19_clean$mother_vaccine, levels = c(0, 1),
                                                labels = c("Vaccinated", "Non-vaccinated"))

label(ImpactsOfCOVID19_clean$mother_vaccine) <- "Maternal COVID-19 vaccination status"

# Adding labels for ethnicity3 and its levels
ImpactsOfCOVID19_clean$ethnicity3 <- factor(ImpactsOfCOVID19_clean$ethnicity3, levels = c(1, 2, 3),
                                            labels = c("Asian descent", "African descent", "Other (Indigenous, Mixed, Caucasian, Other)"))

label(ImpactsOfCOVID19_clean$ethnicity3) <- "Ethnicity"


# Adding labels for education2 and its levels
ImpactsOfCOVID19_clean$education2 <- factor(ImpactsOfCOVID19_clean$education2, levels = c(1, 2, 3),
                                            labels = c("None/primary/lower secondary", "Technical vocational/secondary", "Higher education(Masters/Bachelor)/other"))

label(ImpactsOfCOVID19_clean$education2) <- "Level of education of the mother"



# Adding labels for employment2 and its levels
ImpactsOfCOVID19_clean$employment2 <- factor(ImpactsOfCOVID19_clean$employment2, levels = c(0, 1),
                                             labels = c("Employed", "Unemployed"))

label(ImpactsOfCOVID19_clean$employment2) <- "Employment"


# Adding labels for region_residence and its levels
ImpactsOfCOVID19_clean$region_residence <- factor(ImpactsOfCOVID19_clean$region_residence, levels = c(0, 1),
                                                  labels = c("Paramaribo region", "Nickerie region"))

label(ImpactsOfCOVID19_clean$region_residence) <- "Recruitment region"


# Adding labels for num_house and its levels
ImpactsOfCOVID19_clean$num_house <- factor(ImpactsOfCOVID19_clean$num_house, levels = c(0, 1),
                                           labels = c("4 or fewer", "5 or more"))

label(ImpactsOfCOVID19_clean$num_house) <- "Household size"


# Adding labels for gender_child and its levels
ImpactsOfCOVID19_clean$gender_child <- factor(ImpactsOfCOVID19_clean$gender_child, levels = c(0, 1),
                                              labels = c("Female", "Male"))

label(ImpactsOfCOVID19_clean$gender_child) <- "Gender"


#################### Dataset's structure ####################

# https://statsandr.com/blog/binary-logistic-regression-in-r/#univariable-versus-multivariable-logistic-regression

str(ImpactsOfCOVID19_clean)

ImpactsOfCOVID19_clean

## preview of the final data frame
# print first 6 observations
head(ImpactsOfCOVID19_clean)

# basic descriptive statistics
summary(ImpactsOfCOVID19_clean)


library(crosstable)
library(dplyr)

crosstable(ImpactsOfCOVID19_clean, c(age_mother, education2, ethnicity3, employment2, mother_vaccine, covid_pos, 
                                     pos_household, region_residence, num_house, gender_child, age_child), 
           by=child_vaccinere, 
           total="both") %>% 
  as_flextable(keep_id=FALSE)



table(ImpactsOfCOVID19_clean$education2)


crosstable(ImpactsOfCOVID19_clean, c(child_vaccine), 
           by=child_vaccinere, 
           total="both") %>% 
  as_flextable(keep_id=FALSE)



#################### Normality and non-parametric tests (for age of mother and age of child) ####################

# age_mother

#is age_mother normally distributed?
##The function returns a p-value, which is used to determine if the data is normally distributed. If the p-value is greater than 0.05, you fail to reject the null hypothesis and assume the data is normally distributed. If the p-value is less than 0.05, you reject the null hypothesis and conclude the data is not normally distributed.
shapiro.test(ImpactsOfCOVID19_clean$age_mother)
#p-value = 0.0004681; the data is not normally distributed


#Q-Q plot
qqnorm(ImpactsOfCOVID19_clean$age_mother, main = "Q-Q plot of age of mother")
qqline(ImpactsOfCOVID19_clean$age_mother, col = "red", lwd = 2)

#Mann-Whitney U test (non-parametric) 
wilcox.test(age_mother ~ child_vaccinere, data = ImpactsOfCOVID19_clean)
#p-value = 0.4298; the distribution of age of mother is not significantly different by intention of mothers to vaccinate their children


# age_child

#is age_child normally distributed?
##The function returns a p-value, which is used to determine if the data is normally distributed. If the p-value is greater than 0.05, you fail to reject the null hypothesis and assume the data is normally distributed. If the p-value is less than 0.05, you reject the null hypothesis and conclude the data is not normally distributed.
shapiro.test(ImpactsOfCOVID19_clean$age_child)
#p-value <0.001; the data is not normally distributed


#Q-Q plot
qqnorm(ImpactsOfCOVID19_clean$age_child, main = "Q-Q plot of age of child")
qqline(ImpactsOfCOVID19_clean$age_child, col = "red", lwd = 2)

#Mann-Whitney U test (non-parametric) 
wilcox.test(age_child ~ child_vaccinere, data = ImpactsOfCOVID19_clean)
#p-value = 0.2233; the distribution of age of child is not significantly different by intention of mothers to vaccinate their children


#################### Chi-square tests for categorical variables ####################


# education2 and child_vaccinere

chisq.test(ImpactsOfCOVID19_clean$education2, ImpactsOfCOVID19_clean$child_vaccinere)

table(ImpactsOfCOVID19_clean$education2, ImpactsOfCOVID19_clean$child_vaccinere)

# H0: education2 is not associated with child_vaccinere.
# H1: education2 is associated with child_vaccinere.
# p-value = 0.8549
# We do not reject the null hypothesis and conclude that there is not association between education2 and child_vaccinere.



# ethnicity3 and child_vaccinere

chisq.test(ImpactsOfCOVID19_clean$ethnicity3, ImpactsOfCOVID19_clean$child_vaccinere)

table(ImpactsOfCOVID19_clean$ethnicity3, ImpactsOfCOVID19_clean$child_vaccinere)

# H0: ethnicity3 is not associated with child_vaccinere.
# H1: ethnicity3 is associated with child_vaccinere.
# p-value < 0.001
# We reject the null hypothesis and conclude that there is an association between ethnicity3 and child_vaccinere.


# employment2 and child_vaccinere

chisq.test(ImpactsOfCOVID19_clean$employment2, ImpactsOfCOVID19_clean$child_vaccinere)

table(ImpactsOfCOVID19_clean$employment2, ImpactsOfCOVID19_clean$child_vaccinere)

# H0: employment2 is not associated with child_vaccinere.
# H1: employment2 is associated with child_vaccinere.
# p-value = 0.8762
# We do not reject the null hypothesis and conclude that there is not association between employment2 and child_vaccinere.



# mother_vaccine and child_vaccinere

chisq.test(ImpactsOfCOVID19_clean$mother_vaccine, ImpactsOfCOVID19_clean$child_vaccinere)

table(ImpactsOfCOVID19_clean$mother_vaccine, ImpactsOfCOVID19_clean$child_vaccinere)

# H0: mother_vaccine is not associated with child_vaccinere.
# H1: mother_vaccine is associated with child_vaccinere.
# p-value < 0.001
# We reject the null hypothesis and conclude that there is an association between mother_vaccine and child_vaccinere.


# covid_pos and child_vaccinere

chisq.test(ImpactsOfCOVID19_clean$covid_pos, ImpactsOfCOVID19_clean$child_vaccinere)

table(ImpactsOfCOVID19_clean$covid_pos, ImpactsOfCOVID19_clean$child_vaccinere)

# H0: covid_pos is not associated with child_vaccinere.
# H1: covid_pos is associated with child_vaccinere.
# p-value < 0.001
# We reject the null hypothesis and conclude that there is an association between covid_pos and child_vaccinere.


# pos_household and child_vaccinere

chisq.test(ImpactsOfCOVID19_clean$pos_household, ImpactsOfCOVID19_clean$child_vaccinere)

table(ImpactsOfCOVID19_clean$pos_household, ImpactsOfCOVID19_clean$child_vaccinere)

#not significant


# region_residence and child_vaccinere

chisq.test(ImpactsOfCOVID19_clean$region_residence, ImpactsOfCOVID19_clean$child_vaccinere)

table(ImpactsOfCOVID19_clean$region_residence, ImpactsOfCOVID19_clean$child_vaccinere)

#significant correlation


# num_house and child_vaccinere

chisq.test(ImpactsOfCOVID19_clean$num_house, ImpactsOfCOVID19_clean$child_vaccinere)

table(ImpactsOfCOVID19_clean$num_house, ImpactsOfCOVID19_clean$child_vaccinere)

#not significant


# gender_child and child_vaccinere

chisq.test(ImpactsOfCOVID19_clean$gender_child, ImpactsOfCOVID19_clean$child_vaccinere)

table(ImpactsOfCOVID19_clean$gender_child, ImpactsOfCOVID19_clean$child_vaccinere)

#not significant


#################### Barplots for significantly correlated predictors ####################

# Grouped bar chart
library(ggplot2)

ggplot(ImpactsOfCOVID19_clean, aes(x = child_vaccinere, fill = ethnicity3)) +
  geom_bar(position = "dodge") +
  ylab("Count") +
  xlab("Intentions of COVID-19 vaccination for children") +
  scale_fill_hue() +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(fill = "Ethnicity")


ggplot(ImpactsOfCOVID19_clean, aes(x = child_vaccinere, fill = region_residence)) +
  geom_bar(position = "dodge") +
  ylab("Count") +
  xlab("Intentions of COVID-19 vaccination for children") +
  scale_fill_hue() +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(fill = "Recruitment region")


ggplot(ImpactsOfCOVID19_clean, aes(x = child_vaccinere, fill = mother_vaccine)) +
  geom_bar(position = "dodge") +
  ylab("Count") +
  xlab("Intentions of COVID-19 vaccination for children") +
  scale_fill_hue() +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(fill = "Maternal COVID-19 vaccination status")

ggplot(ImpactsOfCOVID19_clean, aes(x = child_vaccinere, fill = covid_pos)) +
  geom_bar(position = "dodge") +
  ylab("Count") +
  xlab("Intentions of COVID-19 vaccination for children") +
  scale_fill_hue() +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(fill = "Children with previous COVID-19 positive test results")






#################### Reasons for COVID-19 vaccine acceptance for child ####################

# Clean the Data 

#library(tidyverse)

# read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

# select necessary columns and factor variables
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(yes_childvaccine___1, yes_childvaccine___2, yes_childvaccine___3, 
         yes_childvaccine___4, yes_childvaccine___5, yes_childvaccine___6, yes_childvaccine___7
  ) %>%
  mutate(yes_childvaccine___1 = as.factor(yes_childvaccine___1),
         yes_childvaccine___2 = as.factor(yes_childvaccine___2),
         yes_childvaccine___3 = as.factor(yes_childvaccine___3),
         yes_childvaccine___4 = as.factor(yes_childvaccine___4),
         yes_childvaccine___5 = as.factor(yes_childvaccine___5),
         yes_childvaccine___6 = as.factor(yes_childvaccine___6),
         yes_childvaccine___7 = as.factor(yes_childvaccine___7))

# remove rows with any NA
ImpactsOfCOVID19_clean <- na.omit(ImpactsOfCOVID19_Ranalysis)



#Apply labels column by column

ImpactsOfCOVID19_clean$yes_childvaccine___1 <- factor(ImpactsOfCOVID19_clean$yes_childvaccine___1,
                                                      levels = c(0, 1),
                                                      labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$yes_childvaccine___2 <- factor(ImpactsOfCOVID19_clean$yes_childvaccine___2,
                                                      levels = c(0, 1),
                                                      labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$yes_childvaccine___3 <- factor(ImpactsOfCOVID19_clean$yes_childvaccine___3,
                                                      levels = c(0, 1),
                                                      labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$yes_childvaccine___4 <- factor(ImpactsOfCOVID19_clean$yes_childvaccine___4,
                                                      levels = c(0, 1),
                                                      labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$yes_childvaccine___5 <- factor(ImpactsOfCOVID19_clean$yes_childvaccine___5,
                                                      levels = c(0, 1),
                                                      labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$yes_childvaccine___6 <- factor(ImpactsOfCOVID19_clean$yes_childvaccine___6,
                                                      levels = c(0, 1),
                                                      labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$yes_childvaccine___7 <- factor(ImpactsOfCOVID19_clean$yes_childvaccine___7,
                                                      levels = c(0, 1),
                                                      labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean

# Dataset's structure
str(ImpactsOfCOVID19_clean)

# basic descriptive statistics
summary(ImpactsOfCOVID19_clean)






#################### Reasons for COVID-19 vaccine acceptance for mom ####################

# Clean the Data 

#library(tidyverse)

# read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

# select necessary columns and factor variables
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(yes_mothervaccine___1, yes_mothervaccine___2, yes_mothervaccine___3,
         yes_mothervaccine___4, yes_mothervaccine___5, yes_mothervaccine___6,
         yes_mothervaccine___7
  ) %>%
  mutate(yes_mothervaccine___1 = as.factor(yes_mothervaccine___1),
         yes_mothervaccine___2 = as.factor(yes_mothervaccine___2),
         yes_mothervaccine___3 = as.factor(yes_mothervaccine___3),
         yes_mothervaccine___4 = as.factor(yes_mothervaccine___4),
         yes_mothervaccine___5 = as.factor(yes_mothervaccine___5),
         yes_mothervaccine___6 = as.factor(yes_mothervaccine___6),
         yes_mothervaccine___7 = as.factor(yes_mothervaccine___7))

# remove rows with any NA
ImpactsOfCOVID19_clean <- na.omit(ImpactsOfCOVID19_Ranalysis)



#Apply labels column by column

ImpactsOfCOVID19_clean$yes_mothervaccine___1 <- factor(ImpactsOfCOVID19_clean$yes_mothervaccine___1,
                                                       levels = c(0, 1),
                                                       labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$yes_mothervaccine___2 <- factor(ImpactsOfCOVID19_clean$yes_mothervaccine___2,
                                                       levels = c(0, 1),
                                                       labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$yes_mothervaccine___3 <- factor(ImpactsOfCOVID19_clean$yes_mothervaccine___3,
                                                       levels = c(0, 1),
                                                       labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$yes_mothervaccine___4 <- factor(ImpactsOfCOVID19_clean$yes_mothervaccine___4,
                                                       levels = c(0, 1),
                                                       labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$yes_mothervaccine___5 <- factor(ImpactsOfCOVID19_clean$yes_mothervaccine___5,
                                                       levels = c(0, 1),
                                                       labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$yes_mothervaccine___6 <- factor(ImpactsOfCOVID19_clean$yes_mothervaccine___6,
                                                       levels = c(0, 1),
                                                       labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$yes_mothervaccine___7 <- factor(ImpactsOfCOVID19_clean$yes_mothervaccine___7,
                                                       levels = c(0, 1),
                                                       labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean

# Dataset's structure
str(ImpactsOfCOVID19_clean)

# basic descriptive statistics
summary(ImpactsOfCOVID19_clean)



#################### Reasons for COVID-19 vaccine hesitancy for child ####################

# Clean the Data 

#library(tidyverse)

# read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

# select necessary columns and factor variables
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(nonotsure_childvaccine___1, nonotsure_childvaccine___2, nonotsure_childvaccine___3,
         nonotsure_childvaccine___4, nonotsure_childvaccine___5, nonotsure_childvaccine___6,
         nonotsure_childvaccine___7, nonotsure_childvaccine___8, nonotsure_childvaccine___9,
         nonotsure_childvaccine___10, nonotsure_childvaccine___11, nonotsure_childvaccine___12,
         nonotsure_childvaccine___13, nonotsure_childvaccine___14, nonotsure_childvaccine___15,
         nonotsure_childvaccine___16
  ) %>%
  mutate(nonotsure_childvaccine___1 = as.factor(nonotsure_childvaccine___1),
         nonotsure_childvaccine___2 = as.factor(nonotsure_childvaccine___2),
         nonotsure_childvaccine___3 = as.factor(nonotsure_childvaccine___3),
         nonotsure_childvaccine___4 = as.factor(nonotsure_childvaccine___4),
         nonotsure_childvaccine___5 = as.factor(nonotsure_childvaccine___5),
         nonotsure_childvaccine___6 = as.factor(nonotsure_childvaccine___6),
         nonotsure_childvaccine___7 = as.factor(nonotsure_childvaccine___7),
         nonotsure_childvaccine___8 = as.factor(nonotsure_childvaccine___8),
         nonotsure_childvaccine___9 = as.factor(nonotsure_childvaccine___9),
         nonotsure_childvaccine___10 = as.factor(nonotsure_childvaccine___10),
         nonotsure_childvaccine___11 = as.factor(nonotsure_childvaccine___11),
         nonotsure_childvaccine___12 = as.factor(nonotsure_childvaccine___12),
         nonotsure_childvaccine___13 = as.factor(nonotsure_childvaccine___13),
         nonotsure_childvaccine___14 = as.factor(nonotsure_childvaccine___14),
         nonotsure_childvaccine___15 = as.factor(nonotsure_childvaccine___15),
         nonotsure_childvaccine___16 = as.factor(nonotsure_childvaccine___16))

# remove rows with any NA
ImpactsOfCOVID19_clean <- na.omit(ImpactsOfCOVID19_Ranalysis)



#Apply labels column by column

ImpactsOfCOVID19_clean$nonotsure_childvaccine___1 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___1,
                                                            levels = c(0, 1),
                                                            labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___2 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___2,
                                                            levels = c(0, 1),
                                                            labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___3 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___3,
                                                            levels = c(0, 1),
                                                            labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___4 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___4,
                                                            levels = c(0, 1),
                                                            labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___5 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___5,
                                                            levels = c(0, 1),
                                                            labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___6 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___6,
                                                            levels = c(0, 1),
                                                            labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___7 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___7,
                                                            levels = c(0, 1),
                                                            labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___8 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___8,
                                                            levels = c(0, 1),
                                                            labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___9 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___9,
                                                            levels = c(0, 1),
                                                            labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___10 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___10,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___11 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___11,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___12 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___12,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___13 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___13,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___14 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___14,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___15 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___15,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_childvaccine___16 <- factor(ImpactsOfCOVID19_clean$nonotsure_childvaccine___16,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean


# Dataset's structure
str(ImpactsOfCOVID19_clean)

# basic descriptive statistics
summary(ImpactsOfCOVID19_clean)



#################### Reasons for COVID-19 vaccine hesitancy for mom ####################

# Clean the Data 

#library(tidyverse)

# read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

# select necessary columns and factor variables
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(nonotsure_mothervaccine___1, nonotsure_mothervaccine___2, nonotsure_mothervaccine___3,
         nonotsure_mothervaccine___4, nonotsure_mothervaccine___5, nonotsure_mothervaccine___6,
         nonotsure_mothervaccine___7, nonotsure_mothervaccine___8, nonotsure_mothervaccine___9,
         nonotsure_mothervaccine___10, nonotsure_mothervaccine___11, nonotsure_mothervaccine___12,
         nonotsure_mothervaccine___13, nonotsure_mothervaccine___14, nonotsure_mothervaccine___15,
         nonotsure_mothervaccine___16, nonotsure_mothervaccine___17, nonotsure_mothervaccine___18
  ) %>%
  mutate(nonotsure_mothervaccine___1 = as.factor(nonotsure_mothervaccine___1),
         nonotsure_mothervaccine___2 = as.factor(nonotsure_mothervaccine___2),
         nonotsure_mothervaccine___3 = as.factor(nonotsure_mothervaccine___3),
         nonotsure_mothervaccine___4 = as.factor(nonotsure_mothervaccine___4),
         nonotsure_mothervaccine___5 = as.factor(nonotsure_mothervaccine___5),
         nonotsure_mothervaccine___6 = as.factor(nonotsure_mothervaccine___6),
         nonotsure_mothervaccine___7 = as.factor(nonotsure_mothervaccine___7),
         nonotsure_mothervaccine___8 = as.factor(nonotsure_mothervaccine___8),
         nonotsure_mothervaccine___9 = as.factor(nonotsure_mothervaccine___9),
         nonotsure_mothervaccine___10 = as.factor(nonotsure_mothervaccine___10),
         nonotsure_mothervaccine___11 = as.factor(nonotsure_mothervaccine___11),
         nonotsure_mothervaccine___12 = as.factor(nonotsure_mothervaccine___12),
         nonotsure_mothervaccine___13 = as.factor(nonotsure_mothervaccine___13),
         nonotsure_mothervaccine___14 = as.factor(nonotsure_mothervaccine___14),
         nonotsure_mothervaccine___15 = as.factor(nonotsure_mothervaccine___15),
         nonotsure_mothervaccine___16 = as.factor(nonotsure_mothervaccine___16),
         nonotsure_mothervaccine___17 = as.factor(nonotsure_mothervaccine___17),
         nonotsure_mothervaccine___18 = as.factor(nonotsure_mothervaccine___18))

# remove rows with any NA
ImpactsOfCOVID19_clean <- na.omit(ImpactsOfCOVID19_Ranalysis)

# Dataset's structure
str(ImpactsOfCOVID19_clean)

#Apply labels column by column

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___1 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___1,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___2 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___2,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___3 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___3,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___4 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___4,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___5 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___5,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___6 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___6,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___7 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___7,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___8 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___8,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___9 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___9,
                                                             levels = c(0, 1),
                                                             labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___10 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___10,
                                                              levels = c(0, 1),
                                                              labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___11 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___11,
                                                              levels = c(0, 1),
                                                              labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___12 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___12,
                                                              levels = c(0, 1),
                                                              labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___13 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___13,
                                                              levels = c(0, 1),
                                                              labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___14 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___14,
                                                              levels = c(0, 1),
                                                              labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___15 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___15,
                                                              levels = c(0, 1),
                                                              labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___16 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___16,
                                                              levels = c(0, 1),
                                                              labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___17 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___17,
                                                              levels = c(0, 1),
                                                              labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean$nonotsure_mothervaccine___18 <- factor(ImpactsOfCOVID19_clean$nonotsure_mothervaccine___18,
                                                              levels = c(0, 1),
                                                              labels = c("Not selected", "Selected"))

ImpactsOfCOVID19_clean


# Dataset's structure
str(ImpactsOfCOVID19_clean)

# basic descriptive statistics
summary(ImpactsOfCOVID19_clean)