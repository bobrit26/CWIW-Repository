#You might not need all of the packages
#I just copied what I had already used before
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(plm)
library(ggplot2)
library(stargazer)
library(modelsummary)
library(pandoc)

suicides_age_df <- read.csv('regression_suicides_age_spending_revenue.csv')
suicides_sex_df <- read.csv('regression_suicides_sex_spending_revenue.csv')

#I will deselect the raw absolute numbers for viewing convenience - this is unnecessary
suicides_age_df <- suicides_age_df %>%
  select(state, year, five_year_age_groups, suicides_per_100k, spending_per_100k, revenue_per_100k)
suicides_sex_df <- suicides_sex_df %>%
  select(state, year, sex, suicides_per_100k, spending_per_100k, revenue_per_100k)

#Okay, spending and revenue in individual dollars per 100K is way too low-scale
#Let's get them into 100K USD per 100K individuals
suicides_age_df <- suicides_age_df %>%
  mutate(spending_100k_per_100k = spending_per_100k/100000,
         revenue_100k_per_100k = revenue_per_100k/100000)

suicides_sex_df <- suicides_sex_df %>%
  mutate(spending_100k_per_100k = spending_per_100k/100000,
         revenue_100k_per_100k = revenue_per_100k/100000)


#A simple correlation
cor.test(suicides_age_df$suicides_per_100k,
         suicides_age_df$spending_per_100k)
cor.test(suicides_sex_df$suicides_per_100k,
         suicides_sex_df$spending_per_100k)

#correlations by subgroup
suicides_age_df %>%
  group_by(five_year_age_groups) %>%
  summarise(cor = cor.test(suicides_per_100k, spending_per_100k)$estimate,
    p.value = cor.test(suicides_per_100k, spending_per_100k)$p.value)
suicides_sex_df %>%
  group_by(sex) %>%
  summarise(cor = cor.test(suicides_per_100k, spending_per_100k)$estimate,
            p.value = cor.test(suicides_per_100k, spending_per_100k)$p.value)

#Now for the regression
#plm seems appropriate for our 20 years and 50 states panel data
#We want to run a linear regression with fixed effects
#First - per age group
age_model <- plm(suicides_per_100k ~ spending_100k_per_100k*five_year_age_groups + revenue_100k_per_100k, #DV ~ IV1 + control
    #The suicides per 100K are already paired with and calculated for specific age/sex
    data = suicides_age_df,
    model = 'within', #we want fixed effects
    effect = 'twoways', #we want them both for state specifics 
                        #and for year shocks (e.g. 2008)
    index = c('state','year')) #our identifiers, i.e. our state-years

#Then - per gender
sex_model <- plm(suicides_per_100k ~ spending_100k_per_100k*sex + revenue_100k_per_100k,
    data = suicides_sex_df, 
    model = 'within', 
    effect = 'twoways',
    index = c('state','year'))



#And let's also get some visualisation
#General scatterplot with age group-suicides cases
ggplot(suicides_age_df, aes(x = spending_100k_per_100k, y = suicides_per_100k)) +
  geom_point(alpha = 0.3) + #the blobs are overlapping a lot
  geom_smooth(method = 'lm', se = TRUE, colour = 'orange') + #yeah, I know it looks ugly, but it's the best I could come up with
  theme_minimal() +
  labs(title = 'Suicide Rates (Age) ~~ Student Support Services',
       x = 'Student Support Services Spending 100K USD per 100k',
       y = 'Suicide Rates (Age) per 100K')

#and with sex group-suicides cases
ggplot(suicides_sex_df, aes(x = spending_100k_per_100k, y = suicides_per_100k)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', se = TRUE, colour = 'orange') +
  theme_minimal() +
  labs(title = 'Suicide Rates (Sex) ~~ Student Support Services',
       x = 'Student Support Services Spending 100K USD per 100k',
       y = 'Suicide Rates (Sex) per 100K')



#Scatterplots by actual group variation
ggplot(suicides_age_df, aes(x = spending_100k_per_100k, y = suicides_per_100k)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', se = TRUE, colour = 'orange') +
  theme_minimal() +
  facet_wrap(~five_year_age_groups) + #so that we have separate subplots
  labs(title = 'Suicide Rates (Age) by Subgroup ~~ Student Support Services',
       x = 'Student Support Services Spending 100K USD per 100k',
       y = 'Suicide Rates (Age) per 100K')


ggplot(suicides_sex_df, aes(x = spending_100k_per_100k, y = suicides_per_100k)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', se = TRUE, colour = 'orange') +
  theme_minimal() +
  facet_wrap(~sex) +
  labs(title = 'Suicide Rates (Sex) by Subgroup ~~ Student Support Services',
       x = 'Student Support Services Spending 100K USD per 100k',
       y = 'Suicide Rates (Sex) per 100K')

#stargazer
stargazer(age_model, type = 'text',
          title = 'Suicide-Age-Spending Regression',
          dep.var.labels = 'Suicides (Age) per 100K',
          covariate.labels = c('Student Spending per 100K (100K USD)', 'Revenue per 100K (100K USD)'))

stargazer(sex_model, type = 'text',
          title = 'Suicide-Sex-Spending Regression',
          dep.var.labels = 'Suicides (Sex) per 100K',
          covariate.labels = c('Student Spending per 100K (100K USD)', 'Revenue per 100K (100K USD)'))

#It turns out you can't insert htmls into.docx, so I tried another package

#modelsummary
#this will also be on GitHub, so remaking this is optional
  #modelsummary(list('Suicide-Age-Spending' = age_model, 'Suicide-Sex-Spending' = sex_model),
  #             output = 'regression_results.docx')