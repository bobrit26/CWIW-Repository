#The following are the steps taken to clean the raw data
#You do not need to do this again - it remains here for transparency ദ്ദി´ ˘ `)✧
#See script 2 for the actual analysis

library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)
library(stringr)

suicides_age <- read_csv("CDC_Suicide_Rates_I_10-39yy_All_States_1999_2019_X60-X84_Y870.csv")
suicides_sex <- read_csv("CDC_Suicide_Rates_II_Sex,_All_States_1999_2019_X60-X84_Y870.csv")


#getting rid of columns for notes, duplicate year codes and duplicate age codes
suicides_age_clean <- suicides_age[, -c(1,3,5,7)]

#clean names for convenience
suicides_age_clean <- suicides_age_clean %>% clean_names()

#very small numbers of suicides per state-year-age group are hidden by the CDC
#we want to mark them as NA
suicides_age_clean$deaths[suicides_age_clean$deaths == 'Suppressed'] <- NA  
suicides_age_clean$crude_rate[suicides_age_clean$crude_rate == 'Suppressed'] <- NA 

#small N of suicides/population >>> hidden crude rate due to CDC standards
#but we still want to have it for the analysis
suicides_age_clean$crude_rate[suicides_age_clean$crude_rate == 'Unreliable'] <- NA
suicides_age_clean$crude_rate[suicides_age_clean$crude_rate == 'Suppressed'] <- NA 

#Deaths and Crude Rate are chr >>> we want num
suicides_age_clean$deaths <- as.numeric(suicides_age_clean$deaths)
suicides_age_clean$crude_rate <- as.numeric(suicides_age_clean$crude_rate)


#then we do the above for the state-year-sex suicide sheet
suicides_sex_clean <- suicides_sex[, -c(1,3,5,7)] 

suicides_sex_clean <- suicides_sex_clean %>% clean_names()

#let's get rid of the suppressed data
suicides_sex_clean$deaths[suicides_sex_clean$deaths == 'Suppressed'] <- NA 
suicides_sex_clean$crude_rate[suicides_sex_clean$crude_rate == 'Unreliable'] <- NA
suicides_sex_clean$crude_rate[suicides_sex_clean$crude_rate == 'Suppressed'] <- NA

suicides_sex_clean$deaths <- as.numeric(suicides_sex_clean$deaths)
suicides_sex_clean$crude_rate <- as.numeric(suicides_sex_clean$crude_rate)

#let's recalculate the crude rate where it was omitted by the CDC
#this will mean non-ideal standards for the data in low-number cases
#but for the purposes of the seminar paper it should be fine

#first for age
suicides_age_clean <- suicides_age_clean %>%
  mutate(
    crude_rate = ifelse(
      is.na(crude_rate) & !is.na(deaths) & !is.na(population),
      (deaths / population) * 100000,
      crude_rate
    ),
    crude_rate = round(crude_rate, 1)
  )

#then for sex

suicides_sex_clean <- suicides_sex_clean %>%
  mutate(
    crude_rate = ifelse(
      is.na(crude_rate) & !is.na(deaths) & !is.na(population),
      (deaths / population) * 100000,
      crude_rate
    ),
    crude_rate = round(crude_rate, 1)
  )

#renaming for future similarity
suicides_age_clean <- suicides_age_clean %>%
  rename(
    suicides_per_100k = crude_rate
  )

suicides_sex_clean <- suicides_sex_clean %>%
  rename(
    suicides_per_100k = crude_rate
  )

#CSVs are on already on GitHub - this is just for replicability
#just so you can check that I didn't make up the numbers
  #saving clean CSVs as fresh copies to maintain the originals
  #write.csv(suicides_age_clean, 'suicides_age_clean.csv')
  #write.csv(suicides_sex_clean, 'suicides_sex_clean.csv')


#Now we need to clean and format student support spending
#fixing the format of student spending to make it fit the suicide data
student_spending <- read_csv ("NCES_Student_Support_Services_Subtotal_(STE22)_[State_Finance]_1998-2020.csv")

student_spending_clean <- student_spending %>% clean_names()

#obsolete

  #something messes up pivot_longer() and slicing didn't work >>> we need to clean
     #student_spending_clean <- student_spending_clean %>%
  #turn everything into strings
     #mutate(across(-state_name, as.character)) %>%
  #get rid of the semicolon troublemaker
     #mutate(across(-state_name, ~ gsub(';', '',.))) %>%
  #turn them back into numbers back
      #mutate(across(-state_name, as.numeric)) %>%


#it took me too long to figure out that there was a semicolon
#at the end of every state spending for some reason
#thanks NCES (╯°□°）╯︵ ┻━┻
#if I fail the class because I am late with my seminar paper - this entire script is the reason why
#this, the general lack of a sense of responsibility
#I may be playing stupid games and winning stupid prizes
#but hey, at least writing code was fun

#let's also slice to get rid of the interfering descriptions that came with the CSV
student_spending_clean <- student_spending_clean %>% slice(1:50)

#obsolete - will do at the end
  #let's fix the formatting to match the suicide data and rename the columns
    #student_spending_clean <- student_spending_clean %>%
    #  pivot_longer(
    #    cols = -state_name,
    #    names_to = 'school_year',
    #    values_to = 'spending'
    #  )

#let's get rid of the semicolons and ensure the data is actually numeric

student_spending_clean <- student_spending_clean %>%
  mutate(student_support_services_subtotal_ste22_state_finance_1998_99 = as.numeric(gsub(';','', student_support_services_subtotal_ste22_state_finance_1999_00)))
student_spending_clean[,-1] <- lapply(student_spending_clean[,-1], as.numeric)

#make the state names fit the suicide data
student_spending_clean <- student_spending_clean %>%
  mutate(state_name = str_to_title(tolower(state_name)))

#we need normal column names, i.e. just '2018-19', etc.

student_spending_clean <- student_spending_clean %>%
  rename_with(
    ~ str_replace(str_extract(.x, "\\d{4}_\\d{2}$"), "_", "-"),
    starts_with("student_support_services_subtotal")
  )

#okay, now we need to turn school year spending into calendar year spending
#this roughly follows a 4/8 split, i.e. September-December / January-August
#not 100% accurate, since there is some variation in the US by state and school

#this got really confusing, so what we can do is to just write a function 
#separately here that we will apply later to turn school years into calendar years

school_to_calendar <- function(df) {
  years <- names(df)[-1]  
  #we grab the first 4 digits, so that 2019-2020 is 2019, etc.
  #same idea as above, hence \\d{4}, i.e. the first 4 digits
  years <- str_extract(years, "^\\d{4}") 
  years <- as.integer(years)
  
  #this is our year sequence
  cal_years <- (min(years)):(max(years))
  #is this yet another dataframe that makes it all confusing?
  #yes
  #is it also easier to keep track of this by splitting the process into ten different steps?
  #also yes
  #this is technically unnecessary, but it works, so ¯\(ツ)_/¯
  result <- df %>% select(state_name)
  #so that we have the year framework to put in our modified spending
  for (y in cal_years) {
    result[[as.character(y)]] <- 0
  }
  
#no one will read this far (U•ᴥ•U)
  
  #okay, now we need our complicated 4/12-8/12 split to be reflected in the adjusted numbers
  for (i in seq_along(years)) {
    y1 <- years[i]
    y2 <- years[i] + 1
    colname <- names(df)[i+1]
    
    #so 2019 is 8/12 2018-2019
    result[[as.character(y1)]] <- result[[as.character(y1)]] + (8/12) * df[[colname]]
    #and 4/12 2019-2020
    if (as.character(y2) %in% names(result)) {
      result[[as.character(y2)]] <- result[[as.character(y2)]] + (4/12) * df[[colname]]
    }
  }
  #optional
  return(result)
}

#so now we put the function to use
student_spending_calendar <- school_to_calendar(student_spending_clean)

student_spending_calendar <- student_spending_calendar %>%
  #just in case, because we had 1998 and 2020 segments
  select(state_name, `1999`:`2019`)

#you could skip 'student_spending_calendar' step right away
#but I already had my share of dumb moments before, so this way you can take it slowly 
#in case you need to troubleshoot

#we need to turn our wide format into long format to fit the suicide data
student_spending_clean <- student_spending_calendar %>%
  pivot_longer(-state_name,
    names_to = "year",
    values_to = "spending"
  ) %>%
  #just in case
  mutate(year = as.integer(year))

#apparently the state column names are different for the spending and suicide CSVs
student_spending_clean <- student_spending_clean %>%
  rename(state = state_name)

#this is also already on GitHub - left here for replicability
  #saving clean CSVs as fresh copies to maintain the originals again
  #write.csv(student_spending_clean, 'student_spending_clean.csv')


#finally, let's fix our control data - state total tax
#fortunately, the US Census Bureau website let's you download filtered data
#so this is fairly self-explanatory

tax_revenue <- read.csv('US_Census_Bureau_State_Total_Tax_Collection_1999-2019.csv') %>%
  rename(
    year = 1,
    state = 2,
    revenue = 3
  )

#the tax revenue comes in a weird string, million USD format which is no bueno
#let's turn in into normal pure value
tax_revenue <- tax_revenue %>%
  mutate(revenue = as.numeric(gsub(',', '', revenue))*1000000)

#we also need to convert quarters into years
tax_revenue <- tax_revenue %>%
  #we're extracting the first 4 numbers, i.e. the year
  mutate(year = as.integer(str_extract(year, '^\\d{4}'))) %>%
  #and then just sum the quarters up
  group_by(state, year) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE), .groups = 'drop')


tax_revenue_clean <- tax_revenue
  #write.csv(tax_revenue_clean, 'tax_revenue_clean.csv')

#I also need to get actual total population for spending and revenue per 100k
#I'll just extract it from CDC again
total_pop <- read.csv('CDC_Suicide_Rates_Total_Population_All_States_1999_2019_X60-X84_Y870.csv')

total_pop <- total_pop %>% clean_names()

total_pop_clean <- total_pop %>% select(state, year, population)

total_pop_clean <- total_pop_clean %>% rename(total_population = population)

  #write.csv(total_pop_clean, 'total_pop_clean.csv')

#okay, let's get into the final stage of cleaning and grafting

#we want to merge the clean suicide data with the clean spending data
#first for suicide-age-spending-revenue
suicides_age_spending_revenue <- suicides_age_clean %>%
  left_join(student_spending_clean, by = c('state', 'year'))
suicides_age_spending_revenue <- suicides_age_spending_revenue %>%
  left_join(tax_revenue_clean, by = c('state', 'year'))
suicides_age_spending_revenue <- suicides_age_spending_revenue %>%
  left_join(total_pop_clean, by = c('state', 'year'))

str(suicides_age_spending_revenue)

#second for suicide-sex-spending-revenue
suicides_sex_spending_revenue <- suicides_sex_clean %>%
  left_join(student_spending_clean, by = c('state', 'year'))
suicides_sex_spending_revenue <- suicides_sex_spending_revenue %>%
  left_join(tax_revenue_clean, by = c('state', 'year'))
suicides_sex_spending_revenue <- suicides_sex_spending_revenue %>%
  left_join(total_pop_clean, by = c('state', 'year'))

#then we need spending and revenue per 100k
#first for age
suicides_age_spending_revenue <- suicides_age_spending_revenue %>%
  mutate(
    spending_per_100k = (spending / total_population) * 100000,
    revenue_per_100k = (revenue / total_population) * 100000
  )

#then for sex
suicides_sex_spending_revenue <- suicides_sex_spending_revenue %>%
  mutate(
    spending_per_100k = (spending / total_population) * 100000,
    revenue_per_100k = (revenue / total_population) * 100000
  )

#drop NAs for the scatterplots
suicides_age_spending_revenue <- suicides_age_spending_revenue %>%
  drop_na()

suicides_sex_spending_revenue <- suicides_sex_spending_revenue %>%
  drop_na()

  write.csv(suicides_age_spending_revenue, 'regression_suicides_age_spending_revenue.csv')
  write.csv(suicides_sex_spending_revenue, 'regression_suicides_sex_spending_revenue.csv')

#okay, I am WAY behind the schedule