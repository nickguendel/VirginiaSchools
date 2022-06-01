#  This program will import, clean, and identify correlations factors with 
#  drop rates in Virginia for their most recent data available. 
#  Helpful research on factors that correlate with graduation rates

#  1. Set up environment
library(tidyverse)
library(ggplot2)
library(dplyr)
library(reshape2)

#  2. Import Data and Select Data
# Schools Details. I've chosen to select the most recent year prior to COVID Scores
teacher_experience <- read.csv("Teacher Quality.csv", header = TRUE) %>% 
  select(-Level, -Year, -Poverty.Level) %>%  # all data from 2020-2021, I'm looking for the 18-19 data to match
  filter(Title1.Code != "All Schools")
location <- read.csv("SchoolLocationData_Cleaned.csv", header = TRUE) %>%
  mutate(across(where(is.character), str_trim)) %>% # Some observations have spaces in them
  select(-X, -X.1, -X.2, -X.3)
finances <- read.csv("finances.csv", header = TRUE) %>% 
  filter(Year == "2018 - 2019")
teacher_attainment <- read.csv("EducationAttainment1819.csv", header = TRUE)
teacher_provisional_licensed <- read.csv("TeacherProvisionalLicensed.csv", header = TRUE)

# Subgroup details for each school. I've chosen to select the most recent year prior to COVID Scores
graduation_rate <- read.csv("On-Time Graduation Rate.csv", header = TRUE) %>% 
  filter(Year == "2019")
absenteesim <- read.csv("Chronic Absenteeism.csv", header = TRUE) %>% 
  filter(Year == "2018 - 2019")
assessment <- read.csv("Assessment1617to1819.csv", header = TRUE, na.strings = c("","NA","<"))%>% 
  select(-X2016.2017.Pass.Rate, -X2017.2018.Pass.Rate, -Level)

#  3. Joins and Cleaning data 

# reshape from long to wide. The focus is to widen all data around School names
assessment_wide_subject <- assessment %>% 
  pivot_wider(names_from = Subject, values_from = X2018.2019.Pass.Rate)
assessment_wide_subgroup <- assessment %>% 
  pivot_wider(names_from = Subgroup, values_from = X2018.2019.Pass.Rate)

assessment_wide <- assessment %>% 
  pivot_wider(names_from = c(Subject, Subgroup), values_from = X2018.2019.Pass.Rate) %>% 
  select(-Primary, -Division....School, -Div.Num, -Sch.Num) %>% 
  add_column(to_be_Concat_Col="Public Schools", .after = "Div.Name") %>% 
  unite(Division, c("Div.Name", "to_be_Concat_Col"), sep = "")


teacher_attainment_wide <- teacher_attainment %>% 
  pivot_wider(names_from = Type, values_from = Year.Percent)


# Joins school data on teacher quality, finances, and location information
schools_all <- location %>% 
  full_join(teacher_experience, by=c("Division.Schools"="Division", 
                                     "School.Name"="School")) %>% 
  full_join(finances, by=c("School.Name" = "School", 
                           "Division.Schools" = "Division")) %>% 
  full_join(teacher_attainment_wide, by=c("School.Name" = "School", 
                                          "Division.Schools" = "Division")) %>% 
  full_join(teacher_provisional_licensed, by=c("School.Name" = "School", 
                                               "Division.Schools" = "Division"))%>% 
  full_join(assessment_wide, by=c("School.Name" = "Sch.Name", 
                                               "Division.Schools" = "Division"))



#  4. Find Correlations between variables and Dropout
