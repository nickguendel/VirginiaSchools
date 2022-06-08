#  This program will import, clean, and identify correlations factors with 
#  drop rates in Virginia for their most recent data available. 
#  Helpful research on factors that correlate with graduation rates

#  1. Set up environment
library(tidyverse)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)

#  2. Import Data and Select Data
## Schools Details. I've chosen to select the most recent year prior to COVID Scores
teacher_experience <- read.csv("Teacher Quality.csv", header = TRUE) %>% 
  select(Division, School, Title1.Code,Percent.of.Inexperienced.Teachers, 
         Percent.of.Out.of.Field.Teachers) %>%  # all data from 2020-2021, for the 18-19 data to match
  filter(Title1.Code != "All Schools")
location <- read.csv("SchoolLocationData_Cleaned.csv", header = TRUE) %>%
  mutate(across(where(is.character), str_trim)) %>% # Some observations have spaces in them
  select(-X, -X.1, -X.2, -X.3)
finances <- read.csv("finances.csv", header = TRUE) %>% 
  select(Year, Division, School, Total.Per.Pupil.Expenditures) %>% 
  filter(Year == "2018 - 2019")
teacher_attainment <- read.csv("EducationAttainment1819.csv", header = TRUE)
teacher_provisional_licensed <- read.csv("TeacherProvisionalLicensed.csv", header = TRUE) %>% 
  select(Division, School, Provisional.Percent)

## Subgroup details for each school. I've chosen to select the most recent year prior to COVID Scores
graduation_rate <- read.csv("On-Time Graduation Rate.csv", header = TRUE) %>% 
  filter(Year == "2019") %>% 
  select(Division, Subgroup, School, Virginia.On.Time.Graduation.Rate, Completion.Rate)
absenteesim <- read.csv("Chronic Absenteeism.csv", header = TRUE) %>% 
  filter(Year == "2018 - 2019") %>% 
  select(Division, School, Subgroup, Percent.Below.10, Percent.above.10)
assessment <- read.csv("Assessment1617to1819.csv", header = TRUE, na.strings = c("","NA","<"))%>% 
  select(Div.Name, Sch.Name, Subject, Subgroup, X2018.2019.Pass.Rate)

#  3. Joins and Cleaning data 

## reshape from long to wide. The focus is to widen all data around School names
assessment_wide_subject <- assessment %>% 
  pivot_wider(names_from = Subject, values_from = X2018.2019.Pass.Rate)
assessment_wide_subgroup <- assessment %>% 
  pivot_wider(names_from = Subgroup, values_from = X2018.2019.Pass.Rate)

## Widen Assessments 
assessment_wide <- assessment %>% 
  pivot_wider(names_from = c(Subject, Subgroup), values_from = X2018.2019.Pass.Rate) %>% 
  add_column(to_be_Concat_Col="Public Schools", .after = "Div.Name") %>% 
  unite(Division, c("Div.Name", "to_be_Concat_Col"), sep = "")
### Widen Assessment Data For Math
assessment_math <- filter(assessment, assessment$Subject == "Mathematics")
assessment_wide_math <- assessment %>% 
  filter(assessment$Subject == "Mathematics") %>% 
  pivot_wider(names_from = c(Subject, Subgroup), values_from = X2018.2019.Pass.Rate) %>% 
  add_column(to_be_Concat_Col="Public Schools", .after = "Div.Name") %>% 
  unite(Division, c("Div.Name", "to_be_Concat_Col"), sep = "")

## Widen Teacher attainment 
teacher_attainment_wide <- teacher_attainment %>% 
  pivot_wider(names_from = Type, values_from = Year.Percent)
## Widen Graduation Rate 
graduation_rate_wide <- graduation_rate %>% 
  pivot_wider(names_from = Subgroup, values_from = c(Virginia.On.Time.Graduation.Rate, 
                                                     Completion.Rate))

# Joins school data to one data frame
schools_all <- location %>% 
  full_join(teacher_experience, by=c("Division.Schools"="Division", # Teacher experience
                                     "School.Name"="School")) %>% 
  full_join(finances, by=c("School.Name" = "School", # Finances
                           "Division.Schools" = "Division")) %>% 
  full_join(teacher_attainment_wide, by=c("School.Name" = "School", # Teacher education attainment 
                                          "Division.Schools" = "Division")) %>% 
  full_join(teacher_provisional_licensed, by=c("School.Name" = "School", # 
                                               "Division.Schools" = "Division"))%>% 
  full_join(assessment_wide, by=c("School.Name" = "Sch.Name", 
                                               "Division.Schools" = "Division")) %>% 
  full_join(graduation_rate_wide, by=c("School.Name" = "School", 
                                  "Division.Schools" = "Division"))
# Writes joined data to a csv fild
write.csv(schools_all,"schools_all.csv", row.names = FALSE)

# builds data frame for just High schools
schools_all_HS <- location %>% 
  full_join(teacher_experience, by=c("Division.Schools"="Division", # Teacher experience
                                     "School.Name"="School")) %>% 
  full_join(finances, by=c("School.Name" = "School", # Finances
                           "Division.Schools" = "Division")) %>% 
  full_join(teacher_attainment_wide, by=c("School.Name" = "School", # Teacher education attainment 
                                          "Division.Schools" = "Division")) %>% 
  full_join(teacher_provisional_licensed, by=c("School.Name" = "School", # 
                                               "Division.Schools" = "Division"))%>% 
  full_join(assessment_wide, by=c("School.Name" = "Sch.Name", 
                                  "Division.Schools" = "Division")) %>% 
  right_join(graduation_rate_wide, by=c("School.Name" = "School", 
                                       "Division.Schools" = "Division"))
# Writes joined data to a csv fild
write.csv(schools_all_HS,"schools_all_HS.csv", row.names = FALSE)

### Builds Data Frame For Just Math
schools_Math <- location %>% 
  full_join(teacher_experience, by=c("Division.Schools"="Division", # Teacher experience
                                     "School.Name"="School")) %>% 
  full_join(finances, by=c("School.Name" = "School", # Finances
                           "Division.Schools" = "Division")) %>% 
  full_join(teacher_attainment_wide, by=c("School.Name" = "School", # Teacher education attainment 
                                          "Division.Schools" = "Division")) %>% 
  full_join(teacher_provisional_licensed, by=c("School.Name" = "School", # 
                                               "Division.Schools" = "Division"))%>% 
  full_join(assessment_wide_math, by=c("School.Name" = "Sch.Name", 
                                  "Division.Schools" = "Division")) %>% 
  full_join(graduation_rate_wide, by=c("School.Name" = "School", 
                                       "Division.Schools" = "Division"))

#  4. Find Correlations between variables and Dropout

## Filter non-numeric data for correlation function
## finds correlations between all variables
cor_matrix_select_num <- schools_all %>% 
  select(-School.Name, -Principal, -Grades, -Division.Schools, -Street.Adress.and.Phone.Number,
         -Phone.Number, -City, -Zip, -State, -Title1.Code, -Year)

cor_matrix_all_num <- round(cor(cor_matrix_select_num, use = "pairwise.complete.obs"), digits = 2)

cor_matrix_all_num_analysis <-  as.data.frame(cor_matrix_all_num) %>%  
  select(Percent.of.Inexperienced.Teachers, Percent.of.Out.of.Field.Teachers, 
         Total.Per.Pupil.Expenditures, `Bachelor's Degree`, `Master's Degree`, 
         `Doctoral Degree`, Provisional.Percent, `Virginia.On.Time.Graduation.Rate_All Students`)

corrplot(cor_matrix_all_num_analysis, type = "upper", method = "square", tl.pos = 'n')

## Finds Correlations for Math Variables
cor_matrix_select_num_math <- schools_Math %>% 
  select(-School.Name, -Principal, -Grades, -Division.Schools, -Street.Adress.and.Phone.Number,
         -Phone.Number, -City, -Zip, -State, -Title1.Code, -Year, 
         -`Virginia.On.Time.Graduation.Rate_Foster Care`, -`Completion.Rate_Foster Care`)
cor_matrix_all_num_math <- round(cor(cor_matrix_select_num_math, 
                                     use = "pairwise.complete.obs"), digits = 2)
corrplot(cor_matrix_all_num_math, type = "upper", method = "circle", tl.pos = 'n')

view(cor_matrix_HS)
