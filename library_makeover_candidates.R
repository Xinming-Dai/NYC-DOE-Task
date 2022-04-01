# ----------------------
# Xinming Dai
# NYC DOE Data Task
# ----------------------

library(tidyverse)
require(readxl)
# load data
ela_all <- read_excel('school-ela-results-2013-2019-(public).xlsx', 
                         sheet = 'All', col_names = T)
school_demographic <- read_excel('demographic-snapshot-2016-17-to-2020-21---public.xlsx',
                                 sheet = 'School', col_names = T)

# delete the first row, which is useless
ela_all <- 
  ela_all %>% 
  select(-...1)

# replace 's' in score columns with NA
ela_all <- ela_all %>% 
  mutate(`Mean Scale Score` = replace(`Mean Scale Score`, `Mean Scale Score` == 's', NA),
         `# Level 1` = replace(`# Level 1`, `# Level 1` == 's', NA),
         `% Level 1` = replace(`% Level 1`, `% Level 1` == 's', NA),
         `# Level 2` = replace(`# Level 2`, `# Level 2` == 's', NA),
         `% Level 2` = replace(`% Level 2`, `% Level 2` == 's', NA),
         `# Level 3` = replace(`# Level 3`, `# Level 3` == 's', NA),
         `% Level 3` = replace(`% Level 3`, `% Level 3` == 's', NA),
         `# Level 4` = replace(`# Level 4`, `# Level 4` == 's', NA),
         `% Level 4` = replace(`% Level 4`, `% Level 4` == 's', NA),
         `% Level 3+4` = replace(`% Level 3+4`, `% Level 3+4` == 's', NA),
         `# Level 3+4` = replace(`# Level 3+4`, `# Level 3+4` == 's', NA))
# schools with complete score data
ela_all_complete <- ela_all %>% 
  filter(complete.cases(ela_all))

# schools with incomplete score data due to regulations of the Family Educational Rights and Privacy Act (FERPA) 
ela_all_not_complete <- ela_all %>% 
  filter(!complete.cases(ela_all))

# 30% or greater below the state average 
low_proficiency <- 52.3*(1-0.3)
# filter schools qualifying the first requirement
DBN_low_proficiency <- 
  ela_all_complete %>% 
  filter(Grade == 3,
         Year == 2019,
         grepl('K', DBN),
         !startsWith(DBN, '75'),
         `% Level 3+4` <= low_proficiency) %>% 
  distinct(DBN)

# filter schools qualifying the second requirement
# Serve a student population that is at least 65% low-income in the 2018-19 school year
school_low_incomes <- 
  school_demographic %>% 
  filter(grepl(paste(DBN_low_proficiency$DBN, collapse="|"), DBN),
         Year == '2018-19') %>% 
  mutate(`% Poverty` = replace(`% Poverty`, `% Poverty` == "Above 95%", 0.95)) %>% 
  filter(`% Poverty` >= 0.65)

potential_school_list <- 
  school_low_incomes %>% 
  select(DBN, `School Name`)

# save the list of potential schools
write.csv(potential_school_list, file = "potential_school_list.csv")

# 1. more explore ------------------------
# schools in DBN_low_proficiency and school_low_incomes are quiet similar. 
# there is just one school, 13K305, is not in DBN_low_proficiency, because school_demographic doesn't have 13K305's data
# descriptive information on this school could be gathered to determine whether to put it on the list
DBN_low_proficiency$DBN[!(DBN_low_proficiency$DBN %in% school_low_incomes$DBN)]

# 2. more explore ------------------------
district_onlist <- tibble(district = str_sub(potential_school_list$DBN, 1, 2))

count_district_onlist <- 
  district_onlist %>% 
  group_by(district) %>% 
  summarise(count_school = n()) %>% 
  add_row(district = '20', count_school = 0) %>% 
  mutate(district = as.numeric(district)) %>% 
  arrange(district)

district <- 
  ela_all %>% 
  filter(grepl('K', DBN)) %>% 
  distinct(DBN) %>% 
  mutate(district = str_sub(DBN, 1, 2)) %>% 
  group_by(district) %>% 
  summarise(count_school = n()) %>% 
  mutate(percentage_onlist = count_district_onlist$count_school/count_school)

p <- 
  ggplot(data = district_onlist, aes(district))+
  stat_count()+
  scale_y_continuous(breaks = seq(0, 12, by = 2))

ggsave(plot=p, file='district_bar.png', height = 5, width = 5)


