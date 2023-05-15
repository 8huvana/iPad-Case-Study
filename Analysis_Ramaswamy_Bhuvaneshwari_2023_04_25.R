library("tidyverse")

data <- read_csv("schools.csv")
str(data)
# removing columns irrelevant to this project
data <- data[,1:8]

#removing schools without test data

data <- na.omit(data)

mean_2017 <- data %>% 
  filter(`Test Name` == "ELA/Literacy Grade 5") %>% 
  group_by(Participation) %>%  
  summarise(mean_2017 = mean(`Overall ELA Mean Scale Score`))

diff_perc_2017 <- (mean_2017[2,2]/mean_2017[1,2]) - 1

#prepare table for 2016
table_2016 <- data %>% 
  filter(`Test Name` == "ELA/Literacy Grade 5", Year == "2016", Participation == "Pilot") 

#prepare composite table wirh 2017 data
table_201617 <- data %>% 
  filter(`Test Name` == "ELA/Literacy Grade 5", Year == "2017", Participation == "Pilot")

#rename mean score
names(table_201617)[8] ="Mean2017"

#append 2016 scores to composite table
table_201617$Mean2016 <- table_2016$`Overall ELA Mean Scale Score` 

table_201617$Growth <- table_201617$Mean2017 - table_201617$Mean2016

#school with highest growth
table_201617 %>% slice_max(Growth)

#sorting
table_201617 <- arrange(table_201617, desc(Growth))

table_201617$GrowthPerc <- (table_201617$Mean2017 - table_201617$Mean2016)/table_201617$Mean2016

#all schools except top growth school
nontop_201617 <- table_201617[2:5,]

#mean percentage growth for all other schools
percnontop <- nontop_201617 %>% summarise(mean(GrowthPerc))

perctop <- table_201617[1,11]

perctop - percnontop
#A way to assign participation types in R
#table_20161test <- transform(data, type = ifelse(`School ID` %in% c(610066, 610082, 610182, 610534, 609880), "Pilot", "Non-Pilot"))


##free or reduced lunch analysis


lunchdata <- read_csv("lunch.csv")

#list of pilot school IDs
pilots <- c(610066, 610082, 610182, 610534, 609880)

#indicate pilot/non-pilot in separate column

lunchdata <- transform(lunchdata, type = ifelse(SchoolID %in% pilots, "Pilot", "Non-Pilot"))

pilot_subslunch <- lunchdata %>% 
  filter(Year == 2017, type == "Pilot") %>% 
  summarise(perc = sum(SubLunch)/sum(Total))
