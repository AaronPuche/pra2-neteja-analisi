library(dplyr)
library(ggplot2)
library(magrittr)

median <- read.csv("/Users/rogerpardellcarrera/Desktop/UOC/Tipologia i cicle de vida de les dades/Pràctica 2/Dataset/MedianHouseholdIncome2015.csv")
poverty <- read.csv("/Users/rogerpardellcarrera/Desktop/UOC/Tipologia i cicle de vida de les dades/Pràctica 2/Dataset/PercentagePeopleBelowPovertyLevel.csv")
high_school <- read.csv("/Users/rogerpardellcarrera/Desktop/UOC/Tipologia i cicle de vida de les dades/Pràctica 2/Dataset/PercentOver25CompletedHighSchool.csv")
police <- read.csv("/Users/rogerpardellcarrera/Desktop/UOC/Tipologia i cicle de vida de les dades/Pràctica 2/Dataset/PoliceKillingsUS.csv")
race <- read.csv("/Users/rogerpardellcarrera/Desktop/UOC/Tipologia i cicle de vida de les dades/Pràctica 2/Dataset/ShareRaceByCity.csv")

dim(median)
dim(poverty)
dim(high_school)
dim(police)
dim(race)

colnames(median)[1] <- "area_geografica"
colnames(poverty)[1] <- "area_geografica"
colnames(high_school)[1] <- "area_geografica"
colnames(race)[1] <- "area_geografica"

USAv1 <- merge(high_school, poverty, by.x=c("area_geografica", "City"), by.y=c("area_geografica", "City"))
USAv2 <- merge(USAv1, median, by.x=c("area_geografica", "City"), by.y=c("area_geografica", "City"))
USA <- merge(USAv2, race, by.x=c("area_geografica", "City"), by.y=c("area_geografica", "City"))

USA$City <- gsub(" CDP| city| town|\\.| ","", USA$City)
police$city <- gsub(" County| Parish|[^[:alnum:]]","",police$city)


df_clean <- merge(police, USA, by.x=c("state", "city"), by.y=c("area_geografica", "City"))

df_clean$id <- NULL
df_clean$city <- NULL
df_clean$state <- NULL

df_clean %<>% mutate(date=as.Date(date, format = "%d/%m/%y"))

rownames(df_clean) <- 1:nrow(df_clean)

df_clean[df_clean$Median.Income == "-",]$Median.Income <- "0"
df_clean[df_clean$Median.Income == "(X)",]$Median.Income <- "0"
df_clean$Median.Income <- as.numeric(df_clean$Median.Income)
mean_income <- mean(df_clean[df_clean$Median.Income > 0,]$Median.Income)
df_clean$Median.Income[df_clean$Median.Income == 0] <- mean_income

df_clean$manner_of_death <- as.factor(df_clean$manner_of_death)
df_clean$armed <- as.factor(df_clean$armed)
df_clean$gender <- as.factor(df_clean$gender)
df_clean$race <- as.factor(df_clean$race)
df_clean$threat_level <- as.factor(df_clean$threat_level)
df_clean$flee <- as.factor(df_clean$flee)
df_clean$percent_completed_hs <- as.numeric(df_clean$percent_completed_hs)
df_clean$poverty_rate <- as.numeric(df_clean$poverty_rate)
df_clean$share_white <- as.numeric(df_clean$share_white)
df_clean$share_asian <- as.numeric(df_clean$share_asian)
df_clean$share_black <- as.numeric(df_clean$share_black)
df_clean$share_native_american <- as.numeric(df_clean$share_native_american)
df_clean$share_hispanic <- as.numeric(df_clean$share_hispanic)

boxplot(df_clean$Median.Income)

ggplot(df_clean) + geom_density(map = aes((x = Median.Income)))
ggplot(df_clean) + geom_density(map = aes((x = age)))

ggplot(df_clean) + geom_point(map = aes(x = share_white, y = Median.Income))

ggplot(df_clean) + geom_bar(map = aes(gender))
ggplot(df_clean) + geom_bar(map = aes(race))

ggplot(df_clean) + geom_histogram(map = aes(age, fill = race), position = "fill")
ggplot(df_clean) + geom_histogram(map = aes(age, fill = threat_level), position = "fill")
ggplot(df_clean) + geom_histogram(map = aes(age, fill = manner_of_death), position = "fill")

