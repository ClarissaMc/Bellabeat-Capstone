# Loading libraries
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)


# Uploading original data sets
activity <- read.csv("FitBit Fitness Tracker Data/dailyActivity_merged.csv")
calories <- read.csv("FitBit Fitness Tracker Data/dailyCalories_merged.csv")
intensities <- read.csv("FitBit Fitness Tracker Data/dailyIntensities_merged.csv")
steps <- read.csv("FitBit Fitness Tracker Data/dailySteps_merged.csv")
heartrate <- read.csv("FitBit Fitness Tracker Data/heartrate_seconds_merged.csv")
sleep <- read.csv("FitBit Fitness Tracker Data/sleepDay_merged.csv")
weight <- read.csv("FitBit Fitness Tracker Data/weightLogInfo_merged.csv")

device_survey <- read.csv("Fitness Consumer Survey Data/survey 605.csv")
exercise_survey <- read.csv("Fitness Analysis Data/fitness analysis.csv")


# Identifying null values in FitBit Tracker data
sum(is.na(activity))
sum(is.na(calories))
sum(is.na(device_survey))
sum(is.na(exercise_survey))
sum(is.na(heartrate))
sum(is.na(intensities))
sum(is.na(sleep))
sum(is.na(steps))
sum(is.na(weight))

# Checking what columns in weight have nulls
names(weight)
sum(is.na(weight$Id))
sum(is.na(weight$Date))
sum(is.na(weight$WeightKg))
sum(is.na(weight$WeightPounds))
sum(is.na(weight$Fat))

# Fat has all 65 null values
# This column is not vital to my analysis, so I will remove it.
weight_original <- weight
weight <- subset(weight, select = -c(Fat))
names(weight)

# Identifying null values in device and exercise data
sum(is.na(device_survey))
sum(is.na(exercise_survey))

# Identifying and removing duplicate entries in sleep data
View(sleep)
dupe <- sleep[,c('Id','SleepDay','TotalSleepRecords','TotalMinutesAsleep','TotalTimeInBed')]
sleep_duplicates <- sleep[duplicated(dupe) | duplicated(dupe, fromLast = TRUE),]
View(sleep_duplicates)
sleep <- sleep %>% distinct()

# Confirming no duplicates remain
sleep_duplicates <- sleep[duplicated(dupe),]
nrow(sleep[duplicated(sleep),])

# Identifying duplicate entries in fitness analysis data
View(exercise_survey)
exercise_original <- exercise_survey
dupe <- exercise_survey[,c('Your.name.')]
exercise_duplicates <- exercise_survey[duplicated(dupe) | duplicated(dupe, fromLast = TRUE),]
View(exercise_duplicates)

# Removing entries from same person
dupe_indices <- c(147, 267, 282, 330, 348, 389, 442, 472, 491, 505, 538, 539, 540)
exercise_survey <- exercise_survey %>% filter(!row_number() %in% dupe_indices)
View(exercise_survey)

# Confirming no duplicates remain
# Some duplicate names still remain, since only duplicate people were removed
nrow(exercise_duplicates)
length(dupe_indices)
expected_rows <- nrow(exercise_original) - length(dupe_indices) # expected number of remaining rows
actual_rows <- nrow(exercise_survey) # actual number of remaining rows
expected_rows
actual_rows


# Removing unnecessary columns
activity <- subset(activity, select = -c(LoggedActivitiesDistance,SedentaryActiveDistance))
intensities <- subset(intensities, select = -c(SedentaryActiveDistance))
sleep <- subset(sleep, select = -c(TotalSleepRecords))
colnames(exercise_survey)
exercise_survey <- exercise_survey[-c(10,11,13,14)]
exercise_survey <- exercise_survey[-c(12)]


# Renaming columns in exercise_survey
exercise_survey <- exercise_survey %>% 
  rename(
    Name = Your.name.,
    Gender = Your.gender.,
    Age = Your.age.,
    ExerciseImportance = How.important.is.exercise.to.you..,
    FitnessLevel = How.do.you.describe.your.current.level.of.fitness..,
    ExerciseFrequency = How.often.do.you.exercise.,
    ExerciseBarriers = What.barriers..if.any..prevent.you.from.exercising.more.regularly.............Please.select.all.that.apply.,
    ExerciseForms = What.form.s..of.exercise.do.you.currently.participate.in...........................Please.select.all.that.apply.,
    ExerciseLength = How.long.do.you.spend.exercising.per.day..,
    HealthLevel = How.healthy.do.you.consider.yourself.,
    EquipmentPurchase = Have.you.ever.purchased.a.fitness.equipment.,
    ExerciseMotivation = What.motivates.you.to.exercise...........Please.select.all.that.applies..
  )

# Checking for outlier numeric values
View(activity)
min(activity$TotalSteps)
max(activity$TotalSteps)
min(activity$TotalDistance)
max(activity$TotalDistance)
min(activity$TrackerDistance)
max(activity$TrackerDistance)
min(activity$VeryActiveDistance)
max(activity$VeryActiveDistance)
min(activity$ModeratelyActiveDistance)
max(activity$ModeratelyActiveDistance)
min(activity$LightActiveDistance)
max(activity$LightActiveDistance)
min(activity$VeryActiveMinutes)
max(activity$VeryActiveMinutes)
min(activity$FairlyActiveMinutes)
max(activity$FairlyActiveMinutes)
min(activity$LightlyActiveMinutes)
max(activity$LightlyActiveMinutes)
min(activity$SedentaryMinutes)
max(activity$SedentaryMinutes)
min(activity$Calories)
max(activity$Calories)

View(calories)
min(calories$Calories)
max(calories$Calories)

View(exercise_survey)
min(exercise_survey$ExerciseImportance)
max(exercise_survey$ExerciseImportance)
min(exercise_survey$HealthLevel)
max(exercise_survey$HealthLevel)

View(heartrate)
min(heartrate$Value)
max(heartrate$Value)

View(intensities)
min(intensities$SedentaryMinutes)
max(intensities$SedentaryMinutes)
min(intensities$LightlyActiveMinutes)
max(intensities$LightlyActiveMinutes)
min(intensities$FairlyActiveMinutes)
max(intensities$FairlyActiveMinutes)
min(intensities$VeryActiveMinutes)
max(intensities$VeryActiveMinutes)
min(intensities$LightActiveDistance)
max(intensities$LightActiveDistance)
min(intensities$ModeratelyActiveDistance)
max(intensities$ModeratelyActiveDistance)
min(intensities$VeryActiveDistance)
max(intensities$VeryActiveDistance)

View(sleep)
min(sleep$TotalMinutesAsleep)
max(sleep$TotalMinutesAsleep)
min(sleep$TotalTimeInBed)
max(sleep$TotalTimeInBed)

View(steps)
min(steps$StepTotal)
max(steps$StepTotal)

View(weight)
min(weight$WeightKg)
max(weight$WeightKg)
min(weight$WeightPounds)
max(weight$WeightPounds)
min(weight$BMI)
max(weight$BMI)


# Downloading cleaned data
write.csv(activity, "Cleaned Data/activity.csv", row.names = FALSE)
write.csv(calories, "Cleaned Data/calories.csv", row.names = FALSE)
write.csv(device_survey, "Cleaned Data/device_survey.csv", row.names = FALSE)
write.csv(exercise_survey, "Cleaned Data/exercise_survey.csv", row.names = FALSE)
write.csv(heartrate, "Cleaned Data/heartrate.csv", row.names = FALSE)
write.csv(intensities, "Cleaned Data/intensities.csv", row.names = FALSE)
write.csv(sleep, "Cleaned Data/sleep.csv", row.names = FALSE)
write.csv(steps, "Cleaned Data/steps.csv", row.names = FALSE)
write.csv(weight, "Cleaned Data/weight.csv", row.names = FALSE)
