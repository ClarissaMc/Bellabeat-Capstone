# Loading libraries
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("splitstackshape")
install.packages("wesanderson")
library(tidyverse)
library(ggplot2)
library(splitstackshape)
library(data.table)
library(scales)
library(wesanderson)

# Checking how many users use each FitBit feature

# Counting unique ids for each feature
calorie_users <- length(unique(calories$Id))
heartrate_users <- length(unique(heartrate$Id))
intensity_users <- length(unique(intensities$Id))
sleep_users <- length(unique(sleep$Id))
step_users <- length(unique(steps$Id))
weight_users <- length(unique(weight$Id))

# Graphing results
features <- c("Calories", "Heartrate", "Intensity", "Sleep", "Steps", "Weight");
user_count <- c(calorie_users, heartrate_users, intensity_users, sleep_users, step_users, weight_users);
feature_popularity <- data.frame(features, user_count);
View(feature_popularity)

ggplot(data = feature_popularity, mapping = aes(x = reorder(features, -user_count), y = user_count)) + 
  geom_col(aes(fill = features)) +
  labs(x = "Feature", y = "# of Users", title = "FitBit Feature Popularity")

# From the graph, we can tell that all users use the calorie, intensity, and 
# steps features, whereas very few users use the weight and heartrate features.

# From this information, I can suggest Bellabeat better advertise the lesser 
# used features, but focus on developing the more widely used features. If more
# customers start using the less used features, Bellabeat can look into better 
# developing them. 



# Checking the average amount of sleep FitBit users get.
# Dataframe containing users and their average amount of sleep
average_sleep <- data.frame(aggregate(
  x = sleep$TotalMinutesAsleep, 
  by = list(sleep$Id), 
  FUN = mean))
View(average_sleep)
average_sleep <- average_sleep %>% rename(
  Id = Group.1,
  AvgSleep = x
)

# Converting minutes to hours
MinToHr <- function(x) x / 60
average_sleep["AvgSleep"] <- lapply(average_sleep["AvgSleep"], MinToHr)

# Gathering statistics
avg_sleep <- mean(average_sleep$AvgSleep) # average sleep is 6.2 hours
std_sleep <- sd(average_sleep$AvgSleep)   # standard deviation is 2.3 hours

# Plotting average sleep
ggplot(data = average_sleep, mapping = aes(x = AvgSleep, fill = ..x..)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Average Sleep (hrs)", y = "Number of Users", title = "Average Hours of Sleep per User") +
  scale_fill_gradient(low = "pink", high = "cornflowerblue") +
  scale_x_continuous(breaks = round(seq(min(average_sleep$AvgSleep - 1), max(average_sleep$AvgSleep + 2), by = 1), 1))

# This graph tells us that the majority of FitBit users who use the sleep feature
# average 7 hours of sleep. The average amount of sleep adults should get is 
# between 7 - 9 hours.

# While most users are within the recommended hours slept range, there are still
# a good chunk of users who aren't. The average sleep is below the recommendation
# and the standard deviation is 2 hours, indicating there are users who get even
# less sleep, too. Therefore, I can suggest that BellaBeat better advertises 
# their sleep feature, if they have one, and explain how its features can help 
# users get better sleep.



# Gathering demographics of exercise survey
ggplot(exercise_survey,mapping = aes(x = Age)) +
  geom_bar(aes(fill = Age), show.legend = FALSE) +
  labs(x = "Age", y = "Number of Respondents", title = "Age of Exercise Survey Respondents") +
  scale_fill_brewer(palette = "Set3")

age_percents_exercise <- exercise_survey %>%
  count(age = factor(Age)) %>%
  mutate(pct = prop.table(n))

# This graph tells us that most of the exercise survey respondents are 19-25
# years old, which makes up 43.4% of the respondents. The next highest age group
# is 15-18 years old, which makes up 31.2% of the respondents.



# Finding common barriers of entry for fitness.
# Splitting multiple values into multiple binary columns
barriers <- cSplit_e(exercise_survey, "ExerciseBarriers", sep = ";", mode = "binary", 
         type = "character", fill = 0)
View(barriers)
barriers <- barriers[-c(1:2,5:7,9:18)]

# Tallying barriers
barriers_grouped <- group_by(barriers, Gender)
barriers_table <- barriers_grouped %>% summarise(
  regularity = sum(`ExerciseBarriers_I am not regular in anything`),
  motivation = sum(`ExerciseBarriers_I can't stay motivated`),
  time = sum(`ExerciseBarriers_I don't have enough time`),
  enjoyment = sum(`ExerciseBarriers_I don't really enjoy exercising`),
  none = sum(`ExerciseBarriers_I exercise regularly with no barriers`),
  injury = sum(`ExerciseBarriers_I have an injury`),
  laziness = sum(ExerciseBarriers_Laziness, `ExerciseBarriers_Laziness mostly`, ExerciseBarriers_Lazy),
  stamina = sum(`ExerciseBarriers_Less stamina`),
  friends = sum(`ExerciseBarriers_My friends don't come`),
  gym_access = sum(`ExerciseBarriers_No gym near me`),
  travel = sum(ExerciseBarriers_Travel, `ExerciseBarriers_Travel time I skip`)
)
View(barriers_table)
barriers_table <- t(barriers_table)
barriers_table <- data.frame(barriers_table)
barriers_table <- rename(barriers_table,
                         Male = X1,
                         Female = X2)
barriers_table <- barriers_table[-c(1),]

# Reformatting data for graphing
barriers_table$category <- row.names(barriers_table)
barriers_table <- as.data.table(barriers_table)
write.csv(barriers_table, "Cleaned Data/barriers.csv", row.names = FALSE)
barriers_long <- melt(barriers_table, id.vars = "category")

# Gathering statistics
View(barriers_table)
top_reason_count <- max(barriers_table$Female)                # top reason has 133 respondents
top_reason <- rownames(barriers_table)[which.max(barriers_table$Female)]
as.numeric(top_reason)
top_reason <- barriers_table$category[as.numeric(top_reason)] # top reason is time

# Plotting data
ggplot(barriers_long, aes(category, value, fill = variable)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_y_continuous() +
  labs(x = "Reason", y = "Number of People", title = "Reasons Why People Don't Exercise", fill = "Gender") +
  scale_fill_manual(values = c("cornflowerblue", "lightcoral"))

# This graph tells us that the most popular reason why people don't exercise is due to lack of time. Motivation
# comes in second. 

# With this insight, I can advise Bellabeat to add features to their devices that allow users to set a timer
# or schedule where they can work exercising into their routine.



# Analyzing what motivates people to exercise
# Splitting multiple values into multiple binary columns
motivations <- cSplit_e(exercise_survey, "ExerciseMotivation", sep = ";", mode = "binary", 
                     type = "character", fill = 0)
View(motivations)
motivations <- motivations[-c(1:2,5:12)]

# Tallying motivations
motivations_grouped <- group_by(motivations, Gender)
motivations_table <- motivations_grouped %>% summarise(
  money = sum(`ExerciseMotivation_Doing exercises prevents many diseases.So yeah saves a lot of money .`),
  discipline = sum(`ExerciseMotivation_Exercising gives you discipline and focus and removed bad thoughts from your mind.`),
  health = sum(`ExerciseMotivation_Gotta get that alcohol and **** out of the system`, `ExerciseMotivation_To maintain healthy body and mind`, `ExerciseMotivation_I want to be fit`, `ExerciseMotivation_Control Diabetes`, `ExerciseMotivation_Doing exercises prevents many diseases.So yeah saves a lot of money .`),
  interest = sum(`ExerciseMotivation_I'm sorry ... I'm not really interested in exercising`, `ExerciseMotivation_Not doing exercise`),
  weight = sum(1 + `ExerciseMotivation_I want to lose weight`),
  goal = sum(`ExerciseMotivation_I want to achieve a sporting goal`, 1),
  flexibility = sum(`ExerciseMotivation_I want to be flexible`),
  body = sum(`ExerciseMotivation_I want to increase muscle mass and strength`, `ExerciseMotivation_To maintain healthy body and mind`, `ExerciseMotivation_I want to be fit`),
  youngness = sum(`ExerciseMotivation_I want to look young and think young`),
  stress_relief = sum(`ExerciseMotivation_I want to relieve stress`),
  mind = sum(`ExerciseMotivation_Exercising gives you discipline and focus and removed bad thoughts from your mind.`, `ExerciseMotivation_I want to look young and think young`, `ExerciseMotivation_I want to think clearly and I want to play cricket with my grandkids`),
  family = sum(`ExerciseMotivation_I want to think clearly and I want to play cricket with my grandkids`, `ExerciseMotivation_My dad motivates me`),
  personal = sum(`ExerciseMotivation_Personal reasons`)
)   # Since emojis cannot be included in back ticks, I had to manually include their count in the sum functions
View(motivations_table)
motivations_table <- t(motivations_table)
motivations_table <- data.frame(motivations_table)
motivations_table <- rename(motivations_table,
                         Female = X1,
                         Male = X2)
motivations_table <- motivations_table[-c(1),]

# Reformatting data for graphing
motivations_table$category <- row.names(motivations_table)
motivations_table <- as.data.table(motivations_table)
write.csv(motivations_table, "Cleaned Data/motivations.csv", row.names = FALSE)
motivations_long <- melt(motivations_table, id.vars = "category")

# Gathering statistics
View(motivations_table)
top_motivation_count <- max(as.numeric(motivations_table$Female))         # top female motivation has 423 respondents
top_motivation <- rownames(motivations_table)[which.max(motivations_table$Female)]
as.numeric(top_motivation)
top_motivation <- motivations_table$category[as.numeric(top_motivation)] # top female motivation is weight

# Plotting data
ggplot(motivations_long, aes(category, as.numeric(value), fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Motivation", y = "Number of People", title = "Reasons Why People Exercise", fill = "Gender") +
  scale_fill_manual(values = c("lightcoral", "cornflowerblue"))

# This graph tells us that the most popular motivation for exercising is for 
# weight loss and general body appearance. This is true for both males and 
# females. Health comes in at a solid third place.

# Since it's already been established the the second most popular reason for not
# exercising is due to a lack of motivation, Bellabeat can use the insights from
# this graph to make features that keep users motivated. For example, users can
# select from a list of possible motivations which ones pertain to them and 
# the device can use those choices to help keep them motivated.



# Gathering demographics from device survey
# --- Ages ---
age_order <- c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64")
ggplot(device_survey, aes(x = factor(What.is.your.age., age_order))) +
  geom_bar(aes(fill = What.is.your.age.), show.legend = FALSE) +
  labs(x = "Age", y = "Number of Respondents", title = "Ages of Fitness Device Users") +
  scale_fill_brewer(palette = "Set3")

age_percents <- device_survey %>%
  count(age = factor(What.is.your.age.)) %>%
  mutate(pct = prop.table(n))

# Most respondents are 18-24, which makes up 33.33% of the respondents. The next
# largest age group is 25-34, which makes up 20% of the respondents.

# --- Gender ---
ggplot(device_survey, aes(x = What.is.your.gender.)) +
  geom_bar(aes(fill = What.is.your.gender.), show.legend = FALSE) +
  labs(x = "Gender", y = "Number of Respondents", title = "Gender Comparison of Fitness Device Users") +
  scale_fill_manual(values = c("lightcoral", "cornflowerblue", "darkseagreen"))

count(device_survey, gender = factor(What.is.your.gender.))

# 15 respondents are female, 13 are male, and 2 prefer not to say. This 
# indicates a fairly balanced group.

# --- Device Ownership Length ---
ownership_order <- c("Less than 6 months", "6-12 months", "1-2 years", "More than 2 years")
ggplot(device_survey, aes(x = factor(How.long.have.you.been.using.a.fitness.wearable., ownership_order))) +
  geom_bar(aes(fill = What.is.your.gender.), position = "dodge") +
  labs(x = "Length of Ownership", y = "Number of respondents", title = "Length of Fitness Device Ownership", fill = "Gender") +
  scale_fill_manual(values = c("lightcoral", "cornflowerblue", "darkseagreen"))

count(device_survey, ownership_length = factor(How.long.have.you.been.using.a.fitness.wearable.))

# Most respondents (13) have owned their device for less than 6 months . This 
# may produce a bias in my resulting analysis, since it is unclear whether or 
# not these respondents will continue to use their device for longer.
# To avoid this bias, I will focus my analysis on the data coming from those 
# owning their device for 6 or more months. Together, this number of respondents
# is 17.

# -- Device Usage Frequency ---
usage_frequency_order <- c("Rarely", "1-2 times a week", "3-4 times a week", "Daily")

palette <- wes_palette("Royal2", 4, type = "discrete")
ggplot(device_survey, aes(x = factor(How.frequently.do.you.use.your.fitness.wearable., usage_frequency_order))) +
  geom_bar(aes(fill = factor(How.long.have.you.been.using.a.fitness.wearable., ownership_order)), position = "dodge") +
  labs(x = "Frequency", y = "Number of Respondents", title = "Frequency of Fitness Device Usage", fill = "Length of Ownership") +
  scale_fill_manual(values = palette)
  
# Those who have owned their devices longer than 6 months use their device at 
# least 1-2 times a week. Most use their device 3-4 times a week. This may 
# indicate that if a device owner uses their device more frequently, they're
# more likely to continue using it. 
# This finding, coupled with the earlier finding that a significant barrier to 
# exercising is finding the time, strengthens my suggestion of including a
# schedule feature to Bellabeat's devices. 

# Checking impact of fitness wearable on exercise motivation and sleep patterns

# -- Impact on Exercise Motivation ---
agreement_order <- c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")
ggplot(device_survey, aes(x = factor(Has.the.fitness.wearable.helped.you.stay.motivated.to.exercise., agreement_order))) +
  geom_bar(aes(fill = factor(How.long.have.you.been.using.a.fitness.wearable., ownership_order)), position = "dodge") +
  labs(x = "Fitness Wearable Has Improved Motivation", y = "Number of Respondents", title = "Has the fitness wearable helped users stay motivated?", fill = "Length of Ownership") +
  scale_fill_manual(values = palette)

ggplot(device_survey, aes(x = factor(Has.the.fitness.wearable.helped.you.stay.motivated.to.exercise., agreement_order))) +
  geom_bar(aes(fill = What.is.your.gender.), position = "dodge") +
  labs(x = "Fitness Wearable Has Improved Motivation", y = "Number of Respondents", title = "Has the fitness wearable helped users stay motivated?", fill = "Gender") +
  scale_fill_manual(values = c("lightcoral", "cornflowerblue", "darkseagreen"))

motivation_percents <- device_survey %>%
  count(improved_motivation = factor(Has.the.fitness.wearable.helped.you.stay.motivated.to.exercise.)) %>%
  mutate(pct = prop.table(n))

motivation_percent_positive <- motivation_percents$pct[1] + motivation_percents$pct[3]   # 80% of users have improved motivation

# Most users (80%) report their fitness wearable as improving their motivation 
# to exercise. This is a good correlation for Bellabeat to point out in their
# advertisements since we've already established that lack of motivation is the
# second most common reason for not exercising. Bellabeat can also analyze how
# FitBits keep their users motivated and use this information to improve their
# products.

# --- Impact on Sleep ---
ggplot(device_survey, aes(x = factor(Has.the.fitness.wearable.improved.your.sleep.patterns., agreement_order))) +
  geom_bar(aes(fill = factor(How.long.have.you.been.using.a.fitness.wearable., ownership_order)), position = "dodge") +
  labs(x = "Fitness Wearable Has Improved Sleep", y = "Number of Respondents", title = "Has the fitness wearable improved users' sleep?", fill = "Length of Ownership") +
  scale_fill_manual(values = palette)

ggplot(device_survey, aes(x = factor(Has.the.fitness.wearable.improved.your.sleep.patterns., agreement_order))) +
  geom_bar(aes(fill = What.is.your.gender.), position = "dodge") +
  labs(x = "Fitness Wearable Has Improved Sleep", y = "Number of Respondents", title = "Has the fitness wearable improved users' sleep?", fill = "Gender") +
  scale_fill_manual(values = c("lightcoral", "cornflowerblue", "darkseagreen"))

sleep_percents <- device_survey %>%
  count(improved_sleep = factor(Has.the.fitness.wearable.improved.your.sleep.patterns.)) %>%
  mutate(pct = prop.table(n))

sleep_percent_positive <- sleep_percents$pct[1] + sleep_percents$pct[4]   # 80% of users have improved sleep

# Most users (80%) either agree or strongly agree that their fitness wearable has 
# improved their sleep patterns. Since we've already learned that many fitness
# device users don't get enough sleep, Bellabeat can advertise how their devices
# improve users sleep. They can also research how FitBit helps users improve 
# their sleep and use this information to improve their products.