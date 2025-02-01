# Project Overview
</br>
In this case study, I play the position of a junior data analyst for Bellabeat, a high-tech manufacturer of women's health         products. The stakeholders of the company  believes that analyzing non-Bellabeat smart device fitness data could help unlock new   growth opportunities for the company.  I've been entrusted with focusing on one of Bellabeat's products and analyzing              non-Bellabeat smart device data to gain insight into how consumers use their non-Bellabeat smart devices and how these trends can  be applied to Bellabeat customers. I'll be using data from FitBit Fitness Tracker, a public dataset made available by Mobius in    Kaggle. I am going to provide the findings of my research to the Bellabeat executive team, along with my high-level                recommendations that will them with their marketing efforts.
</br>

## Business Task
</br>
Analyzing non-Bellabeat smart device data to gain insight into how consumers 
are using their non-Bellabeat smart devices.
</br>
</br>

**Goals**

+ Find trends among the people using their non-Bellabeat smart devices

+ See if these trends align with Bellabeat users

+ Use that knowledge to make a marketing strategy

</br>

## Data Sources
</br>
For our task we are using [FitBit Fitness Tracker Data](https://www.kaggle.com/datasets/arashnic/fitbit). This Kaggle data set contains personal fitness tracker from thirty fitbit users over the course of a month. Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, calories burned, weight and
sleep monitoring. It includes information can be used to explore users’ habits.
</br>

## Cleaning And Manipulation Of Data
</br>

#### Using R as the data analysis tool for the project 
</br>

**Loading packages**
</br>
```{r}
library(tidyverse)
```
</br>

**Importing csv files and creating data sets**

</br>

```{r}
daily_activity <- read.csv("C:\\Users\\1iCE\\Desktop\\New folder (2)\\daily_data\\dailyActivity_merged.csv")
daily_weight <- read.csv("C:\\Users\\1iCE\\Desktop\\New folder (2)\\daily_data\\weightLogInfo_merged.csv")
daily_sleep <- read.csv("C:\\Users\\1iCE\\Desktop\\New folder (2)\\daily_data\\sleepDay_merged.csv")
```

</br>


**Showing first few data on the data sets**
</br>
```{r}
head(daily_activity)
head(daily_weight)
head(daily_sleep)
```
</br>

**Formatting the data sets**
</br>
```{r}
#Formatting rows of the date column to the standard date format
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, "%m/%d/%Y")
daily_weight$Date <- as.Date(daily_weight$Date, "%m/%d/%Y")
daily_sleep$SleepDay <- as.Date(daily_sleep$SleepDay, "%m/%d/%Y")

#Changing column name to use "Date" as a foreign key
colnames(daily_activity)[2] <- "Date"
colnames(daily_weight)[2] <- "Date"
colnames(daily_sleep)[2] <- "Date"
```
</br>


**Making copy of the data sets to avoid unintentional changes on original data sets**
</br>
```{r}
#coping data sets
activity <- daily_activity
weight <- daily_weight
sleep <- daily_sleep
```
</br>

**Merging data sets**
</br>
```{r}
#Merging
activity_weight <- merge(x=activity, y=weight)
activity_sleep <- merge(x=activity, y=sleep)
```
</br>


## Analysis
</br>

**Organizing and categorizing users based on the duration of their sleep**

Types of sleep:

+ Normal Sleepers: Sleep time is between 360 minutes (6 hours) to 480 minutes (8 hours).

+ Bad Sleepers: Sleep times is less then 360 minutes (6 hours).

+ Over Sleepers: Sleep time is over 480 minutes (8 hours).
</br>

```{r}
#Labeling users according to their sleeping time
sleep_type <- sleep %>%
  mutate(OverSlept = case_when(TotalMinutesAsleep > 480 ~ 1, TRUE ~ 0),
         UnderSlept = case_when(TotalMinutesAsleep < 360 ~ 1, TRUE ~ 0),
         Normal = case_when(TotalMinutesAsleep > 360 & TotalMinutesAsleep < 480 ~ 1, TRUE ~ 0)) %>%
  group_by(Id) %>%
  summarise(OverSlept = sum(OverSlept),
            UnderSlept = sum(UnderSlept),
            Normal = sum(Normal)) %>%
  mutate(SleepType = case_when(OverSlept > UnderSlept & OverSlept > Normal ~ "Over Sleeper",
                               UnderSlept > OverSlept & UnderSlept > Normal ~ "Bad Sleeper",
                               Normal > OverSlept & Normal > UnderSlept ~ "Normal Sleeper")) %>% drop_na()
```
</br>

**Organizing and categorizing users based on their calories burned and distance covered**
</br>
```{r}
activity_type_length <- activity %>%
  group_by(Id) %>%
  summarise(Calories = sum(Calories))%>%
  mutate(ActiveType = case_when(Calories > quantile(Calories, probs = 0.75) ~ ">75%",
                                Calories > quantile(Calories, probs = 0.50) ~ "50%<",
                                Calories > quantile(Calories, probs = 0.25) ~ ">25%",
                                Calories < quantile(Calories, probs = 0.25) ~ "25%<",)) %>% drop_na()

activity_type_distance <- activity %>%
  group_by(Id) %>%
  summarise(TotalDistance = sum(TotalDistance))%>%
  mutate(ActiveType = case_when(TotalDistance > quantile(TotalDistance, probs = 0.75) ~ ">75%",
                                TotalDistance > quantile(TotalDistance, probs = 0.50) ~ "50%<",
                                TotalDistance > quantile(TotalDistance, probs = 0.25) ~ ">25%",
                                TotalDistance < quantile(TotalDistance, probs = 0.25) ~ "25%<",)) %>% drop_na()
```
</br>

**Formatting and merging data for visualization**
</br>
```{r}
#Removing irrelevant rows
activity_type_length <- activity_type_length[,-2]
activity_type_distance <- activity_type_distance[,-2]
sleep_type <- sleep_type[,-2:-4]

#Merging data sets
user_calories_burned_sleep_type <- merge(x=activity_type_length, y=sleep_type, by="Id")
user_distance_sleep_type <- merge(x=activity_type_distance, y=sleep_type, by="Id")
```
</br>

## Graphical Represntaion Of The Key Findings
</br>

**Finding 1**

Our first bar graph shows users sleep type based on how much calories they burned where we can see users who are burning more calories are sleeping better. But the number of bad sleepers and over sleepers are still there below the 75th percentile.
</br>
```{r}
#Effects of calories burned on sleep type
ggplot(data=user_calories_burned_sleep_type) + 
  geom_bar(mapping = aes(x=ActiveType, fill=SleepType), position = position_dodge(), width = 0.7)+
  labs(title = "Peoples Sleep Type Based On How Much Calories They Burned", x="Calories Burned", y="Number Of People", fill="Sleep Type")+
  scale_fill_brewer(palette="Reds")
```
![1](https://github.com/NashidulSarker/Google-Data-Analytics-Case-Study/assets/105308659/51a13da4-eb1e-42cc-83ad-c70da4a334b3)


</br>

**Finding 2**

Our next bar graph shows users sleep type based on how much distance the covered where we can again see users who are covering more distance while walking or jogging are sleeping better. But the discrepancy of bad sleepers and over sleepers are still there after the 75th percentile.
</br>
```{r}
#Effects of distance covered on sleep type
ggplot(data=user_distance_sleep_type) + 
  geom_bar(mapping = aes(x=ActiveType, fill=SleepType), position = position_dodge(), width = 0.7)+
  labs(title = "Peoples Sleep Type Based On How Much Distance They Covered", x="Distance Corved", y="Number Of People", fill="Sleep Type")+
  scale_fill_brewer(palette="Reds")
```
![2](https://github.com/NashidulSarker/Google-Data-Analytics-Case-Study/assets/105308659/daf93c26-dea2-49f2-b425-5e8ad6b9d9df)

</br>

**Finding 3**

We shed some light on the earlier discrepancy of our first two findings in our next two scatter plots. Here, we can see that users who are over the mean weight are the ones who are burning more calories and covering more distance.
</br>
```{r}
#Showing relation between calories burned and weight of the individuals
ggplot(data=activity_weight) + 
  geom_point(mapping = aes(x=Calories, y=WeightKg, color=WeightKg), size=2)+
  geom_smooth(aes(x=Calories, y=WeightKg), se = FALSE)+
  labs(title = "Calories burned by people based how much they weight")

#Showing relation between distance covered and weight of the individuals
ggplot(data=activity_weight) + 
  geom_point(mapping = aes(x=TotalDistance, y=WeightKg, color=WeightKg), size=2)+
  geom_smooth(aes(x=TotalDistance, y=WeightKg), method = lm, se = FALSE)+
  labs(title = "Total distance covered by people based how much they weight", x="Total Distance")
```

![3](https://github.com/NashidulSarker/Google-Data-Analytics-Case-Study/assets/105308659/886e6805-1779-43ad-87a6-e4657517e7c2)
![4](https://github.com/NashidulSarker/Google-Data-Analytics-Case-Study/assets/105308659/c7f9d8ca-6098-4b7a-9ee0-6cafa030a01c)


</br>

**Finding 4**

Our last two scatter plot shows users sleep time based on how much calories they burned and distance they covered. Here we can see a clearly that users who are working out more (by burning calories and covering distance) are the ones who are with normal sleep time. 
</br>
```{r}
#Showing relation between calories burned and minutes slept
ggplot(data=activity_sleep) + 
  geom_point(mapping = aes(x=TotalMinutesAsleep, y=Calories, color=Calories))+
  labs(title = "Relation between calories burned by people and minutes slept", x='Total Minutes Asleep')
  
#Showing relation between distance covered and minutes slept
ggplot(data=activity_sleep) + 
  geom_point(mapping = aes(x=TotalMinutesAsleep, y=TotalDistance, color=TotalDistance))+
  labs(title = "Relation between distance covered by people and minutes slept",x="Total Minutes Asleep", y='Total Distance')
```
![5](https://github.com/NashidulSarker/Google-Data-Analytics-Case-Study/assets/105308659/ebb1ff03-bbe4-403f-9eec-6151be1442d2)
![6](https://github.com/NashidulSarker/Google-Data-Analytics-Case-Study/assets/105308659/196abdf8-a0a1-42cf-b80a-1973411943fc)



</br>

## Conclusion and Recommendations
</br>
After carefully storing, formatting, manipulating, analyzing, and visualizing the data, I have come to a conclusion. The data that was provided was only for 30 individuals and of 30 days. The sample size and duration of the data is low, but two trends have been spotted very clearly.

+ Users who are above the mean weight are the ones who are working out more.

+ Users who are working out more are the ones who are within the range of the normal sleeping period.

</br>

**Recommendations**

</br>
The Bellabeat app provides users with health data related to their activity, sleep, stress,
menstrual cycle, and mindfulness habits. This data can help users better understand their current habits and
make healthy decisions. We can use the app:

+ To provide more insights about users sleeping pattern

+ Find out how much calories they need to burn for a better sleep routine

+ Provide a detail workout based on how calories they need to burn based on their weight

+ Market our services towards people with sleeping issues 

</br>

Bellabeat will win over customers' goodwill by doing this. Resolving issues that have a direct impact on people's lives will enable us to develop and grow the business. However, as we expand, we will not just make more profits, but we will also be able to solve more issues of this nature, and by helping our customers, we will provide them with a healthier way of life.

