# Exercise based on Airbnb data.
# Data source: https://www.kaggle.com/c/airbnb-recruiting-new-user-bookings/data
# Created 2016-05-02

library(dplyr)
library(ggplot2)

getwd()
setwd("/Users/Teradata/Documents/Learning_R/Airbnb")
dir()
sink("Airbnb.txt", append=T, split=T)


# 1. Import data
df_users <- read.csv("train_users_2.csv")
df_sessions <- read.csv("sessions.csv")
df_countries <- read.csv("countries.csv")
df_age_gender_bkts <- read.csv("age_gender_bkts.csv")


# 2. Exam data structure
str(df_sessions)
head(df_sessions)
summary(df_sessions)

str(df_users)
summary(df_users)

summary(df_countries)
summary(df_age_gender_bkts)


# 3. Explore session data
# 3.1 Top action types
df_sessions %>%
  group_by(action_type) %>%
  summarize(n_action_type=n()) %>%
  arrange(desc(n_action_type))

# 3.2 Mapping of action details
df_sessions %>%
  group_by(action_type, action, action_detail) %>%
  summarise(n_records=n()) %>%
  arrange(desc(n_records)) %>%
  write.csv(file="action_map.csv")


# 4. Explore user data
# 4.1 User ID
df_users %>%
  summarize(cnt_records=n(), cnt_unique_users=n_distinct(id)) %>%
  mutate(id_is_unique = (cnt_records == cnt_unique_users))
# There are 213,451 observations. Data is unique on id column.

# 4.2 Age
# quantile function
df_users %>%
  select(age) %>%
  as.matrix() %>%
  quantile(probs=c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm=T)

# define my quantile function
MyQuantile <- function(x, y) {
  newx <- sort(x, na.last=NA)
  l <- length(newx)
  q <- (l - 1) * y + 1
  floorq <- floor(q)
  o <- newx[floorq] + (newx[pmin(l, floorq + 1)] - newx[floorq]) * (floorq - q)
  names(o) <- paste(y*100, "%", sep="")
  return(o)
}

df_users %>%
  select(age) %>%
  as.matrix() %>%
  MyQuantile(y=c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))
