library(tidyverse)
library(lubridate)
library(scales)
library(powerMediation)

# Create Synthetic Data
clicked_adopt_today_1<-sample(0:1, 365*10, replace=T,prob=c(0.72,0.25))
clicked_adopt_today_2<-sample(0:1, 365*10, replace=T,prob=c(0.6,0.4))
clicked_adopt_today<-c(clicked_adopt_today_1,clicked_adopt_today_2)
visit_date<-rep(seq(as.Date("2017/1/1"), as.Date("2017/12/31"),by="1 day"),20)
click_data<-data.frame(visit_date,clicked_adopt_today)
click_data<-click_data[order(clicks_data$dates),]
head(click_data)

# Find oldest and most recent date
min(click_data$visit_date)
max(click_data$visit_date)

# Find Baseline Conversion Rates
# Calculate the mean conversion rate by day of the week
click_data %>%
  group_by(weekdays(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))


# Compute conversion rate by week of the year
click_data_sum <- click_data %>%
  group_by(week = week(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Plot conversion rate to check for seasonality
ggplot(click_data_sum, aes(x = week,
                           y = conversion_rate)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent)

# Here, the data does not have much seasonality. But, if there is seasonality, 
# running control at a point in time where a seasonal high/low  occurs and test in the 
# subsequent point in time where the metric does not have a seasonal high/low may bias the
# test to show that the control is performing better/worse than test.
# As a safeguard for this, we do a power analysis. The whole objective of power analysis
# is to estimate the sample size required.
# The key things required for a power analysis are:
##1. Statistical test you are planning to run
##2. Baseline Value - Value for control condition
##3. Desired Value - Expected value for the test condition
##4. Proportion of data in test (ideally 0.5)
##5. alpha (0.05)
##6. power/1-beta probability correctly rejecting null hypothesis (generally 0.8)

click_data_month<-click_data %>%
  group_by(month(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Compute and look at sample size for experiment in August with a 10 percentage point increase
total_sample_size <- SSizeLogisticBin(p1 = round(as.numeric(click_data_month[8,2]),2),
                                      p2 = round(as.numeric(click_data_month[8,2])+0.1,2),
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size


# Compute and look at sample size for experiment in August with a 5 percentage point increase
total_sample_size <- SSizeLogisticBin(p1 = round(as.numeric(click_data_month[8,2]),2),
                                      p2 = round(as.numeric(click_data_month[8,2])+0.05,2),
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size


# Decide % of users to be included in Test from sample
test.split<-0.5

# Decide Baseline Conversion Rate (CR of Control from historical values)
base.conv<-0.27

# Decide Minimum Effect Size (increase of metric in Test) that you desire to see 
min.effect.size<-0.02

sample.size<-2000
test.sample<-rbinom(sample.size*test.split,1,base.conv+min.effect.size)
control.sample<-rbinom(sample.size*(1-test.split),1,base.conv)

test.cr<-mean(test==1)
control.cr<-mean(control==1)

se.test<-sd(test)/sqrt(sample.size*test.split)
se.control<-sd(control)/sqrt(sample.size*(1-test.split))
se.pooled<-sqrt(var(test)/(sample.size*test.split) + var(control)/(sample.size*(1-test.split)))

# Here we know the test and control samples and hence can calculate sd for the samples
# directly but, if not known, variance can be estimated using p(1-p)
# sqrt(test.cr*(1-test.cr))

# H0: diff = 0
# H1: diff <> 0
(test.cr-control.cr)/se.pooled



# Find Minimum Sample Size
pooled.cr<-base.conv*(1-test.split) + test.conv*test_split
(2*pooled.cr*(1-pooled.cr)*((0.84 + 1.96)^2))/((test.conv-base.conv)^2)



test.conv<-0.3

stat.power<-0.8
alpha<-0.05
sample.size

















