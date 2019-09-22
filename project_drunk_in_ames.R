
'22/09/2019 - Sunday
Name: Carlos Henrique da Silva Amaral'

'PROJECT: WHO IS DRUNK AND WHEN IN AMES, IOWA?'


setwd("~/3. DATACAMP/Projects/Who is drunk and when in Ames, Iowa")

# load the tidyverse suite of packages 
library(tidyverse)


'1. Breath alcohol tests in Ames, Iowa, USA

Ames, Iowa, USA is the home of Iowa State University, a land grant university 
with over 36,000 students. 
By comparison, the city of Ames, Iowa, itself only has about 65,000 residents. 
As with any other college town, Ames has had its fair share of 
alcohol-related incidents. (For example, Google "VEISHEA riots 2014".) 
We will take a look at some breath alcohol
test data from Ames that is published by the State of Iowa.'

#Task 1: Instructions
#First, get the data into your workspace and summarize it by year. 
#Do you notice a pattern over time?

# read the data into your workspace
ba_data <- read.csv("datasets/breath_alcohol_ames.csv", sep=";")

# quickly inspect the data
head(ba_data)

# obtain counts for each year 
ba_year <- ba_data %>% count(year)
ba_year



'2. What is the busiest police department in Ames?

There are two police departments in the data set: 
the Iowa State University Police Department and the Ames Police Department. 
Which one administers more breathalyzer tests?
'
#Task 2: Instructions
#Count the data by location to see which department administers 
#more breathalyzer tests.

# use count to tally up the totals for each department
pds <- ba_data %>% count(location)
pds



'3. Nothing Good Happens after 2am

We all know that "nothing good happens after 2am." 
Thus, there are inevitably some times of the day when breath alcohol tests, 
especially in a college town like Ames, are most and least common.
Which hours of the day have the most and least breathalyzer tests?
'
#Task 3: Instructions
#Summarize the data and create a bar chart of number 
#of tests by hour of the day.

# count by hour and arrange by descending frequency
hourly <- ba_data %>% 
  count(hour) %>% 
  arrange(desc(n))

# use a geom_ to create the appropriate bar chart
ggplot(hourly, aes(x = hour,
                   weight = n)) + 
  geom_bar()



'4. Breathalyzer tests by month

Now that we have discovered which time of day is most common 
for breath alcohol tests, we will determine which time of the year 
has the most breathalyzer tests. 
Which month will have the most recorded tests?
'

#Task 4: Instructions
#We'll look at the month variable to determine the most popular 
#time of year for breathalyzer tests.


# count by month and arrange by descending frequency
monthly <- ba_data %>% 
  count(month) %>% 
  arrange(desc(n))

# make month a factor
monthly$month <- as.factor(monthly$month)

# use a geom_ to create the appropriate bar chart
ggplot(monthly, aes(x = month, 
                    y = n, 
                    fill = month)) + 
  geom_col()



'5. COLLEGE

When we think of (binge) drinking in college towns in America, 
we usually think of something like this image at the left. 
And so, one might suspect that breath alcohol tests are given
to men more often than women and that men drink more than women.
'
#Task 5: Instructions
#Compare test frequency and results for men vs. women.


# count by gender 
ba_data %>% count(gender)

# create a dataset with no NAs in gender 
clean_gender <- ba_data %>% filter(!is.na(gender)) 

clean_gender %>% count(gender)

# create a mean test result variable and save as mean_bas
mean_bas <- clean_gender %>% 
  mutate(meanRes = (Res1 + Res2)/2)
head(mean_bas)

# create side-by-side boxplots to compare the mean 
#blood alcohol levels of men and women
ggplot(mean_bas, aes(x = gender, 
                     y = meanRes,
                     fill = gender)) + 
  geom_boxplot()



'6. Above the legal limit

In the USA, it is illegal to drive with a blood alcohol concentration (BAC) 
above 0.08%. This is the case for all 50 states.
Assuming everyone tested in our data was driving 
(though we have no way of knowing this from the data), 
if either of the results (Res1, Res2) are above 0.08,
the person would be charged with DUI (driving under the influence).
'

#Task 6: Instructions
#Determine what percent of the breathalyzer tests in the data 
#are above the legal limit.

# Filter the data
duis <- ba_data %>% filter(Res1 > 0.08 | Res2 > 0.08)

# proportion of tests that would have resulted in a DUI
p_dui <- nrow(duis)/nrow(ba_data)
p_dui


'7. Breathalyzer tests: is there a pattern over time?
We previously saw that 2am is the most common time of day for 
breathalyzer tests to be administered, and August is the most 
common month of the year for breathalyzer tests. 
Now, we look at the weeks in the year over time.

We briefly use the lubridate package for a bit of date-time manipulation.'

####Task 7: Instructions
#Create a date variable and determine the week in the year each test occurred.

library(lubridate) 

# Create date variable using paste() and ymd()
ba_data <- ba_data %>% mutate(date = ymd(paste(year, month, day)))

# Create a week variable using week()
ba_data <- ba_data %>% mutate(week = week(date))
head(ba_data)




'8. Looking at timelines

How do the weeks differ over time? 
One of the most common data visualizations is the time series, 
a line tracking the changes in a variable over time. 
We will use the new week variable to look at test frequency over time. 
We end with a time series plot showing frequency of breathalyzer tests 
by week in year, with one line for each year.
'
#Task 8: Instructions
#Create a time series plot to compare weeks across years.

# create the weekly data set 
weekly <- ba_data %>% count(week, year)

# ungroup is necessary for the plot later
weekly <- weekly %>% ungroup() 

# make year a factor
weekly <- weekly %>% mutate(year = as.factor(year))

# create the time series plot with one line for each year
ggplot(weekly, aes(x = week, y = n)) + 
  geom_line() + 
  geom_point(aes(color = year)) +  # included to make the plot more readable 
  scale_x_continuous(breaks = seq(0,52,2))  # to make the x-axis more readable 



'9. The end of VEISHEA

From Wikipedia: 
"VEISHEA was an annual week-long celebration held each spring 
on the campus of Iowa State University in Ames, Iowa. 
The celebration featured an annual parade and many open-house 
demonstrations of the university facilities and departments. 
Campus organizations exhibited products, technologies, and 
held fundraisers for various charity groups. 
In addition, VEISHEA brought speakers, lecturers, 
and entertainers to Iowa State. [...] 
VEISHEA was the largest student-run festival in the nation, 
bringing in tens of thousands of visitors to the campus each year."


This over 90-year tradition in Ames was terminated permanently 
after riots in 2014, where drunk celebrators flipped over 
multiple vehicles and tore light poles down. 
This was not the first incidence of violence and severe 
property damage in VEISHEAs history. 
Did former President Leath make the right decision?'

#Task 9: Instructions
#Run the provided code to plot the previous time series chart with annotations 
#pointing to the last two VEISHEA weeks in Iowa State's history.

## Run this code to create the plot 
ggplot() + 
  geom_point(data = weekly, 
             aes(x = week, y = n, color = year)) + 
  geom_line(data = weekly, 
            aes(x = week, y = n, color = year)) +  
  # included to make the plot more readable 
  geom_segment(data = NULL, 
               arrow = arrow(angle = 20, 
                             length = unit(0.1, "inches"),
                             ends = "last", type = "closed"), 
               aes(x = c(20,20), 
                   xend = c(15.5,16), 
                   y = c(21, 20), 
                   yend = c(21, 12.25))) + 
  geom_text(data = NULL, 
            aes(x = 23, 
                y = 20.5, 
                label = "VEISHEA Weeks"), 
            size = 3) + 
  scale_x_continuous(breaks = seq(0,52,2)) 

## Make a decision about VEISHEA. TRUE or FALSE?  
cancelling_VEISHEA_was_right <- TRUE


