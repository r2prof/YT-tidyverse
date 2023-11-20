# CH 04-----

library(tidyverse)

# Tidy data

# We say that a data table is in tidy format if each row represents one 
# observation and columns represent the different variables available for 
# each of these observations. The murders dataset is an example of a tidy data 
# frame.

library(dslabs)
data(murders)
View(murders)
head(murders)

data("CO2")
View(CO2)
head(CO2)

data("ChickWeight")
View(ChickWeight)

data("BOD")
View(BOD)

# For instance, to change the data table by adding a new column, we use mutate. 
# To filter the data table to a subset of rows, we use filter. 
# Finally, to subset the data by selecting specific columns, we use select.


# Manipulating data frames

# 01-mutate----
# Adding a column with mutate
# The function mutate takes the data frame as a first argument and the name and
# values of the variable as a second argument using the convention name = values. 
# So, to add murder rates, we use:


murders <- mutate(murders, rate = total / population * 100000)
murders


# Notice that here we used total and population inside the function, which are 
# objects that are not defined in our workspace. But why don't we get an error?

# This is one of dplyr's main features. Functions in this package, such as 
# mutate, know to look for variables in the data frame provided in the first 
# argument. 

# In the call to mutate above, total will have the values in murders$total. 
# This approach makes the code much more readable.


# 02-filter----
# Subsetting with filter
# Now suppose that we want to filter the data table to only show the entries for 
# which the murder rate is lower than 0.71. To do this we use the filter function, 
# which takes the data table as the first argument and then the conditional 
# statement as the second.
filter(murders, rate <= 0.71)
a= filter(murders, rate <= 0.71)
a
cat("\f")

# 03-filter-----
# Electing columns with select
# Although our data table only has six columns, some data tables include 
# hundreds. If we want to view just a few, we can use the dplyr select function. 
# In the code below we select three columns, assign this to a new object and 
# then filter the new object:

new_table <- select(murders, state, region, rate)
new_table
filter(new_table, rate <= 0.71)

# In the call to select, the first argument murders is an object, but state, 
# region, and rate are variable names.
cat("\f")

# 4.4 Exercises----
# Q-01: 
murders <- mutate(murders, population_in_millions = population / 10^6)
murders

# Q-02:
# If rank(x) gives you the ranks of x from lowest to highest, rank(-x) gives you
# the ranks from highest to lowest. Use the function mutate to add a column rank 
# containing the rank, from highest to lowest murder rate. Make sure you redefine 
# murders so we can keep using this variable.
murders <- mutate(murders, rank = rank(-rate))
murders

cat("\f")
# Q-03: With dplyr, we can use select to show only certain columns. 
# For example, with this code we would only show the states and population sizes:
select(murders, state, population) |> head()

# Use select to show the state names and abbreviations in murders. 
# Do not redefine murders, just show the results.
head(murders)
select(murders, state, abb) |> head()
select(murders, state, population) %>% head()

cat("\f")
# Q-04: 
# The dplyr function filter is used to choose specific rows of the data frame 
# to keep. Unlike select which is for columns, filter is for rows. 
# For example, you can show just the New York row like this:

filter(murders, state == "New York")

# You can use other logical vectors to filter rows.

# Use filter to show the top 5 states with the highest murder rates. 
# After we add murder rate and rank, do not change the murders dataset, 
# just show the result. Remember that you can filter based on the rank column.

filter(murders, rank <= 5 )

cat("\f")
# Q-05:
# We can remove rows using the != operator. 
# For example, to remove Florida, we would do this:
no_florida <- filter(murders, state != "Florida")
no_florida

# Create a new data frame called no_south that removes states from the South region. 
# How many states are in this category? You can use the function nrow for this.
no_south <- filter(murders, region != "South")
no_south

no_south <- filter(murders, region != "South") %>% nrow()
no_south

cat("\f")
# Q-06:
# We can also use %in% to filter with dplyr. 
# You can therefore see the data from New York and Texas like this:
filter(murders, state %in% c("New York", "Texas"))

# Create a new data frame called murders_nw with only the states from the 
# Northeast and the West. How many states are in this category?

filter(murders, region %in% c("Northeast", "West")) %>% nrow()

cat("\f")
# Q-07:
# Suppose you want to live in the Northeast or West and want the murder rate to 
# be less than 1. We want to see the data for the states satisfying these options. 
# Note that you can use logical operators with filter. 

# Here is an example in which we filter to keep only small states in the 
# Northeast region.

filter(murders, population < 5000000 & region == "Northeast")

# Make sure murders has been defined with rate and rank and still has all states. 
# Create a table called my_states that contains rows for states satisfying both 
# the conditions: it is in the Northeast or West and the murder rate is less than 1.
# Use select to show only the state name, the rate, and the rank.
head(murders)
View(murders)

# Note: You can do that with filter directly as below
my_states <- filter(murders, region %in% c("Northeast", "West") & rate <= 1)
my_states

# Note: For select, it didn't work like that. 

# The pipe: %>%----|>
cat("\f")
# With dplyr we can perform a series of operations, for example select and then 
# filter, by sending the results of one function to another using what is called 
# the pipe operator: %>%
# original data --> select --> filter

murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

# same command with |>

murders |> select(state, region, rate) |> filter(rate <= 0.71)


# In general, the pipe sends the result of the left side of the pipe to be the 
# first argument of the function on the right side of the pipe. Here is a very 
# simple example:
  
16 %>% sqrt()

# We can continue to pipe values along:
  
16 %>% sqrt() %>% log2()

# The above statement is equivalent to log2(sqrt(16))

log2(sqrt(16))

# Therefore, when using the pipe with data frames and dplyr, we no longer need 
# to specify the required first argument since the dplyr functions we have 
# described all take the data as the first argument. In the code we wrote:

# 4.6 Exercises----
# Q-01:  The pipe |> can be used to perform operations sequentially without 
#        having to define intermediate objects. Start by redefining murder 
#        to include rate and rank.
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

# murders is the first argument of the select function, and the new data frame 
# (formerly new_table) is the first argument of the filter function.

# Note that the pipe works well with functions where the first argument is the 
# input data. Functions in tidyverse packages like dplyr have this format and 
# can be used easily with the pipe.

murders <- mutate(murders, rate =  total / population * 100000, 
                  rank = rank(-rate))
head(murders)
cat("\f")

my_states <- filter(murders, region %in% c("Northeast", "West") & 
                      rate < 1)
my_states

select(my_states, state, rate, rank)

# The pipe %>% permits us to perform both operations sequentially without 
# having to define an intermediate variable my_states. We therefore could have 
# mutated and selected in the same line like this:


mutate(murders, rate =  total / population * 100000, 
       rank = rank(-rate)) %>% select(state, rate, rank) %>% head()

# Notice that select no longer has a data frame as the first argument. 
# The first argument is assumed to be the result of the operation conducted 
# right before the %>%.

# Reset murders to the original table by using data(murders). Use a pipe to 
# create a new data frame called my_states that considers only states in the 
# Northeast or West which have a murder rate lower than 1, and contains only 
# the state, rate and rank columns. 

# The pipe should also have four components separated by three %>%. The code 
# should look something like this:

data(murders)
my_states <- murders %>%
  mutate(rate =  total / population * 100000, rank = rank(-rate))%>%
   filter(region %in% c("Northeast", "West") & rate < 1) %>% 
    select(state, rate, rank) %>% head()

# 04-Summarizing the data----

# The summarize function in dplyr provides a way to compute summary statistics 
# with intuitive and readable code. 

# We start with a simple example based on heights. The heights dataset includes 
# heights and sex reported by students in an in-class survey.

library(dplyr)
library(dslabs)

data(heights)

str(heights)

head(heights)

hist(heights$height)

s <- heights %>% 
  filter(sex == "Female") %>%
  summarize(average = mean(height), standard_deviation = sd(height))
s

# This takes our original data table as input, filters it to keep only females, 
# and then produces a new summarized table with just the average and the 
# standard deviation of heights. We get to choose the names of the columns of 
# the resulting table. 

# For example, above we decided to use average and standard_deviation, but we 
# could have used other names just the same.

# Because the resulting table stored in s is a data frame, we can access the 
# components with the accessor $:

s$average

s$standard_deviation

# As with most other dplyr functions, summarize is aware of the variable names 
# and we can use them directly. So when inside the call to the summarize function 
# we write mean(height), the function is accessing the column with the 
# name "height" and then computing the average of the resulting numeric vector. 

# We can compute any other summary that operates on vectors and returns a single 
# value. For example, we can add the median, minimum, and maximum heights like this:

heights %>% 
  filter(sex == "Female") %>%
  summarize(median = median(height), minimum = min(height), 
            maximum = max(height))

# Let’s compute the average murder rate for the United States. 
# Remember our data table includes total murders and population size for 
# each state and we have already used dplyr to add a murder rate column:

murders <- murders |> mutate(rate = total/population*100000)

head(murders)
# Remember that the US murder rate is not the average of the state murder rates:

summarize(murders, mean(rate))

# This is because in the computation above the small states are given the same 
# weight as the large ones. The US murder rate is the total number of murders 
# in the US divided by the total US population. So the correct computation is:
us_murder_rate <- murders |>
  summarize(rate = sum(total) / sum(population) * 100000)
us_murder_rate

# Multiple summaries
# Suppose we want three summaries from the same variable such as the median, 
# minimum, and maximum heights. We can use summarize like this:

# But we can obtain these three values with just one line using the quantile 
# function: quantile(x, c(0.5, 0, 1)) returns the median (50th percentile), 
# the min (0th percentile), and max (100th percentile) of the vector x. 

# Here we can’t use summarize because it expects one value per row. For this 
# reason we have to define a function that returns a data frame like this:

median_min_max <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}

heights |> 
  filter(sex == "Female") |>
  summarize(median_min_max(height))

# 05-group_by----
# Group then summarize with group_by
# A common operation in data exploration is to first split data into groups and 
# then compute summaries for each group. For example, we may want to compute the 
# average and standard deviation for men’s and women’s heights separately. 

# The group_by function helps us do this.
heights |> group_by(sex)

# Although not immediately obvious from its appearance, this is now a special 
# data frame called a grouped data frame, and dplyr functions, in particular 
# summarize, will behave differently when acting on this object. 

# Conceptually, you can think of this table as many tables, with the same 
# columns but not necessarily the same number of rows, stacked together in 
# one object.

heights |> 
  group_by(sex) |>
  summarize(average = mean(height), standard_deviation = sd(height))

# 06-pull----
# The us_murder_rate object defined above represents just one number. Yet we 
# are storing it in a data frame:
head(us_murder_rate)
class(us_murder_rate)

# Since, as most dplyr functions, summarize always returns a data frame.

# This might be problematic if we want to use this result with functions that 
# require a numeric value. Here we show a useful trick for accessing values 
# stored in data when using pipes: when a data object is piped that object and 
# its columns can be accessed using the pull function. To understand what we 
# mean take a look at this line of code:
  
us_murder_rate %>% pull(rate)

class(us_murder_rate)

# This returns the value in the rate column of us_murder_rate making it 
# equivalent to us_murder_rate$rate.

# To get a number from the original data table with one line of code we can type:

us_murder_rate <- murders %>% 
summarize(rate = sum(total) / sum(population) * 100000) %>% pull(rate)

us_murder_rate

# which is now a numeric:

class(us_murder_rate)

# 07-arrange----
# Sorting data frames
# When examining a dataset, it is often convenient to sort the table by the 
# different columns. We know about the order and sort function, but for ordering 
# entire tables, the dplyr function arrange is useful. For example, here we order 
# the states by population size:

  
murders %>% arrange(population) %>%
head()

# With arrange we get to decide which column to sort by. To see the states by 
# murder rate, from lowest to highest, we arrange by rate instead:

murders %>% arrange(rate) %>%
head()

# Note that the default behavior is to order in ascending order. In dplyr, the 
# function desc transforms a vector so that it is in descending order. 

# To sort the table in descending order, we can type:

murders %>% 
  arrange(desc(rate)) %>% head(rate)

# Nested sorting
# If we are ordering by a column with ties, we can use a second column to break
# the tie. Similarly, a third column can be used to break ties between first and
# second and so on. Here we order by region, then within region we order by 
# murder rate:
  
murders %>% arrange(region, rate) %>% head()

# The top n

# In the code above, we have used the function head to avoid having the page 
# fill up with the entire dataset. 

# If we want to see a larger proportion, we can use the top_n function. 
# This function takes a data frame as it's first argument, the number of 
# rows to show in the second, and the variable to filter by in the third. 

# Here is an example of how to see the top 5 rows:

murders %>% top_n(5) %>% arrange(rate)


# 4.10 Exercises -----
library(NHANES)
data(NHANES)
View(NHANES)
head(NHANES)

# The NHANES data has many missing values. The mean and sd functions in R will 
# return NA if any of the entries of the input vector is an NA.

# Q-01:
# We will provide some basic facts about blood pressure. 
# First let’s select a group to set the standard. We will use 20-to-29-year-old 
# females. AgeDecade is a categorical variable with these ages. 

# Note that the category is coded like " 20-29", with a space in front! What is 
# the average and standard deviation of systolic blood pressure as saved in the 
# BPSysAve variable? Save it to a variable called ref.

# Hint: Use filter and summarize and use the na.rm = TRUE argument when computing 
# the average and standard deviation. 
# You can also filter the NA values using filter.


ref <- NHANES |> filter(Gender == "female", AgeDecade == " 20-29") |>
  summarize(avg = mean(BPSysAve, na.rm = TRUE), sd = sd(BPSysAve, na.rm = TRUE))

head(ref)

# Q-02:
# Using a pipe, assign the average to a numeric variable ref_avg. 
# Hint: Use the code similar to above and then pull.

ref_avg <- ref |> pull(avg)

ref_avg

class(ref_avg)


# Q-03: 
#  Now report the min and max values for the same group.

ref <- NHANES |> filter(Gender == "female", AgeDecade == " 20-29") |>
  summarize(min_value = min(BPSysAve, na.rm = TRUE), 
            max_value = max(BPSysAve, na.rm = TRUE))
head(ref)

# Q-04
# Compute the average and standard deviation for females, but for each age 
# group separately rather than a selected decade as in question 1. 
# Note that the age groups are defined by AgeDecade. 

# Hint: rather than filtering by age and gender, filter by Gender and then 
# use group_by.

ref_f <- NHANES |> filter(Gender == "female") |>
  group_by(AgeDecade) |>
  summarize(avg_age_f = mean(Age), std_f = sd(Age))

head(ref_f)

# Q-05
# Repeat exercise 4 for males.

ref_m <- NHANES |> filter(Gender == "male") |>
  group_by(AgeDecade) |>
  summarize(avg_age_m = mean(Age), std_m = sd(Age))

head(ref_m)

# Q-06
#  We can actually combine both summaries for exercises 4 and 5 into one line of 
# code. This is because group_by permits us to group by more than one variable. 
# Obtain one big summary table using group_by(AgeDecade, Gender).

ref_age <- NHANES |> group_by(AgeDecade, Gender) |>
  summarize(avg_age = mean(Age), std = sd(Age))

head(ref_age)

View(ref_age)

# For males between the ages of 40-49, compare systolic blood pressure across 
# race as reported in the Race1 variable. Order the resulting table from lowest 
# to highest average systolic blood pressure.

ref_m_race <- NHANES |> filter(Gender == "male", AgeDecade == " 40-49") |>
  group_by(Race1) |>
    arrange((BPSysAve)) 


result <- NHANES |> filter(Gender == "male", AgeDecade == " 40-49") |>
  group_by(Race1) |>
    arrange(AvgSBP)

# Tibbles-----

# Tidy data must be stored in data frames. We introduced the data frame in 
# Section 2.4.1 and have been using the murders data frame throughout the book.
# In Section 4.7.3 we introduced the group_by function, which permits 
# stratifying data before computing summary statistics. But where is the group 
# information stored in the data frame?

murders %>% group_by(region)

# Notice that there are no columns with this information. But, if you look 
# closely at the output above, you see the line A tibble followed by dimensions. 
# We can learn the class of the returned object using:


murders %>% group_by(region) %>% class()


# The tbl, pronounced tibble, is a special kind of data frame. The functions 
# group_by and summarize always return this type of data frame. The group_by 
# function returns a special kind of tbl, the grouped_df. We will say more 
# about these later. For consistency, the dplyr manipulation verbs 
# (select, filter, mutate, and arrange) preserve the class of the input: 
# if they receive a regular data frame they return a regular data frame, 
# while if they receive a tibble they return a tibble. But tibbles are the 
# preferred format in the tidyverse and as a result tidyverse functions that 
# produce a data frame from scratch return a tibble. For example, in Chapter 5 
# we will see that tidyverse functions used to import data create tibbles.

# Tibbles are very similar to data frames. In fact, you can think of them as a 
# modern version of data frames. Nonetheless there are three important 
# differences which we describe next.


# Tibbles display better
# The print method for tibbles is more readable than that of a data frame. 
# To see this, compare the outputs of typing murders and the output of murders 
# if we convert it to a tibble. We can do this using as_tibble(murders). 
# If using RStudio, output for a tibble adjusts to your window size. To see 
# this, change the width of your R console and notice how more/less columns 
# are shown.


# Subsets of tibbles are tibbles
# If you subset the columns of a data frame, you may get back an object that is 
# not a data frame, such as a vector or scalar. For example:
  
class(murders[,4])

# is not a data frame. 


# With tibbles this does not happen:
  
class(as_tibble(murders)[,4])

# This is useful in the tidyverse since functions require data frames as input.

# With tibbles, if you want to access the vector that defines a column, and 
# not get back a data frame, you need to use the accessor $:
  
class(as_tibble(murders)$population)

# A related feature is that tibbles will give you a warning if you try to 
# access a column that does not exist. If we accidentally write Population 
# instead of population this:
  
murders$Population

# returns a NULL with no warning, which can make it harder to debug. 

# In contrast, if we try this with a tibble we get an informative warning:

as_tibble(murders)$Population

# Tibbles can have complex entries
# While data frame columns need to be vectors of numbers, strings, or 
# logical values, tibbles can have more complex objects, such as lists or 
# functions. Also, we can create tibbles with functions:
  
tibble(id = c(1, 2, 3), func = c(mean, median, sd))

# Tibbles can be grouped
# The function group_by returns a special kind of tibble: a grouped tibble. 
# This class stores information that lets you know which rows are in which 
# groups. The tidyverse functions, in particular the summarize function, 
# are aware of the group information.


# Create a tibble using tibble instead of data.frame
# It is sometimes useful for us to create our own data frames. To create a data 
# frame in the tibble format, you can do this by using the tibble function.

grades <- tibble(names = c("John", "Juan", "Jean", "Yao"), 
                 exam_1 = c(95, 80, 90, 85), 
                 exam_2 = c(90, 85, 85, 90))
grades


# Note that base R (without packages loaded) has a function with a very similar 
# name, data.frame, that can be used to create a regular data frame rather than 
# a tibble. One other important difference is that by default data.frame coerces 
# characters into factors without providing a warning or message:


grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90))
grades
class(grades)
class(grades$names)
class(grades$exam_1)

# To avoid this, we use the rather cumbersome argument stringsAsFactors:

grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)
class(grades$names)


# To convert a regular data frame to a tibble, you can use the as_tibble 
# function.

as_tibble(grades) %>% class()



# The placeholder - dot operator
# One of the advantages of using the pipe %>% is that we do not have to keep 
# naming new objects as we manipulate the data frame. 

log(8, base = 2)
2 |> log(8, base = _)
2 %>% log(8, base = .)


# purrr package-----

# In Section 3.5 we learned about the sapply function, which permitted us to 
# apply the same function to each element of a vector. We constructed a function 
# and used sapply to compute the sum of the first n integers for several values 
# of n like this:
  
  
compute_s_n <- function(n){
    x <- 1:n
    sum(x)
  }
n <- 1:25
s_n <- sapply(n, compute_s_n)
s_n
class(s_n)

# This type of operation, applying the same function or procedure to elements 
# of an object, is quite common in data analysis. The purrr package includes 
# functions similar to sapply but that better interact with other tidyverse 
# functions. The main advantage is that we can better control the output type 
# of functions. 

# In contrast, sapply can return several different object types; 
# for example, we might expect a numeric result from a line of code, but sapply 
# might convert our result to character under some circumstances. 

# purrr functions will never do this: they will return objects of a specified 
# type or return an error if this is not possible.

# The first purrr function we will learn is map, which works very similar to 
# sapply but always, without exception, returns a list:
  
require(tidyverse)
library(purrr)
s_n <- map(n, compute_s_n)
class(s_n)

# If we want a numeric vector, we can instead use map_dbl which always returns 
# a vector of numeric values.

s_n <- map_dbl(n, compute_s_n)
class(s_n)


# This produces the same results as the sapply call shown above.

# A particularly useful purrr function for interacting with the rest of the 
# tidyverse is map_df, which always returns a tibble data frame. However, the 
# function being called needs to return a vector or a list with names. For this 
# reason, the following code would result in a Argument 1 must have names error:
  
s_n <- map_df(n, compute_s_n)

# We need to change the function to make this work:
  
compute_s_n <- function(n){
  x <- 1:n
  tibble(sum = sum(x))
}
s_n <- map_df(n, compute_s_n)
class(s_n)

# The purrr package provides much more functionality not covered here. For more 
# details you can consult this online resource.

# Tidyverse conditionals
# A typical data analysis will often involve one or more conditional operations. 
# In Section 3.1 we described the ifelse function, which we will use extensively 
# in this book. In this section we present two dplyr functions that provide further 
# functionality for performing conditional operations.

# case_when
# The case_when function is useful for vectorizing conditional statements. 
# It is similar to ifelse but can output any number of values, as opposed to 
# just TRUE or FALSE. Here is an example splitting numbers into negative, 
# positive, and 0:
  
  
x <- c(-2, -1, 0, 1, 2)
case_when(x < 0 ~ "Negative", 
          x > 0 ~ "Positive", 
          TRUE  ~ "Zero")


# A common use for this function is to define categorical variables based on 
# existing variables. For example, suppose we want to compare the murder rates 
# in four groups of states: New England, West Coast, South, and other. For each 
# state, we need to ask if it is in New England, if it is not we ask if it is 
# in the West Coast, if not we ask if it is in the South, and if not we assign 
# other. Here is how we use case_when to do this:


murders %>% 
  mutate(group = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "New England",
    abb %in% c("WA", "OR", "CA") ~ "West Coast",
    region == "South" ~ "South",
    TRUE ~ "Other")) %>%
  group_by(group) %>%
  summarize(rate = sum(total) / sum(population) * 10^5) 

# between
# A common operation in data analysis is to determine if a value falls inside 
# an interval. We can check this using conditionals. For example, to check if 
# the elements of a vector x are between a and b we can type

x >= a & x <= b

# However, this can become cumbersome, especially within the tidyverse approach. 
# The between function performs the same operation.

between(x, a, b)

















