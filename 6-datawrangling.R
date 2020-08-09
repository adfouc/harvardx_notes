# DATA WRANGLING

## En résumé
# 
# 
# colnames(x) <- paste("x", 1:ncol(x), sep = "_")
# 
# dat %>% separate(key, c("year", "variable_name"), "_")
# df <- data.frame(x = c(NA, "a.b", "a.d", "b.c","e.f.g.h"))
# df %>% separate(x, c("A", "B","C"), fill="right",extra="merge")
# A    B    C
# 1 <NA> <NA> <NA>
#   2    a    b <NA>
#   3    a    d <NA>
#   4    b    c <NA>
#   5    e    f  g.h
#
# wide_data %>% gather(year, fertility, `1960`:`2015`)
# new_tidy_data %>% spread(year, fertility)
#
# gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
# str_split_fixed(string, pattern, n)
# str_detect(text, pattern) # Detect the presence or absence of a pattern in a string. => logical vector
# str_subset(string, pattern) # Keep strings matching a pattern, or find positions.
# str_replace(string, pattern, replacement)

# dl <- tempfile()
# download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
# 
# ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                  col.names = c("userId", "movieId", "rating", "timestamp"))
#
# dplyr joins for dataframes
# left_join(), right_join(), inner_join() 
# full_join() keeps all rows from both tables.
# semi_join() keeps the part of first table for which we have information in the second.
# anti_join() keeps the elements of the first table for which there is no information in the second.
#
# dataframes summaries
# dplyr::count(userId)
# summarize(n_users = n_distinct(userId)
# summarize(n = n(), years = 2018 - first(year), title = title[1])
# top_n(25, rate) %>% arrange(desc(rate))
#   arrange(desc(b_i)) %>% slice(1:10)  %>%   pull(title)
# dataframes manipulations
# bind_rows()
# knitr::kable()
#
# rep(1:4, each = 2)
# [1] 1 1 2 2 3 3 4 4

### Code
# see working directory
getwd()

# change your working directory
setwd()

# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package="dslabs")
list.files(path)

# generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

# copy file from dslabs package to your working directory
file.copy(fullpath, getwd())

# check if the file exists
file.exists(filename)

## readr and readxl packages
##
library(dslabs)
library(tidyverse)    # includes readr
library(readxl)

# inspect the first 3 lines
read_lines("murders.csv", n_max = 3)

# read file in CSV format
dat <- read_csv(filename)

#read using full path
dat <- read_csv(fullpath)
head(dat)

#Ex：
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files

filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat=read.csv(file.path(path, filename))
dat1=read.csv(file.path(path, filename1))
dat2=read.csv(file.path(path, filename2))

# filename is defined in the previous video
# read.csv converts strings to factors
dat2 <- read.csv(filename)
class(dat2$abb)
class(dat2$region)


# ##
# ## dwnld from internet
# The read_csv() function and other import functions can read a URL directly.
# If you want to have a local copy of the file, you can use download.file().
# tempdir() creates a directory with a name that is very unlikely not to be unique.
# tempfile() creates a character string that is likely to be a unique filename.

# Code
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

# data import assessment
library(tidyverse)
url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
wdbc<-read_csv(url,col_names = FALSE)
nrow(wdbc)
ncol(wdbc)

# TIDY DATA
# In tidy data, each row represents an observation and each column represents a different variable.
# In wide data, each row includes several observations and one of the variables is stored in the header.

# Code
library(tidyverse)
library(dslabs)
data(gapminder)

# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

## RESHAPING data
# The tidyr package includes several functions that are useful for tidying data.
# The gather() function converts wide data into tidy data.
# The spread() function converts tidy data to wide data.

# Code
# original wide data
library(tidyverse) 
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

# tidy data from dslabs
library(dslabs)
data("gapminder")
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)

# gather wide data to make new tidy data
new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

# gather all columns except country
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)

# gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)

# convert gathered column names to numeric
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

# ggplot works on new tidy data
new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

## SEPARATE and UNITE
# The separate() function splits one column into two or more columns at a specified character that separates the variables.
# When there is an extra separation in some of the entries, use fill="right" to pad missing values with NAs, or use extra="merge" to keep extra elements together.
# The unite() function combines two columns and adds a separating character.

# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
dat %>% separate(key, c("year", "variable_name"))

# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                 fill = "right")

# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

# separate then spread (Solution 1)
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 

# separate then unite
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")

# full code for tidying data  (Solution 2)
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

##
setwd("../..")
getwd()
d <- read_csv("~/R/datafiles/times.csv")
d
tidy_data<-d %>% gather(year, time, `2015`:`2017`)
tidy_data
tidy_data%>%spread(year,time)

##
d <- read_csv("~/R/datafiles/times2.csv")
d
d%>%gather(key="key",value="value",-age_group)%>% 
  separate(col = key, into = c("year", "variable_name"), sep = "_") %>% 
  spread(key = variable_name, value = value)

##
stats<-data.frame(key=c("allen_height","allen_hand_length","allen_wingspan","bamba_height",
                        "bamba_hand_length","bamba_wingspan"),
                  value=c(75,8.25, 79.25 , 83.25 , 9.75, 94))
stats
stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge") %>% 
  spread(key = variable_name, value = value)
stats %>%
  separate(col = key, into = c("player", "variable_name1", "variable_name2"), sep = "_", fill = "right") %>% 
  unite(col = variable_name, variable_name1, variable_name2, sep = "_") %>% 
  spread(key = variable_name, value = value)
stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_") %>% 
  spread(key = variable_name, value = value)

# assesment part 2 : reshaping data

# co2 is not tidy: to be tidy we would have to wrangle it to have three columns (year, month and value), and then each co2 observation would have a row.
co2

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy<-co2_wide%>%gather(month,co2,-year)
head(co2_tidy)

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

##
library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
dat
dat%>%spread(gender,admitted)
dat_tidy <- spread(dat, gender, admitted)
dat_tidy
tmp <- gather(admissions, key, value, admitted:applicants)
tmp
tmp2<- tmp%>% unite(column_name,c(key,gender))
tmp2
tmp2%>%spread(column_name,value)

## COMBINING TABLES

## The join functions in the dplyr package combine two tables such that matching rows are together.
# left_join() only keeps rows that have information in the first table.
# right_join() only keeps rows that have information in the second table.
# inner_join() only keeps rows that have information in both tables.
# full_join() keeps all rows from both tables.
# semi_join() keeps the part of first table for which we have information in the second.
# anti_join() keeps the elements of the first table for which there is no information in the second.
# 
# only for dataframes!

library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2

# experiment with different joins
left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1, tab2)
semi_join(tab1, tab2)
anti_join(tab1, tab2)

##
## BINDING

# Unlike the join functions, the binding functions do not try to match by a variable, but rather just combine datasets.
# bind_cols() binds two objects by making them columns in a tibble. 
# The R-base function cbind() binds columns but makes a data frame or matrix instead.
# The bind_rows() function is similar but binds rows instead of columns. 
# The R-base function rbind() binds rows but makes a data frame or matrix instead.

bind_cols(a = 1:3, b = 4:6)
bind_cols(a = 1:5, b = 4:6) # error : Argument 2 must be length 5, not 3

tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)
identical(new_tab,tab)

tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)

##
## SET OPERATORS
# 
# By default, the set operators in R-base work on vectors. If tidyverse/dplyr are loaded, they also work on data frames.
# You can take intersections of vectors using intersect(). This returns the elements common to both sets.
# You can take the union of vectors using union(). This returns the elements that are in either set.
# The set difference between a first and second argument can be obtained with setdiff(). Note that this function is not symmetric.
# The function set_equal() tells us if two sets are the same, regardless of the order of elements.

# intersect vectors or data frames
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

# perform a union of vectors or data frames
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

# set difference of vectors or data frames
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)

# setequal determines whether sets have the same elements, regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)

##
##
# The Batting data frame contains the offensive statistics for all baseball players over several seasons.  Filter this data frame to define top as the top 10 home run (HR) hitters in 2016:
#   
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
# Also Inspect the Master data frame, which has demographic information for all players:
Master %>% as_tibble()

# player ID, first name, last name, and number of HR for the top 10 players
top_names <- 
  top%>%left_join(Master)%>%select(playerID,nameFirst,nameLast,HR) # Joining, by = "playerID"

# add a salary column to the top_names data frame
head(Salaries )
top_salary <- 
  Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)


# question 7
awardsID <- AwardsPlayers %>% filter(yearID==2016) %>% select(playerID)

# How many players from the top 10 home run hitters won at least one award in 2016?
top_names %>% select(playerID) %>% intersect(awardsID)%>% nrow()

# ...answer
Awards_2016 <- AwardsPlayers %>% filter(yearID == 2016)
length(intersect(Awards_2016$playerID, top_names$playerID))

# How many players won an award in 2016 but were not one of the top 10 home run hitters in 2016?
awardsID %>% anti_join(top_names)%>% distinct()%>% nrow()

# ...answer => setdiff removes the duplicates ??
length(setdiff(Awards_2016$playerID, top_names$playerID))


##
## WEB SCRAPING

# Web scraping is extracting data from a website.
# The rvest web harvesting package includes functions to extract nodes of an HTML document: html_nodes() extracts all nodes of different types, and html_node() extracts the first node.
# html_table() converts an HTML table to a data frame.


# import a webpage into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

##
## CSS SELECTORS

h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

# You can see how complex the selectors are. In any case we are now ready to extract what we want and create a list:

guacamole <- list(recipe, prep_time, ingredients)
guacamole

# Since recipe pages from this website follow this general layout, we can use this code to create a function that extracts this information:
  
  get_recipe <- function(url){
    h <- read_html(url)
    recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
    prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
    ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
    return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
  } 
# and then use it on any of their webpages:
  
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")

# There are several other powerful tools provided by rvest. 
# For example, the functions html_form(), set_values(), and submit_form() permit you to query a webpage from R. This is a more advanced topic not covered here.

# assesment web scraping
# Load the following web page, which contains information about Major League Baseball payrolls
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")

# The html_nodes() function returns a list of objects of class xml_node. 
# We can see the content of each one using, for example, the html_text() function. 
# You can see the content for an arbitrarily picked component like this:
html_text(nodes[[8]])
# If the content of this object is an html table, we can use the html_table() function to convert it to a data frame:
html_table(nodes[[8]])

html_table(nodes[[3]])
html_table(nodes[[4]])

tab_1<-html_table(nodes[[10]])
tab_2<-html_table(nodes[[19]])
tab_1<-tab_1 %>% select (2:4) %>% rename (Team=X2,Payroll=X3,Average=X4)%>%slice(2:31)
tab_2<-tab_2 %>% rename (Team=X1,Payroll=X2,Average=X3) %>%slice(2:31)
full_join(tab_1,tab_2,by="Team")


##
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
tab <- html_nodes(h, "table")
length(tab) # 39 tables
for  (itab in 1:9){
  val<-html_table(tab[[itab]],fill=TRUE)
  print(itab)
  print(length(val))
  print(names(val[1]))
}


##
## STRING PROCESSING
##

# read in raw murders data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

##
s <- "Hello!"    # double quotes define a string
s <- 'Hello!'    # single quotes define a string
s <- `Hello`    # backquotes do not

s <- "10""    # error - unclosed quotes
s <- '10"'    # correct

# cat shows what the string actually looks like inside R
cat(s)

s <- "5'"
cat(s)

# to include both single and double quotes in string, escape with \
s <- '5'10"'    # error
s <- "5'10""    # error
s <- '5\'10"'    # correct
cat(s)
s <- "5'10\""    # correct
cat(s)

# direct conversion to numeric fails because of commas
murders_raw$population[1:3]
as.numeric(murders_raw$population[1:3])

library(tidyverse)    # includes stringr

# Use the str_detect() function to determine whether a string contains a certain pattern.
# Use the str_replace_all() function to replace all instances of one pattern with another pattern. 
# To remove a pattern, replace with the empty string ("").
# The parse_number() function removes punctuation from strings and converts them to numeric.
# mutate_at() performs the same transformation on the specified column numbers.

# murders_raw was defined in the web scraping section
# detect whether there are commas
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))

# replace commas with the empty string and convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)

# parse_number also removes commas and converts to numeric
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)

murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head

# 2 exemples de conversion des colonnes 2 et 3
dat %>% mutate_at(2:3, parse_number)
dat %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
  mutate_at(2:3, as.numeric)

# Case Study 2: Reported Heights
#
#

# load raw heights data and inspect
library(dslabs)
data(reported_heights)
class(reported_heights$height)

# convert to numeric, inspect, count NAs
x <- as.numeric(reported_heights$height)
head(x,100)
sum(is.na(x))

# keep only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=100)

# calculate cutoffs that cover 99.999% of human population
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)

# keep only entries that either result in NAs or are outside the plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# number of problematic entries
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

str_subset(problems,"^\\d[.,]")
           
# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

## REGEXP

# load stringr through tidyverse
library(tidyverse)

# detect whether a comma is present
pattern <- ","
str_detect(murders_raw$total, pattern) 

# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")

# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")

# highlight the first occurrence of a pattern
install.packages("htmlwidgets")
str_view(s,"cm|inches")

# highlight all instances of a pattern
str_view_all(s, pattern)

##

# s was defined in the previous video
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"

# [56] means 5 or 6
str_view(s, "[56]")

# [4-7] means 4, 5, 6 or 7
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")

# ^ means start of string, $ means end of string
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)

# curly braces define quantifiers: 1 or 2 digits 
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

##
## search and replace

# str_replace() replaces the first instance of the detected pattern with a specified string.
# Spaces are characters and R does not ignore them. Spaces are specified by the special character \\s.
# Additional quantifiers include *, + and ?. 
# * means 0 or more instances of the previous character. 
# ? means 0 or 1 instances. 
# + means 1 or more instances.
# Before removing characters from strings with functions like str_replace() and str_replace_all(), consider whether that replacement would have unintended effects.

# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

# inspect examples of entries with problems
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")

# replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

# R does not ignore whitespace
identical("Hi", "Hi ")

# \\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)%>%cat

# * means 0 or more instances of a character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

# test how *, ? and + differ
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

##
## 
# Groups are defined using parentheses.
# Once we define groups, we can use the function str_match() to extract the values these groups define. 
# str_extract() extracts only strings that match a pattern, not the values defined by groups.
# You can refer to the ith group with \\i. For example, refer to the value in the second group with \\2.

# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

# demonstrate the effect of groups
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)

# demonstrate difference between str_match and str_extract
str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups)

# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

# final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
pattern_with_groups <-"^([4-7])\\s*[,|.|s+]\\s*(\\d*)$"  # semble equivalent

# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% head

str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head

##
## conclusion... testing and improving

# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]    # show problems

pb2<-converted[!index]
str_subset(pb2,"^\\d'.*\\d.*$")%>%cat
str_subset(pb2,"^\\d'\\s*\\d.*$")%>%cat

#
# > schools
# [1] "U. Kentucky"                 "Univ New Hampshire"          "Univ. of Massachusetts"      "University Georgia"         
# [5] "U California"                "California State University"
schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")

##
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

## EXTRACT

# The extract() function behaves similarly to the separate() function but allows extraction of groups from regular expressions.

# first example - normally formatted heights
s <- c("5'10", "6'1")
tab <- data.frame(x = s)

# the separate and extract functions behave similarly
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# second example - some heights with unusual formats
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)

# separate fails because it leaves in extra characters, but extract keeps only the digits because of regex groups
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")  


## Putting it All Together

convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

# Let's start by writing a function that cleans up strings so that all the feet and inches formats use the same x'y format when appropriate.
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

# We can check all the entries we converted using the following code:
  
  new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

# Let's take a look at the shortest students in our dataset using the following code:

new_heights %>% arrange(height) %>% head(n=7)

# We see heights of 53, 54, and 55. In the original heights column, we also have 51 and 52. These short heights are very rare and it is likely that the students actually meant 5'1, 5'2, 5'3, 5'4, and 5'5. But because we are not completely sure, we will leave them as reported

## STRING SPLITTING

# The function str_split() splits a string into a character vector on a delimiter (such as a comma, space or underscore). 
# By default, str_split() generates a list with one element for each original string. 
# Use the function argument simplify=TRUE to have str_split() return a matrix instead.
# The map() function from the purrr package applies the same function to each element of a list. 
# To extract the ith entry of each element x, use map(x, i).
# map() always returns a list. Use map_chr() to return a character vector and map_int() to return an integer.

# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()

# split at commas with str_split function, remove row of column names
x <- str_split(lines, ",") 
x %>% head()
col_names <- x[[1]]
col_names
x <- x[-1]
x %>% head()

# extract first element of each list entry
library(purrr)
# map() applies the same function to each element of a list
map(x, function(y) y[1]) %>% head()
# shortcut 
map(x, 1) %>% head()

# map_chr() to force return a character vector
# extract columns 1-5 as characters, then convert to proper format - NOTE: DIFFERENT FROM VIDEO
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)

dat %>% head

# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 

# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)

##
## Case Study: Extracting a Table from a PDF
# 
# One of the datasets provided in dslabs shows scientific funding rates by gender in the Netherlands:
  
library(dslabs)
data("research_funding_rates")
research_funding_rates 

# Downloading the data
# We start by downloading the PDF document then importing it into R using the following code:
  
install.packages("pdftools")
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)

file.remove(temp_file)

# If we examine the object text we notice that it is a character vector with an entry for each page. So we keep the page we want using the following code:
  
raw_data_research_funding_rates <- txt[2]

# The steps above can actually be skipped because we include the raw data in the dslabs package as well:
  
data("raw_data_research_funding_rates")

# Examining this object,

raw_data_research_funding_rates %>% head

# we see that it is a long string. Each line on the page, including the table rows, is separated by the symbol for newline: \n.
# We can therefore can create a list with the lines of the text as elements:
  
tab <- str_split(raw_data_research_funding_rates, "\n")

# Because we start off with just one element in the string, we end up with a list with just one entry:
  
tab <- tab[[1]]

# By examining this object,

tab %>% head
# we see that the information for the column names is the third and fourth entires:
  
the_names_1 <- tab[3]
the_names_2 <- tab[4]

# In the table, the column information is spread across two lines. 
# We want to create one vector with one name for each column. We can do this using some of the functions we have just learned.

# Let's start with the first line:

the_names_1
# We want to remove the leading space and everything following the comma. We can use regex for the latter. 
# Then we can obtain the elements by splitting using the space. We want to split only when there are 2 or more spaces to avoid splitting success rate. 
# So we use the regex \\s{2,} as follows:

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

# Now let's look at the second line:
  
  the_names_2

# Here we want to trim the leading space and then split by space as we did for the first line:
  
  the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

# Now we can join these to generate one name for each column:
rep(the_names_1, each = 3)
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

# Now we are ready to get the actual data. By examining the tab object, we notice that the information is in lines 6 through 14. 
# We can use str_split() again to achieve our goal:
  
new_research_funding_rates <- tab[6:14] %>%
str_trim %>%
str_split("\\s{2,}", simplify = TRUE) %>%
data.frame(stringsAsFactors = FALSE) %>%
setNames(the_names) %>%
mutate_at(-1, parse_number)

  new_research_funding_rates %>% head()
str(new_research_funding_rates)

# We can see that the objects are identical:
  
identical(research_funding_rates, new_research_funding_rates)

## RECODE
##
# Change long factor names with the recode() function from the tidyverse. 
# Other similar functions include recode_factor() and fct_recoder() in the forcats package in the tidyverse. 
# The same result could be obtained using the case_when() function, but recode() is more efficient to write.

# life expectancy time series for Caribbean countries
library(dslabs)
data("gapminder")
gapminder %>% 
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# display long country names
gapminder %>% 
  filter(region=="Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country) 

# recode long country names and remake plot
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

##
s <- c("5'10", "6'1\"", "5'8inches", "5'7.5")
tab <- data.frame(x = s)
extract(data = tab, col = x, into = c("feet", "inches", "decimal"), 
  regex = "(\\d)'(\\d{1,2})(\\.\\d+)?")

##
str_split("Mandy, Chris and Laura", ",\\s|\\sand\\s")
str_split("Mandy, Chris and Laura", ", | and ", simplify = TRUE)

schedule<-data.frame(day=c("Monday","Tuesday"),staff=c("Mandy, Chris and Laura","Steve, Ruth and Frank"))
schedule
schedule%>%mutate(staff=str_split(staff,", | and "))%>%unnest(cols=c(staff))
separate(schedule, staff, into = c("s1","s2","s3"), sep = ", | and ") %>% gather(key=s,value=staff , -day) %>%
  select(-s)

##
gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short=recode(country, 
                "Central African Republic" = "CAR", 
                "Congo, Dem. Rep." = "DRC",
                "Equatorial Guinea" = "Eq. Guinea")) %>% head()

## Brexit referendum
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)
head(polls)
newpolls<-polls %>% 
  filter(str_detect(Remain, "%"))%>%
  setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")) 
nrow(newpolls)

newpolls%>%mutate(remain_new=as.numeric(str_remove(remain, "%"))/100) %>% select (remain, remain_new)%>%  head(10)
newpolls%>%mutate(remain_new=parse_number(remain)/100) %>% select (remain, remain_new)%>%  head(10)
head(newpolls)
newpolls%>%filter(undecided=="N/A")
str_replace(newpolls$undecided, "N/A", "0")

# regular expression to extract the end day and month from dates. 
temp <- 
  str_extract_all(newpolls$dates, "\\d+\\s[a-zA-Z]+") 
  str_extract_all(newpolls$dates, "[0-9]+\\s[a-zA-Z]+") 
  str_extract_all(newpolls$dates, "\\d{1,2}\\s[a-zA-Z]+") 
  str_extract_all(newpolls$dates, "\\d+\\s[a-zA-Z]{3,5}") 
  
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)


## Dates and Times
##
# Dates are a separate data type in R.The tidyverse includes functionality for dealing with dates through the lubridate package. 
# Extract the year, month and day from a date object with the year(), month() and day() functions.
# Parsers convert strings into dates with the standard YYYY-MM-DD format (ISO 8601 format). Use the parser with the name corresponding to the string format of year, month and day (ymd(), ydm(), myd(), mdy(), dmy(), dym()).
# Get the current time with the Sys.time() function. Use the now() function instead to specify a time zone.
# You can extract values from time objects with the hour(), minute() and second() functions.
# Parsers convert strings into times (for example, hms()). Parsers can also create combined date-time objects (for example, mdy_hms()).

# inspect the startdate column of 2016 polls data, a Date type
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

# ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

# lubridate: the tidyverse date package
library(lubridate)

# select some random dates from polls
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# extract month, day, year from date strings
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))

month(dates, label = TRUE)    # extract month label

# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

now()    # current time in your time zone
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# parse time
x <- c("12:34:56")
hms(x)

#parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)

## Text Mining
##
# The tidytext package helps us convert free form text into a tidy table.
# Use unnest_tokens() to extract individual words and other meaningful chunks of text.
# Sentiment analysis assigns emotions or a positive/negative score to tokens. 
# You can extract sentiments using get_sentiments(). 
#  Common lexicons for sentiment analysis are "bing", "afinn", "nrc" and "loughran".


library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

# In general, we can extract data directly from Twitter using the rtweet package. However, in this case, a group has already compiled data for us and made it available at http://www.trumptwitterarchive.com.

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

# For convenience we include the result of the code above in the dslabs package:
  
library(dslabs)
data("trump_tweets")

# This is data frame with information about the tweet:
  
head(trump_tweets)

# The variables that are included are:
  
names(trump_tweets)

# The help file ?trump_tweets provides details on what each variable represents. The tweets are represented by the text variable:
  
trump_tweets %>% select(text) %>% head

# and the source variable tells us the device that was used to compose and upload each tweet:
  
trump_tweets %>% count(source) %>% arrange(desc(n))

# We can use extract to remove the Twitter for part of the source and filter out retweets.

trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

# We are interested in what happened during the campaign, so for the analysis here we will focus on what was tweeted between the day Trump announced his campaign and election day. So we define the following table:
  
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
         created_at >= ymd("2015-06-17") & 
         created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

# We can now use data visualization to explore the possibility that two different groups were tweeting from these devices. For each tweet, we will extract the hour, in the east coast (EST), it was tweeted then compute the proportion of tweets tweeted at each hour for each device.

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

# We notice a big peak for the Android in early hours of the morning, between 6 and 8 AM. There seems to be a clear different in these patterns. We will therefore assume that two different entities are using these two devices. Now we will study how their tweets differ. To do this we introduce the tidytext package.

# The tidytext package helps us convert free from text into a tidy table. 
# Having the data in this format greatly facilitates data visualization and applying statistical techniques.
#  install.packages("tidytext")
library(tidytext)

# The main function needed to achieve this is unnest_tokens(). 
# A token refers to the units that we are considering to be a data point. 
# The most common tokens will be words, but they can also be single characters, ngrams, sentences, lines or a pattern defined by a regex. 
# The functions will take a vector of strings and extract the tokens so that each one gets a row in the new table. 
# Here is a simple example:
  
example <- data_frame(line = c(1, 2, 3, 4),
                        text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

# Now let's look at a quick example with a tweet number 3008:

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

# Note that the function tries to convert tokens into words and strips characters important to twitter such as # and @. 
# A token in twitter is not the same as in regular English. 
# For this reason, instead of using the default token, words, we define a regex that captures twitter character. 
# The pattern appears complex but all we are defining is a patter that starts with @, # or neither and is followed by any combination of letters or digits:

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
# We can now use the unnest_tokens() function with the regex option and appropriately extract the hashtags and mentions:

campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)
# Another minor adjustment we want to make is remove the links to pictures:

campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)
# Now we are ready to extract the words for all our tweets.

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 
# And we can now answer questions such as "what are the most commonly used words?"

tweet_words %>% 
  count(word) %>%
  arrange(desc(n))
# It is not surprising that these are the top words. The top words are not informative. 
# The tidytext package has database of these commonly used words, referred to as stop words, in text mining:

stop_words
# If we filter out rows representing stop words with filter(!word %in% stop_words$word):

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 
# We end up with a much more informative set of top 10 tweeted words:

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))
# Some exploration of the resulting words (not show here) reveals a couple of unwanted characteristics in our tokens. First, some of our tokens are just numbers (years for example). We want to remove these and we can find them using the regex ^\d+$. Second, some of our tokens come from a quote and they start with '. We want to remove the ' when it's at the start of a word, so we will use str_replace(). We add these two lines to the code above to generate our final table:
  
tweet_words <- campaign_tweets %>% 
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
    unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
    filter(!word %in% stop_words$word &
             !str_detect(word, "^\\d+$")) %>%
    mutate(word = str_replace(word, "^'", ""))
# Now that we have all our words in a table, along with information about what device was used to compose the tweet they came from, 
# we can start exploring which words are more common when comparing Android to iPhone.
  
# For each word we want to know if it is more likely to come from an Android tweet or an iPhone tweet. 
# We previously introduced the odds ratio, a summary statistic useful for quantifying these differences. 
# For each device and a given word, let's call it y, we compute the odds or the ratio between the proportion 
# of words that are y and not y and compute the ratio of those odds. 
# Here we will have many proportions that are 0 so we use the 0.5 correction.

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)
# Given that several of these words are overall low frequency words we can impose a filter based on the total frequency like this:

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)%>%view()
# We already see somewhat of a pattern in the types of words that are being tweeted more in one device versus the other. 
# However, we are not interested in specific words but rather in the tone. 
# Vaziri's assertion is that the Android tweets are more hyperbolic. So how can we check this with data? 
# Hyperbolic is a hard sentiment to extract from words as it relies on interpreting phrases. 
# However, words can be associated to more basic sentiment such as as anger, fear, joy and surprise. 
# In the next section we demonstrate basic sentiment analysis.

# Sentiment Analysis
# In sentiment analysis we assign a word to one or more "sentiment". Although this approach will miss context dependent sentiments, such as sarcasm, when performed on large numbers of words, summaries can provide insights.
# 
# The first step in sentiment analysis is to assign a sentiment to each word. The tidytext package includes several maps or lexicons in the object sentiments:
  
  sentiments 
# There are several lexicons in the tidytext package that give different sentiments. For example, the bing lexicon divides words into positive and negative. We can see this using the tidytext function get_sentiments():
# install.packages("textdata")  

get_sentiments("bing")
# The AFINN lexicon assigns a score between -5 and 5, with -5 the most negative and 5 the most positive.

get_sentiments("afinn")
# The loughran and nrc lexicons provide several different sentiments:
  
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)
# To start learning about how these lexicons were developed, read this help file: ?sentiments.

# For the analysis here we are interested in exploring the different sentiments of each tweet, so we will use the nrc lexicon:
  
nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)
# We can combine the words and sentiments using inner_join(), which will only keep words associated with a sentiment. 
# Here are 10 random words extracted from the tweets:
  
tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)
# Now we are ready to perform a quantitative analysis comparing Android and iPhone by comparing the sentiments of the tweets posted from each device. Here we could perform a tweet by tweet analysis, assigning a sentiment to each tweet. However, this somewhat complex since each tweet will have several sentiments attached to it, one for each word appearing in the lexicon. For illustrative purposes, we will perform a much simpler analysis: we will count and compare the frequencies of each sentiment appears for each device.

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts
# Because more words were used on the Android than on the phone:
  
tweet_words %>% group_by(source) %>% summarize(n = n())
# for each sentiment we can compute the odds of being in the device: proportion of words with sentiment versus proportion of words without and then compute the odds ratio comparing the two devices:
  
sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))
# So we do see some difference and the order is interesting: the largest three sentiments are disgust, anger, and negative! But are they statistically significant? How does this compare if we are just assigning sentiments at random?
  
  # To answer that question we can compute, for each sentiment, an odds ratio and confidence interval. We will add the two values we need to form a two-by-two table and the odds ratio:
  
library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or
# A graphical visualization shows some sentiments that are clearly overrepresented:
  
log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 
# We see that the disgust, anger, negative sadness and fear sentiments are associated with the Android in a way that is hard to explain by chance alone. Words not associated to a sentiment were strongly associated with the iPhone source, which is in agreement with the original claim about hyperbolic tweets.

# If we are interested in exploring which specific words are driving these differences, we can back to our android_iphone_or object:
  
android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))
# We can make a graph:
  
android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
##
##
library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits

data(brexit_polls)
brexit_polls%>%filter(month(startdate)==4)%>%nrow()
brexit_polls%>%mutate(rd=round_date(brexit_polls$enddate,unit="week"))%>%head(10)
brexit_polls%>%filter(round_date(brexit_polls$enddate,unit="week")=="2016-06-12")%>%nrow()
brexit_polls%>%mutate(endday=weekdays(enddate))%>%count(endday)

data(movielens)
movielens%>%mutate(datetime=as_datetime(timestamp),reviewyear=year(datetime))%>% count(reviewyear)%>%arrange(desc(n))
movielens%>%mutate(datetime=as_datetime(timestamp),reviewhour=hour(datetime))%>% count(reviewhour)%>%arrange(desc(n))

movielens%>%filter(str_detect(str_to_upper(title),"HUNGER"))%>%group_by(title)%>%summarize(mean(rating))


##
##
# In this part of the assessment, you will walk through a basic text mining and sentiment analysis task.
# 
# Project Gutenberg is a digital archive of public domain books. The R package gutenbergr facilitates the importation of these texts into R. We will combine this with the tidyverse and tidytext libraries to practice text mining.
# 
# Use these libraries and options:
#  install.packages("gutenbergr")
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
# You can see the books and documents available in gutenbergr like this:
  
gutenberg_metadata

gutenberg_metadata %>%  filter(str_detect(title,"Pride and Prejudice"))%>%  nrow()
gutenberg_works(title=="Pride and Prejudice")
book<-gutenberg_download(1342)  
head(book)
nrow(book)
words<-book%>%unnest_tokens(word,text)
nrow(words)
words<-book%>%unnest_tokens(word,text)%>%filter(!word %in% stop_words$word)
nrow(words)
words<-words%>%filter( ! str_detect(word,".*\\d+.*"))
nrow(words)
words%>%count(word)%>%filter(n>=100)%>%arrange(desc(n))%>%view()
words%>%count(word)%>%filter(n>=100)%>%nrow()

afinn <- get_sentiments("afinn")
afinn_sentiments <- words%>%inner_join(afinn,by="word")
nrow(afinn_sentiments)
view(afinn_sentiments)
afinn_sentiments%>%filter(value>0)%>%nrow() / nrow(afinn_sentiments)
afinn_sentiments%>%filter(value==4)%>%nrow()

gutenberg_works(str_detect(author,"Fitzgerald"),rights=NULL)%>%view()

## Comprehensive Assessment: Puerto Rico Hurricane Mortality 
##

library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
#mac
system2("open", args = fn)
# Windows
system("cmd.exe", input = paste("start", fn))

txt <- pdf_text(fn)
str(txt)
x<-str_split(txt[9],"\n")
x
str(x)
length(x[[1]])
s<-x[[1]]
str(s)
class(s)
s<-str_trim(s)
s[1]
# détecter la 1ere ligne contenant 2015
header_index <- str_which(s,"2015")[1]
s[header_index]
tmp<-str_split(s[header_index],"\\s+",simplify = TRUE)
tmp
month<-tmp[1]
header<-tmp[-1]
month
header
#
tail_index<-str_which(s,"Total")
tail_index
s[tail_index]
#
str_count(s,"\\d+")
numsingledigits<-sum(str_count(s,"\\d+")==1)
length(s)-header_index - (length(s)-tail_index+1)-numsingledigits
# detecter les lignes avec digits et les compter
n <- str_count(s, "\\d+")
sum(n == 1)
which(n==1)
out <- c(1:header_index, which(n==1), tail_index:length(s))
out
s <- s[-out]
s
length(s)
# remove non digits and non space chars
s <- str_remove_all(s, "[^\\d\\s]")
# onvert s into a data matrix with just the day and death count data:
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
# 
tab <- data.frame(s)%>% setNames( c("day",header)) %>%    mutate(month=month)
# attention ceci corrompt les données. En fait des factors à convertir en character puis en numeric !
# tab<-  tab %>% mutate_all(as.numeric)  
tab<-tab %>% mutate_at(2:5, as.character) %>% mutate_at(2:5,parse_number)
str(tab)
tab$'2015'
mean(tab$'2015')
mean(tab$'2016')
# What was the mean number of deaths per day from September 1-19, 2017, before the hurricane hit?
mean(tab$'2017'[1:19])
mean(tab$'2017'[20:30])

# la reponse : AS_DATA_FRAME convertit direct en NUMERIC contrairement a DATA.FRAME()
tab <- s %>% 
  as_data_frame() %>% 
  setNames(c("day", header)) %>%
  mutate_all(as.numeric)
mean(tab$"2015")
str(tab)
#
tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab
# Make a plot of deaths versus day with color to denote year. Exclude 2018 since we have no data. Add a vertical line at day 20, the day that Hurricane María hit in 2017.
tab%>% filter(year!=2018) %>% ggplot(aes(day,deaths,colour=year)) + geom_line()+ geom_vline(xintercept = 20)
