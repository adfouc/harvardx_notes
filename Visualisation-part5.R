




# -----------------------------------------------
# ease comparison
color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
  ggplot(aes(x, y, color = col)) +
  geom_point(size = 5)
p1 + scale_color_manual(values = color_blind_friendly_cols)


# -----------------------------------------------
# Code: Show the data

# dot plot showing the data
heights %>% ggplot(aes(sex, height)) + geom_point()
# jittered, alpha blended point plot
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)

# -----------------------------------------------
# Code: Slope chart
library(tidyverse)
library(dslabs)
data(gapminder)
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)
dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"), location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 

# Code: Bland-Altman plot
library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

#-----------------------------------------
# Code: Tile plot of measles rate by year and state

# sequential and divergent palettes
library(RColorBrewer)
display.brewer.all(type="seq")
display.brewer.all(type="div")


# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)
# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))
# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")
# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")

# Code: Line plot of measles rate by year and state
# compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)
# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")

# -----------------------------------------------
# Titanic exercise
install.packages("titanic")
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic%>% filter(!is.na(Age))%>%
  ggplot(aes(Age,fill=Sex))+geom_density(position="stack",stat="count")

titanic%>% filter(!is.na(Age))%>%
  ggplot(aes(Age,fill=Sex))+geom_density( alpha=0.2)

titanic%>% filter(!is.na(Age))%>%
  mutate(tranche =  case_when (Age<18~"Under18", Age>35 ~"Above35", TRUE ~"18-35")) %>%  
  ggplot(aes(tranche,fill=Sex))+geom_histogram(position="stack",stat="count")

params<-titanic%>%filter(!is.na(Age))%>% summarise(mean=mean(Age),sd=sd(Age))
titanic%>% ggplot(aes(sample=Age))+geom_qq(dparams = params)+geom_abline(slope=1,intercept = 0)

titanic%>%ggplot(aes(Survived,fill=Sex))+geom_bar()
titanic%>%ggplot(aes(Sex, fill=Survived))+geom_bar()

titanic%>% filter(!is.na(Age))%>%
  ggplot(aes(Age,fill=Survived))+geom_density(alpha=0.2,stat="count")
titanic%>% filter(!is.na(Age))%>%
  ggplot(aes(Age,fill=Survived))+geom_density(alpha=0.2)

titanic%>%filter(!is.na(Fare)&Fare!=0)%>%
  ggplot(aes(x=Survived,y=Fare))+geom_boxplot()+geom_jitter(width = 0.2, alpha = 0.2)+scale_y_continuous(trans="log2")

titanic%>%filter(!is.na(Pclass))%>%
  ggplot(aes(x=Pclass,fill=Survived))+geom_bar()
titanic%>%filter(!is.na(Pclass))%>%
  ggplot(aes(x=Pclass,fill=Survived))+geom_bar(position=position_fill())
titanic%>%filter(!is.na(Pclass))%>%
  ggplot(aes(x=Survived,fill=Pclass))+geom_bar(position=position_fill())

titanic%>% filter(!is.na(Age))%>%
  ggplot(aes(Age,fill=Survived))+geom_density(alpha=0.2,stat="count")+
  facet_grid(Pclass~Sex)

# -----------------------------------------------
# Properties of stars exercise
#update.packages()


library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)
mean(stars$magnitude)
sd(stars$magnitude)
stars%>%ggplot(aes(magnitude))+geom_density()
stars%>%ggplot(aes(temp))+geom_density()+scale_x_log10()
stars%>%ggplot(aes(temp))+stat_ecdf()
stars%>%ggplot(aes(temp,magnitude))+geom_point()
stars%>%ggplot(aes(log10(temp),magnitude))+
  geom_point()+
  scale_y_reverse()+
  scale_x_reverse()

library(ggrepel)
stars%>%ggplot(aes(log10(temp),magnitude,label=star))+
  geom_point()+
  scale_y_reverse()+
  scale_x_reverse()+geom_text_repel()+geom_vline(aes(xintercept=log10(5000)),color="red")

stars%>%ggplot(aes(log10(temp),magnitude,label=star,color=type))+
  geom_point()+
  scale_y_reverse()+
  scale_x_reverse()+geom_vline(aes(xintercept=log10(5000)),color="red")

# -----------------------------------------------
# climate change
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data("historic_co2")

temp_carbon%>%filter(!is.na(carbon_emissions))%>%.$year %>%max()
temp_carbon%>%filter(year%in%c(2014,1751))
temp_carbon%>%filter(!is.na(temp_anomaly))%>%summarize(min(year),max(year))
temp_carbon%>%filter(year%in%c(2018,1880))
p<-temp_carbon%>%filter(!is.na(temp_anomaly))%>%ggplot(aes(year,temp_anomaly))+geom_line()
p<-p+geom_hline(aes(yintercept=0),col="blue")
p<-p+ylab("Temperature anomaly (degrees C)")+ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")
#p+geom_text(data=data.frame(text="20th century mean",x=2000,y=0.05),aes(x=x,y=y,label=text,color="blue"))
p<-p+geom_text(aes(x=2000,y=0.05,label="20th century mean"),color="blue")
p+geom_line(aes(year,ocean_anomaly),col="red")+
  geom_line(aes(year,land_anomaly),col="green")

greenhouse_gases%>%ggplot(aes(year,concentration))+ geom_line()+facet_grid(gas~.,scales="free")+
  geom_vline(aes(xintercept=1850))+ylab("Concentration (ch4/no2 ppb, co2 ppm)")+
  ggtitle("Atmospheric greenhouse gas concentration per year, 0-2000")

temp_carbon%>%filter(!is.na(carbon_emissions))%>%ggplot(aes(year,carbon_emissions))+geom_line()

co2_time<-
  historic_co2%>%mutate(source=factor(source))%>%filter(!is.na(co2))%>%ggplot(aes(year,co2,color=source))+geom_line()
co2_time+xlim(-800000,-775000)
co2_time+xlim(-375000,-330000)
co2_time+xlim(-140000,-120000)
co2_time+xlim(-3000,2018)

######################################################################@@
##
##  EXEMPLES DE PLOTS
##
##
######################################################################@@

######################################################################@@
## BOXPLOT ##

data.frame(pc = pc$x[,7], tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc, tissue)) +
  geom_boxplot()

for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

titanic%>%filter(!is.na(Fare)&Fare!=0)%>%
  ggplot(aes(x=Survived,y=Fare))+geom_boxplot()+geom_jitter(width = 0.2, alpha = 0.2)+scale_y_continuous(trans="log2")

qplot(year, n, data = ., geom = "boxplot") 

######################################################################@@
## SCATTERPLOT  + Nuage de points

library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

######################################################################@@
## Faceting

filter(gapminder, year%in%c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent~year)

years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>% 
  filter(year %in% years & continent %in% continents) %>%
  ggplot( aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year) 

# variante
facet_wrap(. ~ year, scales = "free")

library(gridExtra)
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)


######################################################################@@
## BARS
# https://ggplot2.tidyverse.org/reference/geom_bar.html
# 

# by default, count the elements in a given category x or y

ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major)

# to show values y given x:  geom_col()

######################################################################@@
## Basic plots

plot(x, y)

hist(x)

boxplot(rate~region, data = murders)

x <- matrix(1:120, 12, 10)
image(x)

qplot(year, n, data = ., geom = "boxplot") 

######################################################################@@
## Axes, legendes, Echelles SCALES

coord_trans(y = "sqrt") 
scale_y_sqrt()

# orientation axe 
 + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# titres et legendes

  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

# ajustement
library(ggrepel)
...
  geom_point() +
  geom_text_repel() +

# show.legend = FALSE
ggplot(aes(year, life_expectancy, group = country)) +
    geom_line(aes(color = country), show.legend = FALSE) +
    geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
    xlab("") +
    ylab("Life Expectancy")     
  + ggtitle("title ")