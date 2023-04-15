library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
#NOTE: make sure to set the working directory to R
raw = read_csv("../data/lab_sodra.csv")
summary(raw)
 
#data filter for specific code

data=raw %>%
  filter(ecoActCode == 560000)

#PIRMA uzduotis

data%>%
  ggplot(aes(x=avgWage))+
  geom_histogram(bins = 90)+
  scale_x_continuous(labels = scales::number_format(scale = 1e-00))+
  labs(title = "Avarage wage histogram")

#ANTRA uzduotis

data1 = data%>%
  mutate(year=as.numeric(substr(month, 1,4)),
         month=as.numeric(substr(month,5,6)))

top5=data1%>%
  group_by(name)%>%
  summarise(avgWage = mean(avgWage)) %>%
  arrange(desc(avgWage)) %>%
  select(name)%>%
  head(5)

data2 = data1%>%
  filter(name %in% top5$name)

data2%>%
  ggplot(aes(x=month, y=avgWage, group=name, color = name))+
  geom_line()+
  theme_bw()+
  scale_x_continuous(breaks = 1:12, labels = month.name)+
  labs(title="Average wages of the best 5 companies", x= "Month", y="Average Wage")

#TRECIA uzduotis

data2%>%
  ggplot(aes(x = reorder(str_wrap(name, width = 15), -numInsured), y=numInsured, fill= name ))+
  geom_bar(stat = "identity")+
  labs(title = "The number of insured people in the top 5 companies per year", 
       x = "Company name", y = "Total number of insured people")
