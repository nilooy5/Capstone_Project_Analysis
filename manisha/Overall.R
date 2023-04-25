# setwd("D:/University/Sem_3/Capstone/Data/kanishka")
library(dplyr)
library(ggplot2)

library(tidyverse)
df <- read_csv("manisha/DeathsByState.csv")

df1 <- df %>% gather(State, Rate, NSW:Total)



ggplot(df1, aes(x=State, y= Rate, fill=as.factor(Year)))+
  geom_bar(stat = "identity", position = position_dodge())+ 
  facet_grid(Variable ~ .)+ theme_bw() + 
  labs(title = "Proportion of Perinatal, stillbirth and neonatal rate per 1000 live births by State", y="Rate (%)")+
  theme_classic()


# Death by Age

df <- read_csv("manisha/DeathsbyAge.csv")


ggplot(df, aes(x=Year, y= Ratio, fill=Maternal_Age))+
  geom_bar(stat = "identity", position = position_dodge())+ 
  facet_grid(Child_mortality ~ .)+ theme_bw() + 
  labs(title = "Proportion of Perinatal, stillbirth and neonatal rate per 1000 live births by Age Catergory", y="Rate (%)")+
  theme_classic()
