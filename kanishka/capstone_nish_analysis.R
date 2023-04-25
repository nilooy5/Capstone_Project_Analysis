# setwd("replace with the path of your working directory")

# data files - data files located in the same folder as this R script. 

# Check and install the required packages---------------------------------------

if(!("readr" %in% rownames(installed.packages()))){
  install.packages("readr")
}

if(!("tidyverse" %in% rownames(installed.packages()))){
  install.packages("tidyverse")
}

if(!("GGally" %in% rownames(installed.packages()))){
  install.packages("GGally")
}

if(!("gridExtra" %in% rownames(installed.packages()))){
  install.packages("gridExtra")
}

if(!("modelr" %in% rownames(installed.packages()))){
  install.packages("modelr")
}

install.packages("readxl")
library(readxl)

library(readr)     # Data import
library(tidyverse) # Data wrangle and visualization
library(ggplot2)   # Data Visualization
library(GGally)    # Correlation and plotting  
library(gridExtra) # Plot arrangement 
library(Metrics)   # Data modeling and evaluation
library(modelr)

my_theme <- theme(
  panel.background = element_rect( fill = "ivory2" , color ="gray"),
  plot.background = element_rect( color = "black"),
  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
  axis.title.x = element_text(size = 12, hjust = 0.5),
  axis.title.y = element_text(size = 12, hjust = 0.5))

df1 <- read_excel("kanishka/Birthpermonth.xlsx")
df2 <- read_excel("kanishka/DeathsByAge.xlsx")
df3 <- read_excel("kanishka/StillBirthTimeSeries.xlsx")
df4 <- read_excel("kanishka/DeathsByState.xlsx")
df5 <- read_excel("kanishka/maternal_risk_factors_2020.xlsx")

# ************************** Data Pre-processing *******************************

colSums(is.na(df1))

colSums(is.na(df5))

df5 <- subset(df5, select = -c(Rate_Perinatal, Rate_neonatal, Rate_stillbirth, Code))


# ************************** EDA ***********************************************


# ************************** 1 ***********************************************

days_in_month <- c(January = 31, February = 28, March = 31, April = 30, May = 31, June = 30, 
                   July = 31, August = 31, September = 30, October = 31, November = 30, December = 31)

df1$days <- days_in_month[df1$Month]


df1 %>% 
  mutate (BirthperDays = Australia/days) %>%
  group_by(Month) %>%
  summarize(Birth = median(BirthperDays)) %>% 
  ggplot(aes(factor(Month, levels = c("January", "February", "March", "April"
                                      , "May", "June", "July", "August"
                                      , "September", "October", "November", "December"
  )), y = Birth, group = 1)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Month",
    y = "Average babies born in a day",
    title = "Avg births in a day vs the Month"
  ) +
  my_theme
  
# ************************** 2 ***********************************************

names(df2) <- c("Age", "Fetal", "Neonatal", "Perinatal", "Year")

df2 %>%
  group_by(Age) %>%
  summarize(Fetal_deaths = median(Fetal),
            Neonatal_deaths = median(Neonatal),
            Perinatal_deaths = median(Perinatal)) %>%
  gather("Variable", "Value", -Age) %>%
  ggplot(aes(factor(Age, levels = c("Younger than 20",
                                    "20–24",
                                    "25–29",
                                    "30–34",
                                    "35–39",
                                    "40 and over"))
             , y = Value, color = Variable, group = Variable)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Mother's age",
    y = "Proportion of mortalities",
    title = "Mortality proportions vs age of mother"
  ) +
  my_theme

# ************************** 3 ***********************************************

names(df3) <- c( "GroupedBy"    ,
                 "Description"   ,
                 "Year"   ,
                 "TotalBirths",
                 "LiveBirths"   ,
                 "PerinatalDeaths" ,
                 "Stillbirths"  , 
                 "NeonatalDeaths" )

df3 <- df3[df3$Year != 2020,]

df3 %>%
  mutate(still = Stillbirths/TotalBirths) %>%
  ggplot(aes(x = Year, y = still)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Proportion of Stillbirths",
    title = "Rate of Stillbirth vs Year"
  ) +
  my_theme

df3 %>%
  mutate (neonatal = NeonatalDeaths/TotalBirths) %>%
  ggplot(aes(x = Year, y = neonatal)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Proportion of Neonatal Deaths",
    title = "Rate of Neonatal deaths vs Year"
  ) +
  my_theme

df3 %>%
  mutate (perinatal = PerinatalDeaths/TotalBirths) %>%
  ggplot(aes(x = Year, y = perinatal)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Proportion of Perinatal Deaths",
    title = "Rate of Perinatal deaths vs Year"
  ) +
  my_theme

# ************************** 4 ***********************************************

df_sum <- df4 %>%
  group_by(Category) %>%
  summarise_at(vars(-Year), sum)

df_prop <- apply(df_sum[, -which(names(df_sum) == "Category")], 2, function(x) x/sum(x))

df_sum <- cbind(df_sum[, "Category"], df_prop)


plot4 <- subset(df_sum, Category != "Live births") %>%
  gather("Variable", "Value", -Category) %>%
  ggplot(aes(x = Variable, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(
    x = "State",
    y = "Proportion of Deaths",
    title = "Proportion of Deaths vs State"
  ) +
  my_theme


plot4

# ************************** 5 ***********************************************

# function to replace missing values with median
replace_with_median <- function(x) {
  if(is.numeric(x)) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
  }
  x
}

df5_new <- df5 %>%
  mutate_if(is.numeric, replace_with_median)

# socio-economic status ********************************************************

# processing

df5_1 <- subset(df5_new, Disaggregation == "Socioeconomic area of mother's usual residence")
df5_1 <- subset(df5_1, Description != "Not stated")
df5_1 <- subset(df5_1, select = -c(Disaggregation))

df5_1 <- df5_1 %>%
  gather("Variable", "Value", -c(Description, 'Total births'))
df5_1 <- subset(df5_1, Variable != "Live births")

# plot 

df5_1 %>%
  mutate(prop = Value/`Total births`) %>%
  ggplot(aes(x = Description, y = prop, color = Variable, group = Variable)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Socio-Economic Area of Mother's residence",
    y = "Proportion of mortalities",
    title = "Mortality proportions vs Residential Status"
  ) +
  my_theme
  
# Parity ***********************************************************************
  
# processing 

df5_2 <- subset(df5_new, Disaggregation == "Parity—number of previous pregnancies")
df5_2 <- subset(df5_2, select = -c(Disaggregation))
df5_2 <- subset(df5_2, Description != "Not stated")

df5_2 <- df5_2 %>%
  gather("Variable", "Value", -c(Description, 'Total births'))
df5_2 <- subset(df5_2, Variable != "Live births")

# plot

df5_2 %>%
  mutate(prop = Value/`Total births`) %>%
  ggplot(aes(x = Description, y = prop, color = Variable, group = Variable)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Parity (No. of births given before this birth) ",
    y = "Proportion of mortalities",
    title = "Mortality proportions vs Parity"
  ) +
  my_theme

# Remoteness of Mother's residence**********************************************

# processing 

df5_3 <- subset(df5_new, Disaggregation == "Remoteness area of mother's usual residence")
df5_3 <- subset(df5_3, select = -c(Disaggregation))
df5_3 <- subset(df5_3, Description != "Not stated")

df5_3 <- df5_3 %>%
  gather("Variable", "Value", -c(Description, 'Total births'))
df5_3 <- subset(df5_3, Variable != "Live births")

# plot

df5_3 %>%
  mutate(prop = Value/`Total births`) %>%
  ggplot(aes(x = factor(Description, levels = c("Very remote",
                                        "Remote",
                                        "Inner regional",
                                        "Outer regional",
                                        "Major cities"))
             , y = prop, color = Variable, group = Variable)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Remoteness of Mother's residence ",
    y = "Proportion of mortalities",
    title = "Mortality proportions vs Mother's residence"
  ) +
  my_theme

# Smoking***********************************************************************

# processing

df5_4 <- subset(df5_new, Disaggregation == "Smoking status")
df5_4 <- subset(df5_4, select = -c(Disaggregation))
df5_4 <- subset(df5_4, Description != "Not stated")

df5_4 <- df5_4 %>%
  gather("Variable", "Value", -c(Description, 'Total births'))
df5_4 <- subset(df5_4, Variable != "Live births")

# plot

df5_4 %>%
  mutate(prop = Value/`Total births`) %>%
  ggplot(aes(x = Description, y = prop, fill = Variable)) +
  geom_bar(stat = "identity", color = "gray") +
  labs(
    x = "Mother's smoking status",
    y = "Proportion of mortalities",
    title = "Mortality proportions vs Smoking status"
  ) +
  my_theme




