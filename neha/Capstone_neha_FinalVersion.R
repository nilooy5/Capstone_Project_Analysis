# Load necessary libraries
install.packages("readxl")
install.packages("tidyverse")
install.packages("janitor")
library(janitor)
library(readxl)
library(tidyverse)

path = "neha/National-Perinatal-Data-Collection-data-visualisations-2018-data-tables.xlsx"


# Function to read and clean a single sheet from the Excel file
read_sheet <- function(sheet_name) {
  data <- read_excel(path, sheet = sheet_name)
  
  # Find the row containing 'Chapter'
  start_row <- which(data[,1] == "Chapter", arr.ind = TRUE)[1]
  
  # Keep only the rows starting from the header_row
  clean_data <- data[start_row:nrow(data), ]
  
  # Set the 'Chapter' row values as column names
  colnames(clean_data) <- as.character(clean_data[1,])
  clean_data <- clean_data[-1,] # Remove the 'Chapter' row
  
  # Remove empty rows and columns using dplyr
  clean_data <- clean_data %>%
    filter_all(any_vars(!is.na(.))) %>%
    select_if(~any(!is.na(.)))
  
  return(clean_data)
}

# Read relevant tables from Excel file
Maternal_age <- read_sheet("Table 1.2") 
Geography <- read_sheet("Table 1.3") 
Antenatal_visits <- read_sheet("Table 2.1")
Smoking <- read_sheet("Table 2.3") 
BMI <- read_sheet("Table 2.4") 
Maternal_medical_conditions <- read_sheet("Table 2.5")
Gestational_age <- read_sheet("Table 4.1") 
Birthweight <- read_sheet("Table 4.2") 
Birthweight_adjusted_for_gestational_age <- read_sheet("Table 4.3") 
Apgar_score_at_5_minutes <- read_sheet("Table 4.4")
Admission_to_special_care_nursery_or_neonatal_intensive_care_unit <- read_sheet("Table 4.7")

##########################################################################
# Load the required libraries
library(dplyr)
library(ggplot2)

clean_colnames <- function(df) {
  colnames(df) <- gsub(" ", "_", colnames(df))
  colnames(df) <- gsub("[^a-zA-Z0-9_]", "", colnames(df))
  return(df)
}


# Clean up the data
Admission_to_special_care_nursery_or_neonatal_intensive_care_unit_clean <- clean_colnames(Admission_to_special_care_nursery_or_neonatal_intensive_care_unit)
Admission_to_special_care_nursery_or_neonatal_intensive_care_unit_clean %>% 
  filter(!is.na(Chapter))

# Use the cleaned Admission_to_special_care_nursery_or_neonatal_intensive_care_unit dataframe
df <- Admission_to_special_care_nursery_or_neonatal_intensive_care_unit_clean

# Subset the dataframe
df_subset <- df %>%
  select(Chapter, Subgroup, Subgroup_disaggregation, Topic, Topic_disaggregation, CurrentTrend, Year, Numerator, Denominator, Per_cent)

df_subset$Year <- as.numeric(as.character(df_subset$Year), na.rm = TRUE)
# Replace non-numeric characters with NA
df_subset$Numerator <- ifelse(grepl("^\\d+$", df_subset$Numerator), df_subset$Numerator, NA)

# Convert the Numerator column to numeric
df_subset$Numerator <- as.numeric(as.character(df_subset$Numerator))
df_subset$Denominator <- as.numeric(as.character(df_subset$Denominator))
df_subset$Per_cent <- as.numeric(as.character(df_subset$Per_cent))

df_subset <-  df_subset %>% 
  drop_na()
  



total_admissions_by_year <- df_subset
total_admissions_by_year <- subset(total_admissions_by_year, Subgroup_disaggregation %in% "Admitted")

###################################################### Smoking ###############################################################
# Define a list of topic disaggregations to include
smoking_Status <- c("Did not smoke","Smoked")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
smoked <- subset(total_admissions_by_year, Topic_disaggregation %in% c(smoking_Status,"Smoking status (any time during pregnancy)"))

library(ggplot2)

smoked <- subset(smoked, Subgroup_disaggregation != "Total")
# Create the bar graph
ggplot(smoked, aes(x = Topic_disaggregation, y = Per_cent, fill =)) +
  geom_bar(stat = "identity", position = "dodge") +

  # Add chart titles and labels
  labs(title = "Admission to SCN/NICU by Smoking Status During Pregnancy",
       x = "Smoking Status During Pregnancy",
       y = "Percentage",
       fill = "Admitted") +

  # Customize the theme
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))








#################################### Indigenous  ################################################


# Define a list of topic disaggregations to include
indigenous <- c("Non-Indigenous","Not stated","Indigenous")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
df_indigenous <- subset(total_admissions_by_year, Topic %in% c(indigenous,"Indigenous status (mother)") & !Topic_disaggregation == "Total")


# Create a line plot
ggplot(df_indigenous, aes(x = Year, y = Per_cent, group = Topic_disaggregation, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Total Admissions %", title = "Total Admissions by Year and  Indigenous status") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))




#######################################3


# Define a list of topic disaggregations to include
df_age_range <- c("Less than 20", "20-24", "25-29","30-34","35-39","40 and over")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
df_mother_age <- subset(total_admissions_by_year, Topic_disaggregation %in% c(df_age_range,"Maternal age"))


# Create a line plot
ggplot(df_mother_age, aes(x = Year, y = Per_cent, group = Topic_disaggregation, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Total Admissions %", title = "Total Admissions by Year and Mothers Age") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))

##################################### Gender

# Define a list of topic disaggregations to include
gender_list <- c("Male","Female")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
df_child_gender <- subset(total_admissions_by_year, Topic_disaggregation %in% c(gender_list,"Sex"))


# Create a line plot
ggplot(df_child_gender, aes(x = Year, y = Per_cent, group = Topic_disaggregation, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Total Admissions %", title = "Total Admissions by Year and Gender") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))

###################################### State

# Define a list of topic disaggregations to include
state_list <- c("SA","TAS","ACT","Qld","Vic","NT")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
df_state <- subset(total_admissions_by_year, Topic_disaggregation %in% c(state_list,"State and territory of birth"))


# Create a line plot
ggplot(df_state, aes(x = Year, y = Per_cent, group = Topic_disaggregation, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Total Admissions %", title = "Total Admissions by Year and State") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))


##########################################  Birth weight

# Define a list of topic disaggregations to include
birth_weight <- c("Low birthweight","Normal birthweight","High birthweight")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
df_baby_weight <- subset(total_admissions_by_year, Topic_disaggregation %in% c(birth_weight,"Birthweight"))


# Create a line plot
ggplot(df_baby_weight, aes(x = Year, y = Per_cent, group = Topic_disaggregation, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Total Admissions %", title = "Total Admissions by Year and Birth weight") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))



########################################## Body mass index

# Define a list of topic disaggregations to include
bmi <- c("Underweight","Normal weight","Overweight")


# Subset the data frame to include only rows where the topic disaggregation is in the included list
df_baby_bmi <- subset(total_admissions_by_year, Topic_disaggregation %in% c(bmi,"Body mass index"))


# Create a line plot
ggplot(df_baby_bmi, aes(x = Year, y = Per_cent, group = Topic_disaggregation, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Total Admissions %", title = "Total Admissions by Year and Body mass index") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))



########################################## Gestational age

# Define a list of topic disaggregations to include
gestational_age <- c("Pre-term","Post-term","Term")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
df_mothers_gestational_age <- subset(total_admissions_by_year, Topic_disaggregation %in% c(gestational_age,"Gestational age"))


# Create line plot
ggplot(df_mothers_gestational_age, aes(x = Year, y = Per_cent, group = Topic_disaggregation, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Percentage", title = "Line Plot for Trend and Current Data") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_shape_manual(values = c(16, 17)) # Use different shapes for Trend and Current



########################################## Socioeconomic status

# Define a list of topic disaggregations to include
economic_status <- c("Q1 (most disadvantaged)","Q2","Q3","Q4","Q5 (least disadvantaged)")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
df_economic_status <- subset(total_admissions_by_year, Topic_disaggregation %in% c(economic_status,"Socioeconomic status"))


# Create a line plot
ggplot(df_economic_status, aes(x = Year, y = Per_cent, group = Topic_disaggregation, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Total Admissions %", title = "Total Admissions by Year and  Economics status") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))

###################################################### Mode of Birth ###############################################################
# Define a list of topic disaggregations to include
mode_of_birth <- c("Vaginal (non-instrumental)","Vaginal (forceps)","Vaginal (vacuum)","Caesarean section")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
df_mode_birth <- subset(total_admissions_by_year, Topic_disaggregation %in% c(mode_of_birth,"Method of birth"))


# Create a line plot
ggplot(df_mode_birth, aes(x = Year, y = Per_cent, group = Topic_disaggregation, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Total Admissions %", title = "Total Admissions by Year and  Mode of Birth") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))




#######################################################################################################################
###################################################### Smoking ###############################################################
# Define a list of topic disaggregations to include
smoking_Status <- c("Did not smoke","Smoked")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
smoked <- subset(total_admissions_by_year, Topic_disaggregation %in% c(smoking_Status,"Smoking status (any time during pregnancy)"))


# Create a line plot
ggplot(smoked, aes(x = Year, y = Per_cent, group = Topic_disaggregation, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Total Admissions %", title = "Total Admissions by Year and  smoking") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))


##################################################### Country if birth ###############################################################
# Define a list of topic disaggregations to include
Birth_country <- c("Born in Australia","Born overseas")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
birth_country <- subset(total_admissions_by_year, Topic_disaggregation %in% c(Birth_country,"Country of birth (mother)"))


# Create a line plot
ggplot(birth_country, aes(x = Year, y = Per_cent, group = Topic_disaggregation, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Total Admissions %", title = "Total Admissions by Year and  Country of Birth") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))


##################################################### Plularity ###############################################################
# Define a list of topic disaggregations to include
plurality <- c("Singleton","Other multiples","Twins")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
plural_df <- subset(total_admissions_by_year, Topic_disaggregation %in% c(plurality,"Plurality"))


# Create a line plot
ggplot(plural_df, aes(x = Year, y = reorder(factor(Per_cent), Per_cent), group = Topic_disaggregation, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Total Admissions %", title = "Total Admissions by Year and Plurality") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))



##################################################### Remoteness ###############################################################
# Define a list of topic disaggregations to include
remoteness <- c("Inner regional","Major cities","Outer regional","Remote")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
remote_df <- subset(total_admissions_by_year, Topic_disaggregation %in% c(remoteness,"Remoteness area"))


# Create a line plot
ggplot(remote_df, aes(x = Year, y = Per_cent, group = Topic_disaggregation, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Total Admissions %", title = "Total Admissions by Year and  Remoteness") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))




############################################################################################################################################################################################
######################################################### GEOGRAPHY

# Clean up the data
Geography <- clean_colnames(Geography)
Geography <- Geography %>% 
  filter(!is.na(Chapter)) %>% 
  drop_na()



Geography$Year <- as.numeric(as.character(Geography$Year), na.rm = TRUE)
# Replace non-numeric characters with NA
Geography$Numerator <- ifelse(grepl("^\\d+$", Geography$Numerator), Geography$Numerator, NA)

# Convert the Numerator column to numeric
Geography$Numerator <- as.numeric(as.character(Geography$Numerator))
Geography$Denominator <- as.numeric(as.character(Geography$Denominator))
Geography$Per_cent <- as.numeric((Geography$Per_cent))

Geography <- Geography %>% 
  drop_na()

############################################################### Analysis by State

# Define a list of topic disaggregations to include
state <- c("NSW","Vic","Qld","WA","SA","Tas","ACT","NT")

# Subset the data frame to include only rows where the topic disaggregation is in the included list
state_df <- subset(Geography, Subgroup_disaggregation %in% state)
state_df <- subset(state_df, Subgroup %in% "State and territory of birth")


######################################### Maternal age

maternal_age_state <- subset(state_df,Topic %in% "Maternal age")
maternal_age_state <- subset(maternal_age_state,!Topic_disaggregation == "Total" ) 

# Create a line plot with year on the x-axis, percent on the y-axis, and different maternal age groups represented in the legend
p <- ggplot(maternal_age_state, aes(x = Year, y = Per_cent, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Percent", color = "Maternal age group", linetype = "Maternal age group",title = "Total Births percentage by Year,State and maternal age") +
  theme_bw()

# Facet the plot by State_and_Territory
p + facet_wrap(~ Subgroup_disaggregation,scales = "free_y")

##################################33 indigenous mother
indigenous_state <- subset(state_df,Topic %in% "Indigenous status (mother)")
indigenous_state <- subset(indigenous_state,!Topic_disaggregation == "Total" )

# Create a line plot with year on the x-axis, percent on the y-axis, and different maternal age groups represented in the legend
p <- ggplot(indigenous_state, aes(x = Year, y = Per_cent, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Percent",title = "Total Births percentage by Year,State and Indigenous status") +
  theme_bw()

# Facet the plot by State_and_Territory
p + facet_wrap(~ Subgroup_disaggregation,scales = "free_y")

#################################### Socioeconomic status

status_df <- subset(Geography, Subgroup_disaggregation %in% state)
economic_status_state <- subset(status_df,Topic %in% "Socioeconomic status")
economic_status_state <- subset(economic_status_state,!Topic_disaggregation == "Total" & !Topic_disaggregation == "Not stated" )

# Create a line plot with year on the x-axis, percent on the y-axis, and different maternal age groups represented in the legend
p <- ggplot(economic_status_state, aes(x = Year, y = Per_cent, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Percent",title = "Total Births percentage by Year,State and Socioeconomic status") +
  theme_bw()

# Facet the plot by State_and_Territory
p + facet_wrap(~ Subgroup_disaggregation,scales = "free_y")


#################################### Remoteness Area
remoteness <- subset(Geography, Subgroup_disaggregation %in% state)
remote_state <- subset(remoteness,Topic %in% "Remoteness area")
remote_state <- subset(remote_state,!Topic_disaggregation == "Total" )

# Create a line plot with year on the x-axis, percent on the y-axis, and different maternal age groups represented in the legend
p <- ggplot(remote_state, aes(x = Year, y = Per_cent, color = Topic_disaggregation)) +
  geom_line() +
  labs(x = "Year", y = "Percent",title = "Total Births percentage by Year,State and Remoteness") +
  theme_bw()

# Facet the plot by State_and_Territory
p + facet_wrap(~ Subgroup_disaggregation,scales = "free_y")
