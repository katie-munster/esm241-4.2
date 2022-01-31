## Packages ----

library(dplyr) #for data wrangling
library(readxl) #for reading Excel file
library(tidyverse) #for dataframe functions
library(janitor) #for renaming variables
library(ggplot2) #for plotting
library(stringr) #for text manipulation

`%nin%` = Negate(`%in%`)

## Reading data ----

#reading datasets:
#note: this will only work without changing the paths if you open the .Rproj file
data <- read.csv("congress_116_data.csv")
metadata <- read.csv("Combined_Metadata.csv")

#reordering metadata 'variable' to match variable order in 'data':
order <- match(names(data),metadata$variable)
metadata <- metadata[order,]


## Data filtering and visualization to examine electoral vulnerability ----

#selecting for vulnerable House members:
vulnerable <- data %>%
  filter(primary_percent < 0.6 | general_percent < 0.6)

#plotting the distribution of primary and general margins:
group.colors <- c(D = "blue", R = "red", IDP ="green")
ggplot(vulnerable, aes(x= primary_percent, 
                       y= general_percent,
                       color = party)) + 
  geom_point() +
  geom_text(aes(label=district_id),
            hjust=0, vjust=0) +
  scale_color_manual(values=group.colors) + 
  ggtitle("Vote Percentages in 2018 House Elections")


## Data filtering and visualization to examine vulnerability and opinion ----

#selecting Democrats vulnerable to primary challenges who are not strongest possible environmentalists:
vulnerableD <- data %>%
  filter(primary_percent < 0.6 & party == "D" & lcv_score<100)

#plotting the distribution of LCV scores vs. constituent support to address climate:
ggplot(vulnerableD, aes(x= lcv_score, 
                       y= congress)) + 
  geom_point() +
  geom_text(aes(label=district_id),
            hjust=0, vjust=0) +
  ggtitle("LCV Scores of vulnerable Dems compared to \nproportion of constituents that want more action from Congress")



### New code:

# Filter for vulnerable democrats with populations who think their local officials (localofficials > 50) and Congress (congress > 50) should be doing more to address global warming
# Filter for those who say a candidate's views on global warming are important to their vote (priority > 50)
# Filter for those who are somewhat/very worried about global warming (worried > 50)
# Filter for populations with a low percentage of those who hear about global warming in the media (mediaweekly < 25)
# Rank in order of mediaweekly percentages (lowest percentages)
public_opinion_favors_policy <- vulnerableD %>% 
  filter(localofficials > 50) %>% 
  filter(congress > 50) %>% 
  filter(priority > 50) %>% 
  filter(worried > 50) %>% 
  filter(mediaweekly < 25) %>% 
  arrange(mediaweekly)
# TJ Cox is identified as a vulnerable Democrat of interest based on the code above

# TJ Cox data:
district_21 <- vulnerableD %>% 
  filter(district == 21)

