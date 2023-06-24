#1. Load libraries 
library(magrittr)
library(tidyverse)
library(reshape2)
library(ggplot2)

#2. Create a vector of sustainable materials
sustainable_materials <- c("Bamboo", "Recycled Plastic", "Cork", "Compostable Plastics")

#3. Filter a dataset of phone case materials by sustainable materials
phone_case_materials <- read_csv("phone_case_materials.csv")
phone_case_materials %<>% 
  filter(material %in% sustainable_materials)
  
#4. Group the data by material and calculate average grip
average_grip_by_material <- phone_case_materials %>% 
  group_by(material) %>% 
  summarize(avg_grip = mean(grip_rating))
  
#5. Create a bar plot of average grip by material
ggplot(average_grip_by_material, aes(x = material, y = avg_grip)) + 
  geom_bar(stat = "identity", fill = "darkgreen") + 
  ggtitle("Average Grip by Sustainable Material") 

#6. Create a vector of textured phone cases
textured_phone_cases <- c("Bumpers", "Shockproof Cases", "Rubber Cases")

#7. Filter a dataset of phone cases by textured phone cases
phone_cases <- read_csv("phone_cases.csv")
phone_cases %<>% 
  filter(case_style %in% textured_phone_cases)

#8. Group the data by case style and calculate average grip
average_grip_by_style <- phone_cases %>% 
  group_by(case_style) %>% 
  summarize(avg_grip = mean(grip_rating))

#9. Create a bar plot of average grip by phone case style
ggplot(average_grip_by_style, aes(x = case_style, y = avg_grip)) + 
  geom_bar(stat = "identity", fill = "darkgreen") + 
  ggtitle("Average Grip by Textured Phone Case Style")

#10. Create a dataset of sustainable phone cases with textured grip
sustainable_textured_phone_cases <- inner_join(phone_cases, phone_case_materials, 
                                              by = 'case_type') 

#11. Group the data by material and case style and calculate average grip
average_grip_by_material_style <- sustainable_textured_phone_cases %>% 
  group_by(material, case_style) %>% 
  summarize(avg_grip = mean(grip_rating))

#12. Create a facetted bar plot of average grip by material and case style
ggplot(average_grip_by_material_style, 
       aes(x = case_style, y = avg_grip)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  ggtitle("Average Grip by Sustainable Material and Textured Phone Case Style") +
  facet_grid( ~ material)

#13. Create a data frame of sustainable phone cases with textured grip
sustainable_textured_phone_cases_df <- as.data.frame(sustainable_textured_phone_cases)

#14. Select only the columns of interest
sustainable_textured_phone_cases_df %<>% 
  select(model, colour, material, case_style, grip_rating)

#15. Create a summary table of grip ratings
grip_summary_table <- sustainable_textured_phone_cases_df %>% 
  group_by(model, colour, material, case_style) %>% 
  summarize(grip_rating = mean(grip_rating))

#16. Sort the table by grip rating
grip_summary_table %<>% 
  arrange(desc(grip_rating))

#17. Write the summary table to a csv file
write_csv(grip_summary_table, "sustainable_textured_phone_cases_summary.csv")

#18. Create a plot of grip ratings by model
ggplot(sustainable_textured_phone_cases_df, 
       aes(x = model, y = grip_rating)) +
  geom_violin(fill = 'darkgreen') +
  ggtitle("Grip Rating by Sustainable Textured Phone Case Model")

#19. Create a plot of grip ratings by colour
ggplot(sustainable_textured_phone_cases_df, 
       aes(x = colour, y = grip_rating)) +
  geom_violin(fill = 'darkgreen') +
  ggtitle("Grip Rating by Sustainable Textured Phone Case Colour")

#20. Create a plot of grip ratings by case style
ggplot(sustainable_textured_phone_cases_df, 
       aes(x = case_style, y = grip_rating)) +
  geom_violin(fill = 'darkgreen') +
  ggtitle("Grip Rating by Sustainable Textured Phone Case Style")