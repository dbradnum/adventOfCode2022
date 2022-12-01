library(tidyverse)

raw = read_csv("day01/input/day01.txt",col_names = "calories",skip_empty_rows = F)


# part 1 -------------------------------------------------------------------

raw %>% 
  mutate(isBlank = is.na(calories),
         elfId = cumsum(isBlank)) %>% 
  filter(!is.na(calories)) %>% 
  group_by(elfId) %>% 
  summarise(totalCalories = sum(calories)) %>% 
  arrange(-totalCalories) %>% 
  head(1) %>% 
  pull(totalCalories)
  

# part 2 ------------------------------------------------------------------

raw %>% 
  mutate(isBlank = is.na(calories),
         elfId = cumsum(isBlank)) %>% 
  filter(!is.na(calories)) %>% 
  group_by(elfId) %>% 
  summarise(totalCalories = sum(calories)) %>% 
  arrange(-totalCalories) %>% 
  head(3) %>% 
  summarise(topNTotal = sum(totalCalories)) %>% 
  pull(topNTotal)

