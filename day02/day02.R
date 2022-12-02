library(tidyverse)
library(tibble)

mapping = tibble(
  theirs = c("A","B","C"),
  mine = c("X","Y","Z"),
  value = c(1,2,3)
)

raw = read_delim("day02/day02_example.txt",delim = " ",col_names = c("theirs","mine"))

# part 1 ------------------------------------------------------------------

joined = raw %>% 
  inner_join(mapping %>% select(-mine)) %>%
  rename(theirVal = value) %>% 
  inner_join(mapping %>% select(-theirs)) %>% 
  rename(myVal = value) 


joined %>% 
  mutate(result = (myVal - theirVal + 1)%%3 * 3,
         score = myVal + result) %>% 
  summarise(total = sum(score)) %>% 
  pull(total)


# part 2 ------------------------------------------------------------------

joined %>% 
  rename(outcome = myVal) %>% 
  mutate(myVal = (theirVal + outcome) %% 3 + 1,
         score = myVal + (outcome - 1)*3) %>% 
  summarise(total = sum(score)) %>% 
  pull(total)
