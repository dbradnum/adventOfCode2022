library(tidyverse)

raw = read_csv("day04/input/day04.txt", col_names = c("first","second"))


cleaned = raw %>% 
  separate(first,into = (c("firstMin","firstMax"))) %>% 
  separate(second,into = (c("secondMin","secondMax"))) %>% 
  transmute_all(as.numeric) 

# part 1 ------------------------------------------------------------------
cleaned %>% 
  filter((firstMin <= secondMin & firstMax >= secondMax) |
           (secondMin <= firstMin & secondMax >= firstMax)) %>% 
  count() %>% pull()


# part 2 ------------------------------------------------------------------

cleaned %>% 
  filter((firstMax >= secondMin & firstMin <= secondMax) |
           (firstMin <= secondMax & firstMax >= secondMin)) %>% 
  count() %>% pull()

