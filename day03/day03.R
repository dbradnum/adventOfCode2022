library(tidyverse)

raw = read_csv("day03/input/day03_example.txt",col_names = "line")

letterMapping = tibble(char = c(letters,LETTERS),
                       value = 1:52)

# part 1 ------------------------------------------------------------------

raw %>% 
  mutate(first = str_sub(line,1,nchar(line)/2),
         second = str_sub(line,nchar(line)/2 + 1, nchar(line))) %>% 
  mutate(across(first:second,~strsplit(.,""))) %>% 
  mutate(dup = map2_chr(first,second,intersect)) %>% 
  inner_join(letterMapping,by = c("dup" = "char")) %>% 
  summarise(total = sum(value)) %>% 
  pull(total)


# part 2 ------------------------------------------------------------------

raw %>% 
  mutate(groupPos = (row_number() - 1) %% 3,
         group = floor((row_number() - 1) / 3 )) %>% 
  pivot_wider(names_from = groupPos,values_from = line,names_prefix = "line") %>% 
  mutate(across(starts_with("line"),~strsplit(.,""))) %>% 
  select(-group) %>% 
  mutate(priority = pmap_chr(.,~Reduce(intersect,list(..1,..2,..3)))) %>% 
  inner_join(letterMapping,by = c("priority" = "char")) %>% 
  summarise(total = sum(value)) %>% 
  pull(total)

         