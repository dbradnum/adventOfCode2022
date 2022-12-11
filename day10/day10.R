library(tidyverse)

raw = read_csv("day10/day10.txt",col_names = "line") %>% 
  separate(line,into = c("operation","value"),sep = " ", fill = "right") %>% 
  mutate(value = as.numeric(value),
         cycles = if_else(operation == "noop",1,2),
         cycleNumber = cumsum(cycles)) 


xVals = raw %>% 
  complete(cycleNumber = seq(1,max(cycleNumber))) %>% 
  mutate(value = coalesce(value,0)) %>% 
  mutate(x = 1 + cumsum(value),
         # But NB that x only changes AFTER the addx completes. 
         # so deduct the value on the cycle while it is completing
         x = x - ifelse(!is.na(operation) & operation == "addx", value,0)) 


# part 1 ------------------------------------------------------------------

xVals %>% 
  filter(cycleNumber %in% c(20,60,100,140,180,220) )%>% 
  summarise(result = sum(cycleNumber * x ))


# part 2  -----------------------------------------------------------------

pixels = xVals %>% 
  mutate(lit = ((x <= cycleNumber%%40) & (x >= cycleNumber%%40 - 2)))

out = ""
for (i in 1:nrow(pixels)){
  out = paste0(out,ifelse(pixels$lit[i],"#","."))
  if (i%%40 == 0) out = paste0(out,"\n")
}
writeLines(out)
