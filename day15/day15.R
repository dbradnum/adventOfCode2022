library(tidyverse)

positions = tibble(line = read_lines("day15/day15.txt")) %>% 
  mutate(nums = str_extract_all(line,"[-\\d]+")) %>% 
  hoist(nums,Sx = list(1),Sy = list(2),Bx = list(3),By = list(4)) %>% 
  select(-line) %>% 
  mutate_all(as.numeric)

sensorRadii = positions %>% 
  mutate(sensorRadius = abs(By-Sy) + abs(Bx-Sx))


getNonBeaconPositionsForLine = function(lineNo){
  
  covered = positions %>% 
    mutate(sensorRadius = abs(By-Sy) + abs(Bx-Sx)) %>% 
    mutate(minCovered = Sx - (sensorRadius - abs(lineNo - Sy)),
           maxCovered = Sx + (sensorRadius - abs(lineNo - Sy))) %>% 
    filter(minCovered <= maxCovered) %>% 
    select(x1 = minCovered,x2 = maxCovered)
  
  beaconPositions = positions %>% 
    filter(By == lineNo) %>% 
    pull(Bx) %>% 
    unique()
  
  coveredRanges = pmap(covered , function(x1,x2) x1:x2) %>% 
    reduce(union) 
  
  coveredRanges[!(coveredRanges %in% beaconPositions)]
}

getNonBeaconPositionsForLine(2000000) %>% length()


# part 2 ------------------------------------------------------------------

install.packages("BiocManager")
BiocManager::install("IRanges")
library(IRanges)

gridSize = 20


overlaps = function(lineNo){
  
  covered = positions %>% 
    mutate(sensorRadius = abs(By-Sy) + abs(Bx-Sx)) %>% 
    mutate(minCovered = Sx - (sensorRadius - abs(lineNo - Sy)),
           maxCovered = Sx + (sensorRadius - abs(lineNo - Sy))) %>% 
    filter(minCovered <= maxCovered) %>% 
    transmute(x1 = pmax(0,minCovered),
           x2 = pmin(maxCovered,gridSize))
  
  # beaconPositions = positions %>% 
  #   filter(By == lineNo) %>% 
  #   pull(Bx) %>% 
  #   unique()
  
  coveredRanges = IRanges(start=covered$x1, end=covered$x2)
  
  hits <- findOverlaps(coveredRanges) %>% as.data.frame() %>% filter(queryHits != subjectHits)

  sum(width(pintersect(coveredRanges[hits$queryHits],
                   coveredRanges[hits$subjectHits])))
}

nGaps = function(lineNo){
  
  covered = positions %>% 
    mutate(sensorRadius = abs(By-Sy) + abs(Bx-Sx)) %>% 
    mutate(minCovered = Sx - (sensorRadius - abs(lineNo - Sy)),
           maxCovered = Sx + (sensorRadius - abs(lineNo - Sy))) %>% 
    filter(minCovered <= maxCovered) %>% 
    transmute(x1 = pmax(0,minCovered),
              x2 = pmin(maxCovered,gridSize))
  
  # beaconPositions = positions %>% 
  #   filter(By == lineNo) %>% 
  #   pull(Bx) %>% 
  #   unique()
  
  coveredRanges = IRanges(start=covered$x1, end=covered$x2)
  
  gaps(coveredRanges) %>% length()
}


gridSize = 4E6

library(furrr)
library(progressr)

plan(multisession)

with_progress({
  s = seq(2740000,2780000,1)
  
  p <- progressor(steps = length(s))
  
  overlapCount = future_map_dbl(s, ~{
    p()
    nGaps(.x)
  })
})

sum(overlapCount)

test =which.min(overlapCount) + 3345150
nGaps(test)

y = which(!is.na(gaps))
x = max(gaps,na.rm = T)

x * gridSize + y



library(ggforce)
ggplot() +
  geom_circle(aes(x0 = Sx, y0 = Sy, r = sensorRadius),fill = "gray",alpha = 0.2, data = sensorRadii) +
  coord_fixed() 
# + xlim(0,4E6) + ylim(0,4E6)
