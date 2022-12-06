library(tidyverse)
library(slider)

# input = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

input = read_lines("day06/day06.txt")
 
chars = unlist(str_split(input,""))

# part 1 ------------------------------------------------------------------

getMarkerForWindow = function(chars, windowLength){
  
  slices = slide(chars,~.x,.before = windowLength - 1,.complete = F)
  
  min(which(map_lgl(slices,function(x){
    length(unique(x)) == windowLength})))
}

getMarkerForWindow(chars,4)

# part 2 ------------------------------------------------------------------

getMarkerForWindow(chars,14)

