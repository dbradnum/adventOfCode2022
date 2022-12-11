library(tidyverse)

directions = tibble(direction = c("R","L","U","D"),
                    xAdd = c(1,-1,0,0),
                    yAdd = c(0,0,1,-1))

input = read_delim("day09/day09.txt",delim = " ",
                   col_names = c("direction","count")) %>% 
  uncount(count) %>% 
  inner_join(directions) %>% 
  mutate(headX = cumsum(xAdd),
         headY = cumsum(yAdd)) %>% 
  select(-xAdd,-yAdd) %>% 
  mutate(diffX = NA_integer_,diffY = NA_integer_,
         tailX = NA_integer_,tailY = NA_integer_)

input

# part 1 - first attempt, slow! ----------------------------------------------------------

library(tictoc)

tailX = 1
tailY = 1
tic()
for (i in 1:nrow(input)){
  diffX = input$headX[i] - tailX
  diffY = input$headY[i] - tailY
  
  input$diffX[i] = diffX
  input$diffY[i] = diffY
  
  if (diffX > 1){
    tailX = tailX + 1 
    tailY = input$headY[i]
  } else if (diffX < -1){
    tailX = tailX - 1
    tailY = input$headY[i]
  } 
  
  if (diffY > 1){
    tailY = tailY + 1 
    tailX = input$headX[i]
  } else if (diffY < -1){
    tailY = tailY - 1
    tailX = input$headX[i]
  } 
  
  input$tailX[i] = tailX
  input$tailY[i] = tailY
}
toc()
input

input %>% count(tailX,tailY) %>% count()


# part 1 - much simpler -----------------------------------------------------

input = input %>% select(-diffX,-diffY)

tailX = 1
tailY = 1
tic()
for (i in 1:nrow(input)){
  diffX = input$headX[i] - tailX
  diffY = input$headY[i] - tailY
  
  # we never move if we're already within 1
  if (max(abs(input$headX[i] - tailX),
          abs(input$headY[i] - tailY)) > 1){
    # and then when we do move, we move towards head (even if already aligned)
    tailX = tailX + sign(input$headX[i] - tailX)
    tailY = tailY + sign(input$headY[i] - tailY)
  }
  input$tailX[i] = tailX
  input$tailY[i] = tailY  
  
}
toc()
input

input %>% count(tailX,tailY) %>% count()


# part 2 - accumulate tail position --------------------------------------------

input = read_delim("day09/day09.txt",delim = " ",
                   col_names = c("direction","count")) %>% 
  uncount(count) %>% 
  inner_join(directions) %>% 
  mutate(headX = cumsum(xAdd),
         headY = cumsum(yAdd),
         # handle x and y as a single column
         head = map2(headX,headY,c)) %>% 
  select(-xAdd,-yAdd)

moveTail = function(prevTail,head){
    if (max(abs(head-prevTail)) > 1){
    # move both coords in one go
    map2_dbl(head,prevTail, ~ .y + sign(.x - .y))
  } else {
    prevTail
  }
}

# accumulate(input$head,
#            moveTail,
#            .init = c(0,0))

# initialise head with a value that will move the tail to 1,1, 
# but then throw away that init value
part1Result = input %>% 
  mutate(tail = accumulate(head,
                           moveTail,
                           .init = c(0,0))[-1])


part1Result %>% 
  unnest_wider(tail,names_sep = "") %>% 
  count(tail1,tail2) %>% count()

# could do this neatly with tidyeval, but not right now!
part2Result = input %>% 
  mutate(tail1 = accumulate(head,moveTail,.init = c(0,0))[-1],
         tail2 = accumulate(tail1,moveTail,.init = c(0,0))[-1],
         tail3 = accumulate(tail2,moveTail,.init = c(0,0))[-1],
         tail4 = accumulate(tail3,moveTail,.init = c(0,0))[-1],
         tail5 = accumulate(tail4,moveTail,.init = c(0,0))[-1],
         tail6 = accumulate(tail5,moveTail,.init = c(0,0))[-1],
         tail7 = accumulate(tail6,moveTail,.init = c(0,0))[-1],
         tail8 = accumulate(tail7,moveTail,.init = c(0,0))[-1],
         tail = accumulate(tail8,moveTail,.init = c(0,0))[-1],
  ) 

part2Result%>% 
  select(tail) %>% 
  unnest_wider(tail,names_sep = "") %>% 
  count(tail1,tail2) %>% count()
