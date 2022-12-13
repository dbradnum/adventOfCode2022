library(tidyverse)

raw = read_lines("day13/day13.txt") 
raw = raw[str_length(raw) > 0]

inputLists = raw %>% 
  str_replace_all("\\[","list(") %>% 
  str_replace_all("\\]",")") 

compareScalars = function(first,second){
  # print(str_glue("Scalar: {first}  vs  {second}"))
  
  if (is.numeric(first) && is.numeric(second)){
    if (first < second) return(TRUE)
    
    if (first > second) return(FALSE)
    
    return(NA)
  } 
}

compareLists = function(l1,l2){
  index = 1
  # print(str_glue("INDEX: {index}"))
  
  while(TRUE) {
    if (index > length(l1) & index <= length(l2)){
      return(TRUE)
    }
    if (index > length(l2) & index <= length(l1)){
      return(FALSE)
    } 
    if (index > length(l1) & index > length(l2)){
      return(NA)
    }
    
    s1 = l1[[index]]
    s2 = l2[[index]]
    
    if (is.list(s1) & is.list(s2))
    {
      innerResult = compareLists(s1,s2)
      if (!is.na(innerResult)) return(innerResult)
      
    } else if (is.numeric(s1) & is.list(s2)){
      return(compareLists(list(s1),s2))
      
    } else if (is.numeric(s2) & is.list(s1)){
      return(compareLists(s1,list(s2)))
    } else {
      
      result = compareScalars(s1,s2)
      if (!is.na(result) ) return(result)
    }
    index = index + 1
    # print(str_glue("INDEX: {index}"))
  }
}

convertToList = function(string){
  eval(parse(text = string))   
}

getResultForPair = function(pair){
  a = convertToList(inputLists[pair * 2 - 1])
  b = convertToList(inputLists[pair * 2])
  
  compareLists(a,b)
}

nPairs = length(inputLists)/2

sum(which(map_lgl(1:nPairs,getResultForPair)))


# part 2 - apply custom sort algorithm ------------------------------------

raw = c(raw,"[[2]]","[[6]]")

inputLists = raw %>% 
  str_replace_all("\\[","list(") %>% 
  str_replace_all("\\]",")") 

## FOllowing https://www.r-bloggers.com/2017/04/custom-comparison-function-for-sorting-data-in-r/ 
## Make an instance of a custom class, for which we override inequality and index operators
class(inputLists) <- "foo"

## to use the "extract" ([) method, 
## we need to momentarily change the class of x, because 
## otherwise we will end up in an endless loop
'[.foo' <- function(x, i) {
  class(x) <- "list"
  x <- x[i]
  class(x) <- "foo"
  x
}

## define ">" as stated above
## the weird syntax results from the fact that a and b
## are lists with one element, this element being a vector 
## of a pair of numbers
'>.foo' <- function(a,b) {
  # a <- a[[1]]
  # b <- b[[1]]
  !compareLists(convertToList(a),
                convertToList(b))
}

## if we can't find a difference, then there is no difference
'==.foo' <- function(a, b)  {
  is.na(compareLists(convertToList(a),
               convertToList(b)))
}

## we don't need that, but for the sake of completeness...
'<.foo' <- function(a, b) b > a


sorted = unlist(sort(inputLists))

prod(which(sorted == "list(list(2))" | sorted == "list(list(6))"))
