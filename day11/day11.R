library(tidyverse)
library(rlang)

raw = read_lines("day11/day11.txt")
nBlocks = ceiling(length(raw)/7)

extractBlock = function(iBlock){
  
  startIndex = iBlock * 7
  block = raw[(startIndex + 2):(startIndex + 7)]
  
  startItems = str_extract_all(block[1],"\\d+")
  op = str_extract(block[2],"(?<== ).+")
  testDivisor = as.numeric(str_extract(block[3],"(?<=by ).+"))
  truePass = as.numeric(str_extract(block[4],"(?<=monkey ).+"))
  falsePass = as.numeric(str_extract(block[5],"(?<=monkey ).+"))
  
  tibble(monkeyId = iBlock, startItems,op,testDivisor,truePass,falsePass)
}

extracted = map_dfr(0:(nBlocks-1),extractBlock) 

monkeyRules = extracted %>% 
  # mutate(op = str_replace(op,"old","{{old}}")) %>% 
  select(-startItems)



# part 1 - slow! -----------------------------------------------------------

status = extracted %>%
  select(monkeyId,startItems) %>% 
  unnest_longer(startItems,values_to = "old") 

checkCount = rep(0,nBlocks)

tic()
for (loop in 1:20){
  print(loop)
  for (i in 1:nBlocks){
    # print(i)
    toProcess = status %>% filter(monkeyId == i-1)
    
    if (nrow(toProcess)){
      checkCount[i] = checkCount[i] + nrow(toProcess)
      
      # print(toProcess)
      
      processed = toProcess %>% 
        inner_join(monkeyRules,by = "monkeyId") %>%  
        rowwise() %>% 
        mutate(new = eval(parse(text = str_replace_all(op,"old",old))),
               new = floor(new/3),
               monkeyId = if_else(new %% testDivisor == 0,truePass,falsePass),
               old = as.character(new)) %>% 
        select(monkeyId,old)
      
      status = bind_rows(status %>% filter(monkeyId != i-1),
                         processed)
    }
    # print(status)
  }
  
  # print(status)
}
toc()

prod(head(checkCount[order(checkCount,decreasing = T)],2))


# part 2 - trying to get faster ---------------------------------------------------------

# this would take c. 5 mins to complete for entire solution

# notice that we only need to retain the new worry level modulo the product of all divisors;
# this means we can prevent the levels getting ever larger and overflowing
divisorProd = prod(monkeyRules$testDivisor)

status = extracted %>%
  select(monkeyId,startItems) %>% 
  unnest_longer(startItems,values_to = "old") %>% 
  mutate(old = as.numeric(old))

checkCount = rep(0,nBlocks)

tic()
for (loop in 1:1000){
  if (loop%%100 == 0) print(loop)
  
  for (i in 1:nBlocks){
    # print(i)
    toProcess = status %>% 
      filter(monkeyId == i-1) 
    
    if (nrow(toProcess)){
      checkCount[i] = checkCount[i] + nrow(toProcess)
      
      # print(toProcess)
      op = monkeyRules$op[i]
      testDivisor = monkeyRules$testDivisor[i]
      truePass = monkeyRules$truePass[i]
      falsePass = monkeyRules$falsePass[i]
      
      processed = toProcess %>% 
        mutate(old := !! parse_expr(op),
               old = old %% divisorProd, 
               monkeyId = if_else(old %% testDivisor == 0,truePass,falsePass)) 
      
      status = bind_rows(status %>% filter(monkeyId != i-1),
                         processed)
    }
    # print(status)
  }
  
  # print(status)
}
toc()

checkCount
prod(head(checkCount[order(checkCount,decreasing = T)],2))


# part 2 - with lookup ----------------------------------------------------

# try a different approach; note that there are only a certain number of possible 
# inputs now we've used the modulo trick - so compute them all upfront, and then 
# just do lookups. (Another, poss better, approach would be to use memoization 
# to compute and cache?)

# this takes about 45s

divisorProd = prod(monkeyRules$testDivisor)

getLookupFor = function(i){
  op = monkeyRules$op[i]
  testDivisor = monkeyRules$testDivisor[i]
  truePass = monkeyRules$truePass[i]
  falsePass = monkeyRules$falsePass[i]
  
  tibble(old = seq.integer64(0,divisorProd)) %>% 
    mutate(new := !! parse_expr(op),
           new = new %% divisorProd,
           monkeyId = if_else(new %% testDivisor == 0,truePass,falsePass)) %>% 
    select(old = new,monkeyId)
}

# calculate a list of lookups, one per monkey
lookups = map(1:nBlocks,getLookupFor)

status = extracted %>%
  select(monkeyId,startItems) %>% 
  unnest_longer(startItems,values_to = "old") %>% 
  mutate(old = as.integer64(old))

checkCount = rep(0,nBlocks)



tic()
for (loop in 1:10000){
  if (loop%%100 == 0) print(loop)
  
  for (i in 1:nBlocks){
    # print(i)
    
    # use indexing rather than filter; faster and can reuse indices later
    thisMonkey = status$monkeyId == i-1
    toProcess = status[thisMonkey,]
    
    iLookup = lookups[[i]]
    
    if (nrow(toProcess)){
      checkCount[i] = checkCount[i] + nrow(toProcess)
      
      # Initial attempt - but joins are slow, prefer lookup!
      
      # processed = toProcess %>% 
      #   inner_join(lookup,by = c("monkeyId","old")) %>% 
      #   select(monkeyId = nextMonkeyId,old = new)
      
      processed = iLookup[as.numeric(toProcess$old)+1,]
      
      status = bind_rows(status[!thisMonkey,],
                         processed)
    }
    # print(status)
    
  }
  # print(status)
}

toc()

checkCount
prod(head(checkCount[order(checkCount,decreasing = T)],2))


