library(tidyverse)
library(dequer)

inputFile <- "day05/day05.txt"
raw = read_lines(inputFile,skip_empty_rows = F)

splitLine = which(nchar(raw) == 0)

suppressMessages({
  instructions = enframe(raw[(splitLine + 1):length(raw)],name = NULL) %>% 
    mutate(numbers = str_match_all(value,"\\d+")) %>% 
    unnest_wider(numbers) %>% 
    select(-1) %>% 
    set_names("reps","from","to") %>% 
    mutate_all(as.numeric)
})
stacks = raw[1:(splitLine-2)]
nStacks = (nchar(stacks[[length(stacks)]]) + 1)/4

initialStacks = read_fwf(inputFile,
         col_positions = fwf_widths(rep_len(4,nStacks)),
         n_max = splitLine - 2) %>% 
  mutate_all(~str_remove_all(.,"[\\[\\]]"))


# helpers -----------------------------------------------------------------

initialiseQueues = function(){
  queues = list()
  
  for (jCol in 1:nStacks) {
    queues[[jCol]] = stack()
    for (iRow in nrow(initialStacks):1){
      val = initialStacks[[iRow,jCol]]
      if (!is.na(val)){
        push(queues[[jCol]],val)
      }
    }
  }
  queues
}

getResult = function(queues){
  result = ""
  for (iStack in 1:nStacks){
    result = paste0(result,pop(queues[[iStack]]))
  }
  result
}

# part 1 ------------------------------------------------------------------

queues = initialiseQueues()

for (iInst in 1:nrow(instructions)){
  reps = instructions$reps[iInst]
  from = instructions$from[iInst]
  to = instructions$to[iInst]
  for (jRep in 1:reps){
    val = pop(queues[[from]])
    push(queues[[to]],val)
  }
}

getResult(queues)


# part 2 ------------------------------------------------------------------
queues = initialiseQueues()

for (iInst in 1:nrow(instructions)){
  reps = instructions$reps[iInst]
  from = instructions$from[iInst]
  to = instructions$to[iInst]
  tempStack = stack()
  for (jRep in 1:reps){
    push(tempStack,pop(queues[[from]]))
  }
  for (kRep in 1:reps){
    push(queues[[to]],pop(tempStack))
  }
}

getResult(queues)

