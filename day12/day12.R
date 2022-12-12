library(tidyverse)
library(adventdrob)
library(tidygraph)

raw = read_csv("day12/day12.txt",col_names = "line")

g = grid_tidy(raw,line) %>% 
  rename(char = value) %>% 
  mutate(id = row_number())

charMap = tibble(char = c("S",letters,"E"),val = c(1,1:26,26))

validEdges = g %>%
  inner_join(charMap,by = "char") %>% 
  adjacent_join() %>% 
  filter(val2 <= val + 1) %>% 
  select(id, id2)

network = tbl_graph(edges = validEdges)


# part 1 ------------------------------------------------------------------

path = igraph::shortest_paths(network,
                              g$id[which(g$char == "S")],
                              g$id[which(g$char == "E")])$vpath

length(unlist(path)) - 1


# part 2 ------------------------------------------------------------------

# more efficient to compute shortest paths BACKWARDS since can then do in one call
# ... so construct a new graph with edges running the opposite way
revNetwork = tbl_graph(edges = validEdges %>% select(id2,id))

shortestPaths = igraph::shortest_paths(revNetwork,
                                       g$id[which(g$char == "E")
                                       ])

pathLengths = map_dbl(shortestPaths$vpath,~length(unlist(.))-1)

starts = g$id[which(g$char %in% c("S","a"))]

lengths = pathLengths[starts]
validLengths = lengths[lengths > 0]
min(validLengths)
