library(tidyverse)
library(igraph)


# build graph (tree) -------------------------------------------------------------

raw = read_lines("day07/day07.txt")

currentDir = ""

edges = tibble(from = character(),
               to = character(),
               size = character())

for (i in 1:length(raw)){
  # print(raw[i])
  parts = str_split(raw[i]," ",simplify = T)
  # options: cd, ls, dir, file
  if (parts[2] == "cd"){
    if (parts[3] == ".."){
      # find parent
      parent = edges$from[edges$to == currentDir]
      currentDir = parent
    } else {
      currentDir = str_glue("{currentDir}/{parts[3]}")
    }
  } else if (parts[2] == "ls"){
    # do nothing
  } else {
    value = ifelse(parts[1] != "dir",parts[1],NA)
    newRow = tibble(from = currentDir,
                    to = str_glue("{currentDir}/{parts[2]}"),
                    size = value)
    edges = bind_rows(edges,newRow)
  } 
}

edges = edges %>% mutate(size = as.numeric(size)) 
vertices = edges %>% select(name = to,size) %>% bind_rows(tibble(name = "//",size = NA))

g = graph_from_data_frame(edges %>% select(-size),
                          vertices = vertices)


# calculate directory sizes -----------------------------------------------

# iGraph handles the recursion for us, by examining the "ego network" for each node
# NB - maybe not the most efficient approach, unless ego networks are cached...
getTotalDirSize = function(v) {
  sub = induced_subgraph(g,ego(g,
                               order = diameter(g),
                               v,
                               mode = "out")[[1]])
  
  
  tibble(name = v$name,
         totalSize = sum(V(sub)$size,na.rm = T),
         isDir = is.na(v$size))
}

totalDirSizes = map_dfr(V(g),getTotalDirSize) %>% 
  filter(isDir)

totalDirSizes %>% 
  filter(totalSize <= 100000) %>% 
  summarise(result = sum(totalSize)) %>%
  pull(result)


# part 2 ------------------------------------------------------------------

totalSpaceUsed = max(totalDirSizes$totalSize)
extraRequired = 30000000 - (70000000 - totalSpaceUsed)

totalDirSizes %>% 
  filter(totalSize > extraRequired) %>% 
  arrange(totalSize) %>% 
  slice_head(n = 1) %>% 
  pull(totalSize)
