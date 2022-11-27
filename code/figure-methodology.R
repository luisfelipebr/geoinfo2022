
library(DiagrammeR)

grViz("
      digraph {
      
      graph[layout = dot, rankdir = TD]

      input [label = 'NUI dataset', shape = parallelogram]
      step1 [label = '(1) Exploratory data analysis \n to choose the number of clusters', shape = box]
      step2 [label = '(2) Clustering ensemble model \n (k-modes and consensus cluster)', shape = box]
      output [label = 'NUI typologies', shape = parallelogram]
      step3 [label = '(3) Descriptive and \n comparative analysis', shape = sdl_call]
      
      {rank = same; step1}
      {rank = same; input; step2; step3; output}
      
      input -> step1 -> step2
      input -> step2 -> output -> step3
      
      }
      ")
