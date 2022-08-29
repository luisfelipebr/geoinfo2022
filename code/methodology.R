
library(DiagrammeR)

grViz("
      digraph {
      
      graph[layout = dot, rankdir = LR]

      input [label = 'NUI \n data set', shape = parallelogram]
      step1 [label = 'ensemble modelling\n(k-modes with\ndifferent seeds)', shape = box]
      step2 [label = 'consensus cluster\n(relabelling and\nmajority voting)', shape = box]
      output [label = 'NUI \n typologies', shape = parallelogram]

      input -> step1 -> step2 -> output
      
      }
      ")

#step2 [label = 'cluster correspondence \n (flatten and relabel)', shape = box]