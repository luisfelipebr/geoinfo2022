
library(tidyverse)
library(sf)

tabl <- structure(list(Polo = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 
                                          2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 
                                          6L), levels = c("Belo Horizonte", "Brasília", "Juazeiro do Norte", 
                                                          "Marabá", "Porto Alegre", "Recife"), class = "factor"), Typology = structure(c(1L, 
                                                                                                                                       1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
                                                                                                                                       3L, 4L, 4L, 4L, 4L, 4L, 4L), levels = c("T1", "T2", "T3", "T4"
                                                                                                                                       ), class = "factor"), Freq = c(0.3, 0.94, 0.75, 0.52, 0.48, 0.16, 
                                                                                                                                                                      0.35, 0.03, 0.05, 0.1, 0.34, 0.43, 0.31, 0.03, 0.13, 0.23, 0.07, 
                                                                                                                                                                      0.08, 0.04, 0.01, 0.07, 0.15, 0.11, 0.33)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                      -24L))


tabl

ggplot() +
  geom_col(data = tabl, aes(x = Freq, y = Polo, fill = Typology), 
           position_fill(reverse = TRUE)) +
  #  geom_text() +
  scale_y_discrete(limits=rev) +
  scale_fill_manual(values = c("#648FFF", "#785EF0", "#DC267F", "#FE6100")) +
  theme_minimal() +
  theme(legend.position = "top") +
  xlab("") +
  ylab("")

ggplot() +
  geom_col(data = tabl, aes(x = Freq, y = Polo, fill = Typology), 
           position_fill(reverse = TRUE)) +
#  geom_text() +
  scale_y_discrete(limits=rev) +
  scale_fill_manual(values = c("#648FFF", "#785EF0", "#DC267F", "#FE6100")) +
  theme_minimal() +
  theme(legend.position = "top") +
  xlab("") +
  ylab("") +
  ggrepel::geom_label_repel(data = tabl, aes(x = Freq, y = Polo, label = Freq),
                           position = position_fill(vjust = 0.5),
                           force = 1,
                           force_pull = 1,
                           direction = "x",
                           seed = 420)

