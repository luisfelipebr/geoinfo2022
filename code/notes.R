
# open libraries
library(tidyverse)
library(sf)
library(klaR)
library(diceR)

# create new functions
kmodes_elbow <- function(data, modes, seeds) {
  withindiff <- list()
  for (seed in seeds) {
    set.seed(seed)
    model <- kmodes(data = data,
                    modes = modes)
    withindiff[[seed]] <- model$withindiff
  }
  sum <- lapply(FUN = sum, withindiff)
  output <- list()
  output[[1]] <- mean(unlist(sum))
  output[[2]] <- sd(unlist(sum))
  print(Sys.time())
  return(output)
}

kmodes_ensemble <- function(data, modes, seed) {
  set.seed(seed)
  sample <- sample(seq_len(nrow(data)),size = floor(0.8*nrow(data)))
  data2 <- data[sample,]
  set.seed(seed)
  model <- kmodes(data = as.matrix(data2),
                  modes = modes)
  df <- data.frame(nrow = sample,
                   cluster = model[[1]])
  data <- data %>% 
    mutate(nrow = seq(1:nrow(data))) %>%
    left_join(df)
  return(data$cluster)
}

# import data
polos_nuis <- read_sf("input/nuis/POLOS_NUIS.shp")

# apply pre-processing steps
nuis <- polos_nuis %>%
  st_drop_geometry() %>%
  mutate(
    occupation_type = case_when(V5a == 1 ~ "1 Slum",
                                V5a == 2 ~ "2 Informal",
                                V5a == c(3,4,5,6,7,8) ~ "3 Other type"),
    establishment_time = case_when(V6 %in% c(1,2,3) ~ "21_<10y",
                                   V6 == 4 ~ "22_>10y",
                                   V6 == 5 ~ "23_N.A"),
    real_estate = case_when(V7 %in% c(1,2) ~ "31_Increase",
                            V7 == 3 ~ "32_Stable",
                            V7 %in% c(4,5) ~ "33_Decrease",
                            V7 == 6 ~ "34_N.A"),
    urban_contiguity = case_when(V8 == 3 ~ "41_Central",
                                 V8 %in% c(1,2) ~ "42_Periphery or remote"),
    special_zone = case_when(V9 %in% c(1,3) ~ "51_Yes",
                             V9 == 2 ~ "52_No",
                             V9 == 4 ~ "53_N.A"),
    protected_areas = case_when(V10a %in% c(2, 3, 4, 5, 6, 7, 8, 9) ~ "61_Yes",
                                V10a == 1 ~ "62_No"),
    preservation_areas = case_when(V11a == 1 ~ "71_Yes",
                                   V11a == 0 ~ "72_No"),
    risk_situation = case_when(V12a == 3 ~ "81_Yes",
                               V12a == 2 ~ "82_No",
                               V12a == 1 ~ "83_N.A"),
    risk_susceptibility = case_when(V13a %in% c(3,4) ~ "91_Yes",
                                    V13a == 2 ~ "92_No",
                                    V13a == 1 ~ "93_N.A"),
    parcels_access = case_when(V14a %in% c(1,2) ~ "101_Mostly_present",
                               V14a %in% c(3,4) ~ "102_Mostly_absent",
                               V14a == 5 ~ "103_N.A"),
    parcels_layout = case_when(V15a %in% c(1, 2) ~ "111_Well-defined",
                               V15a %in% c(3, 4) ~ "112_Undefined",
                               V15a == 5 ~ "113_N.A"),
    parcels_distance = case_when(V15a %in% c(1, 3) ~ "121_Present",
                                 V15a %in% c(2, 4) ~ "122_Absent",
                                 V15a == 5 ~ "123_N.A"),
    buildings_condition = case_when(V16a == 1 ~ "131_Adequate",
                                    V16a == 2 ~ "132_Mixed",
                                    V16a == 3 ~ "133_Inadequate",
                                    V16a == 4 ~ "134_N.A"),
    infrastructure_condition = case_when(V17 == 1 ~ "141_Adequate",
                                         V17 == 2 ~ "142_Partial",
                                         V17 == 3 ~ "143_Inadequate",
                                         V17 == 4 ~ "144_N.A")
  ) %>%
  rename(id = V0) %>%
  dplyr::select(
    id,
    occupation_type,
    urban_contiguity,
    establishment_time,
    real_estate,
    special_zone,
    protected_areas,
    preservation_areas,
    risk_situation,
    risk_susceptibility,
    parcels_access,
    parcels_layout,
    parcels_distance,
    buildings_condition,
    infrastructure_condition
  ) %>%
  mutate(across(2:15, ~as.factor(.)))

# elbow plot
model1 <- lapply(FUN = kmodes_elbow,
                 X = c(2:10),
                 data = as.matrix(nuis)[,-c(1:2)],
                 seeds = seq(1:100)) %>%
  lapply(FUN = unlist) %>%
  bind_cols() %>%
  t() %>%
  as_tibble() %>%
  setNames(c("mean", "sd")) %>% 
  mutate(model = c(2:10))

ggplot(model1, aes(x=model, y=mean)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
  geom_vline(xintercept = 4, linetype="dashed", size = 0.25) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(2, 10, by = 1)) +
  xlab("k") +
  ylab("dissimilarity") +
  theme(panel.grid.minor.x = element_blank())

# final model
models <- lapply(FUN = kmodes_ensemble,
                 data = nuis[,-c(1:2)],
                 modes = 4,
                 X = seq(1, 100, 1)) %>%
  bind_cols() %>%
  as.matrix()

final1 <- models %>% 
  majority_voting(is.relabelled = FALSE)

final1 <- case_when(final1 == 3 ~ "T1",
                    final1 == 4 ~ "T2",
                    final1 == 2 ~ "T3",
                    final1 == 1 ~ "T4")

output1 <- polos_nuis %>%
  mutate(clusters = final1)
write_sf(output1, "output/nuis_clusters.gpkg")

results_table <- tibble(
  variable = c(
    #names(nuis)[[2]],
    #names(nuis)[[2]],
    #names(nuis)[[2]],
    names(nuis)[[3]],
    names(nuis)[[3]],
    names(nuis)[[4]],
    names(nuis)[[4]],
    names(nuis)[[4]],
    names(nuis)[[5]],
    names(nuis)[[5]],
    names(nuis)[[5]],
    names(nuis)[[5]],
    names(nuis)[[6]],
    names(nuis)[[6]],
    names(nuis)[[6]],
    names(nuis)[[7]],
    names(nuis)[[7]],
    names(nuis)[[8]],
    names(nuis)[[8]],
    names(nuis)[[9]],
    names(nuis)[[9]],
    names(nuis)[[9]],
    names(nuis)[[10]],
    names(nuis)[[10]],
    names(nuis)[[10]],
    names(nuis)[[11]],
    names(nuis)[[11]],
    names(nuis)[[11]],
    names(nuis)[[12]],
    names(nuis)[[12]],
    names(nuis)[[12]],
    names(nuis)[[13]],
    names(nuis)[[13]],
    names(nuis)[[13]],
    names(nuis)[[14]],
    names(nuis)[[14]],
    names(nuis)[[14]],
    names(nuis)[[14]],
    names(nuis)[[15]],
    names(nuis)[[15]],
    names(nuis)[[15]],
    names(nuis)[[15]]
  ),
  categories = c(
    #names(prop.table(table(nuis[,2]))),
    names(prop.table(table(nuis[,3]))),
    names(prop.table(table(nuis[,4]))),
    names(prop.table(table(nuis[,5]))),
    names(prop.table(table(nuis[,6]))),
    names(prop.table(table(nuis[,7]))),
    names(prop.table(table(nuis[,8]))),
    names(prop.table(table(nuis[,9]))),
    names(prop.table(table(nuis[,10]))),
    names(prop.table(table(nuis[,11]))),
    names(prop.table(table(nuis[,12]))),
    names(prop.table(table(nuis[,13]))),
    names(prop.table(table(nuis[,14]))),
    names(prop.table(table(nuis[,15])))
  ),
  total = c(
    #round(prop.table(table(nuis[,2]))[[1]],2),
    #round(prop.table(table(nuis[,2]))[[2]],2),
    #round(prop.table(table(nuis[,2]))[[3]],2),
    round(prop.table(table(nuis[,3]))[[1]],2),
    round(prop.table(table(nuis[,3]))[[2]],2),
    round(prop.table(table(nuis[,4]))[[1]],2),
    round(prop.table(table(nuis[,4]))[[2]],2),
    round(prop.table(table(nuis[,4]))[[3]],2),
    round(prop.table(table(nuis[,5]))[[1]],2),
    round(prop.table(table(nuis[,5]))[[2]],2),
    round(prop.table(table(nuis[,5]))[[3]],2),
    round(prop.table(table(nuis[,5]))[[4]],2),
    round(prop.table(table(nuis[,6]))[[1]],2),
    round(prop.table(table(nuis[,6]))[[2]],2),
    round(prop.table(table(nuis[,6]))[[3]],2),
    round(prop.table(table(nuis[,7]))[[1]],2),
    round(prop.table(table(nuis[,7]))[[2]],2),
    round(prop.table(table(nuis[,8]))[[1]],2),
    round(prop.table(table(nuis[,8]))[[2]],2),
    round(prop.table(table(nuis[,9]))[[1]],2),
    round(prop.table(table(nuis[,9]))[[2]],2),
    round(prop.table(table(nuis[,9]))[[3]],2),
    round(prop.table(table(nuis[,10]))[[1]],2),
    round(prop.table(table(nuis[,10]))[[2]],2),
    round(prop.table(table(nuis[,10]))[[3]],2),
    round(prop.table(table(nuis[,11]))[[1]],2),
    round(prop.table(table(nuis[,11]))[[2]],2),
    round(prop.table(table(nuis[,11]))[[3]],2),
    round(prop.table(table(nuis[,12]))[[1]],2),
    round(prop.table(table(nuis[,12]))[[2]],2),
    round(prop.table(table(nuis[,12]))[[3]],2),
    round(prop.table(table(nuis[,13]))[[1]],2),
    round(prop.table(table(nuis[,13]))[[2]],2),
    round(prop.table(table(nuis[,13]))[[3]],2),
    round(prop.table(table(nuis[,14]))[[1]],2),
    round(prop.table(table(nuis[,14]))[[2]],2),
    round(prop.table(table(nuis[,14]))[[3]],2),
    round(prop.table(table(nuis[,14]))[[4]],2),
    round(prop.table(table(nuis[,15]))[[1]],2),
    round(prop.table(table(nuis[,15]))[[2]],2),
    round(prop.table(table(nuis[,15]))[[3]],2),
    round(prop.table(table(nuis[,15]))[[4]],2)
  ),
  C1 = c(
    #round(prop.table(table(nuis$occupation_type,final1),margin=2)[1,1],2),
    #round(prop.table(table(nuis$occupation_type,final1),margin=2)[2,1],2),
    #round(prop.table(table(nuis$occupation_type,final1),margin=2)[3,1],2),
    round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[1,1],2),
    round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[2,1],2),
    round(prop.table(table(nuis$establishment_time,final1),margin=2)[1,1],2),
    round(prop.table(table(nuis$establishment_time,final1),margin=2)[2,1],2),
    round(prop.table(table(nuis$establishment_time,final1),margin=2)[3,1],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[1,1],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[2,1],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[3,1],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[4,1],2),
    round(prop.table(table(nuis$special_zone,final1),margin=2)[1,1],2),
    round(prop.table(table(nuis$special_zone,final1),margin=2)[2,1],2),
    round(prop.table(table(nuis$special_zone,final1),margin=2)[3,1],2),
    round(prop.table(table(nuis$protected_areas,final1),margin=2)[1,1],2),
    round(prop.table(table(nuis$protected_areas,final1),margin=2)[2,1],2),
    round(prop.table(table(nuis$preservation_areas,final1),margin=2)[1,1],2),
    round(prop.table(table(nuis$preservation_areas,final1),margin=2)[2,1],2),
    round(prop.table(table(nuis$risk_situation,final1),margin=2)[1,1],2),
    round(prop.table(table(nuis$risk_situation,final1),margin=2)[2,1],2),
    round(prop.table(table(nuis$risk_situation,final1),margin=2)[3,1],2),
    round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[1,1],2),
    round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[2,1],2),
    round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[3,1],2),
    round(prop.table(table(nuis$parcels_access,final1),margin=2)[1,1],2),
    round(prop.table(table(nuis$parcels_access,final1),margin=2)[2,1],2),
    round(prop.table(table(nuis$parcels_access,final1),margin=2)[3,1],2),
    round(prop.table(table(nuis$parcels_layout,final1),margin=2)[1,1],2),
    round(prop.table(table(nuis$parcels_layout,final1),margin=2)[2,1],2),
    round(prop.table(table(nuis$parcels_layout,final1),margin=2)[3,1],2),
    round(prop.table(table(nuis$parcels_distance,final1),margin=2)[1,1],2),
    round(prop.table(table(nuis$parcels_distance,final1),margin=2)[2,1],2),
    round(prop.table(table(nuis$parcels_distance,final1),margin=2)[3,1],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[1,1],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[2,1],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[3,1],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[4,1],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[1,1],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[2,1],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[3,1],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[4,1],2)
  ),
  C2 = c(
    #round(prop.table(table(nuis$occupation_type,final1),margin=2)[1,2],2),
    #round(prop.table(table(nuis$occupation_type,final1),margin=2)[2,2],2),
    #round(prop.table(table(nuis$occupation_type,final1),margin=2)[3,2],2),
    round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[1,2],2),
    round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[2,2],2),
    round(prop.table(table(nuis$establishment_time,final1),margin=2)[1,2],2),
    round(prop.table(table(nuis$establishment_time,final1),margin=2)[2,2],2),
    round(prop.table(table(nuis$establishment_time,final1),margin=2)[3,2],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[1,2],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[2,2],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[3,2],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[4,2],2),
    round(prop.table(table(nuis$special_zone,final1),margin=2)[1,2],2),
    round(prop.table(table(nuis$special_zone,final1),margin=2)[2,2],2),
    round(prop.table(table(nuis$special_zone,final1),margin=2)[3,2],2),
    round(prop.table(table(nuis$protected_areas,final1),margin=2)[1,2],2),
    round(prop.table(table(nuis$protected_areas,final1),margin=2)[2,2],2),
    round(prop.table(table(nuis$preservation_areas,final1),margin=2)[1,2],2),
    round(prop.table(table(nuis$preservation_areas,final1),margin=2)[2,2],2),
    round(prop.table(table(nuis$risk_situation,final1),margin=2)[1,2],2),
    round(prop.table(table(nuis$risk_situation,final1),margin=2)[2,2],2),
    round(prop.table(table(nuis$risk_situation,final1),margin=2)[3,2],2),
    round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[1,2],2),
    round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[2,2],2),
    round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[3,2],2),
    round(prop.table(table(nuis$parcels_access,final1),margin=2)[1,2],2),
    round(prop.table(table(nuis$parcels_access,final1),margin=2)[2,2],2),
    round(prop.table(table(nuis$parcels_access,final1),margin=2)[3,2],2),
    round(prop.table(table(nuis$parcels_layout,final1),margin=2)[1,2],2),
    round(prop.table(table(nuis$parcels_layout,final1),margin=2)[2,2],2),
    round(prop.table(table(nuis$parcels_layout,final1),margin=2)[3,2],2),
    round(prop.table(table(nuis$parcels_distance,final1),margin=2)[1,2],2),
    round(prop.table(table(nuis$parcels_distance,final1),margin=2)[2,2],2),
    round(prop.table(table(nuis$parcels_distance,final1),margin=2)[3,2],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[1,2],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[2,2],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[3,2],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[4,2],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[1,2],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[2,2],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[3,2],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[4,2],2)
  ),
  C3 = c(
    #round(prop.table(table(nuis$occupation_type,final1),margin=2)[1,3],2),
    #round(prop.table(table(nuis$occupation_type,final1),margin=2)[2,3],2),
    #round(prop.table(table(nuis$occupation_type,final1),margin=2)[3,3],2),
    round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[1,3],2),
    round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[2,3],2),
    round(prop.table(table(nuis$establishment_time,final1),margin=2)[1,3],2),
    round(prop.table(table(nuis$establishment_time,final1),margin=2)[2,3],2),
    round(prop.table(table(nuis$establishment_time,final1),margin=2)[3,3],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[1,3],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[2,3],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[3,3],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[4,3],2),
    round(prop.table(table(nuis$special_zone,final1),margin=2)[1,3],2),
    round(prop.table(table(nuis$special_zone,final1),margin=2)[2,3],2),
    round(prop.table(table(nuis$special_zone,final1),margin=2)[3,3],2),
    round(prop.table(table(nuis$protected_areas,final1),margin=2)[1,3],2),
    round(prop.table(table(nuis$protected_areas,final1),margin=2)[2,3],2),
    round(prop.table(table(nuis$preservation_areas,final1),margin=2)[1,3],2),
    round(prop.table(table(nuis$preservation_areas,final1),margin=2)[2,3],2),
    round(prop.table(table(nuis$risk_situation,final1),margin=2)[1,3],2),
    round(prop.table(table(nuis$risk_situation,final1),margin=2)[2,3],2),
    round(prop.table(table(nuis$risk_situation,final1),margin=2)[3,3],2),
    round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[1,3],2),
    round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[2,3],2),
    round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[3,3],2),
    round(prop.table(table(nuis$parcels_access,final1),margin=2)[1,3],2),
    round(prop.table(table(nuis$parcels_access,final1),margin=2)[2,3],2),
    round(prop.table(table(nuis$parcels_access,final1),margin=2)[3,3],2),
    round(prop.table(table(nuis$parcels_layout,final1),margin=2)[1,3],2),
    round(prop.table(table(nuis$parcels_layout,final1),margin=2)[2,3],2),
    round(prop.table(table(nuis$parcels_layout,final1),margin=2)[3,3],2),
    round(prop.table(table(nuis$parcels_distance,final1),margin=2)[1,3],2),
    round(prop.table(table(nuis$parcels_distance,final1),margin=2)[2,3],2),
    round(prop.table(table(nuis$parcels_distance,final1),margin=2)[3,3],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[1,3],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[2,3],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[3,3],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[4,3],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[1,3],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[2,3],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[3,3],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[4,3],2)
  ),
  C4 = c(
    #round(prop.table(table(nuis$occupation_type,final1),margin=2)[1,4],2),
    #round(prop.table(table(nuis$occupation_type,final1),margin=2)[2,4],2),
    #round(prop.table(table(nuis$occupation_type,final1),margin=2)[3,4],2),
    round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[1,4],2),
    round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[2,4],2),
    round(prop.table(table(nuis$establishment_time,final1),margin=2)[1,4],2),
    round(prop.table(table(nuis$establishment_time,final1),margin=2)[2,4],2),
    round(prop.table(table(nuis$establishment_time,final1),margin=2)[3,4],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[1,4],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[2,4],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[3,4],2),
    round(prop.table(table(nuis$real_estate,final1),margin=2)[4,4],2),
    round(prop.table(table(nuis$special_zone,final1),margin=2)[1,4],2),
    round(prop.table(table(nuis$special_zone,final1),margin=2)[2,4],2),
    round(prop.table(table(nuis$special_zone,final1),margin=2)[3,4],2),
    round(prop.table(table(nuis$protected_areas,final1),margin=2)[1,4],2),
    round(prop.table(table(nuis$protected_areas,final1),margin=2)[2,4],2),
    round(prop.table(table(nuis$preservation_areas,final1),margin=2)[1,4],2),
    round(prop.table(table(nuis$preservation_areas,final1),margin=2)[2,4],2),
    round(prop.table(table(nuis$risk_situation,final1),margin=2)[1,4],2),
    round(prop.table(table(nuis$risk_situation,final1),margin=2)[2,4],2),
    round(prop.table(table(nuis$risk_situation,final1),margin=2)[3,4],2),
    round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[1,4],2),
    round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[2,4],2),
    round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[3,4],2),
    round(prop.table(table(nuis$parcels_access,final1),margin=2)[1,4],2),
    round(prop.table(table(nuis$parcels_access,final1),margin=2)[2,4],2),
    round(prop.table(table(nuis$parcels_access,final1),margin=2)[3,4],2),
    round(prop.table(table(nuis$parcels_layout,final1),margin=2)[1,4],2),
    round(prop.table(table(nuis$parcels_layout,final1),margin=2)[2,4],2),
    round(prop.table(table(nuis$parcels_layout,final1),margin=2)[3,4],2),
    round(prop.table(table(nuis$parcels_distance,final1),margin=2)[1,4],2),
    round(prop.table(table(nuis$parcels_distance,final1),margin=2)[2,4],2),
    round(prop.table(table(nuis$parcels_distance,final1),margin=2)[3,4],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[1,4],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[2,4],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[3,4],2),
    round(prop.table(table(nuis$buildings_condition,final1),margin=2)[4,4],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[1,4],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[2,4],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[3,4],2),
    round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[4,4],2)
  )#,
  # C5 = c(
  #   round(prop.table(table(nuis$occupation_type,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$occupation_type,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$occupation_type,final1),margin=2)[3,5],2),
  #   round(prop.table(table(nuis$establishment_time,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$establishment_time,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$establishment_time,final1),margin=2)[3,5],2),
  #   round(prop.table(table(nuis$real_estate,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$real_estate,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$real_estate,final1),margin=2)[3,5],2),
  #   round(prop.table(table(nuis$real_estate,final1),margin=2)[4,5],2),
  #   round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[3,5],2),
  #   round(prop.table(table(nuis$special_zone,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$special_zone,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$special_zone,final1),margin=2)[3,5],2),
  #   round(prop.table(table(nuis$protected_areas,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$protected_areas,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$preservation_areas,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$preservation_areas,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$risk_situation,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$risk_situation,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$risk_situation,final1),margin=2)[3,5],2),
  #   round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[3,5],2),
  #   round(prop.table(table(nuis$parcels_access,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$parcels_access,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$parcels_access,final1),margin=2)[3,5],2),
  #   round(prop.table(table(nuis$parcels_layout,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$parcels_layout,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$parcels_layout,final1),margin=2)[3,5],2),
  #   round(prop.table(table(nuis$parcels_distance,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$parcels_distance,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$parcels_distance,final1),margin=2)[3,5],2),
  #   round(prop.table(table(nuis$buildings_condition,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$buildings_condition,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$buildings_condition,final1),margin=2)[3,5],2),
  #   round(prop.table(table(nuis$buildings_condition,final1),margin=2)[4,5],2),
  #   round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[1,5],2),
  #   round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[2,5],2),
  #   round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[3,5],2),
  #   round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[4,5],2)
  # ),
  # C6 = c(
  #   round(prop.table(table(nuis$occupation_type,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$occupation_type,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$occupation_type,final1),margin=2)[3,6],2),
  #   round(prop.table(table(nuis$establishment_time,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$establishment_time,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$establishment_time,final1),margin=2)[3,6],2),
  #   round(prop.table(table(nuis$real_estate,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$real_estate,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$real_estate,final1),margin=2)[3,6],2),
  #   round(prop.table(table(nuis$real_estate,final1),margin=2)[4,6],2),
  #   round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$urban_contiguity,final1),margin=2)[3,6],2),
  #   round(prop.table(table(nuis$special_zone,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$special_zone,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$special_zone,final1),margin=2)[3,6],2),
  #   round(prop.table(table(nuis$protected_areas,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$protected_areas,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$preservation_areas,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$preservation_areas,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$risk_situation,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$risk_situation,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$risk_situation,final1),margin=2)[3,6],2),
  #   round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$risk_susceptibility,final1),margin=2)[3,6],2),
  #   round(prop.table(table(nuis$parcels_access,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$parcels_access,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$parcels_access,final1),margin=2)[3,6],2),
  #   round(prop.table(table(nuis$parcels_layout,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$parcels_layout,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$parcels_layout,final1),margin=2)[3,6],2),
  #   round(prop.table(table(nuis$parcels_distance,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$parcels_distance,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$parcels_distance,final1),margin=2)[3,6],2),
  #   round(prop.table(table(nuis$buildings_condition,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$buildings_condition,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$buildings_condition,final1),margin=2)[3,6],2),
  #   round(prop.table(table(nuis$buildings_condition,final1),margin=2)[4,6],2),
  #   round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[1,6],2),
  #   round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[2,6],2),
  #   round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[3,6],2),
  #   round(prop.table(table(nuis$infrastructure_condition,final1),margin=2)[4,6],2)
  # )
)

write_excel_csv2(results_table, "output/table1.csv")

table(final1)
round(prop.table(table(final1)),2)
write.csv2(round(prop.table(table(nuis$occupation_type, final1), margin = 2),2), "output/table4.csv")
write.csv2(round(prop.table(table(polos_nuis$V1, final1), margin = 1),2), "output/table2.csv")
write.csv2(round(prop.table(table(polos_nuis$V2b, final1), margin = 1),2), "output/table3.csv")

########
f1 <- models1[,1001:2000] %>% majority_voting(is.relabelled = FALSE)
f2 <- models1[,2001:3000] %>% majority_voting(is.relabelled = FALSE)
f3 <- models1[,3001:4000] %>% majority_voting(is.relabelled = FALSE)
f4 <- models1[,4001:5000] %>% majority_voting(is.relabelled = FALSE)
f5 <- models1[,5001:6000] %>% majority_voting(is.relabelled = FALSE)

final4 <- models1[,1:500] %>% majority_voting(is.relabelled = FALSE)
final5 <- models1[,501:1000] %>% majority_voting(is.relabelled = FALSE)
