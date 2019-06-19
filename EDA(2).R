# EDA

# packages

require(tidyverse)
require(magrittr)
require(caret)


# load functions


# read data

ionome_C1_ind2 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C1_ind2_v3_preprocess.csv"))
ionome_C2_ind2 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C2_ind2_v3_preprocess.csv"))
ionome_D1_ind2 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D1_ind2_v3_preprocess.csv"))
ionome_D2_ind2 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D2_ind2_v3_preprocess.csv"))
ionome_C1_ind3 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C1_ind3_v3_preprocess.csv"))
ionome_C2_ind3 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C2_ind3_v3_preprocess.csv"))
ionome_D1_ind3 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D1_ind3_v3_preprocess.csv"))
ionome_D2_ind3 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D2_ind3_v3_preprocess.csv"))

load("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/data_preprocess.R") # main200, main_plot, main_plantheight

  

## ionome analysis
##### overview of each block's ionome data ##### 
#### apparently, there are differences between Ind2 and Ind3 in D1 environment. 
#### in other environments, there are no defferences between 2 and 3, though.
#### Cd and Rb have lots of 0 data, which seems to be due to failures.

# C1 ionome histogram
pdf("../data/plot/overview/ionome_distribution_C1.pdf", width = 10, height = 7)
ionome_C1_ind2 %>% 
  bind_rows(ionome_C1_ind3) %>% 
  mutate(IndID = as.factor(IndID)) %>% 
  drop_na() %>%
  select(-(line:PlotID)) %>% 
  gather(key = "ion", value = value, -IndID) %>% 
  ggplot(aes(x = value, fill = IndID)) +
  geom_histogram(position = "identity", bins = 30, alpha = 0.7) + 
  facet_wrap(~ ion, scales = "free")
dev.off()

# C2 ionome histogram
pdf("../data/plot/overview/ionome_distribution_C2.pdf", width = 10, height = 7)
ionome_C2_ind2 %>% 
  bind_rows(ionome_C2_ind3) %>% 
  mutate(IndID = as.factor(IndID)) %>% 
  drop_na() %>%
  select(-(line:PlotID)) %>% 
  gather(key = "ion", value = value, -IndID) %>% 
  ggplot(aes(x = value, fill = IndID)) +
  geom_histogram(position = "identity", bins = 30, alpha = 0.7) + 
  facet_wrap(~ ion, scales = "free")
dev.off()

# D1 ionome histogram
pdf("../data/plot/overview/ionome_distribution_D1.pdf", width = 10, height = 7)
ionome_D1_ind2 %>% 
  bind_rows(ionome_D1_ind3) %>% 
  mutate(IndID = as.factor(IndID)) %>% 
  drop_na() %>%
  select(-(line:PlotID)) %>% 
  gather(key = "ion", value = value, -IndID) %>% 
  ggplot(aes(x = value, fill = IndID)) +
  geom_histogram(position = "identity", bins = 30, alpha = 0.7) + 
  facet_wrap(~ ion, scales = "free")
dev.off()

# D2 ionome histogram
pdf("../data/plot/overview/ionome_distribution_D2.pdf", width = 10, height = 7)
ionome_D2_ind2 %>% 
  bind_rows(ionome_D2_ind3) %>% 
  mutate(IndID = as.factor(IndID)) %>% 
  drop_na() %>%
  select(-(line:PlotID)) %>% 
  gather(key = "ion", value = value, -IndID) %>% 
  ggplot(aes(x = value, fill = IndID)) +
  geom_histogram(position = "identity", bins = 30, alpha = 0.7) + 
  facet_wrap(~ ion, scales = "free")
dev.off()


##### compare C1 and C2 ionome histogram #####
# warnings can be ignored

pdf("../data/plot/overview/compare_ionome_distribution_C1_and_C2.pdf", width = 10, height = 7)
ionome_C1_ind2 %>% 
  bind_rows(ionome_C2_ind2) %>% 
  mutate(BlockID = as.factor(BlockID)) %>% 
  drop_na() %>%
  select(-line, -PlotID) %>% 
  gather(key = "ion", value = value, -(BlockID:IndID)) %>% 
  ggplot(aes(x = value, fill = BlockID)) +
  geom_histogram(position = "identity", bins = 23, alpha = 0.7) + 
  facet_wrap(~ ion, scales = "free")
dev.off()

#### t-test
ion_name <- colnames(select(ionome_C1_ind2, -(line:IndID)))
pvalues <- rep(NA, length(ion_name))
names(pvalues) <- ion_name
for(ion in ion_name){
  ion_C1 <- ionome_C1_ind2 %>% 
    select(ion) %>% 
    drop_na() %>% 
    .[[1]]
  assign(paste0(ion, "_C1"), ion_C1)
  ion_C2 <- ionome_C2_ind2 %>% 
    select(ion) %>% 
    drop_na() %>% 
    .[[1]]
  assign(paste0(ion, "_C2"), ion_C2)
  pvalues[ion] <- t.test(ion_C1, ion_C2)$p.value
}
pvalues
#### there are some significant differenes (Br, Cl, Cs, Fe, Mn, Mo, Ni, P, S, Sr, Zn)

##### compare D1 and D2 ionome histogram #####
#### warnings can be ignored
#### compare D1(ind3) and D2(ind2) (there are smaller differences than D1(ind2) and D2(ind3))

pdf("../data/plot/overview/compare_ionome_distribution_D1_and_D2.pdf", width = 10, height = 7)
ionome_D1_ind3 %>% 
  bind_rows(ionome_D2_ind2) %>% 
  mutate(BlockID = as.factor(BlockID)) %>% 
  drop_na() %>%
  select(-line, -PlotID) %>% 
  gather(key = "ion", value = value, -(BlockID:IndID)) %>% 
  ggplot(aes(x = value, fill = BlockID)) +
  geom_histogram(position = "identity", bins = 23, alpha = 0.7) + 
  facet_wrap(~ ion, scales = "free")
dev.off()

#### t-test
ion_name <- colnames(select(ionome_D1_ind3, -(line:IndID)))
pvalues <- rep(NA, length(ion_name))
names(pvalues) <- ion_name
for(ion in ion_name){
  ion_D1 <- ionome_D1_ind3 %>% 
    select(ion) %>% 
    drop_na() %>% 
    .[[1]]
  assign(paste0(ion, "_D1"), ion_D1)
  ion_D2 <- ionome_D2_ind2 %>% 
    select(ion) %>% 
    drop_na() %>% 
    .[[1]]
  assign(paste0(ion, "_D2"), ion_D2)
  pvalues[ion] <- t.test(ion_D1, ion_D2)$p.value
}
pvalues
#### there are some significant differenes (Br, Fe, K, Ni, Zn)


# compare any (Env, ind) combination
# setting
Env1 <- "D1"; ind1 <- "2"; Env2 <- "D2"; ind2 <- "2";
comp <- "BlockID"
#
data1 <- paste0("ionome_", Env1, "_ind", ind1)
data2 <- paste0("ionome_", Env2, "_ind", ind2)

# histogram
if(comp == "IndID"){
  data1 %>% 
    get() %>% 
    bind_rows(get(data2)) %>% 
    mutate(BlockID = as.factor(BlockID), IndID = as.factor(IndID)) %>% 
    drop_na() %>%
    select(-line, -PlotID) %>% 
    gather(key = "ion", value = value, -(BlockID:IndID)) %>% 
    ggplot(aes(x = value, fill = IndID)) +                             ############## choose fill = IndID or BlockID
    geom_histogram(position = "identity", bins = 23, alpha = 0.7) + 
    facet_wrap(~ ion, scales = "free")
}else{
  data1 %>% 
    get() %>% 
    bind_rows(get(data2)) %>% 
    mutate(BlockID = as.factor(BlockID), IndID = as.factor(IndID)) %>% 
    drop_na() %>%
    select(-line, -PlotID) %>% 
    gather(key = "ion", value = value, -(BlockID:IndID)) %>% 
    ggplot(aes(x = value, fill = BlockID)) +                             ############## choose fill = IndID or BlockID
    geom_histogram(position = "identity", bins = 23, alpha = 0.7) + 
    facet_wrap(~ ion, scales = "free")
}

# t-test
ion_name <- data1 %>% 
  get() %>% 
  select(-(line:IndID)) %>% 
  colnames()
pvalues <- rep(NA, length(ion_name))
names(pvalues) <- ion_name
for(ion in ion_name){
  ion_Env1 <- data1 %>% 
    get() %>% 
    select(ion) %>% 
    drop_na() %>% 
    .[[1]]
  ion_Env2 <- data2 %>% 
    get() %>% 
    select(ion) %>% 
    drop_na() %>% 
    .[[1]]
  pvalues[ion] <- t.test(ion_Env1, ion_Env2)$p.value
}
pvalues


# C1 C2 Phenotype histogram
pdf("../data/plot/overview/compare_Phenotype_distribution_C1_and_C2_histogram.pdf", width = 10, height = 7)
main200 %>% 
  filter(block %in% c("C1", "C2")) %>% 
  select(-(GrowthStage), -NodeNmb, -PlantHeight_0722_cm, -PlantHeight_0821_cm, -PlantHeight_0903_cm, -StemNmb, -StemLength_cm) %>% 
  gather(key = "phenotype", value = "value", -(ID:ind_nmb)) %>% 
  ggplot(aes(x = value, fill = block)) +
  geom_histogram(position = "identity", alpha = 0.7) +
  facet_wrap(~ phenotype, scale = "free")
dev.off()

# C1 C2 Phenotype bar
pdf("../data/plot/overview/compare_Phenotype_distribution_C1_and_C2_bar.pdf", width = 10, height = 7)
main200 %>% 
  filter(block %in% c("C1", "C2")) %>% 
  select(c(ID:ind_nmb), GrowthStage, NodeNmb, PlantHeight_0722_cm, PlantHeight_0821_cm, PlantHeight_0903_cm, StemNmb, StemLength_cm) %>% 
  gather(key = "phenotype", value = "value", -(ID:ind_nmb)) %>% 
  ggplot(aes(x = value, fill = block)) +
  geom_bar(position = "identity", alpha = 0.7) +
  facet_wrap(~ phenotype, scale = "free")
dev.off()

# D1 D2 Phenotype histogram
pdf("../data/plot/overview/compare_Phenotype_distribution_D1_and_D2_histogram.pdf", width = 10, height = 7)
main200 %>% 
  filter(block %in% c("D1", "D2")) %>% 
  select(-(GrowthStage), -NodeNmb, -PlantHeight_0722_cm, -PlantHeight_0821_cm, -PlantHeight_0903_cm, -StemNmb, -StemLength_cm) %>% 
  gather(key = "phenotype", value = "value", -(ID:ind_nmb)) %>% 
  ggplot(aes(x = value, fill = block)) +
  geom_histogram(position = "identity", alpha = 0.7) +
  facet_wrap(~ phenotype, scale = "free")
dev.off()

# D1 D2 Phenotype bar
pdf("../data/plot/overview/compare_Phenotype_distribution_D1_and_D2_bar.pdf", width = 10, height = 7)
main200 %>% 
  filter(block %in% c("D1", "D2")) %>% 
  select(c(ID:ind_nmb), GrowthStage, NodeNmb, PlantHeight_0722_cm, PlantHeight_0821_cm, PlantHeight_0903_cm, StemNmb, StemLength_cm) %>% 
  gather(key = "phenotype", value = "value", -(ID:ind_nmb)) %>% 
  ggplot(aes(x = value, fill = block)) +
  geom_bar(position = "identity", alpha = 0.7) +
  facet_wrap(~ phenotype, scale = "free")
dev.off()

# C1 D1 Phenotype histogram
pdf("../data/plot/overview/compare_Phenotype_distribution_C1_and_D1_histogram.pdf", width = 10, height = 7)
main200 %>% 
  filter(block %in% c("C1", "D1")) %>% 
  select(-(GrowthStage), -NodeNmb, -PlantHeight_0722_cm, -PlantHeight_0821_cm, -PlantHeight_0903_cm, -StemNmb, -StemLength_cm) %>% 
  gather(key = "phenotype", value = "value", -(ID:ind_nmb)) %>% 
  ggplot(aes(x = value, fill = block)) +
  geom_histogram(position = "identity", alpha = 0.7) +
  facet_wrap(~ phenotype, scale = "free")
dev.off()

# C1 D1 Phenotype bar
pdf("../data/plot/overview/compare_Phenotype_distribution_C1_and_D1_bar.pdf", width = 10, height = 7)
main200 %>% 
  filter(block %in% c("C1", "D1")) %>% 
  select(c(ID:ind_nmb), GrowthStage, NodeNmb, PlantHeight_0722_cm, PlantHeight_0821_cm, PlantHeight_0903_cm, StemNmb, StemLength_cm) %>% 
  gather(key = "phenotype", value = "value", -(ID:ind_nmb)) %>% 
  ggplot(aes(x = value, fill = block)) +
  geom_bar(position = "identity", alpha = 0.7) +
  facet_wrap(~ phenotype, scale = "free")
dev.off()

# regression of phenotype on ionome
ion_main_join <- ionome_C1_ind2 %>% 
  bind_rows(ionome_C1_ind3, ionome_C2_ind2, ionome_C2_ind3, ionome_D1_ind2, ionome_D1_ind3, ionome_D2_ind2, ionome_D2_ind3) %>% 
  mutate(LineID = as.factor(LineID)) %>% 
  select(-LineID) %>% 
  left_join(main200) %>% 
  mutate(LineID = as.factor(LineID), BlockID = as.factor(BlockID), IndID = as.factor(IndID))
data.tmp <- ion_main_join %>% 
  select(As:Zn, NodeNmb) %>% 
  drop_na() %>% 
  as.data.frame() 

models <- c("lm", "ridge", "lasso", "pcr", "pls")   # methods for itelator
mse <- rep(NA, length(models))   # vector for each mse
names(mse) <- models

# train ("lm", "ridge", "lasso", "pcr", "pls")
for(method in models){
  tr.control <- trainControl(method = "cv", number = 10)
  model <- data.tmp %>% 
    train(NodeNmb ~ ., data = ., method = method, 
          preProces = "scale", trControl = tr.control)
  #mse[method] <- mean((select(data.tmp, NodeNmb) - predict(model, test.data))^2)
  print(model)
}
model <- lm(NodeNmb ~ ., data = data.tmp)
stepresult <- step(model)
stepresult

# ionome correlation
cor1 <- ionome_C1_ind2 %>% 
  select(-(LineID:IndID)) %>% 
  drop_na() %>% 
  scale() %>% 
  cor()
  
corall <- ionome_C1_ind2 %>% 
  bind_rows(ionome_C1_ind3, ionome_C2_ind2, ionome_C2_ind3, ionome_D1_ind2, ionome_D1_ind3, ionome_D2_ind2, ionome_D2_ind3) %>% 
  select(-(LineID:IndID)) %>% 
  drop_na() %>% 
  scale() %>% 
  cor()

(cor1 - corall) %>% 
  heatmap(Colv = NA, Rowv = NA)

# scatter plot ind2 vs ind3 of ionome data
ionome_D1_ind2 %>% 
  bind_rows(ionome_D1_ind3) %>% 
  mutate(IndID = as.factor(IndID)) %>% 
  drop_na() %>% 
  select(-(BlockID:PlotID)) %>% 
  gather(key = "ion", value = value, -(LineID:IndID)) %>% 
  spread(key = IndID, value = value) %>% 
  drop_na() %>% 
  rename(ind2 = '2', ind3 = '3') %>% 
  ggplot(aes(x = ind2, y = ind3)) +
  geom_point() + 
  facet_wrap(~ ion, scales = "free")

# correlation
ionome_D1_ind2 %>% 
  bind_rows(ionome_D1_ind3) %>% 
  mutate(IndID = as.factor(IndID)) %>% 
  drop_na() %>% 
  select(-(BlockID:PlotID)) %>% 
  gather(key = "ion", value = value, -(LineID:IndID)) %>% 
  spread(key = IndID, value = value) %>%
  drop_na() %>% 
  rename(ind2 = '2', ind3 = '3') %>% 
  group_by(ion) %>% 
  do(
    cor = cor(.$ind2, .$ind3)
  ) %>% 
  select(cor) %>% 
  .[[1]]
#### low correlation between ind2 and ind3, useless!!


# scatter plot ind2 vs ind3 of phenotype data
main200 %>% 
  filter(IndID %in% c(2, 3), BlockID == "C1") %>% 
  mutate(IndID = as.factor(IndID)) %>% 
  select(-(ID:BlockID)) %>% 
  gather(key = "Phenotype", value = value, -(PlotID:IndID)) %>% 
  spread(key = IndID, value = value) %>% 
  drop_na() %>% 
  rename(ind2 = '2', ind3 = '3') %>% 
  ggplot(aes(x = ind2, y = ind3)) +
  geom_point() + 
  facet_wrap(~ Phenotype, scales = "free")

# correlation
main200 %>% 
  filter(IndID %in% c(2, 3), BlockID == "C1") %>% 
  mutate(IndID = as.factor(IndID)) %>% 
  select(-(ID:BlockID)) %>% 
  gather(key = "Phenotype", value = value, -(PlotID:IndID)) %>% 
  spread(key = IndID, value = value) %>% 
  drop_na() %>% 
  rename(ind2 = '2', ind3 = '3') %>% 
  group_by(Phenotype) %>% 
  do(
    cor = cor(.$ind2, .$ind3)
  ) %>% 
  select(cor) %>% 
  .[[1]]
#### high correlation


# # 主試験20180621
# # 茎長
# 
# g1 <- pheno_chika_20180621 %>% 
#   drop_na() %>% 
#   ggplot(aes(x = 茎長_cm, fill = block)) +
#   geom_bar(position = "identity", alpha = 0.7)
# 
# # 新鮮重
# g3 <- pheno_chika_20180621 %>%
#   drop_na() %>% 
#   ggplot(aes(x = 地上部新鮮重_g, fill = block)) +
#   geom_histogram(bins = 30, position = "identity", alpha = 0.7)
# 
# # 主試験20180824
# # 茎長
# g2 <- pheno_chika_20180824 %>%
#   drop_na() %>% 
#   ggplot(aes(x = 茎長_cm, fill = block)) +
#   geom_bar(position = "identity", alpha = 0.7)
# 
# # 新鮮重
# g4 <- pheno_chika_20180621 %>%
#   drop_na() %>% 
#   ggplot(aes(x = 地上部新鮮重_g, fill = block)) +
#   geom_histogram(bins = 30, position = "identity", alpha = 0.7)
# 
# multiplot(g1, g3, cols = 2)
# 
# # 地下部試験
# g1 <- pheno_shu_endo %>% 
#   select(地上部新鮮重_g, block) %>% 
#   rename(FW_g = 地上部新鮮重_g) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = FW_g, fill = block)) + 
#   geom_histogram(bins = 40, position = "identity", alpha = 0.7)
# 
# g2 <- pheno_shu_endo %>% 
#   select(節数, block) %>%
#   rename(node_number = 節数) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = node_number, fill = block)) +
#   geom_bar(position = "identity", alpha = 0.7)
# 
# g3 <- pheno_shu_endo %>% 
#   select(葉3枚新鮮重_g, block) %>% 
#   rename(leaf3FW_g = 葉3枚新鮮重_g) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = leaf3FW_g, fill = block)) +
#   geom_histogram(bins = 20, position = "identity", alpha = 0.7)
# 
# g4 <- pheno_shu_endo %>% 
#   select(葉新鮮重_g, block) %>% 
#   rename(leafFW_g = 葉新鮮重_g) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = leafFW_g, fill = block)) +
#   geom_histogram(bins = 30, position = "identity", alpha = 0.7)
# 
# g5 <- pheno_shu_endo %>% 
#   select(茎数, block) %>%
#   rename(stem_number = 茎数) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = stem_number, fill = block)) +
#   geom_bar(position = "identity", alpha = 0.7)
# 
# g6 <- pheno_shu_endo %>% 
#   select(茎長_cm, block) %>% 
#   rename(stem_length_cm = 茎長_cm) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = stem_length_cm, fill = block)) +
#   geom_bar(position = "identity", alpha = 0.7)
# 
# pdf("../data/plot/overview/phenotype_distribution_endo.pdf", width = 12, height = 7)
# multiplot(g1, g2, g3, g4, g5, g6, cols = 3)
# dev.off()
# 
# # ionome
# g1 <- pheno_shu_ionome %>% 
#   select(地上部新鮮重_g, block) %>% 
#   rename(FW_g = 地上部新鮮重_g) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = FW_g, fill = block)) + 
#   geom_histogram(bins = 40, position = "identity", alpha = 0.7)
# 
# g2 <- pheno_shu_ionome %>% 
#   select(節数, block) %>%
#   rename(node_number = 節数) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = node_number, fill = block)) +
#   geom_bar(position = "identity", alpha = 0.7)
# 
# g3 <- pheno_shu_ionome %>% 
#   select(地上部乾燥重_g, block) %>% 
#   rename(DW_g = 地上部乾燥重_g) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = DW_g, fill = block)) +
#   geom_histogram(bins = 20, position = "identity", alpha = 0.7)
# 
# g4 <- pheno_shu_ionome %>% 
#   select(茎数, block) %>%
#   rename(stem_number = 茎数) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = stem_number, fill = block)) +
#   geom_bar(position = "identity", alpha = 0.7)
# 
# g5 <- pheno_shu_ionome %>% 
#   select(茎長_cm, block) %>% 
#   rename(stem_length_cm = 茎長_cm) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = stem_length_cm, fill = block)) +
#   geom_bar(position = "identity", alpha = 0.7)
# 
# g6 <- pheno_shu_ionome %>% 
#   select(草丈_0722_cm, block) %>% 
#   rename(plant_height_0722_cm = 草丈_0722_cm) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = plant_height_0722_cm, fill = block)) +
#   geom_bar(position = "identity", alpha = 0.7)
# 
# g7 <- pheno_shu_ionome %>% 
#   select(草丈_0821_cm, block) %>% 
#   rename(plant_height_0821_cm = 草丈_0821_cm) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = plant_height_0821_cm, fill = block)) +
#   geom_bar(position = "identity", alpha = 0.7)
# 
# g8 <- pheno_shu_ionome %>% 
#   select(草丈_0903_cm, block) %>% 
#   rename(plant_height_0903_cm = 草丈_0903_cm) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = plant_height_0903_cm, fill = block)) +
#   geom_bar(position = "identity", alpha = 0.7)
# 
# pdf("../data/plot/overview/phenotype_distribution_ionome.pdf", width = 12, height = 7)
# multiplot(g1,g2,g3,g4,g5,g6,g7,g8,cols=3)
# dev.off()
# 
# pheno_shu_ionome %>% 
#   select(草丈_0722_cm, 系統ID, block) %>% 
#   #filter(block==c("C1","C2")) %>% 
#   rename(plant_height_0722_cm = 草丈_0722_cm) %>% 
#   drop_na() %>% 
#   ggplot(aes(x = plant_height_0722_cm, fill = 系統ID)) +
#   geom_bar(position = "identity", alpha = 0.7)
