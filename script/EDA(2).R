# EDA

# packages

require(Rmisc)
require(tidyverse)
require(magrittr)
require(grid)


# load functions


# read data

ionome_C1_ind2 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C1_ind2_v3.csv"))
ionome_C2_ind2 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C2_ind2_v3.csv"))
ionome_D1_ind2 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D1_ind2_v3.csv"))
ionome_D2_ind2 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D2_ind2_v3.csv"))
ionome_C1_ind3 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C1_ind3_v3.csv"))
ionome_C2_ind3 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C2_ind3_v3.csv"))
ionome_D1_ind3 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D1_ind3_v3.csv"))
ionome_D2_ind3 <- as.tibble(read.csv("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D2_ind3_v3.csv"))

load("../data/preprocess_data/Phenotype/2018/Tottori2_maintest/data_preprocess.R") # main200, main_plot, main_plantheight

  

## ionome analysis
##### overview of each block's ionome data ##### 
#### apparently, there are differences between Ind2 and Ind3 in D1 environment. 
#### in other environments, there are no defferences between 2 and 3, though.
#### Cd and Rb have lots of 0 data, which seems to be due to failures.

# C1 ionome histogram
ion_name <- colnames(select(ionome_C1_ind2, -(line:IndID)))
for(i in 1:length(ion_name)){
  ion <- ion_name[i]
  tmp_data <- ionome_C1_ind2 %>% 
    bind_rows(ionome_C1_ind3) %>% 
    mutate(IndID = as.factor(IndID)) %>% 
    drop_na() %>%
    select(ion, IndID) %>% 
    set_colnames(c("a", "IndID"))
  label <- tmp_data %>% nrow() %>% as.character()
  label_x <- tmp_data %>% .[[1]] %>% max()
  g <- tmp_data %>% 
    ggplot(aes(x = a, fill = IndID)) +
    geom_histogram(bins = 23, position = "identity", alpha = 0.7) +
    xlab(ion) +
    annotate("text", x = label_x, y = 10, label = label)
  assign(paste0("g", as.character(i)), g)
  rm(tmp_data)
}
pdf("../data/plot/overview/ionome_distribution_C1.pdf", width = 10, height = 7)
multiplot(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19, cols = 5)
dev.off()
rm(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19)

# C2 ionome histogram
ion_name <- colnames(select(ionome_C2_ind2, -(line:IndID)))
for(i in 1:length(ion_name)){
  ion <- ion_name[i]
  tmp_data <- ionome_C2_ind2 %>% 
    bind_rows(ionome_C2_ind3) %>% 
    mutate(IndID = as.factor(IndID)) %>% 
    drop_na() %>%
    select(ion, IndID) %>% 
    set_colnames(c("a", "IndID"))
  label <- tmp_data %>% nrow() %>% as.character()
  label_x <- tmp_data %>% .[[1]] %>% max()
  g <- tmp_data %>% 
    ggplot(aes(x = a, fill = IndID)) +
    geom_histogram(bins = 23, position = "identity", alpha = 0.7) +
    xlab(ion) +
    annotate("text", x = label_x, y = 10, label = label)
  assign(paste0("g", as.character(i)), g)
  rm(tmp_data)
}
pdf("../data/plot/overview/ionome_distribution_C2.pdf", width = 10, height = 7)
multiplot(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19, cols = 5)
dev.off()
rm(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19)

# D1 ionome histogram
ion_name <- colnames(select(ionome_D1_ind2, -(line:IndID)))
for(i in 1:length(ion_name)){
  ion <- ion_name[i]
  tmp_data <- ionome_D1_ind2 %>% 
    bind_rows(ionome_D1_ind3) %>% 
    mutate(IndID = as.factor(IndID)) %>% 
    drop_na() %>%
    select(ion, IndID) %>% 
    set_colnames(c("a", "IndID"))
  label <- tmp_data %>% nrow() %>% as.character()
  label_x <- tmp_data %>% .[[1]] %>% max()
  g <- tmp_data %>% 
    ggplot(aes(x = a, fill = IndID)) +
    geom_histogram(bins = 23, position = "identity", alpha = 0.7) +
    xlab(ion) +
    annotate("text", x = label_x, y = 10, label = label)
  assign(paste0("g", as.character(i)), g)
  rm(tmp_data)
}
pdf("../data/plot/overview/ionome_distribution_D1.pdf", width = 10, height = 7)
multiplot(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19, cols = 5)
dev.off()
rm(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19)

# D2 ionome histogram
ion_name <- colnames(select(ionome_D2_ind2, -(line:IndID)))
for(i in 1:length(ion_name)){
  ion <- ion_name[i]
  tmp_data <- ionome_D2_ind2 %>% 
    bind_rows(ionome_D2_ind3) %>% 
    mutate(IndID = as.factor(IndID)) %>% 
    drop_na() %>%
    select(ion, IndID) %>% 
    set_colnames(c("a", "IndID"))
  label <- tmp_data %>% nrow() %>% as.character()
  label_x <- tmp_data %>% .[[1]] %>% max()
  g <- tmp_data %>% 
    ggplot(aes(x = a, fill = IndID)) +
    geom_histogram(bins = 23, position = "identity", alpha = 0.7) +
    xlab(ion) +
    annotate("text", x = label_x, y = 10, label = label)
  assign(paste0("g", as.character(i)), g)
  rm(tmp_data)
}
pdf("../data/plot/overview/ionome_distribution_D2.pdf", width = 10, height = 7)
multiplot(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19, cols = 5)
dev.off()
rm(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19)



##### compare C1 and C2 ionome histogram #####
# warnings can be ignored
ion_name <- colnames(select(ionome_C1_ind2, -(line:IndID)))
for(i in 1:length(ion_name)){
  ion <- ion_name[i]
  tmp_data <- ionome_C1_ind2 %>% 
    bind_rows(ionome_C2_ind2) %>% 
    mutate(BlockID = as.factor(BlockID)) %>% 
    drop_na() %>%
    select(ion, BlockID) %>% 
    set_colnames(c("a", "BlockID"))
  label <- tmp_data %>% nrow() %>% as.character()
  label_x <- tmp_data %>% .[[1]] %>% max()
  g <- tmp_data %>% 
    ggplot(aes(x = a, fill = BlockID)) +
    geom_histogram(bins = 23, position = "identity", alpha = 0.7) +
    xlab(ion) +
    annotate("text", x = label_x, y = 10, label = label)
  assign(paste0("g", as.character(i)), g)
  rm(tmp_data)
}
pdf("../data/plot/overview/ionome_distribution_C1.pdf", width = 10, height = 7)
multiplot(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19, cols = 5)
dev.off()
rm(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19)

#### there might be some variation in Br and Cl...?
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
#### there are some significant differenes



all_ion <- bind_rows(list(ionome_C1_ind2,ionome_C1_ind3,ionome_C2_ind2,ionome_C2_ind3,ionome_D1_ind2,ionome_D1_ind3,ionome_D2_ind2,ionome_D2_ind3)) %>% 
  mutate(BlockID = as.factor(BlockID))  
ion_name <- colnames(select(all_ion, -(line:IndID)))
for(i in 1:2){
  ion <- ion_name[i]
  tmp_data <- all_ion %>% 
    drop_na() %>%
    select(ion, BlockID) %>% 
    set_colnames("a")
  label <- tmp_data %>% nrow() %>% as.character()
  label_x <- tmp_data %>% .[[1]] %>% max()
  g <- tmp_data %>% 
    ggplot(aes(x=a, fill = BlockID)) +
    geom_histogram(bins = 23, position = "identity", alpha = 0.5) +
    xlab(ion) +
    scale_fill_manual(values = c("#0000ee", "#0000ee", "#ee0000", "#ee0000"))
  assign(paste0("g", as.character(i)), g)
  rm(tmp_data)
}

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
