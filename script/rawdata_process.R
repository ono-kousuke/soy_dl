# visualize data

# packages

require(tidyverse)
require(magrittr)
require(grid)

# load functions

# read data

# geno <- read.csv("../data/raw_data/Genotype/genotype190207/Gm198_HCDB_190207.fil.snp.remHet.MS0.95_bi_MQ20_DP3-1000.MAF0.025.imputed.v2._score.csv", row.names = 1)
# pheno_chika_20180621 <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori1_groundtest/表現型_2018地下部試験破壊調査_20180621.csv"))
# pheno_chika_20180824 <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori1_groundtest/表現型_2018地下部試験破壊調査_20180824.csv"))
# pheno_shu_endo <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori2_maintest/Phenotype_2018maintest_endophyte_20181205.csv"))
# pheno_shu_ionome <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori2_maintest/Phenotype_2018maintest_ionome_20190114.csv"))
ionome_C1_ind2 <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C1_ind2_v3.csv"))
ionome_C2_ind2 <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C2_ind2_v3.csv"))
ionome_D1_ind2 <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D1_ind2_v3.csv"))
ionome_D2_ind2 <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D2_ind2_v3.csv"))
ionome_C1_ind3 <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C1_ind3_v3.csv"))
ionome_C2_ind3 <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C2_ind3_v3.csv"))
ionome_D1_ind3 <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D1_ind3_v3.csv"))
ionome_D2_ind3 <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D2_ind3_v3.csv"))
main200_raw <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori2_maintest/Phenotype_2018maintest200line_20190319.csv"))
main_plot_raw <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori2_maintest/Phenotype_2018maintest_plot_20190111.csv"))
main_plantheight_raw <- as.tibble(read.csv("../data/raw_data/Phenotype/2018/Tottori2_maintest/Phenotype_2018maintest_plantheight_20190319.csv"))

# quick view

dat <- 
  summary(dat)
glimpse(dat)

# transform

main200 <- main200_raw %>% 
  select(-年次, -場所, -系統名, -系統名_日) %>% 
  rename(LineID = 系統ID, BlockID = ブロック, PlotID = プロット番号, IndID = 個体番号, PlantHeight_0722_cm = 草丈_0722_cm,
         PlantHeight_0821_cm = 草丈_0821_cm, Leaf3FW_g = 葉3枚新鮮重_g, LeafFW_g = 葉新鮮重_g, FW_g = 地上部新鮮重_g, GrowthStage = 成長ステージ,
         NodeNmb = 節数, StemNmb = 茎数, StemLength_cm = 茎長_cm, PlantHeight_0903_cm = 草丈_0903_cm, DW_g = 地上部乾燥重_g, LeafDW_g = 葉乾燥重_g,
         LeafArea3_cm2 = 葉面積3枚_cm2) %>% 
  mutate(ID = as.factor(ID), LineID = as.factor(LineID))

allnames <- main_plot_raw %>% 
  names() %>% 
  str_replace("植被面積", "PlantCoveringArea") %>% 
  str_replace("草丈", "PlantHeight")
main_plot <- main_plot_raw %>% 
  set_colnames(allnames) %>% 
  select(-年次, -場所, -系統名, -系統名_日, -個体番号) %>% 
  rename(LineID = 系統ID, BlockID = ブロック, PlotID = プロット番号, FloweringDay = 開花日) %>% 
  mutate(ID = as.factor(ID), LineID = as.factor(LineID), FloweringDay = as.Date(FloweringDay))

allnames <- main_plantheight_raw %>% 
  names() %>% 
  str_replace("草丈", "PlantHeight")
main_plantheight <- main_plantheight_raw %>% 
  set_colnames(allnames) %>% 
  select(-年次, -場所, -系統名, -系統名_日) %>% 
  rename(LineID = 系統ID, BlockID = ブロック, PlotID = プロット番号, IndID = 個体番号) %>% 
  mutate(dat.ID = as.factor(dat.ID), LineID = as.factor(LineID))

# BlockID transform c >> C and d >> D
ionome_C1_ind2_process <- ionome_C1_ind2 %>% 
  mutate(BlockID = as.factor("C1")) %>% 
  rename(LineID = line)
ionome_C1_ind3_process <- ionome_C1_ind3 %>% 
  mutate(BlockID = as.factor("C1")) %>% 
  rename(LineID = line)
ionome_C2_ind2_process <- ionome_C2_ind2 %>% 
  mutate(BlockID = as.factor("C2")) %>% 
  rename(LineID = line)
ionome_C2_ind3_process <- ionome_C2_ind3 %>% 
  mutate(BlockID = as.factor("C2")) %>% 
  rename(LineID = line)
ionome_D1_ind2_process <- ionome_D1_ind2 %>% 
  mutate(BlockID = as.factor("D1")) %>% 
  rename(LineID = line)
ionome_D1_ind3_process <- ionome_D1_ind3 %>% 
  mutate(BlockID = as.factor("D1")) %>% 
  rename(LineID = line)
ionome_D2_ind2_process <- ionome_D2_ind2 %>% 
  mutate(BlockID = as.factor("D2")) %>% 
  rename(LineID = line)
ionome_D2_ind3_process <- ionome_D2_ind3 %>% 
  mutate(BlockID = as.factor("D2")) %>% 
  rename(LineID = line)


# write processed data

write.csv(main200, file = "../data/preprocess_data/Phenotype/2018/Tottori2_maintest/Phenotype_2018maintest200line_20190319_preprocess.csv", row.names = FALSE)
write.csv(main_plot, file = "../data/preprocess_data/Phenotype/2018/Tottori2_maintest/Phenotype_2018maintest_plot_20190111_preprocess.csv", row.names = FALSE)
write.csv(main_plantheight, file = "../data/preprocess_data/Phenotype/2018/Tottori2_maintest/Phenotype_2018maintest_plantheight_20190319_preprocess.csv", row.names = FALSE)
write.csv(ionome_C1_ind2_process, file = "../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C1_ind2_v3_preprocess.csv", row.names = FALSE)
write.csv(ionome_C1_ind3_process, file = "../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C1_ind3_v3_preprocess.csv", row.names = FALSE)
write.csv(ionome_C2_ind2_process, file = "../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C2_ind2_v3_preprocess.csv", row.names = FALSE)
write.csv(ionome_C2_ind3_process, file = "../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_C2_ind3_v3_preprocess.csv", row.names = FALSE)
write.csv(ionome_D1_ind2_process, file = "../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D1_ind2_v3_preprocess.csv", row.names = FALSE)
write.csv(ionome_D1_ind3_process, file = "../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D1_ind3_v3_preprocess.csv", row.names = FALSE)
write.csv(ionome_D2_ind2_process, file = "../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D2_ind2_v3_preprocess.csv", row.names = FALSE)
write.csv(ionome_D2_ind3_process, file = "../data/preprocess_data/Phenotype/2018/Tottori2_maintest/ionome_2018maintest(200line_ionometimeline5_endophyte)/2018_Sep_D2_ind3_v3_preprocess.csv", row.names = FALSE)

save(main200, main_plot, main_plantheight, file = "../data/preprocess_data/Phenotype/2018/Tottori2_maintest/data_preprocess.R")
