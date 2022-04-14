# test caller

# load dependencies
source("~/Projects/acic_2022/R/validation_helper.R")
source("~/Projects/acic_2022/R/helper.r")

# create clusters
cl <- makeCluster(detectCores(logical = FALSE))

# load dependencies in cluster
rc <- clusterEvalQ(cl, source("~/Projects/acic_2022/R/validation_helper.R"))
rc <- clusterEvalQ(cl, source("~/Projects/acic_2022/R/helper.r"))

# run models for the first 100 datasets
max_dataset <- 100
res_list <- pblapply(1:max_dataset, run_one_validation, cl = cl)
res_df <- do.call(rbind, res_list)
readr::write_csv(res_df, "test_100_LM_GLM_DID.csv")

# compare with true att
check_df <- check_validation_runs(res_df)
readr::write_csv(check_df, "test_100_LM_GLM_DID_check.csv")

# run bart sequentially for the first 10 datasets
max_dataset <- 10
res_list_bart <- pblapply(1:max_dataset, run_one_validation, methods = list("BART"), cl = NULL)
res_df_bart <- do.call(rbind, res_list_bart)
readr::write_csv(res_df_bart, "test_10_BART.csv")

# compare with other models against true att
comb_df <- bind_rows(res_df, res_df_bart) %>% filter(DATASET_NUM <= 10)
comb_check_df <- check_validation_runs(comb_df)

