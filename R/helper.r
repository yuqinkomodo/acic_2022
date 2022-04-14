# acic_2022 helper functions
# This includes functions to use for the main challange

# load packages
library(tidyverse) # data ETL
library(WeightIt) # IPTW weight
library(did) # for DID
library(dbarts) # for bart
library(cobalt) # descrpitive cohort summary
library(tlverse) # in case we have time to explore tmle
library(pbapply) # parallel processing with progress bar
library(parallel) # parallel computing

# data loader - probably only works on my own machine...
load_data <- function(
  dataset = 1, 
  root = "/Users/yuqin.wei/Projects/Data_Challenge_2022/NewData",
  fix_neg = T,
  format = c("wide", "long")
  ){
  # YW: I think the data loader will need some massive change to load data from S3.
  format = match.arg(format)
  require(tidyverse)
  
  dataset_str <- stringr::str_pad(dataset, 4, pad = "0")
  
  # find subdir with the dataset
  subdir_lst <- paste0("track1", c("a", "b", "c"), "_20220404")
  subdir <- subdir_lst[sapply(subdir_lst, function(d) {
    file.exists(file.path(root, d, "patient", paste0("acic_patient_", dataset_str, ".csv")))
  })]
  
  # load four pieces
  patient <- read_csv(file.path(root, subdir, "patient", paste0("acic_patient_", dataset_str, ".csv")), col_types = "ccdiidf")
  patient_year <- read_csv(file.path(root, subdir, "patient_year", paste0("acic_patient_year_", dataset_str, ".csv")), col_types = "cid")
  practice <- read_csv(file.path(root, subdir, "practice", paste0("acic_practice_", dataset_str, ".csv")), col_types = "cififidddd")
  practice_year <- read_csv(file.path(root, subdir, "practice_year", paste0("acic_practice_year_", dataset_str, ".csv")), col_types = "cidiiiddddddd")
  
  # merge and rename
  patient_year_df <- patient_year %>%
    left_join(patient, by = "id.patient") %>%
    left_join(practice, by = "id.practice") %>%
    left_join(practice_year %>% dplyr::select(id.practice, year, Z, post), by = c("year", "id.practice"))
  colnames(patient_year_df) <- toupper(colnames(patient_year_df))
  colnames(patient_year_df) <- gsub('\\.', '_', colnames(patient_year_df))
  if (fix_neg) patient_year_df$Y <- pmax(patient_year_df$Y, 0.01)
  
  if (format == "wide") {
  # change to wide format 
    patient_year_df <- long_to_wide(patient_year_df)
  }
  return(patient_year_df)
}

### add propensity score weights to a data frame in wide format
add_ps <- function(dfw, z_var = "Z", ps_var = "ps", weight_var = "weights",
                   method = "ps", estimand = "ATT", max_weight = 10, ...) {
  require(WeightIt)
  fml <- paste(z_var, "~ V1 + V2 + V3 + V4 + V5 + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9")
  weightit_model <- weightit(as.formula(fml), data = dfw, method = method, estimand = estimand, ...)
  dfw[,ps_var] <- weightit_model$ps
  dfw[,weight_var] <- pmin(weightit_model$weights, max_weight)
  return(dfw)
}

### pivot a wide data frame to long format, keep only one set of Z and Y variables
wide_to_long <- function(df, z_var = "Z", y_var = "Y") {
  df %>% pivot_longer(cols = c("YEAR1", "YEAR2", "YEAR3", "YEAR4"),
                      values_to = y_var,
                      names_to = "YEAR") %>%
    mutate(YEAR = as.integer(stringr::str_sub(YEAR, 5, 5))) %>%
    mutate(Z_YEAR3 = as.numeric(Z == 1 & YEAR == '3')) %>%
    mutate(Z_YEAR4 = as.numeric(Z == 1 & YEAR == '4')) %>%
    mutate(G = 3*Z)
}

### pivot a long data frame to a wide format
long_to_wide <- function(df, keep_weights = T) {
  id_cols <- c("ID_PATIENT", "ID_PRACTICE", "Z",
               "V1", "V2", "V3", "V4", "V5", 
               "X1", "X2", "X3", "X4", "X5", 
               "X6", "X7", "X8", "X9")
  if ("weights" %in% colnames(data) && keep_weights) id_cols <- c(id_cols, "ps", "weights")
  df %>% 
    pivot_wider(id_cols = all_of(id_cols), 
                values_from = c("Y"), 
                names_from = "YEAR", 
                names_prefix = "YEAR"
    )
}

### fit weighted LM model
fit_model_lm <- function(dfw, dfl) {
  m <- lm(Y ~ Z + factor(YEAR) + Z_YEAR3 + Z_YEAR4, 
          weights = weights, data = dfl)
  return(coef(m)[c('Z_YEAR3', 'Z_YEAR4')])
}

### fit weighted GLM model + Buhani adjustment
fit_model_glm <- function(dfw, dfl) {
  m <- glm(Y ~ factor(YEAR) + Z_YEAR3 + Z_YEAR4, 
           data = dfl, weights = weights, family = Gamma("log"))
  #summary(cost_model4)
  
  # ATT can be calculated by switching on/off interaction terms. Specifically:
  df3 <- dfl %>% filter(Z_YEAR3 == 1)
  pred_3_1 <- df3 %>% predict(m, newdata = ., type = "response")
  pred_3_0 <- df3 %>% mutate(Z_YEAR3 = 0) %>% predict(m, newdata = ., type = "response")
  ATT3 <- mean(pred_3_1 - pred_3_0)
  df4 <- dfl %>% filter(Z_YEAR4 == 1)
  pred_4_1 <- df4 %>% predict(m, newdata = ., type = "response")
  pred_4_0 <- df4 %>% mutate(Z_YEAR4 = 0) %>% predict(m, newdata = ., type = "response")
  ATT4 <- mean(pred_4_1 - pred_4_0)
  return(c(ATT3, ATT4))
}

### fit DID model using did package
fit_model_did <- function(dfw, dfl) {
  require(did)
  dfl$ID_PATIENT <- as.numeric(dfl$ID_PATIENT)
  m <- att_gt("Y", "YEAR", "ID_PATIENT", "G",
              ~ V1 + V2 + V3 + V4 +V5 +X1 +X2 +X3+X4+X5+X6+X7+ X8+X9,
              data = dfl, bstrap = F, 
              print_details = T, clustervars="ID_PRACTICE", est_method = "ipw")
  summary(m)
  return(m$att[2:3])
}

### fit a bart model
fit_model_bart <- function(dfw, dfl) {
  require(dbarts)
  # assume complete cases -- drop NA records without imputation
  df_c <- dfw %>%  
    filter(complete.cases(.))
  # grab treated samples as test group
  df_c_t <- df_c %>% filter(Z == 1) 
  # need to get posterior for both conterfactual arms(?)
  X_test <- 
    rbind(df_c_t,
          df_c_t %>% mutate(Z = 0)
    )
  n <- nrow(df_c_t)
  
  # year3 model
  X <- df_c %>% dplyr::select(YEAR1, YEAR2, ps, Z, V1, V2, V3, V4, V5, X1, X2, X3, X4, X5, X6, X7, X8, X9)
  y <- df_c %>% pull(YEAR3)
  # fit bart
  m3 <- bart(X, y, ntree = 25, nskip = 500, ndpost = 500, x.test = X_test, usequants = T, keeptrainfits = F)
  # calculate att
  y1 <- m3$yhat.test.mean[1:n]
  y0 <- m3$yhat.test.mean[(n+1):(2*n)]
  att3 <- y1 - y0
  rm(m3)
  gc()
  print(mean(att3))
  
  # year4 model
  X <- df_c %>% dplyr::select(YEAR1, YEAR2, YEAR3, ps, Z, V1, V2, V3, V4, V5, X1, X2, X3, X4, X5, X6, X7, X8, X9)
  y <- df_c %>% pull(YEAR4)
  # fit bart
  m4 <- bart(X, y, ntree = 25, nskip = 500, ndpost = 500, x.test = X_test, usequants = T, keeptrainfits = F)
  # calculate att
  y1 <- m4$yhat.test.mean[1:n]
  y0 <- m4$yhat.test.mean[(n+1):(2*n)]
  att4 <- y1 - y0
  rm(m4)
  gc()
  print(mean(att4))
  
  att <- c(att3 = mean(att3), att4 = mean(att4))
  return(att)
}

### fit a selection of models, save result in a dataframe looks like (method, YEAR, ATT)
fit_models <- function(dfw, dfl, dataset, methods = list("LM", "GLM", "DID", "BART")) {
  
  res_lst <- lapply(methods, function(method) {
    #method <- "LM"
    fun <- paste0("fit_model_", tolower(method))
    att <- do.call(fun, list(dfw, dfl))
    data.frame(method = method, YEAR = c(3, 4), ATT = att)
  })
  res <- do.call(rbind, res_lst)
  res
}

### test the pipeline with one dataset
run_one_dataset <- function(dataset) {
  dataset <- 1
  dfw <- load_data(dataset = dataset)
  dfw <- add_ps(dfw)
  dfl <- wide_to_long(dfw)
  res <- fit_models(dfw, dfl, dataset)
  return(res)
}