# acic_2022 helper functions
# This includes functions used for testing/validation from Matt's DGP

# I set up a local sqlite to store Snowflake tables downloaded...
# DFP_Y file on snowflake has a few columns of (Z,Y) generated for each dataset

# create connection to sqlite
connect_sqlite_db <- function(
  path = "/Users/yuqin.wei/Projects/acic_2022",
  db = "dgp.db"
) {
  require(RSQLite)
  require(DBI)
  con <- dbConnect(RSQLite::SQLite(), file.path(path, db))
  dbListTables(con)
  con
}

# query one dataset
get_DGP_Y <- function(con, dataset, table = "DGP_Y_NEW") {
  sql <- paste("SELECT * FROM", table, "WHERE DATASET_NUM = ", dataset, ";")
  dbGetQuery(con, sql)
}

# query true att value for all datasets
get_DGP_ATT <- function(con, table = "DGP_ATT_NEW") {
  sql <- paste("SELECT * FROM", table, ";")
  dbGetQuery(con, sql)
}

# merge one original dataframe in wide format with DGP process Matt created (Y, Z)
merge_dgp <- function(df, df_dgp_y, z_var = "Z_DGP_1", y_var = paste0("Y_DGP_1_WITH_", z_var)) {
  require(tidyverse)
  res <- 
    df %>% select(ID_PRACTICE, ID_PATIENT, 
                  V1, V2, V3, V4, V5,
                  X1, X2, X3, X4, X5, 
                  X6, X7, X8, X9) %>%
    mutate(ID_PATIENT = as.integer(ID_PATIENT)) %>%
    inner_join(
      df_dgp_y %>%
      select(ID_PATIENT, YEAR, !!z_var, !!y_var) %>%
      rename(Z = !!z_var) %>%
      pivot_wider(names_from = "YEAR", 
                  names_prefix = "YEAR",
                  values_from = all_of(y_var))
    )
  res
}

# load one dataset, merge with DGP for the updated (Y, Z) (four specs), run specified models, summarize estimated ATT
run_one_validation <- function(dataset, methods = list("LM", "GLM", "DID")) {
  #dataset <- 1
  con <- connect_sqlite_db()
  df <- load_data(dataset)
  df_dgp_y <- get_DGP_Y(con, dataset)
  # att_dgp <- get_DGP_ATT(con, dataset)
  
  param <- expand.grid(Z_DGP = c(1, 2), Y_DGP = c(1, 2))
  res <- map2_dfr(param$Z_DGP, param$Y_DGP, function(z_dgp, y_dgp) {
    # z_dgp <- 1
    # y_dgp <- 1
    z_var <- paste0("Z_DGP_", z_dgp)
    y_var <- paste0("Y_DGP_", y_dgp, "_WITH_", z_var)
    
    dfw <- merge_dgp(df, df_dgp_y, z_var, y_var)
    dfw <- add_ps(dfw)
    dfl <- wide_to_long(dfw)
    fit <- fit_models(dfw, dfl, dataset, methods = methods)
    res <- data.frame(DATASET_NUM = dataset, Z_DGP = z_dgp, Y_DGP = y_dgp, fit)
  })
  # res <- fit %>% inner_join(att_dgp)
  dbDisconnect(con)
  rownames(res) <- NULL
  return(res)
}

# compare estimated ATT against true ATT and calculate bias/rmse/se
check_validation_runs <- function(res_df) {
  require(tidyverse)
  con <- connect_sqlite_db()
  att_dgp <- get_DGP_ATT(con) %>% rename(TRUE_ATT = ATT)
  check <- res_df %>% 
    inner_join(att_dgp) %>%
    mutate(
      error = ATT - TRUE_ATT
    ) %>%
    group_by(Z_DGP, Y_DGP, YEAR, method) %>%
    summarize(
      n_dataset = n(),
      bias = mean(error),
      rmse = sqrt(mean(error^2)),
      se = sqrt(rmse^2 - bias^2)
    ) %>%
    arrange(Z_DGP, Y_DGP, YEAR, method)
  dbDisconnect(con)
  return(check)
}

### deprecated
snowflake_connecter <- function(
  driver_path = "/Users/yuqin.wei/Projects/Tools/snowflake-jdbc-3.9.2.jar",
  username = "YWEI",
  password = rstudioapi::askForPassword("Snowflake Password"),
  warehouse = "XLARGE_WH",
  database = "SANDBOX_KOMODO",
  schema = "ACIC_CAUSALITY_CHALLENGE_2022"
) {
  require(RJDBC)
  require(dplyr)
  require(dplyr.snowflakedb)
  options(dplyr.jdbc.classpath = driver_path)
  db <- src_snowflakedb(
    user = username, 
    password = password, 
    account = 'komodohealth',
    opts = list(
      warehouse = warehouse,
      db=database,
      schema=schema
    ))
  return(db)
}

get_DGP_Y_multi <- function(max_datasets) {
  sql <- paste("SELECT DATASET_NUM, ID_PATIENT, YEAR, Z_DGP_1, Z_DGP_2, Y_DGP_1_WITH_Z_DGP_1, ETA_Y_DGP_1_WITH_Z_DGP_1, Y_DGP_2_WITH_Z_DGP_1, ETA_Y_DGP_2_WITH_Z_DGP_1, Y_DGP_1_WITH_Z_DGP_2, ETA_Y_DGP_1_WITH_Z_DGP_2, Y_DGP_2_WITH_Z_DGP_2, ETA_Y_DGP_2_WITH_Z_DGP_2 WHERE DATASET_NUM <= ", max_datasets, ";")
  dbGetQuery(db$con, sql)
}

get_DGP_ATT_multi <- function(max_datasets) {
  sql <- paste("SELECT * FROM DGP_ATT WHERE DATASET_NUM <= ", max_datasets, ";")
  dbGetQuery(db$con, sql)
}
