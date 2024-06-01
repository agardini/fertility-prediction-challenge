# This is an example script to generate the outcome variable given the input dataset.
#
# This script should be modified to prepare your own submission that predicts
# the outcome for the benchmark challenge by changing the clean_df and predict_outcomes function.
#
# The predict_outcomes function takes a data frame. The return value must
# be a data frame with two columns: nomem_encr and outcome. The nomem_encr column
# should contain the nomem_encr column from the input data frame. The outcome
# column should contain the predicted outcome for each nomem_encr. The outcome
# should be 0 (no child) or 1 (having a child).
#
# clean_df should be used to clean (preprocess) the data.
#
# run.R can be used to test your submission.

# List your packages here. Don't forget to update packages.R!
library(tidyverse) # as an example, not used here
library(mgcv)
library(ranger)


#################################################
### function to recode variables -----
#
# transform_variable <- function(df, variable_name, codebook) {
#   var_info <- codebook %>% filter(var_name == variable_name)
#
#   if (nrow(var_info) == 0) {
#     stop(paste("Variable", variable_name, "not found in codebook"))
#   }
#
#   var_type <- var_info$type_var
#
#   if (var_type == "categorical") {
#     levels <- strsplit(var_info$values_cat, "; ")[[1]]
#     labels <- strsplit(var_info$labels_cat, "; ")[[1]]
#
#     # Check for length mismatch and adjust labels if needed
#     if (length(labels) == length(levels)) {
#
#       df[[variable_name]] <- factor(df[[variable_name]], levels = levels, labels = labels)
#
#     }} else if (var_type == "date or time") {
#       df[[variable_name]] <- lubridate::dmy(df[[variable_name]])
#
#     }
#
#   return(df[[variable_name]])
# }



##########################################
## Function to clean the dataset ----

clean_df <- function(df, background_df = NULL){

  # listing variables to keep
  var_sub <- c("nomem_encr","outcome_available","cf20m009","cf20m011","cf20m022",
               "cf20m024","cf20m128","cf20m129","cf20m130","cf20m454","cf20m526",
               "ca20g005","ca20g006","ca20g007","ca20g008","ca20g009","ca20g010",
               "ca20g011","ca20g057","ca20g058","ca20g060","ca20g061","ca20g062",
               "ci20m008","ch20m004","ch20m018","ch20m178","cp20l010","cp20l016",
               "cp20l019","cp20l201","cr20m030","cr20m041","cr20m089","cr20m134",
               "cr20m137","cr20m138","cr20m139","cr20m140","cr20m141","cr20m143",
               "cr20m148","cr20m149","cr20m150","cr20m151","cr20m162","cs20m001",
               "cs20m063","cs20m283","cs20m285","cs20m286","cs20m287","birthyear_bg",
               "gender_bg","migration_background_bg","belbezig_2020","burgstat_2020",
               "nettoink_f_2020","oplcat_2020","partner_2020","sted_2020",
               "woning_2020","woonvorm_2020","ca20g078","ca20g065","ca20g087","nettohh_f_2020")
  var_bg2 <- c("positie","aantalhh","aantalki")

  # selecting variables from df
  data_red<-df %>%
    select(all_of(var_sub))

  # selecting variables from data background
  bg_red <- background_df %>%
    select(nomem_encr, wave, all_of(var_bg2)) %>%
    filter(wave > 202000) %>%
    select(-wave) %>%
    distinct() %>%
    group_by(nomem_encr) %>%
    slice_tail(n=1) %>%
    ungroup()

  data_red <- data_red %>%
    left_join(bg_red)

  # recoding variables
  data_red<-data_red %>%
    mutate(having_children_future = case_when(cf20m128==1~0,
                                              cf20m128==3~1,
                                              cf20m128==2~2,
                                              TRUE~NA),
           assets = case_when(ca20g009==1 &ca20g005 ==0 & ca20g006==0 &
                                ca20g007==0& ca20g008==0 & ca20g010==1 & ca20g011==1 ~ 0,
                              ca20g007==1 ~ 1,
                              ca20g006==1 ~ 2,
                              ca20g005==1 ~ 3,
                              ca20g008==1 ~ 4,
                              (ca20g010==2 | ca20g011==2)  ~ 5,
                              TRUE ~ NA),
           presence_debt = case_when(ci20m008==2 ~ 0,
                                     ca20g057==1 ~ 1,
                                     ca20g058==1 ~ 1,
                                     ca20g060==1 ~ 1,
                                     ca20g061==1 ~ 1,
                                     ca20g062==1 ~ 1,
                                     TRUE ~ NA),
           employment_status = case_when(belbezig_2020==1~0,
                                         belbezig_2020 %in% c(2,3)~1,
                                         belbezig_2020 %in% c(4,5,6,11)~2,
                                         belbezig_2020 %in% c(8,9,10,12)~3,
                                         belbezig_2020 %in% c(7,14) ~ 4,
                                         belbezig_2020 == 13 ~ 5
           ),
           type_dwelling = case_when(woning_2020 %in% c(1,4) ~ 0,
                                     woning_2020 %in% c(2,3) ~ 1,
                                     TRUE ~ NA),
           log_net_household_income = log(nettohh_f_2020+1),
           log_net_personal_income = log(nettoink_f_2020+1),
           desired_nr_add_children = case_when(cf20m129==1~1,
                                               cf20m129==2~2,
                                               cf20m129>2~3,
                                               is.na(cf20m129)~0),

           years_next_children = case_when(cf20m130<=1~1,
                                           cf20m130>1 & cf20m130<=3 ~2,
                                           cf20m130>3 & cf20m130<=10 ~3,
                                           cf20m130>10~4,
                                           is.na(cf20m130)~0),

           birth_year_mother_cat = case_when(cf20m009<1945 ~0,
                                             cf20m009>=1945 & cf20m009<1950 ~  1,
                                             cf20m009>=1950 & cf20m009<1955 ~  2,
                                             cf20m009>=1955 & cf20m009<1960 ~  3,
                                             cf20m009>=1960 & cf20m009<1965 ~  4,
                                             cf20m009>=1965 & cf20m009<1970 ~  5,
                                             cf20m009>=1970 & cf20m009<1975 ~  6,
                                             cf20m009>=1975 ~ 7,
                                             TRUE~NA),

           mig_by_origin = case_when(migration_background_bg==0~0,
                                     migration_background_bg %in% c(101,201) ~1,
                                     migration_background_bg %in% c(102,202) ~2,
                                     TRUE ~ NA),
           mig_by_generation = case_when(migration_background_bg==0~0,
                                         migration_background_bg %in% c(101,102) ~1,
                                         migration_background_bg %in% c(201,202) ~2,
                                         TRUE ~ NA),
           civil_status = case_when(burgstat_2020==1 ~0,
                                    burgstat_2020 %in% c(2,3,4)~1,
                                    burgstat_2020==5~2),
           religious_participation = case_when(cr20m041 %in% c(1,2,3,4)~ 0,
                                               cr20m041 %in% c(5,6)~1,
                                               TRUE~NA)

    ) %>%
    mutate(having_children_future=factor(having_children_future,
                                         levels=c(0,1,2),
                                         labels=c("Yes","No","Don't know")),
           assets = factor(assets,
                           levels = c(0,1,2,3,4,5),
                           labels = c("Possession of Assets: type unspecified","real estate",
                                      "investments","insurence","vehicle","other")),
           presence_debt = factor(assets,levels=c(0,1),
                                  labels=c("Not having debts/loans","Having debts/loans")),
           employment_status = factor(employment_status,
                                      levels=c(0,1,2,3,4,5),
                                      labels=c("Paid employment","Self-employed","Unemployed","Inactive","Student or too young","Other"),
           ),
           type_dwelling = factor(type_dwelling,
                                  levels=c(0,1),
                                  labels=c("Owned","Rented")),
           desired_nr_add_children = factor(desired_nr_add_children,
                                            levels=c(0,1,2,3),
                                            labels=c("None","1 child","2 children","3 or more children")),
           years_next_children = factor(years_next_children,
                                        levels=c(0,1,2,3,4),
                                        labels=c("Never","1 year or less","Between 1 and 3 years","Between 3 and 10 years",
                                                 "More than 10 years")),
           birth_year_mother_cat = factor(birth_year_mother_cat,
                                          levels=c(0,1,2,3,4,5,6,7),
                                          labels=c("Before 1945","1945-1949","1950-1954","1955-1959",
                                                   "1960-1964","1965-1969","1970-1974","After 1975")),
           mig_by_origin = factor(mig_by_origin,
                                  levels=c(0,1,2),
                                  labels=c("Native","Western Migrant","Non-Western Migrant")),
           mig_by_generation = factor(mig_by_generation,
                                      levels=c(0,1,2),
                                      labels=c("Native","First Generation Migrant","Second Generation Migrant")),
           civil_status = factor(civil_status,
                                 levels=c(0,1,2),
                                 labels=c("Married","No longer married","Never married")),
           religious_participation = factor(religious_participation,
                                            levels=c(0,1),
                                            labels=c("At least once a week","Less than once a week")))
  
  
  # more preparation steps
  # set to NA over all char columns when empty string
  data_red<-data_red %>%
    mutate(across(where(is.character), ~ na_if(., "")))
  
  # remove outcome available
  data_red = data_red %>% select(-c(outcome_available))

  # transform all character variables to factors
  data_red <- data_red %>%
    mutate(across(where(is.character), as.factor)) %>% 
    # remove some covariates which have very little presence in train set
    select(-c(cf20m129,  cr20m137,  cr20m138,   cr20m139, cr20m140 ,cr20m141, cr20m148, 
              cr20m149, cr20m150, cr20m151, cf20m130, ca20g065, presence_debt, employment_status))
  
  # split numeric and factors
  data_red_num = data_red %>% select_if(is.numeric) 
  data_red_cat = data_red %>%  select_if(is.factor) 

  # create power of 2 and power of 3
  data_red_num_w_power =  data_red_num %>%
    mutate(across(where(is.numeric), list(square = ~ .^2, cube = ~ .^3, sqrt_root = ~.^(.5)), .names = "{col}_{fn}"))

  # remerge numeric variables extended and cat variables
  data_red_2 = dplyr::bind_cols(data_red_num_w_power, df_test_cat)
  
  # create power of 2 and power of the numeric variables
  df_test_num_w_power =  df_test_num %>%
    mutate(across(where(is.numeric), list(square = ~ .^2, cube = ~ .^3, sqrt_root = ~.^(.5)), .names = "{col}_{fn}"))
  

  return(data_red)
}

#-------------------------------- START

# load the swag output with random forest
load("swag_rf_prefer_on_extended_dataset.rda")
out$CVs_f1_score

# check returned df when running this on fake data
fake_test_set = read.csv("PreFer_fake_data.csv")
bg_data = read.csv("PreFer_fake_background_data.csv")

df_test = clean_df(df = fake_test_set, background_df = bg_data)
colnames(df_test)
df_test$nomem_encr
df_test$outcome_available
# impute na on test set and keep this dataset also
load("model_data.RData")
dim(data_new)
dim(df_test)
all(colnames(data_new) %in% colnames(df_test) )
id_col_not_in_test = which(!colnames(data_new) %in% colnames(df_test) )
colnames(data_new)[id_col_not_in_test]

# the outcome is the only variable not present, so all good

# more data preparation steps
# set to NA over all char columns when empty string
df_test <- df_test %>%
  mutate(across(where(is.character), ~ na_if(., "")))

# # define function count prop of missing
# f_prop_not_na = function(x){
#   mean(!is.na(x))
# }
# 
# # check prop of presence per variable, (1- missingness)
# vec_prop_presence = apply(df_test, MARGIN = 2, FUN = f_prop_not_na)
# sort(vec_prop_presence, decreasing = T)
# names(which(vec_prop_presence > 0.8))
# names(which(vec_prop_presence<.8))
# 
# lets not remove them, we use some of them in the training set
df_test = df_test %>% 
  # select(names(which(vec_prop_presence > 0.8))) %>% 
  select(-c(nomem_encr, outcome_available))


df_test <- df_test %>%
  mutate(across(where(is.character), as.factor)) %>% 
  select(-c(cf20m129,  cr20m137,  cr20m138,   cr20m139, cr20m140 ,cr20m141, cr20m148, cr20m149, cr20m150, cr20m151, cf20m130, ca20g065, presence_debt,employment_status))
str(df_test)
# Column names in the desired order
column_order <- c("cf20m009", "cf20m011", "cf20m022", "cf20m024", "cf20m128", 
                  "cf20m454", "cf20m526", "ca20g005", "ca20g006", "ca20g007", 
                  "ca20g008", "ca20g009", "ca20g010", "ca20g011", "ca20g057", 
                  "ca20g058", "ca20g060", "ca20g061", "ca20g062", "ci20m008", 
                  "ch20m004", "ch20m018", "ch20m178", "cp20l010", "cp20l016", 
                  "cp20l019", "cp20l201", "cr20m030", "cr20m041", "cr20m089", 
                  "cr20m134", "cr20m143", "cr20m162", "cs20m001", "cs20m063", 
                  "cs20m283", "cs20m285", "cs20m286", "cs20m287", "birthyear_bg", 
                  "gender_bg", "migration_background_bg", "belbezig_2020", 
                  "burgstat_2020", "nettoink_f_2020", "oplcat_2020", "partner_2020", 
                  "sted_2020", "woning_2020", "woonvorm_2020", "positie", 
                  "aantalhh", "aantalki", "ca20g078", "ca20g087", "nettohh_f_2020", 
                  "having_children_future", "assets", "type_dwelling", 
                  "log_net_household_income", "log_net_personal_income", 
                  "desired_nr_add_children", "years_next_children", 
                  "birth_year_mother_cat", "mig_by_origin", "mig_by_generation", 
                  "civil_status", "religious_participation")

# Reorder columns using dplyr
df_test <- df_test %>%
  select(all_of(column_order))

# impute df
# to use later if we dont have any best perfoming model where all variables are observed



# try imputation with mice
# library(mice)
# df_test_imputed = complete(mice(df_test))

# imputation random forest
df_test_imputed = missForest::missForest(df_test)$ximp
id_not_in_imputed = which(!colnames(df_test) %in% colnames(df_test_imputed) )

# there is not presence_debt because all the entries are 0 in the original df_test
mean(is.na(df_test$presence_debt))

# compare with X_pref here
load("X_pref.rda")
dim(X_pref)
all(colnames(df_test) == colnames(X_pref))
all(colnames(df_test_imputed) == colnames(X_pref))
str(df_test)

# construct the other variables on df_test 
df_test_num = df_test %>% select_if(is.numeric) 
colnames(df_test_num)
df_test_cat = df_test %>%  select_if(is.factor) 
dim(df_test_num)
dim(df_test_cat)
colnames(df_test_cat)

# create power of 2 and power of 3
df_test_num_w_power =  df_test_num %>%
  mutate(across(where(is.numeric), list(square = ~ .^2, cube = ~ .^3, sqrt_root = ~.^(.5)), .names = "{col}_{fn}"))
colnames(df_test_num_w_power)
str(df_test_num_w_power)
dim(df_test_num_w_power)

df_test_extended = dplyr::bind_cols(df_test_num_w_power, df_test_cat)
all(colnames(df_test_extended) == colnames(out$x))

# construct the new variables on the imputed df
df_test_imputed_num = df_test_imputed %>% select_if(is.numeric) 
df_test_imputed_cat = df_test_imputed %>%  select_if(is.factor) 
df_test_imputed_num_w_power =  df_test_imputed_num %>%
  mutate(across(where(is.numeric), list(square = ~ .^2, cube = ~ .^3, sqrt_root = ~.^(.5)), .names = "{col}_{fn}"))


df_test_imputed_extended = dplyr::bind_cols(df_test_imputed_num_w_power, df_test_imputed_cat)
all(colnames(df_test_imputed_extended) == colnames(out$x))

# plot F1 score of swag output
plot(unlist(lapply(X = out$CVs_f1_score, FUN = function(x){max(x, na.rm = T)})), type="l",
     xlab="Model dimension", ylab="Performance (F1 score)")
grid(col="grey80", lty=1)


# select the set of best models
# get a given quantile over all f1 score over all models of all dimensions
alpha=c(.95, .9, .85, .8, .75)
mat = matrix(NA, ncol=length(out$CVs_f1_score), length(alpha))
for(i in seq(length(alpha))){
  for(j in seq(length(out$CVs_f1_score))){
    mat[i,j] = quantile(out$CVs_f1_score[[j]], probs = alpha[i], na.rm = T)
  }
}


plot(mat[1,], type="l", ylim=c(.2, .9))
lines(mat[2,], col=2)
lines(mat[3,], col=3)
lines(mat[4,], col=4)

# 30 , 35

# prendre tous les modele de taillep lus petite egale a 35 qui on un score plus grand
grid(col="grey80")


quantile(c(1,2,3))

quant_to_consider = .80
quantile_f1_score_to_consider = quantile(unlist(out$CVs_f1_score), na.rm = T, probs = quant_to_consider)


quantile_f1_score_to_consider = quantile(out$CVs_f1_score[[35]], probs = .9,na.rm = T)
find_models_above_treshold<- function(vec, threshold) {
  which(vec > threshold)
}

# construct best model id list
best_models_list <- lapply(out$CVs_f1_score, find_models_above_treshold, quantile_f1_score_to_consider)

# subset best model list
best_models_list = best_models_list[1:35]

# obtain the variables associated with each best models
dim(out$x)
extract_associated_variables = function(list_varmat, list_best_model){
  
  max_dimension = length(list_best_model)
  
  list_varmat_best_model = vector("list", max_dimension)
  
  for(i in seq(max_dimension)){
    
    if(length(best_models_list[[i]]) == 0){
      next
    }else{
      list_varmat_best_model[[i]] = list_varmat[[i]][, best_models_list[[i]]]
    }
  }
  return(list_varmat_best_model)
}


list_varmat_best_model = extract_associated_variables(list_varmat = out$VarMat, list_best_model = best_models_list)

# check number of best models
total_nbr_of_best_models = sum(unlist(lapply(best_models_list, FUN = function(x){length(x)})))
total_nbr_of_best_models

# create a big model that countain all ranger models
create_all_models = function(list_varmat_best_model, verbose=T){
  list_all_estimated_models = list()
  max_dimension = length(list_varmat_best_model)
  counter_model = 1
  for(i in seq(max_dimension)){
    


    # these are for the dimension where there are no best models
    if(is.null(list_varmat_best_model[[i]])){
      next
    }
    # this is for the dimensions where there is only one "best" model
    if(is.vector(list_varmat_best_model[[i]])){
      df_sub = suppressMessages(dplyr::bind_cols(out$y, out$x[, list_varmat_best_model[[i]]]))
      colnames(df_sub)[1] = "new_child"
      df_sub_no_na = na.omit(df_sub)
      df_sub_no_na$new_child = as.factor( df_sub_no_na$new_child)
      fit = ranger::ranger(formula = "new_child ~.", data = df_sub_no_na)
      # save estimated model
      list_all_estimated_models[[counter_model]] = fit
      # verbose
      if(verbose){
        cat(paste0("fitted model ", counter_model , "\n"))
      }
      # update counter
      counter_model = counter_model+1

    }
    
    
    if(is.matrix(list_varmat_best_model[[i]])){
      var_mat_dim_i = list_varmat_best_model[[i]]
      ncol_var_mat_dim_i = ncol(var_mat_dim_i)
      for(j in seq(ncol_var_mat_dim_i)){
        df_sub = suppressMessages(dplyr::bind_cols(out$y, out$x[, var_mat_dim_i[, j]  ]))
        colnames(df_sub)[1] = "new_child"
        df_sub_no_na = na.omit(df_sub)
        df_sub_no_na$new_child = as.factor( df_sub_no_na$new_child)
        fit = ranger::ranger(formula = "new_child ~.", data = df_sub_no_na)
        # save estimated model
        list_all_estimated_models[[counter_model]] = fit
        # verbose
        if(verbose){
          cat(paste0("fitted model ", counter_model , "\n"))
        }
        # update counter
        counter_model = counter_model+1
      }
    }
    
  
    # extract varmat for that dimension
  }
  
  return(list_all_estimated_models)
}


set_all_best_models = create_all_models(list_varmat_best_model = list_varmat_best_model, verbose = T)
length(set_all_best_models)


# verify that indeed the df_test_extended have the same column as the train set 
all(colnames(df_test_extended) == colnames(out$x))
all(colnames(df_test_imputed_extended) == colnames(out$x))


return_which_model_can_be_applied = function(vec_observed_var, list_varmat_best_model, total_nbr_of_best_models, verbose =T){
  # vec_observed_var = id_var_present
  
  # create vector that say which model we can use
  vec_model_feasible = vector(mode = "numeric", length = total_nbr_of_best_models)
  max_dim = length(list_varmat_best_model)
  counter= 1
  for(i in seq(max_dim)){

    # these are for the dimension where there are no best models
    if(is.null(list_varmat_best_model[[i]])){
      next
    }
    
    # this is for the dimensions where there is only one "best" model
    if(is.vector(list_varmat_best_model[[i]])){
      vec_model_feasible[counter] =  all(list_varmat_best_model[[i]] %in% vec_observed_var)

      
      # verbose
      if(verbose){
        cat(paste0("tried model ", counter , "\n"))
      }
      # update counter
      counter = counter+1
      
    }
    
    if(is.matrix(list_varmat_best_model[[i]])){
      var_mat_dim_i = list_varmat_best_model[[i]]
      ncol_var_mat_dim_i = ncol(var_mat_dim_i)
      for(j in seq(ncol_var_mat_dim_i)){
        vec_model_feasible[counter] =  all(list_varmat_best_model[[i]][,j] %in% vec_observed_var)
        # verbose
        if(verbose){
          cat(paste0("tried model ", counter , "\n"))
        }
        # update counter
        counter = counter+1
      }
    }
    
  }
  return(vec_model_feasible)
}




max_class <- function(vec) {
  # Count the occurrences of 0 and 1
  count_0 <- sum(vec == 0)
  count_1 <- sum(vec == 1)
  
  # Determine which class has the higher count
  if (count_0 > count_1) {
    return(0)
  } else if (count_1 > count_0) {
    return(1)
  } else {
    return(sample(c(0,1), 1)) # Return NA if counts are equal
  }
}

# prediction step
predict_w_set_best_model = function(df_test_extended, set_all_best_models, df_test_imputed_extended, list_varmat_best_model){
  n_to_predict = nrow(df_test_extended)
  # create vector of prediction
  vec_prediction = vector(mode = "numeric", length = n_to_predict)
  for(i in seq(n_to_predict)){


    # extract row
    row_i = df_test_extended[i, ]
    # obtain the column with presence of observations
    id_var_present = which(!is.na(row_i))
    
    # check which model are possible to apply
    model_that_can_be_applied = return_which_model_can_be_applied(vec_observed_var = id_var_present,
                                                                  list_varmat_best_model = list_varmat_best_model,
                                                                  total_nbr_of_best_models = total_nbr_of_best_models,
                                                                  verbose = F)
    
    # this is if no model at all can be applied:
    if(sum(model_that_can_be_applied)==0){
      # then we use the same row but from the imputed df
      row_i_imputed = df_test_imputed_extended[i,]
      levels_vector <- c(0, 1)
      # Create a factor with levels 0 and 1
      vec_prediction_row_i_imputed <- factor(levels_vector, levels = c(0, 1))
      for(model_i in seq(total_nbr_of_best_models)){
        
        vec_prediction_row_i_imputed[model_i] =as.factor( predict(set_all_best_models[[model_i]], row_i_imputed)$prediction)
      }
      vec_prediction[i] = max_class(vec_prediction_row_i_imputed)
      # otherwise, if there are some top performing model that we can apply, use these guys
    }else{
      levels_vector <- c(0, 1)
      # Create a factor with levels 0 and 1
      vec_prediction_row_i <- factor(levels_vector, levels = c(0, 1))
      # vec_prediction_row_i = vector(mode="character", length= sum(model_that_can_be_applied))
      id_model_that_can_be_applied = which(model_that_can_be_applied == 1)
      for(j in seq(sum(model_that_can_be_applied))){
        vec_prediction_row_i[j] = as.factor(predict(set_all_best_models[[id_model_that_can_be_applied[j]]], row_i)$prediction)
      }
      
      vec_prediction[i]  = max_class(vec_prediction_row_i)
    }
    
    

  }
  return(vec_prediction)

  
}

length(set_all_best_models)
prediction_fake_test_set = predict_w_set_best_model(df_test_extended = df_test_extended, 
                         set_all_best_models = set_all_best_models,df_test_imputed_extended =  df_test_imputed_extended, list_varmat_best_model = list_varmat_best_model)
beepr::beep()

df_fake_test_outcome = read.csv("PreFer_fake_outcome.csv")
df_fake_test_outcome$new_child
prediction_fake_test_set


# how do we perform
confusion <- caret::confusionMatrix(data = as.factor(prediction_fake_test_set), 
                                    reference = as.factor(df_fake_test_outcome$new_child), positive = "1")
# Extract metrics
accuracy <- confusion$overall["Accuracy"]
accuracy
precision <- confusion$byClass["Pos Pred Value"]
precision
recall <- confusion$byClass["Sensitivity"]
recall
f1_score <- 2 * ((precision * recall) / (precision + recall))
f1_score


sort(apply(df_test, MARGIN = 2, FUN = function(x){mean(!is.na(x))}))



predict_outcomes <- function(df, background_df = NULL, model_path = "./model.rds"){
  # Generate predictions using the saved model and the input dataframe.

  # The predict_outcomes function accepts a dataframe as an argument
  # and returns a new dataframe with two columns: nomem_encr and
  # prediction. The nomem_encr column in the new dataframe replicates the
  # corresponding column from the input dataframe The prediction
  # column contains predictions for each corresponding nomem_encr. Each
  # prediction is represented as a binary value: '0' indicates that the
  # individual did not have a child during 2021-2023, while '1' implies that
  # they did.

  # Parameters:
  # df (dataframe): The data dataframe for which predictions are to be made.
  # background_df (dataframe): The background data dataframe for which predictions are to be made.
  # model_path (str): The path to the saved model file (which is the output of training.R).

  # Returns:
  # dataframe: A dataframe containing the identifiers and their corresponding predictions.

  ## This script contains a bare minimum working example
  if( !("nomem_encr" %in% colnames(df)) ) {
    warning("The identifier variable 'nomem_encr' should be in the dataset")
  }

  # Load the model
  model <- readRDS(model_path)

  # Preprocess the fake / holdout data
  df <- clean_df(df, background_df)

  # Exclude the variable nomem_encr if this variable is NOT in your model
  vars_without_id <- colnames(df)[colnames(df) != "nomem_encr"]

  # Generate predictions from model
  predictions <- predict(model,
                         subset(df, select = vars_without_id),
                         type = "response")

  # Create predictions that should be 0s and 1s rather than, e.g., probabilities
  predictions <- ifelse(predictions > 0.25, 1, 0)

  # Output file should be data.frame with two columns, nomem_encr and predictions
  df_predict <- data.frame("nomem_encr" = df[ , "nomem_encr" ], "prediction" = predictions)
  # Force columnnames (overrides names that may be given by `predict`)
  names(df_predict) <- c("nomem_encr", "prediction")

  # Return only dataset with predictions and identifier
  return( df_predict )
}
