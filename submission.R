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
  data_red_2 = dplyr::bind_cols(data_red_num_w_power, data_red_cat)
  
  return(data_red_2)
}


# define function to obtain which model can be applied given observed variable for a row
return_which_model_can_be_applied = function(name_variables_observed, list_varmat_best_model, names_in_training, set_all_best_models, verbose =F){
  
  total_nbr_of_best_models = length(set_all_best_models)
  
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
      names_var_needed_in_model = names_in_training [list_varmat_best_model[[i]]]
      model_possible = as.numeric(all(names_var_needed_in_model %in% name_variables_observed))
      vec_model_feasible[counter] = model_possible
      
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
        names_var_needed_in_model = names_in_training[var_mat_dim_i[,j]]
        model_possible = as.numeric(all(names_var_needed_in_model %in% name_variables_observed))
        vec_model_feasible[counter] = model_possible
        
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


# define function to get the most popular vote among learners
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
    return(sample(c(0,1), 1))
  }
}



# # for testing
# # check returned df when running this on fake data
# df = read.csv("PreFer_fake_data.csv")
# background_df = read.csv("PreFer_fake_background_data.csv")

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
  df_test <- clean_df(df, background_df)
  
  # verify that all variables on which trained are in the df
  all(model$x %in% colnames(df_test))
  
  # create a version of the test set which have all variables imputed
  df_test_imputed = missForest::missForest(df_test)$ximp
  
  # save vec_id 
  vec_nomem_encr = df_test%>%pull(nomem_encr)
  
  # Exclude the variable nomem_encr if this variable is NOT in your model
  df_test = df_test %>% select(-c(nomem_encr))
  df_test_imputed = df_test_imputed  %>% select(-c(nomem_encr))
  
  # check nbr of row to predict in df test
  n_to_predict = nrow(df_test)
  
  # create vector of prediction
  vec_prediction = vector(mode = "numeric", length = n_to_predict)
  
  # total
  total_nbr_of_best_models = length(model$set_all_best_models)

  for(i in seq(n_to_predict)){
      # extract row
      row_i = df_test[i, ]
      # obtain the column with presence of observations
      id_var_present = which(!is.na(row_i))
      name_variables_observed = colnames(df_test)[id_var_present]
      model_that_can_be_applied = return_which_model_can_be_applied(name_variables_observed = name_variables_observed,
                                        list_varmat_best_model =  model$list_varmat_best_model,
                                        names_in_training = model$colnames_X,
                                        set_all_best_models = model$set_all_best_models,
                                        verbose = F
                                          )
      
     # this is if no model at all can be applied:
     if(sum(model_that_can_be_applied)==0){
       # then we use the same row but from the imputed df
       row_i_imputed = df_test_imputed[i,]
       # levels_vector <- c(0, 1)
       # # Create a numeric vector 
       vec_prediction_row_i_imputed = vector(mode = "numeric", length=total_nbr_of_best_models)
       # vec_prediction_row_i_imputed <- factor(levels_vector, levels = c(0, 1))
       for(model_i in seq(total_nbr_of_best_models)){
         pred_factor= predict(model$set_all_best_models[[model_i]], row_i_imputed)$prediction
         # Convert factor to numeric (0 or 1)
         pred_factor_numeric <- as.numeric(as.character(pred_factor))
         vec_prediction_row_i_imputed[model_i] <- pred_factor_numeric

       }
       vec_prediction[i] = max_class(vec_prediction_row_i_imputed)
       # otherwise, if there are some top performing model that we can apply, use these guys
     }else{
       vec_prediction_row_i = vector(mode = "numeric", length=sum(model_that_can_be_applied))
       id_model_that_can_be_applied = which(model_that_can_be_applied == 1)
       for(j in seq(sum(model_that_can_be_applied))){
         pred_factor= predict(model$set_all_best_models[[id_model_that_can_be_applied[j]]], row_i)$prediction
         # Convert factor to numeric (0 or 1)
         pred_factor_numeric <- as.numeric(as.character(pred_factor))
         vec_prediction_row_i[j] <- pred_factor_numeric
       }
       vec_prediction[i]  = max_class(vec_prediction_row_i)
     
     }
      
    }

  # # Generate predictions from model
  # predictions <- predict(model,
  #                        subset(df, select = vars_without_id),
  #                        type = "response")
  
  # Create predictions that should be 0s and 1s rather than, e.g., probabilities
  # predictions <- ifelse(predictions > 0.25, 1, 0)

  # Output file should be data.frame with two columns, nomem_encr and predictions
  df_predict <- data.frame("nomem_encr" = vec_nomem_encr, "prediction" = vec_prediction)
  # Force columnnames (overrides names that may be given by `predict`)
  names(df_predict) <- c("nomem_encr", "prediction")

  # Return only dataset with predictions and identifier
  return( df_predict )
}


# predict_outcomes(df= df, background_df = background_df)
