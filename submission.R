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


  #
  # for (j in var_sub) {
  #   data_red[[j]] <- transform_variable(data_red, j, codebook)
  # }


  return(data_red)
}

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
