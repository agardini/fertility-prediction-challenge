# This is an example script to train your model given the (cleaned) input dataset.
#
# This script will not be run on the holdout data,
# but the resulting model model.joblib will be applied to the holdout data.
#
# It is important to document your training steps here, including seed,
# number of folds, model, et cetera

train_save_model <- function(cleaned_df, outcome_df) {
  # Trains a model using the cleaned dataframe and saves the model to a file.

  # Parameters:
  # cleaned_df (dataframe): The cleaned data from clean_df function to be used for training the model.
  # outcome_df (dataframe): The data with the outcome variable (e.g., from PreFer_train_outcome.csv or PreFer_fake_outcome.csv).

  ## This script contains a bare minimum working example
  set.seed(1) # not useful here because logistic regression deterministic
  # Combine cleaned_df and outcome_df
  model_df <- merge(cleaned_df, outcome_df, by = "nomem_encr")

  # Logistic regression model
  #model <- gam(new_child ~ factor(employment_status)+s(birthyear_bg)+factor(partner_2020)+religious_participation+factor(type_dwelling)+factor(mig_by_origin)+factor(civil_status)+factor(cf20m128), family = "binomial", data = model_df)

  model_df$employment_status<-as.factor(model_df$employment_status)
  model_df$partner_2020<-as.factor(model_df$partner_2020)
  # model_est <- model_df[, c("new_child","employment_status","birthyear_bg","partner_2020")]
  # model_est<-model_est[complete.cases(model_est),]
  # model <- spikeSlabGAM(new_child ~(employment_status+birthyear_bg+partner_2020)^2, data = model_est, family = "binomial")

  model_est <- model_df[, c("new_child","having_children_future","years_next_children",
                            "desired_nr_add_children","birthyear_bg","partner_2020",
                            "log_net_personal_income")]
  model_est<-model_est[complete.cases(model_est),]
  model <- spikeSlabGAM(new_child ~(having_children_future+
                                      years_next_children+desired_nr_add_children+
                                      birthyear_bg+partner_2020)^2+log_net_personal_income,
                        data = model_est, family = "binomial")




  # Save the model
  saveRDS(model, "model.rds")
}
