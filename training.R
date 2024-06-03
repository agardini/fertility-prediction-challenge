# This is an example script to train your model given the (cleaned) input dataset.
#
# This script will not be run on the holdout data,
# but the resulting model model.joblib will be applied to the holdout data.
#
# It is important to document your training steps here, including seed,
# number of folds, model, et cetera


library(tidyverse)

# manual implementation k fold cross validation for random forest ranger
cv_k_fold_rf = function(df, formula, k = 10){
  

  # identify categorical variable
  cat_col = df %>% dplyr::select_if(is.factor) %>% colnames() %>% as.vector()
  # add always to stratify by new child the dependant var
  cat_col = rev(c(cat_col, "new_child"))
  
  # create balanced folds
  set.seed(123)
  df_sub = groupdata2::fold(data = df, k = k, cat_col= as.vector(cat_col))
  
  # create matrix to save measure of performance
  metrics_mat = matrix(NA, ncol= 5, nrow = k)
  
  #Perform 10 fold cross validation
  for(i in 1:k){
    
    # get train and test set
    df_test = df_sub %>% filter(.folds == i) %>% ungroup() %>% select(-c(.folds))
    df_train = df_sub %>% filter(.folds != i) %>% ungroup() %>% select(-c(.folds))
    df_train$new_child = as.factor(df_train$new_child)
    # Use test and train data partitions 
    fit = ranger::ranger(as.formula(formula), data=df_train)
    # fit = glm(as.formula(formula), data = df_train)
    y_pred = predict(fit, data = df_test)
    y_pred_dummy = y_pred$predictions
    # add level 1 if not in
    levels(y_pred_dummy) =c("0", "1")
    true_y = as.factor(df_test$new_child)
    # Create a confusion matrix
    confusion <- caret::confusionMatrix(data = y_pred_dummy, reference = true_y, positive = "1")
    # Extract metrics
    accuracy <- confusion$overall["Accuracy"]
    precision <- confusion$byClass["Pos Pred Value"]
    recall <- confusion$byClass["Sensitivity"]
    f1_score <- 2 * ((precision * recall) / (precision + recall))
    if(is.na(f1_score)){
      f1_score = 0
    }
    balanced_accuracy = confusion$byClass["Balanced Accuracy"]
    # save in matrix
    metrics_mat[i, ] = c(accuracy, precision, recall, f1_score, balanced_accuracy)
  }
  
  # return average over rows (fold)
  vec_to_ret = colMeans(metrics_mat)
  names(vec_to_ret) =  c("accuracy", "precision", "recall", "f1_score", "balanced_accuracy")
  return(list(vec_to_ret,metrics_mat ))
}




# build all possible combinations
model_combination <- function(
    id_screening,
    var_mat
){
  # Generate all combinations of var_mat and id_screening
  A <- rbind(
    matrix(rep(var_mat,length(id_screening)),nrow=nrow(var_mat)),
    rep(id_screening,each=ncol(var_mat))
  )
  
  # Remove duplicates:
  # remove same model
  A <- unique(apply(A,2,sort), MARGIN = 2)
  
  # remove same attributes
  id <- apply(A,2,anyDuplicated)>0
  if(sum(id)==0){
    return(A)
  }else{
    return(subset(A,select=!id))
  }
}


# 
# # # for testing
# data <- read_csv("not_commit/PreFer_train_data.csv")
# # # backgroud data
# data_back <- read_csv("not_commit/PreFer_train_background_data.csv")
# cleaned_df = clean_df(df = data, background_df = data_back)
# outcome_df = read_csv(file = "not_commit/PreFer_train_outcome.csv")
# 


train_save_model <- function(cleaned_df, outcome_df) {
  # Trains a model using the cleaned dataframe and saves the model to a file.

  # Parameters:
  # cleaned_df (dataframe): The cleaned data from clean_df function to be used for training the model.
  # outcome_df (dataframe): The data with the outcome variable (e.g., from PreFer_train_outcome.csv or PreFer_fake_outcome.csv).

  ## This script contains a bare minimum working example
  # set.seed(1) # not useful here because logistic regression deterministic
  
  # Combine cleaned_df and outcome_df and keep only observation for which we observe data
  model_df <- merge(cleaned_df, outcome_df, by = "nomem_encr") %>%
    filter(!is.na(new_child))
  
  # train SWAG with Random forest
  X = model_df %>% dplyr::select(-c(nomem_encr, nomem_encr_cube, nomem_encr_square, nomem_encr_sqrt_root, new_child))
  y = model_df %>% pull(new_child)
  
  # Meta-parameters swag algorithm
  control <- list(pmax = 35,  # maximum dimension explored
                  alpha = .1, #normally a small value, corresponds to the kept at each iteration
                  m = 80L, # max number of models explored per dimension
                  seed = 123L, #for replicability
                  verbose = T #keeps track of completed dimensions)
  )
  
  dim_x <- dim(X)
  p <- dim_x[2]
  n <- dim_x[1]
  
  ## Object storage
  CVs <- vector("list",control$pmax)
  CVs_f1_score <- vector("list",control$pmax)
  CVs_accuracy <- vector("list",control$pmax)
  CVs_precison <- vector("list",control$pmax)
  CVs_recall <- vector("list",control$pmax)
  IDs <- vector("list",control$pmax)
  VarMat <- vector("list",control$pmax)
  Sample_size  <- vector("list",control$pmax)
  
  # create vector to save results of dimension 1
  cv_alpha <- rep(NA,control$pmax)
  cv_errors <- rep(NA,ncol(X))
  cv_errors_f1_score <- rep(NA,ncol(X))
  cv_errors_accuracy <- rep(NA,ncol(X))
  cv_errors_precison <- rep(NA,ncol(X))
  cv_errors_recall <- rep(NA,ncol(X))
  
  # set dimension to 1
  d=1
  
  # create sample size vector to save in list
  sample_size = vector(mode="numeric", length=ncol(X))
  
  # first step screen all variables
  for(i in seq(ncol(X))){
    
    # merge x and y and remove rows with NA at this step
    df_sub = data.frame(new_child = y, X %>% dplyr::select(all_of(i))) %>% na.omit()
    res_cv =  cv_k_fold_rf(df = df_sub, formula ="new_child~.", k = 10)[[1]]
    cv_errors[i] = res_cv["balanced_accuracy"]
    cv_errors_f1_score[i] <- res_cv["f1_score"]
    cv_errors_accuracy[i] <-res_cv["accuracy"]
    cv_errors_precison[i] <- res_cv["precision"]
    cv_errors_recall[i] <- res_cv["recall"]
    sample_size[i] = nrow(df_sub)
    
    if(control$verbose){
      cat(paste0("Variable ",i,"/", ncol(X),". balanced accuracy of ", round(cv_errors[i] , 4), "\n"))
      
    }
  }

  # create varmat for dimension 1
  var_mat <- seq_len(p)
  dim(var_mat) <- c(1,p)
  CVs[[d]] <- cv_errors
  CVs_f1_score[[d]] = cv_errors_f1_score
  CVs_accuracy[[d]] = cv_errors_accuracy
  CVs_precison[[d]] = cv_errors_precison
  CVs_recall[[d]] = cv_errors_recall
  
  cv_alpha[d] <- quantile(cv_errors, (1-control$alpha), na.rm=T)
  IDs[[d]] <- which(cv_errors >= cv_alpha[d])
  id_screening <- IDs[[d]]
  VarMat[[d]] <- var_mat
  Sample_size[[d]] = sample_size
  
  for(d in 2:control$pmax){
    
    # d=2
    
    # Build all combinations
    var_mat <- model_combination(id_screening,subset(VarMat[[d-1L]], select=IDs[[d-1]]))
    
    # Reduce number of model if exceeding `m` and if dimension is greater than 1
    if(ncol(var_mat)>control$m){
      set.seed(123)
      var_mat <- var_mat[,sample.int(ncol(var_mat),control$m)]
    }
    
    # Compute CV errors
    cv_errors <- rep(NA,ncol(var_mat))
    cv_errors_f1_score   <- rep(NA,ncol(var_mat))
    cv_errors_accuracy <- rep(NA,ncol(var_mat))
    cv_errors_precison <- rep(NA,ncol(var_mat))
    cv_errors_recall <- rep(NA,ncol(var_mat))
    
    
    sample_size = vector(mode = "numeric", length = ncol(var_mat))
    
    for(i in seq_len(ncol(var_mat))){
      
      # merge x and y and remove rows with NA at this step
      df_sub = suppressMessages(dplyr::bind_cols(y, X[,var_mat[,i]]))
      df_sub_2 = as.data.frame(na.omit(df_sub))
      colnames(df_sub_2)[1] = "new_child"
      
      res_cv = cv_k_fold_rf(df = df_sub_2, formula ="new_child~.", k = 10)[[1]]
      cv_errors[i] = res_cv["balanced_accuracy"]
      cv_errors_f1_score[i] <- res_cv["f1_score"]
      cv_errors_accuracy[i] <-res_cv["accuracy"]
      cv_errors_precison[i] <- res_cv["precision"]
      cv_errors_recall[i] <- res_cv["recall"]
      
      # cat(paste0("CV computed for combination of variables ", paste(var_mat[,i], collapse = "-"), "\n"))
      sample_size[i] = nrow(df_sub_2)
      
    }
    
    
    # Store results
    CVs[[d]] <- cv_errors
    CVs_f1_score[[d]] = cv_errors_f1_score
    CVs_accuracy[[d]] = cv_errors_accuracy
    CVs_precison[[d]] = cv_errors_precison
    CVs_recall[[d]] = cv_errors_recall
    VarMat[[d]] <- var_mat
    cv_alpha[d] <- quantile(cv_errors,probs = (1-control$alpha),na.rm=T)
    IDs[[d]] <- which(cv_errors >= cv_alpha[d])
    # if proportion of f1 score per tested model , switch to f1 score, to do, for now...
    measure_considered= " balanced accuracy"
    if(d > 10){
      # switch for f1 score
      measure_considered= " f1 score"
      cv_alpha[d] <- quantile(cv_errors_f1_score,probs = (1-control$alpha), na.rm=T) # save in vector of quantile per dimension
      IDs[[d]] <- which(cv_errors_f1_score >= cv_alpha[d])
    }
    
    Sample_size[[d]] = sample_size
    
    
    if(control$verbose) print(paste0("Dimension explored: ", d ,",", measure_considered," at quantile ",(1-control$alpha),":" ,round(cv_alpha[d],4)))
    if(ncol(var_mat)==1) break 
  }
  
  
  # ---------- format output from swag
  out = structure(
    list(
      x=colnames(X),
      y=y,
      control=control,
      CVs_balanced_accuracy=CVs[1:d],
      CVs_f1_score =  CVs_f1_score[1:d] ,
      CVs_accuracy = CVs_accuracy[1:d],
      CVs_precison = CVs_precison[1:d],
      CVs_recall = CVs_recall[1:d],
      VarMat=VarMat[1:d],
      cv_alpha=cv_alpha[1:d],
      IDs=IDs[1:d]
    ),
    class="swag"
  )
  
  # save for dev
  # save(out, file = "out.rda")
  
  # extract best model
  
  # construct set of best models
  total_dim_explored = length(out$CVs_f1_score)
  dim_up_to = min(total_dim_explored, 35)
  quantile_f1_score_to_consider = quantile(out$CVs_f1_score[[dim_up_to]], probs = .9,na.rm = T)
  
  find_models_above_treshold<- function(vec, threshold) {
    which(vec > threshold)
  }
  
  # construct best model id list
  list_best_model <- lapply(out$CVs_f1_score, find_models_above_treshold, quantile_f1_score_to_consider)
  
  # obtain the variables associated with each best models
  extract_associated_variables = function(list_varmat, list_best_model){
    
    max_dimension = length(list_best_model)
    
    list_varmat_best_model = vector("list", max_dimension)
    
    for(i in seq(max_dimension)){
      
      if(length(list_best_model[[i]]) == 0){
        next
      }else{
        list_varmat_best_model[[i]] = list_varmat[[i]][, list_best_model[[i]]]
      }
    }
    return(list_varmat_best_model)
  }
  
  
  list_varmat_best_model = extract_associated_variables(list_varmat = out$VarMat, list_best_model = list_best_model)
  
  # check number of best models
  total_nbr_of_best_models = sum(unlist(lapply(list_best_model, FUN = function(x){length(x)})))
  # total_nbr_of_best_models
  
  # create a big model that countain all ranger models
  create_all_models = function(list_varmat_best_model, X, y, verbose=T){
    
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
        
        names_var_mod = colnames(X)[list_varmat_best_model[[i]]]
        df_sub = suppressMessages(dplyr::bind_cols(y, X %>% select(all_of(names_var_mod))))
    
        colnames(df_sub)[1] = "new_child"
        df_sub_no_na = na.omit(df_sub)
        df_sub_no_na$new_child = as.factor(df_sub_no_na$new_child)
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
          
          names_var_mod = colnames(X)[var_mat_dim_i[,j]]
          df_sub = suppressMessages(dplyr::bind_cols(y, X %>% select(all_of(names_var_mod))))
          
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
  
  # estimate all best models
  set_all_best_models = create_all_models(list_varmat_best_model = list_varmat_best_model, verbose = T, X=X, y=y)
 
  # save list of estimated models
  model = list(
    "set_all_best_models" = set_all_best_models, # contain all estimated RF
    "list_varmat_best_model" = list_varmat_best_model, # contain the index of the variables of each top model
    "colnames_X" = colnames(X) # contain the names of the variables of the training set
  )
  
  # Save the model
  saveRDS(model, "model.rds")
  
  # Inform the user
  cat("Model saved at ", "model.rds", "\n")
  
  # --------------------- previous approach of Aldo

  # # Logistic regression model
  # #model <- gam(new_child ~ factor(employment_status)+s(birthyear_bg)+factor(partner_2020)+religious_participation+factor(type_dwelling)+factor(mig_by_origin)+factor(civil_status)+factor(cf20m128), family = "binomial", data = model_df)
  # 
  # model_df$employment_status<-as.factor(model_df$employment_status)
  # model_df$partner_2020<-as.factor(model_df$partner_2020)
  # # model_est <- model_df[, c("new_child","employment_status","birthyear_bg","partner_2020")]
  # # model_est<-model_est[complete.cases(model_est),]
  # # model <- spikeSlabGAM(new_child ~(employment_status+birthyear_bg+partner_2020)^2, data = model_est, family = "binomial")
  # 
  # model_est <- model_df[, c("new_child","having_children_future","years_next_children",
  #                           "desired_nr_add_children","birthyear_bg","partner_2020",
  #                           "log_net_personal_income")]
  # model_est<-model_est[complete.cases(model_est),]
  # model <- spikeSlabGAM(new_child ~(having_children_future+
  #                                     years_next_children+desired_nr_add_children+
  #                                     birthyear_bg+partner_2020)^2+log_net_personal_income,
  #                       data = model_est, family = "binomial")





}


# train_save_model(cleaned_df = cleaned_df, outcome_df = outcome_df)
# 


