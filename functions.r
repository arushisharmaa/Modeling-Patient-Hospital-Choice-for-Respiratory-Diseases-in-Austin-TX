# Load libraries, add more to libs as needed
libs = c("randomForest", # For random forest modeling
         "rfPermute",    # For random forest feature significance
         "tidyverse",    # For data manipulation and visualization
         "caret",        # For the plotting models
         "nnet",         # For multinom model
         "neuralnet",    # Neural net example
         "xgboost")     # For gradient boosting
invisible(lapply(libs, library, character.only = TRUE))

# Function to perform Multinomial Logistic Regression, Neural Network, and Random Forest with cross-validation
perform_cross_validation <- function(dataset, num_folds=NA, seed=NA, sub_folder="RESPIRATORY/"){
  # Step 1: Data cleaning moved outside function
  # Could add a step to ensure you've passed clean data/check the feature types and correct them here
  
  # Filter out any classes with too few observations, here we're doing 10% of data in each class minimum
  class_proportion = data.frame(table(dataset$THCIC_ID)/nrow(dataset)*100) %>%
    filter(Freq>=1) # we'll only keep classes with at least 1% of data
  dataset = dataset %>%
    filter(THCIC_ID %in% class_proportion$Var1) %>%
    mutate(THCIC_ID = as.factor(THCIC_ID)) %>%
    droplevels() # remove empty levels from analyses below
  #levels(dataset$THCIC_ID) # personal check to see levels were successfully dropped
 
  # If seed is defined by user, then use it. Otherwise will be chosen randomly.
  if(!is.na(seed)){ 
    set.seed(seed)  # For reproducibility
  } # end if
  
  # Step 2: Choose data for each fold of cross-validation
  # Should possibly check there is enough data for the requested number of folds
  # ideally we'll do 80/20 split for data for train/test
  target_num_per_test = round(0.2*nrow(dataset), 0) # 20% of data in test => 80% to train
  target_num_fold = round(nrow(dataset)/target_num_per_test, 0)
  if(!is.na(num_folds)){
    k=num_folds
    warning(paste0("You set num_folds=", num_folds, "\n 80% of data for training would be k=", target_num_fold))
  }else{
    k=target_num_fold 
  } # end if
  
  # This function create k equal size, independent subsets of data
  cv_indices <- createFolds(dataset$THCIC_ID, k = k) 
  
  # Create tibble of empty lists to store evaluation metrics since R doesn't have dictionaries
  evaluation_metrics_df <- tibble(
    Fold = integer(),
    Train_Data = list(),
    Test_Data = list(),
    Multinom_Model = list(),
    Multinom_ConfusionMatrix = list(),
    #Neural_Network_Model = list(),
    #Neural_Network_ConfusionMatrix = list(),
    RF_Model = list(),
    RF_ConfusionMatrix = list(),
    Variable_Importance = list()
    #stringsAsFactors = FALSE
    )

  # Loop through folds
  for(fold in seq_along(cv_indices)){
    train_indices <- unlist(cv_indices[-fold])
    test_indices  <- unlist(cv_indices[fold])
    
    train_data <- dataset[train_indices, ]
    test_data  <- dataset[test_indices, ]
    
    cat(paste0("Fold:", fold, ", Training Data Size:", nrow(train_data), ", Test Data Size:", nrow(test_data)), "\n")
    
    # Multinomial Logistic Regression
    multinom_model <- 
                multinom(THCIC_ID ~ RACE + ZCTA_SVI + drive_time + SPEC_UNIT_1 + ETHNICITY + PAT_AGE_ORDINAL, 
                         data = train_data, maxit = 1000, trace=F) # trace default is TRUE to see convergence
    
    # Predictions and evaluation
    predicted_data_mn <- predict(multinom_model, newdata = test_data)
    cm_multinom <- caret::confusionMatrix(as.factor(predicted_data_mn), as.factor(test_data$THCIC_ID))
    # Neural Network Model
    #nn_model <- neuralnet(THCIC_ID ~ RACE + ZCTA_SVI + drive_time + SPEC_UNIT_1 + ETHNICITY + PAT_AGE_ORDINAL, data = train_data, hidden = c(5, 3), linear.output = TRUE)
    
    # Predictions and evaluation
    #predicted_data_nn <- predict(nn_model, newdata = test_data)
    
    
    # Build a Balanced Random Forest Model & Check the Accuracy 
    sampsize = balancedSampsize(train_data$THCIC_ID)
    rfPermute_model = rfPermute(THCIC_ID ~ RACE + ZCTA_SVI + drive_time + SPEC_UNIT_1 + ETHNICITY + PAT_AGE_ORDINAL, 
                                data = train_data, ntree = 500, num.rep = 1000, num.cores = 6,
                                replace = FALSE, sampsize = sampsize)
    
    rfPermute_performance = predict(rfPermute_model, newdata = test_data)
    rf_conf_mat <- caret::confusionMatrix(as.factor(rfPermute_performance), as.factor(test_data$THCIC_ID))
    
    # Calculate variable importance for Balanced Random Forest
    var_importance <- importance(rfPermute_model)
    
    # Store metrics in the data frame
    evaluation_metrics_df <- bind_rows(
      evaluation_metrics_df,
      tibble(
        Fold                            = fold,
        Train_Data                      = list(train_data),
        Test_Data                       = list(test_data),
        Multinom_Model                  = list(multinom_model),
        Multinom_ConfusionMatrix        = list(cm_multinom),
        RF_Model                        = list(rfPermute_model),
        RF_ConfusionMatrix              = list(rf_conf_mat),
        Variable_Importance             = list(var_importance), 
      )
    )
  } # end loop over cv_indices
  
  # Compare results across folds
  #multinom_accuracies <- numeric(length(evaluation_metrics_df$Multinom_ConfusionMatrix))
  
  # Loop through each confusion matrix and calculate accuracy
  # This could get combined with loop above, unless you want to add some error checks
  overall_model_summary = data.frame()
  class_model_summary   = data.frame()
  for (i in seq_along(cv_indices)){
    ######
    # Multinomial regression model summary stats overall and by class
    mn_cm <- evaluation_metrics_df$Multinom_ConfusionMatrix[[i]]
    # AccuracyNull = no information rate,"which is taken to be the largest class percentage in the data"
    # I.e. model should do much better than if we always chose the largest class as our guess
    mn_accuracies_temp <- data.frame(fold=i, t(mn_cm$overall)) %>%
      mutate(model="MN")
    
    # get Multinomial summary stats for all classes
    mn_temp = data.frame(fold=i, mn_cm$byClass) %>%
      rownames_to_column() %>%
      rename(THCIC_ID=rowname) %>%
      mutate(THCIC_ID = gsub("Class: ", "", THCIC_ID), # get rid of word class before THCIC_ID
             model = "MN") # label the model
    
    ######
    # Random Forest model summary stats overall and by class
    rf_cm = evaluation_metrics_df$RF_ConfusionMatrix[[i]]
    # the AccuracyNull here is calculated in a different way than just largest class, but need to see how
    rf_accuracies_temp <- data.frame(fold=i, t(rf_cm$overall)) %>%
      mutate(model="RF") %>%
      bind_rows(mn_accuracies_temp)
    
    rf_temp = data.frame(fold=i, rf_cm$byClass) %>%
      rownames_to_column() %>%
      rename(THCIC_ID=rowname) %>%
      mutate(THCIC_ID = gsub("Class: ", "", THCIC_ID),
             model = "RF") %>% # get rid of word class before THCIC_ID
      bind_rows(mn_temp)
    
    # rbind each summary stats from all folds
    overall_model_summary  = rbind(overall_model_summary, rf_accuracies_temp)
    class_model_summary = rbind(class_model_summary, rf_temp)
  } # end for i
  

  # Print or visualize the accuracies
  cat("Multinomial Logistic Regression Accuracies:\n")
  print(multinom_accuracies)
  
  #View(evaluation_metrics_df)
  return(evaluation_metrics_df)
} # end function perform_cross_validation

}

print_accuracies <- function(result, fold_value, matrix_type) {
  # Create a data frame to store the values
  accuracies <- data.frame(
    Fold = integer(),
    Accuracies = numeric()
  )
  
  # Store Overall values for the specified Confusion Matrix type
  overall_value <- result[[paste0(matrix_type)]][[5]]$overall[c(1, 3, 4)]
  accuracies <- rbind(accuracies, Fold = fold_value, data.frame(Accuracies = overall_value))

  # Return the data frame if needed
  return(accuracies)
}
