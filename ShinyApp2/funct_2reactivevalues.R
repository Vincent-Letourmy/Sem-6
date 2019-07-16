function_reactiveValues <- function() {
  reactiveValues(
    
                 dataframe_initialisation = NULL,
                 dataframe_initialisationBis = NULL,

                 dataframe_targetconfig = NULL,
                 
                 dataframe_withoutcolselected = NULL,

                 dataframe_dataqualityconfig = NULL,
                 dataframe_dataqualityconfigBis = NULL,
                 
                 df_types = NULL,
                 df_ranges = NULL,
                 nbRowRemovedConsistency = NULL,
                 
                 dataframe_costsconfig = NULL,
                 
                 dataframe_fixing = NULL,
                 
                 dataframe_results = NULL,
                 
                 columnSelected = NULL,
                 
                 tabCosts = NULL,
                 validate = FALSE,
                 
                 # Results
                 resultData = NULL, 
                 accuracy = NULL, 
                 accuracyTab = NULL,
                 resMissingValuesBarChart = NULL,
                 
                 # Initialisation df to compare
                 accuracySaved = NULL,
                 accuracyTabSaved = NULL,
                 resultDataSaved = NULL,
                 
                 # Fixed df
                 accuracyFixed = NULL,
                 accuracyTabFixed = NULL,
                 resultDataFixed = NULL
                 
  )
}