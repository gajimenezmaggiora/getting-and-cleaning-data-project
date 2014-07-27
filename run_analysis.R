main <- function(debug=F) {
  # Function reads the raw UCI HAR Data set and produces a merged, tidy, analysis-read
  # data set that contains the mean values of each variable by subject and activity and
  # is written to the output.csv file
  
  # Define source data path
  source_dir <- './UCI HAR Dataset'
  
  # Check that we have a source data directory
  if ( !file.exists(source_dir) ) {
    stop('Source data directory not found')
  }
  
  # Read training data sets
  train_source_dir <- paste(source_dir, 'train', sep='/')
  train_data <- read.table(paste(train_source_dir, 'X_train.txt', sep='/'))
  train_subjects <- read.table(paste(train_source_dir, 'subject_train.txt', sep='/'))
  train_activities <- read.table(paste(train_source_dir, 'y_train.txt', sep='/'))
  
  # Merge training data sets
  train_data_merge <- cbind(train_subjects, train_activities, train_data)        
  
  if( debug ){
    print('Constructing training data set:')
    print(dim(train_data_merge))
  }
  
  # Read testing data sets
  test_source_dir <- paste(source_dir, 'test', sep='/')
  test_data <- read.table(paste(test_source_dir, 'X_test.txt', sep='/'))
  test_subjects <- read.table(paste(test_source_dir, 'subject_test.txt', sep='/'))
  test_activities <- read.table(paste(test_source_dir, 'y_test.txt', sep='/'))
  
  # Merge testing data sets
  test_data_merge <- cbind(test_subjects, test_activities, test_data)        

  if( debug ){
    print('Constructing testing data set:')
    print(dim(test_data_merge))
  }
  
  # Union both data sets
  data_temp <- rbind(train_data_merge, test_data_merge)

  if( debug ){
    print('Constructing combined temp data set:')
    print(dim(data_temp))
  }
  
  # Get variable names
  features <- read.table(paste(source_dir, 'features.txt', sep='/'))
  
  # Clean variable names
  var_names <- unlist(lapply(features[,2], clean_variable_name, debug=debug))
  
  if( debug ){
    print('Constructing variable names:')
    print(var_names)
  }
  
  # Update merged data frame with variable names
  colnames(data_temp) <- c('subject', 'activity', var_names)
    
  # Get activity names
  activities <- read.table(paste(source_dir, 'activity_labels.txt', sep='/'))
  
  # Convert activity values to names
  data_temp[,2] <- activities[data_temp[,2],2]
    
  # Create final data set that includes only mean and std variables
  data_final <- data_temp[, c(1:2, which(grepl('_mean|_std', colnames(data_temp))))]
  
  # Remove _ from final variable names
  colnames(data_final) <- gsub('_', '', colnames(data_final))
  
  if( debug ){
    print('Construct final data set:')
    print(str(data_final))
  }
  
  # Calculate means for final data set by subject and activity
  results <- aggregate(data_final[,3:ncol(data_final)], by=list(subject=data_final$subject, activity=data_final$activity), mean)
  
  # Write the results to a csv file
  write.csv(results, './output.csv', row.names=F)
  
}

clean_variable_name <- function(name, debug=F) {
  # Function returns a clean variable name
  
  # Lowercase
  name<-tolower(name)
  
  # Replace - and , with _
  name<-gsub('-|,', '_', name)
  
  # Remove ()
  name<-gsub('\\(\\)', '', name)
  
  # Replace ( with _
  name<-gsub('\\(', '_', name)
  
  # Remove )
  name<-gsub('\\)', '', name)
  
  if( debug ) {
    print('Clean variable name:')
    print(name)
  }
  
  return(name)
}

# Call main() with debug=T to get debugging information
main(debug=F)