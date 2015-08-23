extractdata <- function(file) {
  #url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
  #download.file(url, destfile = './dataclean/set.zip', mode = 'wb')
  utils::unzip(file) #, exdir = './')
}

loaddata <- function() {
  bpath <- './UCI HAR Dataset'
  fname_xtrain <- file.path(bpath, 'train', 'X_train.txt')
  fname_ytrain <- file.path(bpath, 'train', 'y_train.txt')   
  fname_xtest  <- file.path(bpath, 'test', 'X_test.txt')
  fname_ytest  <- file.path(bpath, 'test', 'y_test.txt')
  fname_subtrain <- file.path(bpath, 'train', 'subject_train.txt')
  fname_subtest  <- file.path(bpath, 'test' , 'subject_test.txt')
  features_file <- file.path(bpath, 'features.txt')
  ff <- read.table(features_file)
  feature_name <<- as.vector(ff[,2])
  xtrain <<- read.table(fname_xtrain)
  names(xtrain) <<- feature_name
  ytrain <<- read.table(fname_ytrain)
  names(ytrain) <<- c('Activity')
  xtest  <<- read.table(fname_xtest)
  names(xtest) <<- feature_name
  ytest  <<- read.table(fname_ytest)
  names(ytest) <<- c('Activity')
  subtrain <<- read.table(fname_subtrain)
  names(subtrain) <<- c('Subject')
  subtest  <<- read.table(fname_subtest)
  names(subtest) <<- c('Subject')
  labels_file <- file.path(bpath, 'activity_labels.txt')
  act_labels  <<- read.table(labels_file)
  names(act_labels) <<- c('Activity', 'ActivityName')
}

getname <- function() {
  rname <- c('Subject',
                 'Mean of Body Acceleration in X-axis (Time Domain)',
                 'Mean of Body Acceleration in Y-axis (Time Domain)',
                 'Mean of Body Acceleration in Z-axis (Time Domain)',
                 'Standard Deviation of Body Acceleration in X-axis (Time Domain)',
                 'Standard Deviation of Body Acceleration in Y-axis (Time Domain)',
                 'Standard Deviation of Body Acceleration in Z-axis (Time Domain)',
                 'Mean of Gravity Acceleration in X-axis (Time Domain)',
                 'Mean of Gravity Acceleration in Y-axis (Time Domain)',
                 'Mean of Gravity Acceleration in Z-axis (Time Domain)',
                 'Standard Deviation of Gravity Acceleration in X-axis (Time Domain)',
                 'Standard Deviation of Gravity Acceleration in Y-axis (Time Domain)',
                 'Standard Deviation of Gravity Acceleration in Z-axis (Time Domain)',
                 'Mean of Body Acceleration Jerk Signal in X-axis (Time Domain)',
                 'Mean of Body Acceleration Jerk Signal in Y-axis (Time Domain)',
                 'Mean of Body Acceleration Jerk Signal in Z-axis (Time Domain)',
                 'Standard Deviation of Body Acceleration Jerk Signal in X-axis (Time Domain)',
                 'Standard Deviation of Body Acceleration Jerk Signal in Y-axis (Time Domain)',
                 'Standard Deviation of Body Acceleration Jerk Signal in Z-axis (Time Domain)',
                 'Mean of Body Gyroscope in X-axis (Time Domain)',
                 'Mean of Body Gyroscope in Y-axis (Time Domain)',
                 'Mean of Body Gyroscope in Z-axis (Time Domain)',
                 'Standard Deviation of Body Gyroscope in X-axis (Time Domain)',
                 'Standard Deviation of Body Gyroscope in Y-axis (Time Domain)',
                 'Standard Deviation of Body Gyroscope in Z-axis (Time Domain)',
                 'Mean of Body Gyroscope Jerk Signal in X-axis (Time Domain)',
                 'Mean of Body Gyroscope Jerk Signal in Y-axis (Time Domain)',
                 'Mean of Body Gyroscope Jerk Signal in Z-axis (Time Domain)',
                 'Standard Deviation of Body Gyroscope Jerk Signal in X-axis (Time Domain)',
                 'Standard Deviation of Body Gyroscope Jerk Signal in Y-axis (Time Domain)',
                 'Standard Deviation of Body Gyroscope Jerk Signal in Z-axis (Time Domain)',
                 'Mean of Body Acceleration Magnitude (Time Domain)',
                 'Standard Deviation of Body Acceleration Magnitude (Time Domain)',
                 'Mean of Gravity Acceleration Magnitude (Time Domain)',
                 'Standard Deviation of Gravity Acceleration Magnitude (Time Domain)',
                 'Mean of Body Acceleration Jerk Signal Magnitude (Time Domain)',
                 'Standard Deviation of Body Jerk Signal Acceleration Magnitude (Time Domain)',
                 'Mean of Body Gyroscope Magnitude (Time Domain)',
                 'Standard Deviation of Body Gyroscope Magnitude (Time Domain)',
                 'Mean of Body Gyroscope Jerk Signal Magnitude (Time Domain)',
                 'Standard Deviation of Body Gyroscope Jerk Signal Magnitude (Time Domain)',
                 'Mean of Body Acceleration in X-axis (Frequency Domain)',
                 'Mean of Body Acceleration in Y-axis (Frequency Domain)',
                 'Mean of Body Acceleration in Z-axis (Frequency Domain)',
                 'Standard Deviation of Body Acceleration in X-axis (Frequency Domain)',
                 'Standard Deviation of Body Acceleration in Y-axis (Frequency Domain)',
                 'Standard Deviation of Body Acceleration in Z-axis (Frequency Domain)',
                 'Mean of Body Acceleration Jerk Signal in X-axis (Frequency Domain)',
                 'Mean of Body Acceleration Jerk Signal in Y-axis (Frequency Domain)',
                 'Mean of Body Acceleration Jerk Signal in Z-axis (Frequency Domain)',
                 'Standard Deviation of Body Acceleration Jerk Signal in X-axis (Frequency Domain)',
                 'Standard Deviation of Body Acceleration Jerk Signal in Y-axis (Frequency Domain)',
                 'Standard Deviation of Body Acceleration Jerk Signal in Z-axis (Frequency Domain)',
                 'Mean of Body Gyroscope in X-axis (Frequency Domain)',
                 'Mean of Body Gyroscope in Y-axis (Frequency Domain)',
                 'Mean of Body Gyroscope in Z-axis (Frequency Domain)',
                 'Standard Deviation of Body Gyroscope in X-axis (Frequency Domain)',
                 'Standard Deviation of Body Gyroscope in Y-axis (Frequency Domain)',
                 'Standard Deviation of Body Gyroscope in Z-axis (Frequency Domain)',
                 'Mean of Body Acceleration Magnitude (Frequency Domain)',
                 'Standard Deviation of Body Acceleration Magnitude (Frequency Domain)',
                 'Mean of Body Acceleration Jerk Signal Magnitude (Frequency Domain)',
                 'Standard Deviation of Body Jerk Signal Acceleration Magnitude (Frequency Domain)',
                 'Mean of Body Gyroscope Magnitude (Frequency Domain)',
                 'Standard Deviation of Body Gyroscope Magnitude (Frequency Domain)',
                 'Mean of Body Gyroscope Jerk Signal Magnitude (Frequency Domain)',
                 'Standard Deviation of Body Gyroscope Jerk Signal Magnitude (Frequency Domain)',
                 'Activity')
  rname
}

run_analysis <- function() {
  extractdata('./getdata-projectfiles-UCI HAR Dataset.zip')
  loaddata()
  # merge test and training data
  activity <- rbind(ytrain, ytest)
  measurement <- rbind(xtrain, xtest)
  subject  <- rbind(subtrain, subtest)
  stdmean <- measurement[,grep('[Mm]ean\\(|[Ss]td\\(', feature_name)]
  act_labels[,2] <- gsub('_', ' ', act_labels[,2])
  all <- cbind(subject, activity, stdmean)
  mall <- merge(all, act_labels, by.x = 'Activity', by.y = 'Activity')
  mall$Activity <- NULL
  names(mall) <- getname()
  mres <- mall %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))
  write.table(mres, file = 'tidy.txt', row.names = FALSE)
}