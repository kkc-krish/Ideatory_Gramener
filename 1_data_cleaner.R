train <- read.csv("./raw_dataset/train.csv",stringsAsFactors = FALSE)
test <- read.csv("./raw_dataset/test.csv",stringsAsFactors = FALSE)

target <- train[, 21]
train <- train[, 1:20]
test <- test[, 1:20]

colnames(test) <- colnames(train)

proper_feature_names <- function(input_table){
  
  #--------------------------------------
  #
  #
  #
  #--------------------------------------
  
  colnames(input_table) <- tolower(colnames(input_table))
  colnames(input_table) <- gsub('([[:punct:]])|\\s+','_',colnames(input_table))
  while (any(grepl("__",colnames(input_table),fixed = TRUE)) == TRUE){
    colnames(input_table) <- gsub("__","_",colnames(input_table),fixed = TRUE) 
  }
  colnames(input_table) <- gsub("\\*$", "",colnames(input_table))
  return(input_table)
}


dummygen <- function(new_table, original_table, dummified_column, column_values, new_name){ 
  
  #-----------------------------------------------------------------
  # INPUT 1. -- The new cleaned table -- I will attach the dummies.
  # INPUT 2. -- The original table that is being cleaned.
  # INPUT 3. -- The column that has the strings.
  # INPUT 4. -- The unique values in the column encoded.
  # INPUT 5. -- The new name of the columns.
  # OUTPUT -- The new table with the dummy variables.
  #-----------------------------------------------------------------
  
  i <- 0
  
  for (val in column_values){
    i <- i + 1
    new_variable <- data.frame(matrix(0, nrow(new_table), 1))
    new_variable[original_table[,dummified_column] == val, 1] <- 1
    colnames(new_variable) <- paste0(new_name, i)
    new_table <- cbind(new_table,new_variable)
  }
  return(new_table)
}


train <- proper_feature_names(train)
test <- proper_feature_names(test)

data_munger <- function(input_table){
  
  #------------------------------------
  # INPUT 1.: The table to be cleaned.
  # OUTPUT: The cleaned numeric tables.
  #------------------------------------
  
  #----------------------------------------------
  # Defining a target table for the cleaned data.
  #----------------------------------------------
  
  new_table <- data.frame(matrix(0, nrow(input_table), 1))
  new_table[, 1] <- 1:nrow(input_table)
  
  #-----------------------------------------------------
  # The first variable is an artifical ID.
  #-----------------------------------------------------
  
  colnames(new_table) <- c("id")
  
  #--------------------------
  # Account length in weeks.
  #--------------------------
  
  new_table$account_length <- input_table$account_length_weeks
  
  #----------------
  # Message number.
  #----------------
  
  new_table$message_number <- input_table$messages
  
  #------------------
  # Message behavior.
  #------------------
  
  new_table$message_to_account <- log(new_table$message_number/new_table$account_length)
  
  #--------------------------
  # Minutes basic pass down.
  #--------------------------
  
  new_table$day_min <- input_table$day_mins
  
  new_table$eve_min <- input_table$eve_mins
  
  new_table$night_min <- input_table$night_mins
  
  new_table$intl_min <- input_table$intl_mins
  
  #----------------------------
  # Minutes to account length.
  #----------------------------
  
  new_table$day_min_per_a <- input_table$day_mins/input_table$account_length_weeks
  
  new_table$eve_min_per_a <- input_table$eve_mins/input_table$account_length_weeks
  
  new_table$night_min_per_a <- input_table$night_mins/input_table$account_length_weeks
  
  new_table$intl_min_per_a <- input_table$intl_mins/input_table$account_length_weeks
  
  #-------------------------
  # Customer serivce calls.
  #-------------------------

  new_table$customer <- input_table$custserv_calls
  
  #-----------------------------
  # International call package.
  #-----------------------------
  
  new_table$international_plan <- new_table$intl_mins
  
  #-----------------------
  # Message plan package.
  #-----------------------
  
  new_table$message_plan <- input_table$message_plan
  
  #----------------------
  # Raw calls variables.
  #----------------------
  
  new_table$day_calls <- input_table$day_calls
  
  new_table$eve_calls <- input_table$eve_calls
  
  new_table$night_calls <- input_table$night_calls
  
  new_table$intl_calls <- input_table$intl_calls
  
  #-----------------------------
  # Call number to account time.
  #-----------------------------
  
  new_table$day_calls_per_a <- input_table$day_calls/input_table$account_length_weeks
  
  new_table$eve_calls_per_a <- input_table$eve_calls/input_table$account_length_weeks
  
  new_table$night_calls_per_a <- input_table$night_calls/input_table$account_length_weeks
  
  new_table$intl_calls_per_a <- input_table$intl_calls/input_table$account_length_weeks
    
  #-------------------------
  # Call time to call number
  #-------------------------
  
  new_table$day_time_to_number <- input_table$day_mins/(input_table$day_calls + 1)
    
  new_table$eve_time_to_number <- input_table$eve_mins/(input_table$eve_calls + 1)
    
  new_table$night_time_to_number <- input_table$night_mins/(input_table$night_calls + 1)  
    
  new_table$intl_time_to_number <- input_table$intl_mins/(input_table$intl_calls + 1) 
  
  #-------------------------------
  # Dealing with the state string
  #-------------------------------
  
  state <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN",
             "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ",
             "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA",
             "WI", "WV", "WY")
  
  new_table <- dummygen(new_table, input_table, "state", state, "state")
  
  #-----------
  # Area code
  #-----------
  
  area_code <- c(408, 415, 510)
  
  new_table <- dummygen(new_table, input_table, "area_code", area_code, "area_code")
  
  #--------------------
  # Phone manipulation
  #--------------------
  
  input_table$phone <- substr(input_table$phone,1,3)
  
  phone <- as.character(c(382:422))
  
  new_table <- dummygen(new_table, input_table, "phone", phone, "phone")
  
  
  return(new_table)
}


train <- data_munger(train)
test <- data_munger(test)

write.csv(target, "./clean_dataset/target.csv", row.names = FALSE)
write.csv(train,"./clean_dataset/train.csv", row.names = FALSE)
write.csv(test,"./clean_dataset/test.csv", row.names = FALSE)
