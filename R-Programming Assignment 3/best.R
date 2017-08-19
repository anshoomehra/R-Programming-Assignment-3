#Created By: Anshoo Mehra
#Course: Coursera Data Science, R Programming

# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# 
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,

# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message “invalid state”. If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message “invalid outcome”.
# Here is some sample output from the function.
# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome
# >

# example output:
#> best("NC", "heart attack") 
#[1] "CAROLINAS MEDICAL CENTER-NORTHEAST"

best <- function(state, outcome) {
        ## Read hospital care data
        data <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
        
        ## Dataframe (subsetting) with relevant data columns 
        hosp_data_filtered   <- as.data.frame(cbind(data[, 2],   # hospital
                                    data[, 7],   # state
                                    data[, 11],  # heart attack
                                    data[, 17],  # heart failure
                                    data[, 23]), # pneumonia
                              stringsAsFactors = FALSE)
        
        #Label Columns
        colnames(hosp_data_filtered) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        if(!state %in% hosp_data_filtered[, "state"]){
                stop('Invalid State..')
        } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop('Invalid Outcome..')
        } else {
                #Subset Data for enterd State, retrieve Rows Indexes
                state_row_idx <- which(hosp_data_filtered[, "state"] == state)
                #Extracting data for the entered state
                state_data <- hosp_data_filtered[state_row_idx, ]    
                #Extract 30 day Mortality Numbers 
                thirty_day_mort <- as.numeric(state_data[, eval(outcome)])
                #Remove hospitals with NA, and find minimum
                min_val <- min(thirty_day_mort, na.rm = TRUE)
                #Identify Hospital with lowest mortality
                result  <- state_data[, "hospital"][which(thirty_day_mort == min_val)]
                output  <- result[order(result)]
        }
        return(output)
}

