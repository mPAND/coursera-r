#
# Write a function called best that take two arguments: the 2-character
# abbreviated name of a state and an outcome name. The function reads the 
# outcome-of-care-measures.csv file and returns a character vector with the 
# name of the hospital that has the best (i.e. lowest) 30-day mortality for the 
# specified outcome in that state. The hospital name is the name provided in the 
# Hospital.Name variable. The outcomes can be one of “heart attack”, 
# “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
#

best <- function(state,outcome) {

# Read from file
    a <- read.table("outcome-of-care-measures.csv" , header = TRUE, sep =",", na.strings ="Not Available" )

# Use only necessary columns
    b <- a[,c(2,7,11,17,23)]

# naming the attributes
    names(b) <-c("hname","state","heart attack","heart failure","pneumonia")

# Getting the list of state codes
    state_check <- unique(b$state)

# I/P check for invalid State
    if( ! any(state_check==state) ) {
        stop("invalid state")
    }

# List of Outcomes
    outcome_check <- c("heart attack","heart failure","pneumonia")

# I/P check for invalid outcome
    if ( ! any (outcome_check == outcome) )  {
        stop("invalid outcome")
    }

# Splitting the df into list of data frames based on state
    c <- split(b, b$state)

# Retaining the necessary state
    d <- as.data.frame( c[state] )

# Naming the attributes and taking necessary outcome
    names(d) <-c("hname","state","heart attack","heart failure","pneumonia")
    e <- d[,c("hname","state",outcome)]

# order based on outcome and hname and ranking it
    f <- e[order(e[outcome],e["hname"]),]
    g<-f[complete.cases(f),]
    g$rank <- seq_len(nrow(g))

    return(  as.vector(g[g$rank==1,"hname"]) )

}


# Test function calls
#best("TX","heart attack")
#best("TX","heart failure")
#best("MD","heart attack")
#best("MD","pneumonia")
#best("TA","heartfailure")

