#
# Write a function called rankhospital that takes three arguments: the 2-character
# abbreviated name of a state (state), an outcome (outcome), and the ranking of a 
# hospital in that state for that outcome (num). The function reads the 
# outcome-of-care-measures.csv file and returns a character vector with the name 
# of the hospital that has the ranking specified by the num argument
# The num argument can take values “best”, “worst”, or an integer indicating the 
# ranking (smaller numbers are better). If the number given by num is larger than 
# the number of hospitals in that state, then the function should return NA. 
# Hospitals that do not have data on a particular outcome should be excluded from 
# the set of hospitals when deciding the rankings
#
rankhospital <- function(state,outcome,num="best") {

# read from file, eliminate unnecessary columns and name it

    a <- read.table("outcome-of-care-measures.csv" , header = TRUE, sep =",", na.strings ="Not Available" )
    b <- a[,c(2,7,11,17,23)]
    names(b) <-c("hname","state","heart attack","heart failure","pneumonia")

# list of all valid states
   state_check <- unique(b$state)

# Check I/P for invalid state
    if( ! any(state_check==state) ) {
        stop("invalid state")
    }

# List of all possible outcomes
   outcome_check <- c("heart attack","heart failure","pneumonia")

# Check I/P for invalid outcome
    if ( ! any (outcome_check == outcome) )  {
        stop("invalid outcome")
    }

# Split based on state and retain necessary state data & outcome
    
    c <- split(b, b$state)
    d <- as.data.frame( c[state] )
    names(d) <-c("hname","state","heart attack","heart failure","pneumonia")
    e <- d[,c("hname","state",outcome)]

# Order based on outcome , hname and rank it    

    f <- e[order(e[outcome],e["hname"]),]
    g<-f[complete.cases(f),]
    g$rank <- seq_len(nrow(g))

# Get maximum rank
    rmax = max(g$rank)
    rmin = min(g$rank)

# return 1st rank if i/p is best
    if (num == "best" ) {
        return(  as.vector(g[g$rank==rmin,"hname"]) )
    }

# return last rank if i/p is worst
    if (num == "worst" ) {
        return(  as.vector(g[g$rank==rmax,"hname"]) )
    }

# check valid rank and return . if not return NA
    if ( ! num > rmax) {
        return (  as.vector(g[g$rank==num,"hname"])  )
    } else {
        return (NA)
    }

}

#
# I/Ps to check data
#best("TX","heart attack")
#best("TX","heart failure")
#best("MD","heart attack")
#best("MD","pneumonia")
#best("TA","heartfailure")
#
