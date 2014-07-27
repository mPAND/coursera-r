#
# Write a function called rankall that takes two arguments: an outcome name
# (outcome) and a hospital rank- ing (num). The function reads the 
# outcome-of-care-measures.csv file and returns a 2-column data frame containing 
# the hospital in each state that has the ranking specified in num. For example 
# the function call rankall("heart attack", "best") would return a data frame 
# containing the names of the hospitals that are the best in their respective 
# states for 30-day heart attack death rates. The function should return a value 
# for every state (some may be NA). The first column in the data frame is named 
# hospital, which contains the hospital name, and the second column is named state
# , which contains the 2-character abbreviation for the state name. Hospitals that
# do not have data on a particular outcome should be excluded from the set of 
# hospitals when deciding the rankings.
#

rankall <- function(outcome,num="best") {

# read data and massage it
    a <- read.table("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv" , header = TRUE, sep =",", na.strings ="Not Available" )
    b <- a[,c(2,7,11,17,23)]
    names(b) <-c("hname","state","heart attack","heart failure","pneumonia")

# valid state check
    state_check <- sort(unique(b$state))
    outcome_check <- c("heart attack","heart failure","pneumonia")

    if ( ! any (outcome_check == outcome) ) {
        stop("invalid outcome")
    }

# get necessary outcome data
    c <- b[,c("hname","state",outcome)]
    d <- complete.cases(c)
    e <- c[d,]

# convert to list of df based on state
    f <- split(e, e$state)

# calculate rank based on outcome and hname
    ddd <- vector()
    for ( i in names(f)) {
        g <- (f[[i]])
        h <- g[order(g[outcome],g["hname"]),]
        h$rank <- seq_len(nrow(h))
        ddd <- rbind(ddd, h)
    }

# create a new vector based on num
# if num - best get first rank of each state
# if num - worst, get last rank of each state
# if num is integer get entry for that rank else NA

    dd2 <- vector()
    for ( st in state_check) {

        if ( num =="best" ) {
            dd2 <- rbind( dd2,ddd[ddd$rank == 1 & ddd$state==st, c("hname","state") ] )
        } else if ( num == "worst" ) {
            rmax <- max(ddd[ddd$state==st,c("rank")])
            dd2 <- rbind(dd2, ddd[ddd$rank == rmax & ddd$state==st,c("hname","state") ]  )
        } else {
            if ( nrow(ddd[ddd$rank == num & ddd$state==st,]) == 0 ) { 
                dd2 <- rbind(dd2,data.frame(hname=NA,state=st) )
            } else  {
                dd2 <- rbind(dd2,ddd[ddd$rank == num & ddd$state==st,c("hname","state") ] )
            }
        }
    }

# format df and return    
    names(dd2) <- c("hospital","state")
    rownames(dd2) <- dd2$state
    return (dd2)
}
