best <- function(state,outcome) {
 

a <- read.table("outcome-of-care-measures.csv" , header = TRUE, sep =",", na.strings ="Not Available" )
b <- a[,c(2,7,11,17,23)]
names(b) <-c("hname","state","heart attack","heart failure","pneumonia")

state_check <- unique(b$state)

if( ! any(state_check==state) )
{
 stop("invalid state")
}

outcome_check <- c("heart attack","heart failure","pneumonia")

if ( ! any (outcome_check == outcome) ) 
{
stop("invalid outcome")
}

c <- split(b, b$state)
d <- as.data.frame( c[state] )
names(d) <-c("hname","state","heart attack","heart failure","pneumonia")
e <- d[,c("hname","state",outcome)]
f <- e[order(e[outcome],e["hname"]),]
g<-f[complete.cases(f),]
g$rank <- seq_len(nrow(g))
return(  as.vector(g[g$rank==1,"hname"]) )

}



#best("TX","heart attack")
#best("TX","heart failure")
#best("MD","heart attack")
#best("MD","pneumonia")
#best("TA","heartfailure")

