rankall <- function(outcome,num="best") {


a <- read.table("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv" , header = TRUE, sep =",", na.strings ="Not Available" )
b <- a[,c(2,7,11,17,23)]
names(b) <-c("hname","state","heart attack","heart failure","pneumonia")

state_check <- sort(unique(b$state))

outcome_check <- c("heart attack","heart failure","pneumonia")

if ( ! any (outcome_check == outcome) ) 
{
  stop("invalid outcome")
}

c <- b[,c("hname","state",outcome)]
d <- complete.cases(c)
e <- c[d,]

f <- split(e, e$state)

ddd <- vector()
for ( i in names(f)) 
{
 g <- (f[[i]])
 h <- g[order(g[outcome],g["hname"]),]
 h$rank <- seq_len(nrow(h))
 ddd <- rbind(ddd, h)
}

dd2 <- vector()
for ( st in state_check) 
{

 if ( num =="best" ) {
  dd2 <- rbind( dd2,ddd[ddd$rank == 1 & ddd$state==st, c("hname","state") ] )
  } else if ( num == "worst" ) {
  rmax <- max(ddd[ddd$state==st,c("rank")])
  dd2 <- rbind(dd2, ddd[ddd$rank == rmax & ddd$state==st,c("hname","state") ]  )
  } else {
   if ( nrow(ddd[ddd$rank == num & ddd$state==st,]) == 0 )
   { 
    dd2 <- rbind(dd2,data.frame(hname=NA,state=st) )
   } else  {
   dd2 <- rbind(dd2,ddd[ddd$rank == num & ddd$state==st,c("hname","state") ] )
   }
 }

}
names(dd2) <- c("hospital","state")
rownames(dd2) <- dd2$state
return (dd2)
}
