#
# Write a function that takes a directory of data files and a threshold 
# for complete cases and calculates the correlation between sulfate and 
# nitrate for monitor locations where the number of completely observed 
# cases (on all variables) is greater than the threshold. The function 
# should return a vector of correlations for the monitors that meet the 
# threshold requirement. If no monitors meet the threshold requirement, 
# then the function should return a numeric vector of length 0
#
corr <- function(directory, threshold=0) {

    # Initialise empty vectors
	ids <-vector()
	counts <- vector()
	sul <- vector()
	nit <- vector()

	# Initialise empty master vector
	retdata <-as.numeric(vector())

    # Look through all monitors	
    for ( idx in 1:332) {
       	# Form the 0 Prefixed file name
        fname <- formatC(idx, width = 3, format = "d", flag = "0")
        
		# Form the full name along with path info
        fullname = paste(directory,"/",fname,".CSV",sep="")

        # Read the data
		b<-read.csv(fullname)
		# remove na
    	cc <- complete.cases(b)
		b <- b[cc,]

        # Threshold Check for each monitor
        if (length(b$sulfate) <= threshold ) {
		next
		} else {
		     # Append to master vector the correlation of 2 particles
		     retdata <-c(retdata,cor(b$sulfate,b$nitrate))
		}

    }
	
	# Return the master data
	return(retdata)
}
