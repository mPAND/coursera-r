#
# Write a function that reads a directory full of files and reports 
# the number of completely observed cases in each data file. The function 
# should return a data frame where the first column is the name of the 
# file and the second column is the number of complete cases
#
complete <- function(directory, id = 1:332) {

	# initialise empty vectors
	ids <-vector()
	counts <- vector()
	
	# Loop through input monitor id
    for ( idx in id) {

    	# Form the 0 Prefixed file name
		fname <- formatC(idx, width = 3, format = "d", flag = "0")

        # Form the full name along with path info
		fullname = paste(directory,"/",fname,".CSV",sep="")

        # Read file and remove na's
		b<-read.csv(fullname)
    	cc <- complete.cases(b)

        # Append monitor id to master vector
    	ids <- c(ids,idx)
		# Append counts to master vector
	    counts <- c(counts,sum(cc, na.rm=TRUE))

    }

	# Return the data.frame
    return( data.frame(id=ids,nobs=counts) )
}
