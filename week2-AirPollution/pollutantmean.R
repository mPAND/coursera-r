#
# Write a function named 'pollutantmean' that calculates the mean 
# of a pollutant (sulfate or nitrate) across a specified list of monitors.
# The function 'pollutantmean' takes three arguments: 
# 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 
# 'pollutantmean' reads that monitors' particulate matter data from 
# the directory specified in the 'directory' argument and returns the
# mean of the pollutant across all of the monitors, ignoring any missing 
# values coded as NA
#
pollutantmean <- function(directory, pollutant, id = 1:332) {

    count = 1
	# Loop through input monitor id
    for ( idx in id) {
     	# get filename prefixed with 0 like 001, 002
		fname <- formatC(idx, width = 3, format = "d", flag = "0")
        # get full path of file alone with directory
		fullname = paste(directory,"/",fname,".CSV",sep="")

		# Keep appending to data.frame 'a'
		if ( count == 1 ) {
            a <- read.csv(fullname)
        } else {
            a <- rbind(a , read.csv(fullname) )
        }
 
        count <- count + 1
  
    }

# Calculate mean and round off to 4 significant digits	
signif ( mean( a[[pollutant]], na.rm=TRUE) , 4)

}
