pollutantmean <- function(directory, pollutant, id = 1:332) {

        # initialize filedata as empty list
        filedata  <- NA;

        # looping the files defined in id
        for (n in id) {
                tempstr = paste("000", n, sep="");
                # get file name
                filename = paste(directory,"/",substr(tempstr,nchar(tempstr)-2, nchar(tempstr)),".csv", sep="")
                # open file
                tempfile  <- read.csv(filename);
                # get requested data and append to filedata
                filedata <- rbind(filedata, tempfile)
        }
        
        # calculate the mean
        mean(filedata[[pollutant]], na.rm=T);
}