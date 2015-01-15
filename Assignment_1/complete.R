complete <- function(directory, id = 1:332) {
        # load the readPollutantCsv.R file
        source("readPollutantCsv.R");
        
        # initiate nob
        nobs = numeric(0);
        
        # open each file in id and only add the complete case to nob
        for (i in id) {
                tempfile <- readPollutantCsv(directory,i);
                nobs = c(nobs, sum(!is.na(tempfile$sulfate) & !is.na(tempfile$nitrate)))
        }
        
        # return data frame
        data.frame(id=id, nobs = nobs);
}
