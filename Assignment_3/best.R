best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        i <- switch(
                outcome,
                "heart attack" = 11,
                "heart failure" = 17,
                "pneumonia" = 23
        )
        print(i)
        if (is.null(i))
        {
                stop("invalid outcome")
        }
        d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        states <- unique(d$State)
        if (!(state %in% states))
        {
                stop("invalid state")
        }
        d[,11] <- as.numeric(d[,11])
        d[,17] <- as.numeric(d[,17])
        d[,23] <- as.numeric(d[,23])
        x <- d[which(d$State==state),]
        sort(x[which.min(x[,i]), ]$Hospital.Name)[1]
}