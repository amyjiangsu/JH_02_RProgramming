rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        i <- switch(
                outcome,
                "heart attack" = 11,
                "heart failure" = 17,
                "pneumonia" = 23
        )
        if (is.null(i))
        {
                stop("invalid outcome")
        }
        d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        d <- d[which(d$State==state),]
        d[,11] <- as.numeric(d[,11])
        d[,17] <- as.numeric(d[,17])
        d[,23] <- as.numeric(d[,23])
        d <- d[is.na(d[,i]) == F,]
        if (num == "best")
        {
                num = 1
        }
        if (num == "worst")
        {
                num = nrow(d)
                print(num)
        }
        if (num > nrow(d))
        {
                NA
                return
        }
        states <- unique(d$State)
        if (!(state %in% states))
        {
                stop("invalid state")
        }
        d[order(d[i], d["Hospital.Name"]), 2][num]
}