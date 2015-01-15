rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
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
        d[,11] <- as.numeric(d[,11])
        d[,17] <- as.numeric(d[,17])
        d[,23] <- as.numeric(d[,23])
        d <- d[is.na(d[,i]) == F,]
        each1 <- split(d, d$State)
        #states <- sort(unique(d$State))
        each2 <- lapply(each1, function(x) {
                n = num
                if (num == "best")
                {
                        n = 1
                }
                if (num == "worst")
                {
                        n = nrow(x)
                }
                x[order(x[i], x["Hospital.Name"]), ][n, ]
        })
        each3<-as.data.frame(do.call(rbind, each2))
        each4 <- data.frame(hospital = each3[,2], state = names(each1))#each3[,c(2,7)]
        row.names(each4) <- names(each1)
        #colnames(each4)[1] <- "hospital"
        #colnames(each4)[2] <- "state"
        each4
}