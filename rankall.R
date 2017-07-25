rankall <- function(outcome, num = "best") {
    setwd("~/ProgAssignment3")
    
    ## Read outcome data
    care.data <-read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    ## Check that outcome is valid
    if (!isTRUE(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    out.name <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    dat <- data.frame(care.data[7],care.data[out.name[[outcome]]], care.data[2])
    names(dat) <- c("state",outcome,"hospital")
    
    state.sort <- sort(unique(dat[["state"]]))
    rank.dat <- data.frame("hospital" = numeric(length(state.sort)),"state" = numeric(length(state.sort)))
    
    for (i in 1:nrow(rank.dat)){
        mystate <- split(dat,dat[["state"]])
        srt <- mystate[[state.sort[i]]]
        y <- srt[order(srt[,2], srt[,3],na.last = NA),"hospital"]

        if (num <= length(y)){
            rank.dat[i,1] <- y[num]
        }
        else if (num == "best"){
            rank.dat[i,1] <- y[1]
        }
        else if (num == "worst"){
            rank.dat[i,1] <- y[length(y)]
        }
        else {
            return(NA)
        }
        rank.dat[i,2] <- state.sort[i]
        row.names(rank.dat)[i] <- state.sort[i]
    }
    rank.dat
}