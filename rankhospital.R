rankhospital <- function(state, outcome, num = "best") {
    setwd("~/ProgAssignment3")
    
    ## Read outcome data
    care.data <-read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    ## Check that state and outcome are valid
    if(!isTRUE(state %in% care.data$State)){
        stop("invalid state")
    }
    else if (!isTRUE(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    out.name <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    dat <- data.frame(care.data[7],care.data[out.name[[outcome]]], care.data[2])
    names(dat) <- c("state name",outcome,"hospital name")
    
    mystate <- split(dat,dat[["state name"]])
    srt <- mystate[[state]]
    y <- srt[order(srt[,2], srt[,3],na.last = NA),"hospital name"]
    

    if (num <= nrow(srt)){
        y[num]
    }
    else if (num=="best"){
        y[1]
    }
    else if(num == "worst"){
        y[length(y)]
    }
    else {
        return(NA)
    }
}