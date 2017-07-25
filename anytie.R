anytie <- function (state, outcome){
    setwd("~/ProgAssignment3")
    
    care.data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    if(!isTRUE(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
        stop("invalid outcome")
    }
    else if (!isTRUE(state %in% care.data$State)){
        stop("invalid state")
    }
    
    out.name <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    dat <- data.frame(care.data[7], care.data[out.name[outcome]], care.data[2])
    names(dat) <- c("state",outcome,"hospital")
    mystate <- split(dat,dat[["state"]])
    srt <- mystate[[state]]
    mytie <- srt[order(srt[,2], srt[,3],na.last = NA),]
    
    for (i in 1:length(mytie[[outcome]])){
        tie.1 <- mytie[[outcome]][i]
        tie.2 <- mytie[[outcome]][i+1]
        if(tie.1 == tie.2){
            print(paste("there is a tie and the value is",tie.1,sep=" "))
            next
        }
        else if (is.na(tie.2)){
            break
        }
    }
}