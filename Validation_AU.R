# For Australia
# Refer to "APA Markets SOV 2012-2015 09022015 New Way.xlsx"
# valid_au()

valid_au <- function(){
    cln_dt <- read.csv("Clean_AUSTRALIA.csv",stringsAsFactors = F)
    
    test1 <- cln_dt %>%
        select(YMD,BRAND,MODEL,REGIONAL.SEG,LOCAL.SEG,SPEND) %>%
        mutate(NSPEND = sapply(SPEND, function(x){
            nume <- gsub(",","",x)
            return(as.numeric(nume))
        })) %>%
        mutate(QUARTER = sapply(cln_dt$YMD, function(x){
            ymd <- strsplit(x,"/")[[1]]
            if(ymd[[2]] %in% c(1,2,3))
                return(paste(ymd[[1]],"Q1"))
            else if(ymd[[2]] %in% c(4,5,6))
                return(paste(ymd[[1]],"Q2"))
            else if(ymd[[2]] %in% c(7,8,9))
                return(paste(ymd[[1]],"Q3"))
            else if(ymd[[2]] %in% c(10,11,12))
                return(paste(ymd[[1]],"Q4"))
        })) %>%
        mutate(BM = paste(cln_dt$BRAND,cln_dt$MODEL,sep = " "))
    
    segment <- c("B","C","CD","MINI UTILITY","SMALL UTILITY","MEDIUM CROSSOVER UTILITY",
                 "LIGHT PASSENGER CAR","SMALL CAR")
    for(i in 1:length(segment)){
        cat(" [",i,"]",segment[[i]],"\n")
    }
    chooseseg1 <- readline(" Choose a regional segment: ")
    chooseseg2 <- readline(" Choose a local segment: ")
    filtername1 <- segment[[as.numeric(chooseseg1)]]
    filtername2 <- segment[[as.numeric(chooseseg2)]]
    
    test2 <- test1 %>%
        filter(REGIONAL.SEG == filtername1 & LOCAL.SEG == filtername2) %>%
        group_by(BM,QUARTER) %>%
        summarise(SUMSPEND = sum(NSPEND)) %>%
        spread(QUARTER,SUMSPEND) %>%
        as.data.frame
    
    test2[is.na(test2)] <- 0
    rownames(test2) <- test2[,1]
    e$test2 <- test2[,-1]
    colsum <- colSums(e$test2)
    
    for(i in 1:ncol(e$test2)){
        for(j in 1:nrow(e$test2)){
            if(e$test2[j,i] != 0 ){
                e$test2[j,i] <- paste(round((as.numeric(e$test2[j,i])/as.numeric(colsum[[i]])) *100,1),"%",sep = "")
            }
        }
    }
    
    write.csv(e$test2,paste("Validation_AU_",filtername1,"_",filtername2,".csv",sep = ""))
    
}
