# Validation
# For Segment
# valids()

valids() <- function(){
    all_market <- c("AUSTRALIA","INDIA","INDONESIA","SOUTH KOREA", "TAIWAN",
                    "NEW ZEALAND","VIETNAM","CHINA","THAILAND","PHILIPPINES")
    
    repeat{
        message("\n Please Choose Market\n")
        cat(" [1].AUSTRALIA\n","[2].INDIA\n","[3].INDONESIA\n","[4].SOUTH KOREA\n",
            "[5].TAIWAN\n","[6].NEW ZEALAND\n","[7].VIETNAM\n","[8].CHINA\n",
            "[9].THAILAND\n","[10].PHILIPPINES\n")
        market <- readline("Choose Market: ")
        if(market %in% all_market){
            e$market <- market
            cat("\n The market you choosed is:",e$market,"\n\n")
            cat()
        }else if(as.numeric(market) %in% 1:10){
            e$market <- all_market[[as.numeric(market)]]
            cat("\n The market you choosed is:",e$market,"\n\n")
        }else {
            message("\n Wrong Input!!!")
            next
        }
        yon <- readline(" Continue?(Y/N): ")
        if(toupper(yon) == "Y" | yon == ""){
            break
        }else{
            next
        }
    }
    
    filename <- paste("Clean_",e$market,".csv",sep = "")   
    cln_dt <- read.csv(filename,stringsAsFactors = F)
    
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
    
    
    segment <- c("B","C","CD","MINI UTILITY","SMALL UTILITY","MEDIUM CROSSOVER UTILITY")
    for(i in 1:length(segment)){
        cat(" [",i,"]",segment[[i]],"\n")
    }
    chooseseg <- readline(" Choose a segment: ")
    filtername <- segment[[as.numeric(chooseseg)]]
    test2 <- test1 %>%
        filter(REGIONAL.SEG == filtername) %>%
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
    
    filname2 <- paste("Validation_",filtername,"_",e$market".csv",sep = "") 
    write.csv(e$test2,filename2)
        
}
