# Validation 
# For brand level
# From MySQL database
# Refer to "APA Markets SOV 2012-2015 09022015 New Way.xlsx"
# validdb()

require(dplyr)
require(RMySQL)
e <- new.env()

#* Connection 1
con1 <- src_mysql("media",
                  host = "*********",
                  port = 3306,
                  user = "MSU_DB_Manager",
                  password = "Embarassing...")
#* Connection 2
con2 <- dbConnect(MySQL(),
                  host = "*********", 
                  port = 3306, 
                  db = "media",
                  user = "MSU_DB_Manager", 
                  password = "Embarassing...")

validdb <- function(){

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
    
    e$cln_dt <- tbl(con1,"clean") %>%
        select(MARKET,YMD,FAMILY,BRAND,MODEL,SPEND,BDR) %>%
        filter(MARKET == e$market)
    
    cln_dt <- e$cln_dt %>%
        select(YMD,BRAND,SPEND,BDR)
    
    test <- cln_dt %>%
        collect %>%
        mutate(NSPEND = sapply(SPEND, function(x){
            nume <- gsub(",","",x)
            return(as.numeric(nume))
        })) %>%
        mutate(YEAR = sapply(cln_dt$YMD, function(x){
            ymd <- strsplit(x,"/")[[1]]
            return(ymd[[1]])
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
        group_by(BRAND,QUARTER) %>%
        summarise(SUMSPEND = sum(NSPEND)) %>%
        spread(QUARTER,SUMSPEND) %>%
        as.data.frame
    
    test[is.na(test)] <- 0
    rownames(test) <- test[,1]
    e$test <- test[,-1]
    colsum <- colSums(e$test)
    for(i in 1:ncol(e$test)){
        for(j in 1:nrow(e$test)){
            if(e$test[j,i] != 0 ){
                e$test[j,i] <- paste(round((as.numeric(e$test[j,i])/as.numeric(colsum[[i]])) *100,1),"%",sep = "")
            }
        }
    }
    filename2 <- paste("ValidationDB_Brandlv_",e$market,".csv",sep = "")
    write.csv(e$test,filename2)
    
    
}