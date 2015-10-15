##================##
# MEDIA DATA CLEAN #
##================##

# market: "AUSTRALIA","INDIA","INDONESIA","SOUTH KOREA", "TAIWAN","NEW ZEALAND","VIETNAM","CHINA","THAILAND","PHILIPPINES"
# rawfile: depends on the market
# subfile: "SUB.csv"
##       * column1(such as CHINA1): the wrong nameplates to be replaced in the raw data
##       * column2(such as CHINA2): the replacements of the wrong nameplates 
# priorfile: "PRIORITY.csv"
##       * column1(such as CHINA1): the multiple brands 
##       * column2(such as CHINA2): the prior brand name
##       * column3(such as CHINA3): the multiple models
##       * column4(such as CHINA4): the prior model name
# basefile: "ALL_NAMEPLATES.csv"




require(dplyr)
require(tidyr)
require(RMySQL)
e <- new.env()

data_cln <- function(rawfile,
                     subfile = "SUB.csv",
                     priorfile = "PRIORITY.csv",
                     basefile = "ALL_NAMEPLATES.csv"){
    
    # Start:
    step_control <- function(){
        tm1 <- proc.time()
        #1.
        e$check_file()
        e$check_market()
        e$check_column()
        #2.
        e$input_data()
        cat("\n----------------------------------------------------- 10%")
        #3.
        e$sub()
        cat("\n----------------------------------------------------- 15%")
        #4.
        e$find_brand(1)
        cat("\n----------------------------------------------------- 20%")
        e$clean_brand()
        cat("\n----------------------------------------------------- 30%")
        e$find_model(1)
        cat("\n----------------------------------------------------- 40%")
        #5.
        e$other_markets()
        cat("\n----------------------------------------------------- 50%")
        #6.
        e$find_family()
        cat("\n----------------------------------------------------- 60%")
        e$find_segment()
        cat("\n----------------------------------------------------- 65%")
        e$find_bdr()
        cat("\n----------------------------------------------------- 70%")
        e$find_channel()
        cat("\n----------------------------------------------------- 75%")
        e$find_category()
        cat("\n----------------------------------------------------- 80%")
        e$final_clean()
        cat("\n----------------------------------------------------- 90%")
        #7.
        e$checkup()
        cat("\n----------------------------------------------------- 100%")
        cat("\n\n Data clean Complete!...\n\n\n\n")
        tm2 <- proc.time()
        if((tm2-tm1)[[3]] > 60){
            cat("\n Time used [",round((tm2 - tm1)[[3]]/60,1),"] Minutes\n")
        }else
            cat("\n Time used [",round((tm2 - tm1)[[3]],1),"] Seconds\n")
    }
    
    
    ##-----------------
    # (0). Prepare 
    ##-----------------
    
    #~1. Choose market
    # Check if the files are exist in the working directory
    e$check_file <- function(){
        if(file.exists(rawfile) == F){
            stop("\n Raw file doesn't exist in the working directory!")
        }
        if(file.exists(subfile) == F){
            stop("\n Subfile doesn't exist in the working directory!")
        }
        if(file.exists(priorfile) == F){
            stop("\n Priorfile doesn't exist in the working directory!")
        }
        if(file.exists(basefile) == F){
            stop("\n Basefile doesn't exist in the working directory!")
        }
    }
    # Choose a market
    e$check_market <- function(){
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
    }    
        
    #~2. All_column 
    e$all_column <- function(){
        market_name <- gsub(" ","_",e$market)
        e$col_1 <- paste(market_name,"1",sep = "")
        e$col_2 <- paste(market_name,"2",sep = "")
        e$col_3 <- paste(market_name,"3",sep = "")
        e$col_4 <- paste(market_name,"4",sep = "")
        
        if(toupper(e$market) == "AUSTRALIA"){
            e$col_ymd <- 1; e$col_b <- 3; e$col_p <- 4; e$col_chnl <- 5;e$col_spd <- 15;e$col_cat <- NA
        }
        else if(toupper(e$market) == "INDONESIA"){
            e$col_ymd <- 8; e$col_b <- 4; e$col_p <- 5; e$col_chnl <-11;e$col_spd <- 12; e$col_cat <- 3
        }
        else if(toupper(e$market) == "CHINA"){
            e$col_ymd <- 6; e$col_b <- 1; e$col_p <- 2; e$col_chnl <- 7; e$col_spd <- 11;e$col_cat <- 3
        }
        else if(toupper(e$market) == "INDIA"){
            e$col_ymd = 5; e$col_b = 1; e$col_p = 2; e$col_chnl <- NA; e$col_cat <- NA; e$col_spd = 7
        }
        else if(toupper(e$market) == "SOUTH KOREA"){
            e$col_ymd = 13; e$col_b = 1; e$col_p = 3; e$col_chnl = 10; e$col_spd = 14; e$col_cat = NA
        } 
        else if(toupper(e$market) == "TAIWAN"){
            e$col_ymd = 4; e$col_b = 1; e$col_p = 2; e$col_chnl = 3; e$col_spd = 5; e$col_cat = NA
        } 
        else if(toupper(e$market) == "NEW ZEALAND"){
            e$col_ymd = 10; e$col_b = 1; e$col_p = 2; e$col_chnl = 9; e$col_spd = 14; e$col_cat = 3
        } 
        else if(toupper(e$market) == "VIETNAM"){
            e$col_ymd = 6; e$col_b = 1; e$col_p = 2; e$col_chnl = 5; e$col_spd = 7; e$col_cat = NA
        }
        else if(toupper(e$market) == "THAILAND"){
            e$col_ymd = 9; e$col_b = 3; e$col_p = 4; e$col_chnl = 7; e$col_spd = 12; e$col_cat = 1
        }
        else if(toupper(e$market) == "PHILIPPINES"){
            e$col_ymd = 14; e$col_b = 5; e$col_p = 6; e$col_chnl = 9; e$col_spd = 12; e$col_cat = 2
        }
    }    
    #~3. Check column
    e$check_column <- function(){
        cat("\n The default column number in [",e$market,"] is: \n","\n", "col_b(Brand)=",e$col_b, "\n","col_p(Product)=",e$col_p,"\n", 
            "col_spd(Spend,adjusted K USD)=",e$col_spd,"\n","col_chnl(Channel/Media)=",e$col_chnl,"\n","col_ymd(YearMonthDay)=",e$col_ymd,
            "\n","col_cat(Category)=",e$col_cat,"\n")
        cat("\n")
        yon <- readline("Do you want to change the column number? (Y/N): ")
        if(toupper(yon) == "Y"){
            repeat{
                message("\n Please enter the Right Column Number Below: \n")
                e$col_b <- as.numeric(readline("col_b(Brand)= "))
                e$col_p <- as.numeric(readline("col_p(Product)= "))
                e$col_spd <- as.numeric(readline("col_spd(Spend)= "))
                e$col_chnl <- as.numeric(readline("col_chnl(Channel)= "))
                e$col_ymd <- as.numeric(readline("col_ymd(YearMonthDay)= "))
                e$col_cat <- as.numeric(readline("col_cat(Category)= "))
                cat("\n The column number right now is: \n","\n", "col_b=",e$col_b, "\n","col_p=",e$col_p,"\n", 
                    "col_spd=",e$col_spd,"\n","col_chnl=",e$col_chnl,"\n","col_ymd=",e$col_ymd,"\n","col_cat=",e$col_cat,"\n") 
                yon <- readline(" Enter N if you want to Input again: ")
                if(yon != "N"){
                    break
                }else{
                    next
                }
            }
        }
    }
    
    #~4. Rawdata input

    e$input_data <- function(){
        
        #~ All_base
        message("\n Reading basefile...")
        e$all_base <- read.csv(basefile, na.strings = "", stringsAsFactors = FALSE)
        e$all_base <- data.frame(sapply(e$all_base, toupper), stringsAsFactors = FALSE)
        #~ Basepool
        e$basepool1 <- filter(e$all_base,MARKET == e$market)
        e$basepool2 <- filter(e$all_base,MARKET != e$market)
        
        #~ Raw data
        #~ Different in Indonesia
        if(toupper(e$market) == "INDONESIA"){
            message("\n Reading rawfile...")
            indonesia <- read.csv(rawfile,stringsAsFactors = FALSE)
            indonesia_gather <- gather(indonesia,MEDIA,SPEND,-(Heading:MONTH),-(PRINT:TOTAL))
            e$rawdata <- indonesia_gather[-which(indonesia_gather$SPEND == 0),]
            #~ BRAND&PRODUCT
            e$BP <- toupper(paste(e$rawdata[[e$col_b]], e$rawdata[[e$col_p]], sep = " ")) 
        }else{
            message("\n Reading rawfile...")
            e$rawdata <- read.csv(rawfile, stringsAsFactors = FALSE)
            
            #~ BRAND&PRODUCT
            e$BP <- toupper(paste(e$rawdata[[e$col_b]], e$rawdata[[e$col_p]], sep = " ")) 
            
        }
        
        #* Those Bicycle/ airplane/ tractor etc. Will be cleaned to Others
        message("\n Finding Those [BIKE],[AIRPLANE],[TRACTOR]... to be cleaned to OTHER...")
        e$ind_others <- which(grepl(turn_exp1("AIRBUS"),e$BP) == T   
                              | grepl(turn_exp1("BIKE"),e$BP) == T 
                              | grepl(turn_exp1("BICYCLE"),e$BP) == T
                              | grepl(turn_exp1("AIRPLANE"),e$BP) == T
                              | grepl(turn_exp1("PLANE"),e$BP) == T
                              | grepl(turn_exp1("AMBULANCE"),e$BP) == T
                              | grepl(turn_exp1("TRACTOR"),e$BP) == T
                              | grepl(turn_exp1("MOTOCYCLE"),e$BP) == T
                              | grepl(turn_exp1("MOTORCYCLE"),e$BP) == T
                              | grepl(turn_exp1("MOTOBICYCLE"),e$BP) == T
                              | grepl(turn_exp1("AGRICULTURE"),e$BP) == T)
        
        if(length(e$ind_others) > 0){
            e$BP <- e$BP[-e$ind_others]
            #~ e$cln_data
            e$cln_data <- data.frame(INDEX = 1:length(e$BP), BRD_NUM = NA, MDL_NUM = NA, 
                                     FAMILY = NA, BRAND = NA, MODEL = NA, 
                                     BRAND_ORI  = e$rawdata[-e$ind_others,e$col_b],
                                     PRODUCT_ORI = e$rawdata[-e$ind_others,e$col_p],
                                     REGIONAL.SEG = NA, LOCAL.SEG = NA,
                                     CHANNEL = NA, 
                                     CHANNEL_ORI = ifelse(e$market == "INDIA","TV+PRINT+RADIO",e$rawdata[-e$ind_others,e$col_chnl]),
                                     RGNL_MTRPLTN = NA, 
                                     CATEGORY = NA,BDR = NA)
            
            e$cln_data_others <- data.frame(INDEX = 1:length(e$ind_others), BRD_NUM = NA, MDL_NUM = NA, 
                                            FAMILY = NA,  BRAND = NA, MODEL = NA, 
                                            BRAND_ORI  = e$rawdata[e$ind_others,e$col_b],
                                            PRODUCT_ORI = e$rawdata[e$ind_others,e$col_p],
                                            REGIONAL.SEG = NA, LOCAL.SEG = NA,
                                            CHANNEL = NA, 
                                            CHANNEL_ORI = ifelse(e$market == "INDIA","TV+PRINT+RADIO",e$rawdata[e$ind_others,e$col_chnl]),
                                            RGNL_MTRPLTN = NA, 
                                            CATEGORY = NA,BDR = NA)
            
        }else{
            e$cln_data <- data.frame(INDEX = 1:length(e$BP), BRD_NUM = NA, MDL_NUM = NA, 
                                     FAMILY = NA, BRAND = NA, MODEL = NA, 
                                     BRAND_ORI  = e$rawdata[,e$col_b],
                                     PRODUCT_ORI = e$rawdata[,e$col_p],
                                     REGIONAL.SEG = NA, LOCAL.SEG = NA,
                                     CHANNEL = NA, 
                                     CHANNEL_ORI = ifelse(e$market == "INDIA","TV+PRINT_RADIO",e$rawdata[,e$col_chnl]) ,
                                     RGNL_MTRPLTN = NA, 
                                     CATEGORY = NA,BDR = NA)
        }
        
    }
    
    #~5. Turn into regular expression
    turn_exp1 <- function(x){
        paste("^", x, "| ", x, "-| ", x, " | ", x, "$|-", x, "$|-", x, " | ", x, "/|/", x, " ", sep = "")
    }  
    turn_exp2 <- function(x){
        paste(turn_exp1(x), collapse = "|")
    }
    turn_exp3 <- function(x){
        gsub(" ", "-", x)
    }
    turn_exp4 <- function(x){
        paste(turn_exp1(x), turn_exp1(turn_exp3(x)), sep = "|")
    }
    turn_exp5 <- function(x){
        gsub(" ", "", x)
    }
    turn_exp6 <- function(x){
        paste(turn_exp1(x),turn_exp1(turn_exp5(x)),turn_exp1(turn_exp3(x)),turn_exp1(gsub("-","",x)),sep = "|")
    }
    
    #~6. Those Incorrect /Chinese characters will be subtituted to the correct one
    
    e$sub <- function(){
        tm1 <- proc.time()
        #~ First substitute those Incorrect characters or chinese characters
        sublist <- read.csv(subfile,stringsAsFactors = F,na.strings = "")
        sub_num <- length(sublist[e$col_1][!is.na(sublist[e$col_1]),])
        if(sub_num > 0){
            message("\n Substitute Incorrect nameplates...")
            if(sub_num > 500){
                cat("\n Congratulations! China!\n")
            }
            progressbar <- winProgressBar(title = "...", min = 0,
                                          max = sub_num, width = 433)
            i <- 1
            while(i <= length(sublist[[e$col_1]][which(!is.na(sublist[[e$col_1]]))]))  {
                e$BP <- gsub(sublist[[e$col_1]][[i]],paste(" ",sublist[[e$col_2]][[i]]," "),e$BP)
                setWinProgressBar(progressbar, i, title=paste( "...",round(i/sub_num*100, 0),"% Done"))
                i = i+1
            }
            
            close(progressbar)
        }
        
        if(e$market == "CHINA"){
            #~ When it is in Tianchao...
            #* Fullwidth-form blank may be readable only in UTF-8 ...
            yon <- readline(' 全角空格(Just press Enter)...')
            repeat{
                e$BP <- gsub("　"," ",e$BP)
                if((T %in% grepl("　",e$BP)) == F)
                    break
            }   
        }
        #~ Those brackets...
        message("\n Removing Brackets...")
        e$BP <- gsub("\\("," ",e$BP)
        
        #~ Those 2 blanks or more clean to one...
        message("\n Removing spare blanks...")
        repeat{
            e$BP <- gsub("  "," ",e$BP)
            if((T %in% grepl("  ",e$BP)) == F)
                break
        }   
        
        #~ Another Special Chery RUIQI, Should be RIICH ! For the sake of Nissan RUIQI won't be frustrated
        message("\n Chery RUIQI to RIICH...")
        ind_RUIQI <- which(grepl("CHERY",e$BP) == T & grepl("RUIQI",e$BP) == T)
        if(length(ind_RUIQI) > 0 ){
            cat("\n [",length(ind_RUIQI),"] Rows of chery RUIQI cleaned to RIICH!")
            e$BP[ind_RUIQI] <- gsub("RUIQI","RIICH",e$BP[ind_RUIQI])
        }
        tm2 <- proc.time()
        if((tm2-tm1)[[3]] > 60){
            cat("\n Time used [",round((tm2 - tm1)[[3]]/60,1),"] Minutes\n")
        }else
            cat("\n Time used [",round((tm2 - tm1)[[3]],1),"] Seconds\n")
    }
    
    ##----------------------------------
    # (1). Search in specific market
    ##----------------------------------
    #~ 1. Find brand
    e$find_brand <- function(x){
        tm1 <- proc.time()
        if(x == 1){ # Define search in specific market or search in other market
            e$brand <- unique(e$basepool1$BRAND[which(!is.na(e$basepool1$BRAND))])
        }else {
            e$brand <- unique(e$basepool2$BRAND[which(!is.na(e$basepool2$BRAND))])
        }
        message("\n Finding brands...")
        progressbar <- winProgressBar(title = "...", min = 0,max = length(e$brand), width = 433)
        i <- 1
        index_nabrand <- which(is.na(e$cln_data$BRAND))
        while(i <= length(e$brand)){
            index <- e$cln_data$INDEX[index_nabrand][which(grepl(turn_exp6(e$brand[[i]]),e$BP[index_nabrand]) == T)]
            if(length(index) > 0){
                j <- 1 
                while(j <= length(index)){
                    if(is.na(e$cln_data$BRAND[index[[j]]])){
                        e$cln_data$BRAND[index[[j]]] <- e$brand[[i]]
                        e$cln_data$BRD_NUM[index[[j]]] <- 1
                    }
                    else{
                        brand_split <- strsplit(e$cln_data$BRAND[index[[j]]],";")[[1]]
                        e$cln_data$BRAND[index[[j]]] <- 
                            paste(sort(c(brand_split,e$brand[[i]])),collapse = ";")
                        e$cln_data$BRD_NUM[index[[j]]] <- length(c(brand_split,e$brand[[i]]))
                        
                    }
                    j = j+1
                }            
                
            }
            setWinProgressBar(progressbar, i, title=paste( "...",round(i/length(e$brand)*100, 0),"% Done"))
            i = i+1    
        }
        e$cln_data$BRD_NUM[which(is.na(e$cln_data$BRAND))] <- 0
        close(progressbar)
        tm2 <- proc.time()
        cat("\n [",length(e$cln_data$BRAND[which(is.na(e$cln_data$BRAND))]),"]  ROWS NOT FOUND!!\n\n")
        if((tm2-tm1)[[3]] > 60){
            cat("\n Time used [",round((tm2 - tm1)[[3]]/60,1),"] Minutes\n")
        }else
            cat("\n Time used [",round((tm2 - tm1)[[3]],1),"] Seconds\n")
        
        if(e$market == "SOUTH KOREA"){
            #** Look for the GM BRANDS which model is sure
            ind_gm <- which(e$rawdata[[e$col_b]] == "GM Korea" & e$rawdata[[e$col_b]] != e$rawdata[[e$col_p]])
            gmmdl <- c("GENTRA","LACETTI","MATIZ","REZZO","TOSCA","VERITAS","WINSTORM","ALPHEON","AVEO",
                       "CAMARO","CAPTIVA","CRUZE","ORLANDO","SPARK","CORVETTE","TRAX","MALIBU")
            test_gm <- gmmdl 	%>%
                turn_exp6               %>%
                sapply(grepl,e$BP)   
            colnames(test_gm) <- gmmdl
            e$cln_data$MODEL[ind_gm] <- apply(test_gm[ind_gm, ], 1, 
                                              (function(x){paste(names(which(x == TRUE)), collapse = "; ")}))
            
            e$cln_data$BRAND[ind_gm] <- "CHEVROLET"
        }
    }
    
    #~ 2. Clean brand1
    #* Clean those muti brands based on the prior column (Product column in China)
    #* Select brand priority one by one
    e$select_prior1 <- function(){
        index <- grep(";",e$cln_data$BRAND) # Only those two or more brands found will contain ";"
        brandd <- unique(e$cln_data$BRAND[index])
        comp_tbl <- data.frame(Multi.Brand = e$cln_data$BRAND[index],
                               Original.Brand = e$cln_data$BRAND_ORI[index],
                               Original.Product = e$cln_data$PRODUCT_ORI[index],
                               stringsAsFactors = F)
        comp_tbl <- data.frame(Index = 1:length(brandd),
                               comp_tbl[!duplicated(comp_tbl$Multi.Brand),],
                               stringsAsFactors = F)
        ind_exist <- which(brandd %in% e$brd_prior[[e$col_1]])
        if(length(brandd) == length(ind_exist)){ 
            message("\n Priority all selected!")
            cat("\n Check the priority in 'PRIORITY.csv' if you want or start clean brand...")
        }
        else if(length(brandd) > length(ind_exist)){
            cat("\n There are [",length(brandd)-length(ind_exist),"] entries haven't been given Priority ")
            message("\n Select the priority now!\n\n")
            if(length(ind_exist) > 0){
                brandd <- brandd[-ind_exist]
                comp_tbl <- comp_tbl[-ind_exist,]
            }
            
            message("\n You can choose a prior brand if you want, or Press Enter to skip!")
            message(" To make sure you choose the right priority,It's supposed to compare the Original Brands&Products...\n")
            message("\n Enter the brand Name or Number to select, q to quit!")
            
            i <- 1
            while(i <= length(brandd)){
                cat("\n-------------------------------")
                message(" Choose a brand below: ")
                branddi <- as.character(strsplit(brandd[[i]],";")[[1]])
                b <- 1
                while(b <= length(branddi)){
                    cat("[",b,"] ",branddi[[b]],"\n")
                    b = b+1
                }
                message(" Compare to the original: ")
                cat("[ Original Brand :] ",as.character(comp_tbl$Original.Brand[[i]]))
                cat("\n")
                cat("[Original Product:] ",as.character(comp_tbl$Original.Product[[i]]))
                cat("\n-------------------------------------------")
                cat("\n\n")
                
                brand_prior <- readline(" Choose a brand: ")
                
                if(brand_prior == ""){
                    j <- length(e$brd_prior[[e$col_1]][which(!is.na(e$brd_prior[[e$col_1]]))])+1
                    e$brd_prior[[e$col_1]][[j]] <- brandd[[i]]
                    e$brd_prior[[e$col_2]][[j]] <- NA
                    message("\n","[",brandd[[i]],"]"," Will Cleaned to NA!")
                    message("\n","[",length(brandd)-i,"]"," Lines left!")
                }
                
                else if(toupper(brand_prior) %in% branddi){
                    j <- length(e$brd_prior[[e$col_1]][which(!is.na(e$brd_prior[[e$col_1]]))])+1
                    e$brd_prior[[e$col_1]][[j]] <- brandd[[i]]
                    e$brd_prior[[e$col_2]][[j]] <- toupper(brand_prior)
                    message("\n","[",toupper(brand_prior),"]"," has added to the list...")
                    message("\n","[",length(brandd)-i,"]","  Lines left!")
                    
                }
                else if(brand_prior == "q"){
                    break
                }
                else if(as.numeric(brand_prior) %in% 1:length(branddi)){
                    k <- as.numeric(brand_prior)
                    j <- length(e$brd_prior[[e$col_1]][which(!is.na(e$brd_prior[[e$col_1]]))])+1
                    e$brd_prior[[e$col_1]][[j]] <- brandd[[i]]
                    e$brd_prior[[e$col_2]][[j]] <- branddi[[k]]
                    message("\n","[",branddi[[k]],"]"," has added to the list...")
                    message("\n","[",length(brandd)-i,"]"," Lines left!")
                }
                else{
                    message("\n Wrong Input!")
                    message("\n Please choose again... \n")
                    next
                }
                i <- i+1
                
            }
            write.csv(e$brd_prior,"BRAND_PRIOR.csv",row.names = F,na = "")
            message("\n\n The list has saved to the 'BRAND_PRIOR.csv' file...")
            message("\n Copy the list in the 'BRAND_PRIOR.csv'to the Right column of the priorfile('PRIORITY.csv') and save the file,then you can start clean brands")
            yon <- readline(" Are you sure to clean the brands based on the priority you choosed?(Y/N): ")
            if(toupper(yon) == "Y" | yon == ""){
                return(e$clean_brand())
            }else{
                message("\n You'd better choose again!")
                return(e$select_prior1())
            }
        }
    }
    
    #* Clean those mutiple brands based on the priority you selected
    e$clean_brand <- function(){
        message("\n Cleaning brands...")
        e$brd_prior <- read.csv(priorfile,stringsAsFactors = F,na.strings = "")
        index <- grep(";",e$cln_data$BRAND)
        brandd <- unique(e$cln_data$BRAND[index])
        if(sum(brandd %in% e$brd_prior[[e$col_1]]) != length(brandd)){
            return(e$select_prior1())
        }else{
            i <- 1
            while(i <= length(brandd)){
                e$cln_data$BRAND[which(e$cln_data$BRAND == brandd[[i]])] <- 
                    e$brd_prior[[e$col_2]][which(e$brd_prior[[e$col_1]] == brandd[[i]])]
                i = i+1
            }
            # Those priority not sure cleaned to NA
            e$cln_data$BRAND[which(e$cln_data$BRAND == "")] <- NA
        }
    }
    
    #~ 3. Find model 
    e$find_model <- function(x){
        message("\n Finding models...")
        if(x == 1){
            e$u_brd <- unique(e$cln_data$BRAND)
            e$u_brd <- e$u_brd[which(!is.na(e$u_brd))]
            e$basepool <- e$basepool1
        }else{
            e$u_brd <- unique(e$cln_data$BRAND[which(is.na(e$cln_data$MODEL))])
            e$u_brd <- e$u_brd[which(!is.na(e$u_brd))]
            e$basepool <- e$basepool2
        }
        index_namodel <- which(is.na(e$cln_data$MODEL))
        mdl <- sapply(e$u_brd, (function(x) {
            model <- unique(dplyr::filter(e$basepool, BRAND == x)$MODEL)
            if(sum(grepl("\\+",model)) > 0){ ## Special model name contains "+"
                model1 <- model
                model[grep("\\+",model)] <- gsub("\\+","\\\\+",model[grep("\\+",model)])
                index <- e$cln_data$INDEX[index_namodel][which(e$cln_data$BRAND[index_namodel] == x)]
                if (length(model) > 0) {
                    test <- model 	%>%
                        turn_exp6   %>%
                        sapply(grepl, e$BP[index])
                    if (length(e$BP[index]) > 1) {
                        colnames(test) <- model1
                        e$cln_data$MDL_NUM[index] <- rowSums(test)
                        e$cln_data$MODEL[index] <- apply(test, 1, (function(x) {
                            if (sum(x) > 0) {
                                paste(sort(names(which(x == TRUE))), collapse = ";")
                            } else
                                NA
                        }))
                    }
                }
            }else{
                index <- e$cln_data$INDEX[index_namodel][which(e$cln_data$BRAND[index_namodel] == x)]
                if (length(model) > 0) {
                    test <- model 	%>%
                        turn_exp6   %>%
                        sapply(grepl, e$BP[index])
                    if (length(e$BP[index]) > 1) {
                        colnames(test) <- model
                        e$cln_data$MDL_NUM[index] <- rowSums(test)
                        e$cln_data$MODEL[index] <- apply(test, 1, (function(x) {
                            if (sum(x) > 0) {
                                paste(sort(names(which(x == TRUE))), collapse = ";")
                            } else
                                NA
                        }))
                    }
                }
            }
        }))

        #** BMW SERIES
        turn_bmw <- function(x){
            if(is.numeric(x)){
                paste("BMW ",x,"[0-9| ][0-9| ]",sep = "") ## BMW 123 | BMW 1
            }
            else{
                paste("BMW ",x,"[0-9| ]" ,"|","BMW ",x,"-","|","BMW ",x,"$",sep = "")
            }
        }
        
        ind_bmw <- which(e$cln_data$BRAND == "BMW" & is.na(e$cln_data$MODEL))
        if(length(ind_bmw > 1)){
            n <- c(1:7,"GT","I","M","X")
            test_bmw <- turn_bmw(n) %>%
                sapply(grepl, e$BP[ind_bmw])
            
            colnames(test_bmw) <- paste(n,"SERIES",sep=" ")
            e$cln_data$MDL_NUM[ind_bmw] <- rowSums(test_bmw)
            e$cln_data$MODEL[ind_bmw] <- apply(test_bmw, 1,(function(x) {
                if (sum(x) > 0) {
                    paste(names(which(x == TRUE)),collapse = ";")
                } else
                    NA
            }))
        }
        
        #*** LEXUS SEREIS
        turn_lexus <- function(x){
            paste("LEXUS ",x,sep = "")
        }
        ind_lexus <- which(e$cln_data$BRAND == "LEXUS" & is.na(e$cln_data$MODEL))
        if(length(ind_lexus) > 0){
            n <- c("CT","ES","GS","GX","HS","IS","LF","LS","LX","NX","RC","RX","SC")
            test_lexus <- sapply(turn_lexus(n),grepl,e$BP[ind_lexus])
            colnames(test_lexus) <- paste(n,"SERIES",sep=" ")
            e$cln_data$MDL_NUM[ind_lexus] <- rowSums(test_lexus)
            e$cln_data$MODEL[ind_lexus] <- apply(test_lexus, 1,(function(x) {
                if (sum(x) > 0) {
                    paste(names(which(x == TRUE)),collapse = ";")
                } else
                    NA
            }))
        }
        
        #**** MERCEDES BENZ CLASS
        turn_benz <- function(x){
            paste("MERCEDES BENZ ",x,"[0-9| ]",
                  "|","MERCEDES BENZ ",x,"$",
                  "|","MERCEDES BENZ ",x,"-",sep = "")
        }
        ind_benz <- which(e$cln_data$BRAND == "MERCEDES BENZ" & is.na(e$cln_data$MODEL))
        if(length(ind_benz) > 1){
            n <- c("A","B","C","CL","CLA","CLC","CLK","CLS","E","G","GL","GLA","GLK","M","ML","R","S","SL","SLK","SLS","V")
            test_benz <- sapply(turn_benz(n),grepl,e$BP[ind_benz])
            colnames(test_benz) <- paste(n,"CLASS",sep=" ")
            e$cln_data$MDL_NUM[ind_benz] <- rowSums(test_benz)
            e$cln_data$MODEL[ind_benz] <- apply(test_benz, 1,(function(x) {
                if (sum(x) > 0) {
                    paste(names(which(x == TRUE)),collapse = ";")
                } else
                    return(NA)
            }))
            
        }
    }
    
    #~ 4. Clean model
    #* Select model priority 
    e$select_prior2 <- function(){
        # e$mdl_prior <- read.csv(priorfile,stringsAsFactors = F,na.strings = "")
        index <- grep(";",e$cln_data$MODEL) # Only those two or more models found will contain ";"
        modeld <- unique(e$cln_data$MODEL[index])
        comp_tbl <- data.frame(Multi.Model = e$cln_data$MODEL[index],
                               Original.Brand = e$cln_data$BRAND_ORI[index],
                               Original.Product = e$cln_data$PRODUCT_ORI[index],
                               stringsAsFactors = F)
        comp_tbl <- data.frame(Index = 1:length(modeld),
                               comp_tbl[!duplicated(comp_tbl$Multi.Model),],
                               stringsAsFactors = F)
        ind_exist <- which(modeld %in% e$mdl_prior[[e$col_3]])
        if(length(modeld) == length(ind_exist)){ 
            message("\n Priority all selected!")
            cat("\n Check the priority in 'PRIORITY.csv' if you want or Just start clean models...")
        }
        else if(length(modeld) > length(ind_exist)){
            cat("\n There are [",length(modeld)-length(ind_exist),"] entries haven't been given Priority ")
            message("\n Select the priority now!\n\n")
            if(length(ind_exist) > 0){
                modeld <- modeld[-ind_exist]
                comp_tbl <- comp_tbl[-ind_exist,]
            }
            
            message("\n You can choose a prior model if you want, or Press Enter to skip!")
            message(" To make sure you choose the right priority,It's supposed to compare the Original Brands&Products...\n")
            message("\n Enter the model Name or Number to select, q to quit!")
            
            i <- 1
            while(i <= length(modeld)){
                cat("\n-------------------------------")
                message(" Choose a model below: ")
                modeldi <- as.character(strsplit(modeld[[i]],";")[[1]])
                b <- 1
                while(b <= length(modeldi)){
                    cat("[",b,"] ",modeldi[[b]],"\n")
                    b = b+1
                }
                message(" Compare to the original: ")
                cat("[ Original Brand :] ",as.character(comp_tbl$Original.Brand[[i]]))
                cat("\n")
                cat("[Original Product:] ",as.character(comp_tbl$Original.Product[[i]]))
                cat("\n-------------------------------------------")
                cat("\n\n")
                
                model_prior <- readline(" Choose a model: ")
                j <- length(e$mdl_prior[[e$col_3]][which(!is.na(e$mdl_prior[[e$col_3]]))])+1
                
                if(model_prior == ""){
                    e$mdl_prior[[e$col_3]][[j]] <- modeld[[i]]
                    e$mdl_prior[[e$col_4]][[j]] <- NA
                    message("\n","[",modeld[[i]],"]"," Will Cleaned to NA!")
                    message("\n","[",length(modeld)-i,"]"," Lines left!")
                }
                
                else if(toupper(model_prior) %in% modeldi){
                    e$mdl_prior[[e$col_3]][[j]] <- modeld[[i]]
                    e$mdl_prior[[e$col_4]][[j]] <- toupper(model_prior)
                    message("\n","[",toupper(model_prior),"]"," has added to the list...")
                    message("\n","[",length(modeld)-i,"]","  Lines left!")
                    
                }
                else if(model_prior == "q"){
                    break
                }
                else if(as.numeric(model_prior) %in% 1:length(modeldi)){
                    k <- as.numeric(model_prior)
                    e$mdl_prior[[e$col_3]][[j]] <- modeld[[i]]
                    e$mdl_prior[[e$col_4]][[j]] <- modeldi[[k]]
                    message("\n","[",modeldi[[k]],"]"," has added to the list...")
                    message("\n","[",length(modeld)-i,"]"," Lines left!")
                }
                else{
                    message("\n Wrong Input!")
                    message("\n Please choose again... \n")
                    next
                }
                i <- i+1
                
            }
            write.csv(e$mdl_prior,"MODEL_PRIOR.csv",row.names = F,na = "")
            message("\n\n The list has saved to the 'MODEL_PRIOR.csv' file...")
            message("\n Copy the list in the 'MODEL_PRIOR.csv'to the Right column of the priorfile('PRIORITY.csv') and save the file ,then you can start clean models")
            yon <- readline(" Are you sure to clean the models based on the priority you choosed?(Y/N): ")
            if(toupper(yon) == "Y" | yon == ""){
                return(e$clean_model())
            }else{
                message("\n You'd better choose again!")
                return(e$select_prior2())
            }
        }
    }
    
    #* Clean those muti models based on the PRIORITY.csv file
    e$clean_model <- function(){
        message("\n Cleaning models...")
        e$mdl_prior <- read.csv(priorfile,stringsAsFactors = F,na.strings = "")
        index <- grep(";",e$cln_data$MODEL)
        modeld <- unique(e$cln_data$MODEL[index])
        if(sum(modeld %in% e$mdl_prior[[e$col_3]]) != length(modeld)){
            return(e$select_prior2())
        }else{
            i <- 1
            while(i <= length(modeld)){
                e$cln_data$MODEL[which(e$cln_data$MODEL == modeld[[i]])] <- 
                    e$mdl_prior[[e$col_4]][which(e$mdl_prior[[e$col_3]] == modeld[[i]])]
                i = i+1
            }
            # Those priority not sure cleaned to NA
            e$cln_data$MODEL[which(e$cln_data$MODEL == "")] <- NA
        }
    }
    
    ##----------------------------------
    # (2). Search in other market
    ##----------------------------------
    e$other_markets <- function(){
        message("\n\n Finding in other Markets...")
        index1 <- which(is.na(e$cln_data$BRAND))
        index2 <- which(is.na(e$cln_data$MODEL))
        #~ Find brand2
        e$find_brand(2)
        #~ Find model2
        e$find_model(2)
        #~ Clean brand and model
        e$clean_brand()
        e$clean_model()
        #~ Brand/Model found in other markets
        ind_addbrand <- e$cln_data$INDEX[index1][which(!is.na(e$cln_data$BRAND[index1]))]
        ind_addmodel <- e$cln_data$INDEX[index2][which(!is.na(e$cln_data$MODEL[index2]))]
        cat("\n [",length(ind_addmodel) + length(ind_addbrand),"] Rows have been found in other markets!")
        e$index_add <- unique(c(ind_addmodel,ind_addbrand))
        
    }
    
    ##----------------------------------
    # (3). Matching Family
    ##----------------------------------
    e$find_family <- function(){
        message("\n Matching family...")
        #* Match family according to the brand name and model name
        index <- which(!is.na(e$cln_data$BRAND) & !is.na(e$cln_data$MODEL))
        brdmdl <- paste(e$cln_data$BRAND[index], e$cln_data$MODEL[index], sep = " ")
        basepool <- e$all_base[which((e$all_base$BRAND %in% e$cln_data$BRAND[index]) & (e$all_base$MODEL %in% e$cln_data$MODEL[index])), ]
        e$cln_data$FAMILY[index] <- sapply(brdmdl, (function(x){
            fml <- unique(basepool$FAMILY[which(paste(basepool$BRAND, basepool$MODEL, sep = " ") == x)])
            if(length(fml) == 1) 
                return(as.character(fml)) 
            else return(NA)
        }))
        
        #** Match family which only brand name is Sure
        # 1st Condition: In the specific market
        index2 <- which(!is.na(e$cln_data$BRAND) & is.na(e$cln_data$FAMILY))
        if(length(index2) > 0 ){
            basepool2 <- e$basepool1[which(e$basepool1$BRAND %in% e$cln_data$BRAND[index2]),]
            e$cln_data$FAMILY[index2] <-
                sapply(e$cln_data$BRAND[index2], function(x) {
                    fml <- unique(basepool2$FAMILY[which(basepool2$BRAND == x)])
                    if (length(fml) == 1){
                        return(as.character(fml))
                    }else{
                        return(NA)
                    }
                })
        }
        # 2nd Condition: In all the market
        index3 <- which(!is.na(e$cln_data$BRAND) & is.na(e$cln_data$FAMILY))
        if(length(index3) > 0 ){
            basepool3 <- e$all_base[which(e$all_base$BRAND %in% e$cln_data$BRAND[index3]),]
            e$cln_data$FAMILY[index3] <-
                sapply(e$cln_data$BRAND[index3], function(x) {
                    fml <- unique(basepool3$FAMILY[which(basepool3$BRAND == x)])
                    if (length(fml) == 1){
                        return(as.character(fml))
                    }else{
                        return(NA)
                    }
                })
        }
    }
    
    ##----------------------------------
    # (4). Segment
    ##----------------------------------
    e$find_segment <- function(){
        message("\n Finding segment...")
        tm1 <- proc.time()
        index <- which(!is.na(e$cln_data$BRAND) & !is.na(e$cln_data$MODEL))
        brdmdl <- unique(paste(e$cln_data$BRAND[index], e$cln_data$MODEL[index], sep = " "))
        progressbar <- winProgressBar(title = "...", min = 0,
                                      max = length(brdmdl), width = 433)
        
        i <- 1 
        while(i <= length(brdmdl)){
            index2 <- which(paste(e$cln_data$BRAND, e$cln_data$MODEL, sep = " ") == brdmdl[[i]])
            index3 <- which(paste(e$basepool1$BRAND,e$basepool1$MODEL,sep = " ") == brdmdl[[i]])
            regional_seg <- unique(e$basepool1$REGIONAL.SEGMENT[index3])
            local_seg <- unique(e$basepool1$LOCAL.SEGMENT[index3])
            e$cln_data$REGIONAL.SEG[index2] <- paste(regional_seg[which(!is.na(regional_seg))],collapse = "/")
            e$cln_data$LOCAL.SEG[index2] <- paste(local_seg[which(!is.na(local_seg))],collapse = "/")
            setWinProgressBar(progressbar, i, title=paste( "...",round(i/length(brdmdl)*100, 0),"% Done"))
            i = i+1
            
        }
        close(progressbar)
        e$cln_data$REGIONAL.SEG[which(e$cln_data$REGIONAL.SEG == "")] <- NA
        e$cln_data$LOCAL.SEG[which(e$cln_data$LOCAL.SEG == "")] <- NA
        tm2 <- proc.time()
        if((tm2-tm1)[[3]] > 60){
            cat("\n Time used [",round((tm2 - tm1)[[3]]/60,0),"] Minutes")
        }else
            cat("\n Time used [",round((tm2 - tm1)[[3]]),"] Seconds")
    }
    
    
    ##----------------------------------
    # (5). Dealer, Retail, Range, Brand
    ##----------------------------------
    e$find_bdr <- function(){
        message("\n BDR...")
        if(e$market == "NEW ZEALAND"){
            e$cln_data$BDR <- toupper(e$rawdata$Tier[-e$ind_others])
            e$cln_data$BDR[grep("DEALERS",e$cln_data$BDR)] <- "DEALER"
        }else{
            e$cln_data$BDR[which(grepl(turn_exp2(c("RNG", "RANGE")), e$BP) == TRUE)] <- "BRAND"
            e$cln_data$BDR[which(grepl(turn_exp2(c("SPONSRSHP", "SPONSORSHIP")), e$BP) == TRUE)] <- "BRAND"
            e$cln_data$BDR[which(grepl(turn_exp2(c("RTL", "RETAIL")), e$BP) == TRUE)] <- "RETAIL"
            e$cln_data$BDR[which(grepl(turn_exp2(c("DLR", "DEALER", "DLRS", "DEALERS")), e$BP) == TRUE)] <- "DEALER"
        }
    }
    
    ##----------------------------------
    # (6). Channel
    ##----------------------------------
    e$find_channel <- function(){
        message("\n Cleaning Channel...")
        if(e$market == "INDIA"){
            e$cln_data$RGNL_MTRPLTN[which(is.na(e$cln_data$RGNL_MTRPLTN))] <- "UNKNOWN"
            e$cln_data$CHANNEL <- "TV+PRINT+RADIO"
            
            if(length(e$ind_others) > 0){
                e$cln_data_others$RGNL_MTRPLTN[which(is.na(e$cln_data_others$RGNL_MTRPLTN))] <- "UNKNOWN"
                e$cln_data_others$CHANNLE <- "TV+PRINT+RADIO"
            }
        }else{
            e$chnl_prior <- list(TV = c("TV", "TELEVISION","CATV"), 
                                 NEWSPAPER = c("NEWSPAPER", "NP", "NEWSPAPERS", "PRESS"), 
                                 MAGAZINE = c("MAGAZINE", "MG", "MAGAZINES","MZ"), 
                                 ONLINE = c("ONLINE","ON LINE","CABLE", "INTERNET", "DIGITAL"), 
                                 OOH = c("OOH", "OD", "OUT OF HOME", "OUTDOOR", "MS"), 
                                 RADIO = c("RADIO", "RD"), 
                                 CINEMA = "CINEMA", 
                                 PRINT = "PRINT", 
                                 MAIL = "MAIL",
                                 MOBILE = "MOBILE",
                                 "IN-STORE" = "IN-STORE")
            
            
            if(length(e$ind_others) > 0){
                e$channel1 <- toupper(e$rawdata[-e$ind_others, e$col_chnl])
                e$channel2 <- toupper(e$rawdata[e$ind_others,e$col_chnl])
                e$cln_data$RGNL_MTRPLTN[which(grepl("METROPOLITAN", e$channel1) == TRUE)] <- "METROPOLITAN"
                e$cln_data$RGNL_MTRPLTN[which(grepl("REGIONAL", e$channel1) == TRUE)] <- "REGIONAL"
                e$cln_data$RGNL_MTRPLTN[which(is.na(e$cln_data$RGNL_MTRPLTN))] <- "UNKNOWN"
                e$cln_data_others$RGNL_MTRPLTN[which(grepl("METROPOLITAN", e$channel2) == TRUE)] <- "METROPOLITAN"
                e$cln_data_others$RGNL_MTRPLTN[which(grepl("REGIONAL", e$channel2) == TRUE)] <- "REGIONAL"
                e$cln_data_others$RGNL_MTRPLTN[which(is.na(e$cln_data_others$RGNL_MTRPLTN))] <- "UNKNOWN"
                
                
                chnl_1 <- sapply(e$chnl_prior, (function(x){
                    test1 <- grepl(turn_exp2(x), e$channel1)
                    e$cln_data$CHANNEL[which(test1 == TRUE)] <- x[1]
                }))
                chnl_2 <- sapply(e$chnl_prior, (function(x){
                    test1 <- grepl(turn_exp2(x), e$channel2)
                    e$cln_data_others$CHANNEL[which(test1 == TRUE)] <- x[1]
                }))
            }else{
                e$channel1 <- toupper(e$rawdata[, e$col_chnl])
                e$cln_data$RGNL_MTRPLTN[which(grepl("METROPOLITAN", e$channel1) == TRUE)] <- "METROPOLITAN"
                e$cln_data$RGNL_MTRPLTN[which(grepl("REGIONAL", e$channel1) == TRUE)] <- "REGIONAL"
                e$cln_data$RGNL_MTRPLTN[which(is.na(e$cln_data$RGNL_MTRPLTN))] <- "UNKNOWN"
                chnl_1 <- sapply(e$chnl_prior, (function(x){
                    test1 <- grepl(turn_exp2(x), e$channel1)
                    e$cln_data$CHANNEL[which(test1 == TRUE)] <- x[1]
                }))
            }
        }
    }
    
    ##----------------------------------
    # (7). Category
    ##----------------------------------
    e$find_category <- function(){
        message("\n Category...")
        if(length(e$ind_others) > 0){
            if(!is.na(e$col_cat)){
                e$cln_data$CATEGORY <- toupper(e$rawdata[-e$ind_others,e$col_cat])
                e$cln_data_others$CATEGORY <- toupper(e$rawdata[e$ind_others,e$col_cat])
            }
        }else{
            if(!is.na(e$col_cat)){
                e$cln_data$CATEGORY <- toupper(e$rawdata[,e$col_cat])
            }
        }
    }
    
    
    ##----------------------------------
    # (8). Final clean
    ##----------------------------------
    e$final_clean <- function(){
        message("\n Final clean...")
        if(length(e$ind_others) > 0 ){
            e$output1 <-  cbind(MARKET = e$market, 
                                YMD = e$rawdata[-e$ind_others, e$col_ymd],
                                e$cln_data[,-(1:3)],
                                SPEND = e$rawdata[-e$ind_others, e$col_spd])
            e$output2 <- cbind(MARKET = e$market, 
                               YMD = e$rawdata[e$ind_others, e$col_ymd],
                               e$cln_data_others[,-(1:3)],
                               SPEND = e$rawdata[e$ind_others, e$col_spd])
            
            #* Bind those two seperate data frame
            e$output <- rbind(e$output1,e$output2)
        }else{
            e$output <-  cbind(MARKET = e$market, 
                               YMD = e$rawdata[, e$col_ymd],
                               e$cln_data[,-(1:3)],
                               SPEND = as.numeric(e$rawdata[, e$col_spd]))
            
        }
        #** India nameplate level and brand level spend
        if(e$market == "INDIA"){
            message("\n The spend in INDIA contains the Nameplate level and Brand level, except the nameplate we already know, there're other spends that we calculate by SpendInBrandLevel - SpendInNameplateLevl")
            # (1). NamePlate level
            npl_grp <- e$output      %>%
                group_by(MARKET,YMD,FAMILY,BRAND)    %>%
                summarise(SPEND_NPL = sum(SPEND))
            npl_grp <- data.frame(BY = paste(npl_grp$BRAND,npl_grp$YMD,sep = " "),npl_grp)
            
            # (2). Brand level
            #* Brand level file
            repeat{
                message("\n Please save your India Brand level file to a '.csv' file in your working directory.")
                inbrdlv <- readline(" Pls input your Brand Level file name: ")
                if(file.exists(inbrdlv)){
                    e$inbrdlv <- inbrdlv
                    break
                }else{
                    message("\n File doesn't exist, Pls input again")
                    cat("\n-----------------------------------------\n\n")
                    next
                }
            }
            brd_lv <- read.csv(e$inbrdlv, na.strings = "", stringsAsFactors = FALSE)
            brd_lv <- data.frame(sapply(brd_lv, toupper), stringsAsFactors = FALSE)
            # Brand level columns
            repeat{
                message("\n Please identify the column number in your India brand level file.")
                e$incol_brd <- as.numeric(readline(" col_brd(Cleaned Brand)) = "))
                e$incol_ymd <- as.numeric(readline(" col_ymd(YearMonthDay) = "))
                e$incol_spd <- as.numeric(readline(" col_spd(Spend) = "))
                cat("\n The column number right now is: \n","\n", "col_b=",e$incol_brd,"\n", 
                    "col_spd=",e$incol_spd,"\n","col_ymd=",e$incol_ymd,"\n\n") 
                yon <- readline(" Enter N if you want to Input again: ")
                if(yon != "N"){
                    break
                }else{
                    next
                }
            }
           brd_lv <- data.frame(BY = paste(brd_lv[[e$incol_brd]],brd_lv[[e$incol_ymd]],sep = " "),
                                 MARKET = e$market, YMD = brd_lv[[e$incol_ymd]], 
                                 FAMILY = NA,BRAND = brd_lv[[e$incol_brd]],MODEL = "BRAND",
                                 BRAND_ORI= brd_lv[[e$incol_brd]],PRODUCT_ORI = brd_lv[[e$incol_brd]],REGIONAL.SEG = NA,
                                 LOCAL.SEG = NA,CHANNEL = "TV+PRINT+RADIO",CHANNEL_ORI = "TV+PRINT+RADIO",
                                 RGNL_MTRPLTN = "UNKNOWN",CATEGORY = NA,BDR = NA,
                                 SPEND_BRD = as.numeric(brd_lv[[e$incol_spd]]))
            
            # (3). SPEND = Brand_lv - Nameplate_lv
            e$cln_data <- brd_lv %>%
                mutate(SPEND_NPL = sapply(brd_lv$BY,function(x){
                    if(as.character(x) %in% npl_grp$BY){
                        return(npl_grp$SPEND_NPL[which(npl_grp$BY == as.character(x))])
                    }else{
                        return(0)
                    }
                })) %>%
                mutate(SPEND = SPEND_BRD - SPEND_NPL)
            e$find_family()
            e$output <- rbind(e$output,e$cln_data[,-c(1,17,16)])
            
        }
        
        #** Clean those NA brands/models/family...
        #~ Brand&Model
        index1 <- which(!is.na(e$output$BRAND) & is.na(e$output$MODEL))
        e$output$MODEL[index1] <- "BRAND"
        e$output$BRAND[which(is.na(e$output$BRAND))] <- "OTHER"
        e$output$MODEL[which(is.na(e$output$MODEL))] <- "OTHER"
        #~ BDR
        index2 <- which(!is.na(e$output$BRAND) & is.na(e$output$BDR))
        e$output$BDR[index2] <- "BRAND"
        e$output$BDR[which(is.na(e$output$BDR))] <- "OTHER"
        #~ Family
        e$output$FAMILY[which((e$output$MODEL == "BRAND") & is.na(e$output$FAMILY))] <- "BRAND"
        e$output$FAMILY[which(is.na(e$output$FAMILY))] <- "OTHER"
        #~ Segment
        e$output$REGIONAL.SEG[which(e$output$MODEL == "BRAND" & is.na(e$output$REGIONAL.SEG))] <- "BRAND"
        e$output$LOCAL.SEG[which(e$output$REGIONAL.SEG == "BRAND")] <- "BRAND"
        e$output$REGIONAL.SEG[which(is.na(e$output$REGIONAL.SEG))] <- "OTHER"
        e$output$LOCAL.SEG[which(is.na(e$output$LOCAL.SEG))] <- "OTHER"
        #~ Category
        e$output$CATEGORY[which(is.na(e$output$CATEGORY))] <- "UNKNOWN"
        e$output$CATEGORY[which((e$output$CATEGORY) == "")] <- "UNKNOWN"
        #~ Spend 
        e$output$SPEND[which(is.na(e$output$SPEND))] <- 0
    }
    
    
    ##----------------------------------
    # (9). Check up and output
    ##----------------------------------
    e$checkup <- function(){
        #* Check by distinct rows
        channels <- names(e$chnl_prior)
        e$ck <- e$output[which(e$output$FAMILY == "OTHER"
                               |e$output$BRAND == "OTHER"
                               |e$output$MODEL == "BRAND"
                               |(!e$output$CHANNEL %in% channels)),]
        message("\n There're [",nrow(e$ck),"] Rows with problem Family/Brand/Model/Channel!")
        if(nrow(e$ck) > 0){
            filename1 <- paste("DistinctCleanedCheck_",e$market,".csv",sep = "")
            message("\n Those rows with problem has been distincted by the origional Brand&Product.")
            message("\n It's suggest to check up the distincted file and the whole cleaned file at the same time!")
            message("\n The distinct list is in:")
            cat("\n",filename1)
            cat("\n-----------------------------------------------------")
            e$ck <- data.frame(CheckItem = NA,e$ck,BP = paste(e$ck$BRAND_ORI,e$ck$PRODUCT_ORI))
            i <- 1
            while(i  <= nrow(e$ck)){
                index <- which(c(e$ck$FAMILY[[i]] == "OTHER",
                                 e$ck$BRAND[[i]] == "OTHER",
                                 e$ck$MODEL[[i]] == "BRAND",
                                 (!e$ck$CHANNEL[[i]] %in% channels)))
                e$ck$CheckItem[[i]] <- paste(c("FAMILY","BRAND","MODEL","CHANNEL")[index],collapse = "/ ")
                i = i+1
            }
            e$check <- distinct(e$ck,BP)
            e$check <- arrange(e$check,CheckItem)
            e$check <- data.frame(sapply(e$check, toupper), stringsAsFactors = FALSE)
            write.csv(e$check,filename1,row.names = F)
        }else{
            message("\n All rows founded! Check up the output file manually if necessary!")
        }
        #** Output
        #~ The brands/models found in other markets
        if(length(e$index_add) > 0){
            add <- data.frame(MARKET = e$market,"REGIONAL SEGMENT" = NA,"LOCAL SEGMENT" = NA,
                              FAMILY = e$cln_data$FAMILY[e$index_add],
                              BRAND = e$cln_data$BRAND[e$index_add],
                              MODEL = e$cln_data$MODEL[e$index_add])
            
            addu <- add[!duplicated(add$MODEL),]
            filename2 <- paste("FoundInOtherMarkets_",e$market,".csv",sep = "")
            message("\n Saving the list found in other markets...")
            write.csv(addu,filename2,row.names = F,na = "")
            message("\n The list found in other markets have been saved to the file:")
            cat("\n ",filename2)
            message("\n Refer to this file and decide to update the basefile")
        }
        #~ The output .csv file 
        filename3 <- paste("Clean_",e$market,".csv",sep = "")
        cat("\n-------------------------------------------------------------\n")
        message("\n Writing csv output file...")
        write.csv(e$output,filename3,row.names = F)
        message("\n The output csv file is:")
        cat("\n",filename3)
        
        
        #~ Write to MySQL database
        repeat{
            cat("\n----------------------------------------------------------\n")
            message("\n If you have double checked the cleaned data, then you can write it to the MySQL database.")
            message("\n Enter Y if you'r sure or Enter N to clean the data again, q to quit!")
            yon <- readline("Are you sure to write into MySQL database?(Y/N): ")
            if(toupper(yon) == "Y"){
                message("\n Connecting to MySQL...")
                con <- dbConnect(MySQL(),
                                 host = "******", 
                                 port = 3306, 
                                 db = "media",
                                 user = "MSU_DB_Manager", 
                                 password = "Embarassing...")
                tables <- dbListTables(con)
                cat("\n---------------------\n")
                message("\n The tables in the Media database are:")
                for(i in 1:length(tables)){
                    cat("\n [",i,"] ")
                    cat(tables[i])
                }
                cat("\n--------------------------\n")
                repeat{
                    tablename <- readline(" Chose a table to insert into: ")
                    if(tablename %in% tables){
                        e$tablename <- tablename
                        break
                    }else if(as.numeric(tablename) %in% 1:length(tables)){
                        e$tablename <- tables[as.numeric(tablename)]
                        break
                    }else{
                        message("\n Wrong Input!")
                        next
                    }
                }
                message("\n Inserting into MySQL...\n")
                db_insert_into(con, 
                               table = e$tablename, 
                               value = e$output)
                suc <- readline("Write into MySQL database successfully!")
                cat(" [",nrow(e$output),"] rows have inserted into the database...\n\n")
                
                break
                
            }else if(toupper(yon) == "N"){
                start()
            }else if(yon == "q"){
                break
            }else{
                message("\n Wrong input!")
                next
            }
        }

    }
    
    start <- function(){
        message("\n This program is for Ford media data clean,")
        cat("\n Your rawfile is  [",rawfile,"]")
        message("\n Press Enter to Start! q to quit...\n")
        start <- readline(" Start?(Y/N): ")
        if(toupper(start) == "Y"){
            step_control()
        }else if(toupper(start) == "N"){
            stop
        }else if(start == ""){
            step_control()
        }else if(start == "q"){
            stop
        }
    }
    start()
    

}