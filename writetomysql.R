# Write the clened files to MySQL Zz

tomysql <- function(){
    con <- dbConnect(MySQL(),
                     host = "**********", 
                     port = 3306, 
                     db = "media",
                     user = "MSU_DB_Manager", 
                     password = "Embarassing...")
    
    all_market <- c("AUSTRALIA","INDIA","INDONESIA","SOUTH KOREA", "TAIWAN",
                    "NEW ZEALAND","VIETNAM","CHINA","THAILAND","PHILIPPINES")
    
    for(i in 1:length(all_market)){
        
        filename <- paste("Clean_",all_market[[i]],".csv",sep = "")
        output <- read.csv(filename,stringsAsFactors = F)
        message(" Inserting data of ",all_market[[i]],"  ",i/length(all_market)*100,"% Done")
        message(" (Remain ",length(all_market)-i," markets)\n")
        db_insert_into(con, 
                       table = "clean", 
                       value = output)
    }
}