# Just for fun!


betable <- function(filename,col = 5){
    
    file <- read.csv(filename, stringsAsFactors = F)
    col_name <- colnames(file)
    n <- c()
    # get the max length of each column:
    for(i in 1:col){
        x <- nchar(col_name[[i]])
        y <- nchar(file[1,i])
        z <- nchar(file[2,i])
        n[i] <- max(x,y,z)
    }
    cat("\n\n")
    # 1st line:
    for(i in 1:col){
        cat("+")
        x <- n[i] + 2
        for(j in 1:x){
            cat("-")
        }
    }
    cat("+-----+\n")
   # 2nd line:
    for(i in 1:col){
        x <- n[i] - nchar(col_name[i])
        cat("|")
        for(j in 1:floor(x/2+1.5)){
            cat(" ")
        }
        cat(col_name[i])
        for(k in 1:floor(x/2+1)){
            cat(" ")
        }
    }
    cat("| ... |\n")
    # 3rd line:
    for(i in 1:col){
        cat("+")
        x <- n[i] + 2
        for(j in 1:x){
            cat("=")
        }
    }
    cat("+=====+\n")
    # 4th line: 
    for(i in 1:col){
        x <- n[i] - nchar(file[1,i])
        cat("|")
        for(j in 1:floor(x/2+1.5)){
            cat(" ")
        }
        cat(file[1,i])
        for(k in 1:floor(x/2+1)){
            cat(" ")
        }
    }
    cat("| ... |\n")
    # 5th line:
    for(i in 1:col){
        cat("+")
        x <- n[i] + 2
        for(j in 1:x){
            cat("-")
        }
    }
    cat("+-----+\n")
    # 6th line:
    for(i in 1:col){
        x <- n[i] - nchar(file[2,i])
        cat("|")
        for(j in 1:floor(x/2+1.5)){
            cat(" ")
        }
        cat(file[2,i])
        for(k in 1:floor(x/2+1)){
            cat(" ")
        }
    }
    cat("| ... |\n")
    # 7th line:
    for(i in 1:col){
        cat("+")
        x <- n[i] + 2
        for(j in 1:x){
            cat("-")
        }
    }
    cat("+-----+\n")
    # last line:
    for(i in 1:col){
        x <- n[i] - 3
        cat("| ")
        cat("...")
        for(j in 1:(x+1)){
            cat(" ")
        }
    }
    cat("| ... |\n\n")
    
}