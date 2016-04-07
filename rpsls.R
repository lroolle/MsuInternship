number_to_name<-function(number){
    if (number == 0){
         return("rock")}
    else if (number == 1){
         return("spock")}
    else if (number == 2){
         return("paper")}
    else if (number == 3){
         return("lizard")}
    else if (number == 4){
         return ("scissors")}
}

name_to_number<-function(name){
    if(name == "rock"){
         return(0)}
    else if(name == "spock"){
         return(1)}
    else if(name == "paper"){
         return(2)}
    else if(name == "lizard"){
        return (3)}
    else if(name == "scissors"){
        return (4)}
}

rpsls<-function(name){ 
    if(!name %in% as.character(c("rock","lizard","paper","scissors","spock"))){
       message(" Wrong Input  ")
}
    else{
      player_number <- name_to_number(name)
      comp_number <-sample(0:4,size=1)

      cat("You choose ")
      cat(name)
      cat("\n")
      cat("Computer choose ")
      cat(number_to_name(comp_number))
      cat("\n")

      i<- (player_number - comp_number)%%5 
         
      if ((i== 1)| (i== 2)){
         message("Player wins!")}
      else if ((i== 3) | (i== 4)){
         message("Computer wins!")}
      else{
         message("Player and computer tie!")}
  }
}

