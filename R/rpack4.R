#' Reminds users of the author's birthday
#' @export
remind_me <- function(){
  return("The birthday of the owner of this package is 26/10")
}

#' Allows users to cheat with assignment 3.1
#' @param number numeric variable
cheat<- function(number){
  schiphol <- read.csv("https://raw.githubusercontent.com/hannesrosenbusch/schiphol_class/master/schiphol_data.csv")
  if (number == 1){
    return(boxplot(rnorm(n = 100, mean = 3, sd = 1)))
  } else {
    if (number == 2){
      return(plot(schiphol$DATE, schiphol$TAVG, main = "Scatterplot", xlab = "time", ylab = "average temperature"))
    } else if (number == 6){
      return(ggplot(data = Orange, aes(x = factor(Tree, levels = c("1", "2", "3", "4", "5")), y = circumference)) +
               stat_summary(fun = "max", geom = "bar") +
               labs(x = "Tree", y = "max_circumfrence"))
    } else if (number == 16){
      return(rbind(seq(1,3,1), seq(8,12,2), seq(21,27,3)))
    } else
      print("please type a number that is either 1,2,6 or 16")
  }
}
