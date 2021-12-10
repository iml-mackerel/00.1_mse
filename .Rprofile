.First <- function(){
  
  ### load packages
  cran.packages <- c('devtools','reshape2','ggplot2','gridExtra','viridis','plyr','grid','RColorBrewer')
  new.packages <- cran.packages[!(cran.packages %in% utils::installed.packages()[,"Package"])]
  if(length(new.packages)>0) install.packages(new.packages)
  
  git.packages <- c('CCAM')
  new.packages <- git.packages[!(git.packages %in% utils::installed.packages()[,"Package"])]
  if(length(new.packages)>0){
     devtools::install_github("elisvb/CCAM")
   }

  lapply(c(cran.packages,git.packages), function(x) suppressMessages(require(x, character.only = TRUE)))
  
  ### source src directory
  invisible(sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source))
  
  ### ggplot layout
  theme_new <- theme_set(theme_classic())
  theme_new <- theme_update(axis.line.x = element_line(color="black"),
                            axis.line.y = element_line(color="black"),
                            legend.background = element_rect(fill=alpha('blue', 0)))
}
