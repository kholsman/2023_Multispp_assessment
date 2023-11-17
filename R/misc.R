#' misc stuff
#' 
#' 

## decorate lm object with a new class lm_right
lm_right <- function(formula,data,...){
  mod <- lm(formula,data)
  class(mod) <- c('lm_right',class(mod))
  mod
}
## decorate lm object with a new class lm_right
gam_right <- function(formula = y ~ s(x, bs = "cs"),data,...){
  mod <- gam(formula,data)
  class(mod) <- c('gam_right',class(mod))
  mod
}

## decorate lm object with a new class lm_left
lm_left <- function(formula,data,...){
  mod <- lm(formula,data)
  class(mod) <- c('lm_left',class(mod))
  mod
}
## decorate lm object with a new class lm_left
gam_left <- function(formula = y ~ s(x, bs = "cs"),data,...){
  mod <- gam(formula,data)
  class(mod) <- c('gam_left',class(mod))
  mod
}
predictdf.gam_right <- 
  function(model, xseq, se, level){
    ## here the main code: truncate to x values at the right
    init_range = range(model$model$x)
    xseq <- xseq[xseq >=init_range[1]]
    ggplot2:::predictdf.default(model, xseq[-length(xseq)], se, level)
  }
predictdf.gam_left <- 
  function(model, xseq, se, level){
    init_range = range(model$model$x)
    ## here the main code: truncate to x values at the left
    xseq <- xseq[xseq <=init_range[2]]
    ggplot2:::predictdf.default(model, xseq[-length(xseq)], se, level)
  }
predictdf.lm_right <- 
  function(model, xseq, se, level){
    ## here the main code: truncate to x values at the right
    init_range = range(model$model$x)
    xseq <- xseq[xseq >=init_range[1]]
    ggplot2:::predictdf.default(model, xseq[-length(xseq)], se, level)
  }
predictdf.lm_left <- 
  function(model, xseq, se, level){
    init_range = range(model$model$x)
    ## here the main code: truncate to x values at the left
    xseq <- xseq[xseq <=init_range[2]]
    ggplot2:::predictdf.default(model, xseq[-length(xseq)], se, level)
  }
