#'
#'  Misconception of the law of averages: Understanding the empirical law of large numbers
#' 
#'  (c) 2016 Mehmet Suzen 
#'  

rm(list=ls())
set.seed(987231)
library(Rcpp)          # law level implementations
library(LaplacesDemon) # for drawing samples from Bernoulli distribution
library(matlab)        # for tic/toc
# Set working directory to script directory 
Rcpp:::sourceCpp("empirical_law_of_large_numbers.cpp") # diff_binary_vec2
#'
#' Calculate the running difference in a binary vector 
#' 
diff_binary_vec  <- function(bvec) {
 ll <- length(bvec) 
 diff_vec  <- vector("double", ll)
 diff_vec  <- sapply(1:ll, function(i) { abs(i-2.0*sum(bvec[1:i])) } )
 diff_vec
}
#'
#' Calculate the running ratio in a binary vector 
#'
ratio_binary_vec  <- function(bvec) {
 ll         <- length(bvec) 
 ratio_vec  <- vector("double", ll)
 ratio_vec  <- sapply(1:ll, function(i) { abs(i/sum(bvec[1:i])-1.0) } )
 ratio_vec  <- sapply(ratio_vec, function(rv) {if(is.infinite(rv)) { rv <- 0; }; rv })
 ratio_vec
}

#' Wall-call timing difference 
tb <- rbern(20000, 0.5) # a fair-coin
tic()
t1 <- diff_binary_vec(tb)
toc()
tic()
t2 <- diff_binary_vec2(tb)
toc()
tic()
r1 <- ratio_binary_vec(tb)
toc()
tic()
r2 <- ratio_binary_vec2(tb)
toc()
#' 
#' Generate Bernoulli Process
#' 
nr        <- 50    # repeats
nt        <- 20000 # Bernoulli trails
tic()
bern_df <- data.frame(trail=c(), diff=c(), ratio=c())
for(i in 1:nr) {
  cat("repeat:",i, "\n")
  trail   <- rbern(nt, 0.5) # a fair-coin
  diff    <- diff_binary_vec2(trail)
  ratio   <- ratio_binary_vec2(trail)
  bern_df <- rbind(bern_df, cbind(1:nt, diff, ratio))
}

#' Now plot ratio and diff evolution with local regression in ggplot2
names(bern_df) <- c("trail", "diff", "ratio")
library(ggplot2)
p_diff <- ggplot(data=bern_df, aes(x=trail, y=diff)) + geom_smooth(formula="y~x") + 
          theme(
                panel.background = element_blank(), 
                axis.text.x      = element_text(face="bold", color="#000000", size=11),
                axis.text.y      = element_text(face="bold", color="#000000", size=11),
                axis.title.x     = element_text(face="bold", color="#000000", size=11),
                axis.title.y     = element_text(face="bold", color="#000000", size=11))  +
           xlab("Bernoulli Trails") + ylab("Difference between occurance of two outcomes") +
           ggtitle("No Law of Averages: Tail/Heads do not balanced out!")

png(file="no_law_of_averages.png")
p_diff
dev.off()

p_ratio <- ggplot(data=bern_df, aes(x=trail, y=ratio)) + geom_smooth(formula="y~x") + 
          theme(
                panel.background = element_blank(), 
                axis.text.x      = element_text(face="bold", color="#000000", size=11),
                axis.text.y      = element_text(face="bold", color="#000000", size=11),
                axis.title.x     = element_text(face="bold", color="#000000", size=11),
                axis.title.y     = element_text(face="bold", color="#000000", size=11))  +
           xlab("Bernoulli Trails") + ylab("Difference between occurance of two outcomes") +
           ggtitle("Empirical Law of Large Numbers: Ratio of Tails/Heads approach to one")

png(file="law_of_large_numbers.png")
p_ratio
dev.off()

