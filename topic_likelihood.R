
#topic likehood

library(topicmodels)

# data("dtm")

# set number of topics to start with
k <- 10

# set model options
control_LDA_VEM <-
  list(estimate.alpha = TRUE, alpha = 50/k, estimate.beta = TRUE,
       verbose = 0, prefix = tempfile(), save = 0, keep = 0,
       seed = as.integer(100), nstart = 1, best = TRUE,
       var = list(iter.max = 10, tol = 10^-6),
       em = list(iter.max = 10, tol = 10^-4),
       initialize = "random")

# create the sequence that stores the number of topics to 
# iterate over
sequ <- seq(5, 30, by = 5)

# basic loop to iterate over different topic numbers with gc
# after each run to empty out RAM
lda <- vector(mode='list', length = length(sequ))
for(k in sequ) {
  lda[[k]] <- LDA(dtm1[1:20,], k, method= "VEM", control = control_LDA_VEM)
  gc() # here's where I put the garbage collection to free up memory before the next round of the loop
}

# convert list output to dataframe (suggestions for a simpler method are welcome!)
best.model.logLik <- data.frame(logLik = as.matrix(lapply(lda[sequ], logLik)), ntopic = sequ)

# plot
with(best.model.logLik, plot(ntopic, logLik, type = 'l', xlab="Number of topics", ylab="Log likelihood"))
