# function for making dumbbell plots
# to run the function, you first import it using source('dbplot.R')
# you can then call dbplotLR() as a function
# inputs are prevalence, and negative and positive likelihood ratios

dbplotLR <- function(prevalence, negLR, posLR,lablmag=7,textsz=1){

# post-test probabilities
testnegs = 100 * (negLR * prevalence) / (1 - prevalence + (negLR * prevalence))
testpos = 100 * (posLR * prevalence) / (1 - prevalence + (posLR * prevalence))
priors = 100 * prevalence

dbplotprob(priors, testnegs, testpos, lablmag, testsz)

## indices for y values
#idx = 1:length(priors)
#
## extend left hand margin to allow for labels (2nd value is relevant one)
#par(mar=c(5,lablmag,4,2))
#
## plot priors, set x and y axes
#plot(priors,idx,type="p",col="blue", pch=16, xlim=c(0, 100), xlab="Probability (%)", ylab="", yaxt="n", frame.plot=FALSE)
## plot lines
#for(i in idx) lines(x=c(testnegs[i], testpos[i]), y=c(i, i), type="l", lty=2)
## plot testpos and testneg
#points(x=testpos,y=idx,type="p",col="green", pch=16)
#points(x=testnegs,y=idx,type="p",col="red",pch=16)
## plot priors again to overlay
#points(x=priors,y=idx,type="p",col="blue",pch=16)
## add labels to y axis
##axis(2, at=idx, labels=labls, las=1,cex.axis=textsz, tick=FALSE)
#return(1)
}
#
#
#
## lty = 1 solid, 2 dashed, 3 dotted, 4 dotdash, 5 longdash, 6 twodash
## pch see http://www.statmethods.net/advgraphs/parameters.html
