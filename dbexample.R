# script to draw dumbbell plot for death outcome data
# to use need to run following:
# source('dbexample.R')

source('dbplotLR.R')
source('dbplotprob.R')

# set up data
authors <- c("Maitland","Pamba","Ahmed",NA,"Djelantik","Sigauque","Webb",NA)
years <- c("2006", "2004", "2001", NA, "2003", "2009", "2012", NA)
cutoffs <- c(">2s", ">3s", "prolonged", NA,"<86","<90","<96", NA)
Ns <- c("920","4160", "72", NA, "4306", "584", "568", NA)
lrplus <- c( 1.7254, 3.0611, 6.7000, NA, 3.0833, 1.6266, 2.9188, NA)
lrminus <- c(0.8213, 0.8303, 0.4393, NA, 0.5582, 0.7426, 0.4850, NA)
p1 <- c(0.191304, 0.045433, 0.069444, NA, 0.111900, 0.065100, 0.059900, NA)

require(graphics)

win.graph(width=10,height=5)

dbplotLR(p1,lrminus,lrplus,lablmag=19,textsz=0.8)

# add text
par(las=1) # ensure all text is horizontal

for(i in 1:length(authors))
{
    if (!is.na(Ns[i])) {
        mtext(paste(authors[i],"(",years[i],")"),2,line=18,at=i,adj=0,cex=0.8)
        mtext(Ns[i],2,line=12,at=i,adj=0,cex=0.8)
        mtext(cutoffs[i],2,line=9,at=i,adj=0,cex=0.8)
        mtext(as.character(lrplus[i]),2,line=5,at=i,adj=0,cex=0.8)
        mtext(as.character(lrminus[i]),2,line=2,at=i,adj=0,cex=0.8)
    }
}

mtext("Paper",2,line=18,at=9,adj=0,cex=0.8,font=2)
mtext("N",2,line=12,at=9,adj=0,cex=0.8,font=2)
mtext("Cutoff",2,line=9,at=9,adj=0,cex=0.8,font=2)
mtext("LR+",2,line=5,at=9,adj=0,cex=0.8,font=2)
mtext("LR-",2,line=2,at=9,adj=0,cex=0.8,font=2)

mtext("Oxygen saturation",2,line=18.5,at=8,adj=0,cex=0.8,font=2)
mtext("Capillary refill time",2,line=18.5,at=4,adj=0,cex=0.8,font=2)


