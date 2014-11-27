Dumbell Plotting
================

R code to create "dumbell" pre- and post-test probability plots using prevalences and either likelihood ratios or actual pre and post test probabilities.

Note that this isn't a package or library (although I may get around to turning it into one in the future).  You will need to import the contents of the file using source('filename') before being able to use the functions.

Included files:

*dbplotLR.R*  
Contains one function: dbplotLR(prevalence, negLR, posLR, lablmag=7, textsz=1)
This calls dbplotprob with appropriate pre and post test probability values.

*dbplotprob.R*
Contains one function: dbplotprob(prevalence, negprob, posprob, lablmag=7, textsz=1)
This plots a dumbbell plot for values given by the vectors in prevalence, negprob and posprob.
The left hand margin is extended by an amount defined by lablmag to allow for labels.
textsz is not currently used.
    
*epitests.R*
Contains one function: epitests(a,b,c,d,conf.level=0.95)
This is the epi.tests function from an early version of epiR, which has now been changed to have different input format.  This input format is particularly useful in some circumstances, such as calculating likelihood ratios for input for dbplotLR, so I've preserved it.
   

Susannah Fleming - 27 Nov 2014

