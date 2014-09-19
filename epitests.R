# this is the epi.tests function from epiR version 0.9_32
# it is used because the newer versions are no longer compatible with analysis of large amounts of data in list form

epitests <- function (a, b, c, d, conf.level = 0.95) 
{
    N. <- 1 - ((1 - conf.level)/2)
    z <- qnorm(N., mean = 0, sd = 1)
    lower <- "lower"
    upper <- "upper"
    .funincrisk <- function(dat, conf.level) {
        N. <- 1 - ((1 - conf.level)/2)
        a <- dat[, 1]
        n <- dat[, 2]
        b <- n - a
        p <- a/n
        a. <- ifelse(a == 0, a + 1, a)
        b. <- ifelse(b == 0, b + 1, b)
        low <- a./(a. + (b. + 1) * (1/qf(1 - N., 2 * a., 2 * 
            b. + 2)))
        up <- (a. + 1)/(a. + 1 + b./(1/qf(1 - N., 2 * b., 2 * 
            a. + 2)))
        low <- ifelse(a == 0, 0, low)
        up <- ifelse(a == n, 1, up)
        rval <- as.data.frame(cbind(p, low, up))
        names(rval) <- c("est", lower, upper)
        rval
    }
    M1 <- a + c
    M0 <- b + d
    N1 <- a + b
    N0 <- c + d
    total <- a + b + c + d
    tdat <- as.matrix(cbind(M1, total))
    trval <- .funincrisk(tdat, conf.level)
    tp <- trval$est
    tp.low <- trval$lower
    tp.up <- trval$upper
    tprev <- as.data.frame(cbind(tp, tp.low, tp.up))
    names(tprev) <- c("est", lower, upper)
    tdat <- as.matrix(cbind(N1, total))
    trval <- .funincrisk(tdat, conf.level)
    ap <- trval$est
    ap.low <- trval$lower
    ap.up <- trval$upper
    aprev <- as.data.frame(cbind(ap, ap.low, ap.up))
    names(aprev) <- c("est", lower, upper)
    tdat <- as.matrix(cbind(a, M1))
    trval <- .funincrisk(tdat, conf.level)
    se <- trval$est
    se.low <- trval$lower
    se.up <- trval$upper
    sensitivity <- as.data.frame(cbind(se, se.low, se.up))
    names(sensitivity) <- c("est", lower, upper)
    tdat <- as.matrix(cbind(d, M0))
    trval <- .funincrisk(tdat, conf.level)
    sp <- trval$est
    sp.low <- trval$lower
    sp.up <- trval$upper
    specificity <- as.data.frame(cbind(sp, sp.low, sp.up))
    names(specificity) <- c("est", lower, upper)
    tdat <- as.matrix(cbind(a, N1))
    trval <- .funincrisk(tdat, conf.level)
    ppv <- trval$est
    ppv.low <- trval$lower
    ppv.up <- trval$upper
    positive <- as.data.frame(cbind(ppv, ppv.low, ppv.up))
    names(positive) <- c("est", lower, upper)
    tdat <- as.matrix(cbind(d, N0))
    trval <- .funincrisk(tdat, conf.level)
    npv <- trval$est
    npv.low <- trval$lower
    npv.up <- trval$upper
    negative <- as.data.frame(cbind(npv, npv.low, npv.up))
    names(negative) <- c("est", lower, upper)
    lrpos <- (a/M1)/(1 - (d/M0))
    lrpos.low <- exp(log(lrpos) - z * sqrt((1 - se)/(M1 * se) + 
        (sp)/(M0 * (1 - sp))))
    lrpos.up <- exp(log(lrpos) + z * sqrt((1 - se)/(M1 * se) + 
        (sp)/(M0 * (1 - sp))))
    lr.positive <- as.data.frame(cbind(lrpos, lrpos.low, lrpos.up))
    names(lr.positive) <- c("est", lower, upper)
    lrneg <- (1 - (a/M1))/(d/M0)
    lrneg.low <- exp(log(lrneg) - z * sqrt((se)/(M1 * (1 - se)) + 
        (1 - sp)/(M0 * (sp))))
    lrneg.up <- exp(log(lrneg) + z * sqrt((se)/(M1 * (1 - se)) + 
        (1 - sp)/(M0 * (sp))))
    lr.negative <- as.data.frame(cbind(lrneg, lrneg.low, lrneg.up))
    names(lr.negative) <- c("est", lower, upper)
    tdat <- as.matrix(cbind((a + d), total))
    trval <- .funincrisk(tdat, conf.level)
    da <- trval$est
    da.low <- trval$lower
    da.up <- trval$upper
    da.acc <- as.data.frame(cbind(da, da.low, da.up))
    names(da.acc) <- c("est", lower, upper)
    dOR.p <- (a * d)/(b * c)
    lndOR <- log(dOR.p)
    lndOR.var <- 1/a + 1/b + 1/c + 1/d
    lndOR.se <- sqrt(1/a + 1/b + 1/c + 1/d)
    lndOR.l <- lndOR - (z * lndOR.se)
    lndOR.u <- lndOR + (z * lndOR.se)
    dOR.se <- exp(lndOR.se)
    dOR.low <- exp(lndOR.l)
    dOR.up <- exp(lndOR.u)
    dor <- as.data.frame(cbind(dOR.p, dOR.low, dOR.up))
    names(dor) <- c("est", lower, upper)
    ndx <- 1/(se - (1 - sp))
    ndx.1 <- 1/(se.low - (1 - sp.low))
    ndx.2 <- 1/(se.up - (1 - sp.up))
    ndx.low <- min(ndx.1, ndx.2)
    ndx.up <- max(ndx.1, ndx.2)
    nnd <- as.data.frame(cbind(ndx, ndx.low, ndx.up))
    names(nnd) <- c("est", lower, upper)
    c.p <- se - (1 - sp)
    c.1 <- se.low - (1 - sp.low)
    c.2 <- se.up - (1 - sp.up)
    c.low <- min(c.1, c.2)
    c.up <- max(c.1, c.2)
    youden <- as.data.frame(cbind(c.p, c.low, c.up))
    names(youden) <- c("est", lower, upper)
    aprev <- as.data.frame(aprev)
    tprev <- as.data.frame(tprev)
    se <- as.data.frame(sensitivity)
    sp <- as.data.frame(specificity)
    da <- as.data.frame(da.acc)
    dor <- as.data.frame(dor)
    nnd <- as.data.frame(nnd)
    youden <- as.data.frame(youden)
    ppv <- as.data.frame(positive)
    npv <- as.data.frame(negative)
    lr.pos <- as.data.frame(lr.positive)
    lr.neg <- as.data.frame(lr.negative)
    rval <- list(aprev = aprev, tprev = tprev, se = se, sp = sp, 
        da = da, dor = dor, nnd = nnd, youden = youden, ppv = ppv, 
        npv = npv, lr.pos = lr.pos, lr.neg = lr.neg)
    return(rval)
}

