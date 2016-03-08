## /* prisms-summary.R --- 
##' Filename: prisms-summary.R
##' Description: Brief tour of the prism data
##' Author: Noah Peart
##' Created: Tue Mar  8 17:13:52 2016 (-0500)
##' Last-Updated: Tue Mar  8 17:45:16 2016 (-0500)
##'           By: Noah Peart
## */
##' ---
##' title: "Prisms"
##' author: ""
##' date: ""
##' ---

##- setup, echo=FALSE, include=FALSE --------------------
library(knitr)
opts_chunk$set(message=FALSE, cache=FALSE, echo=TRUE)
library(data.table)
library(DT)
library(seedsub)
seeds <- copy(segsub)
subs <- copy(segsub)
## /* end setup */
##' 
##' # Initial look
##' Load the data from AFS and do some quick checks.  Store a local copy in ../temp.
##- load-raw ----------------------------------------

## get the raw data
## mf <- sync.afs::get_key()[, rname]
## library(sync.afs)
## afs <- AFS$new()
## afs$signin()
## dat <- get_data("prismmas12")
## sapply(dat, typeof)
## str(dat)
## prismraw <- dat
## save(prismraw, file="../temp/prismraw.rda", compress="bzip2")
load("../temp/prismraw.rda")
dat <- copy(prismraw)

## /* end load-raw */
##' 
##' # Explore
##' 
##' ## SGDSP
##' segment displacements need to be combined into one column, years stripped. 
##' Table of 88 followed by 9899 data.
##- explore ----------------------------------------
setnames(dat, "CENS", "YEAR")

## SGDSP
table(dat$SGDSP88)
table(dat$SGDSP9899)

## No overlapping SGDSP, just merge
## dat[SGDSP88 != '' & SGDSP9899 != '', .N]
##' ## Species by years
##' 
##' Species collected in the different years.
##- specs-yrs ----------------------------------------

## SPEC by CENS
datatable(dat[, .(Species = list(unique(SPEC))), by=YEAR],
  caption="Species collected by year.")

## Check counts of SPEC by year
samps <- grep("^CTP[1-3]", names(dat), value=TRUE)
tmp <- dat[, lapply(.SD, sum, na.rm=TRUE), by=c("YEAR", "SPEC"), .SDcols = samps]
datatable(tmp, caption="Total counts of CPT by year/spec.")

## /* end specs-yrs */
##' 
##' ## Correlations between prism points
##' 
##' Pairwise pearson correlations / cosine distances between species count vectors
##' for each plot.  This excludes 1988/1989 data because only PIRU was collected,
##' and excludes 2012 data b/c only midpoints of segments were sampled.  Also,
##' only live trees are included (excluding "DEAD", "SNAG").
##- prism-correlation ----------------------------------------

## Check correlations between cpts within segments
tmp <- dat[TYPE=="ALIVE" & YEAR %in% c(1998, 1999), ]
tmp[, PID := .GRP[[1]], by=c("YEAR", "CONTNAM", "STPACE")]  # plot identifier

## In 2012, plots were only censored in the middle
no_ctps <- tmp[, lapply(.SD, sum, na.rm=TRUE), by=PID, .SDcols = samps][
  CTP1 == 0 & CTP3 == 0, ]
unique(tmp[no_ctps, .(YEAR), on="PID"])

## tmp[, c("YEAR", "CONTNAM", "STPACE") := NULL]
cors <- tmp[YEAR != 2012, { 
  corr = cor(cbind(CTP1, CTP2, CTP3), use="pairwise.complete.obs")
  .(CTP1_2 = corr[1, 2], CTP2_3 = corr[2, 3], CTP1_3 = corr[1, 3])
}, by=PID]

library(corrplot)
library(ggplot2)
dd <- melt(cors, id.vars="PID")
dd[, variable := factor(variable, levels=c("CTP1_2", "CTP2_3", "CTP1_3"), ordered=TRUE)]
p1 <- ggplot(dd, aes(variable, value)) +
  geom_line(aes(group = PID), alpha=0.3) + 
  geom_point() +
  geom_boxplot(color="red", fill="transparent", size=1.05) +
  theme_bw() +
  xlab("Prism Position-Position") + ylab("Pearson correlation") +
  ggtitle("Pearson correlation (1998/1999)")

library(lsa)
sims <- tmp[, { 
  co = cosine(cbind(CTP1, CTP2, CTP3))
  .(CTP1_2 = co[1, 2], CTP2_3 = co[2, 3], CTP1_3 = co[1, 3])
}, by=PID]
dd <- melt(sims, id.var="PID")
mus <- dd[, mean(value), by=variable]

p2 <- ggplot(dd, aes(variable, value)) +
  geom_line(aes(group=PID), alpha=0.3) + 
  geom_point() +
  stat_summary(aes(variable, value), inherit.aes=FALSE, 
    geom="errorbar", fun.data=mean_cl_normal, color="red", size=1.1, width=0.2) +
  theme_bw() +
  xlab("Prism Position-Position") + ylab("Cosine Distance") +
  ggtitle("Cosine distance (1998/1999)")

library(gridExtra)
grid.arrange(p1, p2, ncol=2)

## /* end prism-correlation */
##' 
##' Applying the same methods to random groupings of plot-level species vectors.
##' Here, 1000 random samples for each of the prism locations (sampled w/ replacement).
##- prism-random-correlations ----------------------------------------

## Generate a set to randomly sample from
rand <- copy(tmp)
n <- 1000   # sample size
pvec <- 13  # unique species
randset <- split(unlist(rand[, samps, with=FALSE]), 
  rep(seq(length(unique(rand$PID))*3), each=pvec))

tst <- unlist(randset[sample(length(randset), 3*n, TRUE)])
tst <- as.data.table(split(tst, rep(seq.int(3), each=pvec*n)))
setnames(tst, names(tst), samps)
tst[, PID := rep(seq.int(n), each=pvec)]

cors <- tst[, { 
  corr = cor(cbind(CTP1, CTP2, CTP3), use="pairwise.complete.obs")
  .(CTP1_2 = corr[1, 2], CTP2_3 = corr[2, 3], CTP1_3 = corr[1, 3])
}, by=PID]

dd2 <- melt(cors, id.vars="PID")
dd2[, variable := factor(variable, levels=c("CTP1_2", "CTP2_3", "CTP1_3"), ordered=TRUE)]
p1 <- ggplot(dd2, aes(variable, value)) +
  geom_line(aes(group = PID), alpha=0.3) + 
  geom_point() +
  geom_boxplot(color="red", fill="transparent", size=1.05) +
  theme_bw() +
  xlab("Prism Position-Position") + ylab("Pearson correlation") +
  ggtitle("Pearson correlation for Random Samples (1998/1999)")
p1

## /* end prism-random-correlations */
##' ## Correlogram
##' 
##' Correlograms for various groupings of species vectors across gradiets.  These plots
##' were too large to really use very well.  An example looking at just **LW1900**
##' contour plots to slim down the sample size.
##' 
##- correlogram ----------------------------------------
tst <- copy(tmp[CONTNAM == "LW1900"])[, CTP := CTP1 + 2*CTP2 + CTP3]
tst[, PNAME := paste(CONTNAM, STPACE, sep="_")]
tst <- dcast(tst, SPEC ~ PNAME, fill=0, fun=identity, value.var=c("CTP"))

library(corrgram)
col.corrgram <- function(ncol) {
  colorRampPalette(c("darkgoldenrod4", "burlywood1", "darkkhaki", 
    "darkgreen"))(ncol)
}

corrgram(tst,
  text.panel=panel.txt, col.regions=col.corrgram,
  order=TRUE, lower.panel = panel.shade, upper.panel=panel.pie,
  main="Correlogram of Low West segments (PC2/PC1 order)",)
## /* end correlogram */

##'
##' ## Average prism values for each species
##' 
##' Still only looking at 2012 data.
##- spec-averages ----------------------------------------
mu_norm <- mean(unlist(lapply(randset, norm, type="2")))  # average l2-norm

avgs <- tmp[, .(
  Mean=mean(c(CTP1, CTP2, CTP3), na.rm=TRUE),
  StdDev=sd(c(CTP1, CTP2, CTP3), na.rm=TRUE)), by=SPEC]

datatable(avgs, caption="Average species values for 1998/1999 prisms.")
## /* end spec-averages */
##' After weighting central prism sample by a factor 2, the values for prisms by
##' species across plots are shown here (kinda hard to see cause there are so many).
##' The values for are scaled for each species (otherwise ABBA and BECO dominate it
##' too heavily), so the colors are blue where species are most abundant relative to
##' their normal abundance.
##- prism-heat ----------------------------------------
## Weight CTP
tst <- copy(tmp)[, CTP := CTP1 + 2*CTP2 + CTP3]
tst[, PNAME := paste(CONTNAM, STPACE, sep="_")]
tst <- dcast(tst, SPEC ~ PNAME, fill=0, fun=identity, value.var=c("CTP"))

library(d3heatmap)
d3heatmap(tst[, -1, with=FALSE], k_row=4, k_col=20, labRow=tst[, SPEC],
  scale=c("row"))

## /* end prism-heat */
##' ## Distributions across gradients
##' 
##' Look at the distributions of these ... in progress
library(fitdistrplus)
tst <- tmp[SPEC == "ABBA", c(CTP1, CTP2, CTP3)]


