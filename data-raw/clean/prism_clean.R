## /* prism_clean.R --- 
##' Filename: prism_clean.R
##' Description: Cleaning the prism data
##' Author: Noah Peart
##' Created: Fri Feb 26 16:33:56 2016 (-0500)
##' Last-Updated: Tue Mar  8 18:47:30 2016 (-0500)
##'           By: Noah Peart
## */

##' ---
##' title: "Cleaning the Prism data"
##' output:
##'    html_document:
##'       theme: readable
##'       highlight: zenburn
##'       toc: true
##'       code_folding: hide
##'       keep_md: false
##'       self_contained: true
##' ---

##- setup, echo=FALSE, include=FALSE --------------------
library(knitr)
opts_chunk$set(message=FALSE, cache=FALSE, echo=TRUE)
library(data.table)
library(seedsub)
seeds <- copy(segdata)
subs <- copy(segsub)
## /* end setup */
##' # Master files
##- master-files ----------------------------------------

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

## load master data
load("../temp/prismraw.rda")
dat <- copy(prismraw)

## /* end master-files */
##' # Cleaning
##' 
##'   + Renamed `CENS` to `YEAR`
##'   + Merge the `SGDSP88` with `SGDSP9899` (there is no overlap) to single `SGDSP`
##'   + Fix the CTP columns: change `CTP1` and `CTP3` to be endpoints, `CTP2` middle
##'       + 1988 PIRU only, **endpoints** only
##'       + 1989 PIRU only, 3 points
##'       + 1998 all spec, 3 points
##'       + 1999 all spec, 3 points
##'   + Create `CTP` as the weighted prism value: 1*(start+end) + 2*middle
##'   + Change variable types
##---- clean ----------------------------------------
setnames(dat, "CENS", "YEAR")

## merge SGDSP columns
dat[, SGDSP := ifelse(is.na(SGDSP88) | SGDSP88 == '', SGDSP9899, SGDSP88)]
dat[, c("SGDSP88", "SGDSP9899") := NULL]

## Rename the CTP columns to be CTP[1:3] = (start, middle, end)
setnames(dat, paste0("CTP", 1:3), paste0("CTP", c(1,3,2)))

## Create CTP as 1*sides + 2*middle prism values
dat[, CTP := 2*ifelse(is.na(CTP2),0,CTP2) + 
        ifelse(is.na(CTP1), 0, CTP1) + ifelse(is.na(CTP3), 0, CTP3)]

## Types
ints <- c("YEAR", "STPACE", paste0("CTP", 1:3), "CTP")
dat[, (ints) := lapply(.SD, as.integer), .SDcols = ints]

## Save
prism <- dat
save(prism, file="../temp/prism.rda", compress="bzip2")
## /* end clean */
