## /* prism_clean.R --- 
##' Filename: prism_clean.R
##' Description: Cleaning the prism data
##' Author: Noah Peart
##' Created: Fri Feb 26 16:33:56 2016 (-0500)
##' Last-Updated: Tue Mar  8 17:45:35 2016 (-0500)
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
##'   + Created `CTP` the derived weighted prism value
##---- clean ----------------------------------------
setnames(dat, "CENS", "YEAR")

## merge SGDSP columns

## /* end clean */
