pkgname <- "EffectStars"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('EffectStars')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BEPS")
### * BEPS

flush(stderr()); flush(stdout())

### Name: BEPS
### Title: British Election Panel Study
### Aliases: BEPS
### Keywords: datasets multinomial response

### ** Examples

## Not run: 
##D data(BEPS)
##D vignette("BEPS")
## End(Not run)



cleanEx()
nameEx("EffectStars-package")
### * EffectStars-package

flush(stderr()); flush(stdout())

### Name: EffectStars
### Title: Visualization of Categorical Response Models
### Aliases: EffectStars-package EffectStars
### Keywords: package multinomial response ordinal response multinomial
###   logit model sequential logit model cumulative logit model categorical
###   data

### ** Examples

## Not run: 
##D vignette("alligator")
##D vignette("BEPS")
##D vignette("coffee")
##D vignette("election")
##D vignette("insolvency")
##D vignette("PID")  
##D vignette("plebiscite")
##D vignette("womenlabour")
## End(Not run)



cleanEx()
nameEx("PID")
### * PID

flush(stderr()); flush(stdout())

### Name: PID
### Title: Party Identification
### Aliases: PID
### Keywords: datasets multinomial response

### ** Examples

## Not run: 
##D data(PID)
##D vignette("PID")
## End(Not run)



cleanEx()
nameEx("alligator")
### * alligator

flush(stderr()); flush(stdout())

### Name: alligator
### Title: Alligator Food
### Aliases: alligator
### Keywords: datasets multinomial response

### ** Examples

## Not run: 
##D data(alligator)
##D vignette("alligator")
## End(Not run)



cleanEx()
nameEx("coffee")
### * coffee

flush(stderr()); flush(stdout())

### Name: coffee
### Title: Coffee Brands
### Aliases: coffee
### Keywords: datasets multinomial response

### ** Examples

## Not run: 
##D data(coffee)
##D vignette("coffee")
## End(Not run)



cleanEx()
nameEx("election")
### * election

flush(stderr()); flush(stdout())

### Name: election
### Title: Election Data
### Aliases: election
### Keywords: datasets multinomial response

### ** Examples

data(election)
vignette("election")



cleanEx()
nameEx("insolvency")
### * insolvency

flush(stderr()); flush(stdout())

### Name: insolvency
### Title: Insolvency data
### Aliases: insolvency
### Keywords: datasets ordinal response

### ** Examples

## Not run: 
##D data(insolvency)
##D vignette("insolvency")
## End(Not run)



cleanEx()
nameEx("plebiscite")
### * plebiscite

flush(stderr()); flush(stdout())

### Name: plebiscite
### Title: Chilean Plebiscite
### Aliases: plebiscite
### Keywords: datasets multinomial response

### ** Examples

## Not run: 
##D data(plebiscite)
##D vignette("plebiscite")
## End(Not run)



cleanEx()
nameEx("star.cumulative")
### * star.cumulative

flush(stderr()); flush(stdout())

### Name: star.cumulative
### Title: Effect stars for cumulative logit models
### Aliases: star.cumulative
### Keywords: cumulative logit model star plot ordinal response

### ** Examples

## Not run: 
##D vignette("insolvency")
## End(Not run)



cleanEx()
nameEx("star.nominal")
### * star.nominal

flush(stderr()); flush(stdout())

### Name: star.nominal
### Title: Effect stars for multinomial logit models
### Aliases: star.nominal
### Keywords: multinomial logit model star plot multinomial response

### ** Examples

## Not run: 
##D vignette("election")
##D vignette("alligator")
##D vignette("coffee")
##D vignette("plebiscite")
##D vignette("PID")
##D vignette("BEPS")
##D vignette("womenlabour")
## End(Not run)



cleanEx()
nameEx("star.sequential")
### * star.sequential

flush(stderr()); flush(stdout())

### Name: star.sequential
### Title: Effect stars for sequential logit models
### Aliases: star.sequential
### Keywords: sequential logit model star plot ordinal response

### ** Examples

## Not run: 
##D vignette("insolvency")
## End(Not run)



cleanEx()
nameEx("womenlabour")
### * womenlabour

flush(stderr()); flush(stdout())

### Name: womenlabour
### Title: Canadian Women's Labour-Force Participation
### Aliases: womenlabour
### Keywords: datasets multinomial response

### ** Examples

## Not run: 
##D data(womenlabour)
##D vignette("womenlabour")
## End(Not run)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
