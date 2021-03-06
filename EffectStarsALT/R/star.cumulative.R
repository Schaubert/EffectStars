star.cumulative <-
function (formula, data, global = NULL, test.rel = TRUE, test.glob = FALSE, 
    partial = FALSE, globcircle = FALSE, maxit = 100, scale = TRUE, 
    nlines = NULL, select = NULL, dist.x = 1, dist.y = 1, dist.cov = 1, 
    dist.cat = 1, xpd = TRUE, main = "", col.fill = "gray90", 
    col.circle = "black", lwd.circle = 1, lty.circle = "longdash", 
    col.global = "black", lwd.global = 1, lty.global = "dotdash", cex.labels = 1, 
    cex.cat = 0.8, xlim = NULL, ylim = NULL) 
{ 
     if(!is.data.frame(data))
      stop("Argument data has to be of class data.frame")
    if(!is.logical(test.rel))
      stop("Argument test.rel has to be of class logical")
    if(!is.logical(test.glob))
      stop("Argument test.glob has to be of class logical")
    if(!is.logical(globcircle))
      stop("Argument globcircle has to be of class logical")
    if(!is.logical(scale))
      stop("Argument scale has to be of class logical")
     if(!is.logical(partial))
      stop("Argument partial has to be of class logical")
    if(!is.logical(xpd) & !is.na(xpd))
      stop("Argument xpd has to be of class logical or NA")
    if(sum(grep("\\*",as.character(formula)[3]))>0)
      stop("Interactions can not be regarded automatically yet. Please create 
           Interactions in the data frame before calling the function!")
    if(!is.numeric(dist.x))
      stop("Argument dist.x has to be numeric")
    if(!is.numeric(dist.y))
      stop("Argument dist.y has to be numeric") 
    if(!is.numeric(dist.cov))
      stop("Argument dist.cov has to be numeric")
    if(!is.numeric(dist.cat))
      stop("Argument dist.cat has to be numeric")
     
    is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    
    if(!is.null(nlines)){
      if(!is.wholenumber(nlines) & length(nlines)==1){
      stop("Argument nlines has to be NULL or a single number of type integer")}}
     
    par(xpd = xpd)
    response <- all.vars(formula)[1]
    covariates <- all.vars(formula)[-1]
    resp <- data[, colnames(data) == response]

    if(!is.ordered(resp))
      stop("The response has to be of class ordered!")
    
    if(!is.null(select) & !is.vector(select, mode = "numeric"))
      stop("Argument select has to be NULL or a numeric vector")
    
    if(!is.null(select)){select<-sort(unique(floor(select)))}
      

    keylab <- levels(resp)
    z <- length(keylab)
    n <- nrow(data)
    p <- length(covariates)
    # standardize metric covariates

    isfac <- c()
    labvec <- ""
    covlab <- c()
    dummydat <- resp
    constlist<-list()
    #loop: create labels, create dummy-coded data set
    for (j in 1:p) {
      # labels
        actcov <- data[, colnames(data) == covariates[j]]
        if (is.factor(actcov) | is.ordered(actcov)) {
            nlev <- length(levels(actcov))
            isfac <- c(isfac, rep(TRUE, (nlev - 1)))
            covlab <- c(covlab, rep(covariates[j], nlev - 1))
            if (is.ordered(actcov)) {
                parlevs <- paste("(", sort(as.numeric(unique(actcov)))[-nlev], 
                  ": ", levels(actcov)[-nlev], sep = "")
                labvec <- c(labvec, parlevs)
            }
            else {
                if (length(levels(actcov)) == 2) {
                  parlevs <- paste("(", sort(as.numeric(unique(actcov)) - 
                    1)[-1], ": ", levels(actcov)[-1], sep = "")
                }
                else {
                  parlevs <- paste("(", sort(as.numeric(unique(actcov)))[-1], 
                    ": ", levels(actcov)[-1], sep = "")
                }
                labvec <- c(labvec, parlevs)
            }
        }
        else {
            isfac <- c(isfac, FALSE)
            covlab <- c(covlab, covariates[j])
            labvec <- c(labvec, "")
        }
        # dummy coding
        variab <- data[, colnames(data) == covariates[j]]
        levlen <- length(levels(variab))
        if (is.factor(variab) & levlen > 2) {
            if (!is.ordered(variab)) {
                variab <- as.numeric(variab)
                help <- matrix(0, nrow = nrow(data), ncol = levlen - 1)
                for (ooo in 2:levlen) {
                  help[variab == ooo, (ooo - 1)] <- 1
                }
            }
            else {
                help <- matrix(0, nrow = nrow(data), ncol = levlen - 
                  1)
                for (ooo in 1:(levlen - 1)) {
                  help[variab == ooo, ooo] <- 1
                }
            }
            dummydat <- cbind(dummydat, help)
        }
        else {
           if (is.factor(variab)){
               if(sum(levels(as.factor(as.numeric(variab))) == 
                c("1", "2")) == 2) {
                variab <- as.numeric(variab) - 1
            }}
            dummydat <- cbind(dummydat, variab)
        }
    }
    dummydat <- as.data.frame(dummydat)
    colnames(dummydat) <- paste("V", 1:ncol(dummydat), sep = "")
    logxy <- c()
    # compute models and likelihoods 
    if (!partial) {
      for(cc in 1:ncol(dummydat)){
      constlist[[cc]]<-diag(z-1)
      if(cc %in% global){constlist[[cc]]<-matrix(1,ncol=1,nrow=z-1)}}
    names(constlist) <- c("(Intercept)", colnames(dummydat)[-1])
    formdummy<-as.formula(paste("V1~",paste(paste("V",2:ncol(dummydat),sep=""),collapse="+")))
    multobj <- vglm(formdummy, family = cumulative(parallel = FALSE), 
        data = dummydat, maxit = maxit,constraints=constlist)
    likefull <- logLik(multobj)
    coefmat <- t(coefficients(multobj, matrix = TRUE))
    l <- ncol(coefmat)
          if(sum(global>l)>0){global<-global[global<=l]}
    pp<-l - 1
    nonglobal<-(1:l)[!((1:l) %in% global)]
      
    if(!is.null(global)){
    indcat<-apply(coefmat, 2,var)==0
    indcat2 <- rep(indcat, each=z-1)
    indout <- rep(((1:length(indcat))[indcat]-1)*(z-1),each=z-2)+(2:(z-1))
    indcat2 <-indcat2[-indout]
    vcov <- vcov(multobj)[!indcat2,!indcat2]
    sdsmat2 <- matrix(sqrt(diag(vcov)), nrow = z-1)
    if(length(global)==1){
      vcov2<-sqrt(vcov(multobj)[indcat2,indcat2])
                  }else{
    vcov2 <- sqrt(diag(vcov(multobj)[indcat2,indcat2]))}
    sdsmat<-matrix(0,ncol=ncol(coefmat),nrow=nrow(coefmat))
    sdsmat[,nonglobal]<-sdsmat2
    sdsmat[,global]<-matrix(rep(c(vcov2),each=z-1),byrow=FALSE,ncol=length(global))
    }else{
      vcov <- vcov(multobj)
      sdsmat <- matrix(sqrt(diag(vcov)), nrow = z-1)}
      
      # likelihood for relevance test
        if (test.rel) {
            for (coc in 1:(ncol(dummydat) - 1)) {
              constlist2<-constlist
            constlist2<-constlist2[-(coc+1)]
                formhelp3 <- paste("V", (2:ncol(dummydat))[-coc], 
                  sep = "", collapse = "+")
                form3 <- as.formula(paste("V1~", formhelp3, sep = ""))
                if(identical(formhelp3,"V")){form3<-"V1 ~ 1"}
                logxy[coc] <- logLik(vglm(form3, family = cumulative(parallel = FALSE), 
                  data = dummydat, maxit = maxit,constraints=constlist2))
            }
        }
        # likelihood for global test and global coefficients
        if (test.glob | globcircle) {
            likelis <- c()
            formhelp <- paste("V", 2:ncol(dummydat), sep = "", 
                collapse = "+")
            form2 <- as.formula(paste("V1~", formhelp, sep = ""))
            conlist <- vector("list", l)
            conlist[[1]] <- diag(z - 1)
            names(conlist) <- c("(Intercept)", colnames(dummydat)[-1])
            for (ii in 2:(pp + 1)) {
                conlist[[ii]] <- diag(z - 1)
                 if(ii %in% global){ conlist[[ii]]<-matrix(1,ncol=1,nrow=z-1)}
            }
            globals<-c()
            for (co in 1:pp) {
               if(!(co+1) %in% global){
                conlist2 <- conlist
                notpar <- covariates[co]
                conlist2[[co + 1]] <- matrix(1, z - 1, 1)
                multhelp2 <- vglm(form2, family = cumulative(parallel = FALSE), 
                  data = dummydat, constraints = conlist2, maxit = maxit)
                likelis[co] <- logLik(multhelp2)
                globals[co]<-coefficients(multhelp2, TRUE)[co + 1, 1]
               }else{likelis[co]<-NA
                globals[co]<-NA}
            }
        }
    }
    # compute models and likelihoods for partial proportional odds models
    else {
        likelis <- c()
        formhelp <- paste("V", 2:ncol(dummydat), sep = "", collapse = "+")
        form2 <- as.formula(paste("V1~", formhelp, sep = ""))
        multhelp <- vglm(form2, family = cumulative(parallel = TRUE), 
            data = dummydat, maxit = maxit)
        coefhelp <- coefficients(multhelp)[1:(z - 1)]
        likezero <- logLik(multhelp)
        coefnames <- names(coefficients(multhelp)[z:length(coefficients(multhelp))])
        lhelp <- length(coefficients(multhelp))
        start <- z
        pp <- lhelp - z + 1
        conlist <- vector("list", pp + 1)
        conlist[[1]] <- diag(z - 1)
        names(conlist) <- c("(Intercept)", names(coefficients(multhelp)[-(1:(z - 
            1))]))
        for (ii in 2:(pp + 1)) {
            conlist[[ii]] <- matrix(1, z - 1, 1)
        }
        l1 <- (lhelp - z + 2) * (z - 1)
        covarmat <- matrix(0, ncol = l1, nrow = l1)
        covarmat[1:(z - 1), 1:(z - 1)] <- vcov(multhelp)[1:(z - 
            1), 1:(z - 1)]
        startvar <- z
        nonglobal<-(1:(pp+1))[!((1:(pp+1)) %in% global)]
        for (co in 1:pp) {
            conlist2 <- conlist
            notpar <- covariates[co]
            conlist2[[co + 1]] <- diag(z - 1)
            multhelp2 <- vglm(form2, family = cumulative(parallel = FALSE), 
                data = dummydat, constraints = conlist2, maxit = maxit)
            likelis[co] <- logLik(multhelp2)
            coefnow <- coefficients(multhelp2)
            lnow <- length(coefnow)
            end <- start + lnow - lhelp
            coefhelp <- rbind(coefhelp, coefnow[start:end])
            endvar <- startvar + end - start
            covarmat[startvar:endvar, startvar:endvar] <- vcov(multhelp2)[start:end, 
                start:end]
            start <- start + (lnow - lhelp + 1)/(z - 1)
            startvar <- endvar + 1
            # likelihood for relevance test
            if (test.rel) {
                formhelp3 <- paste("V", (2:ncol(dummydat))[-co], 
                  sep = "", collapse = "+")
                form3 <- as.formula(paste("V1~", formhelp3, sep = ""))
                if(identical(formhelp3,"V")){form3<-"V1 ~ 1"}
                logxy[co] <- logLik(vglm(form3, family = cumulative(parallel = TRUE), 
                  data = dummydat), maxit = maxit)
            }
        }
        coefmat <- t(coefhelp)
        colnames(coefmat) <- c("(Intercept)", coefnames)
        l <- ncol(coefmat)
        if(sum(global>l)>0){global<-global[global<=l]}
        coefmat[,global]<-matrix(rep(coefficients(multhelp)[-(1:(z - 2))][global],each=z-1)
          ,ncol=length(global),nrow=z-1,byrow=FALSE)
        vcov <- covarmat
        sdsmat <- matrix(sqrt(diag(vcov)), nrow = z - 1)
    }
    # pvalues for relevance tests
    if (test.rel) {        
      dfs<-rep(z-1,length(logxy))
        dfs[global-1]<-1
        if (partial) {
            lrstats <- 2 * (-logxy + likelis)
        }
        else {
            lrstats <- 2 * (-logxy + likefull)
        }
        lrpvals <- 1 - pchisq(lrstats, df = dfs)
        lrexact <- lrpvals
        lrpvals[lrpvals < 5e-04] <- 0
        lrpvals[lrpvals >= 5e-04 & lrpvals < 0.001] <- 0.001
        lrpvals <- format(lrpvals, digits = 1, nsmall = 3)
    }
    # pvalues for global tests
    if (test.glob) {
        dfs <- z - 2
        if (partial) {
            globstats <- 2 * (likelis - likezero)
        }
        else {
            globstats <- 2 * (-likelis + likefull)
        }
        globpvals <- 1 - pchisq(globstats, df = dfs)
         globex <- globpvals
        globpvals[globpvals < 5e-04] <- 0
        globpvals[globpvals >= 5e-04 & globpvals < 0.001] <- 0.001
        globpvals <- format(globpvals, digits = 1, nsmall = 3)
    }
    
    if(is.null(select)){
      lselect<-l
      select<-1:l
    }else{
      if(sum(select>l)>0){select<-select[select<=l]}
      lselect<-length(select)}
      plusint <- select==1
    
    # compute exponentials and scaling factors
    coefs <- exp(coefmat)
    x1 <- t(coefs)
    fac <- max(x1)
    facs <- apply(x1, 1, max)/fac
    # compute number of lines
   if (is.null(nlines)){
     if (floor(sqrt(lselect)) == sqrt(lselect)) {
            nlines <- sqrt(lselect)
        }
        else {
            nlines <- (floor(sqrt(lselect)) + 1)
        }
}
    l2 <- nlines^2
    # scaling
    facmat <- matrix(facs, nrow = nrow(x1), ncol = ncol(x1))
    odds <- t(x1)
    if (scale) {
        x1 <- x1/facmat
    }
    select<-select[!(select %in% global)]
    # compute locations and limits
    plotmat<-x1[select,]
    if(lselect==1){plotmat<-t(as.matrix(plotmat))}
    loc <- stars(plotmat, nrow = nlines, scale = FALSE, len = 1, cex = 1.2, 
        lwd = 1, flip.labels = FALSE, plot = FALSE) * fac
    loc[, 1] <- loc[, 1] * dist.x * 2
    loc[, 2] <- loc[, 2] * 1.9 * dist.y
    if (is.null(ylim)) {
        ylim <- c(min(loc[, 2]) - 1.5 * max(x1), max(loc[, 2]) + 
            2 * max(x1))
    }
    if (is.null(xlim)) {
        xlim <- c(min(loc[, 1]) - 1.6 * max(x1), max(loc[, 1]) + 
            1.6 * max(x1))
    }
    
     
     
    par(cex = 1, font = 3)
    stars(plotmat, scale = FALSE, draw.segments = FALSE, len = 1, 
        lwd = 1.2, lty = "solid", flip.labels = FALSE, locations = loc, 
        xlim = xlim, ylim = ylim, labels = "", main = main, add = FALSE)
    par(cex = 1, font = 1)
     
    # compute angles in stars, plot category labels
    cosis <- c()
    sinis <- c()
    alts <- 0:(z - 2)
    angle <- alts * ((360/(z - 1)) * pi)/180
    cosis <- cos(angle)
    sinis <- sin(angle)
    xlocs <- ylocs <- matrix(0, ncol = l, nrow = z - 1)
    index<-1
    xx<-x1
    disfac<-0.4
    if(!scale){disfac<-0.3}
    dis2<-max(xx * disfac * dist.cat, xx * disfac * dist.cat)
    for (co in select) {
        dir <- x1[co, ]
        locx <- loc[index, 1]
        locy <- loc[index, 2]
        xlocs[, co] <- rep(locx, z-1) + cosis * dir + cosis * dis2
        ylocs[, co] <- rep(locy, z-1) + sinis * dir + sinis * dis2
        index<-index+1
    }

      if (!scale) {
        facs <- rep(1, l)
    } 
     
    # plot relevance circle
    radii2 <- 1/facs
    if(!identical(select,1)){
    par(lty = lty.circle, lwd = lwd.circle, col = col.circle)
    symbols(c(loc[, 1]), c(loc[, 2]), circles = radii2[select], 
        add = T, inches = F, fg = col.circle, bg = col.fill)
    par(lty = 1, lwd = 1, col = 1)
    } 
     
    par(cex = 1, font = 3)
    stars(plotmat, scale = FALSE, draw.segments = FALSE, len = 1, 
        lwd = 1.2, lty = "solid", flip.labels = FALSE, locations = loc, 
        xlim = xlim, ylim = ylim, labels = "", main = main, add = TRUE)
    par(cex = 1, font = 1)
     
    text(x = xlocs[,select], y = ylocs[,select], labels = keylab[-z], cex = cex.cat, 
        font = 1)
    
    # plot global circle
    locint <- rep(TRUE,lselect)
    if(sum(plusint)==1){locint[1] <- FALSE}
    if (globcircle) {
      if(partial){  
      radii <- c(0, exp(coefficients(multhelp)[-(1:(z - 1))]))/facs
      }else{
        radii <- c(0, exp(globals))/facs
        }
      if(!identical(select,1)){
        par(lty = lty.global, lwd = lwd.global, col = col.global)
        symbols(c(loc[locint, 1]), c(loc[locint, 2]), circles = radii[select[!plusint]], 
            add = T, inches = F, fg = col.global)
        par(lty = 1, lwd = 1, col = 1)
      }
    }
    
    # create label vectors
    lab1 <- labvec
    if (test.rel) {
        if (test.glob) {
            labvec[-1] <- paste(labvec[-1], ", p-rel=", lrpvals, 
                ", p-global=", globpvals, ")", sep = "")
            labvec[-1][!isfac] <- paste("(p-rel=", lrpvals[!isfac], 
                ", p-global=", globpvals[!isfac], ")", sep = "")
        }
        else {
            labvec[-1] <- paste(labvec[-1], ", p-rel=", lrpvals, 
                ")", sep = "")
            labvec[-1][!isfac] <- paste("(p-rel=", lrpvals[!isfac], 
                ")", sep = "")
        }
    }
    else {
        if (test.glob) {
            labvec[-1] <- paste(labvec[-1], ", p-global=", globpvals, 
                ")", sep = "")
            labvec[-1][!isfac] <- paste("(p-global=", globpvals[!isfac], 
                ")", sep = "")
        }
        else {
            labvec[-1][isfac] <- paste(labvec[-1][isfac], ")", 
                sep = "")
        }
    }
    plotlabs <- paste(c("Intercept", covlab), "\n", labvec, sep = "")
    text(y = loc[, 2] + fac * 0.06 * (l2 + 20) * dist.cov, x = loc[, 1], 
        labels = plotlabs[select], font = 1, cex = cex.labels)
   
     # create return lists

    mult2<- vglm(formula, family = cumulative(parallel = TRUE), data = data, maxit = maxit)
    outputnames<-rownames(coefficients(mult2,TRUE))
    outputrows<-colnames(coefficients(mult2,TRUE))
    colnames(sdsmat)<-outputnames
    colnames(odds)<-outputnames
    oddsnames<-paste("odds(P[Y<=",1:(z-1),"])",sep="")
    rownames(odds)<-oddsnames
    rownames(sdsmat)<-outputrows
    coefficients<-log(odds)
    rownames(coefficients)<-outputrows
     ################
     
    if (test.rel) {
        p_rel <- matrix(lrexact, ncol = l - 1)
        colnames(p_rel) <- outputnames[-1]
        if(test.glob){
          globex[global-1]<-NA
           p_glob <- matrix(globex[!is.na(globex)], ncol = length(likelis[!is.na(globex)]))
          colnames(p_glob) <- outputnames[nonglobal][-1]
          returns <- list(odds = odds, 
            coefficients = coefficients, se = sdsmat, p_rel = p_rel, p_global = p_glob, 
                          xlim = xlim, ylim = ylim)
          }else{
        returns <- list(odds = odds, 
            coefficients = coefficients, se = sdsmat, p_rel = p_rel, xlim = xlim, ylim = ylim)
          }
    }
    else {
      if(test.glob){
        globex[global-1]<-NA
           p_glob <- matrix(globex[!is.na(globex)], ncol = length(likelis[!is.na(globex)]))
          colnames(p_glob) <- colnames(odds)[-1]
          returns <- list(odds = odds, 
            coefficients = coefficients, se = sdsmat, p_global = p_glob, xlim = xlim, ylim = ylim)
          }else{
        returns <- list(odds = odds, 
            coefficients = coefficients, se = sdsmat, xlim = xlim, ylim = ylim)
    }
    }
    returns
}
  