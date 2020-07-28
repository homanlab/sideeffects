#
# sideeffects_do.R
#
# created on Tue Oct  1 10:09:18 2019
# Philipp Homan, <philipp dot homan at bli dot uzh dot ch>
#-----------------------------------------------------------------------
#
# load data and create timestamp for makefile
source("sideeffects_load.R")
file.create("../output/R/sideeffects_do.Rout")

#wgl <- long2longer(wg)


indices <- c(
  "prolactinsd",
  "qtcsd",
  "weightsd"
)

widths <- c(
  6.92,
  6.98,
  6.96
)

widths2 <- c(
  7.81,
  6.24,
  6.72 
  )


heights <- c(
  7.8,
  5.1,
  8.5
)

heights2 <- c(
  2.95,
  2.54,
  2.88
  )

#heights2 <- heights * c(0.43, 0.6, 0.38)

greater1s <- c(
  "",
  "Greater in control",
  "Greater in control"
)

greater2s <- c(
  "Greater in treatment",
  "Greater in treatment",
  "Greater in treatment"
)

ylims <- list(c(0, 15), c(0, 2.7), c(0, 2.7))
ylims2 <- list(c(0, 3.5), c(0, 2.7), c(0, 2.7))

# missing values
norigtrials <- sum(!is.na(unique(wgorig$Study_No)))
#alltrials <- sum(!is.na(unique(wgexcl$Study_No)))
#allarms   <- sum(!is.na(unique(wgexcl$Arm_ID)))

#allsdmiss <- sum((!is.na(wgexcl$WeightContM) &
#                 (is.na(wgexcl$WeightContSD) |
#                  !wgexcl$WeightGainSDOrigin %in%
#                 c("original", "p-value")) |
#                (!is.na(wgexcl$ProlactinM) &
#                (is.na(wgexcl$ProlactinSD) |
#                 !wgexcl$ProlactinSDOrigin %in%
#                 c("original", "p-value")) | 
#                (!is.na(wgexcl$QTcM) &
#                (is.na(wgexcl$QTcSD) |
#                 !wgexcl$QTcSDOrigin %in%
#                 c("original", "p-value"))))))  
  
#norigtrials <- sum(!is.na(unique(wgl$id)))
ntrialsincluded <- sum(!is.na(unique(wgl$studynumber)))



# total n per index
ntot <- vector("numeric", length(indices))
ntottx <- vector("numeric", length(indices))
ntotct <- vector("numeric", length(indices))

# total rcts per index
nrct <- vector("numeric", length(indices))

# total missing sds per index
nsdmissing <- vector("numeric", length(indices))

# total comparisons per index
ncomp <- vector("numeric", length(indices))

# vr summary results
vrstat <- list(length(indices))

# vr summary results by med
vrstat2 <- list(length(indices))

# perc vr > 1
vr_count <- vector("numeric", length(indices))


for (i in 1:length(indices)) {
  index <- indices[i]


  # calculate number of studies that did not report
  # variance (but did report mean)
  nsdmissing[i] <- calc_sd_missing(index)
  
  # escalc for index
  # syntax:
  # rdat <- escalc(measure = "VR", 
  #              m1i = mu1tx, n1i = ntx, sd1i = sd1tx, 
  #              m2i = mu1ct, n2i = nct, sd2i = sd1ct, 
  #              data = df)
  rdat1 <- escalc(measure="VR",
                  n1i=n.active,
                  n2i=n.control,
                  sd1i=sd.active,
                  sd2i=sd.control,
                  data=long2wide(wgl, index))

  m1    <- rma(yi = yi, vi = vi, data = rdat1, method = "REML",
               slab = paste(rdat1$id), weighted = TRUE)
  #summary(m1)
  #coef(summary(m1))

  # by drug
  mlist <- lapply(unique(rdat1$drug), function(x) {
     m <- subgroup_escalc(dat=long2wide(wgl, index), index=index,
                          ap=x)
     #coef(summary(m))
  })
  dd <- rma_bydrug(mlist)
  sdd <- dd
  crit <- qnorm(0.05/2, lower.tail = FALSE)
  sdd$ci.lb <- exp(dd$estimate - crit * dd$se)  
  sdd$ci.ub <- exp(dd$estimate + crit * dd$se)  
  sdd$estimate <- exp(dd$estimate)
  sdd$drug <- unique(rdat1$drug)
  #sdd$n <- r

  ## # transform values back to relative scale through exponentiation
  srdat1 <- summary(rdat1, trans = exp, digits = 2)

  # calculate the number (%) of studies that had VR < 1
  vr_count[i] <- sum(aggregate(srdat1[ , "zi"], list(srdat1$id), mean) < 1)

  # Fig. 1 Forest plot VR
  # ----------------------------------------------------------------------
  srdat1$n     <- srdat1$n.active + srdat1$n.control
  srdat1$study <- attr(m1$yi, "slab")

  # get values from model
  fdd         <- df_from_rma(srdat1, m1, lblim=0.1,
                             ublim=ylims[[i]][2]*0.8) 

  # add pval to fdd
  fdd$pval[1] <- summary(m1)$pval 

  # save total n
  ntot[i] <- fdd$n[1]
  ntottx[i] <- sum(rdat1$n.active)
  ntotct[i] <- sum(rdat1$n.control)

  # save total comparisons
  ncomp[i] <- sum(rdat1$ncomp)

  # save vrstat
  vrstat[[i]] <- fdd[1, ]

  # save no of rcts
  nrct[i] <- nrow(srdat1) 

  # plot forest
  fpl         <- gg_forest(fdd = fdd, 
                           ylim = ylims[[i]],
                           greater1 = greater1s[i],
                           greater2 = greater2s[i],
                           ylab = "Study",
                           xlab = "Variability ratio (VR)",
                           cilab = "VR [95% CI]")

  plot(fpl[[1]])

  # save plot
  fn <- paste0("../output/figures/", index, "_fig1.pdf")
  cat(paste0("saving figure ", fn, " to disk ..."))
  ggsave(plot=fpl[[1]],
         filename=fn,
         width = widths[i], height = heights[i])


  # Model 2: VR model over treatment (by antipsychotics)
  # ----------------------------------------------------------------------
  # aggregate data over antipsychotics
  rdat1 <- escalc(measure="VR",
                  n1i=n.active,
                  n2i=n.control,
                  sd1i=sd.active,
                  sd2i=sd.control,
                  data=long2wide(wgl, index))
  rdat1p <- rdat1 %>%
    gather(key=tx, value=sd.active, sd.control)

  # calculate pooled sds for active and control
  datm <- rdat1 %>%
    group_by(drug) %>%
    dplyr::summarize(ntxm = sum(n.active),
                     nctm = sum(n.control),
                     sd1txm = sqrt(sum(sd.active^2 * (n.active -1))/
                                   (ntxm-length(n.active))),
                     sd1ctm = sqrt(sum(sd.control^2 * (n.control -1))/
                                   (nctm-length(n.control))))

  # calculate VR  
  rdat2 <- escalc(measure = "VR", 
                n1i = ntxm, sd1i = sd1txm, 
                n2i = nctm, sd2i = sd1ctm, 
                data = datm)

  # Fit random-effects models
  m2    <- rma(yi = yi, vi = vi, data = rdat2, method = "REML",
               slab = paste(rdat2$drug))

  #summary(m2)
  #coef(summary(m2)) 

  # transform values back to relative scale through exponentiation
  srdat2 <- summary(rdat2, trans = exp, digits = 2)



  # Fig. 2 Forest plot VR by medication
  # ----------------------------------------------------------------------
  # overall n
  srdat2$n     <- srdat2$ntxm + srdat2$nctm
  srdat2$study <- attr(m2$yi, "slab")

  # extract values from model
  fdd2         <- df_from_rma(srdat2, m2, ublim=ylims2[[i]][2]*0.8,
                              sumstat = TRUE)

  # save vrstat
  vrstat2[[i]] <- fdd2[1, ]

  # forest plot
  fpl <- gg_forest(fdd = fdd2, 
                   ylab = "Drug",
                   xlab = "Variability ratio (VR)",
                   cilab = "VR [95% CI]",
                   ylim = ylims2[[i]],
                   sumstat = TRUE)
  fpl[[1]]

  # save plot
  fn <- paste0("../output/figures/", index, "_fig2.pdf")
  cat(paste0("saving figure ", fn, " to disk ..."))
  ggsave(plot=fpl[[1]],
         filename=fn,
         width = widths2[i], height = heights2[i])

  cat(" done!", sep="\n")
}

