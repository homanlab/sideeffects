#
# sideeffects_func.R
#
# created on Tue Oct  1 10:09:18 2019
# Philipp Homan, <philipp dot homan at bli dot uzh dot ch>
#-----------------------------------------------------------------------
#
# common libraries
libs <- c(
 "tidyr",
 "dplyr",
 "devtools",
 "readr",
 "readxl",
 "sendmailR",
 "broom",
 "ggplot2",
 "tidyverse",
 "cowplot",
 "metafor",
 "represearch",
 "knitr",
 "rmarkdown",
 "papaja",
 "kableExtra"
)


if (!require("pacman")) install.packages("pacman")
library("pacman")
pacman::p_load(char=libs)



mail_authors <- function(mydata=NULL, send=FALSE) { 

  # emailing stuff
  # cave: works with original xls data (not cleaned)
  # --------------------------------------------------------------------
  # extract variables)
  corr_auth <- mydata$corr_auth
  corr_auth_email <- mydata$corr_auth_email
  full_study_name <- mydata$FullStudyName
  answer_after_1_mail <- mydata$answer_after_1_mail
  #missing_data <- mydata$missing_data
  #weight_gain_sd <- mydata$weight_gain_sd
  weight_gain_sd_origin <- mydata$SideeffectsSDOrigin
  #weight_gain_m <- mydata$weight_gain_m

  weight_gain_sd_origin <- ifelse(is.na(weight_gain_sd_origin), "",
                                  weight_gain_sd_origin)

  #corr_auth <- c("Philipp Homan",
  #               "Maria Neumeier")
  #
  #corr_auth_email <- c("philipp.homan@bli.uzh.ch",
  #                     "maria.neumeier@bli.uzh.ch")


  # assign mail data
  from <- "maria.neumeier@bli.uzh.ch"
  bcc <- "maria.neumeier@bli.uzh.ch"
  subject <- "Request for missing data from your RCT"
  mailControl=list(smtpServer="smtp.uzh.ch")


  # loop over corresponding authors
  for (i in 1:length(corr_auth)) {
  #for (i in 1:3) {
    #if (missing_data[i] == "yes") {
    #if (is.na(weight_gain_sd)) {
    if (!(weight_gain_sd_origin[i] == "original") &&
        is.na(answer_after_1_mail) &&
        !(is.na(corr_auth_email[i]))) {
      #cat(corr_auth)

      body1 <- paste0("Dear ", corr_auth[i], ", ")
      body2 <- paste(
        "",
        "",
        "As mentioned in a previous email from 4 weeks ago",
        "we are currently working on a meta analysis",
        "on side effects of antipsychotic treatment.",
        "We are using the data that was kindly shared with us",
        "by the group of Stefan Leucht, MD,",
        "from Munich (Huhn et al. 2019, Lancet).", 
        "For this analysis we would need weight gain, ",
        "prolactin, and QTc data at baseline and outcome ",
        "for treatment and placebo from your study:",
        #"is the standard deviation of ", missing_data,
        "",
        full_study_name[i],
        "",
        "We would very be grateful if you could share these data with us.",
        "",
        "I look forward to hearing from you.",
        "",
        "Sincerely,",
        "",
        "Maria Neumeier",
        "--",
        "Maria Neumeier",
        "Resident Physician",
        "University of Zurich",
        "Lenggstrasse 31",
        "8032 Zurich",
        "Switzerland",
        "maria.neumeier@bli.uzh.ch",
        "",
        "",
        sep="\n")
      #print(i)

      # merge greeting and body
      body <- paste0(body1, body2)

      cat(body)
      #cat(corr_auth_email[i])
      #cat("")
      #cat(i)
      #cat("")

      # send email
      if (send==TRUE) {
        s <- sendmail(from=from,
                      to=corr_auth_email[i],
                      bcc=bcc,
                      subject=subject,
                      msg=body,
                      control=mailControl)
      }
    }

  }
}


long2wide <- function (mydata, measure="weightsd") {
  mydatap <- mydata %>%
    dplyr::select(id, studynumber, index, treatment, val, n, drug) %>%
    filter(measure==index) %>%
    mutate(val=as.numeric(val))

  # remove studies without two arms
  mydatapc <- mydatap %>%
    group_by(studynumber) %>%
    dplyr::summarize(narms=sum(!is.na(id)))

  # filter out any remaining studies with just one arm
  if (sum(mydatapc$narms==1) > 0) {
    mydatapc2 <- mydatap %>%
      filter(studynumber != mydatapc$studynumber[mydatapc$narms==1])
  } else {
    mydatapc2 <- mydatap
  }
  
  
  #  group_by(id, index, treatment) %>%
  #  dplyr::summarize(
  #           narms=sum(!is.na(n)),
  #           sdp=sqrt(sum(sd^2)/narms))
  #           

  # retain treatments
  mydatadrugs <- mydatapc2 %>% dplyr::select(studynumber, drug)
  
  # spread data, active vs control for index measure
  mydatap2.1 <- mydatapc2[, c(-6, -7)] %>%
    spread(key=index, value=val) %>%
    spread(key=treatment, value=measure) %>%
    dplyr::rename(sd.active=active, sd.control=control)

  # spread data, active vs control for n 
  mydatap2.2 <- mydatapc2[, c(-5, -7)] %>%
    spread(key=index, value=n) %>%
    spread(key=treatment, value=measure) %>%
    dplyr::rename(n.active=active, n.control=control)

  # link the two tibbles
  mydatap2.12 <- bind_cols(mydatap2.1, mydatap2.2[, c(3, 4)]) %>%
    filter(!is.na(sd.active))

  # summarize individual studies
  #mydatap3 <- mydatap2.12 %>%
  #  group_by(studynumber) %>%
  #  dplyr::summarize(active_agg=sqrt(sum((n.active-1)*wsd.active^2+
  #                                       (n.active^2))/sum(n.active)))
  # aggregate multiple active arms
  mydatap3 <- mydatap2.12 %>%
    group_by(studynumber) %>%
    dplyr::summarize(
             ncomp=length(n.active),
             nactm=sum(n.active),
             sd.active=sqrt(sum(sd.active^2 * (n.active-1))/
                            (sum(n.active)-length(n.active)))
           )  
  
  # merge with mydata
  mydatap4 <- mydatap3 %>%
    left_join(mydatap2.12 %>% na.omit() %>% dplyr::select(-sd.active)) %>%
    na.omit()
  mydatap5 <- mydatap4[, c(1, 5, 4, 6, 3, 8, 2)] %>%
    mutate(nactm=nactm) %>%
    dplyr::rename(n.active=nactm) %>%
    left_join(mydatadrugs %>%
              filter(!drug=="Placebo") %>%
              distinct(studynumber, .keep_all=TRUE))

  return(mydatap5)

  # retain treatments
  #mydatadrugs <- mydatap %>% dplyr::select(id, drug)

  #mydataw1 <- spread(mydatap %>% filter(index==measure) %>%
  #                   dplyr::select(id, index, treatment, val),
  #                   key=index, value=val)
  #mydataw1a <- spread(mydatap %>% filter(index==measure) %>%
  #                   dplyr::select(id, index, treatment, n),
  #                   key=index, value=n) %>%
  #  rename(n=measure)
  #mydataw1w <- spread(mydataw1, treatment, measure) %>%
  #  filter(!is.na(active), !is.na(control))
  #mydataw1aw <- spread(mydataw1a, treatment, n) %>%
  #  filter(!is.na(active), !is.na(control)) %>%
  #  rename(n.active=active, n.control=control)

  #mydataw1wb <- left_join(mydataw1w, mydataw1aw) %>%
  #  left_join(mydatadrugs) %>%
  #  filter(!drug=="Placebo")

  #return(mydataw1wb)

  #return(list("weight"=mydataw1w,
  #            "qtc"=mydataw2w,
  #            "prolactin"=mydataw3w))

}

long2longer <- function (mydata) {

  mydatal <- mydata %>%
    gather(key=index, value=val, weightsd, qtcsd,
           prolactinsd, weightm, qtcm, prolactinm,
           weightn, qtcn, prolactinn)
  return(mydatal)
}

prepareindex <- function (mydata, index) {
  # prepare data frame for specific index for vr
  # calculation
  mydatap <- mydata %>% dplyr::select(id, year, contains(index))
  return(mydatap)
}

addspecifier <- function (mydata) {
  # add a specifier tracking how many arms a study has
  mydata$newid <- paste0(mydata$id, "_", mydata$drug)
  return(mydata)
}


  


calcvr <- function (mydata, measure="VR") {
  # calculate vr
  # variable of interest should be y (y1, y2)
  # dataframe must be in wide format

  # calculate VR
  rdat <- escalc(measure = "VR", 
                m1i = mu1tx, n1i = ntx, sd1i = sd1tx, 
                m2i = mu1ct, n2i = nct, sd2i = sd1ct, 
                data = mydata)


  # fit random-effects models
  m1    <- rma(yi = yi, vi = vi, data = rdat, method = "REML",
               slab = paste(df$id), weighted = TRUE)
  #summary(m1)
  #coef(summary(m1))
  return(m1)
}

# Forest plot with ggplot
#-----------------------------------------------------------------------
gg_forest <- function(fdd, 
                      xlab = "Coefficient of variation ratio",
                      ylab = "Study",
                      greater1 = "Greater in control",
                      greater2 = "Greater in treatment",
                      cilab = "CVR [95% CI]",
                      ylim = c(0, 2.7),
                      sumstat = TRUE) {
  #
  # forest plot with ggplot2

  # create two shapes for polygon of summary es
  fr <- fdd[1, ]
  s1 <- data.frame(x = c(1, 0.5, 0.5, 1),
                   y = c(fr$ci.lb, fr$es, fr$es,
                       fr$ci.ub),
                   row = "lower")
  s2 <- data.frame(x = c(1, 1.5, 1.5, 1),
                   y = c(fr$ci.lb, fr$es, fr$es,
                       fr$ci.ub),
                   row = "upper")
  s <- bind_rows(s1, s2)
  
  fp1 <- fdd %>% 
    ggplot(aes(x = study2, y = es)) +
    geom_errorbar(aes(ymax = ci.ub.c, ymin = ci.lb.c,
                width = 0.0, col = type), size = 0.4) +
    geom_point(aes(shape = type, size = nsize, col = type)) +
    scale_shape_manual(values = c(19, 18)) +
    scale_color_manual(values = c("black", "white")) +
    scale_size(range = c(1, 2)) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "",
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      legend.title = element_blank(),
      panel.grid = element_blank(),
      #axis.line.x = element_line(),
      #axis.ticks.x = element_line(),
      axis.title.x = element_text(face="bold"),
      axis.title.y = element_text(face="bold"),
      axis.text = element_text(color="black"),
      axis.text.y = element_text(size = 8, color="black")
    ) +
   ylim(ylim) +
   xlab(ylab) + 
   ylab(xlab)  
   #scale_y_continuous(breaks = c(0, 1, 2), limits = c(0, 2), 
    #expand = c(0, 0)) +

  if (sumstat == TRUE) {
    fp2 <- fp1 + geom_point(data = s, aes(x = x, y = y),
                            size = 0.01, col = "white") +
      geom_polygon(data = s, aes(x = x, y = y))
  } else {
    fp2 <- fp1
  }
  

#th <- textGrob(idheader, gp=gpar(fontsize=11, fontface="bold"))
#tl <- textGrob("Variability ratio (VR)", gp=gpar(fontsize=11,
#                                              fontface="bold"))

# annotation
fp3 <- fp2 + 
  coord_flip(clip = "off") +
  geom_hline(aes(yintercept = 1), lty = 2, 
             size = 0.5, col = "black") + 
  annotate("text", y = ylim[2], x = nrow(fdd) + 1.5, 
           label = paste0(cilab), hjust = 1, fontface = "bold") +
  annotate("text", y = ylim[2], x = 1:nrow(fdd), hjust = 1,
           label = paste0(
                       sprintf("%.2f", round(fdd$es, 2)),
                        " [",
                       sprintf("%.2f", round(fdd$ci.lb, 2)),
                       ", ",
                       sprintf("%.2f", round(fdd$ci.ub, 2)),
                       "]"), size = 2.5) +
  annotate("text", y = 0.85, x = nrow(fdd) + 1.5, 
           label = greater1, hjust = 1, 
           fontface = "bold")  +
  annotate("text", y = 1.15, x = nrow(fdd) + 1.5, 
           label = greater2,
           hjust = 0, fontface = "bold")  +
  annotate("text", y = 0.05, x = nrow(fdd) + 1.5,
           label = "N", hjust = 1, fontface = "bold") +
  annotate("text", y = 0.05, x = 1:nrow(fdd), label = paste0(fdd$n), 
           hjust = 1, size = 2.5) 
  #annotation_custom(th, xmin=nrow(fdd)+1.5, xmax=nrow(fdd)+1.5,
  #                  ymin=-0.25, ymax=-0.25) 
  #annotation_custom(tl, xmin=-2.0, xmax=-2.0,
  #                ymin=1, ymax=1)

  if (sum(!is.na(fdd$ci.ub.a)) > 0) {
    fp3 <- fp3 +
      geom_segment(aes(x = study2, xend = study2, y = es,
                       yend = ci.ub.a), size = 0.4,
                 arrow = arrow(length = unit(0.01, "npc"), 
                               ends = "last", type = "closed"))
  }

  if (sum(!is.na(fdd$ci.lb.a)) > 0) {
    fp3 <- fp3 +
      geom_segment(aes(x = study2, xend = study2, y = es, 
                       yend = ci.lb.a),
                 arrow = arrow(length = unit(0.01, "npc"), 
                               ends = "last", type = "closed"))
  }
  
  #plot(fp2)
  return(list(fp3))
}

# Variability ratio (VR): extract results
#-----------------------------------------------------------------------
df_from_rma <- function(dat, 
                        rmamod, 
                        lblim = 0.2, 
                        ublim = 2,
                        refoffset = 16, 
                        addrefs = FALSE,
                        sumstat = TRUE) {
  #
  # extract values from rma data

  # create data frame for individual studies
  fd <- data.frame(es = round(dat$yi, 2),
               se = dat$sei,
               ci.lb = round(dat$ci.lb, 2),
               ci.ub = round(dat$ci.ub, 2),
               n = dat$n,
               nsize = dat$n,
               type = "study",
               #study = attr(rmamod$yi, "slab"))
               study = dat$study)

  # inverse ranking according to effect sizes
  idx     <- (sort(fd$es, index.return = TRUE, 
                   decreasing = TRUE)$ix)
  fd      <- fd[(sort(fd$es, index.return = TRUE, 
                      decreasing = TRUE)$ix),]
  fd$rank <- 2:(nrow(dat) + 1)

  # add reference if needed
  if (addrefs == TRUE) {
    fd$refno <- rev(1:nrow(dat)) + refoffset
    fd$studywithref <- paste(fd$study, " (", fd$refno, ")", sep="")
  } else {
    fd$studywithref <- fd$study
  }
 
  pmod <- predict(rmamod, trans = exp, digits = 2)

  fd2 <- data.frame(es = round(pmod$pred, 2),
                 se = NA,
                 ci.lb = round(pmod$ci.lb, 2),
                 ci.ub = round(pmod$ci.ub, 2),
                 type = "Summary",
                 study = "Summary",
                 studywithref = "Summary",
                 rank = 1,
                 n = sum(fd$n),
                 nsize = mean(fd$n)) 

  if (sumstat == TRUE) {
    fdd <- bind_rows(fd2, fd)
  } else {
    fdd <- fd
  }

  fdd$study2 <- factor((fdd$studywithref[fdd$rank]),
                      levels = (fdd$studywithref[fdd$rank]),
                      ordered = TRUE)

  # ci limits and arrows stuff
  fdd$ci.lb.c <- ifelse(fdd$ci.lb < lblim, fdd$es, fdd$ci.lb) 
  fdd$ci.ub.c <- ifelse(fdd$ci.ub > ublim, fdd$es, fdd$ci.ub) 
  fdd$ci.lb.a <- ifelse(fdd$ci.lb < lblim, lblim, NA) 
  fdd$ci.ub.a <- ifelse(fdd$ci.ub > ublim, ublim, NA) 

  return(fdd)
}

parse_vrstat <- function(dat) {
  # create a nice text output for a vr statistic
  str <- paste0("VR = ", dat$es, "; ",
                "95% CI: ", dat$ci.lb, " - ", dat$ci.ub, "; ",
                "P ", parse_pval(dat$pval)
                )
  return(str)
}

subgroup_escalc <- function(dat, index="weightsd",
                            ap="Risperidone") {
  datp <- dat %>%
   filter(drug == ap)
  rdat <- escalc(measure="VR",
                 n1i=n.active,
                 n2i=n.control,
                 sd1i=sd.active,
                 sd2i=sd.control,
                 data=datp)
  
  m    <- rma(yi = yi, vi = vi, data = rdat, method = "REML",
               slab = paste(rdat$id), weighted = TRUE)
  #sm <- as.data.frame((m))
  #sm$n <- nrow(rdat)
  sm <- (coef(summary(m)))
  sm$n <- sum(datp$n.active) + sum(datp$n.control)
  #print(sm)
  return(sm)
}

rma_bydrug <- function(mlist) {
  dat <- data.frame(matrix(NA, ncol=length(names(mlist[[1]]))))
  colnames(dat) <- names(mlist[[1]])
  for (i in 1:length(mlist)) {
    rownames(mlist[[i]]) <- ""
    dat[i, ] <- mlist[[i]]
  }
  return(dat)
}


calc_sd_missing <- function(index="weightsd") {
  indexc <- strtrim(index, nchar(index)-2)

  # build file name and restrict to control arms
  dat <- read_csv(paste0("../data/",
                         "sideeffects_",
                         indexc,
                         "_excl.csv")) %>%
    filter(drug=="Placebo") %>%
    # this is need as there are duplicate control
    # arms in the data
    distinct(.keep_all=TRUE)
  sdmissing <- sum(!is.na(dat[, paste0(indexc, "m")]))
  sdmissingperc <- sdmissing/nrow(dat)
  return(sdmissingperc)
}




