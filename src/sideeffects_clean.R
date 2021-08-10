#
# sideeffects_clean.R
#
# created on Tue Oct  1 10:09:18 2019
# Maria Neumeier, <maria dot neumeier at bli dot uzh dot ch>
#-----------------------------------------------------------------------
source("sideeffects_load.R")

mydata <- read_excel("../data/sideeffects_corrected.xlsx")

# save cases that will be excluded (missing data)
#mydataex <- mydata %>%
#  filter(!grepl("SDOrigin",) %in% c("original", "p-value"))

# remove unwanted columns (#1, #23)
mydata <- mydata[, -c(1, 23)]

# what data to include
mydataincl <- mydata%>%
  filter(

    (SideeffectsSDOrigin %in% c("original","p-value") &
    !is.na(WeightContSD)) | 

    (ProlactinSDOrigin %in% c("original","p-value") &
    !is.na(ProlactinSD)) | 

    (QTcSDOrigin %in% c("original","p-value") &
    !is.na(QTcSD))  

  )


# get total number of trials in original database
mydataexcl <- mydata %>%
    filter(

    (!is.na(WeightContM) &
     is.na(WeightContSD) |
     grepl("estimated", SideeffectsSDOrigin)) |

    (!is.na(ProlactinM) &
     is.na(ProlactinSD) |
     grepl("estimated", ProlactinSDOrigin)) |

    (!is.na(QTcM) &
     is.na(QTcSD) |
     grepl("estimated", QTcSDOrigin)) 

    )
  
mydataexcl <- mydata %>%
  filter(!Arm_ID %in% mydataincl$Arm_ID)
write_csv(x=mydataexcl, path="../data/sideeffects_excl.csv", na="")

mydata <- mydata%>%
  filter(Arm_ID %in% mydataincl$Arm_ID)

# prolactin
mydatap <- mydata %>%
  dplyr::rename(prolactinsd=ProlactinSD,
                prolactinm=ProlactinM,
                prolactinn=ProlactinN,
                prolactinsdorigin=ProlactinSDOrigin,
                prolactinextraction=ProlactinChangeEndpoint,
                studynumber=Study_No,
                id=Study_ID,
                drug=Drug,
                arm=Arm_ID,
                year=Year,
                n=No_randomised) %>%
  mutate(treatment=ifelse(drug=="Placebo", "control", "active")) %>%
  dplyr::select(-matches("Weight|QTc"))

mydatapex <- mydatap %>% 
  filter(!prolactinsdorigin %in% c("original", "p-value")) %>%
  dplyr::select(studynumber, id, arm, year, drug, n,
                prolactinm, prolactinsd, prolactinn)

# qtc
mydataq <- mydata %>%
  dplyr::rename(qtcsd=QTcSD,
                qtcm=QTcM,
                qtcn=QTcN,
                qtcsdorigin=QTcSDOrigin,
                qtcextraction=QTcChangeEndpoint,
                studynumber=Study_No,
                id=Study_ID,
                drug=Drug,
                arm=Arm_ID,
                year=Year,
                n=No_randomised) %>%
  mutate(treatment=ifelse(drug=="Placebo", "control", "active")) %>%
  dplyr::select(-matches("Prolactin|Weight"))

mydataqex <- mydataq %>% 
  filter(!qtcsdorigin %in% c("original", "p-value")) %>%
  dplyr::select(studynumber, id, arm, year, drug, n,
                qtcm, qtcsd, qtcn)

# weight gain
# restrict to complete cases (SideeffectsSDOrigin == "original")
# and rename variables
mydataw <- mydata %>%
  dplyr::rename(weightsd=WeightContSD,
                weightm=WeightContM,
                weightn=WeightContN,
                weightsdorigin=SideeffectsSDOrigin,
                weightextraction=WeightContChangeEndpoint,
                studynumber=Study_No,
                id=Study_ID,
                drug=Drug,
                arm=Arm_ID,
                year=Year,
                n=No_randomised) %>%
  mutate(treatment=ifelse(drug=="Placebo", "control", "active")) %>%
  dplyr::select(-matches("Prolactin|QTc"))

mydatawex <- mydataw %>% 
  filter(!weightsdorigin %in% c("original", "p-value")) %>%
  dplyr::select(studynumber, id, arm, year, drug, n,
                weightm, weightsd, weightn)

# remove duplicate arms
mydatapnd <- mydatap %>% distinct(arm, .keep_all=TRUE)
mydataqnd <- mydataq %>% distinct(arm, .keep_all=TRUE)
mydatawnd <- mydataw %>% distinct(arm, .keep_all=TRUE)

# save distinct cleaned data to disk
write_csv(mydatapnd, path="../data/sideeffects_prolactin.csv", na="")
write_csv(mydatapex, path="../data/sideeffects_prolactin_excl.csv", na="")
write_csv(mydataqnd, path="../data/sideeffects_qtc.csv", na="")
write_csv(mydataqex, path="../data/sideeffects_qtc_excl.csv", na="")
write_csv(mydatawnd, path="../data/sideeffects_weight.csv", na="")
write_csv(mydatawex, path="../data/sideeffects_weight_excl.csv", na="")

# create one single data tibble
mydatapndl <- mydatapnd %>%
  gather(key=index, value=val,
         prolactinm, prolactinsd, prolactinn, prolactinsdorigin,
         prolactinextraction)

mydataqndl <- mydataqnd %>%
  gather(key=index, value=val,
         qtcm, qtcsd, qtcn, qtcsdorigin,
         qtcextraction)

mydatawndl <- mydatawnd %>%
  gather(key=index, value=val,
         weightm, weightsd, weightn, weightsdorigin,
         weightextraction)

write_csv(bind_rows(mydatapndl, mydataqndl, mydatawndl),
          path="../data/sideeffects_all.csv", na="")


# mail 2 authors for missing data
# mydata <- read_excel("../data/sideeffects_corrected.xlsx")
# mail_authors(mydata=mydata)
