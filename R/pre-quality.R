# Script to format data before quality assessment

#---------- SETUP ----------#

args = commandArgs(trailingOnly=TRUE)
landing_path <- args[1]
input_path <- args[2]
library(managedatbiomelab)
dat <- read_dat(landing_path, "csv")


#---------- FORMAT ----------#

cols_rm <- c("stations",
             "creusage_method")

dat <- dat[,-cols_rm]

dat <- dat[,c("material_sample_id_biodiv",
              "decimal_longitude",
              "decimal_latitude",
              "especes",
              "sampling_effort_m2",
              "abondance",
              "biomasse")]

colnames(dat)[colnames(dat) == "biomasse"] <- "biomasse_g"


#---------- SAVE ----------#

write.csv(dat, input_path, row.names = FALSE)
