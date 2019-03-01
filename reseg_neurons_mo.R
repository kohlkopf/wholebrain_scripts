# run this script after registrations have been completed for all sections

library(wholebrain)
library(naturalsort)
library(stringr)
library(dplyr)


#set the wd, should be at the level where all of the output*** files are
wd <- "U:/qiq/2019-01-07_MO_PEA_IAMM_MIX/Processed/MO/"
setwd(wd)

images <- get.images(x = paste0(wd, "."))

#use Qiang's filters
load(file=paste0(wd, "/saved_filters/initial_filters.RData"))

file_list <- naturalsort(list.files(paste0(wd, "specific_reg"), full.names = TRUE))
suffixes <- NULL
for(registration in file_list){
  load(registration)
  suffix <- as.numeric(as.character(str_match(registration, "_([0-9]+)_")[2]))
  assign(paste0("regi_", suffix), regi)
  suffixes <- c(suffixes, as.numeric(suffix))
}


for(suffix in suffixes){
  seg<-segment(images[suffix], display=FALSE, filter = neuron_seg$filter, channel=2)
  # no need to compute forward warps again, ignore warning
  # loads image in output*** folders, need to be in working directory
  output <- inspect.registration(get(paste0("regi_", suffix)), seg, soma = TRUE, forward.warps = FALSE, device=FALSE, batch.mode = TRUE)
  assign(paste0("dataset_", suffix), output)
  dev.off()
}

#bind em all
bound <- bind_rows(mget(paste0("dataset_", suffixes)), .id = "column_label")
bound$animal <- "MO"

#save final table
if(!dir.exists(file.path(wd, "dataset_post_registration/"))){dir.create(file.path(wd, "dataset_post_registration/"), showWarnings = FALSE)}
write.table(file = file.path(wd, "dataset_post_registration/pea_iamm_dataset.csv"), bound, sep = ",")


