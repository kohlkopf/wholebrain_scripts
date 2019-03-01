# run this script after registrations have been determined

wd <- "D:/qiq/2019-01-07_MO_PEA_IAMM_MIX/Flipped/PEA+IAMM/"
setwd(wd)
library(wholebrain)
library(naturalsort)
library(stringr)
library(dplyr)

images <- get.images(x = paste0(wd, "images"))

#set spacing between periods in millimeters
smp<-0.06

# Next pick several images far apart from eachother and manually inspect them for what coordinate they are at by comparing to
# http:/mouse.brain-map.org/experiment/siv/?imageId=102162070
# begin when the frontal cortex comes in to view

inspected <- c(40, 60, 80, 120, 135)


#assign brain coordinates for inspected sections.
smp.coord<-c(2.845, 1.345, 0.020, -2.255, -3.280)

#now we can just generate all the intermediate coordinates automatically with the map.to.atlas() function
#assign brain coordinates for all sections in this brain.
coord <- map.to.atlas(image.number=inspected, 
                      coordinate=smp.coord, 
                      sampling.period=smp, 
                      number.of.sections=length(images)
)

#use Qiang's filters
load(file=paste0(wd, "/saved_filters/initial_filters.RData"))
brain_seg$filter$resize<-0.08
#or 12499
brain_seg$filter$Max<-25000


dev.off()

file_list <- naturalsort(list.files(paste0(wd, "specific_reg"), full.names = TRUE))
#suffices <- c(sprintf("%0.2d", 1:99),sprintf("%0.3d", 100:300))
suffices <- NULL
for(registration in file_list){
  load(registration)
  assign(paste0("regi", str_extract(registration, "_[0-9]+")), regi)
  suffices <- c(suffices, str_extract(registration, "_[0-9]+"))
}


for(i in 37:143){
  seg<-segment(images[i], display=FALSE, filter = neuron_seg$filter, channel=2)
  # no need to compute forward warps agaon
  # loads image in output* folders, need to be in working directory
  output <- inspect.registration(get(paste0("regi", suffices[i-36])), seg, soma = TRUE, forward.warps = FALSE, device=FALSE, batch.mode = TRUE)
  assign(paste0("dataset", suffices[i-36]), output)
  
}

#74 is missing
for(i in 37:142){print(paste0("dataset", suffices[i-36]))}

#bind em all
bound <- bind_rows(mget(paste0("dataset_", c(37:73,75:143))), .id = "column_label")
bound$animal <- "PEA+IAMM"

#save final table
if(!dir.exists(file.path(wd, "dataset_post_registration/"))){dir.create(file.path(wd, "dataset_post_registration/"), showWarnings = FALSE)}
write.table(file = file.path(wd, "dataset_post_registration/pea_iamm_dataset.csv"), bound, sep = ",")


