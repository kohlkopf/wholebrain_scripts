# script for Qiangs FU brain, run on Windows with the RStudio viewer pane as the graphics device.

# It is necessary to interact wtih this script
wd <- "D:/qiq/2019-01-07_MO_PEA_IAMM_MIX/Flipped/auto_trials/FU"
setwd(wd)
library(wholebrain)

# get listing
images <- get.images(paste0(wd, "/images"))

#set spacing between periods in millimeters
smp<-0.06

# Next pick several images far apart from eachother and manually inspect them for what coordinate they are at by comparing to
# http:/mouse.brain-map.org/experiment/siv/?imageId=102162070
# begin when the frontal cortex comes in to view

inspected <- c(47, 66, 84, 101, 133)

#inspected <- c(length(images)-10:length(images))

#for(i in 1:length(inspected)){imshow(images[inspected[i]])}

#assign brain coordinates for inspected sections.
smp.coord<-c(3.145, 1.345, 0.145, -0.955, -2.98)

#now we can just generate all the intermediate coordinates automatically with the map.to.atlas() function
#assign brain coordinates for all sections in this brain.
rm(coord)
coord <- map.to.atlas(image.number=inspected, 
                      coordinate=smp.coord, 
                      sampling.period=smp, 
                      number.of.sections=length(images)
)

#plot the coords to ensure nothing is way off
plot(coord[0:155])

#use predefined/consistent filters for this set of brains
load(file= "../../saved_filter/initial_filters_qiang.RData")
brain_seg$filter$resize<-0.08
brain_seg$filter$Max<-20000




# Where did you leave off?

#load_index <- 112
#next_index <- load_index+1

#datasets <- NULL
#load(file=paste0(wd, '/datasets/', tools::file_path_sans_ext(basename(images[load_index])), 'accum_dataset.RData'))

#nrow(table(datasets$image))

dev.off()

#begin with image 37
#coord[42] <- coord[41] #bregma 2.705 atlas was busted
for(i in 47:137){
    seg<-segment(images[i], display=FALSE, filter = neuron_seg$filter, channel=2)
    
    regi<-registration(images[i], coordinate=coord[i], filter = brain_seg$filter, display=TRUE, channel=0, batch.mode = TRUE)
    #re-register with added/removed points
    dev.off()
    
    if(!dir.exists(file.path(wd, "/registrations/"))){dir.create(file.path(wd, "/registrations/"), showWarnings = FALSE)}
    dataset<-inspect.registration(regi, seg, soma = TRUE, forward.warps = TRUE, batch.mode = TRUE)
    dev.copy(pdf, paste0(wd, '/registrations/',tools::file_path_sans_ext(basename(images[i])), '_registrations', '.pdf'), width=18, height=8)
    dev.off()
    
    #for the first case, initialize datasets
    if(!exists("datasets")){datasets <- dataset}
    datasets<-rbind(datasets, dataset)
    
    #create directory, save progress
    if(!dir.exists(file.path(wd, "/session_data/"))){dir.create(file.path(wd, "/session_data/"), showWarnings = FALSE)}
    save(file=paste0(wd, '/session_data/', tools::file_path_sans_ext(basename(images[i])),'_session_data', '.RData'), seg, regi, dataset)
    
    #create directory, save the registration filter
    if(!dir.exists(file.path(wd, "/specific_reg/"))){dir.create(file.path(wd, "/specific_reg/"), showWarnings = FALSE)}
    save(file=paste0(wd, '/specific_reg/', tools::file_path_sans_ext(basename(images[i])), '_registration_info.RData'), regi)
    
    #create directory, save the datasets df
    if(!dir.exists(file.path(wd, "/datasets/"))){dir.create(file.path(wd, "/datasets/"), showWarnings = FALSE)}
    save(file=paste0(wd, '/datasets/', tools::file_path_sans_ext(basename(images[i])), '_accum_dataset.RData'), datasets)
    dev.off()
}


#remove neurons from datasets
#datasets<-datasets[!(datasets$image %in% tools::file_path_sans_ext(basename(images[47]))), ]

#---
#---
#---
#---
#---
#---
#---
#---
#---
#---
#---
#---
#---
#---
#---
#---
# Everything down here is for visualization and data summary. The datasets object should have all of the segmentation information.

datasets_accum <- datasets

datasets_accum$animal <- "FU_auto"
save(file=paste0(wd, "/datasets/auto_fu_datasets_accum.Robj"), datasets_accum)
load(file=paste0(wd, "/datasets/auo_fu_datasets_accum.Robj"))
write.table(file=paste0(wd, "/datasets/auto_fu_datasets_accum.Robj"), datasets_accum, sep = ",")


#make schematic plot
for(i in 50:70){schematic.plot(dataset=datasets[datasets$image %in% tools::file_path_sans_ext(basename(images[i])),])}

#get regions sorted according to intensity
mean.intensity<-tapply(datasets$intensity, datasets$acronym, mean)
#sort them
mean.intensity<-sort(mean.intensity, decreasing= TRUE)
#print a table with acronym and name
outputdata<-data.frame(parent.name = name.from.acronym(get.acronym.parent(names(mean.intensity))), parent.acronym =  get.acronym.parent(names(mean.intensity)), name = name.from.acronym(names(mean.intensity)), acronym = names(mean.intensity), intensity = mean.intensity)
#write to spreadsheet file
write.csv(outputdata, file = "radioactivity_sorted_by_region.csv", row.names=FALSE)

#make a webmap into current project folder
if(!dir.exists(file.path(wd, "webmap"))){dir.create(file.path(wd, "webmap"), showWarnings = FALSE)}
pixel.resolution<-1 #1 micron
makewebmap(img = images[69], filter = neuron_seg$filter, registration = regi, dataset = dataset, scale = 0.64, fluorophore = "c-Fos", folder.name = "webmap/")
