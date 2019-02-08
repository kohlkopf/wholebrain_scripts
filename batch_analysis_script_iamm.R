# script for Qiangs PEA & IAMM brain, run on Windows with the RStudio viewer pane as the graphics device.

# It is necessary to interact wtih this script
wd <- "D:/qiq/2019-01-07_MO_PEA_IAMM_MIX/Flipped/IAMM"
setwd(wd)
library(wholebrain)

# get listing
images <- get.images(paste0(wd, "/images"))


#set spacing between periods in millimeters
smp<-0.06

# Next pick several images far apart from eachother and manually inspect them for what coordinate they are at by comparing to
# http:/mouse.brain-map.org/experiment/siv/?imageId=102162070
# begin when the frontal cortex comes in to view

inspected <- c(40, 64, 83, 105, 135)

for(i in 1:length(inspected)){imshow(images[inspected[i]])}

inspected <- c(length(images)-10:length(images))

for(i in 1:length(inspected)){imshow(images[inspected[i]])}

#lapply(X=inspected, function(x){makewebmap(images[x])})

imshow(images[135])

#assign brain coordinates for inspected sections.
smp.coord<-c(2.845, 1.345, 0.245, -0.955, -3.180)

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


#either develop filters on 33 or use old filters below
#neuron_seg <- segment(images[45], channel=2)
#neuron_seg$filter
#brain_seg <- segment(images[45], channel=0)
#brain_seg$filter$resize<-0.08
##brain_seg$filter$brain.threshold <- 112
if(!dir.exists(file.path(wd, "saved_filters"))){dir.create(file.path(wd, "saved_filters"), showWarnings = FALSE)}
#save(neuron_seg, brain_seg, file=paste0(wd, "/saved_filters/initial_filters.RData"))

#use old filters
load(file= "..//saved_filter/initial_filters_qiang.RData")
brain_seg$filter$resize<-0.08
#or 12499
brain_seg$filter$Max<-35000




# Where did you leave off?

load_index <- 72
next_index <- load_index+1

#datasets <- NULL
load(paste0(wd, '/datasets/up_to_image_', load_index, '_accumulated_dataset.RData'))

nrow(table(datasets$image))

dev.off()


i

#begin with image 37
#coord[42] <- coord[41] #bregma 2.705 atlas was busted
for(i in 124:length(images)){
  #xpos = 200, ypos = -50, width = 35, height = 15
    #x11()
    seg<-segment(images[i], display=FALSE, filter = neuron_seg$filter, channel=2)
    
    
    input.points=""
    regi<-registration(images[i], coordinate=coord[i], filter = brain_seg$filter, display=TRUE, channel=0, batch.mode = TRUE)
    input.points<-readline(prompt="Input a vector of points to remove, enter if OK: ")
    if(!(input.points == "")){regi<-remove.corrpoints(regi, eval(parse(text=input.points)))}
    regi<-add.corrpoints(regi)
    #re-register with added/removed points
    regi<-registration(images[i], coordinate=coord[i], filter = brain_seg$filter, display=TRUE, channel=0, correspondance=regi)
    dev.off()
    
    if(!dir.exists(file.path(wd, "/registrations/"))){dir.create(file.path(wd, "/registrations/"), showWarnings = FALSE)}
    dataset<-inspect.registration(regi, seg, soma = TRUE, forward.warps = TRUE, batch.mode = TRUE)
    dev.copy(pdf, paste0(wd, '/registrations/',tools::file_path_sans_ext(basename(images[i])), '.pdf'), width=18, height=8)
    dev.off()
    
    #for the first case, initialize datasets
    if(!exists("datasets")){datasets <- dataset}
    datasets<-rbind(datasets, dataset)
    
    #create directory, save progress
    if(!dir.exists(file.path(wd, "/session_data/"))){dir.create(file.path(wd, "/session_data/"), showWarnings = FALSE)}
    save(file=paste0(wd, '/session_data/', tools::file_path_sans_ext(basename(images[i])), '.RData'), seg, regi, dataset)
    
    #create directory, save the registration filter
    if(!dir.exists(file.path(wd, "/specific_reg/"))){dir.create(file.path(wd, "/specific_reg/"), showWarnings = FALSE)}
    save(file=paste0(wd, '/specific_reg/image_', i, '_registration_info.RData'), regi)
    
    #create directory, save the datasets df
    if(!dir.exists(file.path(wd, "/datasets/"))){dir.create(file.path(wd, "/datasets/"), showWarnings = FALSE)}
    save(file=paste0(wd, '/datasets/up_to_image_', i, '_accumulated_dataset.RData'), datasets)
    dev.off()
}


#remove neurons from datasets
datasets<-datasets[!(datasets$image %in% tools::file_path_sans_ext(basename(images[79:86]))), ]

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

# If you've been a bit sloppy and have partial datasets, below will accumulated all of the neurons and exclude duplicates
# It goes through each saved dataset and adds any new rows (based on the image name) it won't overwrite, so it takes the orig
# read in datasets backwards!

#load in all of the partial datasets, rename the object once loaded
i=1
suffices <- sprintf("%0.3d", 1:length(file_list))
for(item in file_list){
  load(item);
  assign(paste0("datasets_", suffices[i]), datasets);
  i<-i+1;
}

j=1
#loop through all of the loaded dataset dataframes
for(dataset in mget(sort(grep("^datasets_[0-9]+", ls(), value=TRUE), decreasing=TRUE))){
  #initialize datasets_accum dataframe
  if(j==1){datasets_accum <- dataset;j<-1;};
  #merge the datasets verticaly, subset non-accum df to prevent duplicates
  datasets_accum <- rbind(datasets_accum, dataset[!(dataset$image %in% datasets_accum$image),]);
  j<-j+1;
}

paste0(j, " datasets were merged.")
save(file=paste0(wd, "/datasets/datasets_accum.Robj"), datasets)




# Everything down here is for visualization and data summary. The datasets object should have all of the segmentation information.

datasets_accum <- datasets

datasets_accum$animal <- "IAMM"
save(file=paste0(wd, "/datasets/datasets_accum.Robj"), datasets)

#remove the first few sections, code issues in wholebrain
#datasets_right$right.hemisphere<-TRUE

glassbrain(datasets_accum, cex=4, plane="coronal")
#plot specific slides to demonstrate wholebrain plotting willy nilly
glassbrain(datasets[datasets$image %in% tools::file_path_sans_ext(basename(images[40:50])),], cex=4, plane="coronal")
coord[40:50]

glassbrain(datasets)
glassbrain(datasets[grep("Olfactory|olfactory", datasets$name),])

#draw midline plane, call after calling glassbrain
planes3d(0,0,1, col = 'lightblue', alpha = 0.9)

#choose specific perspective
perspective<-list(FOV = 30, 
                  ignoreExtent = FALSE, 
                  listeners = 1L, 
                  mouseMode = structure(c("trackball", "zoom", "fov", "pull"
                  ), .Names = c("left", "right", "middle", "wheel")), 
                  skipRedraw = FALSE, 
                  userMatrix = structure(c(0.620774269104004, 0.410500437021255, 
                                           -0.667928755283356, 0, -0.0296718962490559, -0.839048981666565, 
                                           -0.543246030807495, 0, -0.783427774906158, 0.357052087783813, 
                                           -0.508679509162903, 0, 0, 0, 0, 1), 
                                         .Dim = c(4L, 4L)), scale = c(1,  1, 1), 
                  viewport = structure(c(0L, 0L, 1280L, 720L), .Names = c("x", "y", "width", "height")), 
                  zoom = 1, windowRect = c(0L, 45L,  1280L, 765L), 
                  family = "sans", font = 1L, cex = 1, useFreeType = TRUE)
par3d(perspective)


#dot plot
dot.plot(datasets[datasets$image %in% tools::file_path_sans_ext(basename(images[28])),])
dot.plot(datasets)
quartz()
dot.plot(datasets[grep("Olfactory|olfactory", datasets$name),])

#make schematic plot
for(i in 29){schematic.plot(dataset=datasets[datasets$image %in% tools::file_path_sans_ext(basename(images[i])),])}

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
