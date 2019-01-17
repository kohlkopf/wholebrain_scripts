# It is necessary to interact wtih this script
wd <- "~/120hrs/"
setwd(wd)
library(wholebrain)

# get listing, saved ls output with natural numerical sorting, produced with code below
# gls d -1 -v $PWD/*.* | vim - (gnu ls, natural sort)
images <- readLines('~/120hrs/local_osx_listing.txt')

#set spacing between periods in millimeters
smp<-0.06

# Next pick four different images far apart from eachother and manually inspect them for what coordinate they are at by comparing to www.openbrainmap.org
# http:/mouse.brain-map.org/experiment/siv/?imageId=102162070
# cant't use the first image! not robust, subtracts bregma due to creating a sequence
inspected <- c(2, 29, 61, 78, 84, 99, 118)


#lapply(X=inspected, function(x){makewebmap(images[x])})

#assign brain coordinates for inspected sections.
smp.coord<-c(5.245, 3.245, 1.345, 0.145, -0.18, -1.155, -2.48)

#now we can just generate all the intermediate coordinates automatically with the map.to.atlas() function
#assign brain coordinates for all sections in this brain.
coord <- map.to.atlas(image.number=inspected, 
                      coordinate=smp.coord, 
                      sampling.period=smp, 
                      number.of.sections=length(images)
)

#plot the coords to ensure nothing is way off
plot(coord[0:155])


#either develop filters on 33 or use old filters below
neuron_seg <- segment(images[77], channel=2)
brain_seg <- segment(images[78], channel=0)
save(neuron_seg, brain_seg, file="~/120hrs/saved_filters/filters.RData")

#use old filters
load("~/120hrs/saved_filters/filters.RData")
brain_seg$filter$resize<-0.08

#plate 3 is inaccessible, kludge to to use plates 2 and 4 and 5
#not a good solution, incorrect depth in glassbrain
coord[1]<-5.2
coord[2]<-5.2
coord[3]<-5.06
coord[4]<-4.855
coord[5]<-4.855

datasets <- NULL
load('~/120hrs/datasets/up_to_image_127_accumulated_dataset.RData')

nrow(table(datasets$image))

#only do images[55:127] due to low quality sections
coord[c(55,127)]
for(i in 129:length(images)){
  seg<-segment(images[i], display=FALSE, filter = neuron_seg$filter, channel=2)
  
  quartz(width=26.7, height=10)
  input.points=""
  regi<-registration(images[i], coordinate=coord[i], filter = brain_seg$filter, display=TRUE, channel=0)
  input.points<-readline(prompt="Input a vector of points to remove, enter if OK: ")
  if(!(input.points == "")){
    regi<-remove.corrpoints(regi, eval(parse(text=input.points)))}
  regi<-add.corrpoints(regi)
  #re-register with added/removed points
  regi<-registration(images[i], coordinate=coord[i], filter = brain_seg$filter, display=TRUE, channel=0, correspondance=regi)
  dev.off()
  
  if(!dir.exists(file.path(wd, "registrations"))){dir.create(file.path(wd, "registrations"), showWarnings = FALSE)}
  dataset<-inspect.registration(regi, seg, soma = TRUE, forward.warps = TRUE, batch.mode = TRUE)
  dev.copy(pdf, paste0(wd, 'registrations/edited_',tools::file_path_sans_ext(basename(images[i])), '.pdf'), width=18, height=8)
  dev.off()
  
  #for the first case, initialize datasets
  if(!exists("datasets")){datasets <- dataset}
  datasets<-rbind(datasets, dataset)
  
  #create directory, save progress
  if(!dir.exists(file.path(wd, "session_data"))){dir.create(file.path(wd, "session_data"), showWarnings = FALSE)}
  save(file=paste0(wd, 'session_data/', tools::file_path_sans_ext(basename(images[i])), '.RData'), seg, regi, dataset)
  
  #create directory, save the datasets df
  if(!dir.exists(file.path(wd, "datasets"))){dir.create(file.path(wd, "datasets"), showWarnings = FALSE)}
  save(file=paste0(wd, 'datasets/up_to_image_', i, '_accumulated_dataset.RData'), datasets)
  
  dev.off()
}

table(datasets$image)

colnames(datasets)

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

#after manually inspecting the registrations, I've chosen a subset to re-register
redo_index<-c(3, 4, 5, 24, 25, 26, 28, 32, 34, 35, 66)
images[redo_index]
#delete called neurons from given sections from datasets
datasets<-datasets[!(datasets$image %in% tools::file_path_sans_ext(basename(images[127:155]))), ]
nrow(datasets)

brain_seg$filter$resize<-0.1

for(i in redo_index){
  seg<-segment(images[i], display=FALSE, filter = neuron_seg$filter, channel=2)
  
  quartz(width=15, height=10)
  input.points=""
  regi<-registration(images[i], coordinate=coord[i], filter = brain_seg$filter, display=TRUE, channel=0)
  input.points<-readline(prompt="Input a vector of points to remove, enter if OK: ")
  if(!(input.points == "")){
  	regi<-remove.corrpoints(regi, eval(parse(text=input.points)))}
  regi<-add.corrpoints(regi)
  #re-register with added/removed points
  regi<-registration(images[i], coordinate=coord[i], filter = brain_seg$filter, display=TRUE, channel=0, correspondance=regi)
  dev.off()
  
  dataset<-inspect.registration(regi, seg, soma = TRUE, forward.warps = TRUE, batch.mode = TRUE)
  dev.copy(pdf, paste0('~/Desktop/registrations/edited_',tools::file_path_sans_ext(basename(images[i])), '.pdf'), width=18, height=8)
  dev.off()
  
  #for the first case, initialize datasets
  if(!exists("datasets")){datasets <- dataset}
  datasets<-rbind(datasets, dataset)
  
  #create directory, save progress
  if(!dir.exists(file.path(wd, "session_data"))){dir.create(file.path(wd, "session_data"), showWarnings = FALSE)}
  save(file=paste0(wd, 'session_data/', tools::file_path_sans_ext(basename(images[i])), '.RData'), seg, regi, dataset)
  
  #create directory, save the datasets df
  if(!dir.exists(file.path(wd, "datasets"))){dir.create(file.path(wd, "datasets"), showWarnings = FALSE)}
  save(file=paste0(wd, 'datasets/up_to_image_', i, '_accumulated_dataset.RData'), datasets)
  
  dev.off()
}

#save complete, edited datasets
save(file=paste0(wd, 'datasets/edited_full_accumulated_dataset.RData'), datasets)


#remove the first few sections, code issues in wholebrain
datasets_right$right.hemisphere<-TRUE

#3d brain, colored by region
glassbrain(datasets, cex=4, plane="coronal", laterality=FALSE, col=heat.colors(100)[as.numeric(cut(log2(datasets$intensity), breaks = 100))])
#colored by intensity
glassbrain(datasets, cex=4, plane="coronal", laterality=FALSE)
#plot specific slides to demonstrate wholebrain plotting willy nilly
#glassbrain(datasets[datasets$image %in% tools::file_path_sans_ext(basename(images[31:33])),], cex=4, plane="coronal")
glassbrain(datasets)

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
dot.plot(datasets[datasets$image %in% tools::file_path_sans_ext(basename(images[100])),])
dot.plot(datasets)

#make schematic plot
for(i in 100){schematic.plot(dataset=datasets[datasets$image %in% tools::file_path_sans_ext(basename(images[i])),])}

#get regions sorted according to intensity
mean.intensity<-tapply(datasets$intensity, datasets$acronym, mean)
#sort them
mean.intensity<-sort(mean.intensity, decreasing= TRUE)
#print a table with acronym and name
outputdata<-data.frame(parent.name = name.from.acronym(get.acronym.parent(names(mean.intensity))), parent.acronym =  get.acronym.parent(names(mean.intensity)), name = name.from.acronym(names(mean.intensity)), acronym = names(mean.intensity), intensity = mean.intensity)
#write to spreadsheet file
write.csv(outputdata, file = "radioactivity_sorted_by_region.csv", row.names=FALSE)

#make a webmap into current project folder
pixel.resolution<-1 #1 micron
makewebmap(img = images, filter = neuron$filter, registration = regi, dataset = dataset, scale = 0.64, fluorophore = "Radioactivity")
