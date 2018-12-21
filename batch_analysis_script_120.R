# It is necessary to interact wtih this script
setwd("~/Desktop")
library(wholebrain)

# get listing, saved ls output with natural numerical sorting, produced with code below
# gls d -1 -v $PWD/*.* | vim - (gnu ls, natural sort)
images <- readLines('~/Desktop/listing_brief.txt')

#set spacing between periods in millimeters
smp<-0.06

#Next pick four different images far apart from eachother and manually inspect them for what coordinate they are at by comparing to www.openbrainmap.org
#33:MTP-Cre-H129dTT-72hrs_06_Image.vsi.Collection_X-74.1_mmY-64.67_mm_Layer0.tif
#55:MTP-Cre-H129dTT-72hrs_10_Image.vsi.Collection_X-67.6_mmY-64.24_mm_Layer5.tif
#91:MTP-Cre-H129dTT-72hrs_16_Image.vsi.Collection_X-64.52_mmY-66.44_mm_Layer1.tif
#133:MTP-Cre-H129dTT-72hrs_23_Image.vsi.Collection_X-63.26_mmY-64.12_mm_Layer5.tif

inspected<-c(32, 55, 91, 133)
images[inspected]

lapply(x=inspected, function(x){makewebmap(images[x])})

#assign brain coordinates for inspected sections.
smp.coord<-c(3.245, 1.645, -0.555, -3.58)

#now we can just generate all the intermediate coordinates automatically with the map.to.atlas() function
#assign brain coordinates for all sections in this brain.
coord <- map.to.atlas(image.number=inspected, 
                    coordinate=smp.coord, 
                    sampling.period=smp, 
                    number.of.sections=length(images)
)

#either develop filters on 33 or use old filters below
#neurons<-segment(images[33], channel=2)
#brain<-segment(images[33], channel=0)

#use old filters
load("~/Desktop/other_output/filters_22.RData")
brain$filter$resize<-0.1

#plate 3 is inaccessible, kludge to to use plates 2 and 4 and 5
#not a good solution, incorrect depth in glassbrain
coord[1]<-5.2
coord[2]<-5.2
coord[3]<-5.06
coord[4]<-4.855
coord[5]<-4.855

load('~/Desktop/datasets/edited_full_accumulated_dataset.RData')
gc()

# for(i in seq_along(images)[-1]){
# for(i in seq_along(images)){
	
brain$filter$resize<-0.1
	
segment(images[32], filter = neurons$filter, channel=2)
	
for(i in 69:144){
  seg<-segment(images[i], display=FALSE, filter = neurons$filter, channel=2)
  
  quartz(width=15, height=10)
  input.points=""
  regi<-registration(images[i], coordinate=coord[i], filter = brain$filter, display=TRUE, channel=0)
  input.points<-readline(prompt="Input a vector of points to remove, enter if OK: ")
  if(!(input.points == "")){
  	regi<-remove.corrpoints(regi, eval(parse(text=input.points)))}
  regi<-add.corrpoints(regi)
  #re-register with added/removed points
  regi<-registration(images[i], coordinate=coord[i], filter = brain$filter, display=TRUE, channel=0, correspondance=regi)
  dev.off()
  
  dataset<-inspect.registration(regi, seg, soma = TRUE, forward.warps = TRUE, batch.mode = TRUE)
  dev.copy(pdf, paste0('~/Desktop/registrations/',tools::file_path_sans_ext(basename(images[i])), '.pdf'), width=18, height=8)
  dev.off()
  
  datasets<-rbind(datasets, dataset)
  save(file=paste0('~/Desktop/session_data/', tools::file_path_sans_ext(basename(images[i])), '.RData'), seg, regi, dataset)
  save(file=paste0('~/Desktop/datasets/image_', i, '_accumulated_dataset.RData'), datasets)
  
  dev.off()
}

#after manually inspecting the registrations, I've chosen a subset to re-register
redo_index<-c(3, 4, 5, 24, 25, 26, 28, 32, 34, 35, 66)
images[redo_index]
#delete called neurons from given sections from datasets
datasets<-datasets[!(datasets$image %in% tools::file_path_sans_ext(basename(images[redo_index]))), ]

brain$filter$resize<-0.12

for(i in redo_index){
  seg<-segment(images[i], display=FALSE, filter = neurons$filter, channel=2)
  
  quartz(width=15, height=10)
  input.points=""
  regi<-registration(images[i], coordinate=coord[i], filter = brain$filter, display=TRUE, channel=0)
  input.points<-readline(prompt="Input a vector of points to remove, enter if OK: ")
  if(!(input.points == "")){
  	regi<-remove.corrpoints(regi, eval(parse(text=input.points)))}
  regi<-add.corrpoints(regi)
  #re-register with added/removed points
  regi<-registration(images[i], coordinate=coord[i], filter = brain$filter, display=TRUE, channel=0, correspondance=regi)
  dev.off()
  
  dataset<-inspect.registration(regi, seg, soma = TRUE, forward.warps = TRUE, batch.mode = TRUE)
  dev.copy(pdf, paste0('~/Desktop/registrations/edited_',tools::file_path_sans_ext(basename(images[i])), '.pdf'), width=18, height=8)
  dev.off()
  
  datasets<-rbind(datasets, dataset)
  save(file=paste0('~/Desktop/session_data/edited_', tools::file_path_sans_ext(basename(images[i])), '.RData'), seg, regi, dataset)
  save(file=paste0('~/Desktop/datasets/edited_image_', i, '_accumulated_dataset.RData'), datasets)
  
  dev.off()
}

#save complete, edited datasets
save(file=paste0('~/Desktop/datasets/edited_full_accumulated_dataset.RData'), datasets)


#remove the first few sections, code issues in wholebrain
datasets_less<-datasets[datasets$AP < 4.855,]
datasets_right<-datasets_less
datasets_right$right.hemisphere<-TRUE

#3d brain, colored by region
glassbrain(datasets_right, cex=4, plane="sagittal", laterality=FALSE, col=heat.colors(100)[as.numeric(cut(log2(datasets$intensity), breaks = 100))])
#colored by intensity
glassbrain(datasets_right, cex=4, plane="sagittal", laterality=FALSE)
#plot specific slides to demonstrate wholebrain plotting willy nilly
#glassbrain(datasets[datasets$image %in% tools::file_path_sans_ext(basename(images[31:33])),], cex=4, plane="sagittal")

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
datasets_right$right.hemisphere<-TRUE
dot.plot(datasets_less)
dot.plot(datasets[datasets$image %in% tools::file_path_sans_ext(basename(images[21])),])

#make schematic plot
for(i in 24){schematic.plot(dataset=datasets[datasets$image %in% tools::file_path_sans_ext(basename(images[i])),])}

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
