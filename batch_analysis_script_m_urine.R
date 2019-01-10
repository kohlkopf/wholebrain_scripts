# It is necessary to interact wtih this script
wd <- "~/m_urine/"
setwd(wd)
library(wholebrain)

# get listing, saved ls output with natural numerical sorting, produced with code below
# gls d -1 -v $PWD/*.* | vim - (gnu ls, natural sort)
images <- readLines('~/m_urine/local_osx_listing.txt')

#set spacing between periods in millimeters
smp<-0.06

# Next pick four different images far apart from eachother and manually inspect them for what coordinate they are at by comparing to www.openbrainmap.org
# http:/mouse.brain-map.org/experiment/siv/?imageId=102162070
# cant't use the first image! not robust, subtracts bregma due to creating a sequence
inspected <- c(2, 15, 21, 37, 48, 75)

#for(i in 1:length(inspected)){imshow(images[inspected[i]])}
#lapply(X=inspected, function(x){makewebmap(images[x])})

imshow(images[20])

#assign brain coordinates for inspected sections.
smp.coord<-c(1.9, 1.345, 1, -0.02, -1, -2.55)

#now we can just generate all the intermediate coordinates automatically with the map.to.atlas() function
#assign brain coordinates for all sections in this brain.
rm(coord)
coord <- map.to.atlas(image.number=inspected, 
                      coordinate=smp.coord, 
                      sampling.period=smp, 
                      number.of.sections=length(images)
)

#plot the coords to ensure nothing is way off
# we do observe a bump at index 37, not sure what is going on, bregma has been confirmed
plot(coord[0:155])


#either develop filters on 33 or use old filters below
neuron_seg <- segment(images[21], channel=2)
brain_seg <- segment(images[15], channel=0)
if(!dir.exists(file.path(wd, "saved_filters"))){dir.create(file.path(wd, "saved_filters"), showWarnings = FALSE)}
save(neuron_seg, brain_seg, file="~/m_urine/saved_filters/filters.RData")

#use old filters
load("~/m_urine/saved_filters/filters.RData")
brain_seg$filter$resize<-0.08

#datasets <- NULL
#load('~/m_urine/datasets/up_to_image_127_accumulated_dataset.RData')

nrow(table(datasets$image))


for(i in 69:length(images)){
    seg<-segment(images[i], display=FALSE, filter = neuron_seg$filter, channel=2)
    
    quartz(width=26.7, height=10)
    input.points=""
    regi<-registration(images[i], coordinate=coord[i], filter = brain_seg$filter, display=TRUE, channel=0)
    input.points<-readline(prompt="Input a vector of points to remove, enter if OK: ")
    if(!(input.points == "")){regi<-remove.corrpoints(regi, eval(parse(text=input.points)))}
    regi<-add.corrpoints(regi)
    #re-register with added/removed points
    regi<-registration(images[i], coordinate=coord[i], filter = brain_seg$filter, display=TRUE, channel=0, correspondance=regi)
    dev.off()
    
    if(!dir.exists(file.path(wd, "registrations"))){dir.create(file.path(wd, "registrations"), showWarnings = FALSE)}
    dataset<-inspect.registration(regi, seg, soma = TRUE, forward.warps = TRUE, batch.mode = TRUE)
    #dev.copy(pdf, paste0(wd, 'registrations/edited_',tools::file_path_sans_ext(basename(images[i])), '.pdf'), width=18, height=8)
    dev.off()
    
    ##for the first case, initialize datasets
    #if(!exists("datasets")){datasets <- dataset}
    #datasets<-rbind(datasets, dataset)
    #
    ##create directory, save progress
    #if(!dir.exists(file.path(wd, "session_data"))){dir.create(file.path(wd, "session_data"), showWarnings = FALSE)}
    #save(file=paste0(wd, 'session_data/', tools::file_path_sans_ext(basename(images[i])), '.RData'), seg, regi, dataset)
    #
    ##create directory, save the registration filter
    #if(!dir.exists(file.path(wd, "specific_reg"))){dir.create(file.path(wd, "specific_reg"), showWarnings = FALSE)}
    #save(file=paste0(wd, 'specific_reg/image_', i, '_registration_info.RData'), regi)
    #
    ##create directory, save the datasets df
    #if(!dir.exists(file.path(wd, "datasets"))){dir.create(file.path(wd, "datasets"), showWarnings = FALSE)}
    #save(file=paste0(wd, 'datasets/up_to_image_', i, '_accumulated_dataset.RData'), datasets)
    
    dev.off()
}

#remove neurons from datasets
#datasets<-datasets[!(datasets$image %in% tools::file_path_sans_ext(basename(images[93]))), ]

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
#--

datasets$animal <- "male_urine_stimulated"

#remove the first few sections, code issues in wholebrain
datasets_right$right.hemisphere<-TRUE

glassbrain(datasets, cex=4, plane="coronal", laterality=FALSE)
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
