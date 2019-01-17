# The single_image_vignette.html accompanies this script.

library(wholebrain)

setwd("Images/")

## Bring in the image

#this function needs a folder
#image <- get.images(x='~/Images/', type="tif")
image <- "/Users/kk2252/Images/MTP-Cre-H129dTT-120hrs_37_Image.vsi.Collection_X-82.64_mmY-62.86_mm_Layer5.tif"

#---

## Segment
### Neurons

neuron_seg <- segment(input=image, channel=2)

### Brain outline

brain_seg <- segment(input=image, channel=0)
#set resize manually
brain_seg$filter$resize <- 0.08

#---

## Register

imshow(image)

for(i in 1){
    quartz(width=15, height=10)
    input.points=""
    regi<-registration(image, filter = brain_seg$filter, coordinate=1.4, display=TRUE, channel=0)
    input.points<-readline(prompt="Input a vector of points to remove, enter if OK: ")
    if(!(input.points == "")){regi<-remove.corrpoints(regi, eval(parse(text=input.points)))}
    regi<-add.corrpoints(regi)
    #re-register with added/removed points and show
    regi<-registration(image, coordinate=1.9, filter = brain_seg$filter, display=TRUE, channel=0, correspondance=regi)
    dev.off()
}

#---

## Save the info

inspect.registration(regi, neuron_seg, soma = TRUE, forward.warps = TRUE, batch.mode = FALSE)
dataset<-inspect.registration(regi, neuron_seg, soma = TRUE, forward.warps = TRUE, batch.mode = FALSE)

#---

## Visualize the neurons

dot.plot(dataset)

schematic.plot(dataset)

pixel.resolution<-1 #1 micron
#this will output in the script folder
makewebmap(img = image, filter = brain_seg$filter, registration = regi, dataset = dataset, scale = 0.64, fluorophore = "tdTomato")

glassbrain(dataset, plane="coronal", laterality=FALSE)
planes3d(0,0,1, col = 'lightblue', alpha = 0.9)

#---
