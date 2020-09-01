#----------------------------------------------------------------------------------------------------------------------------------#
# Function by: Christy Rollinson
# Project: Living Collections Phenology Forecasting
# Purpose: A function that will download and convert our phenology observations
# Inputs: 2018 to present phenology monitoring data from the googlesheet "Phenology_Observations_GoogleForm" in the "LivingCollections-Phenology/Data_Observations/" folder
# Outputs:A dataframe containing cleaned information from the googleform
#----------------------------------------------------------------------------------------------------------------------------------#

clean.google <- function(google.key = "1eEsiJ9FdDiNj_2QwjT5-Muv-t0e-b1UGu0AFBRyITSg", collection="Quercus", dat.yr=lubridate::year(Sys.Date())){
  # get the data from a particular sheet
  dat.raw <- data.frame(googlesheets4::read_sheet(ss=google.key, sheet=paste(collection, "Raw Observations", dat.yr, sep=" "), col_types="c"))
  # names(dat.raw)
  # summary(dat.raw[,1:10])
  
  # Renaming some columns
  names(dat.raw)[grep("OPTIONAL", names(dat.raw))] <- "Notes"
  names(dat.raw)[grep("species", names(dat.raw))] <- "Species"
  names(dat.raw) <- gsub("..Intensity.", "", names(dat.raw))
  names(dat.raw) <- gsub("..Observed.", "", names(dat.raw))
  
  # Recoding for easier names -- there must be a better way to do this, but I can't figure it out right now
  names(dat.raw) <- car::recode(names(dat.raw),
                                "'Breaking.leaf.buds'='leaf.breaking.buds.observed';
                                  'Leaf.breaking.buds'='leaf.breaking.buds.observed';
                                  'Leaf.breaking.bud.intensity'='leaf.breaking.buds.intensity';
                                  'Leaf.observed'='leaf.present.observed';
                                  'Leaf.intensity'='leaf.present.intensity';
                                  'Leaf.increasing.in.size'='leaf.increasing.observed';
                                  'Leaf.increasing.in.size.intensity'='leaf.increasing.intensity';
                                  'Leaf.color.observed'='leaf.color.observed';
                                  'Leaf.color.intensity'='leaf.color.intensity';
                                  'Leaf.falling.observed'='leaf.falling.observed';
                                  'Flower.buds.observed'='flower.buds.observed';
                                  'Flower.buds.intensity'='flower.buds.intensity';
                                  'Flower.open.observed'='flower.open.observed';
                                  'Flower.open.intensity'='flower.open.intensity';
                                  'Flower.pollen.release.observed'='flower.pollen.observed';
                                  'Flower.pollen.release.intensity'='flower.pollen.intensity';
                                  'Fruit.observed'='fruit.present.observed';
                                  'Fruit.intensity'='fruit.present.intensity';
                                  'Fruit.ripe.observed'='fruit.ripe.observed';
                                  'Fruit.ripe.intensity'='fruit.ripe.intensity';
                                  'Fruit.drop.observed'='fruit.drop.observed';
                                  'Fruit.drop.intensity'='fruit.drop.intensity'")
  
  # Coming up with handy groups for our columns
  cols.meta <- c("Timestamp", "Observer", "Date.Observed", "Species", "PlantNumber", "Notes")
  pheno.leaf <- names(dat.raw)[grep("leaf", tolower(names(dat.raw)))]
  pheno.flower <- names(dat.raw)[grep("flower", tolower(names(dat.raw)))]
  pheno.fruit <- names(dat.raw)[grep("fruit", tolower(names(dat.raw)))]
  pheno.abund <- c("leaf.breaking.buds.intensity", "flower.buds.intensity", "fruit.present.intensity", "fruit.drop.intensity")
  
  # Setting things to factors
  # for(i in 1:ncol(dat.raw)){
  #   if(class(dat.raw[,i])=="character") dat.raw[,i] <- as.factor(dat.raw[,i])
  # }
  # summary(dat.raw)
  cols.id <- grep("accession", names(dat.raw))
  
  dat.clean <- dat.raw[,c(cols.meta[!cols.meta=="PlantNumber"], pheno.leaf, pheno.flower, pheno.fruit)]
  dat.clean$PlantNumber <- as.factor(apply(dat.raw[,cols.id], 1, FUN=function(x) {x[which(!is.na(x))][1]})) # Get the PlantNumber
  dat.clean$Timestamp <- strptime(dat.clean$Timestamp, format="%m/%d/%Y %H:%M:%S")
  dat.clean$Date.Observed <- as.Date(dat.clean$Date.Observed, format="%m/%d/%Y")
  dat.clean$Observer <- as.factor(dat.clean$Observer)
  dat.clean$Species <- as.factor(dat.clean$Species)
  
  # Converting everything that is not a date to a factor (except notes)
  for(COL in c(pheno.leaf, pheno.flower, pheno.fruit)){
    dat.clean[,COL] <- gsub(" ", "", dat.clean[,COL])
    dat.clean[,COL] <- as.factor(dat.clean[,COL])
    
    # Now do the ordering based on the levels
    if(grepl("observed", COL)){
      dat.clean[,COL] <- car::recode(dat.clean[,COL], "'?'='Unsure'; 'Didnotlookfor'='No Observation'; 'Did not look for'='No Observation'")
      dat.clean[,COL] <- factor(dat.clean[,COL], levels=c("No", "Yes", "Unsure", "No Observation"))
    } else if(COL == "flower.pollen.intensity") {
      dat.clean[,COL] <- factor(dat.clean[,COL], levels=c("None", "Little", "Some", "Lots"))
    } else if(COL %in% pheno.abund){
      dat.clean[,COL] <- factor(dat.clean[,COL], levels=c("0", "<3", "3-10", "11-100", "101-1,000", "1,001-10,000", ">10,000"))
    } else {
      dat.clean[,COL] <- factor(dat.clean[,COL], levels=c("0%", "<5%", "5-24%", "25-49%", "50-74%", "75-94%", ">95%"))
    }
  }
  
  dat.clean <- dat.clean[,c(cols.meta, pheno.leaf, pheno.flower, pheno.fruit)] # Just re-organizing to how I like to see things
  summary(dat.clean)
  
  # Get rid of observations that have TEST in them or are before our last phenology training
  rows.remove <- c(which(is.na(dat.clean$Species)), grep("TEST", toupper(dat.clean$NOTES)), grep("TEST", toupper(dat.clean$Observer)) )
  if(length(rows.remove)>0) dat.clean <- dat.clean[!(1:nrow(dat.clean) %in% rows.remove),] # 
  
  dat.clean <- droplevels(dat.clean) # Get rid of unused levels
  summary(dat.clean)
  
  
  return(dat.clean)
}