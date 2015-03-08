###########################################################
## This script will pump out plots of a species range in NC
## based on csv's downloaded from http://fishnet2.net/search.aspx
## You can request an api key to automate the process from:
## Nelson Rios at nrios@tulane.edu .
###########################################################
## Scipt takes about 10 minutes to execute on a 80k line csv
## IT will create a maps directory in your current working 
## directory, and save all of the plots there. 
###########################################################
## This script freeware and is not licensed in anyway,
## scott.smith@ncfishes.com for any questions
###########################################################



# returns string with whitespaces removed(trailing and leading) 
trim <- function (x) {gsub("^\\s+|\\s+$", "", x)}


cleanData <- function(name) {
        species <- name
        # subset data by species
        data.sub <- data[data$ScientificName == species,]
        # subset desired columns
        data.sub <- data.sub[,c(4,9,10, 15)]
        names(data.sub) <- c("count", "lat", "lon", "county")
        # Remove any observations with malformed longitudes(Some were 0.00000, 0.0000)
        data.sub <- data.sub[!(data.sub$lon==0 | data.sub$lat==0),]
        # Convert the class from character to numeric
        data.sub$lon <- as.numeric(data.sub$lon)
        data.sub$lat <- as.numeric(data.sub$lat)
        # Repair longitude where author missed a negative (all North America should be +lat, -lon)
        data.sub$lon <- -abs(data.sub$lon)
        # Repair latitude where -lat
        data.sub$lat <- abs(data.sub$lat)
        # Remove ouliers that were incorrectly geolabeled. These values represent the NS and EW bounds of NC 
        data.sub <- data.sub[data.sub$lon < -75,]
        data.sub <- data.sub[data.sub$lon > -84.5,]
        data.sub <- data.sub[data.sub$lat < 36.6,]
        data.sub <- data.sub[data.sub$lat > 33.8,]
        # Remove all NA values from data.sub
        data.sub <- data.sub[!is.na(data.sub$lon),]
        
        # Some observations have NA under IndividualCount, Instead of discarding these values, we set all NA's to 1
        # It is safe to assume at least one fish was caught, or they wouldnt be reporting this observation
        data.sub$count[is.na(data.sub$count)] <- 1
        # Remove (70% Ethanol) from the count variable
        data.sub$count <- gsub("\\(.*", "", data.sub$count)
        # Remove whitespaces left over
        data.sub$count <- trim(data.sub$count)
        data.sub$count <- as.numeric(data.sub$count)
        
        return(data.sub)
}



plotData <- function(name, dat){      
        data.sub <- dat
        species <- name
        # exit the plot function if no observations are left in the data after cleaning
        if(length(data.sub$count) < 1) { 
                print(paste("Error with", species))
                return(NULL)
        }
        

        
        ##############################################
        ## PLOTTING CODE
        ##############################################
        par(mar=c(3.1,3.1,3.1,2.1))
        
        # Create a folder to place maps in
        tempdir <- paste0(getwd(),"/maps")
        if (!file.exists(tempdir)){
                dir.create(tempdir)                
        }
        
        # This creates a variable of the number of observations
        tot <- paste("n=",sum(data.sub$count), "As of: ", Sys.Date())
        
        # Create a string with an underscore between genus and specific epithet
        species.nospace <- gsub("\\s","_",species)
        name <- paste0(getwd(),"/maps/",species.nospace,"_plot1.png")
        #png(name,600,600)
        CairoPNG(filename = name, width = 706, height = 449,
                 pointsize = 12, bg = "transparent")
        # Get polygon data for county shapes
        states_map <- map_data("county","north carolina")
        p <- ggplot() +  
                geom_polygon(data=states_map, aes(x=long, y=lat, group=group), fill="light green",colour="black")+
                coord_map("polyconic") +    
                geom_point(data=data.sub,aes(x=lon, y=lat, group=county), color="#3E72DA", cex=3) + 
                ggtitle(paste("Collection Locations of",species,"in NC")) +
                annotate("text", x = -82, y = 34.5, label = tot) +
                
                theme(axis.line=element_blank(),
                      plot.title = element_text(lineheight=.8, face="bold"),
                      axis.text.x=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      panel.background=element_blank(),
                      panel.border=element_blank(),
                      panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank(),
                      plot.background=element_blank())
        print(p)
        dev.off()
}


#######################################
## Begin Script
#######################################

# Load packages
library(Cairo)
library(ggplot2)

# Since we can't download the data automatically, set the path to the location of your csv
# Windows users, note the forward facing slashes! 
csvLocation <- "C:/Users/Admin/Documents/CourseraClasses/rangemap/range_all_counties/NC Fish Collection Sites.csv"

# Read the csv file downlaoded from fishnet2.net and delete whitespace before and after words
data <- read.csv(csvLocation, header=TRUE, sep=",", stringsAsFactors=FALSE,  strip.white = TRUE, na.strings = c("NA",""))


# Remove blanks and 0's in the longitude variable (i.e. keep only geopositioned collections)
data <- data[which(data$Longitude != ""), ]
data <- data[which(data$Longitude != "0"), ]


speciesCount <- unique(data$ScientificName)
for (i in 1:length(speciesCount)){
        # Some elements of speciescount are malformed "pomotis" instead of "acantharchus pomotis"
        # here we try to look for two or more words in an element
        if(grepl("\\w+\\s\\w+",speciesCount[i])){  
                tempdata <- cleanData(speciesCount[i])
                plotData(speciesCount[i], tempdata)
        }
}





