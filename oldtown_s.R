# This is where the R script appears
# Everything behind the "#" symbol is comments and R will not run

# 1. Install packages to enable R to do many amazing things beyond the "base" ----
## Pacman is a package that helps load/run other packages, very convenient in my opinion
install.packages("pacman")
## After installing, the library() function is used to load the packages
library(pacman)
## Then, we use the p_load() function to install/load other required packages
pacman::p_load(rio,dplyr,ggplot2,leaflet,leaflet.extras,sp,htmlwidgets)

# 2. Import the polygon file ----
## The import() function of "rio" package helps import most file types
oldtown_spdf <- import("oldtown_spdf.RData")
## Let's visualise the polygon using the plot() function!
plot(oldtown_spdf)

# 3. Import the oldtown photos file ----
## Please explore the oldtown_raw dataframe using the view icon in the "envionment" panel
oldtown_raw <- read.csv("oldtown.csv")

## Let's assume that we performed a topic modelling algorithm (LDA), here's the final file
oldtown_lda <- read.csv("oldtown_lda.csv")

# Protip: you can see the photo by going to the url http://www.flickr.com/photos/[uid]/[photoid]

# 4. Creating the first map ----
## After we have the dataframe, we are going to create a map using leaflet() function
oldtown_map <- oldtown_lda %>% # This "%>%" is the piping function that can chain many functions together, read it as "then"
        # We then create a map using a leaflet() function, we also specify some other configurations, you can try removing and only have leaflet()
        leaflet(option = leafletOptions(dragging = TRUE, minZoom = 4.7, maxZoom = 18)) %>%
        # There is nothing on the map yet so we have to add an open-sourced map called "ESRI"
        addProviderTiles("Esri.WorldStreetMap", group = "Esri") %>%
        # We also add another map on top. These two maps (laying on top of one another) can be toggled later
        addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
        # We set the view and zoom to the oldtown Bangkok, please try adjusting the number
        setView(lng = 100.503186,
                lat = 13.746717,
                zoom = 14) 
# You can visualise the map by running the leaflet object that we just created
oldtown_map

# 5. Visualise the locations of photos ----
oldtown_map %>%
        addCircleMarkers(lat = ~lat, lng = ~lng, 
                         radius = 0.1, opacity = 0.1, color = "black")

# 6. Visualise the locations of photos by the region of residence of users ----
## Dot plot by region
oldtown_map %>%
        # Let's add oldtown polygons on the map first 
        addPolygons(data = oldtown_spdf,
                    weight = 1, stroke = TRUE, color = "white", opacity = 0.8, 
                    fillColor = "blue", fillOpacity = 0.1) %>%
        # Add circle markers of photos taken by users from "thailand"
        addCircleMarkers(data = oldtown_lda[oldtown_lda$region == "thailand",], 
                         lng = ~lng, 
                         lat = ~lat, 
                         radius = 2, opacity = 0.3, color = "grey",
                         group = "Thailand") %>%
        # Add circle markers of photos taken by users from "asia"
        addCircleMarkers(data = oldtown_lda[oldtown_lda$region == "asia",], 
                         lng = ~lng, 
                         lat = ~lat, 
                         radius = 2, opacity = 0.3, color = "red",
                         group = "Asia") %>%
        # Add circle markers of photos taken by users from "europe"
        addCircleMarkers(data = oldtown_lda[oldtown_lda$region == "europe",], 
                         lng = ~lng, 
                         lat = ~lat, 
                         radius = 2, opacity = 0.3, color = "blue",
                         group = "Europe") %>%
        # Add circle markers of photos taken by users from "namerica"
        addCircleMarkers(data = oldtown_lda[oldtown_lda$region == "namerica",], 
                         lng = ~lng, 
                         lat = ~lat, 
                         radius = 2, opacity = 0.3, color = "green",
                         group = "North America") %>%
        # Add a control panel to toggle between the four regions
        addLayersControl(baseGroups = c("Thailand","Asia","Europe","North America"),
                         position = "topright") %>%
        addFullscreenControl()

# 7. Visualise the locations of photos by region and topic
## Before visualising, we need to create a list of region-topic pairing
layers <- list()
region_topic_grid <- expand.grid(topic_name = colnames(oldtown_lda)[11:24], region = c("thailand","asia","europe","namerica")) %>%
        mutate(region_topic = paste(region,topic_name,sep = "_"))
for (i in 1:nrow(region_topic_grid)) {
        layers[[i]] <- oldtown_lda %>%
                filter(topic_name == as.character(region_topic_grid[i,1]),
                       region == as.character(region_topic_grid[i,2]))
}

## Draw a map
oldtown_dots <- oldtown_map %>%
        ## Let's add oldtown polygons on the map first 
        addPolygons(data = oldtown_spdf,
                    weight = 1, stroke = TRUE, color = "white", opacity = 0.8, 
                    fillColor = "blue", fillOpacity = 0.1)
## Run a for loop ploting the layer of dots (users from Thailand). The reason we run separately is to change colors
for (i in 1:14) {
        oldtown_dots <- oldtown_dots %>% addCircleMarkers(data = layers[[i]], group = as.character(region_topic_grid[i,3]), lng = ~lng, lat = ~lat, radius = 4, stroke = TRUE, weight = 1, opacity = 0.3, color = "black", fill = TRUE, fillColor = "grey", fillOpacity = 0.3)
}
for (i in 15:28) {
        oldtown_dots <- oldtown_dots %>% addCircleMarkers(data = layers[[i]], group = as.character(region_topic_grid[i,3]), lng = ~lng, lat = ~lat, radius = 4, stroke = TRUE, weight = 1, opacity = 0.3, color = "black", fill = TRUE, fillColor = "red", fillOpacity = 0.3)
}
for (i in 29:42) {
        oldtown_dots <- oldtown_dots %>% addCircleMarkers(data = layers[[i]], group = as.character(region_topic_grid[i,3]), lng = ~lng, lat = ~lat, radius = 4, stroke = TRUE, weight = 1, opacity = 0.3, color = "black", fill = TRUE, fillColor = "blue", fillOpacity = 0.3)
}
for (i in 43:56) {
        oldtown_dots <- oldtown_dots %>% addCircleMarkers(data = layers[[i]], group = as.character(region_topic_grid[i,3]), lng = ~lng, lat = ~lat, radius = 4, stroke = TRUE, weight = 1, opacity = 0.3, color = "black", fill = TRUE, fillColor = "green", fillOpacity = 0.3)
}

oldtown_dots <- oldtown_dots %>% 
        # Add control panel to control base groups (toggle) and overlaygroups (overlaying)
        addLayersControl(overlayGroups = region_topic_grid[1:56,3],
                         baseGroups = c("CartoDB","Esri"),
                         position = "topright") %>%
        # Start with hidden dots
        hideGroup(region_topic_grid[1:56,3]) %>%
        addFullscreenControl()

oldtown_dots

# Save the map to a html file
saveWidget(oldtown_dots, "bangkok_oldtown.html", title = "Photos by region/topic")

