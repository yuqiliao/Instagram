## Instagram Data Analysis
## Yuqi Liao
## Aug 22, 2018


## Ideas
# - general descriptives
#   - top 5 most commented posts/top 5 most liked posts
#   - do people post more in am or pm, in weekday or weekend, in which hour of the day?
# - what are the most frequent locations for posts that have location pins
#   - need to remove some generic pins such as "washington d.c."
#   - visualize the locations (dots, and heatmaps?)
# - analyze the texts
#   - what are the most used words, emojis, hastags? (simple descriptives)
#   - what are the most "cliched" topics (topic modeling)
# - analyze the comments (optional)
# - modeling
#   - what predits likes received? or comments received?


### Set things up -----
setwd("/Users/Yuqi/Google Drive/Skyladder/Instagram/Data")
#setwd("G:/Instagram Project/wethepeopledc")

library(rjson) #this package and jsonlite use the same verbs
library(jsonlite)
library(devtools)
library(instaR)  #install_github("pablobarbera/instaR/instaR")
library(RCurl)
library(dplyr)
library(plyr)
library(lubridate)
library(urbnmapr) #devtools::install_github("UrbanInstitute/urbnmapr")
library(wesanderson) #install.packages("wesanderson")

### Creating an OAuth token
# instagram_app_id  <- "<insert your app id>"
# instagram_app_secret <- "insert your app secret>"
# token <- instaOAuth(app_id=instagram_app_id,
#                     app_secret=instagram_app_secret)
# 
# token
# my_access_token <- token$credentials$access_token
# my_access_token
# 
# 
# ### Read in data
# wethepeopledc_df <- jsonlite::fromJSON(txt = "wethepeopledc.json")
# wethepeopledc_ls <- rjson::fromJSON(file = "wethepeopledc.json")
# 
# ### Create long and lats based on the location ids (if a post has one) -----
# I later realized that the instaR::getLocation() could also do the job
# e.g.
# >getLocation( location_id=372247132, token=token)
# latitude longitude location_id     location_name
# 1 38.89816 -77.02107   372247132 Capital One Arena
# length(wethepeopledc_ls)
# 
# for (i in 1:length(wethepeopledc_ls)){
#   #pause between each iteraction? - from the error message: "You have exceeded the maximum number of requests per hour. You have performed a total of 4564 requests in the last hour. Our general maximum limit is set at 500 requests per hour."
#   Sys.sleep(8)
#   print(paste0("processing the ", i,"th row: ", Sys.time()))
#   #get location id
#   location_id <- wethepeopledc_ls[[i]]$location$id
#   
#   if (is.null(location_id)){
#     wethepeopledc_ls[[i]]$location$latitude <- NA
#     wethepeopledc_ls[[i]]$location$longitude <- NA
#     
#   } else {
#     
#   #find out its lons and lats using the Instagram API endpoints
#   url <- paste0("https://api.instagram.com/v1/locations/",
#                 location_id,
#                 "?access_token=",
#                 my_access_token)
#   
#   url_loc <- rjson::fromJSON(json_str = getURL(url))
#   
#   #if no error message is reported, continue
#   if (is.null(url_loc$error_message)) {
#     #add lons and lats info back to the list
#     wethepeopledc_ls[[i]]$location$latitude <- url_loc$data$latitude
#     wethepeopledc_ls[[i]]$location$longitude <- url_loc$data$longitude
#   } else {
#     print(paste0("i have to stop here at the ", i,"th row due to error message: ", url_loc$error_message, Sys.time()))
#   }
#   } 
#   
# }
# 
# # the above code took 23 hours to run, save the Rdata object
# save(wethepeopledc_ls, file = "G:/Instagram Project/wethepeopledc/wethepeople_with_location.rdata")

#load(file = "G:/Instagram Project/wethepeopledc/wethepeople_with_location.rdata")
#load(file = "/Users/Yuqi/Google Drive/Skyladder/Instagram/Data/wethepeople_with_location.rdata")

# ### Create data frame for further analysis -----
# out <- data.frame("comments" = character(0),
#                   "comments_disabled" = logical(0),
#                   "edge_media_preview_like" = integer(0),
#                   "edge_media_to_caption" = character(0),
#                   "edge_media_to_comment" = integer(0),
#                   "id" = character(0),
#                   "is_video" = logical(0),
#                   "location_id" = integer(0),
#                   "location_has_public_page" = logical(0),
#                   "location_name" = character(0),
#                   "location_slug" =  character(0),
#                   "location_latitude" = numeric(0),
#                   "location_longitude" = numeric(0),
#                   "tags" = character(0),
#                   "taken_at_timestamp" = integer(0),
#                   "urls" = character(0),
#                   "username"= character(0))
# 
# for (i in 1:length(wethepeopledc_ls)){
#   
#   ##create a out_temp data frame of 1 row
#   out_temp <- data.frame("comments" = character(1),
#                          "comments_disabled" = logical(1),
#                          "edge_media_preview_like" = integer(1),
#                          "edge_media_to_caption" = character(1),
#                          "edge_media_to_comment" = integer(1),
#                          "id" = character(1),
#                          "is_video" = logical(1),
#                          "location_id" = integer(1),
#                          "location_has_public_page" = logical(1),
#                          "location_name" = character(1),
#                          "location_slug" =  character(1),
#                          "location_latitude" = numeric(1),
#                          "location_longitude" = numeric(1),
#                          "tags" = character(1),
#                          "taken_at_timestamp" = integer(1),
#                          "urls" = character(1),
#                          "username"= character(1))
#   
#   ##$comments
#   if (length(wethepeopledc_ls[[i]]$comments$data) == 0){
#     temp_comment <- NA 
#   } else { 
#     temp_comment <- list()
#     for(a in 1:length(wethepeopledc_ls[[i]]$comments$data)){
#       temp_comment <- paste(temp_comment, wethepeopledc_ls[[i]]$comments$data[[a]]$text, sep = "\n")
#     }
#   }
#   out_temp$comments <- temp_comment
#   
#   ##$comments_disabled
#   out_temp$comments_disabled <- wethepeopledc_ls[[i]]$comments_disabled
#   
#   ##$edge_media_preview_like
#   out_temp$edge_media_preview_like <- wethepeopledc_ls[[i]]$edge_media_preview_like$count
#   
#   ##$edge_media_to_caption #there are rare cases (e.g. row 5196) where there is no caption
#   if (length(wethepeopledc_ls[[i]]$edge_media_to_caption$edges) == 0){
#     temp_caption <- NA
#   } else {
#     temp_caption <- wethepeopledc_ls[[i]]$edge_media_to_caption$edges[[1]]$node$text
#   }
#   out_temp$edge_media_to_caption <- temp_caption
#   
#   ##$edge_media_to_comment
#   out_temp$edge_media_to_comment <- wethepeopledc_ls[[i]]$edge_media_to_comment$count
#   
#   ##$id
#   out_temp$id <- wethepeopledc_ls[[i]]$id
#   
#   ##$is_video
#   out_temp$is_video <- wethepeopledc_ls[[i]]$is_video
#   
#   ##$location - location_id
#   out_temp$location_id <- wethepeopledc_ls[[i]]$location$id
#   
#   ##$location - location_has_public_page
#   out_temp$location_has_public_page <- wethepeopledc_ls[[i]]$location$has_public_page
#   
#   ##$location - location_name
#   out_temp$location_name <- wethepeopledc_ls[[i]]$location$name
#   
#   ##$location - location_slug
#   out_temp$location_slug <- wethepeopledc_ls[[i]]$location$slug
#   
#   ##$location - location_latitude
#   out_temp$location_latitude <- wethepeopledc_ls[[i]]$location$latitude
#   
#   ##$location - location_longitude
#   out_temp$location_longitude <- wethepeopledc_ls[[i]]$location$longitude
#   
#   ##$tags
#   if (length(wethepeopledc_ls[[i]]$tags) == 0){
#     temp_tags <- NA
#   } else {
#     #wethepeopledc_ls[[i]]$tags may length more than 1
#     temp_tags <- toString(wethepeopledc_ls[[i]]$tags)
#   }
#   out_temp$tags <- temp_tags
#   
#   ##$taken_at_timestamp
#   out_temp$taken_at_timestamp <- wethepeopledc_ls[[i]]$taken_at_timestamp
#   
#   ##$urls #wethepeopledc_ls[[i]]$urls may have length more than 1 (when people posts multiple pictures); also the videos may be expired "URL signature expired" 
#   out_temp$urls <- toString(wethepeopledc_ls[[i]]$urls)
#   
#   ##$username
#   out_temp$username <- wethepeopledc_ls[[i]]$username
#   
#   ## rbind out_temp to out
#   out <- rbind.fill(out, out_temp)
# }
# 
# 
### Further clean the data frame -----
# convert the time column into a time object
# data <- out %>%
#   mutate(time = as_datetime(taken_at_timestamp, tz = "US/Eastern"))
# 
# # create time-related variables
# data <- data %>% 
#   mutate(year = year(time),
#          month = month(time),
#          day = day(time),
#          day_of_week = wday(time),
#          hour = hour(time),
#          quarter = quarter(time),
#          am = am(time),
#          pm = pm(time),
#          is_weekend = ifelse(day_of_week %in% c(6, 7), TRUE, FALSE))
# ### create FIPS code based on the lats and lons, using FCC APIs
# # prepare a list of URLs
# url <- paste0("https://geo.fcc.gov/api/census/block/find?latitude=",
#                                data$location_latitude,"&longitude=",
#                                data$location_longitude,"&format=json")
# 
# # define a get_fips functiont to call FCC's API
# get_fips <- function(x){
#   url_json <- rjson::fromJSON(json_str = getURL(x))
#   fips <- url_json$Block$FIPS 
#   #if fips is null, turn it into NA
#   if(is.null(fips)){
#     fips <- NA
#   } 
#   return(fips)
# }
# 
# # get fips
# fips <- sapply(url, get_fips)
# 
# # combine fips and the data object
# a <- bind_cols(data, as.data.frame(unlist(fips)))
# 
# data <- data %>% 
#   bind_cols(as.data.frame(unlist(fips))) %>% 
#   dplyr::rename(fips = `unlist(fips)`) 
# # create a 11-digit version of the fips code (at the census track level), the original 15-digit version is at the block level
# data <- data %>% 
#   mutate(fips_11digt = substr(fips, 1, 11)) 




# save(data, file = "/Users/Yuqi/Google Drive/Skyladder/Instagram/Data/wethepeople_clean_data.rdata")
load("/Users/Yuqi/Google Drive/Skyladder/Instagram/Data/wethepeople_clean_data.rdata")


### Descriptives -----
summary(data)
summary(as.factor(data$year))
summary(as.factor(data$month))
summary(as.factor(data$day))
summary(as.factor(data$day_of_week)) #interesting
summary(as.factor(data$hour)) #interesting
summary(as.factor(data$quarter))
# there is one post that disabled comments
data[data$comments_disabled == TRUE,]



### most likes received -----
summary(data$edge_media_preview_like)

data %>% 
  arrange(desc(edge_media_preview_like)) %>% 
  select(id) %>% 
  head()

data[data$id == "1742342435071537244",]
data[data$id == "1683838892712597150",]
data[data$id == "1652266661444569810",]
data[data$id == "1550768162563604311",]
data[data$id == "1629037250133901946",]


# most comments received -----
summary(data$edge_media_to_comment)

data %>% 
  arrange(desc(edge_media_to_comment)) %>% 
  select(id) %>% 
  head()

data[data$id == "1756737130363004810",]
data[data$id == "1730615329723582673",]
data[data$id == "1716327519130989529",]
data[data$id == "1701253423566751905",]
data[data$id == "1644635929104470880",]


### find post of a specific date -----
data %>% 
  filter(year == 2016 & month == 10 & day == 12) %>% 
  View()



### what are the most frequent locations for posts that have location pins  -----

data %>% 
  group_by(location_name,location_id) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  View()

data %>% 
  group_by(location_id) %>% 
  tally() %>% 
  arrange(desc(n)) 

a <- data %>% 
  add_count(location_name, sort = TRUE) %>% 
  dplyr::rename(n_location_name = n) %>% 
  #select()
  #View()

  
  
### point map -----
install_github("dkahle/ggmap")
library(ggmap)

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
ggmap(get_googlemap())
google_key <- "<insert your google key>"
register_google(key = google_key)
geocode("waco texas")

DC <- get_map(location="Washington, DC", zoom=12,
              color = "bw", source = "stamen", maptype = "toner") 
ggmap(DC)

DC2 <- get_map(location="Washington, DC", zoom=11,
              color = "bw", source = "stamen", maptype = "toner") 
ggmap(DC2)

# DC3 <- get_map(location="Washington, DC", zoom=12,
#                color = "bw", source = "osm") 
# ggmap(DC3)


ggmap(DC) + 
  geom_point(data = data,
             aes(x = location_longitude, y = location_latitude, color = as.factor(quarter)),
             size = 1,
             alpha = 0.2) +
  facet_grid(. ~ quarter)

ggmap(DC2) + 
  geom_point(data = data,
             aes(x = location_longitude, y = location_latitude),
             size = 0.8,
             alpha = 0.5,
             color = "#9A191C")


### area map (census track level) -----
#Download & clean census tract shapefile for DC
libs <- c("dplyr",
          "tidyr",
          "magrittr",
          "foreign",
          "sp",
          "maptools",
          "ggplot2",
          "broom",
          "rgeos",
          "RColorBrewer",
          "tigris")
lapply(libs, library, character.only = TRUE)

DC.shp <- tracts(state = "DC", county = "01")
#View(DC.shp)

DC.df  <- tidy(DC.shp, region="GEOID")
names(DC.df)[7] <- "tract"

# Map the plain shapefile
ggplot(DC.df) +
  aes(x=long, y=lat, group=group) +
  geom_path(color="black") +
  coord_map()

# Changing colors and adding labels
ggplot(DC.df) +
  aes(x=long, y=lat, group=group) +
  geom_polygon(color="white", fill="#053769") +
  labs(title="DC Census Tracts", x="Longitude", y="Latitude") +
  coord_map()

# calculate statistics in data, grouped by fips_11digts, and then merge with Cook.df data
# data %>% 
#   filter(!(is.na(location_id)) & is.na(fips_11digt)) %>% 
#   View() ###YL: Note that there are 125 rows that has location_id but has no lons and lats, (or in 2 cases, the lons and lats are off), thus have no fips. Update: I confirmed that those edge cases are no fault of the instagram API, they are probabbly user-defined locations that has the correct id_name and id_slug, but incorrect location_id, thus incorrect lons and lats (mostly NA)



data_loc_count <- data %>% 
  #remove the NA and the generic Washignton D.C. posts
  filter(!(is.na(location_id) | location_id == "213480180")) %>% 
  filter(!is.na(fips_11digt)) %>% #treating those 125 rows mentioned above as missing for now
  group_by(fips_11digt) %>% 
  tally() %>% ##YL: note that there are also cases that indicates lons and lats may not be returned correctly. for example, there is one location that is in Georgia but it should be in a dc school. `data %>% filter(fips_11digt == "13121010603")`. Update: I confirmed that those edge cases are no fault of the instagram API, they are probabbly user-defined locations that has the correct id_name and id_slug, but incorrect location_id, thus incorrect lons and lats (mostly NA)
  #drop a few outliers (fips_11digt) not within DC (keep only fips that are starts with "11", meaning DC)
  filter(substr(fips_11digt, 1, 2) == "11") 

# merge data_loc_count with DC.df
DC.df_update <- DC.df %>% 
  left_join(data_loc_count, by = c("tract" = "fips_11digt")) %>% 
  # change NA into 0
  mutate(n = replace(n, which(is.na(n)), 0))

# draw
DC.df_update$category <- cut(DC.df_update$n, breaks = c(0,10,50,100,Inf), right = FALSE)

ggplot(DC.df_update) +
  aes(x=long, y=lat, group=group) +
  geom_polygon(aes(fill=category, group=group), color="#990000", size = 1) +
  geom_polygon(aes(fill=category, group=group), color="white") +
  scale_fill_brewer(palette = "OrRd",
                    name = "Number of \n@WeThePeopleDC \nPosts by Census Track",
                    breaks = levels(DC.df_update$category),
                    labels = c("Less than 10", "10 to 50", "50 to 100", "100 or more")) +
  #labs(title="@WeThePeopleDC Location by Census Track (2015 - 2018)", x="Longitude", y="Latitude") +
  coord_map() +
  ggthemes::theme_map() +
  theme(legend.position = c(0, 0.2),
        legend.title = element_text(colour="#9A191C", size=12, face="bold"),
        legend.text = element_text(colour="#9A191C", size = 10, face = "bold"))



# ### try urban map r 
# ggplot() + 
#   geom_polygon(data = urbnmapr::states %>% filter(state_abbv=="DC"), mapping = aes(x = long, y = lat, group = group), fill = "grey", color = "white") +
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45)















