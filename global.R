### global.R --- 
## 
## Filename: global.R
## Description: 
## Author: Sergio-Feliciano Mendoza-Barrera
## Maintainer: 
## Created: Sat Apr 25 23:15:16 2015 (-0500)
## Version: 
## Package-Requires: ()
## Last-Updated: Sat Apr 25 23:18:32 2015 (-0500)
##           By: Sergio-Feliciano Mendoza-Barrera
##     Update #: 3
## URL: 
## Doc URL: 
## Keywords: 
## Compatibility: 
## 
######################################################################
## 
### Commentary: 
## 
## 
## 
######################################################################
## 
### Change Log:
## 
## 
######################################################################
## 
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or (at
## your option) any later version.
## 
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
## 
######################################################################
## 
### Code:
require(devtools)
require(rCharts)
require(twitteR)
require(maps)
require(geosphere)
require(RColorBrewer)
require(RCurl)
require(base64enc)
require(leaflet)

options(stringsAsFactors = F)

## @knitr plotMap
plotMap <- function(userName = "mitxalumni", userSelectLocation = "Boston", width = 1600, height = 800, nMax = 1000){

        ## Loading auxiliary functions
        ## source("twitterUtils.R")

######################################################################
        ## Set options globally
        ## Set SSL certs globally
        options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

######################################################################
        ## Authentication in Twitter, see:
        ## http://thinktostart.wordpress.com/2013/05/22/twitter-authentification-with-r/

        ## MITXAlumni twitteR account read only access
        consumer_key <- "Sscgaum5T49aip7S0xiZJe55x"
        consumer_secret <- "QYtxfsAraaA8tMFASsYq6vCeX8MdcCHpuk0aOjD5C3CbNmhzFN"
        access_token <- "2596627068-ACzqBZtIhNj1FYRwn0nU6Yauj33vfL9PF802GGV"
        access_secret <- "pRI4Wxhq66gJGOUDpcdUUz5giczwsvhHVqf48XPhSzbQs"

        ## getOption("httr_oauth_cache")
        options(httr_oauth_cache = FALSE)
        ## getOption("httr_oauth_cache")
        setup_twitter_oauth(consumer_key,
                                  consumer_secret,
                                  access_token,
                                  access_secret)

######################################################################
        ## Test data
        ## userSelectLocation <- "Boston"
        ## userName <- "mitxalumni"
        ## nMax <- 10

######################################################################
        ## Get location data
        cat("Getting data from Twitter, this may take a moment.\n")

######################################################################
        ## Getting data from Twitter
        userData <- getUser(userName)
        userLocation <- location(userData)

        if(is.null(userLocation) | userLocation == ""){
                userLocation <- userSelectLocation
                ## userLocation = trim(userLocation)
                if(nchar(userLocation) < 2){stop("*** We can not find your location from Twitter")}
        }

        ## followers <- userData$getFollowers(n=nMax)              # test code
        followers <- userData$getFollowers(n=nMax)
        followersLocation <- sapply(followers,function(x){location(x)})
        ## following <- userData$getFriends(n=nMax)                # test code
        following <- userData$getFriends(n=nMax)
        followingLocation <- sapply(following,function(x){location(x)})

        ## Load the geographic data
        data(world.cities)
        data(us.cities)
        data(canada.cities)

        ## Find the latitude and longitude of the user
        cat("Getting geographic (latitude/longitude) of Twitter users.\n")
        userLL <- findLatLon(userLocation)$latlon
        if(any(is.na(userLL))){stop("We can't find the latitude and longitude of your location from Twitter")}

        ## Find the latitude and longitude of each of the followers/following
        ## and calcualte the distance to the user
        followersLL = matrix(NA, nrow = length(followers), ncol = 4)
        followingLL = matrix(NA, nrow = length(following), ncol = 4)

        for(i in 1:length(followers)){
                if(length(followersLocation[[i]]) > 0){
                        tmpLL = findLatLon(trim(followersLocation[[i]]))
                        if(any(!is.na(tmpLL$latlon))){
                                followersLL[i,] = c(unlist(tmpLL$latlon),distCosine(userLL,tmpLL$latlon),unlist(tmpLL$cont))
                        }
                }
        }

        for(i in 1:length(following)){
                if(length(followingLocation[[i]]) > 0){
                        tmpLL = findLatLon(trim(followingLocation[[i]]))
                        if(any(!is.na(tmpLL$latlon))){
                                followingLL[i,] =  c(unlist(tmpLL$latlon),distCosine(userLL,tmpLL$latlon),unlist(tmpLL$cont))
                        }
                }
        }

        followingLL = followingLL[order(-followingLL[,3]),]
        followersLL = followersLL[order(-followersLL[,3]),]

        followingLL = followingLL[!is.na(followingLL[,1]),]
        followersLL = followersLL[!is.na(followersLL[,1]),]

######################################################################
        ## Visualisation code
        ## Leaflet library
        map <- Leaflet$new()
        map$tileLayer(provider = 'Stamen.TonerLite')
        map$set(width = 1600, height = 800)
        map$setView(c(20.0, 15.0), zoom = 3) # Latitude, Logitude logic

        for(i in 1:nrow(followersLL)){
                map$marker(followersLL[i, 2:1], bindPopup = paste("<p>",
                                                    format((followersLL[i, 3] / 1000),
                                                           big.mark=",", digits=2,
                                                           nsmall=2), " km away",
                                                    "</p>", sep = ""))
        }

        for(i in 1:nrow(followingLL)){
                map$marker(followingLL[i, 2:1], bindPopup = paste("<p>",
                                                    format((followingLL[i, 3]
                                                            / 1000),
                                                           big.mark=",",
                                                           digits=2,
                                                           nsmall=2),
                                                    " km away", "</p>", sep
                                                    = ""))
        }

        map$enablePopover(TRUE)
        map$fullScreen(TRUE)
        return(map)
}

## this function patches a problem with the lookup function
## ignore for now
patchLookupUsers <- function(users, ...)
        {
                batches <- split(users, ceiling(seq <- along(users)/100))
                results <- lapply(batches, function(batch) {
                                          params <- twitteR:::parseUsers(batch)
                                          twitteR:::twInterfaceObj$doAPICall(paste("users", "lookup", sep = "/"),
                                                                             params = params, ...)
                                  })
                out <- sapply(do.call(c, results), twitteR:::buildUser)
                out
        }

## get a user's followers and their location
getFollowers <- function(user, nMax=1000, ...)
        {
                followers=patchLookupUsers(user$getFollowerIDs(n=nMax))
                followersLocation = sapply(followers,location)
                list(users=followers, location=followersLocation)
        }

## get latitude and longitude from a location name
findLatLon <- function(loc)
        {
                latlon = NA
                cont = NA

                ## Asia = 1, Africa = 2, North America = 3, South America = 4, Australia/New Zealand = 5, Europe = 6
                ## hand coding the continents, not pretty
                continents = matrix(NA,nrow=length(unique(world.cities[,2])),ncol=2)
                continents[,1] = unique(world.cities[,2])
                continents[1:10,2] = c(1,1,1,2,1,1,1,1,1,1)
                continents[11:20,2]= c(1,1,2,1,1,2,1,2,2,2)
                continents[21:30,2] = c(2,1,6,6,6,6,6,6,6,6)
                continents[31:40,2] = c(6,6,6,6,2,4,4,1,2,1)
                continents[41:50,2] = c(4,6,1,4,6,1,3,1,6,6)
                continents[51:60,2] = c(3,2,4,2,6,1,6,1,3,2)
                continents[61:70,2] = c(1,2,2,2,3,6,3,3,6,6)
                continents[71:80,2] = c(1,1,2,6,3,4,3,4,6,1)
                continents[81:90,2] = c(3,3,3,2,2,6,6,6,6,4)
                continents[91:100,2] = c(2,5,2,2,3,1,1,1,1,1)
                continents[101:110,2] = c(1,2,1,1,1,3,2,5,1,6)
                continents[111:120,2] = c(1,6,1,1,2,6,1,1,6,2)
                continents[121:130,2] = c(6,6,6,1,1,3,4,3,4,2)
                continents[131:140,2] = c(6,6,2,2,1,1,1,4,1,1)
                continents[141:150,2] = c(1,2,2,1,1,1,4,6,6,2)
                continents[151:160,2] = c(4,1,1,1,1,2,4,6,2,2)
                continents[161:170,2] = c(1,2,2,1,6,2,1,1,6,1)
                continents[171:180,2] = c(1,1,1,2,6,2,2,6,1,1)
                continents[181:190,2] = c(2,6,2,1,6,6,3,3,3,3)
                continents[191:200,2] = c(2,2,2,2,3,2,3,2,3,1)
                continents[201:210,2] = c(3,2,2,2,2,2,2,1,6,2)
                continents[211:220,2] = c(1,3,1,6,2,4,3,6,3,4)
                continents[221:230,2] = c(1,1,1,3,2,3,3,6,1,6)
                continents[231:232,2] = c(2,1)


                ## Get the first element of the location
                ## firstElement = strsplit(loc,"[^[:alnum:]]")[[1]][1]
                firstElement = strsplit(loc,",")[[1]][1]
                if(is.na(firstElement)){firstElement="zzzzzzzzz"}

                ## See if it is a city
                tmp = grep(firstElement,world.cities[,1],fixed=TRUE)
                tmp2 = grep(firstElement,state.name,fixed=TRUE)
                tmp3 = grep(firstElement,world.cities[,2],fixed=TRUE)

                if(length(tmp) == 1){
                        latlon = world.cities[tmp,c(5,4)]
                        cont = continents[which(world.cities[tmp,2]==continents[,1]),2]
                }else if(length(tmp) > 1){
                        tmpCities = world.cities[tmp,]
                        latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
                        cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
                }else if(length(tmp2) == 1){
                        latlon = c(state.center$x[tmp2],state.center$y[tmp2])
                        cont = 3
                }else if(length(tmp3) > 0){
                        tmpCities = world.cities[tmp3,]
                        latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
                        cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
                }

                return(list(latlon=latlon,cont=as.numeric(cont)))

        }

## get points along the great circle between two locations
getGreatCircle <- function(userLL,relationLL)
        {
                tmpCircle = greatCircle(userLL,relationLL)
                start = which.min(abs(tmpCircle[,1] - userLL[1,1]))
                end = which.min(abs(tmpCircle[,1] - relationLL[1]))
                greatC = tmpCircle[start:end,]
                return(greatC)
        }

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

######################################################################
### global.R ends here
