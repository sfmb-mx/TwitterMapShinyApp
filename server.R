### server.R --- 
## 
## Filename: server.R
## Description: 
## Author: Sergio-Feliciano Mendoza-Barrera
## Maintainer: 
## Created: Sat Apr 25 23:15:16 2015 (-0500)
## Version: 
## Package-Requires: ()
## Last-Updated: Sat Apr 25 23:18:06 2015 (-0500)
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
## @knitr server
require(shiny)
require(rCharts)

shinyServer(function(input, output, session){

                    observeEvent(input$SubmitButton, {

                                         output$minibuffer <- renderText("*** Loading Twitter data, please wait...:")

                                         output$map_container <- renderMap({
                                             ##... # do some work
                                              output$minibuffer <- renderText("*** Network map (Max. 2000 contacts) DONE for:")
                                             output$minibufferUser <- renderText(input$userName)
                                             plotMap(input$userName)

                                     })
                                 })
            })

######################################################################
### server.R ends here
