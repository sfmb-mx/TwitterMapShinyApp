### ui.R --- 
## 
## Filename: ui.R
## Description: 
## Author: Sergio-Feliciano Mendoza-Barrera
## Maintainer: 
## Created: Sat Apr 25 23:15:16 2015 (-0500)
## Version: 
## Package-Requires: ()
## Last-Updated: Sat Apr 25 23:19:52 2015 (-0500)
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

require(shiny)
require(rCharts)

shinyUI(bootstrapPage( 
    tags$link(href='style.css', rel='stylesheet'),
    tags$script(src='app.js'),
    includeHTML('www/credits.html'),
    fluidRow(
        column(3,
               p('*** Please enter your username, then submit.'),
               p('*** Please wait after submit...'),
               textOutput('minibuffer'),
               textOutput('minibufferUser')
               ),

        column(3#,
               ),
        column(3#,
               ),
        column(3,
               textInput("userName", label =
                             h4("Enter your username"), value =
                                 "mitxalumni"),
               actionButton("SubmitButton", "Submit")
               )
        ),
        mapOutput('map_container')
))

######################################################################
### ui.R ends here
