---
  title: "Stat 21 Final Project"
author: "Swarthmore College"
date: "Due: Nov 10th, by noon ET"
output:
  pdf_document: 
  toc: no
html_document:
  code_folding: show
toc: yes
toc_float: yes
urlcolor: blue
---
  
```{asis, directions=TRUE}

This assignment is due on to be submitted on Gradescope on __November 10__ by __12:00pm ET__. Please use the `homework-q-and-a` channel on Slack to post any related questions or error messages. 

***General instructions for all assignments***: 
  
  You must submit your completed assignment as a single __PDF__ document uploaded to __Gradescope__. For instructions on how to do this, please watch this 2 minute video: https://youtu.be/KMPoby5g_nE. You must use R markdown to write up your solutions. For any homework problems that involve coding in R, you must provide __both__ the code and the requested output. You can find a R markdown homework template here: http://www.swarthmore.edu/NatSci/sthornt1/Stat021/Stat21.html. Please make sure each problem is __clearly labeled__ and that any handwritten components (such as pictures or equations) are easily readable as pictures within the R markdown document. You may want to use a service like CamScanner (https://www.camscanner.com/) to help you upload handwritten pages. 

You are allowed to work with your classmates on this homework assignment but you must disclose the names of anyone you collaborate with at the top of your solutions. Each homework assignment is worth 20 points. One problem will be chosen at random to be graded for correctness and the other problems will be graded for completion. At the end of the semester, your lowest homework grade will be dropped. No homework solutions will be provided.  

+ Use this file as the template for your submission. You can delete unnecessary text (e.g. these instructions) but make sure to keep the nicely formatted "Problem 1", "Problem 2", "a.", "b.", etc
+ Upload your knitted HTML or PDF file to the Homework 1 submission section on Gradescope.  
Name this file as: [SwatID]_stat21_hw06.pdf (e.g.  and "sthornt1_stat21_hw06.pdf"). You only need to upload one file, but please make sure that your graphs, code, and answers to each question appear in the appropriate place. If we cannot see your code/graphs/answers, we cant give you credit for your work!
  + Your file should contain the code to answer each question in its own code block. Your code should produce plots/output that will be automatically embedded in the output file.
+ Each answer must be supported by written statements (unless otherwise specified).
+ Include the name of anyone you collaborated with at the top of the assignment.
+ In order to knit this document, make sure you have installed the following packages in your version of RStudio: `ggplot2`, `tidyverse`, `gridExtra`, `gcookbook`, `knitr`

***
```


```{r setup, include=FALSE}
###########################
# DEFAULT SETTINGS
###########################
knitr::opts_chunk$set(message = FALSE) # include this if you don't want markdown to knit messages
knitr::opts_chunk$set(warning = FALSE) # include this if you don't want markdown to knit warnings
knitr::opts_chunk$set(echo=TRUE) # set echo=FALSE to hide code from html output

###########################
# LIBRARIES
###########################
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(gcookbook)
library(knitr)
```



2) Build your own statistical model: ANOVA, SLR, or MLR. If you do this option I ask that you limit 


https://dataverse.harvard.edu/dataverse/harvard/
https://www.census.gov/data/academy/data-gems/2018/api.html
https://opendataphilly.org/
  
  
  