##############################################################
### parsing script for animex data with rich text ############
### edited for new sql query with only one rich text field ###
### Author: Antoine Champetier, 3RCC #########################
### date: 12.06.2025 #########################################
##############################################################

# This script is designed to parse the data coming from sql queries from animex where rich text is an issue
# the script performs a few basic tests to check that the resulting data is correctly conformed.
# the script uses two datafiles, one with the rich text fields and one without. 
# the data without rich text fields is only used to check that nothing in fields that are not rich text are causing issues and that the reading of the full datafile is not interrupted (checking number of lines)

# this version assumes that there are 1 fields with rich text causing issues:
# 1) rich text
# in addition, there must be a line start and end marker.
# the number of rich text fields and order can be easily adjusted, with the delicate part being the loop "for(i in 1:line_count){...
# in theory (i.e. it has not been tested), this script should work for sql query with other the non-rich text columns (removing or adding fields between or before and after rich text)


# This is the sql query used to extract the data:
# StartLineMarker~APPLICATION_ID~NATIONAL_NUMBER~NATNR_valid_from~CANTONAL_NUMBER~DECISION_DATE~APPLICATION_STATUS~TYPE~PUBLICATION_TITLE~TITLE~SEVERITY_DEGREE~RichtextMarker1~RichTEXT~RichtextMarker2~EDITOR_NAME~AREA_OF_APPLICATION~PREDOMINANT_USE~OTHER_USE~OTHER_STUDIES~DISEASE_TYPE~HUMAN_DISEASES~OTHER_DISEASE~ANIMAL_DISEASES~LAW_PROCEDURES~GUIDELINES_TEXT~AnimalCategories~EndLineMarker
# #MARK_START_LINE#



## set up ####
rm(list = ls())

library(plyr)
library(dplyr)
#library(readxl)

## paths to the data files, one with all rich texts, the second with only fields that don't contain rich text
filepath_1 <-  "O:/CUG/FBTV/Education_Training_3RCC/4th Extract/ANIMEX_Applications_selection220224.txt"
filepath_2 <-  "O:/CUG/FBTV/Education_Training_3RCC/4th Extract/ANIMEX_Applications_selection220224_ohneRTR.txt"


## definition of the markers used in the sql extraction.
start_line_pattern <- "#MARK_START_LINE#"

pre_richtext_pattern <- "#MARK_RICH_TEXT_1#"
post_richtext_pattern <- "#MARK_RICH_TEXT_2#"

end_line_pattern <- "#MARK_END_LINE#"

## importing of data without rich text to check issues in remaining fields and to get the total number of lines in data ####

## read the no-text file
df_applications_ohne<-read.delim(filepath_2,
                                 sep='~',
                                 quote="",
                                 header=TRUE,
                                 fileEncoding="UTF-16LE",
                                 allowEscapes = FALSE)


## test that start and end lines as well as markers are well conformed.
## the principle of the check is simply that there should be only markers in the marker columns.
no_error_indicator <- TRUE # indicator that remains true only if all marker tests are passed.
if( unique(df_applications_ohne$StartLineMarker) != c(start_line_pattern)){
  print("start line error")
  no_error_indicator <- FALSE
}

if( unique(df_applications_ohne$EndLineMarker) != c(end_line_pattern)){
  print("end line error")
  no_error_indicator <- FALSE
}

if( unique(df_applications_ohne$RichtextMarker1) != c(pre_richtext_pattern)){
  print("rich text error")
  no_error_indicator <- FALSE
}

if( unique(df_applications_ohne$RichtextMarker2) != c(post_richtext_pattern)){
  print("rich text error")
  no_error_indicator <- FALSE
}

if(no_error_indicator){
  print("no parsing errors detected in file without text fields")
}

## saving the number of rows and the list of variables which will be checked with the full data file.
number_rows_data_no_text <- nrow(df_applications_ohne) 
list_variables_no_text <- names(df_applications_ohne)
rm(df_applications_ohne, no_error_indicator)

## importing the full data ####


df_applications<-read.delim(filepath_1,
                            sep='~',
                            quote="",
                            header=TRUE,
                            fileEncoding="UTF-16LE",
                            allowEscapes = FALSE)



## Turning dataframe into vector-shaped dataframe
vec_applications = as.vector(t(as.matrix(df_applications)))
df_vec_applications<- data.frame(vec_applications)


## finding marker locations
mark_line_start = which(df_vec_applications$vec_applications == start_line_pattern)
mark_rich_text_start = which(df_vec_applications$vec_applications == pre_richtext_pattern)
mark_rich_text_end = which(df_vec_applications$vec_applications == post_richtext_pattern)
mark_line_end = which(df_vec_applications$vec_applications == end_line_pattern)


## checking that all markers are present in same number
line_count <- length(mark_line_start) 
if(line_count == length(mark_line_end)&
   line_count == length(mark_rich_text_start)&
   line_count == length(mark_rich_text_end)  ){
  print("all markers check ok")
}


## checking that the lines are the same as the according to the marker count and the no-text file
if( i != line_count){
  print("error: the number of lines re-parsed is not the same as the lines in the no-text file")
} else {
  print("line count matches between marker check and the no-text file import")
}


## building a dataframe of marker position for ease of reference
markers <- data.frame("line_start" = mark_line_start, 
                      "rich_text_start" = mark_rich_text_start,
                      "rich_text_end" = mark_rich_text_end,
                      "line_end" = mark_line_end)

## calculating distance between marker columns.
markers$dist_1 <- markers$rich_text_start - markers$line_start-1
markers$dist_3 <- markers$line_end - markers$rich_text_end-1

## checking that the distanced between markers are consistent across lines
if(length(unique(markers$dist_1))==1 &
   length(unique(markers$dist_2))==1 ){
  distance_texts <- c(unique(markers$dist_1),
                      unique(markers$dist_2))
  print("all distances between markers consistent across file")
}

if(sum(distance_texts) + 4 + 1 == ncol(df_applications)){ # 4 markers and 1 text column
  print("sum of distances plus markers plus text fields adds up to columns source file")
}else {
  print("error in number of columns outside of rich text and markers. this error may be due to a change in the sql query or a change in the data structure")
}

if(sum(distance_texts) + 4 == length(list_variables_no_text)){ #note that the 4 text fields are not in the file without text!!
  print("sum of distances plus markers plus text fields adds up to columns in the file without text")
}else {
  print("error as columns in the file without text do not match the number based on marker tracking. this error may be due to a change in the sql query or a change in the data structure")
}


# building an empty dataframe to fill line by line
df_vec <- c( df_vec_applications$vec_applications)
df <- df_applications[c(),]

## this is the key loop where "re-parsing" is done.
for(i in 1:line_count){
  temp_line <- c( df_vec[(mark_line_start[i]):(mark_rich_text_start[i])], # for normal columns, the import is direct
                  toString(df_vec[(mark_rich_text_start[i]+1):(mark_rich_text_end[i]-1)]), # all "stuff" between markers for one rich-text colum are combined in a single string 
                  df_vec[(mark_rich_text_end[i]):(mark_line_end[i])])
  #
  if( length(temp_line) != ncol(df_applications)){
    print(paste("error in pasting line number: ",i)) # this is an additional (and hopefully redundan) test to catch any cases that would have slipped through in previous checks
  }
  df <- rbind(df,temp_line ) # adding the temporary line to the reparsed dataframe
}


names(df)<- names(df_applications) # in case names have been altered

save(df, file = "ANIMEX_Applications_selection220224.Rdata") # the data is saved in Rdata format
# note that the text fields still need cleaning! there will be spaced and periods added by this reparsing, and html tags and other text formating
