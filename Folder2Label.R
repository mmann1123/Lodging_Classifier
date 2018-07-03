
#' 
#' Takes label folders for tensor training and stores labels  in the image database
#' 
#' Files should be sorted in the following order:
#' out_image_path:
#'       LabelFolder1:
#'            TrueFolder:
#'                Image_i.jpg
#'            FalseFolder: 
#'                Image_i.jpg
#'            OtherFolder:   
#'      LabelFolder2:
#'            TrueFolder:
#'                Image_i.jpg
#'            FalseFolder:
#'            OtherFolder:       
#'  
#'
#' @param database: path and name of RDS or dta file as provided by IFPRI with images where labels will be stored
#' @param image_ext: extension type of image for example .jpg
#' @param out_image_path: path of folder where images were sorted for training
#' @param image_column: column name with all 
#' @keywords label sort
#' @export
#' 
#' 


Folder2Label = function(database,image_path,image_ext='.jpg',out_image_path,image_colum){
  require(tidyr)
  require(dplyr)
  # read in database
  database_ext = strsplit(database,".", fixed = TRUE)[[1]][2]
  if(database_ext =='rds' | database_ext == 'RDS') in_data = readRDS(database) else
    if(database_ext =='dta' ){require(readstata13); in_data = read.dta13(database)} else
      print('database type unknown please provide dta or rds file')
  
  # add file name column to join on 
  in_data$file_name =  basename(as.character(in_data[,image_column]) )
  
  
    # find images to be sorted
  file_path = list.files(out_image_path,pattern = image_ext,recursive = T, full.names = T)

  
  # split path into column name for database, label, and file name
  grab_last_dir = function(x) unlist(strsplit(dirname(x),'/'))[length(unlist(strsplit(dirname(x),'/')))]
  grab_2ndlast_dir = function(x) unlist(strsplit(dirname(x),'/'))[length(unlist(strsplit(dirname(x),'/')))-1]
  
  
  # create table with all needed data spread labels across columns from folder names
  label_tb = tibble(column = sapply(file_path, grab_2ndlast_dir,USE.NAMES = F),
                    label = sapply(file_path, grab_last_dir,USE.NAMES = F),
                    file_name = basename(file_path),file_path=file_path)
  label_tb = label_tb %>%  spread(column, label)
  
  
  # join to original data 
  out_data = left_join(in_data,label_tb, by='file_name')
  
  return(out_data)
} 

# 
# # Example
# image_ext = '.jpg'
# database = '/media/ssd/Lodging_Classifier/Data/cropmonitor_merged_updated_images.rds'
# out_image_path = '/media/ssd/Lodging_Classifier/Data/sorted'
# in_data = Folder2Label(database,image_path,image_ext='.jpg',out_image_path)
# 
# table(in_data$Lodging)
# table(in_data$Soil.y)
# table(in_data$OtherIssues.y)
# table(in_data$Harvest.y)

# library(imager)
# file2plot = basename(in_data[in_data$Soil == 'False' & !is.na(in_data$Soil),'image' ]  )
# plot(load.image((paste0('/media/ssd/Lodging_Classifier/Data/sorted/Soil/True/',file2plot[50]))))
# 
# saveRDS(in_data,'/media/ssd/Lodging_Classifier/Data/cropmonitor_merged_updated_images.rds')




