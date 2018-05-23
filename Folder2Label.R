
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
#' @keywords label sort
#' @export
#' 
#' 


Folder2Label = function(database,image_path,image_ext='.jpg',out_image_path){
    require(dplyr)
    # read in database
    database_ext = strsplit(database,".", fixed = TRUE)[[1]][2]
    if(database_ext =='rds' | database_ext == 'RDS') in_data = readRDS(database) else
      if(database_ext =='dta' ){require(readstata13); in_data = read.dta13(database)} else
        print('database type unknown please provide dta or rds file')
    
    # find images to be sorted
    files_to_process = list.files(out_image_path,pattern = image_ext,recursive = T, full.names = T)
    
    # split path into column name for database, label, and file name
    grab_last_dir = function(x) unlist(strsplit(dirname(x),'/'))[length(unlist(strsplit(dirname(x),'/')))]
    grab_2ndlast_dir = function(x) unlist(strsplit(dirname(x),'/'))[length(unlist(strsplit(dirname(x),'/')))-1]
    
    column = sapply(files_to_process, grab_2ndlast_dir,USE.NAMES = F)
    label = sapply(files_to_process, grab_last_dir,USE.NAMES = F)
    file = basename(files_to_process)
    
    # create database columns with correct names 
    in_data[ unique(column)] = NA
     
    # create image name in in_data
    in_data$file = basename(in_data$image)
    
    label_data = data.frame(column=column, label= label, file=file)
    
    # for ecah unique column name add appropriate labels
    for(column_name in unique(column)){
        
        in_data = left_join(in_data, label_data, by = 'file')
      
        # # find file location in database (returns row# in files_to_process that matches database image location) use 
        # # e.g. label[match_order] file[match_order]
        # match_order = match( basename(in_data$image), file[column==column_name] )
        # 
        # # replace values of _sorted column with appropriate label
        # in_data[,column_name] = label[match_order]
        
    }
    return(in_data)
} 
  
  
# Example 
image_ext = '.jpg'
database = '/media/ssd/Lodging_Classifier/Data/cropmonitor_merged_updated_images.rds'
out_image_path = '/media/ssd/Lodging_Classifier/Data/sorted/'
in_data = Folder2Label(database,image_path,image_ext='.jpg',out_image_path)

table(in_data$Lodging)
table(in_data$Soil)
table(in_data$OtherIssues)
table(in_data$Harvest)

library(imager)
file2plot = basename(in_data[in_data$Soil == 'False' & !is.na(in_data$Soil),'image' ]  )
plot(load.image((paste0('/media/ssd/Lodging_Classifier/Data/sorted/Soil/True/',file2plot[50]))))

saveRDS(in_data,'/media/ssd/Lodging_Classifier/Data/cropmonitor_merged_updated_images.rds')




