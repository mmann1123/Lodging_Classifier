#' 
#' Assisted Label generation for images
#' 
#'
#' @param database: path and name of RDS or dta file as provided by IFPRI with images where labels will be stored
#' @param image_path: path of folder holding aoi images to be used for training
#' @param image_ext: extension type of image for example .jpg
#' @param in_label: dta or rds where additional labels are stored
#' @keywords gcc calculation, QA/GC
#' @export

 

ImageLabeler = function(image_path = '/media/ssd/Lodging_Classifier/aoi/',image_ext = '.jpg',
                        database = '/media/ssd/Lodging_Classifier/cropmonitor_merged.rds', in_label = NULL){
  
    plot_jpeg = function(path, add=FALSE){
      require('jpeg',quietly = T)
      jpg = readJPEG(path, native=T) # read the file
      res = dim(jpg)[2:1] # get the resolution, [x, y]
      if (!add) # initialize an empty plot area if add==FALSE
        plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=0,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
      rasterImage(jpg,1,1,res[1],res[2])
    }
    
    
    # get list of all images 
    image_list = list.files(image_path,pattern = paste0('*',image_ext),recursive = T,full.names = T)
    #head(image_list)
    
    # read in database
    database_ext = strsplit(database,".", fixed = TRUE)[[1]][2]
    if(database_ext =='rds' | database_ext == 'RDS') in_data = readRDS(database) else
      if(database_ext =='dta' ){require(readstata13); in_data = read.dta13(database)} else
        print('database type unknown please provide dta or rds file')
    
    # get any existing labels
    if(!is.null(in_label)){
      in_label = readRDS('/media/ssd/crop_image_classifier/Data/cropmonitor_subset.rds')
      in_data[basename(in_data$image) %in% in_label$image_name[in_label$lodging=='No'] ,'Lodging'] = 'False'
      in_data[basename(in_data$image) %in% in_label$image_name[in_label$lodging=='Yes'] ,'Lodging'] = 'True'
    }
    
    
    for(i in 1:length(image_list)){
      a_image = image_list[i]
      # skip completed
      cur_value = in_data[grepl(basename(a_image), in_data$image),'Lodging']
      if(!is.na(cur_value)){print(i);print('isnt na');next} 
      
      # plot image and query user
      plot_jpeg(a_image)
      response = readline(prompt="Lodging: y, n, r (replace last),q (quit) ")
      # store response  
      if(response == 'y'|response == 'Y'){
        in_data[grepl(basename(a_image), in_data$image),'Lodging'] = 'True'
      } else if(response == 'n'|response == 'N'){
        in_data[grepl(basename(a_image), in_data$image),'Lodging'] = 'False'
      } else if(response == 'r'|response == 'R'){
        in_data[grepl(basename(image_list[i-1]), in_data$image),'Lodging'] = NA
        response = readline(prompt="Lodging: y n or r (replace last) ")
      if(response == 'y'|response == 'Y'){
          in_data[grepl(basename(a_image), in_data$image),'Lodging'] = 'True'
      }
      if(response == 'n'|response == 'N'){
          in_data[grepl(basename(a_image), in_data$image),'Lodging'] = 'False'
      }
      } else if(response == 'q'|response == 'Q'){
          print('saving')
          saveRDS(in_data,file = database) 
          break
      } else{
          in_data[grepl(basename(a_image), in_data$image),'Lodging'] = NA
      }
      saveRDS(in_data,file = database) 
    }
    
    print(table(in_data$Lodging))
}


# Example:
image_path = '/media/ssd/Lodging_Classifier/sorted/'
image_ext = '.jpg'
database = '/media/ssd/Lodging_Classifier/cropmonitor_merged.rds'
in_label = '/media/ssd/crop_image_classifier/Data/Pictures Data CLEAN 04_21_17.dta'
ImageLabeler(image_path,image_ext,database, in_label)

  