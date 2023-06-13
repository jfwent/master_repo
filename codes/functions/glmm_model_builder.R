# custom glmm function with glmmTMB package

function_glmm <- function(my_data, biodiv_ind){
  
  library(glmmTMB)
  
  valid_options <- c("shannon", "simpson", "richness")
  
  if(!biodiv_ind %in% valid_options){
    stop("Invalid value for 'biodiv_ind'. It must be one of: ", paste(valid_options, collapse = ", "))
  }
  
  if(biodiv_ind == "shannon" ){
    
    model <- glmmTMB(shannon ~ urban + forest + grass + pasture + crop + wet + barren + (1 | Ecoregion),
                     data = my_data,
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim)
    )
    
  } else if( biodiv_ind == "richness" ) {
    
    model <- glmmTMB(richness ~ urban + forest + grass + pasture + crop + wet + barren + (1 | Ecoregion),
                     data = my_data,
                     family = poisson(link = "log"),
                     control = glmmTMBControl(optimizer = optim)
    )
    
  } else {
    
    model <- glmmTMB(simpson ~ urban + forest + grass + pasture + crop + wet + barren + (1 | Ecoregion),
                     data = my_data,
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim)
    )
  }
  
  return(model)
  
}

