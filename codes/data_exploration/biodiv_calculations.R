##### div indices calculations

#load data
rm(list=ls())

load("data/Lica/BBS_partition_abundance.rda")
usbbs.data <- BBS_partition_abundance; rm(BBS_partition_abundance)


#=========


years <- unique(usbbs.data$year) # insert your timepoint
fun.vector <- c(min, mean, max)
fun.names <- c("min", "mean", "max") 
n <- 1 # position in function aggregate naming vector 

for (fun.now in fun.vector){
  
  for(y in years){
    
    # set up loop
    segments <- unique(usbbs.data$partition)
    i <- 1
    new.matrix <- TRUE
    
    if(y==2001)
      usbbs.temp <- usbbs.data %>% filter( year == 2001 )
    if(y==2016)
      usbbs.temp <- usbbs.data %>% filter( year == 2016 )  
    
    for(segment.now in segments){
      
      sp.matrix.now <- usbbs.temp %>%
        dcast(partition ~ species, fun.aggregate = fun.now,  value.var = segment.now, fill = 0) %>% 
        as_tibble() 
      
      # modify partition name
      sp.matrix.now$partition <- paste0(sp.matrix.now$partition,"_",i)
      
      if(new.matrix==T)
        sp.matrix <- sp.matrix.now else
          sp.matrix <- rbind(sp.matrix, sp.matrix.now) # add on each other
      
      new.matrix <- FALSE
      
      #update partition segment number 
      i <- i+1
    }
  }   
}