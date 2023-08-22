####################################################################
# load data
####################################################################

original_data <-read.csv(paste0(TraitDataFolder,'AVONET_Duplicate_Data.csv'),h=T)

####################################################################
# data processing and cleaning
####################################################################
#Select only relevant columns.
combined_data <- original_data[c(1,8:18,21:22)]

#Select only duplicate Measurerment pairs.
pair_data <- subset(combined_data,Sample.Number == "1" | Sample.Number == "2")

#General annotations
strict <- 'Standardised methods'
not_strict <- 'Non-standardised methods'
italic_R <- 'paste(italic(R), \" =\")'
italic_n <- 'paste(", ",italic(n), \" =\")'

#######################################################################
# make figure 3 and 4 - [Beak culmen] compare Measure 1 vs. Measure 2
#######################################################################
##Beak length (culmen)
#Select trait, split pairs into separate columns, and remove pairs with NAs
beak_culmen <- pair_data[c(1:2,12:13)]
names(beak_culmen)[2] <- "Beak.culmen"
beak_culmen_M1 <-subset(beak_culmen,Sample.Number=="1")
beak_culmen_M2 <-subset(beak_culmen,Sample.Number=="2")
beak_culmen_split <- merge(beak_culmen_M1,beak_culmen_M2,by="Unique.Specimen.Identifier")
beak_culmen_split <- beak_culmen_split[complete.cases(beak_culmen_split),]

#Calculate, mean, sd and cv.
beak_culmen_split$mean <- apply(beak_culmen_split[, c("Beak.culmen.x","Beak.culmen.y")],1,mean)
beak_culmen_split$sd <- apply(beak_culmen_split[, c("Beak.culmen.x","Beak.culmen.y")],1,sd)
beak_culmen_split$cv <- (beak_culmen_split$sd / beak_culmen_split$mean) * 100

#Calculate mean_Var for each trait, correlation coefficient and number of samples
#Standardised methods
data_beak_culmen_s <- subset(beak_culmen_split, Protocol.both.pairs.x == '1')
beak_culmen_mean_var_s <- round(mean(data_beak_culmen_s$cv),digits = 2)
beak_culmen_sample_numbers_s <- dplyr::count(data_beak_culmen_s)
beak_culmen_cor_s <- round(cor(data_beak_culmen_s$Beak.culmen.x,data_beak_culmen_s$Beak.culmen.y),3)

#Non-standardised methods
data_beak_culmen_ns <- subset(beak_culmen_split, Protocol.both.pairs.x == '0')
beak_culmen_mean_var_ns <- round(mean(data_beak_culmen_ns$cv),digits = 2)
beak_culmen_sample_numbers_ns <- dplyr::count(data_beak_culmen_ns)
beak_culmen_cor_ns <- round(cor(data_beak_culmen_ns$Beak.culmen.x,data_beak_culmen_ns$Beak.culmen.y),3)

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
#Standardised methods
scatter_beak_culmen_s <- 
  ggplot(data_beak_culmen_s, aes(x=Beak.culmen.x, y=Beak.culmen.y))+
  geom_point() +
  annotate('text',label=paste(italic_R,beak_culmen_cor_s,
                              italic_n,beak_culmen_sample_numbers_s,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('Beak length (culmen): ', beak_culmen_mean_var_s, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,450)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,450)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(2,0,0,0), "cm"))
scatter_beak_culmen_s

#Standardised methods
scatter_beak_culmen_ns <- 
  ggplot(data_beak_culmen_ns, aes(x=Beak.culmen.x, y=Beak.culmen.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,beak_culmen_cor_ns,
                              italic_n,beak_culmen_sample_numbers_ns,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('Beak length (culmen): ', beak_culmen_mean_var_ns, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,450)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,450)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(2,0,0,0), "cm"))
scatter_beak_culmen_ns

#######################################################################
# make figure 3 and 4 - [Beak nares] compare Measure 1 vs. Measure 2
#######################################################################
##Beak length (nares)
#Select trait, split pairs into separate columns, and remove pairs with NAs
beak_nares <- pair_data[c(1,3,12:13)]
names(beak_nares)[2] <- "Beak.nares"
beak_nares_M1 <-subset(beak_nares,Sample.Number=="1")
beak_nares_M2 <-subset(beak_nares,Sample.Number=="2")
beak_nares_split <- merge(beak_nares_M1,beak_nares_M2,by="Unique.Specimen.Identifier")
beak_nares_split <- beak_nares_split[complete.cases(beak_nares_split),]

#Calculate, mean, sd and cv.
beak_nares_split$mean <- apply(beak_nares_split[, c("Beak.nares.x","Beak.nares.y")],1,mean)
beak_nares_split$sd <- apply(beak_nares_split[, c("Beak.nares.x","Beak.nares.y")],1,sd)
beak_nares_split$cv <- (beak_nares_split$sd / beak_nares_split$mean) * 100

#Calculate mean_Var for each trait, correlation coefficient and number of samples
#Standardised methods
data_beak_nares_s <- subset(beak_nares_split, Protocol.both.pairs.x == '1')
beak_nares_mean_var_s <- round(mean(data_beak_nares_s$cv),digits = 2)
beak_nares_sample_numbers_s <- dplyr::count(data_beak_nares_s)
beak_nares_cor_s <- round(cor(data_beak_nares_s$Beak.nares.x,data_beak_nares_s$Beak.nares.y),3)

#Non-standardised methods
data_beak_nares_ns <- subset(beak_nares_split, Protocol.both.pairs.x == '0')
beak_nares_mean_var_ns <- round(mean(data_beak_nares_ns$cv),digits = 2)
beak_nares_sample_numbers_ns <- dplyr::count(data_beak_nares_ns)
beak_nares_cor_ns <- round(cor(data_beak_nares_ns$Beak.nares.x,data_beak_nares_ns$Beak.nares.y),3)

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
#Standardised methods
scatter_beak_nares_s <- 
  ggplot(data_beak_nares_s, aes(x=Beak.nares.x, y=Beak.nares.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,beak_nares_cor_s,
                              italic_n,beak_nares_sample_numbers_s,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('Beak length (nares): ', beak_nares_mean_var_s, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,145)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,145)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(2,0,0,0), "cm"))
scatter_beak_nares_s

#Standardised methods
scatter_beak_nares_ns <- 
  ggplot(data_beak_nares_ns, aes(x=Beak.nares.x, y=Beak.nares.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,beak_nares_cor_ns,
                              italic_n,beak_nares_sample_numbers_ns,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('Beak length (nares): ', beak_nares_mean_var_ns, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,145)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,145)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(2,0,0,0), "cm"))
scatter_beak_nares_ns

#######################################################################
# make figure 3 and 4 - [Beak width] compare Measure 1 vs. Measure 2
#######################################################################
##Beak width
#Select trait, split pairs into separate columns, and remove pairs with NAs
beak_width <- pair_data[c(1,4,12:13)]
names(beak_width)[2] <- "Beak.width"
beak_width_M1 <-subset(beak_width,Sample.Number=="1")
beak_width_M2 <-subset(beak_width,Sample.Number=="2")
beak_width_split <- merge(beak_width_M1,beak_width_M2,by="Unique.Specimen.Identifier")
beak_width_split <- beak_width_split[complete.cases(beak_width_split),]

#Calculate, mean, sd and cv.
beak_width_split$mean <- apply(beak_width_split[, c("Beak.width.x","Beak.width.y")],1,mean)
beak_width_split$sd <- apply(beak_width_split[, c("Beak.width.x","Beak.width.y")],1,sd)
beak_width_split$cv <- (beak_width_split$sd / beak_width_split$mean) * 100

#Calculate mean_Var for each trait, correlation coefficient and number of samples
#Standardised methods
data_beak_width_s <- subset(beak_width_split, Protocol.both.pairs.x == '1')
beak_width_mean_var_s <- round(mean(data_beak_width_s$cv),digits = 2)
beak_width_sample_numbers_s <- dplyr::count(data_beak_width_s)
beak_width_cor_s <- round(cor(data_beak_width_s$Beak.width.x,data_beak_width_s$Beak.width.y),3)

#Non-standardised methods
data_beak_width_ns <- subset(beak_width_split, Protocol.both.pairs.x == '0')
beak_width_mean_var_ns <- round(mean(data_beak_width_ns$cv),digits = 2)
beak_width_sample_numbers_ns <- dplyr::count(data_beak_width_ns)
beak_width_cor_ns <- round(cor(data_beak_width_ns$Beak.width.x,data_beak_width_ns$Beak.width.y),3)

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
#Standardised methods
scatter_beak_width_s <- 
  ggplot(data_beak_width_s, aes(x=Beak.width.x, y=Beak.width.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,beak_width_cor_s,
                              italic_n,beak_width_sample_numbers_s,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('Beak width: ', beak_width_mean_var_s, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,110)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,110)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(2,0,0,0), "cm"))
scatter_beak_width_s

#Standardised methods
scatter_beak_width_ns <- 
  ggplot(data_beak_width_ns, aes(x=Beak.width.x, y=Beak.width.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,beak_width_cor_ns,
                              italic_n,beak_width_sample_numbers_ns,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('Beak width: ', beak_width_mean_var_ns, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,110)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,110)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(2,0,0,0), "cm"))
scatter_beak_width_ns

#######################################################################
# make figure 3 and 4 - [Beak depth] compare Measure 1 vs. Measure 2
#######################################################################
##Beak depth
#Select trait, split pairs into separate columns, and remove pairs with NAs
beak_depth <- pair_data[c(1,5,12:13)]
names(beak_depth)[2] <- "Beak.depth"
beak_depth_M1 <-subset(beak_depth,Sample.Number=="1")
beak_depth_M2 <-subset(beak_depth,Sample.Number=="2")
beak_depth_split <- merge(beak_depth_M1,beak_depth_M2,by="Unique.Specimen.Identifier")
beak_depth_split <- beak_depth_split[complete.cases(beak_depth_split),]

#Calculate, mean, sd and cv.
beak_depth_split$mean <- apply(beak_depth_split[, c("Beak.depth.x","Beak.depth.y")],1,mean)
beak_depth_split$sd <- apply(beak_depth_split[, c("Beak.depth.x","Beak.depth.y")],1,sd)
beak_depth_split$cv <- (beak_depth_split$sd / beak_depth_split$mean) * 100

#Calculate mean_Var for each trait, correlation coefficient and number of samples
#Standardised methods
data_beak_depth_s <- subset(beak_depth_split, Protocol.both.pairs.x == '1')
beak_depth_mean_var_s <- round(mean(data_beak_depth_s$cv),digits = 2)
beak_depth_sample_numbers_s <- dplyr::count(data_beak_depth_s)
beak_depth_cor_s <- round(cor(data_beak_depth_s$Beak.depth.x,data_beak_depth_s$Beak.depth.y),3)

#Non-standardised methods
data_beak_depth_ns <- subset(beak_depth_split, Protocol.both.pairs.x == '0')
beak_depth_mean_var_ns <- round(mean(data_beak_depth_ns$cv),digits = 2)
beak_depth_sample_numbers_ns <- dplyr::count(data_beak_depth_ns)
beak_depth_cor_ns <- round(cor(data_beak_depth_ns$Beak.depth.x,data_beak_depth_ns$Beak.depth.y),3)

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
#Standardised methods
scatter_beak_depth_s <- 
  ggplot(data_beak_depth_s, aes(x=Beak.depth.x, y=Beak.depth.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,beak_depth_cor_s,
                              italic_n,beak_depth_sample_numbers_s,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('Beak depth: ', beak_depth_mean_var_s, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,140)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,140)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(2,0,0,0), "cm"))
scatter_beak_depth_s

#Standardised methods
scatter_beak_depth_ns <- 
  ggplot(data_beak_depth_ns, aes(x=Beak.depth.x, y=Beak.depth.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,beak_depth_cor_ns,
                              italic_n,beak_depth_sample_numbers_ns,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('Beak depth: ', beak_depth_mean_var_ns, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,140)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,140)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(2,0,0,0), "cm"))
scatter_beak_depth_ns

#######################################################################
# make figure 3 and 4 - [Tarsus length] compare Measure 1 vs. Measure 2
#######################################################################
##Tarsus length
#Select trait, split pairs into separate columns, and remove pairs with NAs
tarsus_length <- pair_data[c(1,6,12:13)]
names(tarsus_length)[2] <- "tarsus.length"
tarsus_length_M1 <-subset(tarsus_length,Sample.Number=="1")
tarsus_length_M2 <-subset(tarsus_length,Sample.Number=="2")
tarsus_length_split <- merge(tarsus_length_M1,tarsus_length_M2,by="Unique.Specimen.Identifier")
tarsus_length_split <- tarsus_length_split[complete.cases(tarsus_length_split),]

#Calculate, mean, sd and cv.
tarsus_length_split$mean <- apply(tarsus_length_split[, c("tarsus.length.x","tarsus.length.y")],1,mean)
tarsus_length_split$sd <- apply(tarsus_length_split[, c("tarsus.length.x","tarsus.length.y")],1,sd)
tarsus_length_split$cv <- (tarsus_length_split$sd / tarsus_length_split$mean) * 100

#Calculate mean_Var for each trait, correlation coefficient and number of samples
#Standardised methods
data_tarsus_length_s <- subset(tarsus_length_split, Protocol.both.pairs.x == '1')
tarsus_length_mean_var_s <- round(mean(data_tarsus_length_s$cv),digits = 2)
tarsus_length_sample_numbers_s <- dplyr::count(data_tarsus_length_s)
tarsus_length_cor_s <- round(cor(data_tarsus_length_s$tarsus.length.x,data_tarsus_length_s$tarsus.length.y),3)

#Non-standardised methods
data_tarsus_length_ns <- subset(tarsus_length_split, Protocol.both.pairs.x == '0')
tarsus_length_mean_var_ns <- round(mean(data_tarsus_length_ns$cv),digits = 2)
tarsus_length_sample_numbers_ns <- dplyr::count(data_tarsus_length_ns)
tarsus_length_cor_ns <- round(cor(data_tarsus_length_ns$tarsus.length.x,data_tarsus_length_ns$tarsus.length.y),3)

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
#Standardised methods
scatter_tarsus_length_s <- 
  ggplot(data_tarsus_length_s, aes(x=tarsus.length.x, y=tarsus.length.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,tarsus_length_cor_s,
                              italic_n,tarsus_length_sample_numbers_s,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('Tarsus length: ', tarsus_length_mean_var_s, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,360)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,360)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(-3,0,0,0), "cm"))
scatter_tarsus_length_s

#Standardised methods
scatter_tarsus_length_ns <- 
  ggplot(data_tarsus_length_ns, aes(x=tarsus.length.x, y=tarsus.length.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,tarsus_length_cor_ns,
                              italic_n,tarsus_length_sample_numbers_ns,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('Tarsus length: ', tarsus_length_mean_var_ns, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,360)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,360)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(-3,0,0,0), "cm"))
scatter_tarsus_length_ns

#######################################################################
# make figure 3 and 4 - [Wing length] compare Measure 1 vs. Measure 2
#######################################################################
##Wing length
#Select trait, split pairs into separate columns, and remove pairs with NAs
wing_length <- pair_data[c(1,9,12:13)]
names(wing_length)[2] <- "wing.length"
wing_length_M1 <-subset(wing_length,Sample.Number=="1")
wing_length_M2 <-subset(wing_length,Sample.Number=="2")
wing_length_split <- merge(wing_length_M1,wing_length_M2,by="Unique.Specimen.Identifier")
wing_length_split <- wing_length_split[complete.cases(wing_length_split),]

#Calculate, mean, sd and cv.
wing_length_split$mean <- apply(wing_length_split[, c("wing.length.x","wing.length.y")],1,mean)
wing_length_split$sd <- apply(wing_length_split[, c("wing.length.x","wing.length.y")],1,sd)
wing_length_split$cv <- (wing_length_split$sd / wing_length_split$mean) * 100

#Calculate mean_Var for each trait, correlation coefficient and number of samples
#Standardised methods
data_wing_length_s <- subset(wing_length_split, Protocol.both.pairs.x == '1')
wing_length_mean_var_s <- round(mean(data_wing_length_s$cv),digits = 2)
wing_length_sample_numbers_s <- dplyr::count(data_wing_length_s)
wing_length_cor_s <- round(cor(data_wing_length_s$wing.length.x,data_wing_length_s$wing.length.y),3)

#Non-standardised methods
data_wing_length_ns <- subset(wing_length_split, Protocol.both.pairs.x == '0')
wing_length_mean_var_ns <- round(mean(data_wing_length_ns$cv),digits = 2)
wing_length_sample_numbers_ns <- dplyr::count(data_wing_length_ns)
wing_length_cor_ns <- round(cor(data_wing_length_ns$wing.length.x,data_wing_length_ns$wing.length.y),3)

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
#Standardised methods
scatter_wing_length_s <- 
  ggplot(data_wing_length_s, aes(x=wing.length.x, y=wing.length.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,wing_length_cor_s,
                              italic_n,wing_length_sample_numbers_s,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('Wing length: ', wing_length_mean_var_s, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,850)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,850)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(-3,0,0,0), "cm"))
scatter_wing_length_s

#Standardised methods
scatter_wing_length_ns <- 
  ggplot(data_wing_length_ns, aes(x=wing.length.x, y=wing.length.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,wing_length_cor_ns,
                              italic_n,wing_length_sample_numbers_ns,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('wing length: ', wing_length_mean_var_ns, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,850)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,850)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(-3,0,0,0), "cm"))
scatter_wing_length_ns

#######################################################################
# make figure 3 and 4 - [Kipp's distance] compare Measure 1 vs. Measure 2
#######################################################################
#Kipp's distance
#Select trait, split pairs into separate columns, and remove pairs with NAs
kipps <- pair_data[c(1,7,12:13)]
names(kipps)[2] <- "kipps"
kipps_M1 <-subset(kipps,Sample.Number=="1")
kipps_M2 <-subset(kipps,Sample.Number=="2")
kipps_split <- merge(kipps_M1,kipps_M2,by="Unique.Specimen.Identifier")
kipps_split <- kipps_split[complete.cases(kipps_split),]

#Calculate, mean, sd and cv.
kipps_split$mean <- apply(kipps_split[, c("kipps.x","kipps.y")],1,mean)
kipps_split$sd <- apply(kipps_split[, c("kipps.x","kipps.y")],1,sd)
kipps_split$cv <- (kipps_split$sd / kipps_split$mean) * 100

#Calculate mean_Var for each trait, correlation coefficient and number of samples
#Standardised methods
kipps_s <- subset(kipps_split, Protocol.both.pairs.x == '1')
kipps_mean_var_s <- round(mean(kipps_s$cv),digits = 2)
kipps_sample_numbers_s <- dplyr::count(kipps_s)
kipps_cor_s <- round(cor(kipps_s$kipps.x,kipps_s$kipps.y),3)

#Non-standardised methods
kipps_ns <- subset(kipps_split, Protocol.both.pairs.x == '0')
kipps_mean_var_ns <- round(mean(kipps_ns$cv),digits = 2)
kipps_sample_numbers_ns <- dplyr::count(kipps_ns)
kipps_cor_ns <- round(cor(kipps_ns$kipps.x,kipps_ns$kipps.y),3)

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
#Standardised methods
scatter_kipps_s <- 
  ggplot(kipps_s, aes(x=kipps.x, y=kipps.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,kipps_cor_s,
                              italic_n,kipps_sample_numbers_s,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0("Kipp's distance: ", kipps_mean_var_s, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,820)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,820)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(-3,0,0,0), "cm"))
scatter_kipps_s

#Standardised methods
scatter_kipps_ns <- 
  ggplot(kipps_ns, aes(x=kipps.x, y=kipps.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,kipps_cor_ns,
                              italic_n,kipps_sample_numbers_ns,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0("Kipp's distance: ", kipps_mean_var_ns, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,820)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,820)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(-3,0,0,0), "cm"))
scatter_kipps_ns

#######################################################################
# make figure 3 and 4 - [Tail length] compare Measure 1 vs. Measure 2
#######################################################################
##Tail length
#Select trait, split pairs into separate columns, and remove pairs with NAs
tail_length <- pair_data[c(1,11:13)]
names(tail_length)[2] <- "tail.length"
tail_length_M1 <-subset(tail_length,Sample.Number=="1")
tail_length_M2 <-subset(tail_length,Sample.Number=="2")
tail_length_split <- merge(tail_length_M1,tail_length_M2,by="Unique.Specimen.Identifier")
tail_length_split <- tail_length_split[complete.cases(tail_length_split),]

#Calculate, mean, sd and cv.
tail_length_split$mean <- apply(tail_length_split[, c("tail.length.x","tail.length.y")],1,mean)
tail_length_split$sd <- apply(tail_length_split[, c("tail.length.x","tail.length.y")],1,sd)
tail_length_split$cv <- (tail_length_split$sd / tail_length_split$mean) * 100

#Calculate mean_Var for each trait, correlation coefficient and number of samples
#Standardised methods
data_tail_length_s <- subset(tail_length_split, Protocol.both.pairs.x == '1')
tail_length_mean_var_s <- round(mean(data_tail_length_s$cv),digits = 2)
tail_length_sample_numbers_s <- dplyr::count(data_tail_length_s)
tail_length_cor_s <- round(cor(data_tail_length_s$tail.length.x,data_tail_length_s$tail.length.y),3)

#Non-standardised methods
data_tail_length_ns <- subset(tail_length_split, Protocol.both.pairs.x == '0')
tail_length_mean_var_ns <- round(mean(data_tail_length_ns$cv),digits = 2)
tail_length_sample_numbers_ns <- dplyr::count(data_tail_length_ns)
tail_length_cor_ns <- round(cor(data_tail_length_ns$tail.length.x,data_tail_length_ns$tail.length.y),3)

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
#Standardised methods
scatter_tail_length_s <- 
  ggplot(data_tail_length_s, aes(x=tail.length.x, y=tail.length.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,tail_length_cor_s,
                              italic_n,tail_length_sample_numbers_s,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('Tail length: ', tail_length_mean_var_s, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,1280)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,1280)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(-3,0,0,0), "cm"))
scatter_tail_length_s

#Standardised methods
scatter_tail_length_ns <- 
  ggplot(data_tail_length_ns, aes(x=tail.length.x, y=tail.length.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,tail_length_cor_ns,
                              italic_n,tail_length_sample_numbers_ns,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  annotate('text',label= strict,x=-Inf,y=-Inf,hjust=-4.4,vjust=-1,size=4)+
  theme(legend.position = "none") +
  ggtitle(paste0('Tail length: ', tail_length_mean_var_ns, '%' ))+
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,1280)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,1280)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(-3,0,0,0), "cm"))
scatter_tail_length_ns

#######################################################################
# make miscellaneous plot functions
#######################################################################
label_beak_culmen_s <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                         list(cv=beak_culmen_cor_s,n=beak_culmen_sample_numbers_s$n))
label_beak_culmen_ns <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                        list(cv=beak_culmen_cor_ns,n=beak_culmen_sample_numbers_ns$n))
label_beak_nares_s <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                          list(cv=beak_nares_cor_s,n=beak_nares_sample_numbers_s$n))
label_beak_nares_ns <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                         list(cv=beak_nares_cor_ns,n=beak_nares_sample_numbers_ns$n))
label_beak_width_s <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                        list(cv=beak_width_cor_s,n=beak_width_sample_numbers_s$n))
label_beak_width_ns <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                       list(cv=beak_width_cor_ns,n=beak_width_sample_numbers_ns$n))
label_beak_depth_s <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                        list(cv=beak_depth_cor_s,n=beak_depth_sample_numbers_s$n))
label_beak_depth_ns <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                       list(cv=beak_depth_cor_ns,n=beak_depth_sample_numbers_ns$n))

label_tarsus_length_s <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                         list(cv=tarsus_length_cor_s,n=tarsus_length_sample_numbers_s$n))
label_tarsus_length_ns<- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                        list(cv=tarsus_length_cor_ns,n=tarsus_length_sample_numbers_ns$n))
label_wing_length_s <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                          list(cv=wing_length_cor_s,n=wing_length_sample_numbers_s$n))
label_wing_length_ns <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                         list(cv=wing_length_cor_ns,n=wing_length_sample_numbers_ns$n))
label_kipps_s <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                           list(cv=kipps_cor_s,n=kipps_sample_numbers_s$n))
label_kipps_ns <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                          list(cv=kipps_cor_ns,n=kipps_sample_numbers_ns$n))
label_tail_length_s <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                         list(cv=tail_length_cor_s,n=tail_length_sample_numbers_s$n))
label_tail_length_ns <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                        list(cv=tail_length_cor_ns,n=tail_length_sample_numbers_ns$n))
######################################################################
# Final Figure S3
######################################################################
#Combined plot - duplicates using Standardised (Avonet) protocol
plot_grid(
  NULL,
  scatter_beak_culmen_s,scatter_beak_nares_s,
  scatter_beak_width_s,scatter_beak_depth_s,
  NULL,
  NULL,
  scatter_tarsus_length_s,scatter_wing_length_s,
  scatter_kipps_s,scatter_tail_length_s,
  NULL,
  ncol = 6,
  nrow=2,
  rel_widths = c(0.05,1,1,1,1,0.05,0.05,1,1,1,1,0.05),scale = 0.95) +
  draw_label(label_beak_culmen_s, x = 0.0550, y = 0.8525, size = 15, hjust = 0) +
  draw_label(label_beak_nares_s, x = 0.2980, y = 0.8525, size = 15, hjust = 0) +
  draw_label(label_beak_width_s, x = 0.5410, y = 0.8525, size = 15, hjust = 0) +
  draw_label(label_beak_depth_s, x = 0.7840, y = 0.8525, size = 15, hjust = 0) +
  draw_label(label_tarsus_length_s, x = 0.0550, y = 0.4325, size = 15, hjust = 0) +
  draw_label(label_wing_length_s, x = 0.2980, y = 0.4325, size = 15, hjust = 0) +
  draw_label(label_kipps_s, x = 0.5410, y = 0.4325, size = 15, hjust = 0) +
  draw_label(label_tail_length_s, x = 0.7900, y = 0.4325, size = 15, hjust = 0) 

ggsave(filename = paste0(figFolder,'Fig S3 - Standardised methods.pdf'),width = 18, 
       height = 12, dpi = 600, units = "in", device='pdf')

######################################################################
# Final Figure S4
######################################################################
#Combined plot - duplicates using Non-standardised protocols
plot_grid(
  NULL,
  scatter_beak_culmen_ns,scatter_beak_nares_ns,
  scatter_beak_width_ns,scatter_beak_depth_ns,
  NULL,
  NULL,
  scatter_tarsus_length_ns,scatter_wing_length_ns,
  scatter_kipps_ns,scatter_tail_length_ns,
  NULL,
  ncol = 6,
  nrow=2,
  rel_widths = c(0.05,1,1,1,1,0.05,0.05,1,1,1,1,0.05),scale = 0.95) +
  draw_label(label_beak_culmen_ns, x = 0.0550, y = 0.8525, size = 15, hjust = 0) +
  draw_label(label_beak_nares_ns, x = 0.2980, y = 0.8525, size = 15, hjust = 0) +
  draw_label(label_beak_width_ns, x = 0.5410, y = 0.8525, size = 15, hjust = 0) +
  draw_label(label_beak_depth_ns, x = 0.7840, y = 0.8525, size = 15, hjust = 0) +
  draw_label(label_tarsus_length_ns, x = 0.0550, y = 0.4325, size = 15, hjust = 0) +
  draw_label(label_wing_length_ns, x = 0.2980, y = 0.4325, size = 15, hjust = 0) +
  draw_label(label_kipps_ns, x = 0.5410, y = 0.4325, size = 15, hjust = 0) +
  draw_label(label_tail_length_ns, x = 0.7900, y = 0.4325, size = 15, hjust = 0) 

ggsave(filename = paste0(figFolder,'Fig S4 - Non-standardised methods.pdf'),width = 18, 
       height = 12, dpi = 600, units = "in", device='pdf')


