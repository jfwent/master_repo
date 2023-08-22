####################################################################
# load data
####################################################################

original_data <-read.csv(paste0(TraitDataFolder,'AVONET_Duplicate_Data.csv'),h=T)

####################################################################
# data processing and cleaning
####################################################################
#Select only relevant columns.
combined_data <- original_data[c(1,8:18,20)]

#Select only duplicate Measurerment pairs.
pair_data <- subset(combined_data,Sample.Number == "1" | Sample.Number == "2")

#General annotations and inset plot coordinates
italic_R <- 'paste(italic(R), \" =\")'
italic_n <- 'paste(", ",italic(n), \" =\")'
#label_a <- textGrob("a",gp = gpar(fontsize = 30, col = 'black', fontface = 'bold'), x = 0.02, y = 0.95, hjust = 0)
#label_b <- textGrob("b",gp = gpar(fontsize = 30, col = 'black', fontface = 'bold'), x = 0.02, y = 0.95, hjust = 0)
#label_c <- textGrob("c",gp = gpar(fontsize = 30, col = 'black', fontface = 'bold'), x = 0.02, y = 0.95, hjust = 0)
#label_d <- textGrob("d",gp = gpar(fontsize = 30, col = 'black', fontface = 'bold'), x = 0.02, y = 0.95, hjust = 0)  

#######################################################################
# make figure 1 - [Beak culmen] compare Measure 1 vs. Measure 2
#######################################################################
##Beak Length (culmen)
#Select trait, split pairs into separate columns, and remove pairs with NAs
beak_culmen <- pair_data[c(1:2,12)]
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
beak_culmen_mean_var <- round(mean(beak_culmen_split$cv),digits = 2)
beak_culmen_sample_numbers <- dplyr::count(beak_culmen_split)
beak_culmen_cor <- round(cor(beak_culmen_split$Beak.culmen.x,beak_culmen_split$Beak.culmen.y),3)

#Calculate values for the Bland-Altman plots
beak_culmen_baseline <-beak_culmen_split$Beak.culmen.x
beak_culmen_post <-beak_culmen_split$Beak.culmen.y
beak_culmen_diff <- (beak_culmen_post - beak_culmen_baseline)
beak_culmen_diffp <- ((beak_culmen_post - beak_culmen_baseline)/beak_culmen_baseline)+1
beak_culmen_sd.diff <- sd(beak_culmen_diff)
beak_culmen_sd.diffp <- sd(beak_culmen_diffp)
beak_culmen_BA <- data.frame(beak_culmen_baseline, beak_culmen_post, beak_culmen_diff, beak_culmen_diffp)

#Create specific text functions
label_beak_culmen <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                list(cv=beak_culmen_cor,n=beak_culmen_sample_numbers$n))

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
scatter_beak_culmen <- 
  ggplot(beak_culmen_split, aes(x=Beak.culmen.x, y=Beak.culmen.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,beak_culmen_cor,
                              italic_n,beak_culmen_sample_numbers,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  theme(legend.position = "none") +
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = mean(beak_culmen_BA$beak_culmen_diffp), colour = "blue",linetype=1,size=1.2)+
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,450)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,450)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(0,0,0,0), "cm"))
scatter_beak_culmen

#Create Bland-Altman plot to illustrate proportional differencers between specimen Measurerments
diffplot_beak_culmen <- ggplot(beak_culmen_BA, aes(beak_culmen_baseline, beak_culmen_diffp)) + 
  geom_point(size=2, colour = rgb(0,0,0, alpha = 0.5)) + 
  geom_hline(yintercept = mean(beak_culmen_BA$beak_culmen_diffp),colour="blue",linetype=1,size=1) +
  geom_hline(yintercept = 1, colour = "red", size = 1,linetype="dashed") +
  geom_hline(yintercept = mean(beak_culmen_BA$beak_culmen_diffp) + 1.96*beak_culmen_sd.diffp, linetype = 2) +
  geom_hline(yintercept = mean(beak_culmen_BA$beak_culmen_diffp) - 1.96*beak_culmen_sd.diffp, linetype = 2) +
  ggtitle(paste0('Beak length (culmen): ', beak_culmen_mean_var, '%' )) +
  scale_y_continuous(breaks = (seq(0.6, 2.2, 0.2)), limits = c(0.5, 2.2))+
  ylab("Difference between Measurer 1 & 2") +
  xlab("Mean trait value (mm)")+ 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         plot.title = element_text(color="black", size=18,face="bold"),
         axis.title.x = element_text(color="black", size=15),
         axis.text = element_text(color = "black",size=10),
         axis.ticks = element_line(size = 1),
         axis.title.y = element_text(color="black", size=15),
         panel.background = element_rect(colour = "black", size=1.5),
         plot.margin=unit(c(1,1,1,1), "cm"))
diffplot_beak_culmen

combined_beak_culmen <- ggdraw(diffplot_beak_culmen)+
  draw_plot(scatter_beak_culmen, width = 0.4, height = 0.4, x = 0.52, y = 0.49)+
  draw_label(label_beak_culmen,x = 0.66, y = 0.15, hjust = 0)

#Optional: Save plot
#ggsave(filename = paste0(figFolder,'Combined_beak_culmen.pdf'),width = 7,height = 7, dpi = 600, units = "in", device='pdf')
#ggsave(filename = paste0(figFolder,'Combined_beak_culmen.jpg'),width = 7,height = 7, dpi = 600, units = "in", device='jpg')

#######################################################################
# make figure 1 - [Beak nares] compare Measure 1 vs. Measure 2
#######################################################################
##Beak Length (nares)
#Select trait, split pairs into separate columns, and remove pairs with NAs
beak_nares <- pair_data[c(1,3,12)]
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
beak_nares_mean_var <- round(mean(beak_nares_split$cv),digits = 2)
beak_nares_sample_numbers <- dplyr::count(beak_nares_split)
beak_nares_cor <- round(cor(beak_nares_split$Beak.nares.x,beak_nares_split$Beak.nares.y),3)

#Calculate values for the Bland-Altman plots
beak_nares_baseline <-beak_nares_split$Beak.nares.x
beak_nares_post <-beak_nares_split$Beak.nares.y
beak_nares_diff <- (beak_nares_post - beak_nares_baseline)
beak_nares_diffp <- ((beak_nares_post - beak_nares_baseline)/beak_nares_baseline)+1
beak_nares_sd.diff <- sd(beak_nares_diff)
beak_nares_sd.diffp <- sd(beak_nares_diffp)
beak_nares_BA <- data.frame(beak_nares_baseline, beak_nares_post, beak_nares_diff, beak_nares_diffp)

#Create specific text functions
label_beak_nares <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                list(cv=beak_nares_cor,n=beak_nares_sample_numbers$n))

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
scatter_beak_nares <- 
  ggplot(beak_nares_split, aes(x=Beak.nares.x, y=Beak.nares.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,beak_nares_cor,
                              italic_n,beak_nares_sample_numbers,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  theme(legend.position = "none") +
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = mean(beak_nares_BA$beak_nares_diffp), colour = "blue",linetype=1,size=1.2)+
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,210)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,210)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(0,0,0,0), "cm"))
scatter_beak_nares

#Create Bland-Altman plot to illustrate proportional differencers between specimen Measurerments
diffplot_beak_nares <- ggplot(beak_nares_BA, aes(beak_nares_baseline, beak_nares_diffp)) + 
  geom_point(size=2, colour = rgb(0,0,0, alpha = 0.5)) +
  geom_hline(yintercept = mean(beak_nares_BA$beak_nares_diffp),colour="blue",linetype=1,size=1) +
  geom_hline(yintercept = 1, colour = "red", size = 1,linetype="dashed") +
  geom_hline(yintercept = mean(beak_nares_BA$beak_nares_diffp) + 1.96*beak_nares_sd.diffp, linetype = 2) +
  geom_hline(yintercept = mean(beak_nares_BA$beak_nares_diffp) - 1.96*beak_nares_sd.diffp, linetype = 2) +
  ggtitle(paste0('Beak length (nares): ', beak_nares_mean_var, '%' )) +
  scale_y_continuous(breaks = (seq(0.6, 2.2, 0.2)), limits = c(0.5, 2.2))+
  ylab("Difference between Measurer 1 & 2") +
  xlab("Mean trait value (mm)")+ 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(1,1,1,1), "cm"))
diffplot_beak_nares

combined_beak_nares <- ggdraw(diffplot_beak_nares)+
  draw_plot(scatter_beak_nares, width = 0.4, height = 0.4, x = 0.52, y = 0.49)+
  draw_label(label_beak_nares,x = 0.66, y = 0.15, hjust = 0)

#Optional: Save plot
#ggsave(filename = paste0(figFolder,'Combined_beak_nares.pdf'),width = 7,height = 7, dpi = 600, units = "in", device='pdf')
#ggsave(filename = paste0(figFolder,'Combined_beak_nares.jpg'),width = 7,height = 7, dpi = 600, units = "in", device='jpg')

#######################################################################
# make figure 1 - [Beak width] compare Measure 1 vs. Measure 2
#######################################################################
##Beak width
#Select trait, split pairs into separate columns, and remove pairs with NAs
beak_width <- pair_data[c(1,4,12)]
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
beak_width_mean_var <- round(mean(beak_width_split$cv),digits = 2)
beak_width_sample_numbers <- dplyr::count(beak_width_split)
beak_width_cor <- round(cor(beak_width_split$Beak.width.x,beak_width_split$Beak.width.y),3)

#Calculate values for the Bland-Altman plots
beak_width_baseline <-beak_width_split$Beak.width.x
beak_width_post <-beak_width_split$Beak.width.y
beak_width_diff <- (beak_width_post - beak_width_baseline)
beak_width_diffp <- ((beak_width_post - beak_width_baseline)/beak_width_baseline)+1
beak_width_sd.diff <- sd(beak_width_diff)
beak_width_sd.diffp <- sd(beak_width_diffp)
beak_width_BA <- data.frame(beak_width_baseline, beak_width_post, beak_width_diff, beak_width_diffp)

#Create specific text functions
label_beak_width <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                               list(cv=beak_width_cor,n=beak_width_sample_numbers$n))

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
scatter_beak_width <- 
  ggplot(beak_width_split, aes(x=Beak.width.x, y=Beak.width.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,beak_width_cor,
                              italic_n,beak_width_sample_numbers,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  theme(legend.position = "none") +
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = mean(beak_width_BA$beak_width_diffp), colour = "blue",linetype=1,size=1.2)+
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,110)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,110)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(0,0,0,0), "cm"))
scatter_beak_width

#Create Bland-Altman plot to illustrate proportional differencers between specimen Measurerments
diffplot_beak_width <- ggplot(beak_width_BA, aes(beak_width_baseline, beak_width_diffp)) + 
  geom_point(size=2, colour = rgb(0,0,0, alpha = 0.5)) +
  geom_hline(yintercept = mean(beak_nares_BA$beak_nares_diffp),colour="blue",linetype=1,size=1) +
  geom_hline(yintercept = 1, colour = "red", size = 1,linetype="dashed") +
  geom_hline(yintercept = mean(beak_width_BA$beak_width_diffp) + 1.96*beak_width_sd.diffp, linetype = 2) +
  geom_hline(yintercept = mean(beak_width_BA$beak_width_diffp) - 1.96*beak_width_sd.diffp, linetype = 2) +
  ggtitle(paste0('Beak width: ', beak_width_mean_var, '%' )) +
  scale_y_continuous(breaks = (seq(0.6, 2.2, 0.2)), limits = c(0.5, 2.2))+
  ylab("Difference between Measurer 1 & 2") +
  xlab("Mean trait value (mm)")+ 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(1,1,1,1), "cm"))
diffplot_beak_width

combined_beak_width <- ggdraw(diffplot_beak_width)+
  draw_plot(scatter_beak_width, width = 0.4, height = 0.4, x = 0.52, y = 0.49)+
  draw_label(label_beak_width,x = 0.66, y = 0.15, hjust = 0)

#Optional: Save plot
#ggsave(filename = paste0(figFolder,'Combined_beak_width.pdf'),width = 7,height = 7, dpi = 600, units = "in", device='pdf')
#ggsave(filename = paste0(figFolder,'Combined_beak_width.jpg'),width = 7,height = 7, dpi = 600, units = "in", device='jpg')

#######################################################################
# make figure 1 - [Beak depth] compare Measure 1 vs. Measure 2
#######################################################################
##Beak depth
#Select trait, split pairs into separate columns, and remove pairs with NAs
beak_depth <- pair_data[c(1,5,12)]
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
beak_depth_mean_var <- round(mean(beak_depth_split$cv),digits = 2)
beak_depth_sample_numbers <- dplyr::count(beak_depth_split)
beak_depth_cor <- round(cor(beak_depth_split$Beak.depth.x,beak_depth_split$Beak.depth.y),3)

#Calculate values for the Bland-Altman plots
beak_depth_baseline <-beak_depth_split$Beak.depth.x
beak_depth_post <-beak_depth_split$Beak.depth.y
beak_depth_diff <- (beak_depth_post - beak_depth_baseline)
beak_depth_diffp <- ((beak_depth_post - beak_depth_baseline)/beak_depth_baseline)+1
beak_depth_sd.diff <- sd(beak_depth_diff)
beak_depth_sd.diffp <- sd(beak_depth_diffp)
beak_depth_BA <- data.frame(beak_depth_baseline, beak_depth_post, beak_depth_diff, beak_depth_diffp)

#Create specific text functions
label_beak_depth <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                               list(cv=beak_depth_cor,n=beak_depth_sample_numbers$n))

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
scatter_beak_depth <- 
  ggplot(beak_depth_split, aes(x=Beak.depth.x, y=Beak.depth.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,beak_depth_cor,
                              italic_n,beak_depth_sample_numbers,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  theme(legend.position = "none") +
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = mean(beak_depth_BA$beak_depth_diffp), colour = "blue",linetype=1,size=1.2)+
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,140)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,140)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(0,0,0,0), "cm"))
scatter_beak_depth

#Create Bland-Altman plot to illustrate proportional differencers between specimen Measurerments
diffplot_beak_depth <- ggplot(beak_depth_BA, aes(beak_depth_baseline, beak_depth_diffp)) + 
  geom_point(size=2, colour = rgb(0,0,0, alpha = 0.5)) +
  geom_hline(yintercept = mean(beak_nares_BA$beak_nares_diffp),colour="blue",linetype=1,size=1) +
  geom_hline(yintercept = 1, colour = "red", size = 1,linetype="dashed") +
  geom_hline(yintercept = mean(beak_depth_BA$beak_depth_diffp) + 1.96*beak_depth_sd.diffp, linetype = 2) +
  geom_hline(yintercept = mean(beak_depth_BA$beak_depth_diffp) - 1.96*beak_depth_sd.diffp, linetype = 2) +
  ggtitle(paste0('Beak depth: ', beak_depth_mean_var, '%' )) +
  scale_y_continuous(breaks = (seq(0.6, 2.2, 0.2)), limits = c(0.5, 2.2))+
  ylab("Difference between Measurer 1 & 2") +
  xlab("Mean trait value (mm)")+ 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(1,1,1,1), "cm"))
diffplot_beak_depth

combined_beak_depth <- ggdraw(diffplot_beak_depth)+
  draw_plot(scatter_beak_depth, width = 0.4, height = 0.4, x = 0.52, y = 0.49)+
  draw_label(label_beak_depth,x = 0.66, y = 0.15, hjust = 0)

#Optional: Save plot
#ggsave(filename = paste0(figFolder,'Combined_beak_depth.pdf'),width = 7,height = 7, dpi = 600, units = "in", device='pdf')
#ggsave(filename = paste0(figFolder,'Combined_beak_depth.jpg'),width = 7,height = 7, dpi = 600, units = "in", device='jpg')

######################################################################
# Final Figure S1
######################################################################
plot_grid(combined_beak_culmen,combined_beak_nares,
          combined_beak_width,combined_beak_depth,
          ncol = 2,
          nrow=2)

ggsave(filename = paste0(figFolder,'Fig S1 - Beak traits.pdf'),width = 14, 
       height = 14, dpi = 600, units = "in", device='pdf')

ggsave(filename = paste0(figFolder,'Fig S1 - Beak traits.jpg'),width = 14, 
       height = 14, dpi = 600, units = "in", device='jpg')

##-------------------------------------------------------------------##

#######################################################################
# make figure 2 - [Tarsus length] compare Measure 1 vs. Measure 2
####################################################################### 
##tarsus Length
#Select trait, split pairs into separate columns, and remove pairs with NAs
tarsus_length <- pair_data[c(1,6,12)]
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
tarsus_length_mean_var <- round(mean(tarsus_length_split$cv),digits = 2)
tarsus_length_sample_numbers <- dplyr::count(tarsus_length_split)
tarsus_length_cor <- round(cor(tarsus_length_split$tarsus.length.x,tarsus_length_split$tarsus.length.y),3)

#Calculate values for the Bland-Altman plots
tarsus_length_baseline <-tarsus_length_split$tarsus.length.x
tarsus_length_post <-tarsus_length_split$tarsus.length.y
tarsus_length_diff <- (tarsus_length_post - tarsus_length_baseline)
tarsus_length_diffp <- ((tarsus_length_post - tarsus_length_baseline)/tarsus_length_baseline)+1
tarsus_length_sd.diff <- sd(tarsus_length_diff)
tarsus_length_sd.diffp <- sd(tarsus_length_diffp)
tarsus_length_BA <- data.frame(tarsus_length_baseline, tarsus_length_post, tarsus_length_diff, tarsus_length_diffp)

#Create specific text functions
label_tarsus_length <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                list(cv=tarsus_length_cor,n=tarsus_length_sample_numbers$n))

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
scatter_tarsus_length <- 
  ggplot(tarsus_length_split, aes(x=tarsus.length.x, y=tarsus.length.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,tarsus_length_cor,
                              italic_n,tarsus_length_sample_numbers,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  theme(legend.position = "none") +
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = mean(tarsus_length_BA$tarsus_length_diffp), colour = "blue",linetype=1,size=1.2)+
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,340)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,340)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(0,0,0,0), "cm"))
scatter_tarsus_length

#Create Bland-Altman plot to illustrate proportional differencers between specimen Measurerments
diffplot_tarsus_length <- ggplot(tarsus_length_BA, aes(tarsus_length_baseline, tarsus_length_diffp)) + 
  geom_point(size=2, colour = rgb(0,0,0, alpha = 0.5)) +
  geom_hline(yintercept = mean(tarsus_length_BA$tarsus_length_diffp),colour="blue",linetype=1,size=1) +
  geom_hline(yintercept = 1, colour = "red", size = 1,linetype="dashed") +
  geom_hline(yintercept = mean(tarsus_length_BA$tarsus_length_diffp) + 1.96*tarsus_length_sd.diffp, linetype = 2) +
  geom_hline(yintercept = mean(tarsus_length_BA$tarsus_length_diffp) - 1.96*tarsus_length_sd.diffp, linetype = 2) +
  ggtitle(paste0('Tarsus length: ', tarsus_length_mean_var, '%' )) +
  scale_y_continuous(breaks = (seq(0.6, 2.2, 0.2)), limits = c(0.5, 2.2))+
  ylab("Difference between Measurer 1 & 2") +
  xlab("Mean trait value (mm)")+ 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(1,1,1,1), "cm"))
diffplot_tarsus_length

combined_tarsus_length <- ggdraw(diffplot_tarsus_length)+
  draw_plot(scatter_tarsus_length, width = 0.4, height = 0.4, x = 0.52, y = 0.49)+
  draw_label(label_tarsus_length,x = 0.66, y = 0.15, hjust = 0)

#Optional: Save plot
#ggsave(filename = paste0(figFolder,'Combined_tarsus_length.pdf'),width = 7,height = 7, dpi = 600, units = "in", device='pdf')
#ggsave(filename = paste0(figFolder,'Combined_tarsus_length.jpg'),width = 7,height = 7, dpi = 600, units = "in", device='jpg')

#######################################################################
# make figure 2 - [Wing length] compare Measure 1 vs. Measure 2
####################################################################### 
##wing Length
#Select trait, split pairs into separate columns, and remove pairs with NAs
wing_length <- pair_data[c(1,7,12)]
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
wing_length_mean_var <- round(mean(wing_length_split$cv),digits = 2)
wing_length_sample_numbers <- dplyr::count(wing_length_split)
wing_length_cor <- round(cor(wing_length_split$wing.length.x,wing_length_split$wing.length.y),3)

#Calculate values for the Bland-Altman plots
wing_length_baseline <-wing_length_split$wing.length.x
wing_length_post <-wing_length_split$wing.length.y
wing_length_diff <- (wing_length_post - wing_length_baseline)
wing_length_diffp <- ((wing_length_post - wing_length_baseline)/wing_length_baseline)+1
wing_length_sd.diff <- sd(wing_length_diff)
wing_length_sd.diffp <- sd(wing_length_diffp)
wing_length_BA <- data.frame(wing_length_baseline, wing_length_post, wing_length_diff, wing_length_diffp)

#Create specific text functions
label_wing_length <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                  list(cv=wing_length_cor,n=wing_length_sample_numbers$n))

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
scatter_wing_length <- 
  ggplot(wing_length_split, aes(x=wing.length.x, y=wing.length.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,wing_length_cor,
                              italic_n,wing_length_sample_numbers,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  theme(legend.position = "none") +
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = mean(wing_length_BA$wing_length_diffp), colour = "blue",linetype=1,size=1.2)+
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,840)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,840)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(0,0,0,0), "cm"))
scatter_wing_length

#Create Bland-Altman plot to illustrate proportional differencers between specimen Measurerments
diffplot_wing_length <- ggplot(wing_length_BA, aes(wing_length_baseline, wing_length_diffp)) + 
  geom_point(size=2, colour = rgb(0,0,0, alpha = 0.5)) + 
  geom_hline(yintercept = mean(wing_length_BA$wing_length_diffp),colour="blue",linetype=1,size=1) +
  geom_hline(yintercept = 1, colour = "red", size = 1,linetype="dashed") +
  geom_hline(yintercept = mean(wing_length_BA$wing_length_diffp) + 1.96*wing_length_sd.diffp, linetype = 2) +
  geom_hline(yintercept = mean(wing_length_BA$wing_length_diffp) - 1.96*wing_length_sd.diffp, linetype = 2) +
  ggtitle(paste0('Wing length: ', wing_length_mean_var, '%' )) +
  scale_y_continuous(breaks = (seq(0.6, 2.2, 0.2)), limits = c(0.5, 2.2))+
  ylab("Difference between Measurer 1 & 2") +
  xlab("Mean trait value (mm)")+ 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(1,1,1,1), "cm"))
diffplot_wing_length

combined_wing_length <- ggdraw(diffplot_wing_length)+
  draw_plot(scatter_wing_length, width = 0.4, height = 0.4, x = 0.52, y = 0.49)+
  draw_label(label_wing_length,x = 0.66, y = 0.15, hjust = 0)

#Optional: Save plot
#ggsave(filename = paste0(figFolder,'Combined_wing_length.pdf'),width = 7,height = 7, dpi = 600, units = "in", device='pdf')
#ggsave(filename = paste0(figFolder,'Combined_wing_length.jpg'),width = 7,height = 7, dpi = 600, units = "in", device='jpg')

#######################################################################
# make figure 2 - [Kipp's distance] compare Measure 1 vs. Measure 2
####################################################################### 
##Kipp's distance
#Select trait, split pairs into separate columns, and remove pairs with NAs
kipps <- pair_data[c(1,8,12)]
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
kipps_mean_var <- round(mean(kipps_split$cv),digits = 2)
kipps_sample_numbers <- dplyr::count(kipps_split)
kipps_cor <- round(cor(kipps_split$kipps.x,kipps_split$kipps.y),3)

#Calculate values for the Bland-Altman plots
kipps_baseline <-kipps_split$kipps.x
kipps_post <-kipps_split$kipps.y
kipps_diff <- (kipps_post - kipps_baseline)
kipps_diffp <- ((kipps_post - kipps_baseline)/kipps_baseline)+1
kipps_sd.diff <- sd(kipps_diff)
kipps_sd.diffp <- sd(kipps_diffp)
kipps_BA <- data.frame(kipps_baseline, kipps_post, kipps_diff, kipps_diffp)

#Create specific text functions
label_kipps <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                list(cv=kipps_cor,n=kipps_sample_numbers$n))

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
scatter_kipps <- 
  ggplot(kipps_split, aes(x=kipps.x, y=kipps.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,kipps_cor,
                              italic_n,kipps_sample_numbers,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  theme(legend.position = "none") +
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = mean(kipps_BA$kipps_diffp), colour = "blue",linetype=1,size=1.2)+
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,380)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,380)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(0,0,0,0), "cm"))
scatter_kipps

#Create Bland-Altman plot to illustrate proportional differencers between specimen Measurerments
diffplot_kipps <- ggplot(kipps_BA, aes(kipps_baseline, kipps_diffp)) + 
  geom_point(size=2, colour = rgb(0,0,0, alpha = 0.5)) + 
  geom_hline(yintercept = mean(kipps_BA$kipps_diffp),colour="blue",linetype=1,size=1) +
  geom_hline(yintercept = 1, colour = "red", size = 1,linetype="dashed") +
  geom_hline(yintercept = mean(kipps_BA$kipps_diffp) + 1.96*kipps_sd.diffp, linetype = 2) +
  geom_hline(yintercept = mean(kipps_BA$kipps_diffp) - 1.96*kipps_sd.diffp, linetype = 2) +
  ggtitle(paste0("Kipp's distance: ", kipps_mean_var, '%' )) +
  scale_y_continuous(breaks = (seq(0.2, 2.6, 0.2)), limits = c(0.1, 2.7))+
  ylab("Difference between Measurer 1 & 2") +
  xlab("Mean trait value (mm)")+ 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(1,1,1,1), "cm"))
diffplot_kipps

combined_kipps <- ggdraw(diffplot_kipps)+
  draw_plot(scatter_kipps, width = 0.4, height = 0.4, x = 0.52, y = 0.49)+
  draw_label(label_kipps,x = 0.66, y = 0.15, hjust = 0)

#Optional: Save plot
#ggsave(filename = paste0(figFolder,'Combined_kipps.pdf'),width = 7,height = 7, dpi = 600, units = "in", device='pdf')
#ggsave(filename = paste0(figFolder,'Combined_kipps.jpg'),width = 7,height = 7, dpi = 600, units = "in", device='jpg')

#######################################################################
# make figure 2 - [Tail length] compare Measure 1 vs. Measure 2
####################################################################### 
##tail Length
#Select trait, split pairs into separate columns, and remove pairs with NAs
tail_length <- pair_data[c(1,11,12)]
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
tail_length_mean_var <- round(mean(tail_length_split$cv),digits = 2)
tail_length_sample_numbers <- dplyr::count(tail_length_split)
tail_length_cor <- round(cor(tail_length_split$tail.length.x,tail_length_split$tail.length.y),3)

#Calculate values for the Bland-Altman plots
tail_length_baseline <-tail_length_split$tail.length.x
tail_length_post <-tail_length_split$tail.length.y
tail_length_diff <- (tail_length_post - tail_length_baseline)
tail_length_diffp <- ((tail_length_post - tail_length_baseline)/tail_length_baseline)+1
tail_length_sd.diff <- sd(tail_length_diff)
tail_length_sd.diffp <- sd(tail_length_diffp)
tail_length_BA <- data.frame(tail_length_baseline, tail_length_post, tail_length_diff, tail_length_diffp)

#Create specific text functions
label_tail_length <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                list(cv=tail_length_cor,n=tail_length_sample_numbers$n))

#Create Scatter plot to illustrate alignment of Measurer 1 and Measurer 2
scatter_tail_length <- 
  ggplot(tail_length_split, aes(x=tail.length.x, y=tail.length.y)) +
  geom_point() +
  annotate('text',label=paste(italic_R,tail_length_cor,
                              italic_n,tail_length_sample_numbers,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  theme(legend.position = "none") +
  labs(x ="Measurer 2", y = "Measurer 1") +
  geom_abline(intercept = mean(tail_length_BA$tail_length_diffp), colour = "blue",linetype=1,size=1.2)+
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,1310)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,1310)) +
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(0,0,0,0), "cm"))
scatter_tail_length

#Create Bland-Altman plot to illustrate proportional differencers between specimen Measurerments
diffplot_tail_length <- ggplot(tail_length_BA, aes(tail_length_baseline, tail_length_diffp)) + 
  geom_point(size=2, colour = rgb(0,0,0, alpha = 0.5)) +
  geom_hline(yintercept = mean(tail_length_BA$tail_length_diffp),colour="blue",linetype=1,size=1) +
  geom_hline(yintercept = 1, colour = "red", size = 1,linetype="dashed") +
  geom_hline(yintercept = mean(tail_length_BA$tail_length_diffp) + 1.96*tail_length_sd.diffp, linetype = 2) +
  geom_hline(yintercept = mean(tail_length_BA$tail_length_diffp) - 1.96*tail_length_sd.diffp, linetype = 2) +
  ggtitle(paste0('Tail length: ', tail_length_mean_var, '%' )) +
  scale_y_continuous(breaks = (seq(0.6, 2.2, 0.2)), limits = c(0.5, 2.2))+
  ylab("Difference between Measurer 1 & 2") +
  xlab("Mean trait value (mm)")+ 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="black", size=18,face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.text = element_text(color = "black",size=10),
        axis.ticks = element_line(size = 1),
        axis.title.y = element_text(color="black", size=15),
        panel.background = element_rect(colour = "black", size=1.5),
        plot.margin=unit(c(1,1,1,1), "cm"))
diffplot_tail_length

combined_tail_length <- ggdraw(diffplot_tail_length)+
  draw_plot(scatter_tail_length, width = 0.4, height = 0.4, x = 0.52, y = 0.49)+
  draw_label(label_tail_length,x = 0.66, y = 0.15, hjust = 0)

#Optional: Save plot
#ggsave(filename = paste0(figFolder,'Combined_tail_length.pdf'),width = 7,height = 7, dpi = 600, units = "in", device='pdf')
#ggsave(filename = paste0(figFolder,'Combined_tail_length.jpg'),width = 7,height = 7, dpi = 600, units = "in", device='jpg')

######################################################################
# Final Figure S2
######################################################################
plot_grid(combined_tarsus_length,combined_tail_length,
          combined_wing_length,combined_kipps,
          ncol = 2,
          nrow=2)

ggsave(filename = paste0(figFolder,'Fig S2 - Body traits.pdf'),width = 14, 
       height = 14, dpi = 400, units = "in", device='pdf')

ggsave(filename = paste0(figFolder,'Fig S2 - Body traits.jpg'),width = 14, 
       height = 14, dpi = 600, units = "in", device='jpg')
