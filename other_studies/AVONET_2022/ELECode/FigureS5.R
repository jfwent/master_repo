####################################################################
# load data
####################################################################

original_data <- read.csv(paste0(TraitDataFolder,'AVONET_Raw_Data.csv'),h=T)
extant_species <- read.csv(paste0(TraitDataFolder,'AVONET_Extant_Species_List.csv'),h=T)

####################################################################
# data processing and cleaning
####################################################################
#Select only relevant columns.
required_data <- original_data[c(2,6,14,18:19,23)]
birdlife <- subset(extant_species,Taxonomy=="BirdLife")
combined_data <- merge(required_data,birdlife,by.x= "Species1_BirdLife",by.y="Species.name",all.x = T)
colnames(combined_data)[1] <- "BirdLife.name"
colnames(combined_data)[8] <- "BirdLife.genus"
colnames(combined_data)[9] <- "BirdLife.family"
colnames(combined_data)[10] <- "BirdLife.order"

#Subset field and museum specimens.
museum <- subset(combined_data,Data.type=="1")
field <- subset(combined_data,Data.type=="2")

#Calculate means
mean_museum <- museum %>% 
  group_by(BirdLife.order,BirdLife.family,BirdLife.name)%>% 
  summarise(across(c("Beak.Length_Culmen","Tarsus.Length","Wing.Length","Tail.Length"), ~ mean(.x, na.rm = TRUE)))
colnames(mean_museum)[4] <- "Beak_length_museum"
colnames(mean_museum)[5] <- "Tarsus_length_museum"
colnames(mean_museum)[6] <- "Wing_length_museum"
colnames(mean_museum)[7] <- "Tail_length_museum"

mean_field <- field %>% 
  group_by(BirdLife.order,BirdLife.family,BirdLife.name)%>% 
  summarise(across(c("Beak.Length_Culmen", "Tarsus.Length","Wing.Length","Tail.Length"), ~ mean(.x, na.rm = TRUE)))
colnames(mean_field)[4] <- "Beak_length_field"
colnames(mean_field)[5] <- "Tarsus_length_field"
colnames(mean_field)[6] <- "Wing_length_field"
colnames(mean_field)[7] <- "Tail_length_field"

field_museum <- merge(mean_field,mean_museum,by="BirdLife.name",all.x=T)

beak_culmen <- field_museum[c(1:4,10)]
names(beak_culmen)[2] <- "BirdLife.order"
names(beak_culmen)[3] <- "BirdLife.family"
names(beak_culmen)[4] <- "Field"
names(beak_culmen)[5] <- "Museum"
beak_culmen <- beak_culmen[complete.cases(beak_culmen),]

tarsus_length <- field_museum[c(1:3,5,11)]
names(tarsus_length)[2] <- "BirdLife.order"
names(tarsus_length)[3] <- "BirdLife.family"
names(tarsus_length)[4] <- "Field"
names(tarsus_length)[5] <- "Museum"
tarsus_length <- tarsus_length[complete.cases(tarsus_length),]

wing_length <- field_museum[c(1:3,6,12)]
names(wing_length)[2] <- "BirdLife.order"
names(wing_length)[3] <- "BirdLife.family"
names(wing_length)[4] <- "Field"
names(wing_length)[5] <- "Museum"
wing_length <- wing_length[complete.cases(wing_length),]

tail_length <- field_museum[c(1:3,7,13)]
names(tail_length)[2] <- "BirdLife.order"
names(tail_length)[3] <- "BirdLife.family"
names(tail_length)[4] <- "Field"
names(tail_length)[5] <- "Museum"
tail_length <- tail_length[complete.cases(tail_length),]

#Create dataframes for GLMMs, ignore the errors - as it just means there is a change in the variable name
beak_culmen2 <- data.frame(beak_culmen[1:3],stack(beak_culmen[4:5]))
tarsus_length2 <- data.frame(tarsus_length[1:3],stack(tarsus_length[4:5]))
wing_length2 <- data.frame(wing_length[1:3],stack(wing_length[4:5]))
tail_length2 <- data.frame(tail_length[1:3],stack(tail_length[4:5]))

#General annotations
italic_R <- 'paste(italic(R), \" =\")'
italic_n <- 'paste(", ",italic(n), \" =\")'

#######################################################################
# make figure 5 - [Beak length] compare field vs. museum samples
#######################################################################
#GLMM for plot
beak_culmen_glmm <- lmer(values~ind+(1|BirdLife.order/BirdLife.family),data=beak_culmen2)
beak_culmen_glmm_output <- anova(beak_culmen_glmm) 

#Calculate, mean, sd and cv.
beak_culmen$mean <- apply(beak_culmen[, c("Field","Museum")],1,mean)
beak_culmen$sd <- apply(beak_culmen[, c("Field","Museum")],1,sd)
beak_culmen$cv <- (beak_culmen$sd / beak_culmen$mean) * 100

#Calculate mean_Var for each trait, correlation coefficient and number of samples
beak_culmen_mean_var <- round(mean(beak_culmen$cv),digits = 2)
beak_culmen_sample_numbers <- dplyr::count(beak_culmen)
beak_culmen_cor <- round(cor(beak_culmen$Field,beak_culmen$Museum),3)
beak_culmen_stat <- t.test(beak_culmen$Field,beak_culmen$Museum,paired = F,alternative = "two.sided")

#Calculate values for the Bland-Altman plots
beak_culmen_baseline <-beak_culmen$Field
beak_culmen_post <-beak_culmen$Museum
beak_culmen_diff <- (beak_culmen_post - beak_culmen_baseline)
beak_culmen_diffp <- ((beak_culmen_post - beak_culmen_baseline)/beak_culmen_baseline)+1
beak_culmen_sd.diff <- sd(beak_culmen_diff)
beak_culmen_sd.diffp <- sd(beak_culmen_diffp)
beak_culmen_BA <- data.frame(beak_culmen_baseline, beak_culmen_post, beak_culmen_diff, beak_culmen_diffp)

#SCreate specific text functions
label_beak_culmen <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                list(cv=beak_culmen_cor,n=beak_culmen_sample_numbers$n))
label_beak_culmen_stat <- substitute(paste("GLMM: ",italic("F")," = ",fvalue,", ",italic("p")," = ",stat),
                                     list(stat=pvalue(beak_culmen_glmm_output$`Pr(>F)`),
                                          fvalue=format(beak_culmen_glmm_output$`F value`,digits=2)))

#Create Scatter plot to illustrate alignment of field and museum samples
scatter_beak_culmen <- 
  ggplot(beak_culmen, aes(x=Field, y=Museum)) +
  geom_point() +
  annotate('text',label=paste(italic_R,beak_culmen_cor,
                              italic_n,beak_culmen_sample_numbers,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  theme(legend.position = "none") +
  labs(x ="Live sample", y = "Specimen") +
  geom_abline(intercept = mean(beak_culmen_BA$beak_culmen_diffp), colour = "blue",linetype=1,size=1.2)+
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
scatter_beak_culmen

#Step 11: Create Bland-Altman plot to illustrate proportional differencers between samples
diffplot_beak_culmen <- ggplot(beak_culmen_BA, aes(beak_culmen_baseline, beak_culmen_diffp)) + 
  geom_point(size=2, colour = rgb(0,0,0, alpha = 0.5)) +
  geom_hline(yintercept = mean(beak_culmen_BA$beak_culmen_diffp),colour="blue",linetype=1,size=1) +
  geom_hline(yintercept = 1, colour = "red", size = 1,linetype="dashed") +
  geom_hline(yintercept = mean(beak_culmen_BA$beak_culmen_diffp) + 1.96*beak_culmen_sd.diffp, linetype = 2) +
  geom_hline(yintercept = mean(beak_culmen_BA$beak_culmen_diffp) - 1.96*beak_culmen_sd.diffp, linetype = 2) +
  ggtitle(paste0('Beak length (culmen): ', beak_culmen_mean_var, '%' )) +
  scale_y_continuous(breaks = (seq(0.6, 1.8, 0.2)), limits = c(0.6, 1.7))+
  ylab("Difference between specimen \nand live sample means (S/L)") +
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
  draw_plot(scatter_beak_culmen, width = 0.35, height = 0.35, x = 0.58, y = 0.54)+
  draw_label(label_beak_culmen,x = 0.58, y = 0.15, hjust = 0)+
  draw_label(label_beak_culmen_stat,x = 0.58, y = 0.18, hjust = 0)
  
#Optional: Save individual plots
#ggsave(filename = paste0(figFolder,'FM_beak_culmen.pdf'),width = 7,height = 7, dpi = 600, units = "in", device='pdf')
#ggsave(filename = paste0(figFolder,'FM_beak_culmen.jpg'),width = 7,height = 7, dpi = 600, units = "in", device='jpg')

#######################################################################
# make figure 5 - [Tail length] compare field vs. museum samples
#######################################################################
#GLMM for plot
tail_length_glmm <- lmer(values~ind+(1|BirdLife.order/BirdLife.family),data=tail_length2)
tail_length_glmm_output <- anova(tail_length_glmm) 

#Calculate, mean, sd and cv.
tail_length$mean <- apply(tail_length[, c("Field","Museum")],1,mean)
tail_length$sd <- apply(tail_length[, c("Field","Museum")],1,sd)
tail_length$cv <- (tail_length$sd / tail_length$mean) * 100

#Calculate mean_Var for each trait, correlation coefficient and number of samples
tail_length_mean_var <- round(mean(tail_length$cv),digits = 2)
tail_length_sample_numbers <- dplyr::count(tail_length)
tail_length_cor <- round(cor(tail_length$Field,tail_length$Museum),3)
tail_length_stat <- t.test(tail_length$Field,tail_length$Museum,paired = F,alternative = "two.sided")

#Calculate values for the Bland-Altman plots
tail_length_baseline <-tail_length$Field
tail_length_post <-tail_length$Museum
tail_length_diff <- (tail_length_post - tail_length_baseline)
tail_length_diffp <- ((tail_length_post - tail_length_baseline)/tail_length_baseline)+1
tail_length_sd.diff <- sd(tail_length_diff)
tail_length_sd.diffp <- sd(tail_length_diffp)
tail_length_BA <- data.frame(tail_length_baseline, tail_length_post, tail_length_diff, tail_length_diffp)

#Create specific text functions
label_tail_length <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                list(cv=tail_length_cor,n=tail_length_sample_numbers$n))
label_tail_length_stat <- substitute(paste("GLMM: ",italic("F")," = ",fvalue,", ",italic("p")," = ",stat),
                                     list(stat=pvalue(tail_length_glmm_output$`Pr(>F)`),
                                          fvalue=format(tail_length_glmm_output$`F value`,digits=2)))

#Create Scatter plot to illustrate alignment of field and museum samples
scatter_tail_length <- 
  ggplot(tail_length, aes(x=Field, y=Museum)) +
  geom_point() +
  annotate('text',label=paste(italic_R,tail_length_cor,
                              italic_n,tail_length_sample_numbers,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  theme(legend.position = "none") +
  labs(x ="Live sample", y = "Specimen") +
  geom_abline(intercept = mean(tail_length_BA$tail_length_diffp), colour = "blue",linetype=1,size=1.2)+
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,350)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,350)) +
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

#Create Bland-Altman plot to illustrate proportional differencers between samples
diffplot_tail_length <- ggplot(tail_length_BA, aes(tail_length_baseline, tail_length_diffp)) + 
  geom_point(size=2, colour = rgb(0,0,0, alpha = 0.5)) + 
  geom_hline(yintercept = mean(tail_length_BA$tail_length_diffp),colour="blue",linetype=1,size=1) +
  geom_hline(yintercept = 1, colour = "red", size = 1,linetype="dashed") +
  geom_hline(yintercept = mean(tail_length_BA$tail_length_diffp) + 1.96*tail_length_sd.diffp, linetype = 2) +
  geom_hline(yintercept = mean(tail_length_BA$tail_length_diffp) - 1.96*tail_length_sd.diffp, linetype = 2) +
  ggtitle(paste0('Tail length: ', tail_length_mean_var, '%' )) +
  scale_y_continuous(breaks = (seq(0.6, 1.8, 0.2)), limits = c(0.6, 1.7))+
  ylab("Difference between specimen \nand live sample means (S/L)") +
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
  draw_plot(scatter_tail_length, width = 0.35, height = 0.35, x = 0.58, y = 0.54)+
  draw_label(label_tail_length,x = 0.58, y = 0.15, hjust = 0)+
  draw_label(label_tail_length_stat,x = 0.58, y = 0.18, hjust = 0)

#Optional: Save individual plots
#ggsave(filename = paste0(figFolder,'FM_tail_length.pdf'),width = 7,height = 7, dpi = 600, units = "in", device='pdf')
#ggsave(filename = paste0(figFolder,'FM_tail_length.jpg'),width = 7,height = 7, dpi = 600, units = "in", device='jpg')

#######################################################################
# make figure 5 - [Tarsus length] compare field vs. museum samples
#######################################################################
#GLMM for plot
tarsus_length_glmm <- lmer(values~ind+(1|BirdLife.order/BirdLife.family),data=tarsus_length2)
tarsus_length_glmm_output <- anova(tarsus_length_glmm) 

#Calculate, mean, sd and cv.
tarsus_length$mean <- apply(tarsus_length[, c("Field","Museum")],1,mean)
tarsus_length$sd <- apply(tarsus_length[, c("Field","Museum")],1,sd)
tarsus_length$cv <- (tarsus_length$sd / tarsus_length$mean) * 100

#Calculate mean_Var for each trait, correlation coefficient and number of samples
tarsus_length_mean_var <- round(mean(tarsus_length$cv),digits = 2)
tarsus_length_sample_numbers <- dplyr::count(tarsus_length)
tarsus_length_cor <- round(cor(tarsus_length$Field,tarsus_length$Museum),3)
tarsus_length_stat <- t.test(tarsus_length$Field,tarsus_length$Museum,paired = F,alternative = "two.sided")

#Calculate values for the Bland-Altman plots
tarsus_length_baseline <-tarsus_length$Field
tarsus_length_post <-tarsus_length$Museum
tarsus_length_diff <- (tarsus_length_post - tarsus_length_baseline)
tarsus_length_diffp <- ((tarsus_length_post - tarsus_length_baseline)/tarsus_length_baseline)+1
tarsus_length_sd.diff <- sd(tarsus_length_diff)
tarsus_length_sd.diffp <- sd(tarsus_length_diffp)
tarsus_length_BA <- data.frame(tarsus_length_baseline, tarsus_length_post, tarsus_length_diff, tarsus_length_diffp)

#Create specific text functions
label_tarsus_length <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                list(cv=tarsus_length_cor,n=tarsus_length_sample_numbers$n))
label_tarsus_length_stat <- substitute(paste("GLMM: ",italic("F")," = ",fvalue,", ",italic("p")," = ",stat),
                                     list(stat=pvalue(tarsus_length_glmm_output$`Pr(>F)`),
                                          fvalue=format(tarsus_length_glmm_output$`F value`,digits=2)))

#Create Scatter plot to illustrate alignment of field and museum samples
scatter_tarsus_length <- 
  ggplot(tarsus_length, aes(x=Field, y=Museum)) +
  geom_point() +
  annotate('text',label=paste(italic_R,tarsus_length_cor,
                              italic_n,tarsus_length_sample_numbers,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  theme(legend.position = "none") +
  labs(x ="Live sample", y = "Specimen") +
  geom_abline(intercept = mean(tarsus_length_BA$tarsus_length_diffp), colour = "blue",linetype=1,size=1.2)+
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,90)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,90)) +
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

#Create Bland-Altman plot to illustrate proportional differencers between specimen samples
diffplot_tarsus_length <- ggplot(tarsus_length_BA, aes(tarsus_length_baseline, tarsus_length_diffp)) + 
  geom_point(size=2, colour = rgb(0,0,0, alpha = 0.5)) +
  geom_hline(yintercept = mean(tarsus_length_BA$tarsus_length_diffp),colour="blue",linetype=1,size=1) +
  geom_hline(yintercept = 1, colour = "red", size = 1,linetype="dashed") +
  geom_hline(yintercept = mean(tarsus_length_BA$tarsus_length_diffp) + 1.96*tarsus_length_sd.diffp, linetype = 2) +
  geom_hline(yintercept = mean(tarsus_length_BA$tarsus_length_diffp) - 1.96*tarsus_length_sd.diffp, linetype = 2) +
  ggtitle(paste0('Tarsus length: ', tarsus_length_mean_var, '%' )) +
  scale_y_continuous(breaks = (seq(0.6, 1.8, 0.2)), limits = c(0.6, 1.7))+
  ylab("Difference between specimen \nand live sample means (S/L)") +
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
  draw_plot(scatter_tarsus_length, width = 0.35, height = 0.35, x = 0.58, y = 0.54)+
  draw_label(label_tarsus_length,x = 0.58, y = 0.15, hjust = 0)+
  draw_label(label_tarsus_length_stat,x = 0.58, y = 0.18, hjust = 0)

#Optional: Save individual plots
#ggsave(filename = paste0(figFolder,'FM_tarsus_length.pdf'),width = 7,height = 7, dpi = 600, units = "in", device='pdf')
#ggsave(filename = paste0(figFolder,'FM_tarsus_length.jpg'),width = 7,height = 7, dpi = 600, units = "in", device='jpg')

#######################################################################
# make figure 5 - [Wing length] compare field vs. museum samples
#######################################################################
#GLMM for plot
wing_length_glmm <- lmer(values~ind+(1|BirdLife.order/BirdLife.family),data=wing_length2)
wing_length_glmm_output <- anova(wing_length_glmm) 

#Calculate, mean, sd and cv.
wing_length$mean <- apply(wing_length[, c("Field","Museum")],1,mean)
wing_length$sd <- apply(wing_length[, c("Field","Museum")],1,sd)
wing_length$cv <- (wing_length$sd / wing_length$mean) * 100

#Calculate mean_Var for each trait, correlation coefficient and number of samples
wing_length_mean_var <- round(mean(wing_length$cv),digits = 2)
wing_length_sample_numbers <- dplyr::count(wing_length)
wing_length_cor <- round(cor(wing_length$Field,wing_length$Museum),3)
wing_length_stat <- t.test(wing_length$Field,wing_length$Museum,paired = F,alternative = "two.sided")

#Calculate values for the Bland-Altman plots
wing_length_baseline <-wing_length$Field
wing_length_post <-wing_length$Museum
wing_length_diff <- (wing_length_post - wing_length_baseline)
wing_length_diffp <- ((wing_length_post - wing_length_baseline)/wing_length_baseline)+1
wing_length_sd.diff <- sd(wing_length_diff)
wing_length_sd.diffp <- sd(wing_length_diffp)
wing_length_BA <- data.frame(wing_length_baseline, wing_length_post, wing_length_diff, wing_length_diffp)

#Create specific text functions
label_wing_length <- substitute(paste(italic("R"["c"])," = ", cv, ", ",italic("n"),  " = ", n),
                                list(cv=wing_length_cor,n=wing_length_sample_numbers$n))
label_wing_length_stat <- substitute(paste("GLMM: ",italic("F")," = ",fvalue,", ",italic("p")," = ",stat),
                                     list(stat=pvalue(wing_length_glmm_output$`Pr(>F)`),
                                          fvalue=format(wing_length_glmm_output$`F value`,digits=2)))

#Create Scatter plot to illustrate alignment of field and museum samples
scatter_wing_length <- 
  ggplot(wing_length, aes(x=Field, y=Museum)) +
  geom_point() +
  annotate('text',label=paste(italic_R,wing_length_cor,
                              italic_n,wing_length_sample_numbers,sep='~'),
           x=-Inf,y=-Inf,hjust=-0.12,vjust=-33.5,size=4,parse=T)+
  theme(legend.position = "none") +
  labs(x ="Live sample", y = "Specimen") +
  geom_abline(intercept = mean(wing_length_BA$wing_length_diffp), colour = "blue",linetype=1,size=1.2)+
  geom_abline(intercept = 0, colour = "red", size = 1,linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,320)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,320)) +
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

#Create Bland-Altman plot to illustrate proportional differencers between specimen samples
diffplot_wing_length <- ggplot(wing_length_BA, aes(wing_length_baseline, wing_length_diffp)) + 
  geom_point(size=2, colour = rgb(0,0,0, alpha = 0.5)) + 
  geom_hline(yintercept = mean(wing_length_BA$wing_length_diffp),colour="blue",linetype=1,size=1) +
  geom_hline(yintercept = 1, colour = "red", size = 1,linetype="dashed") +
  geom_hline(yintercept = mean(wing_length_BA$wing_length_diffp) + 1.96*wing_length_sd.diffp, linetype = 2) +
  geom_hline(yintercept = mean(wing_length_BA$wing_length_diffp) - 1.96*wing_length_sd.diffp, linetype = 2) +
  ggtitle(paste0('Wing length: ', wing_length_mean_var, '%' )) +
  scale_y_continuous(breaks = (seq(0.6, 1.8, 0.2)), limits = c(0.6, 1.7))+
  ylab("Difference between specimen \nand live sample means (S/L)") +
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
  draw_plot(scatter_wing_length, width = 0.35, height = 0.35, x = 0.58, y = 0.54)+
  draw_label(label_wing_length,x = 0.58, y = 0.15, hjust = 0)+
  draw_label(label_wing_length_stat,x = 0.58, y = 0.18, hjust = 0)

#Optional: Save individual plots
#ggsave(filename = paste0(figFolder,'FM_wing_length.pdf'),width = 7,height = 7, dpi = 600, units = "in", device='pdf')
#ggsave(filename = paste0(figFolder,'FM_wing_length.jpg'),width = 7,height = 7, dpi = 600, units = "in", device='jpg')

######################################################################
# Final Figure S5
######################################################################
#Saved combined plots into Fig S1 and S2
plot_grid(combined_beak_culmen,combined_tarsus_length,
          combined_wing_length,combined_tail_length,
          ncol = 2,
          nrow=2,
          #labels = c('a','b','c','d'),
          label_size = 20)

ggsave(filename = paste0(figFolder,'Figure S5.pdf'),width = 14, 
       height = 14, dpi = 400, units = "in", device='pdf')
ggsave(filename = paste0(figFolder,'Figure S5.jpg'),width = 14, 
       height = 14, dpi = 600, units = "in", device='jpg')


