##### biodiversity indices calculations
#=========
####load data
rm(list=ls())

load("data/Lica/BBS_partition_abundance.rda")
abund <- BBS_partition_abundance; rm(BBS_partition_abundance)

load("data/Lica/BBS_partition_abundance_2021.rda")

bbs2021 <- bbs2021 %>% select(-segment)

abund.all <- bind_rows(abund, bbs2021)

#=========
#### calculate diversity indices for birds for all years

library(dplyr); library(reshape2); library(vegan); library(tidyr)

years.all <- unique(abund.all$year)

div_list <- list()
sp_mat_list <- list()

for(i in seq_along(years.all)){
  
  tmp <- abund.all %>%
    subset(year == years.all[i]) %>%
    group_by(partition)
  
  sp.matrix <- dcast(tmp, partition ~ animal_jetz, 
                     fun.aggregate = mean,  
                     value.var = "seg_abundance",
                     fill = 0)
  
  sp.mat <- sp.matrix
  sp.mat$year <- years.all[i]
  sp_mat_list[[i]] <- sp.mat
  
  rownames(sp.matrix) <- sp.matrix$partition
  sp.matrix <- sp.matrix[,-1]
  
  div_df <- as.data.frame(matrix(NA, nrow = length(unique(tmp$partition)), ncol=4))
  colnames(div_df) <- c("shannon", "simpson", "richness", "segment")
  
  div_df$shannon <- diversity(sp.matrix, index = "shannon")
  div_df$simpson <- diversity(sp.matrix, index = "simpson")
  div_df$richness <- rowSums(sp.matrix != 0)
  div_df$segment <- unique(tmp$partition)
  
  div_list[[i]] <- div_df
}

names(div_list) <- years.all

rm(i, tmp, sp.matrix, div_df, sp.mat)

save(sp_mat_list, file = "/Users/jonwent/Downloads/sp_mat_list.rda")
rm(sp_mat_list)

### create 1 data frame with 3 columns of shannon, simpson and richness where rows are segment+year combinations

rich.all <- lapply(div_list, function(df) df[[3]])
rich.stack <- stack(rich.all); rm(rich.all)
colnames(rich.stack) <- c("richness", "year")
rich.stack$ID <- seq.int(nrow(rich.stack))

seg.all <- lapply(div_list, function(df) df[[4]])
seg.stack <- stack(seg.all); rm(seg.all)
colnames(seg.stack) <- c("segment", "year")
seg.stack$ID <- seq.int(nrow(seg.stack))

shannon.all <- lapply(div_list, function(df) df[[1]])
shannon.stack <- stack(shannon.all); rm(shannon.all)
colnames(shannon.stack) <- c("shannon", "year")
shannon.stack$ID <- seq.int(nrow(shannon.stack))

simpson.all <- lapply(div_list, function(df) df[[2]])
simpson.stack <- stack(simpson.all); rm(simpson.all)
colnames(simpson.stack) <- c("simpson", "year")
simpson.stack$ID <- seq.int(nrow(simpson.stack))

biodiv.stack1 <- dplyr::full_join(seg.stack, rich.stack, by = "ID")
biodiv.stack2 <- dplyr::full_join(shannon.stack, simpson.stack, by = "ID")
biodiv.df <- merge(biodiv.stack1, biodiv.stack2, by="ID")

rm(simpson.stack, shannon.stack, rich.stack, seg.stack, biodiv.stack1, biodiv.stack2)

biodiv.df <- biodiv.df %>% select(year.x.x, segment, richness, shannon, simpson)
biodiv.df <- rename(biodiv.df, year = year.x.x)

biodiv.size <- biodiv.df %>% group_by(year) %>% summarize(num=n())

save(biodiv.df, file="data/Lica/biodiv_indices.rda")

# rich.wide <- rich_stack %>%
#   pivot_wider(rows = everything(), names_from = "year", values_from = "richness")

#=========

#investigate the number of segments per year with no observed birds (richness == 0)

seg_NAN <- list()

for(i in seq_along(years.all)){
  div.now <- div_list[[i]]
  seg.now <- div.now[div.now$richness == 0,]
  seg_NAN[[i]] <- seg.now
}

rm(i, seg.now, div.now)

#==========

#Set up visualizations of the biodiversity indices

library(ggplot2); library(hrbrthemes); library(viridis)

#==========
### violin plots of Shannon and simspon index

for(i in seq_along(years.all)){
  div.now <- div_list[[i]]
  div.now <- div.now[,-(3:4)]
  div.now.long <- div.now %>%
    pivot_longer(cols = everything(), names_to = "name", values_to = "value")

  div.now.long <- div.now.long %>% arrange(name)

  sample_size = div.now.long %>% group_by(name) %>% summarize(num=n())
  
  plot <- div.now.long %>%
    left_join(sample_size) %>%
    mutate(myaxis = paste0(name, "\n", "n=", num)) %>%
    ggplot( aes(x = myaxis, y = value, fill = name)) +
    geom_violin(width = 1.4, position = "identity") +
    geom_boxplot(width = 0.1,
                 color = "grey",
                 alpha = 0.2) +
    scale_fill_viridis(discrete = TRUE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle(paste0("Biodiversity indices in the year ", years.all[i])) +
    xlab("")+
    ylab("")
  
  print(plot)
}

rm(div.now, div.now.long, plot, sample_size, i)

#============
## ridge line plots to visualize the years for an index together

library(ggridges)

###plot
ggplot(biodiv.df, aes(x = `richness`, y = `year`, fill = after_stat(x)))+ 
  geom_density_ridges_gradient(scale = 3,
                               rel_min_height = 0.02,
                               bandwidth = 2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 65)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.17))) +
  scale_fill_viridis(name = "richness", option = "C") +
  ylab("") +
  xlab("Richness")+
  ggtitle(paste0("USBBS Richness 2000-2021"))

ggplot(biodiv.df, aes(x = `shannon`, y = `year`, fill = after_stat(x)))+ 
  geom_density_ridges_gradient(scale = 3.5, rel_min_height = 0.02, bandwidth = 0.1) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.9, 4)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.2))) +
  scale_fill_viridis(name = "shannon", option = "C") +
  ylab("") +
  xlab("Shannon index")+
  ggtitle(paste0("USBBS Shannon index 2000-2021"))

ggplot(biodiv.df, aes(x = `simpson`, y = `year`, fill = after_stat(x)))+ 
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.02, bandwidth = 0.01) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.65, 1)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.14))) +
  scale_fill_viridis(name = "simpson", option = "C")+
  ylab("") +
  xlab("Simpson Index") +
  ggtitle(paste0("USBBS Simpson index 2000-2021"))
