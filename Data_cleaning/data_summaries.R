library(ggplot2)
library(ggpubr)
library(reshape2)
library(gridExtra)
library(grid)
library(scico) #for colour

#Carabid species number
length(unique(final_connections$Carabidae_GBIFID))

#Fungus species number
length(unique(final_connections$Fungus_GBIFID))

# number of references (those with yet missing ref code are not included, +2)
length(unique(final_connections$final_ref_code))

# number of unique connections
length(unique(paste(final_connections$Fungus_GBIFID, final_connections$Carabidae_GBIFID)))

# number of unique countries
length(unique(final_connections$UN))


# number of unique connections
length(unique(paste(final_connections$Carabidae_GBIFID, 
                    final_connections$Fungus_GBIFID, 
                    final_connections$UN)))

# number of unique connections in geographic regions
aggregate(paste(final_connections$Carabidae_GBIFID, 
                       final_connections$Fungus_GBIFID), list(final_connections$REGION, final_connections$Region.Name), length)


colnames(final_connections)
years<-min(final_connections$Publication.Year, na.rm = T):
  max(final_connections$Publication.Year, na.rm = T)


connections_year<-lapply(years,
                         function(x) unique(final_connections[final_connections$Publication.Year==x, 
                                                    c("Carabidae_GBIFID", "Fungus_GBIFID")]))

connections_year<-lapply(connections_year, function(x) {x<-x[!rownames(x)=="NA",]
                                                        return(x)})

c_connections<-function(x) {if(x==1){connections_year[[1]]} else
  {zz<-do.call(rbind, connections_year[1:x])
  unique(zz)}}

n_connections<-function(x) {if(x==1){cum_connection[[1]]} else
{zz<-as.data.frame(cum_connection[x])
yy<-as.data.frame(cum_connection[x-1])
zz<-zz[!paste(zz[,1], zz[,1]) %in% paste(yy[,1], yy[,1]),]
zz}}


cum_connection<-lapply(1:length(connections_year), c_connections)
new_connection<-lapply(1:length(connections_year), n_connections)

connections_year_sum<-as.data.frame(sapply(connections_year, nrow))
connections_year_sum$cum_connections<-sapply(cum_connection, nrow)
connections_year_sum$new_connections<-sapply(new_connection, nrow)

connections_year_sum$year<-as.character(years)

#length is different because some data rows are missing publication year that will be fixed
connections_year_sum$cum_connections

rownames(connections_year_sum)<-years
colnames(connections_year_sum)[1]<-"sum_connections"


sum_smooth <- as.data.frame(predict(smooth.spline(connections_year_sum$year, 
                                                  connections_year_sum$sum_connections, spar=.7),
                                    as.numeric(connections_year_sum$year)))

sumlink_plot<-ggplot(data=connections_year_sum, aes(x=years, y=sum_connections), color="dark green") +
  geom_bar(stat="identity", fill=alpha("dark green", 0.4))+
  theme_minimal()+
  #geom_smooth(method = "auto", color="dark green")
  geom_line(data = sum_smooth, aes(x = x, y = y), color="dark green", size=1.2)


cum_smooth <- as.data.frame(predict(smooth.spline(connections_year_sum$year, 
                                                  connections_year_sum$cum_connections, spar=.7),
                                    as.numeric(connections_year_sum$year)))

cumlink_plot<-ggplot(data=connections_year_sum, aes(x=years, y=cum_connections), color="dark green") +
  geom_bar(stat="identity", fill=alpha("dark green", 0.4))+
  theme_minimal()+
    xlab("Year")+
    ylab("Cumulative number of\nconnections")+
  #geom_smooth(method = "auto", color="dark green")
  geom_line(data = cum_smooth, aes(x = x, y = y), color="dark green", size=1.2)


new_smooth <- as.data.frame(predict(smooth.spline(connections_year_sum$year, 
                                 connections_year_sum$new_connections, spar=.7),
                    as.numeric(connections_year_sum$year)))

newlink_plot<-ggplot(data=connections_year_sum, aes(x=years, y=new_connections), color="dark green") +
  geom_bar(stat="identity", fill=alpha("dark green", 0.4))+
  theme_minimal()+
  xlab("Year")+
  ylab("Number of\nnew connections")+
  #geom_smooth(method = "auto", color="dark green")
  geom_line(data = new_smooth, aes(x = x, y = y), color="dark green", size=1.2)

link_saturation<-ggarrange(newlink_plot, cumlink_plot,  nrow = 2, ncol = 1)
ggsave("../Plots/link_saturation.pdf", link_saturation, width = 10, height = 8)


### heatmap and side barplots of interactions and taxon number
subfamily_taxa<-aggregate(cleaned_carabid_species$GBIFID, 
                          list(cleaned_carabid_species$subfamily), 
                          function(x) length(unique(x)))
order_taxa<-aggregate(updated_fungus_species$GBIFID, 
                      list(updated_fungus_species$class), 
                      function(x) length(unique(x)))

interaction_numbers<-tapply(final_connections$Fungus_GBIFID, list(final_connections$Carabidae_subfamily, 
                                               final_connections$Fungus_class), length)
interaction_numbers[is.na(interaction_numbers)]<-0


int_num<-melt(interaction_numbers)
colnames(int_num)<-c("Carabidae", "Fungi", "No_links")
as.character(unique(int_num$Carabidae))

assoc_heatmap<-ggplot(int_num, aes(Carabidae, Fungi, fill= log(1+No_links))) + 
    geom_tile() +
    scale_fill_scico(name="Log number of links",palette = 'bilbao', begin=0.05) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
          legend.direction = "horizontal",
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 7))+
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

# getting the legend
tmp <- ggplot_gtable(ggplot_build(assoc_heatmap))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]

# removing legend from heatmap
assoc_heatmap_clean <- assoc_heatmap +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
          axis.ticks.y = element_blank(), axis.title.x = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          legend.position="none")


barcols<-scico(30, palette = 'berlin')[23]
cara_barplot<-ggplot(subfamily_taxa, aes(x=Group.1, y=log(1+x)))+
    geom_bar(stat="identity", fill=barcols)+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title.x = element_text(size = 20, margin = margin(10,0,0,0)),
          axis.ticks.y = element_blank(),
          legend.position = "none")+ 
          labs(x = "Carabidae subfamilies", y="log(1+taxon number)")

fungi_barplot<-ggplot(order_taxa, aes(x=Group.1, y=log(1+x)))+
    geom_bar(stat="identity", fill=barcols)+
    theme_minimal()+
    scale_y_continuous()+
    theme(axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 20, margin = margin(0,10,0,0), angle = -90),
          legend.position="none")+
    coord_flip()+ 
    labs(x = "Fungi classes", y="log(1+taxon number)")



#grob.title <- textGrob("Main Title", hjust = 0.5, vjust = 0.5, gp = gpar(fontsize = 20))
heatmap_all<-grid.arrange(cara_barplot, legend, assoc_heatmap_clean, fungi_barplot, nrow = 2, ncol = 2, 
             widths = c(50, 30), heights = c(50, 90))


ggsave("../Plots/heatmap_all.pdf", heatmap_all, width = 10, height = 10)
