###
  
  filename <-""
  print(filename)
  
  content <- read.csv(filename)
  
  print (content)
  
  
  library(ggplot2)
  library(dplyr)
  library(ggthemes)
  library(extrafont)
  library(readr)
  font_import()
  font()
  
  
  myColors = c("#A6611A", "#DFC27D", "#6e6c6b", "#80CDC1", "#018571")
  
  content %>%
    ggplot(aes(x = Caste,y=value,fill=performance))+
    geom_bar(position="dodge",  stat="identity", size=0.1, alpha=2,width=0.7)+
    labs(title = "MLA Performances vs. Caste",
         fill = "Performance")+
    scale_fill_brewer(palette="Dark2")+
    theme_stata()+
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text.x = element_text(size=5),axis.title = element_text(), text = element_text(family = "Rubik"))+
    geom_text(aes(label=value), vjust=1.5,position = position_dodge(0.5), size=1.5)
  

  filename <- paste0(location,file,sep="")
  print(filename)
  
  content1 <- read.csv(filename1)
  
  print (content1)
  
  
  library(ggplot2)
  library(dplyr)
  library(ggthemes)
  library(extrafont)
  library(readr)
  library(scales)
  # font_import()
  font()
  
  
  myColors = c("#A6611A", "#DFC27D", "#6e6c6b", "#80CDC1", "#018571")
  
  content %>%
    ggplot(aes(x = Age,y=value,fill=performance))+
    geom_bar(position="dodge",  stat="identity", size=0.5, alpha=4,width=0.8)+
    labs(fill = "Performance")+
    scale_fill_brewer(palette="accent")+
    theme_solarized()+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),axis.title = element_text(), text = element_text(family = "Rubik"))+
    geom_text(aes(label=value), vjust=-0.5,position = position_dodge(0.8), size=3)+
    theme(panel.background = element_rect(fill = "azure3", colour = "#6D9EC1",size = 2, linetype = "solid"),plot.background = element_rect(fill = "#ABA300"))+
    facet_grid(. ~ title) +
    theme(strip.background = element_rect(fill="#cd9600"), strip.text = element_text(size=15, colour="white"))
  
  
  content %>%
    ggplot(aes(x=" ", y=value, group=performance, colour=performance, fill=performance)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start=0) + 
    facet_grid(.~ Age) +
    # theme(strip.background = element_rect(fill="#cd9600"), strip.text = element_text(size=15, colour="white"))
    theme_stata() +
    scale_fill_stata() +
    labs(title = "MLA Performances vs. Age",
         fill = "Performance")+
    theme(plot.background = element_rect(fill = "#BFD5E3"))+
    geom_text(aes(label = paste0(value,"%")), position = position_stack(vjust = 0.5),show.legend = FALSE, size=2,colour="black")+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),text = element_text(family = "Rubik"))+
    coord_polar(theta = "y") +
    theme(axis.text = element_blank(),axis.ticks = element_blank(),panel.grid  = element_blank())
   
  
  content$title = "MLA Performance vs Age"
  
  content %>%
    ggplot(aes(x = Age,y=value,fill=performance))+
    geom_bar(position="dodge",  stat="identity", size=0.5, alpha=4,width=0.8)+
    labs(fill = "Performance")+
    theme_hc() +
    scale_fill_hc() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),axis.title = element_text(), text = element_text(family = "Rubik"))+
    geom_text(aes(label=value), vjust=-0.5,position = position_dodge(0.8), size=3)+
    facet_grid(. ~ title) 
  

###
  #BFD5E3