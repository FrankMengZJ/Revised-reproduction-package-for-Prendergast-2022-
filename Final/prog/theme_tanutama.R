
  # Create new theme
  library(ggplot2)
  theme_tanutama <- function(base_size = 11, base_family = "", base_line_size = base_size/22, 
                            base_rect_size = base_size/22) {
    theme_void(base_size = base_size, base_family = base_family, 
               base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
      theme(
        plot.margin = grid::unit(c(3,10,10,20),"pt"),
        plot.title    = element_text(face="bold",size=18,vjust=15,hjust=0),
        plot.subtitle = element_text(face="bold",size=14,vjust=18,hjust=0),  # this is y-axis title
        axis.title = element_text(),
        axis.title.y = element_text(vjust=1.25, angle = 90, size=14),
        axis.title.x = element_text(margin=margin(7,0,7,0),size=14),
        axis.text  = element_text(colour="gray50"),
        axis.text.y= element_text(vjust=-0.5,size=12,margin=margin(0,-10,0,0)),
        axis.text.x= element_text(vjust=-0.5,size=12),
        axis.line.x= element_line(size=0.75),
        axis.ticks.x = element_line(),
        axis.ticks.length = unit(3, "pt"),
        panel.grid.major.y = element_line(colour="gray90",size=0.6),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        #legend.position=c(0,1.05),  
        #legend.justification = c(0,0),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.text = element_text(size=12),
        legend.spacing.x = unit(10, "pt"),
        legend.margin=margin(t = 0, unit='cm')
      ) 
    }
    
  test <- ggplot(data=diamonds, aes(carat,x,color=price)) + 
    geom_point(size=2.5) + 
    labs(title="Diamond Size and Value",
         subtitle="x-dimension") +
    xlab("Carat") +
    scale_color_continuous(name = "Diamond price") +
    theme_tanutama()
  