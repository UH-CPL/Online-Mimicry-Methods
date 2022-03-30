#Subject
fdraw_area_plot_DomEmo <- function(subj_facs_df, subj, area_plot_type) {
  area_plot <- subj_facs_df %>%
    select(Treatment_Time_New, 
           F_1) %>% 
    gather(key = "Expression", value = "Value", -Treatment_Time_New)
  area_plot$Expression = area_plot$Value
  area_plot$Value[which(!is.na(area_plot$Expression))] = 1
  area_plot$Value = as.numeric(area_plot$Value)
  
  if (area_plot_type=='area') {
    area_plot <- ggplot(area_plot) +
      geom_area(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
                alpha = 0.5)
  } else if (area_plot_type=='bar') {
    area_plot <- ggplot(area_plot) +
      geom_bar(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
               alpha = 0.5, 
               stat = "identity")
  }
  
  
  area_plot <- area_plot +
    xlab("") +
    ylab("") +
    ggtitle(sub) +
    
    scale_color_manual(values = c("Neutral"="Light Grey",
                                  "Surprised"="Cyan",
                                  "Sad"="Blue",
                                  "Happy"="Green",
                                  "Afraid"="Orange",
                                  "Disgusted"="Brown",
                                  "Angry"="Red"
    )) +
    
    scale_fill_manual(values = c("Neutral"="Light Grey",
                                 "Surprised"="Cyan",
                                 "Sad"="Blue",
                                 "Happy"="Green",
                                 "Afraid"="Orange",
                                 "Disgusted"="Brown",
                                 "Angry"="Red"
    )) +
    
    theme_bw() +
    theme(text=element_text(size=10),
          axis.text = element_text(size=10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position='top',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    size=10,
                                    margin=margin(t=0, r=0, b=0, l=0)), ##top, right, bottom, left
          plot.margin = unit(c(0.5, 2, 0.5, 0.5), "lines")) ##top, right, bottom, left
  # 
  
  return(area_plot)
}





#Left
ldraw_area_plot_DomEmo <- function(subj_facs_df, subj, area_plot_type) {
  area_plot <- subj_facs_df %>%
    select(Treatment_Time_New, 
           L_1) %>% 
    gather(key = "Expression", value = "Value", -Treatment_Time_New)
  area_plot$Expression = area_plot$Value
  area_plot$Value[which(!is.na(area_plot$Expression))] = 1
  area_plot$Value = as.numeric(area_plot$Value)
  
  if (area_plot_type=='area') {
    area_plot <- ggplot(area_plot) +
      geom_area(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
                alpha = 0.5)
  } else if (area_plot_type=='bar') {
    area_plot <- ggplot(area_plot) +
      geom_bar(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
               alpha = 0.5, 
               stat = "identity")
  }
  
  
  area_plot <- area_plot +
    xlab("") +
    ylab("") +
    ggtitle("Left Judge") +
    
    scale_color_manual(values = c("neutral"="Light Grey",
                                  "surprised"="Cyan",
                                  "sad"="Blue",
                                  "happy"="Green",
                                  "afraid"="Orange",
                                  "disgusted"="Brown",
                                  "angry"="Red"
    )) +
    
    scale_fill_manual(values = c("neutral"="Light Grey",
                                 "surprised"="Cyan",
                                 "sad"="Blue",
                                 "happy"="Green",
                                 "afraid"="Orange",
                                 "disgusted"="Brown",
                                 "angry"="Red"
    )) +
    
    theme_bw() +
    theme(text=element_text(size=10),
          axis.text = element_text(size=10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position='top',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    size=10,
                                    margin=margin(t=0, r=0, b=0, l=0)), ##top, right, bottom, left
          plot.margin = unit(c(0.5, 2, 0.5, 0.5), "lines")) ##top, right, bottom, left
  # 
  
  return(area_plot)
}





#Center
cdraw_area_plot_DomEmo <- function(subj_facs_df, subj, area_plot_type) {
  area_plot <- subj_facs_df %>%
    select(Treatment_Time_New, 
           C_1) %>% 
    gather(key = "Expression", value = "Value", -Treatment_Time_New)
  area_plot$Expression = area_plot$Value
  area_plot$Value[which(!is.na(area_plot$Expression))] = 1
  area_plot$Value = as.numeric(area_plot$Value)
  
  if (area_plot_type=='area') {
    area_plot <- ggplot(area_plot) +
      geom_area(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
                alpha = 0.5)
  } else if (area_plot_type=='bar') {
    area_plot <- ggplot(area_plot) +
      geom_bar(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
               alpha = 0.5, 
               stat = "identity")
  }
  
  
  area_plot <- area_plot +
    xlab("") +
    ylab("") +
    ggtitle("Center Judge") +
    
    scale_color_manual(values = c("neutral"="Light Grey",
                                  "surprised"="Cyan",
                                  "sad"="Blue",
                                  "happy"="Green",
                                  "afraid"="Orange",
                                  "disgusted"="Brown",
                                  "angry"="Red"
    )) +
    
    scale_fill_manual(values = c("neutral"="Light Grey",
                                 "surprised"="Cyan",
                                 "sad"="Blue",
                                 "happy"="Green",
                                 "afraid"="Orange",
                                 "disgusted"="Brown",
                                 "angry"="Red"
    )) +
    
    theme_bw() +
    theme(text=element_text(size=10),
          axis.text = element_text(size=10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position='top',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    size=10,
                                    margin=margin(t=0, r=0, b=0, l=0)), ##top, right, bottom, left
          plot.margin = unit(c(0.5, 2, 0.5, 0.5), "lines")) ##top, right, bottom, left
  # 
  
  return(area_plot)
}





#Right  
rdraw_area_plot_DomEmo <- function(subj_facs_df, subj, area_plot_type) {
  area_plot <- subj_facs_df %>%
    select(Treatment_Time_New, 
           R_1) %>% 
    gather(key = "Expression", value = "Value", -Treatment_Time_New)
  area_plot$Expression = area_plot$Value
  area_plot$Value[which(!is.na(area_plot$Expression))] = 1
  area_plot$Value = as.numeric(area_plot$Value)
  
  if (area_plot_type=='area') {
    area_plot <- ggplot(area_plot) +
      geom_area(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
                alpha = 0.5)
  } else if (area_plot_type=='bar') {
    area_plot <- ggplot(area_plot) +
      geom_bar(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
               alpha = 0.5, 
               stat = "identity")
  }
  
  
  area_plot <- area_plot +
    xlab("") +
    ylab("") +
    ggtitle("Right Judge") +
    
    scale_color_manual(values = c("neutral"="Light Grey",
                                  "surprised"="Cyan",
                                  "sad"="Blue",
                                  "happy"="Green",
                                  "afraid"="Orange",
                                  "disgusted"="Brown",
                                  "angry"="Red"
    )) +
    
    scale_fill_manual(values = c("neutral"="Light Grey",
                                 "surprised"="Cyan",
                                 "sad"="Blue",
                                 "happy"="Green",
                                 "afraid"="Orange",
                                 "disgusted"="Brown",
                                 "angry"="Red"
    )) +
    
    theme_bw() +
    theme(text=element_text(size=10),
          axis.text = element_text(size=10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position='top',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    size=10,
                                    margin=margin(t=0, r=0, b=0, l=0)), ##top, right, bottom, left
          plot.margin = unit(c(0.5, 2, 0.5, 0.5), "lines")) ##top, right, bottom, left
  # 
  
  return(area_plot)
}
