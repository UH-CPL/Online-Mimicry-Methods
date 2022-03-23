#Subject
fdraw_area_plot <- function(subj_facs_df, subj, treatment, area_plot_type) {
   area_plot <- subj_facs_df %>% 
      select(Treatment_Time_New, 
             F_Angry, 
             F_Disgusted, 
             F_Afraid, 
             F_Happy, 
             F_Sad, 
             F_Surprised, 
             F_Neutral) %>% 
      gather(key = "Expression", value = "Value", -Treatment_Time_New) %>% 
      mutate(Expression = recode_factor(Expression,
                                        'F_Angry'='Angry',
                                        'F_Disgusted'='Disgusted',
                                        'F_Afraid'='Afraid',
                                        'F_Happy'='Happy',
                                        'F_Sad'='Sad',
                                        'F_Surprised'='Surprised',
                                        'F_Neutral'='Neutral'
      )) %>% 
      
      # ggplot(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression))
      ggplot()
   # geom_point(aes(x=Treatment_Time_New, y=1.2, colour=Task), shape=15, size=2) +
   # scale_color_manual(values = c("Email" = "green",  "Report" = "white"))
   
   
   if (area_plot_type=='area') {
      area_plot <- area_plot +
         geom_area(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
                   alpha = 0.5)
   } else if (area_plot_type=='bar') {
      area_plot <- area_plot +
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
ldraw_area_plot <- function(subj_facs_df, subj, treatment, area_plot_type) {
   area_plot <- subj_facs_df %>% 
      select(Treatment_Time_New, 
             L_angry, 
             L_disgusted, 
             L_afraid, 
             L_happy, 
             L_sad, 
             L_surprised, 
             L_neutral) %>% 
      gather(key = "Expression", value = "Value", -Treatment_Time_New) %>% 
      mutate(Expression = recode_factor(Expression,
                                        'L_angry'='Angry',
                                        'L_disgusted'='Disgusted',
                                        'L_afraid'='Afraid',
                                        'L_happy'='Happy',
                                        'L_sad'='Sad',
                                        'L_surprised'='Surprised',
                                        'L_neutral'='Neutral'
      )) %>% 
      
      #ggplot(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression)) +
      ggplot()
      #geom_point(aes(x=Treatment_Time_New, y=1.2, colour=G_Direction), shape=15, size=2) +
      #scale_color_manual(values = c("Right" = "green",  "Left" = "white"))
   
   
   if (area_plot_type=='area') {
      area_plot <- area_plot +
         geom_area(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
                   alpha = 0.5)
   } else if (area_plot_type=='bar') {
      area_plot <- area_plot +
         geom_bar(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
                  alpha = 0.5, 
                  stat = "identity")
   }
   
   
   area_plot <- area_plot +
      xlab("") +
      ylab("") +
      ggtitle("Left Judge") +
      
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
            legend.position='left',
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5,
                                      size=10,
                                      margin=margin(t=0, r=0, b=0, l=0)), ##top, right, bottom, left
            plot.margin = unit(c(0.5, 2, 0.5, 0.5), "lines")) ##top, right, bottom, left
   # 
   
   return(area_plot)
}



#Center
cdraw_area_plot <- function(subj_facs_df, subj, treatment, area_plot_type) {
   area_plot <- subj_facs_df %>% 
      select(Treatment_Time_New, 
             C_angry, 
             C_disgusted, 
             C_afraid, 
             C_happy, 
             C_sad, 
             C_surprised, 
             C_neutral) %>% 
      gather(key = "Expression", value = "Value", -Treatment_Time_New) %>% 
      mutate(Expression = recode_factor(Expression,
                                        'C_angry'='Angry',
                                        'C_disgusted'='Disgusted',
                                        'C_afraid'='Afraid',
                                        'C_happy'='Happy',
                                        'C_sad'='Sad',
                                        'C_surprised'='Surprised',
                                        'C_neutral'='Neutral'
      )) %>% 
      
      # ggplot(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression))
      ggplot()
   # geom_point(aes(x=Treatment_Time_New, y=1.2, colour=Task), shape=15, size=2) +
   # scale_color_manual(values = c("Email" = "green",  "Report" = "white"))
   
   
   if (area_plot_type=='area') {
      area_plot <- area_plot +
         geom_area(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
                   alpha = 0.5)
   } else if (area_plot_type=='bar') {
      area_plot <- area_plot +
         geom_bar(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
                  alpha = 0.5, 
                  stat = "identity")
   }
   
   
   area_plot <- area_plot +
      xlab("") +
      ylab("") +
      ggtitle("Center Judge") +
      
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
            legend.position='left',
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5,
                                      size=10,
                                      margin=margin(t=0, r=0, b=0, l=0)), ##top, right, bottom, left
            plot.margin = unit(c(0.5, 2, 0.5, 0.5), "lines")) ##top, right, bottom, left
   # 
   
   return(area_plot)
}




#Right  
rdraw_area_plot <- function(subj_facs_df, subj, treatment, area_plot_type) {
   area_plot <- subj_facs_df %>% 
      select(Treatment_Time_New, 
             R_angry, 
             R_disgusted, 
             R_afraid, 
             R_happy, 
             R_sad, 
             R_surprised, 
             R_neutral) %>% 
      gather(key = "Expression", value = "Value", -Treatment_Time_New) %>% 
      mutate(Expression = recode_factor(Expression,
                                        'R_angry'='Angry',
                                        'R_disgusted'='Disgusted',
                                        'R_afraid'='Afraid',
                                        'R_happy'='Happy',
                                        'R_sad'='Sad',
                                        'R_surprised'='Surprised',
                                        'R_neutral'='Neutral'
      )) %>% 
      
      #ggplot(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression)) +
      ggplot()
   #geom_point(aes(x=Treatment_Time_New, y=1.2, colour=G_Direction), shape=15, size=2) +
   #scale_color_manual(values = c("Right" = "green",  "Left" = "white"))
   
   if (area_plot_type=='area') {
      area_plot <- area_plot +
         geom_area(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
                   alpha = 0.5)
   } else if (area_plot_type=='bar') {
      area_plot <- area_plot +
         geom_bar(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
                  alpha = 0.5, 
                  stat = "identity")
   }
   
   
   area_plot <- area_plot +
      xlab("Time [s]") +
      ylab("") +
      ggtitle("Right Judge") +
      
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
            legend.position='left',
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5,
                                      size=10,
                                      margin=margin(t=0, r=0, b=0, l=0)), ##top, right, bottom, left
            plot.margin = unit(c(0.5, 2, 0.5, 0.5), "lines")) ##top, right, bottom, left
   # 
   
   return(area_plot)
}