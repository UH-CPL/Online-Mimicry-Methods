library(ggplot2)
library(ggnewscale)

#Heatmap Function
draw_heatmap <- function(mat, fileName, main){
   d_mat = mat
   #Data Reshaping for plotting
   df = reshape2::melt(d_mat)
   print(head(df))
   print(sum(df$value, na.rm = T))
   #Creating a filename and defining the directory to save the plot
   fileName = paste0("Abstract/AbstractPlots/",fileName)
   
   df$Var1 = as.factor(df$Var1)
   df$Var2 = as.factor(df$Var2)
   
   df["val2"] = NA
   for (i in 1:nrow(df)) {
     v = as.character(df$Var1[i]) == as.character(df$Var2[i])
     if (v == TRUE) {
       df$val2[i] = df$value[i]
       df$value[i] = NA
     }
     
   }
   
   # #Plotting heatmap
   # plot1 = ggplot(df, aes(Var1, Var2, fill = value, label = value))+
   #    geom_tile()+
   #    geom_text(col="black")+
   #    scale_fill_gradientn(
   #       colours = c(low ="white","blue"), name = "", na.value = "white")+
   #    # scale_fill_viridis(name = "",limits = c(min(d_mat, na.rm = T),max(d_mat, na.rm = T)),oob=squish)+
   #    xlab("")+
   #    ylab("")+
   #    theme_bw()+
   #    theme(text = element_text(size=12), 
   #          panel.background = element_blank(),
   #          panel.grid = element_blank())+
   #    ggtitle(main)
   
     plot1 = ggplot(df, aes(x=Var1, y=Var2, label = val2)) +
     
     geom_tile(aes(fill=val2)) +
     geom_text(col="black")+
     # geom_text(aes(label=diagonal_val)) +
     #geom_text(aes(label=100*val2/sum(df$val2))) +
     scale_fill_gradientn(colours = c("lightgray","dimgray"), name = "", na.value = "white") +
     # scale_fill_gradientn(colours = c("white", "lightgray","dimgray"), name = "") +
     # scale_fill_gradientn(colours = c("yellow", "orange", "tomato2", "red", "red4", "black"), name = "") +
     
     new_scale("fill") +
     geom_tile(aes(fill=value), 
               data = subset(df, value >= 0)) +
     geom_text(aes(label = value)) +
     # geom_text(aes(label=non_diagonal_upper_matrix_val), data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
     #geom_text(aes(label=100*non_diagonal_upper_matrix_val/sum(heat_map_df$non_diagonal_upper_matrix_val)), 
     #          data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
     scale_fill_gradientn(colours = c("white", "yellow", "pink"), name = "", na.value = "white") + xlab("") + ylab("") + ggtitle(main) + 
       scale_x_discrete(position = "top") 
   
   
   print(plot1)
   ggsave(paste0(fileName,".pdf"), plot1, height = 8, width = 10)
}


#Heatmap Function
draw_heatmap2 <- function(mat, fileName, main, sub){
   mian = main
   d_mat = renamematrix2(mat)
   #Data Reshaping for plotting
   df = reshape2::melt(d_mat)
   #print(head(df))
   #print(sum(df$value, na.rm = T))
   frames = round(sum(d_mat, na.rm = T))
   main = paste0(main," / ",frames)
   #Creating a filename and defining the directory to save the plot
   fileName = paste0("Plots/",fileName)
   
   df$Var1 = as.factor(df$Var1)
   df$Var2 = as.factor(df$Var2)
   
   df["val2"] = NA
   for (i in 1:nrow(df)) {
      v = as.character(df$Var1[i]) == as.character(df$Var2[i])
      if (v == TRUE) {
         df$val2[i] = df$value[i]
         df$value[i] = NA
      }
      
   }
   
   # #Plotting heatmap
   # plot1 = ggplot(df, aes(Var1, Var2, fill = value, label = value))+
   #    geom_tile()+
   #    geom_text(col="black")+
   #    scale_fill_gradientn(
   #       colours = c(low ="white","blue"), name = "", na.value = "white")+
   #    # scale_fill_viridis(name = "",limits = c(min(d_mat, na.rm = T),max(d_mat, na.rm = T)),oob=squish)+
   #    xlab("")+
   #    ylab("")+
   #    theme_bw()+
   #    theme(text = element_text(size=12), 
   #          panel.background = element_blank(),
   #          panel.grid = element_blank())+
   #    ggtitle(main)
   
   assign(paste0("p",sub), ggplot(df, aes(x=Var1, y=Var2, label = val2)) +
      
      geom_tile(aes(fill=val2)) +
      #geom_text(aes(label = ""))+
      # geom_text(aes(label=diagonal_val)) +
      #geom_text(aes(label=100*val2/sum(df$val2))) +
      scale_fill_gradientn(colours = c("lightgray","dimgray"), name = "", na.value = "white") +
      # scale_fill_gradientn(colours = c("white", "lightgray","dimgray"), name = "") +
      # scale_fill_gradientn(colours = c("yellow", "orange", "tomato2", "red", "red4", "black"), name = "") +
      
      new_scale("fill") + theme(legend.position = "none")+
      geom_tile(aes(fill=value), 
                data = subset(df, value >= 0)) +
         
      theme(plot.title = element_text(hjust = 0.5, vjust = -5)) +
      #geom_text(aes(label = "")) +
      # geom_text(aes(label=non_diagonal_upper_matrix_val), 
      #   data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
      #geom_text(aes(label=100*non_diagonal_upper_matrix_val/sum(heat_map_df$non_diagonal_upper_matrix_val)), 
      #          data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
      scale_fill_gradientn(colours = c("white", "yellow", "pink"), 
                           name = "", na.value = "white") + xlab("") + ylab("") + ggtitle(main) + 
      scale_x_discrete(position = "top"), envir = .GlobalEnv)
   
   
   
      # ggplot(df, aes(x=Var1, y=Var2, label = val2)) +
      # 
      # geom_tile(aes(fill=val2)) +
      # #geom_text(aes(label = ""))+
      # # geom_text(aes(label=diagonal_val)) +
      # #geom_text(aes(label=100*val2/sum(df$val2))) +
      # scale_fill_gradientn(colours = c("lightgray","dimgray"), name = "", na.value = "white") +
      # # scale_fill_gradientn(colours = c("white", "lightgray","dimgray"), name = "") +
      # # scale_fill_gradientn(colours = c("yellow", "orange", "tomato2", "red", "red4", "black"), name = "") +
      # 
      # new_scale("fill") +
      # geom_tile(aes(fill=value),
      #           data = subset(df, value >= 0)) +
      # #geom_text(aes(label = "")) +
      # # geom_text(aes(label=non_diagonal_upper_matrix_val), data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
      # #geom_text(aes(label=100*non_diagonal_upper_matrix_val/sum(heat_map_df$non_diagonal_upper_matrix_val)),
      # #          data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
      # scale_fill_gradientn(colours = c("white", "yellow", "pink"), name = "", na.value = "white") + xlab("") + ylab("") + ggtitle(main) +
      # scale_x_discrete(position = "top")
      # 
      # 


   #print(plot1)
   #ggsave(paste0(fileName,".pdf"), paste0("p",sub), height = 8, width = 10)
}




draw_heatmapEmpty <- function(mat, fileName, main, sub){
   mian = main
   d_mat = renamematrix(mat)
   #Data Reshaping for plotting
   df = reshape2::melt(d_mat)
   #print(head(df))
   #print(sum(df$value, na.rm = T))
   frames = round(sum(d_mat, na.rm = T))
   main = paste0(main," / ",frames)
   #Creating a filename and defining the directory to save the plot
   fileName = paste0("Plots/",fileName)
   
   df$Var1 = as.factor(df$Var1)
   df$Var2 = as.factor(df$Var2)
   
   df["val2"] = NA
   for (i in 1:nrow(df)) {
      v = as.character(df$Var1[i]) == as.character(df$Var2[i])
      if (v == TRUE) {
         df$val2[i] = df$value[i]
         df$value[i] = NA
      }
      
   }
   
   # #Plotting heatmap
   # plot1 = ggplot(df, aes(Var1, Var2, fill = value, label = value))+
   #    geom_tile()+
   #    geom_text(col="black")+
   #    scale_fill_gradientn(
   #       colours = c(low ="white","blue"), name = "", na.value = "white")+
   #    # scale_fill_viridis(name = "",limits = c(min(d_mat, na.rm = T),max(d_mat, na.rm = T)),oob=squish)+
   #    xlab("")+
   #    ylab("")+
   #    theme_bw()+
   #    theme(text = element_text(size=12), 
   #          panel.background = element_blank(),
   #          panel.grid = element_blank())+
   #    ggtitle(main)
   
   assign(paste0("p",sub), ggplot(df, aes(x=Var1, y=Var2, label = val2)) +
             
             geom_tile(aes(fill=val2)) +
             #geom_text(aes(label = ""))+
             # geom_text(aes(label=diagonal_val)) +
             #geom_text(aes(label=100*val2/sum(df$val2))) +
             scale_fill_gradientn(colours = c("white","white"), name = "", na.value = "white") +
             # scale_fill_gradientn(colours = c("white", "lightgray","dimgray"), name = "") +
             # scale_fill_gradientn(colours = c("yellow", "orange", "tomato2", "red", "red4", "black"), name = "") +
             
             new_scale("fill") + theme(legend.position = "none")+
             geom_tile(aes(fill=value), 
                       data = subset(df, value >= 0)) +
             
             theme(plot.title = element_text(hjust = 0.5, vjust = -5)) +
             #geom_text(aes(label = "")) +
             # geom_text(aes(label=non_diagonal_upper_matrix_val), 
             #   data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
             #geom_text(aes(label=100*non_diagonal_upper_matrix_val/sum(heat_map_df$non_diagonal_upper_matrix_val)), 
             #          data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
             scale_fill_gradientn(colours = c("white", "white", "white"), 
                                  name = "", na.value = "white") + xlab("") + ylab("") + ggtitle(main) + 
             scale_x_discrete(position = "top"), envir = .GlobalEnv)
}

