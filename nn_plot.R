my_curve <- function(x,y){
radian <- sqrt((x[1] - x[2])^2 + (y[1] - y[2])^2) * 0.5/sqrt(1-0.9^2)
A_sum <- pi - asin((x[2] - x[1])/2/radian/sqrt(1-0.9^2))
A_diff <- acos(0.9)
A1 <- A_sum + A_diff
A2 <- A_sum - A_diff
center_pos <- c(x[1] - cos(A1)*radian, y[1]-sin(A1)*radian)
y_diff <- (y[2] - y[1])/2
x_diff <- sqrt(radian^2 - y_diff^2)
angle_step <- (A2 - A1)/20
angle_seq <- seq(A1,A2, by = angle_step)
points_x <- cos(angle_seq) * radian + center_pos[1]
points_y <- sin(angle_seq) * radian + center_pos[2]
lines(points_x, points_y)
}


nn_plot <- function(nn){

  #node_list <- get_node_info(connect_df)
  node_df <- nn$node_df
  connect_df <- nn$connect_df
  level_df <- as.data.frame(table(node_df$level))
  colnames(level_df) <- c("level_id", "cnt")
  level_df$level_id <- as.numeric(level_df$level_id)
  
  x_lim <- prod(level_df$cnt + 1)
  y_lim <- x_lim
  y_step_size <- y_lim/(nrow(level_df) + 1)
  
  node_df$x <- NA
  node_df$y <- NA
  
  for(i in level_df$level_id){
    cnt_i <- level_df$cnt[i]
    x_pos <- (1:cnt_i)*(x_lim/(cnt_i + 1))
    y_pos <- i * y_step_size
    node_df$x[node_df$level == i] <- x_pos
    node_df$y[node_df$level == i] <- y_pos
  }
  
  plot(node_df$x,node_df$y,cex = 4, axes=FALSE, frame.plot = F,
       ann=FALSE, xlim = c(1,max(node_df$x+1)))
  text(y ~x, labels=node_id,data=node_df, cex=0.9, font=2)
  
  
  
  for(i in 1:nrow(connect_df)){
    
    if(connect_df$Disabled[i] == 'N'){
      x <- c(node_df$x[node_df$node_id== connect_df$In[i]] , node_df$x[node_df$node_id==connect_df$Out[i]] )
      y <- c(node_df$y[node_df$node_id==connect_df$In[i]] , node_df$y[node_df$node_id==connect_df$Out[i]] )
      if(x[1]==x[2] & node_df$level[node_df$node_id==connect_df$In[i]] <  node_df$level[node_df$node_id==connect_df$Out[i]] - 1 ){
        my_curve(x,y)  
      }else{
        lines(x,y)
      }
      
    }
    
  }
  
  
  
}