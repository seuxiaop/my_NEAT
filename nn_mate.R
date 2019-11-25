nn_mate <- function(nn1, nn2, fitness1 , fitness2, disable_p = 0.7){
  
  connect_df <- merge(nn1$connect_df,nn2$connect_df, all.x = T, all.y = T,by = "Marker", suffixes = c("_p1","_p2"))
  connect_df$random_pick <- ifelse(runif(nrow(connect_df))<=0.5,1,2)
  connect_df$fitness_pick <- ifelse((fitness1 == fitness2), 0, ifelse(fitness1 > fitness2,1,2))
  connect_df$final_pick <- connect_df$random_pick
  index <- (is.na(connect_df$In_p1) | is.na(connect_df$In_p2)) & (connect_df$fitness_pick != 0)
  connect_df$final_pick[index ] <- connect_df$fitness_pick[index]
  
  new_connect_df <- rbind(nn1$connect_df[nn1$connect_df$Marker %in% connect_df$Marker[connect_df$final_pick == 1],   ],
                          nn2$connect_df[nn2$connect_df$Marker %in% connect_df$Marker[connect_df$final_pick == 2],   ])
  
  new_connect_df <- new_connect_df[order(new_connect_df$Marker),]
  
  index <- ifelse(is.na(connect_df$Disabled_p1), 'N',connect_df$Disabled_p1) == 'Y' |
    ifelse(is.na(connect_df$Disabled_p2), 'N',connect_df$Disabled_p2) == 'Y' 
  
  disabled_node_list <-  connect_df$Marker[index]
  disabled_node_list <- disabled_node_list[runif(length(disabled_node_list)) <= disable_p]

  if(length(disabled_node_list) >0){
    new_connect_df$Disabled[new_connect_df$Marker %in% disabled_node_list] <- 'Y'
  }
  new_connect_df$Disabled[!new_connect_df$Marker %in% disabled_node_list] <- 'N'
  
  new_node_df <-  unique(rbind(nn1$node_df[,c(1,2)],nn2$node_df[,c(1,2)]))
  new_node_df <- new_node_df[new_node_df$node_id %in% new_connect_df$Out |
                               new_node_df$node_id %in% new_connect_df$In,]
  new_node_df$level  <- 1
  max_level <- 1
  node_in <- new_node_df$node_id[new_node_df$node_type == "sensor"]
  
  while(length(node_in) > 0){
    node_out <- unique(new_connect_df$Out[new_connect_df$In %in% node_in])
    new_node_df$level[new_node_df$node_id %in% node_out] <- max_level + 1
    node_in <- node_out
    max_level <- max_level + 1
  }
  new_node_df$level[new_node_df$node_type == "output"] <- max(new_node_df$level) + 1
  new_node_df$level <- rank(new_node_df$level, ties.method = "min")
  new_node_df$level <-as.numeric(as.factor(new_node_df$level ))
  nn <- list(node_df = new_node_df, connect_df = new_connect_df)
  # nn_plot(nn)
  # nn$node_df
  
  return(list(nn))
  
}

