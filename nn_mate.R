nn_mate <- function(nn1, nn2, fitness1 , fitness2, disable_p = 0.7){
  
  connect_df <- merge(nn1$connect_df,nn2$connect_df, all.x = T, all.y = T,by = "Marker", suffixes = c("_p1","_p2"))
  connect_df$random_pick <- ifelse(runif(nrow(connect_df))<=0.5,1,2)
  connect_df$fitness_pick <- ifelse((fitness1 == fitness2), 0, ifelse(fitness1 > fitness2,1,2))
  connect_df$final_pick <- connect_df$random_pick
  index <- (is.na(connect_df$In_p1) | is.na(connect_df$In_p2)) & connect_df$fitness_pick != 0
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
  
  new_node_df <- data.frame(node_id = unique(c(new_connect_df$In, new_connect_df$Out)), node_type = "hidden"  )
  new_node_df$node_type <- as.character(new_node_df$node_type)
  new_node_df$node_type[new_node_df$node_id %in% nn1$node_df$node_id[nn1$node_df$node_type == "sensor"]] <- "sensor"
  new_node_df$node_type[new_node_df$node_id %in% nn1$node_df$node_id[nn1$node_df$node_type == "output"]] <- "output"
  new_node_df <- new_node_df[order(new_node_df$node_id),]
  
  nn <- list(node_df = new_node_df, connect_df = new_connect_df)
  return(list(nn))
  
}

