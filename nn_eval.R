nn_eval <- function(nn, fun_act, input = c(1,1) ){  # input length equals to the number of sensor nodes
  
  node_df <- nn$node_df
  connect_df <- nn$connect_df
  node_df$value <- 0
  node_df$value[node_df$node_type == "sensor"] <- input
  # node_info <- get_node_info(connect_df)
  # node_df <- merge(node_df, node_info[,c("node_id","level")])
  max_level <- max(node_df$level)
  node_df$id <- 1:nrow(node_df)
  
  connect_df <- merge(connect_df, node_df[,c("node_id","id")], by.x="Out", by.y="node_id")
  
  i <- 2
  while(i <= max_level){
    current_nodes <- node_df$node_id[node_df$level == i]
    current_connect <- connect_df[connect_df$Out %in% current_nodes & 
                                    connect_df$Disabled == "N", ]
    current_connect <- merge(current_connect, node_df[,1:4], by.x = "In", by.y = "node_id")
    
    new_value <- current_connect %>% dplyr::group_by(Out,id) %>% dplyr::summarise(value =sum(value*Weight)) %>% as.data.frame()
    new_value$value <- fun_act(new_value$value)
    node_df[new_value$id,"value"] <- new_value$value 
    i <- i + 1
  }
  
  return(node_df$value[node_df$node_type == "output"])
  
}
