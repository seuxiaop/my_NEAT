nn_init <- function(n_input = 2){
  
  node_df <- data.frame(
    node_id = 1:(n_input+1),
    node_type = c(rep("sensor",n_input),"output"),
    level = c(rep(1,n_input),2)
  )
  connect_df <- data.frame(In = (1:n_input), 
                           Out = rep(n_input + 1, n_input),
                           Weight = runif(n_input, min= -1, max = 1),
                           Marker = (1:n_input),
                           Disabled = rep("N",n_input)
  )
  connect_df$Disabled <- as.character(connect_df$Disabled)
  
  return(list(node_df = node_df,connect_df = connect_df))
  
}