weight_mutation <- function(nn, scale = 0.1, p = 0.8){
  connect_df <- nn$connect_df
  n <- nrow(connect_df)
  connect_df$perturb_flag <- runif(n) <= p 
  connect_df$perturb_value <- runif(n, min= -scale, max=scale) 
  connect_df$Weight[connect_df$perturb_flag] <- connect_df$Weight[connect_df$perturb_flag] + connect_df$perturb_value[connect_df$perturb_flag]
  n1 <- nrow(connect_df[!connect_df$perturb_flag, ])
  if(n1 > 0){
    connect_df$Weight[!connect_df$perturb_flag] <- runif(n1, min = -1, max= 1)
  }
  connect_df$perturb_flag <- NULL
  connect_df$perturb_value <- NULL
  nn$connect_df <- connect_df
  
  return(nn)
}