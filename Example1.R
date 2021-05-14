stopifnot( require("tidyverse") )
stopifnot( require("ggplot2") )
stopifnot( require("cowplot") )

## Step 1: create some data...
cluster.n <- c(100,100,100)
cluster.mean <- list(x1=c(18,25,15),x2=c(12,14,11),x3=c(5,10,5))

make_cluster <- function(n,center_vector){
  dim = length(center_vector);
  names=paste0("x",1:dim)
  matrix( rnorm(n*dim,mean=c(center_vector) ),byrow = T,ncol=dim) %>% as_tibble() %>% set_names(names)
}

zz<-map2(cluster.mean,cluster.n, ~make_cluster(.y,.x)) %>% 
  map2_df(1:length(cluster.n),~mutate(.x,cluster=paste0("cluster_",.y))) %>%
  ## shuffle the data...
  sample_n(.,nrow(.))


## Step 2: plot the x1,x2,x3 data...
p <- plot_grid( 
  zz %>% ggplot( aes(x=x1,y=x2,color=cluster)) +geom_point(),
       zz %>% ggplot( aes(x=x1,y=x3,color=cluster)) +geom_point(),
       zz %>% ggplot( aes(x=x2,y=x3,color=cluster)) +geom_point()
)

