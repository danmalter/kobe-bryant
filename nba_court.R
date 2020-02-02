geom_shotplot <- function(...){
  list(annotation_custom(grid::rasterGrob(png::readPNG("~/GitHub/kobe-bryant-kaggle/nba_court.png"), 
                                          width = unit(1,"npc"), 
                                          height = unit(1,"npc"))),
       geom_point()
       ,coord_flip()
  )
}
