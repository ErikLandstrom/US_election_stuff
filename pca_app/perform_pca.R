


perform_pca <- function(tb, cat_var1, cat_var2) {
  library(broom)
  
  cat_var1 = enquo(cat_var1)
  cat_var2 = enquo(cat_var2)
  
  temp <- tb %>% 
    nest() %>% 
    mutate(
      pca = purrr::map(data, ~ prcomp(.x %>% select(-County, -!!cat_var1:-!!cat_var2),
                                      scale = TRUE,
                                      center = TRUE)),
      pca_aug = purrr::map2(pca, data, ~ augment(.x, data = .y))
    )
  
  return(temp)
}


