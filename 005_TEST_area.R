###  TEST area
library(tidyverse)

## simple
t <- tibble(x = c(1,2,3),
            y = c(0.5,0,-1),
            z = c(NA,1,2))
t %>% mutate(z2 = z*2) %>% 
        select(x,z=z2
           ) %>% 
filter(!is.na(z))

## 
t <- tibble(x = c(1,2,3),
            y = c(0.5,0,-1),
            z = c(NA,1,2)) %>%
        
        mutate(z2 = z*2) %>% 
        
        select(x,z=z2
           ) %>% 
        
        filter(!is.na(z))
## embelish with +    ERRORS:!

t <- tibble(x = c(1,2,3),
            y = c(0.5,0,-1),
                 z = c(NA,1,2)) %>%
+        mutate(z2 = z*2) %>% 
        
        select(x,z=z2
           ) %>% 
        
        filter(!is.na(z))
