#July 2014 weight

# for clothes, subtract 0.6 kg (1.3 lbs)
adj=1.3

# step 1, weights with clothes:
wt_with_clothes= c (146.2,147.5,146.0,145.8,146.8,148.2, 148.2, 147.0,146.5,148.0,148.8, 149.6,149.6)
wt_without_clothes = wt_with_clothes - adj
length(wt_without_clothes)

# step 2, weights naked (different days)
wt_naked=c(144.5, 144.6, 144.6, 144.2, 142.4, 144.8,145.8, 144.5,147.0)
length(wt_naked)

# concatinate
wt_July_2014 = c (wt_without_clothes,wt_naked)
length(wt_July_2014)
mean(wt_July_2014)
sd(wt_July_2014)

boxplot(wt_July_2014
        )


