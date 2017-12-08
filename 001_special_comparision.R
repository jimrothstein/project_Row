#001_special_comparision


##################
# Row - Compare months of Oct - 2012-2017
##################



require(tidyverse)

# build tibble (manually)

x <- "Oct"
t <- tibble(yr = c(2012,2013,2014,2015,2016,2017) + 10/12,
            month = c(x,x,x,x,x,x),
            meters = c(190.5,127.6,155.1,129.8,54.0,53.1),
            time = c(20+41/60,
                     13+44/60,
                     17+10/60,
                     15+50/60,
                     6+53/60,
                     6+52/60),
            A1C = c(4.6,5.1,5.0,5.7,5.6,5.7),
            meters/31,
            time/31,
            comment = c("","","","A1C=Jul no row","","")
)

t
glimpse(t)  # transpose, by column!  so can see 
options(tibble.print.max = 100,
        tibble.print.min = 10,
        tibble.width = Inf)
t
g <- ggplot(t,aes(x=yr,y=`meters/31`))
g + geom_point(size=3) + 
        geom_vline(xintercept=2014+11/12, color='red', linetype=2) +
        labs(title = "Avg Meters Per Day",
             subtitle = "OCT 2012-2017",
             caption="source: my records",
             x="year") +
        annotate ("text", label="LAS",x=2012.833,y=3) +
        geom_point(aes(x=yr, y=A1C), size=1, color="red")

#g <-ggplot(t,aes(x=yr,y=`time/31`))
#g + geom_point()

