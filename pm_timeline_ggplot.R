## A horizontal timeline telling the story of the UK's prime ministers
library(plotly)
library(jsonlite)
library(ggplot2)
library(lubridate)
library(data.table)
library(magrittr)
prime_ministers <-
  fromJSON(
    'https://raw.githubusercontent.com/TimelineConsortium/Timeline-Data/master/uk-prime-ministers.json'
  ) %>% as.data.table %>% .[,facet:=NULL]

prime_ministers[,start_date:= as.Date(start_date)]
prime_ministers[, end_date := ymd(substr(end_date, 0, 10))]

setnames(
  prime_ministers,
  c('content_text', 'category'), #old
  c('name', 'party') #new
)
prime_ministers$party %>% unique

# Colours for the political parties
parties <- c("Whig","Tory","Conservative","Peelite",'Liberal','Labour')
party_colours <- c( "#FF7F00", "#98285C","#0087DC",'#0660C4','#FDBB30', "#DC241f")
prime_ministers[,party:= factor(party, levels=parties, ordered=TRUE)]

#Add a decade column
prime_ministers[,decade:=floor_date(start_date,"10 years")]
prime_ministers[,number_in_decade := 1:.N,by=decade]

# Alternate label positions up and down each decade
positions <- c(0.5,-0.5,1,-1,1.5,-1.5)
directions <- c(1, -1)

decades<-unique(prime_ministers$decade)

# A small table to define the direction of our vertical lines on timeline
line_pos <- data.table(
  "decade"=decades,
  "position"=rep(positions, length.out=(length(decades))),
  "direction"=rep(directions, length.out=(length(decades)))
)

# Merge with prime minister dataset
prime_ministers <- prime_ministers[line_pos, on='decade']

# Create a plot position for prime minister name text, so they don't clash
text_offset <- 0.1
prime_ministers[,text_position := 
           (number_in_decade * 
              text_offset * direction) 
         + position
]

# Decade data.table to label the axis on timeline
start_decade <- min(prime_ministers$decade) - years(10)
end_decade <- max(prime_ministers$decade) + years(10)
decade_range <- seq(start_decade, end_decade, by='10 years')
decade_dt <- data.table(decade_range, decade_name = paste0(format(decade_range,'%Y'),'s'))

## Create plot ##
p <-
  prime_ministers %>% 
  ggplot(
    aes(x = decade,y = 0,color = party,label = paste(format(start_date,'%b %Y'),name))
    ) 
  
  p <- p + labs(col = "Political Party") + theme_classic() 
    
  
  p <- p + scale_color_manual(values = party_colours,
                     labels = parties,
                     drop = FALSE)
  p <- p+guides(color = guide_legend(nrow = 1))
  
  p <- p + geom_hline(yintercept = 0,
             color = "black",
             size = 0.3) 
  
  # Vertical lines for each decade
  p <- p + geom_segment(
    data = prime_ministers[number_in_decade == 1],
    aes(y = position, yend = 0, xend = decade),
    color = 'lightgray',
    size = 0.2
  ) 
  
  p <- p + geom_point(aes(y = 0), size = 3)
  
# Don't show axes, appropriately position legend
  p <- p + theme(
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      legend.position = "bottom",
      legend.text=element_text(size=10),
      legend.title=element_text(size=10)
      ) 

# Show decade label
  p <- p + geom_text(
    data = decade_dt,
    aes(
      x = decade_range-years(5),
      y = -0.2,
      label = decade_name,
    ),
    size = 3,
    color = 'gray47',
    angle = 90
  )

# Show project text
  p <- p +  geom_text(aes(y = text_position
                          
                          ), size =2.5
                      
                      )

p
