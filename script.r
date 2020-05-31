source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("ggplot2");
libraryRequireInstall("lubridate");
libraryRequireInstall("data.table");
libraryRequireInstall("magrittr");
libraryRequireInstall("plotly")
libraryRequireInstall("stringRR")
####################################################

################### Actual code ####################
# Clean up Power BI dataset
dataset <- Values
prime_ministers <- dataset %>% as.data.table

setnames(
  prime_ministers,
  c('Name', 'Party', 'Start Date', 'End Date'),
  #old
  c('name', 'party', 'start_date', 'end_date') #new
)

#Dates
prime_ministers[, start_date := ymd(substr(start_date, 0, 10))]
prime_ministers[, end_date := ymd(substr(end_date, 0, 10))]
setorder(prime_ministers,start_date)

# Colours for the political parties
parties <-
  c("Whig", "Tory", "Conservative", "Peelite", 'Liberal', 'Labour')
party_colours <-
  c("#FF7F00",    "#98285C","#0087DC",'#0660C4','#FDBB30',"#DC241f")

prime_ministers[, party := factor(party, levels = parties, ordered = TRUE)]

#Add a decade column
prime_ministers[, decade := floor_date(start_date, "10 years")]
prime_ministers[, number_in_decade := 1:.N, by = decade]

# Alternate label positions up and down each decade
positions <- c(0.5, -0.5, 1.7, -1.7, 2.9, -2.9)
directions <- c(1,-1)

decades <- unique(prime_ministers$decade)

# A small table to define the direction of vertical lines / text on timeline
line_pos <- data.table(
  "decade" = decades,
  "position" = rep(positions, length.out = (length(decades))),
  "direction" = rep(directions, length.out = (length(decades)))
)

# Merge with prime minister dataset
prime_ministers <- prime_ministers[line_pos, on = 'decade']

# Create a plot position for prime minister name text, so they don't clash
text_offset <- 0.25
prime_ministers[, text_position :=
                  (number_in_decade * text_offset * direction)
                + position]

# Decade data.table to label the axis on timeline
decade_dt <- unique(prime_ministers[,.(decade,direction)])
decade_dt[,decade_label := year(decade)]

## Create plot
p <- ggplot(prime_ministers)

p <- p +  theme_classic()

p <- p + scale_color_manual(values = party_colours,
                            labels = parties,
                            drop = FALSE)

p <- p + geom_hline(yintercept = 0,
                    color = "black",
                    size = 0.3,show.legend = F)

#Vertical lines for each decade
p <- p + geom_segment(
  data = prime_ministers[number_in_decade == 1],
  aes(y = position, yend = 0,x=decade, xend = decade),
  color = 'lightgray',
  size = 0.2,show.legend = F
)

#Little marker on each decade, jitter so that political parties don't overlap
p <- p + geom_point(aes(x=start_date, y=0,colour = party),
                    shape = 3,show.legend = F
    )

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
  legend.position = "top",
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10)
) + guides(col = guide_legend(nrow=1))

#Show decade label
p <- p + geom_text(
  data = decade_dt,
  aes(
    x = decade,
    y = direction*-0.2,
    label = decade_label,
  ),
  size = 3,
  color = 'gray47',show.legend = F
)

#Tooltip for plotly
prime_ministers[,text:=paste0(
  name, ', ',  party, '<br>'
  ,format(start_date,'%d %b %Y'),' to ',format(end_date,'%d %b %Y')
)]


# Show project text
p <- p +  geom_text(aes(label = name,
                        text=text,x=decade
                         ,y = text_position,colour=party),
                     size = 3, family = 'sans',
                    show.legend = F, fontface='bold')



####################################################

############# Create and save widget ###############
g = ggplotly(p, tooltip = 'text') %>%
  layout(legend = list(
    orientation = 'h',
    y = 1.1,
    x = 0.1,
    title = list(text = 'Party')
  ));

internalSaveWidget(g, 'out.html');
####################################################
