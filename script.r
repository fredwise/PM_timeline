source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("ggplot2");
libraryRequireInstall("lubridate");
libraryRequireInstall("data.table");
libraryRequireInstall("magrittr");
libraryRequireInstall("plotly")
####################################################

################### Actual code ####################
# Clean up Power BI dataset
dataset <- Values
prime_ministers <- dataset %>% as.data.table

setnames(
  prime_ministers,
  c('Name', 'Party', 'Start Date', 'End Date'),#old
  c('name', 'party', 'start_date', 'end_date') #new
)

prime_ministers[, start_date := ymd(substr(start_date, 0, 10))]
prime_ministers[, end_date := ymd(substr(end_date, 0, 10))]
prime_ministers[,name := iconv(name,to="ASCII//TRANSLIT")]
prime_ministers[,name := gsub(pattern = 'A ', replacement = ' ', x = name)]

setorder(prime_ministers, start_date)

prime_ministers$party %>% unique

# Colours for the political parties
parties <-
  c("Whig", "Tory", "Conservative", "Peelite", 'Liberal', 'Labour')
party_colours <-
  c("#FF7F00",
    "#98285C",
    "#0087DC",
    '#0660C4',
    '#FDBB30',
    "#DC241f")
prime_ministers[, party := factor(party, levels = parties, ordered = TRUE)]
prime_ministers[, label := paste(format(start_date, '%b %Y'), name)]

#Add a decade column
prime_ministers[, decade := floor_date(start_date, "10 years")]
prime_ministers[, number_in_decade := 1:.N, by = decade]

# Alternate label positions up and down each decade
positions <- c(0.5,-0.5, 1,-1, 1.5,-1.5, 2, -2)
directions <- c(1, -1)

decades <- unique(prime_ministers$decade)

# A small table to define the direction of our vertical lines on timeline
line_pos <- data.table(
  "decade" = decades,
  "position" = rep(positions, length.out = (length(decades))),
  "direction" = rep(directions, length.out = (length(decades)))
)

# Merge with prime minister dataset
prime_ministers <- prime_ministers[line_pos, on = 'decade']

# Create a plot position for prime minister name text, so they don't clash
text_offset <- 0.1
prime_ministers[, text_position :=
                  (number_in_decade *
                     text_offset * direction)
                + position]

# Decade data.table to label the axis on timeline
start_decade <- min(prime_ministers$decade) - years(10)
end_decade <- max(prime_ministers$decade) + years(10)
decade_range <- seq(start_decade, end_decade, by = '10 years')
decade_dt <-
  data.table(decade_range, decade_name = paste0(format(decade_range, '%Y'), 's'))


## Create plot ##
p <- ggplot(prime_ministers)

p <- p + labs(col = "Political Party") + theme_classic()

p <- p + scale_color_manual(values = party_colours,
                            labels = parties,
                            drop = FALSE)

p <- p + geom_hline(yintercept = 0,
                    color = "black",
                    size = 0.3)

# Vertical lines for each decade
p <- p + geom_segment(
  data = prime_ministers[number_in_decade == 1],
  aes(
    y = position,
    yend = 0,
    x = decade,
    xend = decade
  ),
  color = 'lightgray',
  size = 0.2
)

p <- p + geom_point(aes(x = decade,
                        y = 0,
                        color = party), size = 3)

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
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  legend.key.width = unit(5, 'cm')
)

# Show decade label
p <- p + geom_text(
  data = decade_dt,
  aes(
    x = decade_range - years(5),
    y = -0.3,
    label = decade_name,
  ),
  size = 2,
  color = 'gray47',
  angle = 90
)

# Show project text
p <- p +  geom_text(aes(
  x = decade,
  colour = party,
  label = label,
  y = text_position,
  text =
    paste(
      'Prime Minister:', name,'<br>',
      'Party:',party,'<br>',
      'Start Date:',start_date,'<br>',
      'End Date:',end_date,'<br>'
    )
),

size = 2.0);

####################################################

############# Create and save widget ###############
g = ggplotly(p, tooltip = 'text') %>%
  layout(legend = list(
    orientation = 'h',
    y = 0,
    x = 0.1,
    title = list(text = '<b> Party </b>')
  ));
internalSaveWidget(g, 'out.html');
####################################################
