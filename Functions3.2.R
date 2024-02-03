
#remind_me function
#this function returns the name and birthdays of my friends
remind_me <- function() {
  data.frame(
    Names = c("Bjarne", "Victor", "Leo", "Jette", "Greta"),
    Birthday = c(
      "1999-10-27",
      "2002-05-03",
      "2000-03-12",
      "2012-02-07",
      "2004-04-21"
    )
  )
}


#cheat function
#this function provides the solution for exercises from 3.1

cheat <- function(exercise = "Q3.1.6") {
  switch(exercise,
         "Q3.1.6" = {

           'library(dplyr)

            chicken_weight <- ChickWeight

            chick_numbers <- c("1", "20", "5", "40", "19")

            subset_chicken <- chicken_weight %>%
              filter(Chick %in% chick_numbers) %>%
              group_by(Chick) %>%
              summarise(max_weight = max(weight))

            print(subset_chicken)

            ggplot(data = subset_chicken, aes(x = factor(Chick, levels = c("1", "20", "5", "40", "19")), y = max_weight)) +
              geom_col() +
              labs(x = "Chick")'
         },

         "Q3.1.11" = {

           'library(gganimate)

            library(cranlogs)

            library(gifski)

            plotly_downloads <- cran_downloads("plotly", from = "2013-01-01", to = "2024-01-01")
            ggplot2_downloads <- cran_downloads("ggplot2", from = "2013-01-01", to = "2024-01-01")

            downloads <- rbind(plotly_downloads, ggplot2_downloads)

            animation <- ggplot(data = downloads, aes(x = date, y = count, colour = package)) +
              geom_line() +
              labs(title = "Package popularity over time", x = "date", y = "Package Downloads") +
              theme_bw() + #adjusted theme to resemble model graph
              transition_reveal(date)
            animate(animation, nframes = 100, duration = 5, end_pause = 10, renderer = gifski_renderer())'
         },

         "Q3.1.13" = {

           'plotstock <- function(stock = "TSLA", year = "2023", file_name = "stock.png") {
            png(file = file_name)
            getSymbols(stock)
            chartSeries(get(stock), subset = year, name = stock)
            dev.off()
          }

          plotstock()

          plotstock("AAPL", "2022", "applestock.png")'
         },

         "Try a different exercise number. This one is not in the data base")
}

#make_art function

make_art <- function(seed = "13216961") { #sets defeault seed
  set.seed(seed) #sets seed to seed that was input
  random_number <- runif(1, min = 10000, max = 100000) #creates a random number between 10000 and 1000000
  art_df <- data.frame(column1 = rnorm(random_number), column2 = rnorm(random_number), column3 = rnorm(random_number)) #creates a data frame with 3 columns. Each are filled with random numbers from a normal distribution and n= random_number which determines how many random numbers are drawn.

  library(tidyverse)
  library(plotly)

  colour_random <- rgb(runif(random_number), runif(random_number), runif(random_number)) #creates a random red green blue colour

  plot_ly(data = art_df, x = ~column1, y = ~column2, z = ~column3, type = "scatter3d", mode = "markers", marker = list(opacity = 0.7, color = colour_random)) %>% #creates 3d scatter. Colour of each marker is a random rgb colour
    layout(scene = list( #removes the grid and title label
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE, title = ""),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE, title = ""),
      zaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE, title = "")
    ))
}


make_art() #zoom in and out for interactive effect
