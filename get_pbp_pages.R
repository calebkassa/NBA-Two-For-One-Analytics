# Caleb Kassa
# This script fetches the play-by-play html pages for every game played from the 
# 2014-15 season through 2018-19 from https://www.basketball-reference.com/, 
# and saves them as local files in the folder 'pbp_pages'.

seasons <- c(2015:2019)
months <- c('october', 'november', 'december', 'january',
            'february', 'march', 'april', 'may', 'june')

gameids <- c()
for (season in seasons) {
  for (month in months) {
    url <- paste0('https://www.basketball-reference.com/leagues/NBA_', season, 
                  '_games-', month, '.html')
    page <- scan(url, what = '', sep = '\n')
    page <- page[grep("csk=", page)]
    gameids <- append(gameids, gsub(".*(\\d{9}[A-Z]{3}).*", "\\1", page), 
                      length(gameids))
  }
}

for (gameid in gameids) {
  Sys.sleep(runif(1, 2, 4)) # Pause for 2-4 seconds in between each query
  url <- paste0('https://www.basketball-reference.com/boxscores/pbp/', gameid, 
                '.html')
  file.name <- paste0("pbp_pages/", gameid, ".html")
  html.page <- try(scan(url, what = "", sep = "\n"))
  if (html.page == "try-error"){
    cat(paste("Error fetching:", url, "\n"))
  }
  else
    writeLines(html.page, file.name)
}
