# Caleb Kassa
# This script creates a function to extract the play by play data from 
# the html pages in 'pbp_pages', and combines them all to create one large 
# dataframe.

create.df <- function(x) {
  # Get this over with now:
  x <- gsub("&nbsp;", "", x)
  
  # Get game ID and team names:
  gameid <- grep("\\d{9}[A-Z]{3}", x, value = T)[1]
  gameid <- gsub(".*(\\d{9}[A-Z]{3}).*", "\\1", gameid)
  nameLine <- grep("<title>(.*) Play-By-Play", x)
  names <- gsub(".*<title>(.*) Play-By-Play.*", "\\1", x[nameLine])
  away.name <- gsub("(.*) at .*", "\\1", names)
  home.name <- gsub(".* at (.*)", "\\1", names)
  
  # Keep lines with the game quarter, time, and plays; remove everything else:
  quarters <- grep("1st Q|2nd Q|3rd Q|4th Q|1st OT", x)
  time.lines <- grep("<td>\\d+:\\d+.\\d", x)
  keep.lines <- c(quarters, time.lines, (time.lines + 1))
  
  # Get the lines with game times and plays:
  temp <- x[sort(keep.lines)]
  temp <- gsub("<[^<>]*>", "", temp)
  new.time.lines <- grep("\\d+:\\d+.\\d", temp)
  temp[new.time.lines] <- paste(temp[new.time.lines], 
                                temp[new.time.lines + 1], sep = ",")
  temp <- temp[-(new.time.lines + 1)]
  
  # Identify non-play lines to remove from data-frame. 
  # We may use them to determine quarters later.
  nonplay.lines <- grep("\\d-\\d", temp, invert = T)
  
  # Identify home/away lines:
  home.lines <- grep("\\d+.0,\\d+-", temp)
  away.lines <- grep("\\d+.0,\\d+-", temp, invert = T)
  
  # Identify score lines:
  score.lines <- grep("\\+", temp)
  temp[score.lines] <- gsub("(.*)(\\+\\d)(.*)", "\\1\\3,\\2", temp[score.lines])
  
  # Identify free throw lines and add a comma after the clause:
  ft.lines <- grep("free throw \\d of \\d", temp)
  temp[ft.lines] <- gsub("(free throw \\d of \\d(?=\\d))", "\\1,", temp[ft.lines], 
                         perl = T)
  
  # Format home/away lines:
  temp[home.lines] <- gsub("(\\d+:\\d+.\\d,)(\\d+-\\d+)(.*)", 
                           "\\1\\2,\\3", temp[home.lines])
  temp[away.lines] <- gsub("(\\d+:\\d+.\\d,)(.*)((?<!\\d)\\d+-\\d+)(.*)", 
                           "\\1\\3,\\2\\4", temp[away.lines], perl = T)
  
  # Remove duplicate commas, trailing commas, and then add two columns 
  # to the end of all non-scoring plays
  temp <- gsub(",,", ",", temp)
  temp <- gsub(",$", "", temp)
  temp[-score.lines] <- gsub("(.*)", "\\1, ", temp[-score.lines])
  
  # Add team names:
  temp[away.lines] <- gsub("(.*)", paste0("\\1,", away.name), temp[away.lines])
  temp[home.lines] <- gsub("(.*)", paste0("\\1,", home.name), temp[home.lines])
  
  # Determine quarters:
  q1 <- min(grep("1st Q", temp))
  q2 <- min(grep("2nd Q", temp))
  q3 <- min(grep("3rd Q", temp))
  q4 <- min(grep("4th Q", temp))
  temp[q1:(q2-1)] <- gsub("(.*)", "\\1,1", temp[q1:(q2-1)])
  temp[q2:(q3-1)] <- gsub("(.*)", "\\1,2", temp[q2:(q3-1)])
  temp[q3:(q4-1)] <- gsub("(.*)", "\\1,3", temp[q3:(q4-1)])
  
  # 4th Quarter/Overtime:
  ot <- grep("1st OT", temp)
  if (length(ot) == 0) {
    temp[q4:length(temp)] <- gsub("(.*)", "\\1,4", temp[q4:length(temp)])
  } else {
    temp[q4:(min(ot)-1)] <- gsub("(.*)", "\\1,4", temp[q4:(min(ot)-1)])
    temp[min(ot):length(temp)] <- gsub("(.*)", "\\1,OT", temp[min(ot):length(temp)])
  }
  
  
  # Build matrix. Need to remove non-play lines identified earlier:
  y <- matrix(trimws(unlist(strsplit(temp[-nonplay.lines], ","))),
              ncol=6, byrow=TRUE)
  
  # Remove substitutions:
  sub.lines <- grep("enters the game", y[,3])
  y <- y[-sub.lines,]
  
  # Remove referee timeouts:
  ref.lines <- grep("Official timeout", y[,3])
  y <- y[-ref.lines,]
  
  # Create separate columns for home/away score:
  away.score <- gsub("(\\d+)-\\d+", "\\1", y[,2])
  home.score <- gsub("\\d+-(\\d+)", "\\1", y[,2])
  
  # Format the points scored column:
  y[,4] <- as.numeric(gsub("\\+", "", y[,4]))
  y[is.na(y[,4]), 4] <- 0
  
  # Create dataframe:
  df <- data.frame(time = y[,1], play = y[,3], team = y[,5], 
                   away.score = away.score, home.score = home.score, 
                   gameid = rep(gameid, nrow(y)), 
                   away.team = rep(away.name, nrow(y)), 
                   home.team = rep(home.name, nrow(y)), 
                   points_scored = y[,4], period = y[,6], 
                   stringsAsFactors = F)
}

# Get a vector of the play-by-play html pages:
files <- dir("pbp_pages", full.names=T)

# Pull them all into a list and set their names to their game IDs:
all <- lapply(files, function (x) scan(x, what='n', sep='\n'))
names(all) <- gsub("pbp_pages/(.*).html", "\\1", files)

# x <- lapply(all, function(page) lapply(page, create.df))
# df <- do.call(rbind, do.call(rbind, x))

# Could not get the above code to work for some reason, had to resort 
# to a loop...

df <- data.frame()
for (i in 1:length(files)) {
  df <- rbind(df, create.df(all[[i]]))
}

write.csv(df, "pbp_df.csv", row.names = F)


