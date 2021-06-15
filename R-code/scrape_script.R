library(rvest)
library(data.table)
library(lubridate)

temp = data.frame()
pdga_players = data.frame()
for(i in 1:113210) {
  url = paste0("https://www.pdga.com/player/",i,"/history")
  
  pdga = try(read_html(url),silent = T)
  if(class(pdga)=="try-error") next 
  #name
  name = pdga %>% html_nodes("h1") %>% html_text()
  
  
  # rating history
  date = pdga %>% html_nodes(".date") %>% html_text()
  rating = pdga %>% html_nodes(".player-rating")%>% html_text()
  round = pdga %>% html_nodes(".round")%>% html_text()
  str(rating)
  if  (length(date) != 0) {
    temp = data.frame(pdga = i, player = name[1],date = date, rating = rating[-1], rounds = round[-1])
    pdga_players = bind_rows(pdga_players,temp)
  }
  print(i)
  if (i > i2) {
    i2 = i + 150
    print("sleep")
    Sys.sleep(60)
    
  }
}

fwrite(pdga_players, "H:/pdga.csv")



