library(httr)
library(readxl)

GET("https://inresg.sharepoint.com/:x:/r/sites/CNMA/_layouts/15/Doc2.aspx?action=edit&sourcedoc=%7Bd6e4946b-c2b2-49cf-92fe-033bf10962e5%7D&wdOrigin=TEAMS-MAGLEV.p2p_ns.rwc&wdExp=TEAMS-TREATMENT&wdhostclicktime=1750795696774&web=1, write_disk("tempfile.xlsx", overwrite = TRUE)

url <- "https://inresg.sharepoint.com/:x:/r/sites/CNMA/_layouts/15/Doc2.aspx?action=edit&sourcedoc=%7Bd6e4946b-c2b2-49cf-92fe-033bf10962e5%7D&wdOrigin=TEAMS-MAGLEV.p2p_ns.rwc&wdExp=TEAMS-TREATMENT&wdhostclicktime=1750795696774&web=1"
cNMA_data <- GET(url)
cNMA_data


GET("https://inresg.sharepoint.com/:x:/r/sites/CNMA/Shared/Documents/General/CNMA/Master/Database/4-22-25.xlsx", write_disk("cNMA_data.xlsx", overwrite = TRUE))

https://inresg.sharepoint.com/:x:/r/sites/CNMA/Shared%20Documents/General/CNMA%20Master%20Database%204-22-25.xlsx?d=wd6e4946bc2b249cf92fe033bf10962e5&csf=1&web=1&e=WmgT6a


https://inresg.sharepoint.com/:x:/r/sites/CNMA/Shared%20Documents/General/CNMA%20Master%20Database%204-22-25.xlsx?d=wd6e4946bc2b249cf92fe033bf10962e5&csf=1&web=1&e=WmgT6a