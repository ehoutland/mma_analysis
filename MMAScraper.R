#program containing functions to scrape mma data from ufcstats.com

library(rvest)
library(tidyverse)
library(stringr)
library(pbapply)

#checks if link will give a timed-out error
#from https://krzysztofprzygodzki.wordpress.com/2015/09/09/a-solution-for-rvest-timed-out-error/
read_html_NE = function(x) {
  
  page_src = try(read_html(x), silent = T)
  
  # test if Page.src is erroneous
  if (class(page_src)[1] == "try-error") {
    
    error_cond = attr(page_src, "condition")
    
    # check if error condition contains “Timed out” phrase.  If regexpr cannot find a
    # match it returns -1
    timed_out = str_detect(error_cond, regex("time.*out", ignore_case = T))
    
    # we want to continue only on “timed out” error
    if (timed_out == TRUE) {
      
      # print information in the console
      print(paste(x, ": Timed out. Trying to reconnect in 30s. Please wait..."))
      Sys.sleep(30)
      
      return(read_html_NE(x))
    }
  }
  
  return(page_src)
}

#function to determine whether link is fight, fighter, or event
link_type <- function(link){
  return(
    case_when(
     str_detect(link, "fighter-details") ~ "fighter",
     str_detect(link, "fight-details") ~ "fight",   
     str_detect(link, "event-details") ~ "event" 
    )
   )
}

#function to extract fighter data into table
extract_fighter <- function(link){
  
  fighter_page <- read_html_NE(link) #fighter webpage
  
  #extracting fighter's web id
  fighter_id <- str_remove(link, "http://ufcstats.com/fighter-details/") %>%
    str_remove("[^[:alnum:] ]")
  
  #extracting fighter's name
  fighter_name <- fighter_page %>%
    html_nodes(".b-content__Nickname , .b-content__title-highlight") %>%
    html_text(trim = T) %>%
    setNames(., c("Name", "Nickname"))

  #extracting fighter's vital info
  fighter_vitals <- fighter_page %>%
    html_nodes(".js-guide") %>%
    html_text(trim = T) %>%
    .[1] %>%
    str_split("\\n\\s{2,}\\n\\s{2,}") %>%
    flatten_chr %>%
    matrix(ncol = 2, byrow = T) %>%
    `rownames<-`(str_sub(.[, 1], end = -2)) %>%
    .[, -1]

  #extracting statisttics by fight
  fighter_table <- fighter_page %>%
    html_nodes(".js-fight-table") %>%
    html_table(fill = T) %>%
    flatten_df %>%
    .[-1, ] %>%
    separate(Fighter, into = c("Fighter", "Opponent"), sep = "\\s{2,}") %>%
    separate(Str, into = c("Str", "Opponent Str"), sep = "\\s{2,}") %>%
    separate(Td, into = c("Td", "Opponent Td"), sep = "\\s{2,}") %>%
    separate(Sub, into = c("Sub", "Opponent Sub"), sep = "\\s{2,}") %>%  
    separate(Pass, into = c("Pass", "Opponent Pass"), sep = "\\s{2,}")  %>%
    separate(Event, into = c("Event", "Date"), sep = "\\s{2,}") %>%
    separate(Method, into = c("Method", "Details"), sep = "\\s{2,}")

  #associating events with fighter
  fighter_event_links <- fighter_page %>%
    html_nodes(".b-link_style_black") %>%
    html_attr("href")

  #extracting links to fights on fighter page
  fight_links <- fighter_page %>%
    html_nodes(".b-fight-details__table-row") %>%
    html_attr("data-link")

  #compiling linked events and fights
  linked_pages <- c(fighter_event_links, fight_links) %>%
    unique %>%
    na.omit
  
  #final fighter table
  fighter <- list("ID" = fighter_id, "Name" = fighter_name, 
                  "Vitals" = fighter_vitals, "Match History" = fighter_table, 
                  "Linked Pages" = linked_pages, "Link" = link
                  )
  
  closeAllConnections()
  return(fighter)
}

#function to extract fight data into table
extract_fight <- function(link){
  
  fight_page <- read_html_NE(link) #webpage containing fight
  
  #fight's web id
  fight_id <- str_remove(link, "http://ufcstats.com/fight-details/") %>%
    str_remove("[^[:alnum:] ]")
  
  #pages linked to on fight page
  linked_pages <- fight_page %>%
    html_nodes(".b-link, a") %>%
    html_attr("href") %>%
    str_subset("ufcstats.com") %>%
    unique  
  
  #determines if fight doesn't have any info
  table_na <- fight_page %>%
    html_node("table") %>%
    is.na
    
  if(table_na){
  #create dummy table and end execution if no info
    fight <- list("ID" = fight_id, "Totals" = NULL, "Sig Strikes" = NULL, 
      "Linked Pages" = linked_pages, "Link" = link)
    
    closeAllConnections()
    return(fight)
    } else{
      fight_table_totals <- fight_page %>%
        html_nodes(".b-fight-details__table") %>%
        .[[1]] %>%
        html_table %>%
        set_names(make.unique(names(.))) %>%
        separate(Fighter, into = c("Fighter 1 Name", "Fighter 2 Name"), sep = "\\s{2,}") %>%
        separate(KD, into = c("Fighter 1 KD", "Fighter 2 KD"), sep = "\\s{2,}") %>%
        separate(`Sig. str.`, into = c("Fighter 1 Sig Str", "Fighter 2 Sig Str"), sep = "\\s{2,}") %>%
        separate(`Fighter 1 Sig Str`, into = c("Fighter 1 Sig Str Succ", "Fighter 1 Sig Str Att"), sep = " of ") %>%
        separate(`Fighter 2 Sig Str`, into = c("Fighter 2 Sig Str Succ", "Fighter 2 Sig Str Att"), sep = " of ") %>%
        separate(`Total str.`, into = c("Fighter 1 Str", "Fighter 2 Str"), sep = "\\s{2,}") %>%
        separate(`Fighter 1 Str`, into = c("Fighter 1 Str Succ", "Fighter 1 Str Att"), sep = " of ") %>%
        separate(`Fighter 2 Str`, into = c("Fighter 2 Str Succ", "Fighter 2 Str Att"), sep = " of ") %>%
        separate(`Td %`, into = c("Fighter 1 TD", "Fighter 2 TD"), sep = "\\s{2,}") %>%
        separate(`Fighter 1 TD`, into = c("Fighter 1 TD Succ", "Fighter 1 TD Att"), sep = " of ") %>%
        separate(`Fighter 2 TD`, into = c("Fighter 2 TD Succ", "Fighter 2 TD Att"), sep = " of ") %>% 
        separate(`Sub. att`, into = c("Fighter 1 Sub Att", "Fighter 2 Sub Att"), sep = "\\s{2,}") %>%
        separate(`Rev.`, into = c("Fighter 1 Rev", "Fighter 2 Rev"), sep = "\\s{2,}") %>% 
        separate(`Pass`, into = c("Fighter 1 Pass", "Fighter 2 Pass"), sep = "\\s{2,}") %>%
        select(-c(`Sig. str. %`, `Td %.1`))
  
      #extracting fight details
      fight_table_sig_str <- fight_page %>%
        html_nodes(".b-fight-details__table") %>%
        .[[2]] %>%
        html_table %>%
        separate(Fighter, into = c("Fighter 1 Name", "Fighter 2 Name"), sep = "\\s{2,}") %>%
        separate(`Sig. str`, into = c("Fighter 1 Sig Str", "Fighter 2 Sig Str"), sep = "\\s{2,}") %>%
        separate(`Fighter 1 Sig Str`, into = c("Fighter 1 Sig Str Succ", "Fighter 1 Sig Str Att"), sep = " of ") %>%
        separate(`Fighter 2 Sig Str`, into = c("Fighter 2 Sig Str Succ", "Fighter 2 Sig Str Att"), sep = " of ") %>% 
        separate(`Head`, into = c("Fighter 1 Head", "Fighter 2 Head"), sep = "\\s{2,}") %>%  
        separate(`Fighter 1 Head`, into = c("Fighter 1 Head Succ", "Fighter 1 Head Att"), sep = " of ") %>%
        separate(`Fighter 2 Head`, into = c("Fighter 2 Head Succ", "Fighter 2 Head Att"), sep = " of ") %>%  
        separate(`Body`, into = c("Fighter 1 Body", "Fighter 2 Body"), sep = "\\s{2,}") %>%  
        separate(`Fighter 1 Body`, into = c("Fighter 1 Body Succ", "Fighter 1 Body Att"), sep = " of ") %>%
        separate(`Fighter 2 Body`, into = c("Fighter 2 Body Succ", "Fighter 2 Body Att"), sep = " of ") %>%   
        separate(`Leg`, into = c("Fighter 1 Leg", "Fighter 2 Leg"), sep = "\\s{2,}") %>%  
        separate(`Fighter 1 Leg`, into = c("Fighter 1 Leg Succ", "Fighter 1 Leg Att"), sep = " of ") %>%
        separate(`Fighter 2 Leg`, into = c("Fighter 2 Leg Succ", "Fighter 2 Leg Att"), sep = " of ") %>%        
        separate(`Distance`, into = c("Fighter 1 Distance", "Fighter 2 Distance"), sep = "\\s{2,}") %>%  
        separate(`Fighter 1 Distance`, into = c("Fighter 1 Distance Succ", "Fighter 1 Distance Att"), sep = " of ") %>%
        separate(`Fighter 2 Distance`, into = c("Fighter 2 Distance Succ", "Fighter 2 Distance Att"), sep = " of ") %>%   
        separate(`Clinch`, into = c("Fighter 1 Clinch", "Fighter 2 Clinch"), sep = "\\s{2,}") %>%  
        separate(`Fighter 1 Clinch`, into = c("Fighter 1 Clinch Succ", "Fighter 1 Clinch Att"), sep = " of ") %>%
        separate(`Fighter 2 Clinch`, into = c("Fighter 2 Clinch Succ", "Fighter 2 Clinch Att"), sep = " of ") %>%   
        separate(`Ground`, into = c("Fighter 1 Ground", "Fighter 2 Ground"), sep = "\\s{2,}") %>%  
        separate(`Fighter 1 Ground`, into = c("Fighter 1 Ground Succ", "Fighter 1 Ground Att"), sep = " of ") %>%
        separate(`Fighter 2 Ground`, into = c("Fighter 2 Ground Succ", "Fighter 2 Ground Att"), sep = " of ") %>%
        mutate(Round = rownames(.))  %>%
        select(Round, everything(), -`Sig. str. %`) 
  
      #final fight table
      fight <- list("ID" = fight_id, "Totals" = fight_table_totals, "Sig Strikes" = fight_table_sig_str, 
                "Linked Pages" = linked_pages, "Link" = link)
  
      closeAllConnections()
      return(fight)   
      
    }
}

#extract info on event
extract_event <- function(link){
  
  event_page <- read_html_NE(link)  #event's web address
  
  #event's web id
  event_id <- str_remove(link, "http://ufcstats.com/event-details/") %>%
    str_remove("[^[:alnum:] ]") 
  
  #extracting event name
  event_name <- event_page %>%
    html_nodes(".b-content__title-highlight") %>%
    html_text(trim = T) 
  
  #extracting event details
  event_table <- event_page %>%
    html_nodes(".b-fight-details__table") %>%
    html_table %>%
    flatten_df %>%
    separate(Fighter, into = c("Fighter 1 Name", "Fighter 2 Name"), sep = "\\s{2,}") %>%
    separate(`Str`, into = c("Fighter 1 Str", "Fighter 2 Str"), sep = "\\s{2,}") %>%
    separate(`Td`, into = c("Fighter 1 TD", "Fighter 2 TD"), sep = "\\s{2,}") %>% 
    separate(`Sub`, into = c("Fighter 1 Sub Att", "Fighter 2 Sub Att"), sep = "\\s{2,}") %>% 
    separate(`Pass`, into = c("Fighter 1 Pass", "Fighter 2 Pass"), sep = "\\s{2,}") %>%  
    #if no method details, separate fills details column with NA
    #separate throws a warning for this
    separate(Method, into = c("Method", "Details"), sep = "\\s{2,}") %>%
    mutate(Event = event_name,
           Winner = if_else(str_detect(`W/L`, "draw"), "DRAW", `Fighter 1 Name`)) %>%
    select(Event, Winner, everything(), -`W/L`)
  
  #extracting pages linked to form event
  linked_pages <- event_page %>% 
    html_nodes(".b-link_style_black") %>%
    html_attr("href") %>%
    na.omit %>%
    unique
  
  #final event table
  event <- list("ID" = event_id, "Name" = event_name, "Results" = event_table, 
                "Linked Pages" = linked_pages, "Link" = link)   
  
  closeAllConnections()
  return(event)
}

#function to scrape website
#links = urls to scrape
#type = which type of links to scrape
#sleep = how long to wait before  scraping next link (avoid time-outs)
#base = pre-existing list of fighters, fights, or events to start from
#time_limit = max time to scrape before terminating (in seconds)
#rescrape = re-run on newly obtained links?
mma_scrape <- function(links, type = c("fighter", "fight", "event"), sleep = 0, 
                       base = NULL, time_limit = NULL, rescrape = T){
  
  #start time of most recent loop
  start_time <- if(!is_null(base)){
      base$`Start Time`
  } else{
      proc.time()
  } 
  
  elapsed_time <- (proc.time() - start_time)["elapsed"] #total time since execution
  
  #compilation of links that have already been processed
  old_links <- if(!is_null(base)){
    base %>%
      flatten %>%
      map("Link") %>%
      flatten_chr
    } else{
        NULL
    }   
  
  #ends if exceeds time limit
  if(!is_null(time_limit) & !is_null(base)){
    if(elapsed_time > time_limit){
      print(paste0("Time limit reached after scraping ", 
                   length(old_links), " pages in ", 
                   round(elapsed_time, 1), " sec."))  
      return(base)
      
    } 
    
  } 
  
  if(length(links) == 0){
      print(paste0("Finished scraping ", 
                   length(old_links), " pages in ", 
                   round(elapsed_time, 1), " sec."))  
      return(base)
    
  } else{
    
      #printing number of new links scraped
      print(paste0("Retrieved ", length(links), " links."))
      
      #identifying type of link
      is_fighter <- which(link_type(links) == "fighter")
      is_fight <- which(link_type(links) == "fight")
      is_event <- which(link_type(links) == "event")
      
      #number of links to scrape
      scrape_length <- length(c(if("fighter" %in% type){links[is_fighter]}, 
                                if("event" %in% type){links[is_event]},
                                if("fight" %in% type){links[is_fight]})
                              )
      
      #adding fighte rlinks to fighter table
      if("fighter" %in% type){
        print(paste0("Scraping ", length(links[is_fighter]), " fighters."))
        fighters <- pblapply(links[is_fighter], 
                             function(link){
                               elapsed_time_lapply <- (proc.time() - start_time)["elapsed"]
                               if(!is_null(time_limit)){
                                 if(elapsed_time_lapply > time_limit){
                                                                  return(NULL)
                                                               }}
                               Sys.sleep(sleep)
                               return(extract_fighter(link))
                                                }
                             ) %>% compact 
      } else{
        fighters <- NULL
      }
      
      #adding fight links to fight table
      if("fight" %in% type){
        print(paste0("Scraping ", length(links[is_fight]), " fights."))
        fights <- pblapply(links[is_fight], 
                           function(link){
                             elapsed_time_lapply <- (proc.time() - start_time)["elapsed"]
                             if(!is_null(time_limit)){
                               if(elapsed_time_lapply > time_limit){
                                                              return(NULL)
                                                            }}
                             Sys.sleep(sleep)
                             return(extract_fight(link))
                                            }
                           ) %>% compact         
      } else{
        fights <- NULL
      }
      
      #adding event links to event table
      if("event" %in% type){
        print(paste0("Scraping ", length(links[is_event]), " events."))
        events <- pblapply(links[is_event], 
                           function(link){
                             elapsed_time_lapply <- (proc.time() - start_time)["elapsed"]
                             if(!is_null(time_limit)){
                               if(elapsed_time_lapply > time_limit){
                                                              return(NULL)
                                                           }}
                             Sys.sleep(sleep)
                             return(extract_event(link))
                                            }
                          ) %>% compact
      } else{
        events <- NULL
      }

      elapsed_time <- (proc.time() - start_time)["elapsed"] #total time elapsed
    
      #end when no new pages to scrape
      if(!is_null(base)){
        print(paste0("Scraped ", length(old_links)+scrape_length, 
                     " pages in ", round(elapsed_time, 1), " sec."))
      } else{
        print(paste0("Scraped ", scrape_length, 
                     " pages in ", round(elapsed_time, 1), " sec."))     
      }   
    
      #list of scraped data tables
      scraped <- list("Fighters" = fighters, "Fights" = fights, "Events" = events,
                    "Start Time" = start_time) 
    
      #extract new links from scraped data
      new_links <- scraped %>%
        flatten %>%
        map("Linked Pages") %>%
        flatten_chr %>%
        unique %>%
        .[which(link_type(.) %in% type)] %>%
        setdiff(., old_links)
    
      #re-run with newly-obtained links
      if(rescrape){
        return(mma_scrape(links = new_links, type = type, 
               sleep = sleep, base = scraped, 
               time_limit = time_limit))
      } else{
        #return obtained data
        return(scraped)
      }
    }
}

#test
scrape <- mma_scrape(c("http://ufcstats.com/fighter-details/032cc3922d871c7f",
                        "http://ufcstats.com/fighter-details/029eaff01e6bb8f0"), sleep = 1,
                     type = c("fighter", "event"), rescrape = T, time_limit = 60)

link_type(test)