# function to force monotonocity on nominally monotone vector and return differenced series correct with initial value
make_vec <- function(x) {
  #function to take a nominally monotone vector and force monotonicity by back propagation
  index_fix <-  which(diff(x)< 0)
  len_if <- length(index_fix)
  while (len_if > 0){
    index_use <- max(index_fix)
    x[index_use] <- x[index_use + 1]
    index_fix <- which(diff(x) < 0)
    len_if <- length(index_fix)
  }

  x_out <- x -lag(x)

  x_out[1] <- x[1]

  return(x_out)
}


#functions to aggregate
rolling_week <- function(date_vector, end_date = as.Date(Sys.Date())){
  #Coerce to dates
  date_vector <- as.Date(date_vector)
  end_date = as.Date(end_date)

  min_date <- min(date_vector)

  period_length <- as.numeric(difftime(end_date, min_date))
  period_weeks <- floor(period_length / 7)

  week_ends <-  c(end_date + lubridate::days(1), end_date - lubridate::weeks(1:period_weeks))

  out <- period_weeks - cut(date_vector, breaks = week_ends, include_lowest = TRUE, right = TRUE, label = FALSE) + 1

  return(out)
}

daily_to_weekly <- function(df) {
  last_date <- max(df$Date_reported)
  df1 <- df %>%
    mutate(week_num = rolling_week(date_vector = Date_reported,
                                   end_date = last_date)
    ) %>%
    group_by(week_num) %>%
    summarize(
      deaths = sum(deaths),
      week_end = max(Date_reported)
    )
}



#make plots of counts
count_plot <- function(dfx,location,date_calc_end, date_calc_start,agg_weekly = FALSE){
  #distinguish baseline from most recent 14 days
  dfx <- dfx %>% filter(NAME == location) #%>% select(-GEOID)
  #cut_date <- max(dfx$Date_reported) - 14

  if(agg_weekly) {
    dfx <- daily_to_weekly(dfx) %>%
      filter(!is.na(week_num)) %>%
      rename(Date_reported = week_end)
  }

  dfx$phase <- ifelse(dfx$Date_reported < date_calc_start,
                      "history",
                      ifelse(dfx$Date_reported >= date_calc_start &
                               dfx$Date_reported <= date_calc_end,
                             "baseline","post_baseline")
  )

  dfx$phase <-  factor(dfx$phase, levels = c("history","baseline","post_baseline"))




  #create medians
  dfA1 <- dfx %>% filter(phase == 'baseline')
  med_A1 <- median(dfA1$deaths, na.rm=TRUE)

  baseline_start <- min(dfA1$Date_reported)
  baseline_end <- max(dfA1$Date_reported)

  p1 <- ggplot(data=dfx,aes(x=Date_reported,y=deaths))+
    theme_bw()+
    geom_point(aes(shape=phase),size=rel(2))+
    # facet_wrap(~Measure,
    #            ncol = 1,
    #            scales = 'free_y')+
    labs(title=paste0("Death counts for ", location),
         subtitle=paste0("Dashed line median for baseline period: ",as.character(baseline_start)," to ",as.character(baseline_end)))+

    geom_hline(aes(yintercept=med_A1),lty=2)+

    ylab("")+
    xlab("Date Reported")+
    annotate("rect", fill = "blue", alpha = 0.1,
             xmin = cut_date + .5, xmax = max(dfx$Date_reported)+.5,
             ymin = -Inf, ymax = Inf) #+
    # scale_shape_discrete(na.translate=FALSE)+
    # theme(legend.position = c(0.95, 0.95),
    #       legend.justification = c("right", "top"),
    #       legend.text = element_text(size = 8),
    #       legend.title= element_text(size = 10))

  # #problem on March 30 for positive tests, the back check method leads to 100% positive testing.
  # if(location=="WI" & !agg_weekly) {
  #   p1 <- p1 + labs(caption = "Set aside Pct Positive on 30 March:  cleaning method gives 100% positive")
  # }

  p1
}


##########################
# expects weekly bucket of data
##########################
plot_ccharts <- function(df1,
                         region_name,
                         date_calc_end,
                         date_calc_start,
                         title=NULL,
                         subtitle=NULL) {
  out_list <- list()
  df1 <- df1 %>%
    mutate(weeks_ago = as.integer(week_num - 1),
           week_end = Date_reported)

  df1$phase <- ifelse(dfx$Date_reported < date_calc_start,
                      "history",
                      ifelse(df1$Date_reported >= date_calc_start &
                               df1$Date_reported <= date_calc_end,
                             "baseline","post_baseline")
  )

  df1$phase <-  factor(df1$phase, levels = c("history","baseline","post_baseline"))

  CW <- df1$deaths[1]

  df <- df1 %>%
    filter(phase == "baseline")

  mean_deaths = mean(df$deaths)

  UCL = mean_deaths + 3*sqrt(mean_deaths)

  LCL = max(0, mean_deaths - 3*sqrt(mean_deaths))

  out_list$cchart <- ggplot(data=df,aes(x=week_end,y=deaths)) +
    geom_hline(yintercept = mean_deaths)+
    geom_hline(yintercept = UCL, linetype = 'dashed')+
    geom_hline(yintercept =  LCL, linetype = 'dashed')+
    geom_point(size = rel(2), shape = 1)+
    geom_line()+
    theme_bw()+
    #theme(axis.text.x = element_text(angle = 90))+
    labs(title=paste0(title, region_name),
         x = "End date for 7 Day bins", y = "",
         subtitle= subtitle)+
    geom_point(data=df1[df1$phase != "baseline",],aes(x=week_end,y=deaths),
               colour = "blue", size=rel(2))

  #now center the plot

  out_list$cchart_ctr <- ggplot(data=df,aes(x=week_end,y=deaths - CW)) +
    geom_hline(yintercept = mean_deaths - CW) +
    geom_hline(yintercept = UCL - CW, linetype = 'dashed') +
    geom_hline(yintercept = LCL - CW, linetype = 'dashed') +
    geom_point(size = rel(2), shape = 1) +
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 90)) +
    labs(title=paste0(title, region_name),
         x = "End date for 7 Day bins", y = "",
         subtitle=paste0(subtitle, " \n centered on
                         next week's death count ",as.integer(CW)))+
    geom_point(data=df1[1,],aes(x=week_end,y=deaths - CW),
               colour = "blue", size=rel(2))


  return(out_list)

}


###############i chart
plot_icharts <- function(df1,
                         region_name,
                         date_calc_end,
                         date_calc_start,
                         title=NULL,
                         subtitle=NULL) {
  list_out <- list()

  CW <- df1$deaths[1]

  df1 <- df1 %>%
    mutate(weeks_ago = as.integer(week_num - 1),
           week_end = Date_reported)

  df1$phase <- ifelse(dfx$Date_reported < date_calc_start,
                      "history",
                      ifelse(df1$Date_reported >= date_calc_start &
                               df1$Date_reported <= date_calc_end,
                             "baseline","post_baseline")
  )

  df1$phase <-  factor(df1$phase, levels = c("history","baseline","post_baseline"))

  #reorder the df to start with oldest date

  df <- df1 %>%
    filter(phase == "baseline") %>%
    arrange(week_end)

  MR_deaths = abs(diff(df$deaths))

  sigma_aver = mean(MR_deaths)/1.128

  sigma_med  = median(MR_deaths)/0.9554

  mean_deaths = mean(df$deaths)

  UCL = mean_deaths + 3*sigma_med

  LCL = max(0, mean_deaths - 3*sigma_med)

  list_out$ichart <- ggplot(data=df,aes(x=week_end,y=deaths)) +
    geom_hline(yintercept = mean_deaths)+
    geom_hline(yintercept = UCL, linetype = 'dashed')+
    geom_hline(yintercept =  LCL, linetype = 'dashed')+
    geom_point(size = rel(2), shape = 1)+
    geom_line()+
    theme_bw()+
    #theme(axis.text.x = element_text(angle = 90))+
    labs(title=paste0(title,region_name),
         x = "End date for 7 Day bins", y = "",
         subtitle=subtitle)+
    geom_point(data=df1[df1$phase != "baseline",],aes(x=week_end,y=deaths),
               colour = "blue", size=rel(2))


  list_out$ichart_ctr <- ggplot(data=df,aes(x=week_end,y=deaths - CW)) +
    geom_hline(yintercept = mean_deaths - CW)+
    geom_hline(yintercept = UCL - CW, linetype = 'dashed')+
    geom_hline(yintercept =  LCL - CW, linetype = 'dashed')+
    geom_point(size = rel(2), shape = 1)+
    theme_bw()+
    #theme(axis.text.x = element_text(angle = 90))+
    labs(title=paste0("Individuals control chart for deaths: ",region_name),
         x = "End date for 7 Day bins", y = "",
         subtitle=paste0(subtitle, "\n centered on next week's
                                 case count ",as.integer(CW))) +
    geom_point(data=df1[1,],aes(x=week_end,y=case_weekly - CW),
               colour = "blue", size=rel(2))

  return(list_out)
}

#############Laney charts
plot_Laney_charts <- function(df1,region_name) {

  list_out <- list()

  df1 <- df1 %>%
    mutate(weeks_ago = as.integer(week_num - 1),
           week_end = Date_reported)

  CW <- df1$deaths[1]

  df <- df1 %>%
    filter(weeks_ago > 0) %>%
    arrange(week_end)

  mean_deaths = mean(df$deaths)

  #compute z scores and assess variation in z scores for baseline period
  df$zi <- (df$deaths - mean_deaths)/sqrt(mean_deaths)

  Rzi <- abs(diff(df$zi))
  #use median calculation to align with ichart
  #sigma_zi <- mean(Rzi)/1.128

  sigma_zi <- median(Rzi)/0.9554

  Laney_sigma <- sqrt(mean_deaths)*sigma_zi

  cprime_UCL <- mean_deaths + 3*Laney_sigma

  cprime_LCL <- max(0, mean_deaths - 3*Laney_sigma)


  list_out$Laney_chart <- ggplot(data=df,aes(x=week_end,y=deaths)) +
    theme_bw()+
    geom_point(size=rel(2), shape = 1)+
    #geom_line()+
    labs(title= paste0("c' control chart for deaths: ", region_name),
         x = "End date for 7 Day bins", y = "",
         subtitle=paste0("Limits based on Laney calculations from baseline period; sigma inflation = ", round(sigma_zi,2))) +
    geom_hline(yintercept=mean_deaths)+
    geom_hline(yintercept=cprime_LCL,linetype='dashed')+
    geom_hline(yintercept=cprime_UCL,linetype='dashed')+
    theme(axis.text.x = element_text(angle = 90))+
    geom_point(data=df1[1,],aes(x=week_end,y=deaths),
               colour = "blue", size=rel(2))


  list_out$Laney_chart_ctr <- ggplot(data=df,aes(x=week_end,y=deaths - CW)) +
    theme_bw()+
    geom_point(size=rel(2), shape = 1)+
    #geom_line()+
    labs(title= paste0("c' control chart for deaths: ", region_name),
         x = "End date for 7 Day bins", y = "",
         subtitle=paste0("Limits based on Laney calculations
                         (within and between week variation)
                          \n centered on next week's case count ",as.integer(CW))) +
    geom_hline(yintercept=mean_deaths - CW)+
    geom_hline(yintercept=cprime_LCL - CW,linetype='dashed')+
    geom_hline(yintercept=cprime_UCL - CW,linetype='dashed')+
    theme(axis.text.x = element_text(angle = 90))+
    geom_point(data=df1[1,],aes(x=week_end,y=deaths - CW),
               colour = "blue", size=rel(2))

  return(list_out)

}
