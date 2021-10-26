## ------------------------------------------------------------------------
library(data.table)  
library(readr)
library(dplyr)

## ------------------------------------------------------------------------
hours = c()
mins = c()
for(i in 0:23){
  for(j in 1:6){
    hours<-c(hours,i)
    j = j+1
    }
  i = i+1
}
for(j in 0:5){
  
  mins<-c(mins,j*10)
  j = j+1
}


## ------------------------------------------------------------------------
library(data.table)  
library(readr)
files <- list.files(path = "./eco/05/",pattern = ".csv")
setwd('./eco/05/')
temp <- lapply(files, read_csv,header = FALSE)
data <- rbindlist(temp)


## ------------------------------------------------------------------------
file_names <- dir("./eco/04_sm_csv/04/") #where you have your files

your_data_frame <- do.call(rbind,lapply(file_names,read.csv))

## ------------------------------------------------------------------------
df = read.csv("./eco/04_sm_csv/04/2013-01-12.csv")


## ------------------------------------------------------------------------
hours = c()
mins = c()
for(i in 0:23){
  for(j in 1:6){
    hours<-c(hours,i)
    j = j+1
    }
  i = i+1
}
for(j in 0:5){
  
  mins<-c(mins,j*10)
  j = j+1
}


## ----warning=FALSE-------------------------------------------------------
files <- list.files(path = "./eco/05/",pattern = ".csv")
setwd('./eco/05/')


n <- 60*10
##filePaths <- list.files(data_dir, "\\.csv$", full.names = TRUE)
result_05 <- do.call(rbind, lapply(files, function(path) {
  if (read_lines(path) !=""){
    df1 <-fread(path, header = FALSE)
    if(nrow(df1)==144*n){
      df <-aggregate(df1, list(rep(1:(nrow(df1) %/% n + 1), each = n, len = nrow(df1))), mean)[-1]
      df[["source"]] <- rep(path, nrow(df))
      df[["hour"]]<-hours
      df[["min"]]<-rep(mins,24)
      df
    }
  }
}))

## ------------------------------------------------------------------------
write.csv(result_05,'05_sm1.csv',row.names = FALSE)


## ------------------------------------------------------------------------
df = result_05


## ------------------------------------------------------------------------
library(stringr)
df$source = df$source %>% str_remove('.csv')

df$source = as.Date(df$source)


## ------------------------------------------------------------------------
names = c("powerallphases","powerl1", "powerl2","powerl3","currentneutral", "currentl1","currentl2","currentl3","voltagel1","voltagel2","voltagel3","phaseanglevoltagel2l1","phaseanglevoltagel3l1","phaseanglecurrentvoltagel1","phaseanglecurrentvoltagel2","phaseanglecurrentvoltagel3","date","hour","min")
colnames(df)<-names

## ------------------------------------------------------------------------
write.csv(df,'05_sm2.csv',row.names = FALSE)


## ------------------------------------------------------------------------
library(plotly)


## ------------------------------------------------------------------------
df$time = paste(df$hour,":",df$min)


## ------------------------------------------------------------------------
fig <- plot_ly(df,
        x= ~bill_length_mm,
        y= ~body_mass_g,
        color = ~species,
        type='scatter', mode='markers') %>% 
  plotly::layout(
    xaxis = list(title='Bill length (mm)'),
    yaxis = list(title='Body mass (g)'),
    title = 'Palmer Penguins'
  )


## ------------------------------------------------------------------------
df_day <-df%>% group_by(date)%>% summarise_at(vars(powerallphases), funs(mean(., na.rm=TRUE)))

## ------------------------------------------------------------------------
df$datetime = with(df, as.POSIXct(paste(date, time), format="%Y-%m-%d %H : %M"))


## ------------------------------------------------------------------------
#library(gapminder)
fig <- plot_ly(df_day, x = ~date, y = ~powerallphases,
               #color = ~country, 
               type='scatter', mode='lines',
               hovertemplate=paste('<b>',df_day$date,'</b><br>Year=%{x}<br>Life Expectancy=%{y:.2f} yrs')) %>% 
  plotly::layout(xaxis=list(title='Year'),
         yaxis=list(title='Life Expectancy'),
         showlegend=FALSE)
fig

## ------------------------------------------------------------------------
df$month = month(df$date)
df$day = mday(df$date)
df$year = year(df$date)


## ------------------------------------------------------------------------
df_day2<-df%>% group_by(month,day)%>% summarise_at(vars(powerallphases), funs(mean(., na.rm=TRUE)))


## ------------------------------------------------------------------------
fig2 <- plot_ly(df_day2, x = ~day, y = ~powerallphases,
               color = ~as.character(month), 
               type='scatter', mode='lines',
               hovertemplate=paste('<b>',df_day2$month,'</b><br>Year=%{x}<br>Life Expectancy=%{y:.2f} yrs')) %>% 
  plotly::layout(xaxis=list(title='Year'),
         yaxis=list(title='Life Expectancy'),
         showlegend=FALSE)
fig2


## ------------------------------------------------------------------------
df_hour = df%>% group_by(month,hour)%>% summarise_at(vars(powerallphases), funs(mean(., na.rm=TRUE)))


## ------------------------------------------------------------------------
fig3 <- plot_ly(df_hour, x = ~hour, y = ~powerallphases,
               color = ~as.character(month), 
               type='scatter', mode='lines',
               hovertemplate=paste('<b>',df_hour$month,'</b><br>Year=%{x}<br>Life Expectancy=%{y:.2f} yrs')) %>% 
  plotly::layout(xaxis=list(title='Year'),
         yaxis=list(title='Life Expectancy'),
         showlegend=FALSE)
fig3

## ------------------------------------------------------------------------

        fig.add_scatter(name = column,
                        x = v['Date'],
                        y = v[column], 
                        visible=True if k=='daily' else False # 'daily' values are shown from the start
                       )
                
    # one button per dataframe to trigger the visibility
    # of all columns / traces for each dataframe
    button =  dict(label=k,
                   method = 'restyle',
                   args = ['visible',visibility[i]])
    buttons.append(button)

# include dropdown updatemenu in layout
fig.update_layout(updatemenus=[dict(type="dropdown",
                                    direction="down",
                                    buttons = buttons)])
fig.show()

## ------------------------------------------------------------------------
df$year = year(df$date)
df_year = df%>% group_by(month,year)%>% summarise_at(vars(powerallphases), funs(mean(., na.rm=TRUE)))


## ------------------------------------------------------------------------

diamonds <- dplyr::bind_rows(diamonds.1,diamonds.2)

x1 <- diamonds[ which(diamonds$ID=='group 1'),"cut"]
x2 <- diamonds[ which(diamonds$ID=='group 2'),"cut"]
y1 <- diamonds[ which(diamonds$ID=='group 1'),"price"]
y2 <- diamonds[ which(diamonds$ID=='group 2'),"price"]

color1 <- diamonds[ which(diamonds$ID=='group 1'),"clarity"]
color2 <- diamonds[ which(diamonds$ID=='group 2'),"clarity"]

fig <- plotly::plot_ly( diamonds, x = ~cut, y = ~powerallphases, color = ~clarity, type = "box") %>%
  plotly::layout(title = "Drop down menus - Time Frame"
                 ,xaxis = list(title= " Diamond Cut")
                 ,yaxis = list(title = "Diamond Price")
                 ,updatemenus = list(
                   list(
                     y = 0.7,
                     buttons = list(
                       # Modify attributes for option 1 in the dropdown list
                       list(method = "update" # "restyle": modify data or data attributes, "update": modify data and layout attributes
                           ,args= list(list(x=list(x1$cut), y=list(y1$price), color=list(color1$clarity)))
                           ,label = "Group 1")
                       # Modify attributes for option 2 in the dropdown list
                       ,list(method = "update"
                             ,args= list(list(x=list(x2$cut), y= list(y2$price), color=list(color2$clarity)))
                             ,label = "Group 2")
                     )
                   )
                  )
                 )

htmlwidgets::saveWidget(as_widget(fig), "luenMWE.html")


## ------------------------------------------------------------------------
fig <- plot_ly(df_day, x = ~date, y = ~powerallphases,
               #color = ~country, 
               type='scatter', mode='lines',
               hovertemplate=paste('<b>',df_day$date,'</b><br>Year=%{x}<br>Life Expectancy=%{y:.2f} yrs')) %>% 
  plotly::layout(xaxis=list(title='Year'),
         yaxis=list(title='Life Expectancy'),
         showlegend=FALSE)
fig

fig2 <- plot_ly(df_day2, x = ~day, y = ~powerallphases,
               color = ~as.character(month), 
               type='scatter', mode='lines',
               hovertemplate=paste('<b>',df_day2$month,'</b><br>Year=%{x}<br>Life Expectancy=%{y:.2f} yrs')) %>% 
  plotly::layout(xaxis=list(title='Year'),
         yaxis=list(title='Life Expectancy'),
         showlegend=FALSE)
fig2


## ------------------------------------------------------------------------
fig <- plot_ly()
fig <- fig %>% add_lines(x = df_day$date, y = df_day$powerallphases)
fig <- fig %>% add_lines(x = df_day2$day, y = df_day2$powerallphases,
               color = ~as.character(df_day2$month), visible = F)
fig <- fig %>% layout(
    title = "Drop down menus - Styling",
    #xaxis = list(domain = c(0.1, 1)),
    #yaxis = list(title = "y"),
    updatemenus = list(


      list(
        y = 0.7,
        buttons = list(
          list(method = "restyle",
               args = list(
                 list("visible", list(TRUE, FALSE)),
                 list(xaxis = list(title = "Total Power Consumption"))
               ),
               label = "Annual"),

          list(method = "restyle",
               args = list(
                 list("visible", list(FALSE, TRUE)),
                 list("y",df_day2$powerallphases),
                 list("x",df_day2$day)
               ),
               label = "Daily")))
    )
  )

fig

## ------------------------------------------------------------------------
p <- iris %>%
  plot_ly(
    type = 'scatter', 
    x = ~Sepal.Length, 
    y = ~Petal.Length,
    text = ~Species,
    hoverinfo = 'text',
    mode = 'markers', 
    transforms = list(
      list(
        type = 'filter',
        target = ~Species,
        operation = '=',
        value = unique(iris$Species)[1]
      )
  )) %>% layout(
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(iris$Species)[1]),
               label = unique(iris$Species)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(iris$Species)[2]),
               label = unique(iris$Species)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(iris$Species)[3]),
               label = unique(iris$Species)[3])
        )
      )
    )
  )


## ------------------------------------------------------------------------
df_day1 <-df%>% group_by(date,year,month,day)%>% summarise_at(vars(powerallphases), funs(sum(., na.rm=TRUE)))


## ------------------------------------------------------------------------
library(idbr)
library(dplyr)
fig <- df %>%
  plot_ly(
    x = ~datetime, 
    y = ~powerallphases, 
  #  color = ~month, 
    frame = ~date, 
  #  text = ~country, 
  #  hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )

fig <- fig %>% animation_opts(
    1000, easing = "elastic", redraw = FALSE
  )
fig <- fig %>% animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )
fig <- fig %>% animation_slider(
    currentvalue = list(prefix = "YEAR ", font = list(color="red"))
  )

fig


## ------------------------------------------------------------------------
library(plotly)


fig <- plot_ly(df, type = 'scatter', mode = 'lines',color = "#6987C9")%>%
  add_trace(x = ~datetime, y = ~powerallphases,
            hovertemplate=paste('<b>',df$datetime,'</b><br>Total Power=%{y}')) %>%
  layout(showlegend = F, title='Total Power Consumption - Household #5',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          
                          list(count=1, label="1d", step="day", stepmode="backward"),
                          list(count=7, label="1wk", step="day", stepmode="backward"),
                          list(count=1, label="1mo", step="month", stepmode="backward"),
                          list(count=6, label="6mo", step="month", stepmode="backward"),

                          list(step="all")
                        ))))
fig <- fig %>%
  layout(
         xaxis = list(zerolinecolor = 'black',
                      zerolinewidth = 1,
                      gridcolor = 'ffff',
                      title = 'Time Frame'),
         yaxis = list(zerolinecolor = 'black',
                      zerolinewidth = 1,
                      gridcolor = 'lightgray',
                      title = 'Total Power',
                      showline= T, linewidth=1, linecolor='black',
                      dtick = 2000),
         plot_bgcolor='#FFFFFF', margin = 0.1, width = 900)
fig

## ------------------------------------------------------------------------
library(htmlwidgets)
saveWidget(fig, "plotly.html", selfcontained = F, libdir = "lib")

