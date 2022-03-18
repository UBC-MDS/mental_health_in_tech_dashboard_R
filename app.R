library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
#library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(dplyr)

app <- Dash$new(
  external_stylesheets = dbcThemes$BOOTSTRAP
  )

logo <- "https://cdn-icons-png.flaticon.com/512/2017/2017231.png"
gitlogo = "https://cdn-icons-png.flaticon.com/512/25/25231.png"
data <- read.csv('data/processed/survey.csv')


chart_tpye = c("Bar","Pie")
genderlist = c("Male","Female","Other")
sizelist = c("1-5","6-25","26-100","100-500","500-1000","More than 1000")
agelist = c("18-24","25-34","35-44","45-54","55+")

pie_chart <- function(df, col, title=NULL, colors=NULL, width=650, height=550,l=0,r=0,t=0,b=0){
  col2 <- sym(col)
  df_p <- df %>% 
    group_by({{ col2 }}) %>% 
    summarize(cnt=n()/nrow(df)) %>%
    ungroup()
  
  #if (is.null(title)){
    #title = col
  #}
  
  if (is.null(colors)){
    fig <- plot_ly(df_p, 
                   labels = as.formula(paste0('~', col)), 
                   values = ~cnt,
                   hoverinfo='label+percent',
                   textinfo='label+percent',
                   type = "pie",
                   width = width,
                   height = height
    )
  } else {
    fig <- plot_ly(df_p, 
                   labels = as.formula(paste0('~', col)), 
                   values = ~cnt,
                   hoverinfo='label+percent',
                   textinfo='label+percent',
                   type = "pie",
                   width = width,
                   height = height,
                   marker=list(colors=colors)
    )
  }
  fig <- fig %>% layout(title=title, margin=list(l=l, r=r, t=t, b=b))
}

bar_chart <- function(df, col, title=NULL, colors=NULL, width=650, height=550,l=50,r=0,t=0,b=0){
  col2 <- sym(col)
  df_p <- df %>% 
    filter({{ col2 }} != "") %>%
    group_by({{ col2 }}) %>% 
    summarize(cnt=n()/nrow(df)) %>%
    ungroup() 
  
  #if (is.null(title)){
    #title = col
  #}
  
  if (is.null(colors)){
    fig <- plot_ly(df_p,
                   x=as.formula(paste0('~', col)), 
                   y=~cnt,
                   hoverinfo='label+percent',
                   type = "bar",
                   width = width,
                   height = height
    )
  } else {
    fig <- plot_ly(df_p,
                   x=as.formula(paste0('~', col)), 
                   y=~cnt,
                   hoverinfo='label+percent',
                   type = "bar",
                   width = width,
                   height = height,
                   marker=list(colors=colors)
    )
  }
  fig <- fig %>% layout(title=title,
                        margin=list(l=l, r=r, t=t, b=b),
                        xaxis=list(title="% of Respondents"),
                        yaxis=list(title="Responses",
                                   tickformat=".0%")
  )
}

tabs <- htmlDiv(
  list(
    dbcTabs(
      list(
        dbcTab(label = "Summary Overview", tab_id = "tab-1"),
        dbcTab(label = "Interactive View", tab_id = "tab-2"),
        dbcTab(label = "Map View", tab_id = "tab-3")
      ),
      id = "tabs",
      active_tab = "tab-1"
    ),
    div(id = "content")
  )
)

tab1 <- htmlDiv(
  list(
    dbcContainer(
      list(
        htmlBr(),
        dbcRow(
          list(
            dbcCol(
              list(
                dbcToast(
                  list(
                    htmlH1("Mental Health in Tech Dashboard"),
                    htmlBr(),
                    htmlP(
                      list(
                        htmlH2("Introduction"),
                        htmlBr(),
                        htmlP(
                          list(
                            "In this dashboard we want explore the attitude towards mental health in tech companies. We assume that the gender, age, company size, whether the company provides mental health benefits are likely to be correlated with our research question. We also explore the geographical distribution of respondents.", 
                            htmlBr(), 
                            "The first summary tab presents an overall distribution of respondents. The interactive tab allows you to choose a specific question to explore. The map tab shows the response to a specific question by states."
                            )
                        ),
                        htmlBr(),
                        htmlH3("Data Source"),
                        htmlP("The data set used in this dashboard is from the link below. This dataset is from a 2014 survey that measures attitudes towards mental health and frequency of mental health disorders in the tech workplace."),
                        dccLink(
                          href="https://www.kaggle.com/osmi/mental-health-in-tech-survey",
                          title="Data set"),
                        htmlBr(),
                        htmlH3("Original Survey"),
                        htmlP("The OSMI conducts the mental health in tech survey every year. The survey data for other years can be found at this link below."),
                        dccLink(href="https://osmihelp.org/research", title="Original Research")
                      )
                    )
                  )
                )
              ),width=5,style=list('margin-right'='0px','margin-left'='-150px')
            ),
            dbcCol(
              list(
                dbcRow(
                  list(
                    dbcCol(
                      dbcToast(
                        dccGraph(
                          id='gender',
                          figure=pie_chart(data,"Gender",title="<b>Gender distribution of respondents</b>", colors=c('pink','lightskyblue','lightgray'),width=430,height=350,t=50,l=50),
                          style=list('margin-left'='20px','width'='100%','height'='400px')
                        ),style=list('position'='center','width'='500px','height'='430px')
                      ),class_name = 'chart-box', style=list('margin-bottom'='20px','margin-right'='0px','margin-left'='0px')
                    ),
                    dbcCol(
                      dbcToast(
                        dccGraph(
                          id='age',
                          figure=bar_chart(data,"Age",title="<b>Age distribution of respondents</b>", width=430,height=350,t=50),
                          style=list('margin-left'='20px','width'='100%','height'='400px')
                        ),style=list('position'='center','width'='500px','height'='430px')
                      ),class_name = 'chart-box', style=list('margin-bottom'='20px','margin-right'='0px','margin-left'='-10px')
                    )
                  )
                ),
                dbcRow(
                  list(
                    dbcCol(
                      dbcToast(
                        dccGraph(
                          id='size',
                          figure=bar_chart(data,"Q5",title="<b>Company Size (Number of Employee)</b>", width=430,height=350,t=50),
                          style=list('margin-left'='20px','width'='100%','height'='400px')
                        ),style=list('position'='center','width'='500px','height'='430px')
                      ),class_name = 'chart-box', style=list('margin-right'='0px','margin-left'='0px')
                    ),
                    dbcCol(
                      dbcToast(
                        dccGraph(
                          id='benefit',
                          figure=pie_chart(data,"Q8",title="<b>Percentage whose knowledge about their \n company's offer for mental health benefits</b>", colors=c('lightgray','skyblue','lightskyblue'),width=430,height=350,t=50,l=50),
                          style=list('margin-left'='20px','width'='100%','height'='400px')
                        ),style=list('position'='center','width'='500px','height'='430px')
                      ),class_name = 'chart-box', style=list('margin-right'='0px','margin-left'='-10px')
                    )
                  )
                )
              ),style=list('margin-right'='0px','margin-left'='-150px')
            )
          )
        )
      )
    )
  )
)


qdict = tibble("Q11"="Does your employer provide resources to learn more about mental health issues and how to seek help?",
             "Q12"="Is your anonymity protected if you choose to take advantage of mental health or substance abuse treatment resources?",
             "Q13"="How easy is it for you to take medical leave for a mental health condition?",
             "Q14"="Do you think that discussing a mental health issue with your employer would have negative consequences?",
             "Q15"="Do you think that discussing a physical health issue with your employer would have negative consequences?",
             "Q16"="Would you be willing to discuss a mental health issue with your coworkers?",
             "Q17"="Would you be willing to discuss a mental health issue with your direct supervisor(s)?",
             "Q18"="Would you bring up a mental health issue with a potential employer in an interview?",
             "Q19"="Would you bring up a physical health issue with a potential employer in an interview?",
             "Q20"="Do you feel that your employer takes mental health as seriously as physical health?",
             "Q21"="Have you heard of or observed negative consequences for coworkers with mental health conditions in your workplace?")

tab2 <- htmlDiv(
  list(
    dbcContainer(
      list(
        htmlBr(),
        dbcRow(
          list(
            dbcCol(
              list(
                dbcToast(
                  list(
                    htmlH1("Mental Health in Tech Dashboard"),
                    htmlBr(),
                    htmlP(
                      list(
                        htmlH2("Instruction"),
                        htmlBr(),
                        htmlP("Please select the research question and respondents you would like to explore. By default, the plot includes all respondents in the data set."),
                        htmlH4("Plot type"),
                        htmlBr(),
                        dccRadioItems(
                          id='chart-widget',
                          options = c("Pie", "Bar"),
                          value="Pie",
                          labelStyle=list(display="block")
                        ),
                        htmlBr(),
                        
                        htmlH4("Survey questions"),
                        dccDropdown(
                          id='q-widget',
                          value = colnames(qdict)[1],
                          options = qdict %>% colnames %>% purrr::map(function(col) list(label = unlist(qdict[col], use.names = FALSE), value = col)),
                          optionHeight = 100
                        ),
                        htmlBr(),
                        
                        htmlH4("Gender"),
                        dccDropdown(
                          id = 'gender-widget',
                          value = genderlist,
                          options = genderlist %>% purrr::map(function(col) list(label = col, value = col)),
                          multi = TRUE
                        ),
                        htmlBr(),
                        
                        htmlH4("Age"),
                        dccDropdown(
                          id = 'age-widget',
                          value = agelist,
                          options = agelist %>% purrr::map(function(col) list(label = col, value = col)),
                          multi = TRUE
                        ),
                        htmlBr(),
                        
                        htmlH4("Company size"),
                        dccDropdown(
                          id = 'size-widget',
                          value = sizelist,
                          options = sizelist %>% purrr::map(function(col) list(label = col, value = col)),
                          multi = TRUE
                        )
                      )
                    )
                  )
                )
              ),width=5,style=list('margin-right'='0px','margin-left'='20px')
            ),
            dbcCol(
              list(
                dbcToast(
                  list(
                    htmlH3(id='display-question'),
                    htmlBr(),
                    htmlBr(),
                    dccGraph(
                      id='interactive',style=list('margin-left'='100px','width'='100%','height'='700px'),
                    )
                  ),style=list('position'='center', 'width'='100%', 'height'='800px')
                )
              ),class_name = 'chart-box', style=list('margin-right'='0px','margin-left'='-150px')
            )
          )
        )
      )
    )
  )
)



tab3 <- htmlDiv(
  list(
    dbcContainer(
      list(
        htmlBr(),
        dbcRow(
          list(
            dbcCol(
              list(
                dbcToast(
                  list(
                    htmlH1("Mental Health in Tech Dashboard"),
                    htmlBr(),
                    htmlP(
                      list(
                        htmlH2("Instruction"),
                        htmlBr(),
                        htmlP("Please select the research question and response you would like to explore. The map will show you the percentage of your chosen response by states."),
                        htmlBr(),
                        htmlH4("Survey questions"),
                        dccDropdown(
                          id='map_q-widget',
                          value = colnames(qdict)[1],
                          options = qdict %>% colnames %>% purrr::map(function(col) list(label = unlist(qdict[col], use.names = FALSE), value = col)),
                          optionHeight = 100
                        ),
                        htmlBr(),
                        
                        htmlH4("Response"),
                        dccDropdown(id="answer-widget")
                      )
                    )
                  )
                )
              ),width=5,style=list('margin-right'='0px','margin-left'='-150px')
            ),
            dbcCol(
              list(
                dbcToast(
                  list(
                    htmlH3(id='map_display-question'),
                    htmlBr(),
                    htmlBr(),
                    dccGraph(
                      id='interactive-map',style=list('margin-left'='50px','width'='100%','height'='700px'),
                    )
                  ),style=list('position'='center', 'width'='100%', 'height'='750px')
                )
              ),class_name = 'chart-box', style=list('margin-right'='0px','margin-left'='-50px')
            )
          )
        )
      )
    )
  )
)

navbar <- dbcNavbar(
  dbcContainer(
    list(
      a(
        # Use row and col to control vertical alignment of logo / brand
        dbcRow(
          list(
            dbcCol(img(src = logo, height = "30px")),
            dbcCol(dbcNavbarBrand("Mental Health in Tech Dashboard", className = "ms-2")),
            dbcCol(htmlImg(src=gitlogo,height="30px"))
          ),
          align = "left",
          className = "g-0"
        ),
        href = "https://github.com/UBC-MDS/mental_health_in_tech_dashboard_r",
        style = list("textDecoration" = "none")
      )
    )
  ),
  color = "lightskyblue",
  dark = TRUE
)

licensebar <- htmlFooter(
  dbcContainer(
    htmlP("Mental Health in Tech Dashboard was created by Jordan Casoli, Nick Lisheng Mao, Hatef Rahmani and Ho Kwan Lio. The materials are licensed under the terms of the MIT license (Copyright (c) 2022 Master of Data Science at the University of British Columbia)."),
  )
)

app %>% set_layout(
  dbcRow(
    list(
      navbar,        
      tabs,
      licensebar
    )
  )
)


app$callback(
  output("content", "children"),
  list(input("tabs", "active_tab")),
  function(at) {
    if (at == "tab-1") {
      return(tab1)
    } else if (at == "tab-2") {
      return(tab2)
    } else if (at == "tab-3") {
      return(tab3)
    }
    return(p("This should not ever be displayed"))
  }
)


app$callback(
  output("display-question", 'children'),
  list(input('q-widget', "value")),
  function(question){
    htmlH3(qdict[question] %>% pull())
  }
)
  
app$callback(
  output("interactive", 'figure'),
  list(input('q-widget', "value"),
       input('chart-widget', "value"),
       input('gender-widget', "value"),
       input('age-widget', "value"),
       input('size-widget', "value")),
  
  function(question, chart_type, gender, age, size){
    df2 <- data %>% filter(Gender %in% unlist(gender,use.names=FALSE)
                            & Age %in% unlist(age,use.names=FALSE)
                            & Q5 %in% unlist(size,use.names=FALSE))
    if (chart_type == "Pie"){
      colors = c('skyblue','navy','lightgray')
      fig<-pie_chart(df2, question, title=NULL, colors)
      fig
    } else if (chart_type == "Bar"){
      colors = c('skyblue','navy','lightgray')
      fig<-bar_chart(df2, question, title=NULL, colors)
      fig
    }
  }
)

answerDict <- array(list(),11,dimnames=list(c('Q11','Q12','Q13','Q14','Q15','Q16','Q17','Q18','Q19','Q20','Q21')))
answerDict$Q11<-c("Yes","No","Don't know")
answerDict$Q12<-c("Yes","No","Don't know")
answerDict$Q13<-c("Very difficult","Somewhat difficult","Somewhat easy","Very easy","Don't know")
answerDict$Q14<-c("Yes","No","Maybe")
answerDict$Q15<-c("Yes","No","Maybe")
answerDict$Q16<-c("Yes","No","Some of them")
answerDict$Q17<-c("Yes","No","Some of them")
answerDict$Q18<-c("Yes","No","Maybe")
answerDict$Q19<-c("Yes","No","Maybe")
answerDict$Q20<-c("Yes","No","Don't know")
answerDict$Q21<-c("Yes","No")


app$callback(
  output("map_display-question", 'children'),
  list(input('map_q-widget', "value")),
  function(question){
    htmlH3(qdict[question] %>% pull())
  }
)

app$callback(
  output("answer-widget",'options'),
  list(input('map_q-widget','value')),
  function(question){
    answerDict[question] %>% unlist(use.names = FALSE) %>% purrr::map(function(col) list(label = col, value = col))
  }
)

app$callback(
  output("interactive-map", 'figure'),
  list(input('map_q-widget', "value"),
       input('answer-widget', "value")),
  function(question, response){
    question_q <- sym(question)
    map_df <- data %>% filter(Country=="United States") %>%
      select(state, {{ question_q }}) %>%
      group_by(state,{{ question_q }}) %>%
      summarize(cnt = n()) %>%
      mutate(Percent = cnt/sum(cnt)) %>%
      filter({{ question_q }} == response)
    
    geo <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    fig <- plot_geo(map_df, locationmode = 'USA-states') %>% 
      add_trace(z = ~Percent, text = ~state, locations = ~state,
                color = ~Percent, colors = 'Blues') %>%
      layout(geo = geo, width=750, height=600,
             margin=list(l=0,r=0,t=0,b=0))
    
    fig
  }
)

#app$run_server()
app$run_server(host = '0.0.0.0')






