## Libraries
library(DBI)
library(DT)
library(RPostgreSQL)
library(shinydashboard)
library(shiny)
library(hrbrthemes)
library(tidyr)
library(dplyr)
library(ggplot2)

# Connect to the MySQL database: con
con <- dbConnect(RPostgreSQL::PostgreSQL(), 
                 dbname = "spry", 
                 host = "spry.cgrmtgar0fom.us-east-1.rds.amazonaws.com", 
                 port = 5432,
                 user = "spry_postgres",
                 password = "LmmhkMCR0EZhpIHrlJ2N")

# # Get table names
tables <- dbListTables(con)

# Display structure of tables
str(tables)

# Queries
paperwork <- dbGetQuery(con, "
  SELECT
    date_trunc ('month',created_at) AS created_date
    , created_by_user_role
    , COUNT(DISTINCT id) as created
    , COUNT(CASE WHEN paperwork_form_status ='APPROVED' THEN id END) as approved
    , COUNT(CASE WHEN paperwork_form_status ='COMPLETED' THEN id END) as completed
    , COUNT(CASE WHEN paperwork_form_status ='VIEWED' THEN id END) as viewed
    , COUNT(CASE WHEN paperwork_form_status ='NOT_STARTED' THEN id END) as not_started
    , COUNT(CASE WHEN paperwork_form_status ='IN_PROGRESS' THEN id END) as in_progress
    , COUNT(CASE WHEN paperwork_form_status ='REJECTED' THEN id END) as rejected
    
    FROM paperwork_form
    
    GROUP BY 1,2
    ORDER BY 1 ASC")

recruit <- dbGetQuery(con, "
  SELECT
   date_trunc('day',created_at) as created_date
  , psa_base.name as school_name
  , psa_base.sports_team as sports_team
  , COUNT(DISTINCT psa_base.id) as total_recruits
  , COUNT(CASE WHEN psa_base.is_added_through_questionnaire is TRUE THEN psa_base.id END) as psa_added_thru_questionnaire
  
  FROM
          (SELECT
          psa.id
          ,psa.created_at
          , INITCAP(psa.commitment_status) as commitment_status
          , psa.high_school_graduation_year
          , psa.sport_team_id
          , st.name as sports_team
          , psa.university_id
          , uuu.name
          , ccc.email as recruitment_coach_email
          , psa.is_added_through_questionnaire
          
          FROM prospective_student_athlete psa
                  JOIN university uuu ON uuu.id = psa.university_id
              JOIN coach ccc ON ccc.id = psa.recruitment_coach_id
              JOIN sport_team st ON st.id = psa.sport_team_id
              
          WHERE uuu.is_active IS TRUE
            AND uuu.is_test_data IS FALSE   
            AND uuu.id NOT IN (66977385, 66811820, 67620535, 66983148, 67431718,67364443)) as psa_base
            
  GROUP BY 1,2,3")

recruit_detail <- dbGetQuery(con, "SELECT
        date_trunc('day', psa.created_at) as created_date
        , concat(psa.first_name, ' ',psa.last_name) as name        
        , INITCAP(psa.commitment_status) as commitment_status
        , psa.high_school_graduation_year
        , st.name as sports_team
        , uuu.name as school_name
        , ccc.email as recruitment_coach_email
        , psa.is_added_through_questionnaire
        
        FROM prospective_student_athlete psa
                JOIN university uuu ON uuu.id = psa.university_id
            JOIN coach ccc ON ccc.id = psa.recruitment_coach_id
            JOIN sport_team st ON st.id = psa.sport_team_id
            
        WHERE uuu.is_active IS TRUE
          AND uuu.is_test_data IS FALSE   
          AND uuu.id NOT IN (66977385, 66811820, 67620535, 66983148, 67431718,67364443)          ")          

recruit_tbl <- dbGetQuery(con, "SELECT
  	uni.name as college
  	, uni.id as uni_id 
  	, uni.division as division
  	, st.name as team
  	, psaq.is_public as is_public
  	, (CASE WHEN psaq.is_public is TRUE AND (count(distinct psqr.id) >0) THEN 'Active' ELSE 'Not Active' END) as survey_is_active
  	, count(distinct psqr.id) as submitted_responses
  	, COUNT(CASE WHEN psqr.response_status = 'PENDING' THEN psqr.id END) as pending_questionnaires
  	, COUNT(CASE WHEN psqr.response_status = 'DISMISSED' THEN psqr.id END) as dismissed_questionnaires
  	, COUNT(CASE WHEN psqr.response_status = 'APPROVED' AND psqr.acknowledged_at IS NOT NULL THEN psqr.id END) as approved_questionnaires
  	, to_char(MAX(psqr.submitted_at),'YYYY-MM-DD') as most_recent_submission
  	, to_char(MIN (psqr.submitted_at),'YYYY-MM-DD') as first_submission
  	
  	FROM university_sport_team_map as uni_map 
  		JOIN university uni ON uni.id = uni_map.university_id
  		JOIN sport_team st ON st.id = uni_map.sport_team_id
  		LEFT JOIN psa_questionnaire psaq ON psaq.sport_team_id = st.id AND uni.id = psaq.university_id
  		LEFT JOIN psa_questionnaire_response psqr ON psaq.id = psqr.psa_questionnaire_id
  		
  	WHERE uni.is_active IS TRUE				 
  		AND uni.is_test_data IS FALSE   				 
  		AND uni.id NOT IN (66977385, 66811820, 67620535, 66983148, 67431718,67364443)
  		AND uni.spry_product_plan = 'SPRY_CONNECT'
  		AND uni.division <> 'JUCO_CC'
  	
  	GROUP BY 1,2,3,4,  5
  
  ORDER BY 1 ASC,4 ASC	")          


recruit_form <- dbGetQuery(con, "select date_trunc('day',rf.created_at) as created_date
	, rf.id as id
	, rf.created_by_user_role as creator_role
	, rf.status
	, u.name as school
	, st.name as team
	, is_cara_events_approval_on
from recruiting_form rf 
join recruiting_form_batch rfb
	on rf.recruiting_form_batch_id = rfb.id
join university u 
	on rfb.university_id = u.id
	and u.is_active IS TRUE
            AND u.is_test_data IS FALSE   
            AND u.id NOT IN (66977385, 66811820, 67620535, 66983148, 67431718,67364443)
join sport_team st 
	on rfb.sport_team_id = st.id ")          
recruit_form[which(recruit_form$creator_role == "COACH"),]$creator_role <- 'Coach'

cara_on <- dbGetQuery(con, "select school
  , division
  , automated_survey_settings 
  , count(distinct team_name)
  , count(distinct cara_log_id) as total_logs
  from (SELECT
   uni.name as school
  , initcap(uni.division) as division
  , st.name as team_name
  , cls.cara_survey_settings_status as automated_survey_settings
  -- , st.id
  , cl.id as cara_log_id
  , initcap(cls.cara_logs_generation_frequency) as cara_logs_frequency
  , cl.period_start_date as cara_period_start_date
  , cl.period_end_date as cara_period_end_date
  , cl.submitted_at as cara_logs_submitted
  , submitters.email as submitted_by_email_address
  , submitters.first_name as submitters_first_name
  , submitters.last_name as submitters_last_name
  , initcap(cl.cara_logs_status) as cara_log_status
  , cl.approved_or_rejected_at as approval_rejection_date
  , approvers.email as approved_by_admin_email
  , approvers.first_name as approvers_first_name
  , approvers.last_name as approvers_last_name
  , COUNT(DISTINCT surveys.user_id) as total_SAs_to_receive_survey
  , COUNT(DISTINCT CASE WHEN surveys.status = 'CONFIRMED' AND surveys.completed_at IS NOT NULL THEN surveys.user_id END) as total_SAs_to_complete
  , COUNT(DISTINCT CASE WHEN surveys.status = 'CONFIRMED' AND surveys.completed_at IS NOT NULL AND surveys.comment IS NOT NULL THEN surveys.user_id END) as total_SAs_to_comment
  
   
  FROM cara_logs_settings cls
  	LEFT JOIN sport_team st ON st.id = cls.sport_team_id
  	LEFT JOIN cara_logs cl on cl.cara_logs_settings_id = cls.id 
   JOIN university uni ON uni.id = cls.university_id
   	LEFT JOIN users submitters ON submitters.id = cl.submitted_by
   	LEFT JOIN users approvers ON approvers.id = cl.approved_or_rejected_by
   	LEFT JOIN cara_survey surveys ON surveys.cara_logs_id = cl.id
  	
  -- WHERE cls.university_id = 67530432
  GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, 16, 17
  ORDER BY 1, 3 ASC, 6 ASC) as base
  group by 1,2,3")

paperwork[which(paperwork$created_by_user_role=='STUDENT_ATHLETE'),]$created_by_user_role <- "SA"
paperwork[which(paperwork$created_by_user_role=='STUDENT_ADMIN'),]$created_by_user_role <- "Admin"
paperwork[which(paperwork$created_by_user_role=='COACH'),]$created_by_user_role <- "Coach"
paperwork_by_role <- paperwork %>% group_by(created_date,created_by_user_role) %>%
  gather(key = "status", value = "counts", created:rejected)
colnames(recruit_detail) <- c("Created Date","Recruit Name", "Commitment Status", 
                              "High School Graduation Year", "Sports Team",
                              "School Name", "Recruitment Coach Email",
                              "Added From Questionnaire")
colnames(cara_on) <- c("School Name", "Division", "Is Cara On", "Number of Teams",
                       "Total Logs")

# Build KPI Summary
ui <- dashboardPage(
  dashboardHeader(title = "Growth Summary"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Growth Summary", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("By School", icon = icon("th"), tabName = "by_school",
               badgeLabel = "new", badgeColor = "navy")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab: Growth Overall 
      tabItem(tabName = "dashboard",
              h2("Overall Growth Dashboard"),
              sidebarPanel(
                dateRangeInput(
                  inputId = "date_range",
                  label = "Select date range",
                  start = as.Date('2023/01/01'), 
                  end = Sys.Date() + 2 ,
                  min = as.Date('2022/01/01'), 
                  max = Sys.Date() + 2
                ),
                selectInput(
                  inputId = "role",
                  label = "Select role",
                  choices = c("SA", "Coach","Admin"),
                ),
                selectInput(
                  inputId = "school", 
                  label = "Select school", 
                  choices = unique(recruit_tbl$college)),
                selectInput(
                  inputId = "sport", 
                  label = "Select sport", 
                  choices = unique(recruit_tbl$team)),
                selectInput(
                  inputId = "grad_year", 
                  label = "Select Recruit High School Graduation Year", 
                  choices = unique(recruit_detail$`High School Graduation Year`)),
              ),
              dataTableOutput("recruiting_sum"),
              dataTableOutput("recruits_total"),
              dataTableOutput("paperwork_sum"),
              dataTableOutput("recruit_form_sum"),
              mainPanel(
                plotOutput("paperwork_role"),
                plotOutput("recruiting_total"),
                plotOutput("questionnaire_total"),
              ),
              
              dataTableOutput("recruiting_detail"),
              dataTableOutput("recruiting_tbl"),
              dataTableOutput("cara_on"),
              
              
      )
      
      # Second tab: By School 
      # tabItem(tabName = "by_school",
      #         h2("Metrics by School"),
      #         sidebarPanel(
      #           selectInput(
      #             inputId = "school", 
      #             label = "Select school", 
      #             choices = unique(logins_agg_by_role$name)),
      #         ),
      #         mainPanel(
      #           # plotOutput("logins"),
      #           # plotOutput("users"),
      #           # plotOutput("users_role"),
      #           # plotOutput("logins_role"),
      #           plotOutput("login_agg_school")
      #         )
      # )
    ),
  ))

server <- function(input, output) {
  paperwork_role_data <- reactive({
    chart_nts <- paperwork_by_role %>%
      dplyr::filter(created_by_user_role == input$role) %>%
      group_by(created_date, status) %>%
      dplyr::summarise(counts = sum(counts))
    data.frame(chart_nts)
  })
  
  recruiting_data <- reactive({
    chart_nts <- recruit %>%
      dplyr::filter(school_name == input$school & sports_team == input$sport) %>%
      group_by(created_date) %>%
      dplyr::summarise(total_recruits = sum(total_recruits), 
                       total_questionnaire = sum(psa_added_thru_questionnaire))
    data.frame(chart_nts)
  })
  
  recruit_detail_data <- reactive({
    chart_nts <- recruit_detail %>%
      dplyr::filter(`School Name` == input$school  & `Sports Team` == input$sports_team & `High School Graduation Year` == input$grad_year)
    data.frame(chart_nts)
  })
  
  output$t6 <- renderDT({paperwork_role_data()})
  output$t7 <- renderDT({recruiting_data()})
  output$t8 <- renderDT({recruit_detail_data()})
  
  output$paperwork_role <- renderPlot({
    data <- paperwork_role_data()
    ggplot(data, aes(x=created_date, y=counts, color=status)) +
      geom_line() +
      theme(title =element_text(size=10, face='bold')) +
      labs(title="Status of Created Paper Work (filter by role)",
           x="",y="Counts of Forms")
  })
  
  output$recruiting_total <- renderPlot({
    data <- recruiting_data()
    ggplot(data, aes(x=created_date, y=total_recruits)) +
      geom_line() +
      theme(title =element_text(size=10, face='bold')) +
      labs(title="Total Recruits",x="",y="total")
  })
  
  output$questionnaire_total <- renderPlot({
    data <- recruiting_data()
    ggplot(data, aes(x=created_date, y=total_questionnaire)) +
      geom_line() +
      theme(title =element_text(size=10, face='bold')) +
      labs(title="Total Questionnaires",x="",y="total")
  })
  
  output$recruiting_detail <- renderDataTable({recruit_detail})
  
  output$recruiting_tbl <- renderDataTable({recruit_tbl})
  
  output$recruiting_sum <- renderDataTable({
    recruit_tbl %>%
      dplyr::filter(college == input$school) %>%
      group_by(team, college) %>%
      dplyr::summarise(Submitted_Responses = sum(submitted_responses), 
                       Pending_Questionnaires = sum(pending_questionnaires),
                       Dismissed_Questionnaires = sum(dismissed_questionnaires))
    
  })
  
  output$paperwork_sum <- renderDataTable({
    recruit_form %>%
      dplyr::filter(school == input$school & team == input$sport) %>%
      group_by(status) %>%
      dplyr::summarise(`Total Forms` = n_distinct(id))
  })
  
  output$recruit_form_sum <- renderDataTable({
    paperwork %>%
      dplyr::filter(created_by_user_role == input$role) %>%
      dplyr::summarise(total_completed = sum(completed),
                       total_approved = sum(approved),
                       total_viewed = sum(viewed), 
                       total_created = sum(created), 
                       total_not_started = sum(not_started),
                       total_in_progress = sum(in_progress),
                       total_rejected = sum(rejected))
  })
  
  output$recruits_total <- renderDataTable({
    recruit %>%
      dplyr::filter(school_name == input$school) %>%
      group_by(sports_team) %>%
      dplyr::summarise(`Total Recruits` = sum(total_recruits),
                       `Total PSA Through Questionnaire` = sum(psa_added_thru_questionnaire))
    
  })
  
  output$cara_on <- renderDataTable({cara_on})
  
}

shinyApp(ui, server)
