server <- function(input, output, session) {
  print("hello")
  #for demotest
  result <- getData(1)
  output$qno <- renderText("QUESTION:1")
  output$out <- renderUI(result[1])
  output$option1 <- renderUI(result[2])
  output$option2 <- renderUI(result[3])
  output$option3 <- renderUI(result[4])
  output$option4 <- renderUI(result[5])
  
  value <- reactiveVal(1)
  ans <- reactiveVal(result[6])

  #for demotest result
  
  
  x <- reactiveVal(0)
  y <-  reactiveVal(0)
  i <- reactiveVal(0)
  day <-reactiveVal(1)
  
  #----------------------------------
  #----------------------------------
  
  #intializations for mocktest1
  m1_result <- getData_mocktest1(1)
  output$m1_qno <- renderText("QUESTION:1")
  output$m1_out <- renderUI(m1_result[1])
  output$m1_option1 <- renderUI(m1_result[2])
  output$m1_option2 <- renderUI(m1_result[3])
  output$m1_option3 <- renderUI(m1_result[4])
  output$m1_option4 <- renderUI(m1_result[5])
  m1_ans <- reactiveVal(m1_result[6])
  
  m1_value <- reactiveVal(1)
  m1_value1 <- reactiveVal(1)
  m1_value2 <- reactiveVal(1)
  m1_value3 <- reactiveVal(1)
  m1_value4 <- reactiveVal(1)
  m1_value5 <- reactiveVal(1)
  m1_value6 <- reactiveVal(1)
  m1_database_used <- reactiveVal(1)
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #intializations for mocktest2
  m2_result <- getData_mocktest2(1)
  output$m2_qno <- renderText("QUESTION:1")
  output$m2_out <- renderUI(m2_result[1])
  output$m2_option1 <- renderUI(m2_result[2])
  output$m2_option2 <- renderUI(m2_result[3])
  output$m2_option3 <- renderUI(m2_result[4])
  output$m2_option4 <- renderUI(m2_result[5])
  m2_ans <- reactiveVal(m2_result[6])
  
  m2_value <- reactiveVal(1)
  m2_value1 <- reactiveVal(1)
  m2_value2 <- reactiveVal(1)
  m2_value3 <- reactiveVal(1)
  m2_value4 <- reactiveVal(1)
  m2_value5 <- reactiveVal(1)
  m2_value6 <- reactiveVal(1)
  m2_database_used <- reactiveVal(1)
  
  #----------------------------------
  #----------------------------------
  #++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #intializations for mocktest3
  m3_result <- getData_mocktest3(1)
  output$m3_qno <- renderText("QUESTION:1")
  output$m3_out <- renderUI(m3_result[1])
  output$m3_option1 <- renderUI(m3_result[2])
  output$m3_option2 <- renderUI(m3_result[3])
  output$m3_option3 <- renderUI(m3_result[4])
  output$m3_option4 <- renderUI(m3_result[5])
  m3_ans <- reactiveVal(m3_result[6])
  
  m3_value <- reactiveVal(1)
  m3_value1 <- reactiveVal(1)
  m3_value2 <- reactiveVal(1)
  m3_value3 <- reactiveVal(1)
  m3_value4 <- reactiveVal(1)
  m3_value5 <- reactiveVal(1)
  m3_value6 <- reactiveVal(1)
  m3_database_used <- reactiveVal(1)
  
  #----------------------------------
  #----------------------------------
  observeEvent(input$login_btn,{
    myMail <- input$login_mail
    myPassword <- input$login_password
    
    print(myPassword)
    data <- c(myMail,myPassword)
    
    print(data)
    a <- check_login_details(data)
    print(a)
    if(a==1){
      #login verification done succesfully
      insert_data_into_prsnt_table(myMail)
      print("a=1")
      output$logged_in_succesfully_msg <- renderText("YOU HAVE LOGGED IN SUCCESFULLY")
      output$logged_in_succesfully_demotest <- renderText("CLICK IF LOGGED IN SUCCESFULLY")
      output$logged_in_succesfully_studentpage <- renderText("CLICK TO NAVIGATE TO STUDENT PROFILE")
    }
    else if(a==2)
    {
      #new user
      output$logged_in_succesfully_msg <- renderText("Please Register first and then login")
    }
    else{
      #there is no user with that mailid send notification that
      output$logged_in_succesfully_msg <- renderText("You have entered wrong password")
    }
  })
  #----------------------------------
  #----------------------------------
  observeEvent(input$reg_btn,{
    myName <- input$reg_name
    myMail <- input$reg_mail
    myPswd <- input$reg_password
    myAge <- input$reg_age
    print(input$reg_gender)
    if(input$reg_gender=="1")
    {myGender<-"Male"}
    else
    {myGender<-"Female"}
    myDate<-paste(input$reg_day1,"-",input$reg_month1,"-",input$reg_year1)
    print(myDate)
    data <- c(myName,myMail,myPswd,myAge,myGender,myDate)
    
    print(data)
    
    insert_reg_data(data)
    print("inserted data in reg table")
    insert_intial_std_model_data(myMail)
  })
  
  observeEvent(input$Prev_btn, {
    x <- input$radio_ans
    y <- as.integer(x)
    print(y)
    print("hello")
    print(ans())
    if( y == ans()){
      resp_saving(value(),y,1)
      q_no_update_query <- sprintf("update answer_storage set q_no = %s, database = %s,result=%s where id = %s",1,1,1,value())
    }
    else{
      resp_saving(value(),y,0)
      q_no_update_query <- sprintf("update answer_storage set q_no = %s, database = %s,result=%s where id = %s",1,1,0,value())
    }
    dempster_shafer(value())
    newValue <- value() - 1
    value(newValue)
    result <- getData(value())
    output$qno <- renderUI(paste("QUESTION:",value()))
    output$out <- renderUI(result[1])
    output$option1 <- renderUI(result[2])
    output$option2 <- renderUI(result[3])
    output$option3 <- renderUI(result[4])
    output$option4 <- renderUI(result[5])
    new_result <- result[6]
    ans(new_result)
  })
  
  observeEvent(input$Next_btn, {
    x <- input$radio_ans
    y <- as.integer(x)
    print(y)
    #if y == 0 mark that questiono 
    print("hello")
    print(ans())
    if( y == ans()){
      resp_saving(value(),y,1)
      q_no_update_query <- sprintf("update answer_storage set q_no = %s, database = %s,result=%s where id = %s",1,1,1,value())
    }
    else{
      resp_saving(value(),y,0)
      q_no_update_query <- sprintf("update answer_storage set q_no = %s, database = %s,result=%s where id = %s",1,1,0,value())
    }
    dbSendQuery(db,q_no_update_query)
    
    dempster_shafer(value())
    newValue <- value() + 1     
    value(newValue)
    if(value()==10){
      output$demo_result <- renderText("You are done with your test click to continue")
    }
    else{
      
      result <- getData(value())
      output$qno <- renderUI(paste("QUESTION:",value()))
      output$out <- renderUI(result[1])
      output$option1 <- renderUI(result[2])
      output$option2 <- renderUI(result[3])
      output$option3 <- renderUI(result[4])
      output$option4 <- renderUI(result[5])
      new_result <- result[6]
      print("new_res is ")
      print(new_result)
      ans(new_result)
      print("new ans is")
      print(ans)
    }
  })
  
  observeEvent(input$m3_Next_btn,{
    x <- input$m3_radio_ans
    y <- as.integer(x)
    print("hello")
    print(m3_ans())
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("DATABASE USED WAS:")
    print(m3_database_used())
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print(dbReadTable(db,"answer_storage"))
    if(m3_database_used()== 1){
      print("inside 1 if")
      if(y == m3_ans()){
        print(m3_value1())
        print(y)
        print(m3_database_used())
        m3_resp_saving(m3_value1(),y,1,m3_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no = %s, database = %s,result=%s where id = %s",m3_value1(),1,1,m3_value())
      }
      else{
        m3_resp_saving(m3_value1(),y,0,m3_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no = %s, database = %s,result=%s where id = %s",m3_value1(),1,0,m3_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m3_dempster_shafer(m3_value1(),m3_database_used())
      m3_newValue1 <- m3_value1() + 1     
      m3_value1(m3_newValue1)
    }
    else if(m3_database_used() == 2){
      print("inside 2 if")
      if(y == m3_ans()){
        m3_resp_saving(m3_value2(),y,1,m3_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m3_value2(),2,1,m3_value())
      }
      else{
        m3_resp_saving(m3_value2(),y,0,m3_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m3_value2(),2,0,m3_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m3_dempster_shafer(m3_value2(),m3_database_used())
      m3_newValue2 <- m3_value2() + 1     
      m3_value2(m3_newValue2)
    }
    else if(m3_database_used() == 3){
      print("inside 3 if")
      if(y == m3_ans()){
        m3_resp_saving(m3_value3(),y,1,m3_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m3_value3(),3,1,m3_value())
      }
      else{
        m3_resp_saving(m3_value3(),y,0,m3_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m3_value3(),3,0,m3_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m3_dempster_shafer(m3_value3(),m3_database_used())
      m3_newValue3 <- m3_value3() + 1     
      m3_value3(m3_newValue3)
    }
    else if(m3_database_used() == 4){
      print("inside 4 if")
      if(y == m3_ans()){
        m3_resp_saving(m3_value4(),y,1,m3_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m3_value4(),4,1,m3_value())
      }
      else{
        m3_resp_saving(m3_value4(),y,0,m3_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m3_value4(),4,0,m3_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m3_dempster_shafer(m3_value4(),m3_database_used())
      m3_newValue4 <- m3_value4() + 1     
      m3_value4(m3_newValue4)
    }
    else if(m3_database_used() == 5){
      print("inside 5 if")
      if(y == m3_ans()){
        m3_resp_saving(m3_value5(),y,1,m3_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m3_value5(),5,1,m3_value())
      }
      else{
        m3_resp_saving(m3_value5(),y,0,m3_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m3_value5(),5,0,m3_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m3_dempster_shafer(m3_value5(),m3_database_used())
      m3_newValue5 <- m3_value5() + 1     
      m3_value5(m3_newValue5)
    }
    else if(m3_database_used() == 6){
      print("inside 6 if")
      if(y == m3_ans()){
        m3_resp_saving(m3_value6(),y,1,m3_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m3_value6(),6,1,m3_value())
      }
      else{
        m3_resp_saving(m3_value6(),y,0,m3_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m3_value6(),6,0,m3_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m3_dempster_shafer(m3_value6(),m3_database_used())
      m3_newValue6 <- m3_value6() + 1     
      m3_value6(m3_newValue6)
    }
    print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
    
    m3_newValue <- m3_value()+1
    m3_value(m3_newValue)
    
    levels_data <- get_levels()
    print(levels_data)
    
    next_level <- get_next_level(levels_data)
    print("###############################################################")
    print(next_level)
    
    m3_database_used((next_level))
    
    print("PRINTING NEW VALUES:")
    print(m3_value1())
    print(m3_value2())
    print(m3_value3())
    print(m3_value4())
    print(m3_value5())
    print(m3_value6())
    if(m3_value()==11){
      output$mock_result3 <- renderText("You are done with your test click to continue")
    }
    else{
      if(m3_database_used()==1){
        print("1 database used")
        m3_result <- m3_getData(m3_value1(),m3_database_used())
      }
      if(m3_database_used()==2){
        print("2 database used")
        m3_result <- m3_getData(m3_value2(),m3_database_used())
      }
      if(m3_database_used()==3){
        print("3 database used")
        m3_result <- m3_getData(m3_value3(),m3_database_used())
      }
      if(m3_database_used()==4){
        print("4 database used")
        m3_result <- m3_getData(m3_value4(),m3_database_used())
      }
      if(m3_database_used()==5){
        print("5 database used")
        m3_result <- m3_getData(m3_value5(),m3_database_used())
      }
      if(m3_database_used()==6){
        print("6 database used")
        m3_result <- m3_getData(m3_value6(),m3_database_used())
      }
      print("hello")
      print(m3_result[6])
      output$m3_qno <- renderUI(paste("QUESTION:",m3_value()))
      output$m3_out <- renderUI(m3_result[1])
      output$m3_option1 <- renderUI(m3_result[2])
      output$m3_option2 <- renderUI(m3_result[3])
      output$m3_option3 <- renderUI(m3_result[4])
      output$m3_option4 <- renderUI(m3_result[5])
      m2_ans(m3_result[6])
      print("new ans is")
      print(m3_ans)
    }
  })
  
  observeEvent(input$m2_Next_btn,{
    x <- input$m2_radio_ans
    y <- as.integer(x)
    print("hello")
    print(m2_ans())
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("DATABASE USED WAS:")
    print(m2_database_used())
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print(dbReadTable(db,"answer_storage"))
    if(m2_database_used()== 1){
      print("inside 1 if")
      if(y == m2_ans()){
        print(m2_value1())
        print(y)
        print(m2_database_used())
        m2_resp_saving(m2_value1(),y,1,m2_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no = %s, database = %s,result=%s where id = %s",m2_value1(),1,1,m2_value())
      }
      else{
        m2_resp_saving(m2_value1(),y,0,m2_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no = %s, database = %s,result=%s where id = %s",m2_value1(),1,0,m2_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m2_dempster_shafer(m2_value1(),m2_database_used())
      m2_newValue1 <- m2_value1() + 1     
      m2_value1(m2_newValue1)
    }
    else if(m2_database_used() == 2){
      print("inside 2 if")
      if(y == m2_ans()){
        m2_resp_saving(m2_value2(),y,1,m2_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m2_value2(),2,1,m2_value())
      }
      else{
        m2_resp_saving(m2_value2(),y,0,m2_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m2_value2(),2,0,m2_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m2_dempster_shafer(m2_value2(),m2_database_used())
      m2_newValue2 <- m2_value2() + 1     
      m2_value2(m2_newValue2)
    }
    else if(m2_database_used() == 3){
      print("inside 3 if")
      if(y == m2_ans()){
        m2_resp_saving(m2_value3(),y,1,m2_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m2_value3(),3,1,m2_value())
      }
      else{
        m2_resp_saving(m2_value3(),y,0,m2_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m2_value3(),3,0,m2_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m2_dempster_shafer(m2_value3(),m2_database_used())
      m2_newValue3 <- m2_value3() + 1     
      m2_value3(m2_newValue3)
    }
    else if(m2_database_used() == 4){
      print("inside 4 if")
      if(y == m2_ans()){
        m2_resp_saving(m2_value4(),y,1,m2_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m2_value4(),4,1,m2_value())
      }
      else{
        m2_resp_saving(m2_value4(),y,0,m2_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m2_value4(),4,0,m2_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m2_dempster_shafer(m2_value4(),m2_database_used())
      m2_newValue4 <- m2_value4() + 1     
      m2_value4(m2_newValue4)
    }
    else if(m2_database_used() == 5){
      print("inside 5 if")
      if(y == m2_ans()){
        m2_resp_saving(m2_value5(),y,1,m2_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m2_value5(),5,1,m2_value())
      }
      else{
        m2_resp_saving(m2_value5(),y,0,m2_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m2_value5(),5,0,m2_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m2_dempster_shafer(m2_value5(),m2_database_used())
      m2_newValue5 <- m2_value5() + 1     
      m2_value5(m2_newValue5)
    }
    else if(m2_database_used() == 6){
      print("inside 6 if")
      if(y == m2_ans()){
        m3_resp_saving(m2_value6(),y,1,m2_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m2_value6(),6,1,m2_value())
      }
      else{
        m2_resp_saving(m3_value6(),y,0,m2_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m2_value6(),6,0,m2_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m2_dempster_shafer(m2_value6(),m2_database_used())
      m2_newValue6 <- m2_value6() + 1     
      m2_value6(m2_newValue6)
    }
    print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
    
    m2_newValue <- m2_value()+1
    m2_value(m2_newValue)
    
    levels_data <- get_levels()
    print(levels_data)
    
    next_level <- get_next_level(levels_data)
    print("###############################################################")
    print(next_level)
    
    m2_database_used((next_level))
    
    print("PRINTING NEW VALUES:")
    print(m2_value1())
    print(m2_value2())
    print(m2_value3())
    print(m2_value4())
    print(m2_value5())
    print(m2_value6())
    if(m2_value()==11){
      output$mock_result2 <- renderText("You are done with your test click to continue")
    }
    else{
      if(m2_database_used()==1){
        print("1 database used")
        m2_result <- m2_getData(m2_value1(),m2_database_used())
      }
      if(m2_database_used()==2){
        print("2 database used")
        m2_result <- m2_getData(m2_value2(),m2_database_used())
      }
      if(m2_database_used()==3){
        print("3 database used")
        m2_result <- m2_getData(m2_value3(),m2_database_used())
      }
      if(m2_database_used()==4){
        print("4 database used")
        m2_result <- m2_getData(m2_value4(),m2_database_used())
      }
      if(m2_database_used()==5){
        print("5 database used")
        m2_result <- m3_getData(m2_value5(),m2_database_used())
      }
      if(m2_database_used()==6){
        print("6 database used")
        m2_result <- m2_getData(m2_value6(),m2_database_used())
      }
      print("hello")
      print(m2_result[6])
      output$m2_qno <- renderUI(paste("QUESTION:",m2_value()))
      output$m2_out <- renderUI(m2_result[1])
      output$m2_option1 <- renderUI(m2_result[2])
      output$m2_option2 <- renderUI(m2_result[3])
      output$m2_option3 <- renderUI(m2_result[4])
      output$m2_option4 <- renderUI(m2_result[5])
      m2_ans(m2_result[6])
      print("new ans is")
      print(m2_ans)
    }
  })
  
  
  observeEvent(input$m_Next_btn,{
    x <- input$m_radio_ans
    y <- as.integer(x)
    print("hello")
    print(m1_ans())
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("DATABASE USED WAS:")
    print(m1_database_used())
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print(dbReadTable(db,"answer_storage"))
    if(m1_database_used()== 1){
      print("inside 1 if")
      if(y == m1_ans()){
        m_resp_saving(m1_value1(),y,1,m1_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no = %s, database = %s,result=%s where id = %s",m1_value1(),1,1,m1_value())
      }
      else{
        m_resp_saving(m1_value1(),y,0,m1_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no = %s, database = %s,result=%s where id = %s",m1_value1(),1,0,m1_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m_dempster_shafer(m1_value1(),m1_database_used())
      m1_newValue1 <- m1_value1() + 1     
      m1_value1(m1_newValue1)
    }
    else if(m1_database_used() == 2){
      print("inside 2 if")
      if(y == m1_ans()){
        m_resp_saving(m1_value2(),y,1,m1_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m1_value2(),2,1,m1_value())
      }
      else{
        m_resp_saving(m1_value2(),y,0,m1_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m1_value2(),2,0,m1_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m_dempster_shafer(m1_value2(),m1_database_used())
      m1_newValue2 <- m1_value2() + 1     
      m1_value2(m1_newValue2)
    }
    else if(m1_database_used() == 3){
      print("inside 3 if")
      if(y == m1_ans()){
        m_resp_saving(m1_value3(),y,1,m1_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m1_value3(),3,1,m1_value())
      }
      else{
        m_resp_saving(m1_value3(),y,0,m1_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m1_value3(),3,0,m1_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m_dempster_shafer(m1_value3(),m1_database_used())
      m1_newValue3 <- m1_value3() + 1     
      m1_value3(m1_newValue3)
    }
    else if(m1_database_used() == 4){
      print("inside 4 if")
      if(y == m1_ans()){
        m_resp_saving(m1_value4(),y,1,m1_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m1_value4(),4,1,m1_value())
      }
      else{
        m_resp_saving(m1_value4(),y,0,m1_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m1_value4(),4,0,m1_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m_dempster_shafer(m1_value4(),m1_database_used())
      m1_newValue4 <- m1_value4() + 1     
      m1_value4(m1_newValue4)
    }
    else if(m1_database_used() == 5){
      print("inside 5 if")
      if(y == m1_ans()){
        m_resp_saving(m1_value5(),y,1,m1_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m1_value5(),5,1,m1_value())
      }
      else{
        m_resp_saving(m1_value5(),y,0,m1_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s , database = %s,result=%s where id = %s",m1_value5(),5,0,m1_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m_dempster_shafer(m1_value5(),m1_database_used())
      m1_newValue5 <- m1_value5() + 1     
      m1_value5(m1_newValue5)
    }
    else if(m1_database_used() == 6){
      print("inside 6 if")
      if(y == m1_ans()){
        m_resp_saving(m1_value6(),y,1,m1_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m1_value6(),6,1,m1_value())
      }
      else{
        m_resp_saving(m1_value6(),y,0,m1_database_used())
        q_no_update_query <- sprintf("update answer_storage set q_no =%s, database = %s,result=%s where id = %s",m1_value6(),6,0,m1_value())
      }
      print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      print(q_no_update_query)
      dbSendQuery(db,q_no_update_query)
      m_dempster_shafer(m1_value6(),m1_database_used())
      m1_newValue6 <- m1_value6() + 1     
      m1_value6(m1_newValue6)
    }
    print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
    
    m1_newValue <- m1_value()+1
    m1_value(m1_newValue)
    
    levels_data <- get_levels()
    print(levels_data)
    
    next_level <- get_next_level(levels_data)
    print("###############################################################")
    print(next_level)
    
    m1_database_used((next_level))
    
    print("PRINTING NEW VALUES:")
    print(m1_value1())
    print(m1_value2())
    print(m1_value3())
    print(m1_value4())
    print(m1_value5())
    print(m1_value6())
    if(m1_value()==11){
      output$mock_result <- renderText("You are done with your test click to continue")
    }
    else{
      if(m1_database_used()==1){
        print("1 database used")
        m1_result <- m_getData(m1_value1(),m1_database_used())
      }
      if(m1_database_used()==2){
        print("2 database used")
        m1_result <- m_getData(m1_value2(),m1_database_used())
      }
      if(m1_database_used()==3){
        print("3 database used")
        m1_result <- m_getData(m1_value3(),m1_database_used())
      }
      if(m1_database_used()==4){
        print("4 database used")
        m1_result <- m_getData(m1_value4(),m1_database_used())
      }
      if(m1_database_used()==5){
        print("5 database used")
        m1_result <- m_getData(m1_value5(),m1_database_used())
      }
      if(m1_database_used()==6){
        print("6 database used")
        m1_result <- m_getData(m1_value6(),m1_database_used())
      }
      print("hello")
      print(m1_result[6])
      output$m1_qno <- renderUI(paste("QUESTION:",m1_value()))
      output$m1_out <- renderUI(m1_result[1])
      output$m1_option1 <- renderUI(m1_result[2])
      output$m1_option2 <- renderUI(m1_result[3])
      output$m1_option3 <- renderUI(m1_result[4])
      output$m1_option4 <- renderUI(m1_result[5])
      m1_ans(m1_result[6])
      print("new ans is")
      print(m1_ans)
    }
    
  })
  
  observeEvent(input$demo_btn,{
    print("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS")
    attempted <- dbGetQuery(db,"select count(*) from answer_storage where result != -1")
    print(attempted)
    output$m_answers_attempted <- renderUI(paste0("Number of answers attempted : ",as.character(attempted)))
    wrong <- dbGetQuery(db,"select count(*) from answer_storage where result = 0")
    print(wrong)
    output$m_wrong_attempted <- renderUI(paste0("Number of wrong answers attempted : ",as.character(wrong)))
    
    right <- dbGetQuery(db,"select count(*) from answer_storage where result = 1")
    print(right)
    output$m_correct_attempted <- renderUI(paste0("Number of correct answers attempted : ",as.character(right)))
    
    m_r1 <- dbGetQuery(db,"select result from answer_storage where id = 1")
    m_query1 <- sprintf("select question from demotest_questions where id = 1")
    print(m_query1)
    res1 <- dbGetQuery(db,m_query1)
    
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print("bhargav1")
    print(m_r1)
    print(res1)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$d_q1 <- renderUI(as.character(res1))
    if(m_r1==1){
      output$d_a1 <- renderUI("Attempted correctly")
    }
    else if(m_r1 == 0){
      output$d_a1 <- renderUI("Attempted wrong")
    }
    else{
      output$d_a1 <- renderUI("Unattempted")
    }
    
    #-------------------------------------------------------------------
    

    m_r2 <- dbGetQuery(db,"select result from answer_storage where id = 2")
    m_query2 <- sprintf("select question from demotest_questions where id = 2")
    res2 <- dbGetQuery(db,m_query2)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_r2)
    print(res2)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$d_q2 <- renderUI(as.character(res2))
    if(m_r2==1){
      output$d_a2 <- renderUI("Attempted correctly")
    }
    else if(m_r2 == 0){
      output$d_a2 <- renderUI("Attempted wrong")
    }
    else{
      output$d_a2 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    

    m_r3 <- dbGetQuery(db,"select result from answer_storage where id = 3")
    m_query3 <- sprintf("select question from demotest_questions where id = 3")
    res3 <- dbGetQuery(db,m_query3)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_r3)
    print(res3)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$d_q3 <- renderUI(as.character(res3))
    if(m_r3==1){
      output$d_a3 <- renderUI("Attempted correctly")
    }
    else if(m_r3 == 0){
      output$d_a3 <- renderUI("Attempted wrong")
    }
    else{
      output$d_a3 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_r4 <- dbGetQuery(db,"select result from answer_storage where id = 4")
    m_query4 <- sprintf("select question from demotest_questions where id = 4")
    res4 <- dbGetQuery(db,m_query4)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_r4)
    print(res4)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$d_q4 <- renderUI(as.character(res4))
    if(m_r4==1){
      output$d_a4 <- renderUI("Attempted correctly")
    }
    else if(m_r4 == 0){
      output$d_a4 <- renderUI("Attempted wrong")
    }
    else{
      output$d_a4 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_r5 <- dbGetQuery(db,"select result from answer_storage where id = 5")
    m_query5 <- sprintf("select question from demotest_questions where id = 5")
    res5 <- dbGetQuery(db,m_query5)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_r5)
    print(res5)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$d_q5 <- renderUI(as.character(res5))
    if(m_r5==1){
      output$d_a5 <- renderUI("Attempted correctly")
    }
    else if(m_r5 == 0){
      output$d_a5 <- renderUI("Attempted wrong")
    }
    else{
      output$d_a5 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_r6 <- dbGetQuery(db,"select result from answer_storage where id = 6")
    m_query6 <- sprintf("select question from demotest_questions where id = 6")
    res6 <- dbGetQuery(db,m_query6)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_r6)
    print(res6)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$d_q6 <- renderUI(as.character(res6))
    if(m_r6==1){
      output$d_a1 <- renderUI("Attempted correctly")
    }
    else if(m_r6 == 0){
      output$d_a6 <- renderUI("Attempted wrong")
    }
    else{
      output$d_a6 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------

    m_r7 <- dbGetQuery(db,"select result from answer_storage where id = 7")
    m_query7 <- sprintf("select question from demotest_questions where id = 7")
    res7 <- dbGetQuery(db,m_query7)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_r7)
    print(res7)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$d_q7 <- renderUI(as.character(res7))
    if(m_r7==1){
      output$d_a7 <- renderUI("Attempted correctly")
    }
    else if(m_r7 == 0){
      output$d_a7 <- renderUI("Attempted wrong")
    }
    else{
      output$d_a7 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------

    m_r8 <- dbGetQuery(db,"select result from answer_storage where id = 8")
    m_query8 <- sprintf("select question from demotest_questions where id = 8")
    res8 <- dbGetQuery(db,m_query8)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_r8)
    print(res8)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$d_q8 <- renderUI(as.character(res8))
    if(m_r8==1){
      output$d_a8 <- renderUI("Attempted correctly")
    }
    else if(m_r8 == 0){
      output$d_a8 <- renderUI("Attempted wrong")
    }
    else{
      output$d_a8 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_r9 <- dbGetQuery(db,"select result from answer_storage where id = 9")
    m_query9 <- sprintf("select question from demotest_questions where id = 9")
    res9 <- dbGetQuery(db,m_query9)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_r9)
    print(res9)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$d_q9 <- renderUI(as.character(res9))
    if(m_r9==1){
      output$d_a9 <- renderUI("Attempted correctly")
    }
    else if(m_r9 == 0){
      output$d_a9 <- renderUI("Attempted wrong")
    }
    else{
      output$d_a9 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_r10 <- dbGetQuery(db,"select result from answer_storage where id = 10")
    m_query10 <- sprintf("select question from demotest_questions where id = 10")
    res10 <- dbGetQuery(db,m_query10)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_r10)
    print(res10)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$d_q10 <- renderUI(as.character(res10))
    if(m_r10==1){
      output$d_a10 <- renderUI("Attempted correctly")
    }
    else if(m_r10 == 0){
      output$d_a10 <- renderUI("Attempted wrong")
    }
    else{
      output$d_a10 <- renderUI("Unattempted")
    } 
    #-------------------------------------------------------------------
    
    h <- get_plot_data()
    output$demo_graph <- renderPlot(barplot(h,xlab = "Topics",ylab="student_level",names = c("ratio","probability","geometry","pNc","aptitude"),col = c("yellow","red","blue","pink","green")))
    
    
    
  })
  
  observeEvent(input$mock_btn,{
    #answers attempted 
    print("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS")
    attempted <- dbGetQuery(db,"select count(*) from answer_storage where result != -1")
    print(attempted)
    output$m_answers_attempted <- renderUI(paste0("Number of answers attempted : ",as.character(attempted)))
    wrong <- dbGetQuery(db,"select count(*) from answer_storage where result = 0")
    print(wrong)
    output$m_wrong_attempted <- renderUI(paste0("Number of wrong answers attempted : ",as.character(wrong)))
    
    right <- dbGetQuery(db,"select count(*) from answer_storage where result = 1")
    print(right)
    output$m_correct_attempted <- renderUI(paste0("Number of correct answers attempted : ",as.character(right)))
    
    #-------------------------------------------------------------------
    
    m_db1 <- dbGetQuery(db,"select database from answer_storage where id = 1")
    m_q1 <- dbGetQuery(db,"select q_no from answer_storage where id = 1")
    m_r1 <- dbGetQuery(db,"select result from answer_storage where id = 1")
    t1 <- paste0("demotest_",m_db1)
    m_query1 <- sprintf("select question from %s where id = %s",t1,m_q1)
    print(m_query1)
    res1 <- dbGetQuery(db,m_query1)
    
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print("bhargav1")
    print(m_db1)
    print(m_q1)
    print(m_r1)
    print(t1)
    print(res1)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr_q1 <- renderUI(as.character(res1))
    if(m_r1==1){
      output$mr_a1 <- renderUI("Attempted correctly")
    }
    else if(m_r1 == 0){
      output$mr_a1 <- renderUI("Attempted wrong")
    }
    else{
      output$mr_a1 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_db2 <- dbGetQuery(db,"select database from answer_storage where id = 2")
    m_q2 <- dbGetQuery(db,"select q_no from answer_storage where id = 2")
    m_r2 <- dbGetQuery(db,"select result from answer_storage where id = 2")
    t2 <- paste0("demotest_",m_db2)
    m_query2 <- sprintf("select question from %s where id = %s",t2,m_q2)
    res2 <- dbGetQuery(db,m_query2)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db2)
    print(m_q2)
    print(m_r2)
    print(t2)
    print(res2)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr_q2 <- renderUI(as.character(res2))
    if(m_r2==1){
      output$mr_a2 <- renderUI("Attempted correctly")
    }
    else if(m_r2 == 0){
      output$mr_a2 <- renderUI("Attempted wrong")
    }
    else{
      output$mr_a2 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_db3 <- dbGetQuery(db,"select database from answer_storage where id = 3")
    m_q3 <- dbGetQuery(db,"select q_no from answer_storage where id = 3")
    m_r3 <- dbGetQuery(db,"select result from answer_storage where id = 3")
    t3 <- paste0("demotest_",m_db3)
    m_query3 <- sprintf("select question from %s where id = %s",t3,m_q3)
    res3 <- dbGetQuery(db,m_query3)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db3)
    print(m_q3)
    print(m_r3)
    print(t3)
    print(res3)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr_q3 <- renderUI(as.character(res3))
    if(m_r3==1){
      output$mr_a3 <- renderUI("Attempted correctly")
    }
    else if(m_r3 == 0){
      output$mr_a3 <- renderUI("Attempted wrong")
    }
    else{
      output$mr_a3 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_db4 <- dbGetQuery(db,"select database from answer_storage where id = 4")
    m_q4 <- dbGetQuery(db,"select q_no from answer_storage where id = 4")
    m_r4 <- dbGetQuery(db,"select result from answer_storage where id = 4")
    t4 <- paste0("demotest_",m_db4)
    m_query4 <- sprintf("select question from %s where id = %s",t4,m_q4)
    res4 <- dbGetQuery(db,m_query4)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db4)
    print(m_q4)
    print(m_r4)
    print(t4)
    print(res4)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr_q4 <- renderUI(as.character(res4))
    if(m_r4==1){
      output$mr_a4 <- renderUI("Attempted correctly")
    }
    else if(m_r4 == 0){
      output$mr_a4 <- renderUI("Attempted wrong")
    }
    else{
      output$mr_a4 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_db5 <- dbGetQuery(db,"select database from answer_storage where id = 5")
    m_q5 <- dbGetQuery(db,"select q_no from answer_storage where id = 5")
    m_r5 <- dbGetQuery(db,"select result from answer_storage where id = 5")
    t5 <- paste0("demotest_",m_db5)
    m_query5 <- sprintf("select question from %s where id = %s",t5,m_q5)
    res5 <- dbGetQuery(db,m_query5)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db5)
    print(m_q5)
    print(m_r5)
    print(t5)
    print(res5)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr_q5 <- renderUI(as.character(res5))
    if(m_r5==1){
      output$mr_a5 <- renderUI("Attempted correctly")
    }
    else if(m_r5 == 0){
      output$mr_a5 <- renderUI("Attempted wrong")
    }
    else{
      output$mr_a5 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db6 <- dbGetQuery(db,"select database from answer_storage where id = 6")
    m_q6 <- dbGetQuery(db,"select q_no from answer_storage where id = 6")
    m_r6 <- dbGetQuery(db,"select result from answer_storage where id = 6")
    t6 <- paste0("demotest_",m_db6)
    m_query6 <- sprintf("select question from %s where id = %s",t6,m_q6)
    res6 <- dbGetQuery(db,m_query6)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db6)
    print(m_q6)
    print(m_r6)
    print(t6)
    print(res6)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr_q6 <- renderUI(as.character(res6))
    if(m_r6==1){
      output$mr_a1 <- renderUI("Attempted correctly")
    }
    else if(m_r6 == 0){
      output$mr_a6 <- renderUI("Attempted wrong")
    }
    else{
      output$mr_a6 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db7 <- dbGetQuery(db,"select database from answer_storage where id = 7")
    m_q7 <- dbGetQuery(db,"select q_no from answer_storage where id = 7")
    m_r7 <- dbGetQuery(db,"select result from answer_storage where id = 7")
    t7 <- paste0("demotest_",m_db7)
    m_query7 <- sprintf("select question from %s where id = %s",t7,m_q7)
    res7 <- dbGetQuery(db,m_query7)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db7)
    print(m_q7)
    print(m_r7)
    print(t7)
    print(res7)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr_q7 <- renderUI(as.character(res7))
    if(m_r7==1){
      output$mr_a7 <- renderUI("Attempted correctly")
    }
    else if(m_r7 == 0){
      output$mr_a7 <- renderUI("Attempted wrong")
    }
    else{
      output$mr_a7 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db8 <- dbGetQuery(db,"select database from answer_storage where id = 8")
    m_q8 <- dbGetQuery(db,"select q_no from answer_storage where id = 8")
    m_r8 <- dbGetQuery(db,"select result from answer_storage where id = 8")
    t8 <- paste0("demotest_",m_db8)
    m_query8 <- sprintf("select question from %s where id = %s",t8,m_q8)
    res8 <- dbGetQuery(db,m_query8)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db8)
    print(m_q8)
    print(m_r8)
    print(t8)
    print(res8)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr_q8 <- renderUI(as.character(res8))
    if(m_r8==1){
      output$mr_a8 <- renderUI("Attempted correctly")
    }
    else if(m_r8 == 0){
      output$mr_a8 <- renderUI("Attempted wrong")
    }
    else{
      output$mr_a8 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db9 <- dbGetQuery(db,"select database from answer_storage where id = 9")
    m_q9 <- dbGetQuery(db,"select q_no from answer_storage where id = 9")
    m_r9 <- dbGetQuery(db,"select result from answer_storage where id = 9")
    t9 <- paste0("demotest_",m_db9)
    m_query9 <- sprintf("select question from %s where id = %s",t9,m_q9)
    res9 <- dbGetQuery(db,m_query9)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db9)
    print(m_q9)
    print(m_r9)
    print(t9)
    print(res9)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr_q9 <- renderUI(as.character(res9))
    if(m_r9==1){
      output$mr_a9 <- renderUI("Attempted correctly")
    }
    else if(m_r9 == 0){
      output$mr_a9 <- renderUI("Attempted wrong")
    }
    else{
      output$mr_a9 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db10 <- dbGetQuery(db,"select database from answer_storage where id = 10")
    m_q10 <- dbGetQuery(db,"select q_no from answer_storage where id = 10")
    m_r10 <- dbGetQuery(db,"select result from answer_storage where id = 10")
    t10 <- paste0("demotest_",m_db10)
    m_query10 <- sprintf("select question from %s where id = %s",t10,m_q10)
    res10 <- dbGetQuery(db,m_query10)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db10)
    print(m_q10)
    print(m_r10)
    print(t10)
    print(res10)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr_q10 <- renderUI(as.character(res10))
    if(m_r10==1){
      output$mr_a10 <- renderUI("Attempted correctly")
    }
    else if(m_r10 == 0){
      output$mr_a10 <- renderUI("Attempted wrong")
    }
    else{
      output$mr_a10 <- renderUI("Unattempted")
    } 
    #-------------------------------------------------------------------
    
    dbSendQuery(db,"update answer_storage set q_no = 0, database =0, result = -1")
    h <- get_plot_data()
    output$mock_graph <- renderPlot(barplot(h,xlab = "Topics",ylab="student_level",names = c("ratio","probability","geometry","pNc","aptitude"),col = c("yellow","red","blue","pink","green")))
    
    
  })
  
  observeEvent(input$mock_btn2,{
    #answers attempted 
    print("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS")
    attempted <- dbGetQuery(db,"select count(*) from answer_storage where result != -1")
    print(attempted)
    output$m2_answers_attempted <- renderUI(paste0("Number of answers attempted : ",as.character(attempted)))
    wrong <- dbGetQuery(db,"select count(*) from answer_storage where result = 0")
    print(wrong)
    output$m2_wrong_attempted <- renderUI(paste0("Number of wrong answers attempted : ",as.character(wrong)))
    right <- dbGetQuery(db,"select count(*) from answer_storage where result = 1")
    print(right)
    output$m2_correct_attempted <- renderUI(paste0("Number of correct answers attempted : ",as.character(right)))
    
    #-------------------------------------------------------------------
    
    m_db1 <- dbGetQuery(db,"select database from answer_storage where id = 1")
    m_q1 <- dbGetQuery(db,"select q_no from answer_storage where id = 1")
    m_r1 <- dbGetQuery(db,"select result from answer_storage where id = 1")
    t1 <- paste0("mocktest2_test",m_db1)
    m_query1 <- sprintf("select question from %s where id = %s",t1,m_q1)
    print(m_query1)
    res1 <- dbGetQuery(db,m_query1)
    
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print("bhargav1")
    print(m_db1)
    print(m_q1)
    print(m_r1)
    print(t1)
    print(res1)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr2_q1 <- renderUI(as.character(res1))
    if(m_r1==1){
      output$mr2_a1 <- renderUI("Attempted correctly")
    }
    else if(m_r1 == 0){
      output$mr2_a1 <- renderUI("Attempted wrong")
    }
    else{
      output$mr2_a1 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_db2 <- dbGetQuery(db,"select database from answer_storage where id = 2")
    m_q2 <- dbGetQuery(db,"select q_no from answer_storage where id = 2")
    m_r2 <- dbGetQuery(db,"select result from answer_storage where id = 2")
    t2 <- paste0("demotest_",m_db2)
    m_query2 <- sprintf("select question from %s where id = %s",t2,m_q2)
    res2 <- dbGetQuery(db,m_query2)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db2)
    print(m_q2)
    print(m_r2)
    print(t2)
    print(res2)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr2_q2 <- renderUI(as.character(res2))
    if(m_r2==1){
      output$mr2_a2 <- renderUI("Attempted correctly")
    }
    else if(m_r2 == 0){
      output$mr2_a2 <- renderUI("Attempted wrong")
    }
    else{
      output$mr2_a2 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_db3 <- dbGetQuery(db,"select database from answer_storage where id = 3")
    m_q3 <- dbGetQuery(db,"select q_no from answer_storage where id = 3")
    m_r3 <- dbGetQuery(db,"select result from answer_storage where id = 3")
    t3 <- paste0("demotest_",m_db3)
    m_query3 <- sprintf("select question from %s where id = %s",t3,m_q3)
    res3 <- dbGetQuery(db,m_query3)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db3)
    print(m_q3)
    print(m_r3)
    print(t3)
    print(res3)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr2_q3 <- renderUI(as.character(res3))
    if(m_r3==1){
      output$mr2_a3 <- renderUI("Attempted correctly")
    }
    else if(m_r3 == 0){
      output$mr2_a3 <- renderUI("Attempted wrong")
    }
    else{
      output$mr2_a3 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_db4 <- dbGetQuery(db,"select database from answer_storage where id = 4")
    m_q4 <- dbGetQuery(db,"select q_no from answer_storage where id = 4")
    m_r4 <- dbGetQuery(db,"select result from answer_storage where id = 4")
    t4 <- paste0("demotest_",m_db4)
    m_query4 <- sprintf("select question from %s where id = %s",t4,m_q4)
    res4 <- dbGetQuery(db,m_query4)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db4)
    print(m_q4)
    print(m_r4)
    print(t4)
    print(res4)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr2_q4 <- renderUI(as.character(res4))
    if(m_r4==1){
      output$mr2_a4 <- renderUI("Attempted correctly")
    }
    else if(m_r4 == 0){
      output$mr2_a4 <- renderUI("Attempted wrong")
    }
    else{
      output$mr2_a4 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_db5 <- dbGetQuery(db,"select database from answer_storage where id = 5")
    m_q5 <- dbGetQuery(db,"select q_no from answer_storage where id = 5")
    m_r5 <- dbGetQuery(db,"select result from answer_storage where id = 5")
    t5 <- paste0("demotest_",m_db5)
    m_query5 <- sprintf("select question from %s where id = %s",t5,m_q5)
    res5 <- dbGetQuery(db,m_query5)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db5)
    print(m_q5)
    print(m_r5)
    print(t5)
    print(res5)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr2_q5 <- renderUI(as.character(res5))
    if(m_r5==1){
      output$mr2_a5 <- renderUI("Attempted correctly")
    }
    else if(m_r5 == 0){
      output$mr2_a5 <- renderUI("Attempted wrong")
    }
    else{
      output$mr2_a5 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db6 <- dbGetQuery(db,"select database from answer_storage where id = 6")
    m_q6 <- dbGetQuery(db,"select q_no from answer_storage where id = 6")
    m_r6 <- dbGetQuery(db,"select result from answer_storage where id = 6")
    t6 <- paste0("demotest_",m_db6)
    m_query6 <- sprintf("select question from %s where id = %s",t6,m_q6)
    res6 <- dbGetQuery(db,m_query6)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db6)
    print(m_q6)
    print(m_r6)
    print(t6)
    print(res6)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr2_q6 <- renderUI(as.character(res6))
    if(m_r6==1){
      output$mr2_a1 <- renderUI("Attempted correctly")
    }
    else if(m_r6 == 0){
      output$mr2_a6 <- renderUI("Attempted wrong")
    }
    else{
      output$mr2_a6 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db7 <- dbGetQuery(db,"select database from answer_storage where id = 7")
    m_q7 <- dbGetQuery(db,"select q_no from answer_storage where id = 7")
    m_r7 <- dbGetQuery(db,"select result from answer_storage where id = 7")
    t7 <- paste0("demotest_",m_db7)
    m_query7 <- sprintf("select question from %s where id = %s",t7,m_q7)
    res7 <- dbGetQuery(db,m_query7)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db7)
    print(m_q7)
    print(m_r7)
    print(t7)
    print(res7)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr2_q7 <- renderUI(as.character(res7))
    if(m_r7==1){
      output$mr2_a7 <- renderUI("Attempted correctly")
    }
    else if(m_r7 == 0){
      output$mr2_a7 <- renderUI("Attempted wrong")
    }
    else{
      output$mr2_a7 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db8 <- dbGetQuery(db,"select database from answer_storage where id = 8")
    m_q8 <- dbGetQuery(db,"select q_no from answer_storage where id = 8")
    m_r8 <- dbGetQuery(db,"select result from answer_storage where id = 8")
    t8 <- paste0("demotest_",m_db8)
    m_query8 <- sprintf("select question from %s where id = %s",t8,m_q8)
    res8 <- dbGetQuery(db,m_query8)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db8)
    print(m_q8)
    print(m_r8)
    print(t8)
    print(res8)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr2_q8 <- renderUI(as.character(res8))
    if(m_r8==1){
      output$mr2_a8 <- renderUI("Attempted correctly")
    }
    else if(m_r8 == 0){
      output$mr2_a8 <- renderUI("Attempted wrong")
    }
    else{
      output$mr2_a8 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db9 <- dbGetQuery(db,"select database from answer_storage where id = 9")
    m_q9 <- dbGetQuery(db,"select q_no from answer_storage where id = 9")
    m_r9 <- dbGetQuery(db,"select result from answer_storage where id = 9")
    t9 <- paste0("demotest_",m_db9)
    m_query9 <- sprintf("select question from %s where id = %s",t9,m_q9)
    res9 <- dbGetQuery(db,m_query9)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db9)
    print(m_q9)
    print(m_r9)
    print(t9)
    print(res9)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr2_q9 <- renderUI(as.character(res9))
    if(m_r9==1){
      output$mr2_a9 <- renderUI("Attempted correctly")
    }
    else if(m_r9 == 0){
      output$mr2_a9 <- renderUI("Attempted wrong")
    }
    else{
      output$mr2_a9 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db10 <- dbGetQuery(db,"select database from answer_storage where id = 10")
    m_q10 <- dbGetQuery(db,"select q_no from answer_storage where id = 10")
    m_r10 <- dbGetQuery(db,"select result from answer_storage where id = 10")
    t10 <- paste0("demotest_",m_db10)
    m_query10 <- sprintf("select question from %s where id = %s",t10,m_q10)
    res10 <- dbGetQuery(db,m_query10)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db10)
    print(m_q10)
    print(m_r10)
    print(t10)
    print(res10)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr2_q10 <- renderUI(as.character(res10))
    if(m_r10==1){
      output$mr2_a10 <- renderUI("Attempted correctly")
    }
    else if(m_r10 == 0){
      output$mr2_a10 <- renderUI("Attempted wrong")
    }
    else{
      output$mr2_a10 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    dbSendQuery(db,"update answer_storage set q_no = 0, database =0, result = -1")
    
    h <- get_plot_data()
    output$mock2_graph <- renderPlot(barplot(h,xlab = "Topics",ylab="student_level",names = c("ratio","probability","geometry","pNc","aptitude"),col = c("yellow","red","blue","pink","green")))
  })
  
  observeEvent(input$mock_btn3,{
    #answers attempted 
    print("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS")
    attempted <- dbGetQuery(db,"select count(*) from answer_storage where result != -1")
    print(attempted)
    output$m3_answers_attempted <- renderUI(paste0("Number of answers attempted : ",as.character(attempted)))
    wrong <- dbGetQuery(db,"select count(*) from answer_storage where result = 0")
    print(wrong)
    output$m3_wrong_attempted <- renderUI(paste0("Number of wrong answers attempted : ",as.character(wrong)))
    right <- dbGetQuery(db,"select count(*) from answer_storage where result = 1")
    print(right)
    output$m3_correct_attempted <- renderUI(paste0("Number of correct answers attempted : ",as.character(right)))
    
    #-------------------------------------------------------------------
    
    m_db1 <- dbGetQuery(db,"select database from answer_storage where id = 1")
    m_q1 <- dbGetQuery(db,"select q_no from answer_storage where id = 1")
    m_r1 <- dbGetQuery(db,"select result from answer_storage where id = 1")
    t1 <- paste0("mocktest2_test",m_db1)
    m_query1 <- sprintf("select question from %s where id = %s",t1,m_q1)
    print(m_query1)
    res1 <- dbGetQuery(db,m_query1)
    
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print("bhargav1")
    print(m_db1)
    print(m_q1)
    print(m_r1)
    print(t1)
    print(res1)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr3_q1 <- renderUI(as.character(res1))
    if(m_r1==1){
      output$mr3_a1 <- renderUI("Attempted correctly")
    }
    else if(m_r1 == 0){
      output$mr3_a1 <- renderUI("Attempted wrong")
    }
    else{
      output$mr3_a1 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_db2 <- dbGetQuery(db,"select database from answer_storage where id = 2")
    m_q2 <- dbGetQuery(db,"select q_no from answer_storage where id = 2")
    m_r2 <- dbGetQuery(db,"select result from answer_storage where id = 2")
    t2 <- paste0("demotest_",m_db2)
    m_query2 <- sprintf("select question from %s where id = %s",t2,m_q2)
    res2 <- dbGetQuery(db,m_query2)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db2)
    print(m_q2)
    print(m_r2)
    print(t2)
    print(res2)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr3_q2 <- renderUI(as.character(res2))
    if(m_r2==1){
      output$mr3_a2 <- renderUI("Attempted correctly")
    }
    else if(m_r2 == 0){
      output$mr3_a2 <- renderUI("Attempted wrong")
    }
    else{
      output$mr3_a2 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_db3 <- dbGetQuery(db,"select database from answer_storage where id = 3")
    m_q3 <- dbGetQuery(db,"select q_no from answer_storage where id = 3")
    m_r3 <- dbGetQuery(db,"select result from answer_storage where id = 3")
    t3 <- paste0("demotest_",m_db3)
    m_query3 <- sprintf("select question from %s where id = %s",t3,m_q3)
    res3 <- dbGetQuery(db,m_query3)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db3)
    print(m_q3)
    print(m_r3)
    print(t3)
    print(res3)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr3_q3 <- renderUI(as.character(res3))
    if(m_r3==1){
      output$mr3_a3 <- renderUI("Attempted correctly")
    }
    else if(m_r3 == 0){
      output$mr3_a3 <- renderUI("Attempted wrong")
    }
    else{
      output$mr3_a3 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_db4 <- dbGetQuery(db,"select database from answer_storage where id = 4")
    m_q4 <- dbGetQuery(db,"select q_no from answer_storage where id = 4")
    m_r4 <- dbGetQuery(db,"select result from answer_storage where id = 4")
    t4 <- paste0("demotest_",m_db4)
    m_query4 <- sprintf("select question from %s where id = %s",t4,m_q4)
    res4 <- dbGetQuery(db,m_query4)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db4)
    print(m_q4)
    print(m_r4)
    print(t4)
    print(res4)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr3_q4 <- renderUI(as.character(res4))
    if(m_r4==1){
      output$mr3_a4 <- renderUI("Attempted correctly")
    }
    else if(m_r4 == 0){
      output$mr3_a4 <- renderUI("Attempted wrong")
    }
    else{
      output$mr3_a4 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    m_db5 <- dbGetQuery(db,"select database from answer_storage where id = 5")
    m_q5 <- dbGetQuery(db,"select q_no from answer_storage where id = 5")
    m_r5 <- dbGetQuery(db,"select result from answer_storage where id = 5")
    t5 <- paste0("demotest_",m_db5)
    m_query5 <- sprintf("select question from %s where id = %s",t5,m_q5)
    res5 <- dbGetQuery(db,m_query5)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db5)
    print(m_q5)
    print(m_r5)
    print(t5)
    print(res5)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr3_q5 <- renderUI(as.character(res5))
    if(m_r5==1){
      output$mr3_a5 <- renderUI("Attempted correctly")
    }
    else if(m_r5 == 0){
      output$mr3_a5 <- renderUI("Attempted wrong")
    }
    else{
      output$mr3_a5 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db6 <- dbGetQuery(db,"select database from answer_storage where id = 6")
    m_q6 <- dbGetQuery(db,"select q_no from answer_storage where id = 6")
    m_r6 <- dbGetQuery(db,"select result from answer_storage where id = 6")
    t6 <- paste0("demotest_",m_db6)
    m_query6 <- sprintf("select question from %s where id = %s",t6,m_q6)
    res6 <- dbGetQuery(db,m_query6)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db6)
    print(m_q6)
    print(m_r6)
    print(t6)
    print(res6)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr3_q6 <- renderUI(as.character(res6))
    if(m_r6==1){
      output$mr3_a1 <- renderUI("Attempted correctly")
    }
    else if(m_r6 == 0){
      output$mr3_a6 <- renderUI("Attempted wrong")
    }
    else{
      output$mr3_a6 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db7 <- dbGetQuery(db,"select database from answer_storage where id = 7")
    m_q7 <- dbGetQuery(db,"select q_no from answer_storage where id = 7")
    m_r7 <- dbGetQuery(db,"select result from answer_storage where id = 7")
    t7 <- paste0("demotest_",m_db7)
    m_query7 <- sprintf("select question from %s where id = %s",t7,m_q7)
    res7 <- dbGetQuery(db,m_query7)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db7)
    print(m_q7)
    print(m_r7)
    print(t7)
    print(res7)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr3_q7 <- renderUI(as.character(res7))
    if(m_r7==1){
      output$mr3_a7 <- renderUI("Attempted correctly")
    }
    else if(m_r7 == 0){
      output$mr3_a7 <- renderUI("Attempted wrong")
    }
    else{
      output$mr3_a7 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db8 <- dbGetQuery(db,"select database from answer_storage where id = 8")
    m_q8 <- dbGetQuery(db,"select q_no from answer_storage where id = 8")
    m_r8 <- dbGetQuery(db,"select result from answer_storage where id = 8")
    t8 <- paste0("demotest_",m_db8)
    m_query8 <- sprintf("select question from %s where id = %s",t8,m_q8)
    res8 <- dbGetQuery(db,m_query8)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db8)
    print(m_q8)
    print(m_r8)
    print(t8)
    print(res8)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr3_q8 <- renderUI(as.character(res8))
    if(m_r8==1){
      output$mr3_a8 <- renderUI("Attempted correctly")
    }
    else if(m_r8 == 0){
      output$mr3_a8 <- renderUI("Attempted wrong")
    }
    else{
      output$mr3_a8 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db9 <- dbGetQuery(db,"select database from answer_storage where id = 9")
    m_q9 <- dbGetQuery(db,"select q_no from answer_storage where id = 9")
    m_r9 <- dbGetQuery(db,"select result from answer_storage where id = 9")
    t9 <- paste0("demotest_",m_db9)
    m_query9 <- sprintf("select question from %s where id = %s",t9,m_q9)
    res9 <- dbGetQuery(db,m_query9)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db9)
    print(m_q9)
    print(m_r9)
    print(t9)
    print(res9)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr3_q9 <- renderUI(as.character(res9))
    if(m_r9==1){
      output$mr3_a9 <- renderUI("Attempted correctly")
    }
    else if(m_r9 == 0){
      output$mr3_a9 <- renderUI("Attempted wrong")
    }
    else{
      output$mr3_a9 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    m_db10 <- dbGetQuery(db,"select database from answer_storage where id = 10")
    m_q10 <- dbGetQuery(db,"select q_no from answer_storage where id = 10")
    m_r10 <- dbGetQuery(db,"select result from answer_storage where id = 10")
    t10 <- paste0("demotest_",m_db10)
    m_query10 <- sprintf("select question from %s where id = %s",t10,m_q10)
    res10 <- dbGetQuery(db,m_query10)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(m_db10)
    print(m_q10)
    print(m_r10)
    print(t10)
    print(res10)
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    output$mr3_q10 <- renderUI(as.character(res10))
    if(m_r10==1){
      output$mr3_a10 <- renderUI("Attempted correctly")
    }
    else if(m_r10 == 0){
      output$mr3_a10 <- renderUI("Attempted wrong")
    }
    else{
      output$mr3_a10 <- renderUI("Unattempted")
    }
    #-------------------------------------------------------------------
    
    dbSendQuery(db,"update answer_storage set q_no = 0, database =0, result = -1")
    
    h <- get_plot_data()
    output$mock3_graph <- renderPlot(barplot(h,xlab = "Topics",ylab="student_level",names = c("ratio","probability","geometry","pNc","aptitude"),col = c("yellow","red","blue","pink","green")))
  })
  
  
  
  observeEvent(input$student_profile_btn,{
    print("inside student profile button")
    hype <- get_plot_data()
    print(hype)
    output$demo_graph <- renderPlot(barplot(hype,xlab = "Topics",ylab="student_level",names = c("ratio","probability","geometry","pNc","aptitude"),col = c("yellow","red","blue","pink","green")))
    query <- sprintf("select mailid from prsnt_student where name like 'shravya';")
    res <- dbGetQuery(db,query)
    print(res)
    query <- sprintf("select * from studentmodel_final where mail_id = '%s';",res)
    data1<- dbGetQuery(db,query) 
    
    probs <- c(as.numeric(data1[7]),as.numeric(data1[8]),as.numeric(data1[9]),as.numeric(data1[10]),as.numeric(data1[11])) 
    print("probs content is")
    print(probs)
    
    df <- get_data_frame(probs,x)
    print("final df is")
    print(df)
    
    flag <- 0
    
    i <- 0
    
    if(df[1] ==1 ){
      if(flag ==0)
      {
        print("date is")
        
        a <- paste0("day",day())
        print(a)
        flag <- 1
        i <- 2
        output$l1 <- renderText(a)
        output$ratio1 <- renderText("ratio session")
        output$ratio2 <- renderText("ratio session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag==1){
        if (i<1)
        {
          print("date is")
          b<- paste0("day",day())
          print(b)
          output$l1 <- renderText(b)
          output$ratio1 <- renderText("ratio session")
          output$ratio2 <- renderText("ratio session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
      }
    }
    if(df[2]==1){
      
      if(flag==0)
      {
        print("date is")
        c <- paste0("day",day())
        print(c)
        flag<-1
        i<-2
        output$l2 <- renderText(c)
        output$ratio2 <- renderText("ratio session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
    if(df[3]==1){
      if(flag ==0)
      {
        print("date is")
        d <- paste0("day",day())
        print(d)
        flag <- 1
        i <- 4
        output$l3 <- renderText(d)
        output$ratio3 <- renderText("ratio session")
        output$ratio4 <- renderText("ratio session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag==1){
        if (i<3)
        {
          print("date is")
          e <- paste0("day",day())
          print(e)
          output$l3 <- renderText(e)
          output$ratio3 <- renderText("ratio session")
          output$ratio4<- renderText("ratio session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    if(df[4]==1)
    {
      if(flag==0)
      {
        print("date is")
        f <- paste0("day",day())
        print(f)
        flag <- 1
        i <- 4
        output$l4 <- renderText(f)
        output$ratio4 <- renderText("ratio session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      
    }
    if(df[5]==1){
      if(flag ==0)
      {
        print("date is")
        g <- paste0("day",day())
        print(g)
        flag <- 1
        i <- 6
        output$l5 <- renderText(g)
        output$probability5 <- renderText("ratio session")
        output$probability6<- renderText("ratio session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag==1){
        if (i<5)
        {
          print("date is")
          h <- paste0("day",day())
          print(h)
          output$l5 <- renderText(h)
          output$ratio5 <- renderText("ratio session")
          output$ratio6<- renderText("ratio session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    if(df[6] == 1){
      if(flag==0)
      {
        print("date is")
        j <- paste0("day",day())
        print(j)
        flag <- 1
        i <- 6
        output$l6 <- renderText(j)
        output$ratio6 <- renderText("ratio session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
    flag1 <- 0
    print("FOR PROBABILIY------------------")
    if(df[7]==1){
      if(flag1 ==0)
      {
        print("FIRST IF")
        print("date is")
        a2 <- paste0("day",day())
        print(a2)
        flag1 <- 1
        i <- 8
        
        output$l7 <- renderText(a2)
        output$probability1 <- renderText("probability session")
        output$probability2<- renderText("probability session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag1==1){
        if (i<7)
        {
          print("date is")
          l <- paste0("day",day())
          print(l)
          output$l7 <- renderText(l)
          output$probability1 <- renderText("probability session")
          output$probability2 <- renderText("probability session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    if(df[8]==1){
      
      if(flag1==0)
      {
        
        print("date is")
        m <- paste0("day",day())
        print(m)
        flag1 <- 1
        i <- 8
        output$l8 <- renderText(m)
        output$probability2 <- renderText("probability session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
    if(df[9]==1){
      if(flag1 ==0)
      {
        print("date is")
        n <- paste0("day",day())
        print(n)
        flag1 <- 1
        i <- 9
        output$l9 <- renderText(n)
        output$probability3 <- renderText("probability session")
        output$probability4<- renderText("probability session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag1==1){
        if (i<9)
        {
          print("date is")
          o <- paste0("day",day())
          print(o)
          output$l9 <- renderText(o)
          output$probability3 <- renderText("probability session")
          output$probability4<- renderText("probability session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    
    if(df[10]==1){
      
      if(flag1==0)
      {
        
        print("date is")
        p <- paste0("day",day())
        print(p)
        flag1 <- 1
        i <- 10
        output$l10 <- renderText(p)
        output$probability4 <- renderText("probability session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
    
    
    if(df[11]==1){
      if(flag1 ==0)
      {
        print("date is")
        q <- paste0("day",day())
        print(q)
        flag1 <- 1
        i <- 11
        output$l11 <- renderText(q)
        output$probability5 <- renderText("probability session")
        output$probability6<- renderText("probability session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag1==1){
        if (i<11)
        {
          print("date is")
          r <- paste0("day",day())
          print(r)
          output$l11 <- renderText(r)
          output$probability5 <- renderText("probability session")
          output$probability6<- renderText("probability session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    
    if(df[12]==1){
      
      if(flag1==0)
      {
        
        print("date is")
        s <- paste0("day",day())
        print(s)
        flag1 <- 1
        i <- 12
        output$l12 <- renderText(s)
        output$probability6 <- renderText("probability session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
    
    flag2 <- 0
    
    if(df[13]==1){
      if(flag2 ==0)
      {
        print("date is")
        t <- paste0("day",day())
        print(t)
        flag2 <- 1
        i <- 13
        output$l13 <- renderText(t)
        output$geometry1 <- renderText("geometry session")
        output$geometry2<- renderText("geometry session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag2==1){
        if (i<13)
        {
          print("date is")
          u <- paste0("day",day())
          print(u)
          output$l13 <- renderText(u)
          output$geometry1 <- renderText("geometry session")
          output$geometry2<- renderText("geometry session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    
    if(df[14]==1){
      
      if(flag2==0)
      {
        
        print("date is")
        v <- paste0("day",day())
        print(v)
        flag2 <- 1
        i <- 14
        output$l14 <- renderText(v)
        output$geometry2 <- renderText("geometry session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
    
    
    if(df[15]==1){
      if(flag2 ==0)
      {
        print("date is")
        w <- paste0("day",day())
        print(w)
        flag2 <- 1
        i <- 15
        output$l15 <- renderText(w)
        output$geometry3 <- renderText("geometry session")
        output$geometry4 <- renderText("geometry session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag2==1){
        if (i<15)
        {
          print("date is")
          x <- paste0("day",day())
          print(x)
          output$l15 <- renderText(x)
          output$geometry3 <- renderText("geometry session")
          output$geometry4<- renderText("geometry session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    
    if(df[16]==1){
      
      if(flag2==0)
      {
        
        print("date is")
        y <- paste0("day",day())
        print(y)
        flag2 <- 1
        i <- 16
        output$l16 <- renderText(y)
        output$geometry4 <- renderText("geometry session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
    
    
    if(df[17]==1){
      if(flag2 ==0)
      {
        print("date is")
        z <- paste0("day",day())
        print(z)
        flag2 <- 1
        i <- 17
        output$l17 <- renderText(z)
        output$geometry5 <- renderText("geometry session")
        output$geometry6<- renderText("geometry session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag2==1){
        if (i<17)
        {
          print("date is")
          a1 <- paste0("day",day())
          print(a1)
          output$l17 <- renderText(a1)
          output$geometry5 <- renderText("geometry session")
          output$geometry6<- renderText("geometry session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    
    if(df[18]==1){
      
      if(flag2==0)
      {
        
        print("date is")
        b1 <- paste0("day",day())
        print(b1)
        flag <- 1
        i <- 18
        output$l18 <- renderText(b1)
        output$geometry6 <- renderText("geometry session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
    
    flag3 <- 0
    if(df[19]==1){
      if(flag3 ==0)
      {
        print("date is")
        c1 <- paste0("day",day())
        print( c1)
        flag3 <- 1
        i <- 19
        output$l19 <- renderText(c1)
        output$pNc1 <- renderText("pNc session")
        output$pNc2 <- renderText("pNc session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag3==1){
        if (i<19)
        {
          print("date is")
          d1 <- paste0("day",day())
          print(d1)
          output$l19 <- renderText(d1)
          output$pNc1 <- renderText("pNc session")
          output$pNc2 <- renderText("pNc session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    
    if(df[20]==1){
      
      if(flag3==0)
      {
        
        print("date is")
        e1 <- paste0("day",day())
        print(e1)
        flag3 <- 1
        i <- 20
        output$l20 <- renderText(e1)
        output$pNc2 <- renderText("pNc session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
    
    
    if(df[21]==1){
      if(flag3 ==0)
      {
        print("date is")
        f1 <- paste0("day",day())
        print( f1)
        flag3 <- 1
        i <- 21
        output$l21 <- renderText(f1)
        output$pNc3 <- renderText("pNc session")
        output$pNc4 <- renderText("pNc session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag3 == 1){
        if (i<21)
        {
          print("date is")
          g1 <- paste0("day",day())
          print(g1)
          output$l21 <- renderText(g1)
          output$pNc3 <- renderText("pNc session")
          output$pNc4 <- renderText("pNc session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    
    if(df[22]==1){
      
      if(flag3==0)
      {
        
        print("date is")
        h1 <- paste0("day",day())
        print(h1)
        flag3 <- 1
        i <-22
        output$l22 <- renderText(h1)
        output$pNc4 <- renderText("pNc session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
    
    
    
    if(df[23]==1){
      if(flag3 ==0)
      {
        print("date is")
        i1 <- paste0("day",day())
        print( i1)
        flag3 <- 1
        i <- 23
        output$l23 <- renderText(i1)
        output$pNc5 <- renderText("pNc session")
        output$pNc6<- renderText("pNc session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag3==1){
        if (i<23)
        {
          print("date is")
          j1 <- paste0("day",day())
          print(j1)
          output$l23 <- renderText(j1)
          output$pNc5 <- renderText("pNc session")
          output$pNc6 <- renderText("pNc session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    
    if(df[24]==1){
      
      if(flag3 == 0)
      {
        
        print("date is")
        k1 <- paste0("day",day())
        print(k1)
        flag3 <- 1
        i <-24
        output$l24 <- renderText(k1)
        output$pNc6 <- renderText("pNc session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
    flag4 <- 0
    if(df[25]==1){
      if(flag4 ==0)
      {
        print("date is")
        l1 <- paste0("day",day())
        print( l1)
        flag4 <- 1
        i <- 25
        output$l25 <- renderText(l1)
        output$aptitude1 <- renderText("aptitude session")
        output$aptitude2 <- renderText("aptitude session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag4 == 1){
        if (i<25)
        {
          print("date is")
          m1 <- paste0("day",day())
          print(m1)
          output$l25 <- renderText(m1)
          output$aptitude1 <- renderText("aptitude session")
          output$aptitude2 <- renderText("aptitude session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    
    if(df[26]==1){
      
      if(flag4==0)
      {
        
        print("date is")
        n1 <- paste0("day",day())
        print(n1)
        flag4 <- 1
        i <-26
        output$l26 <- renderText(n1)
        output$aptitude2 <- renderText("pNc session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
    
    if(df[27]==1){
      if(flag4 ==0)
      {
        print("date is")
        o1 <- paste0("day",day())
        print(o1)
        flag4 <- 1
        i <- 27
        output$l27 <- renderText(o1)
        output$aptitude3 <- renderText("aptitude session")
        output$aptitude4 <- renderText("aptitude session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag4 == 1){
        if (i<27)
        {
          print("date is")
          p1 <- paste0("day",day())
          print(p1)
          output$l27 <- renderText(p1)
          output$aptitude3 <- renderText("aptitude session")
          output$aptitude4 <- renderText("aptitude session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    
    if(df[28]==1){
      
      if(flag4==0)
      {
        
        print("date is")
        p1 <- paste0("day",day())
        print(p1)
        flag4 <- 1
        i <-28
        output$l28 <- renderText(p1)
        output$aptitude4 <- renderText("pNc session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
    
    
    if(df[29]==1){
      if(flag4 ==0)
      {
        print("date is")
        q1 <- paste0("day",day())
        print(q1)
        flag4 <- 1
        i <- 29
        output$l29 <- renderText(q1)
        output$aptitude5 <- renderText("aptitude session")
        output$aptitude6 <- renderText("aptitude session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
      else if(flag4==1){
        if (i<29)
        {
          print("date is")
          r1 <- paste0("day",day())
          print(r1)
          output$l29 <- renderText(r1)
          output$aptitude5 <- renderText("aptitude session")
          output$aptitude6 <- renderText("aptitude session")
          new_day <- day() +1
          day(new_day)
          i <- i+2
          print("now new dATE is:")
          print(day())
        }
        
      }
    }
    
    if(df[30]==1){
      
      if(flag4==0)
      {
        
        print("date is")
        s1 <- paste0("day",day())
        print(s1)
        flag4 <- 1
        i <-30
        output$l30 <- renderText(s1)
        output$aptitude6 <- renderText("pNc session")
        new_day <- day() +1
        day(new_day)
        print("now new dATE is:")
        print(day())
      }
    }
  })
  
}