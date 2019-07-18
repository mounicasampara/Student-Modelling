library(shiny)
library(RSQLite)
library(DT)
library(DBI)
library(dplyr)
library(dbplyr)

sqlitePath <- "d:/mydb1.db"
print(sqlitePath)
db <- dbConnect(SQLite(), sqlitePath)
print(db)

getData_mocktest2 <- function(id){
  query <- sprintf("select question from mocktest2_test1 where ID= %s",id)
  #print(query)
  question <- dbGetQuery(db,query)
  #print(question)
  query1 <- sprintf("select option1 from mocktest2_test1 where ID= %s",id)
  query2 <- sprintf("select option2 from mocktest2_test1 where ID= %s",id)
  query3 <- sprintf("select option3 from mocktest2_test1 where ID= %s",id)
  query4 <- sprintf("select option4 from mocktest2_test1 where ID= %s",id)
  query5 <- sprintf("select Answer from mocktest2_test1 where ID = %s",id)
  
  option1<-dbGetQuery(db,query1)
  option2<-dbGetQuery(db,query2)
  option3<-dbGetQuery(db,query3)
  option4<-dbGetQuery(db,query4)
  answer<-dbGetQuery(db,query5)
  
  x<-c(question,option1,option2,option3,option4,answer)
  return(x) 
}

getData_mocktest1 <- function(id){
  query <- sprintf("select question from demotest_1 where ID= %s",id)
  #print(query)
  question <- dbGetQuery(db,query)
  #print(question)
  query1 <- sprintf("select option1 from demotest_1 where ID= %s",id)
  query2 <- sprintf("select option2 from demotest_1 where ID= %s",id)
  query3 <- sprintf("select option3 from demotest_1 where ID= %s",id)
  query4 <- sprintf("select option4 from demotest_1 where ID= %s",id)
  query5 <- sprintf("select Answer from demotest_1 where ID = %s",id)
  
  option1<-dbGetQuery(db,query1)
  option2<-dbGetQuery(db,query2)
  option3<-dbGetQuery(db,query3)
  option4<-dbGetQuery(db,query4)
  answer<-dbGetQuery(db,query5)
  
  x<-c(question,option1,option2,option3,option4,answer)
  return(x) 
}

getData_mocktest3 <- function(id){
  query <- sprintf("select question from mocktest3_test1 where ID= %s",id)
  #print(query)
  question <- dbGetQuery(db,query)
  #print(question)
  query1 <- sprintf("select option1 from mocktest3_test1 where ID= %s",id)
  query2 <- sprintf("select option2 from mocktest3_test1 where ID= %s",id)
  query3 <- sprintf("select option3 from mocktest3_test1 where ID= %s",id)
  query4 <- sprintf("select option4 from mocktest3_test1 where ID= %s",id)
  query5 <- sprintf("select Answer from mocktest3_test1 where ID = %s",id)
  
  option1<-dbGetQuery(db,query1)
  option2<-dbGetQuery(db,query2)
  option3<-dbGetQuery(db,query3)
  option4<-dbGetQuery(db,query4)
  answer<-dbGetQuery(db,query5)
  
  x<-c(question,option1,option2,option3,option4,answer)
  return(x) 
}


check_login_details <- function(data){
  query <- sprintf("select count(mailId) from registration where mailid like '%s' and password like '%s';",data[1],data[2])
  print(query)
  res <- dbGetQuery(db,query)
  print(res)
  print(data[2])
  print(length(res))
  
  if(res==0)
  {
    print("new user or incorrect login")
    query1 <- sprintf("select count(mailId) from registration where mailId like '%s';",data[1])
    res2<- dbGetQuery(db,query1)
    print(res2)
    if(res2==0)#new user
    {
      return(2)
    }
    else#wrong pswd
      return(0)
  }
  if(res==1)#correct login credentials
  {
    return(1)
  }
  
}
#----------------------------------
#----------------------------------

insert_reg_data <- function(data){
  query <- sprintf("insert into registration values ('%s','%s','%s','%s','%s','%s')",
                   data[1],data[2],data[3],data[4],data[5],data[6])
  print(query)
  dbSendQuery(db,query)
}
#----------------------------------
#----------------------------------
insert_intial_std_model_data <- function(mail){
  query <- sprintf("insert into std_model values ('%s',%s,%s,%s,%s,%s,%s);",
                   mail,0,0,0,0,0,0)
  print(query)
  dbSendQuery(db,query)
}
#----------------------------------
#----------------------------------
getData <- function(id){
  query <- sprintf("select question from demotest_questions where ID= %s",id)
  #print(query)
  question <- dbGetQuery(db,query)
  #print(question)
  query1 <- sprintf("select option1 from demotest_questions where ID= %s",id)
  query2 <- sprintf("select option2 from demotest_questions where ID= %s",id)
  query3 <- sprintf("select option3 from demotest_questions where ID= %s",id)
  query4 <- sprintf("select option4 from demotest_questions where ID= %s",id)
  query5 <- sprintf("select Answer from demotest_questions where ID = %s",id)
  
  option1<-dbGetQuery(db,query1)
  option2<-dbGetQuery(db,query2)
  option3<-dbGetQuery(db,query3)
  option4<-dbGetQuery(db,query4)
  answer<-dbGetQuery(db,query5)
  
  x<-c(question,option1,option2,option3,option4,answer)
  return(x) 
}

m_getData <- function(id,data_base){
  table_name <- paste0("demotest",data_base)
  print(table_name)
  query <- sprintf("select question from %s where ID= %s",table_name,id)
  #print(query)
  question <- dbGetQuery(db,query)
  #print(question)
  query1 <- sprintf("select option1 from %s where ID= %s",table_name,id)
  query2 <- sprintf("select option2 from %s where ID= %s",table_name,id)
  query3 <- sprintf("select option3 from %s where ID= %s",table_name,id)
  query4 <- sprintf("select option4 from %s where ID= %s",table_name,id)
  query5 <- sprintf("select Answer from %s where ID = %s",table_name,id)
  
  option1<-dbGetQuery(db,query1)
  option2<-dbGetQuery(db,query2)
  option3<-dbGetQuery(db,query3)
  option4<-dbGetQuery(db,query4)
  answer<-dbGetQuery(db,query5)
  
  x<-c(question,option1,option2,option3,option4,answer)
  return(x) 
}
m2_getData <- function(id,data_base){
  table_name <- paste0("mocktest2_test",data_base)
  print(table_name)
  query <- sprintf("select question from %s where ID= %s",table_name,id)
  #print(query)
  question <- dbGetQuery(db,query)
  #print(question)
  query1 <- sprintf("select option1 from %s where ID= %s",table_name,id)
  query2 <- sprintf("select option2 from %s where ID= %s",table_name,id)
  query3 <- sprintf("select option3 from %s where ID= %s",table_name,id)
  query4 <- sprintf("select option4 from %s where ID= %s",table_name,id)
  query5 <- sprintf("select Answer from %s where ID = %s",table_name,id)
  
  option1<-dbGetQuery(db,query1)
  option2<-dbGetQuery(db,query2)
  option3<-dbGetQuery(db,query3)
  option4<-dbGetQuery(db,query4)
  answer<-dbGetQuery(db,query5)
  
  x<-c(question,option1,option2,option3,option4,answer)
  return(x) 
}

m3_getData <- function(id,data_base){
  table_name <- paste0("mocktest3_test",data_base)
  print(table_name)
  query <- sprintf("select question from %s where ID= %s",table_name,id)
  #print(query)
  question <- dbGetQuery(db,query)
  #print(question)
  query1 <- sprintf("select option1 from %s where ID= %s",table_name,id)
  query2 <- sprintf("select option2 from %s where ID= %s",table_name,id)
  query3 <- sprintf("select option3 from %s where ID= %s",table_name,id)
  query4 <- sprintf("select option4 from %s where ID= %s",table_name,id)
  query5 <- sprintf("select Answer from %s where ID = %s",table_name,id)
  
  option1<-dbGetQuery(db,query1)
  option2<-dbGetQuery(db,query2)
  option3<-dbGetQuery(db,query3)
  option4<-dbGetQuery(db,query4)
  answer<-dbGetQuery(db,query5)
  
  x<-c(question,option1,option2,option3,option4,answer)
  return(x) 
}

resp_saving <- function(id,ans,res)
{
  print("inside response saving function")
  query <- sprintf("update demotest_questions set selectedans = %s where ID= %s",ans,id)
  dbSendQuery(db,query)
  query1 <- sprintf("update demotest_questions set result = %s where ID= %s",res,id)
  dbSendQuery(db,query1)
  
}

m_resp_saving <- function(id,ans,res,data_base){
  print("inside mocktest response saving function")
  table_name <- paste0("demotest",data_base)
  query <- sprintf("update %s set selectedans = %s where ID =%s",table_name,ans,id)
  dbSendQuery(db,query)
  query1 <- sprintf("update %s set result = %s where ID =%s",table_name,res,id)
  dbSendQuery(db,query1)
}

m2_resp_saving <- function(id,ans,res,data_base){
  print("inside mocktest response saving function")
  table_name <- paste0("mocktest2_test",data_base)
  query <- sprintf("update %s set selectedans = %s where ID =%s",table_name,ans,id)
  dbSendQuery(db,query)
  query1 <- sprintf("update %s set result = %s where ID =%s",table_name,res,id)
  dbSendQuery(db,query1)
}

m3_resp_saving <- function(id,ans,res,data_base){
  print("inside mocktest response saving function")
  table_name <- paste0("mocktest3_test",data_base)
  query <- sprintf("update %s set selectedans = %s where ID =%s",table_name,ans,id)
  dbSendQuery(db,query)
  query1 <- sprintf("update %s set result = %s where ID =%s",table_name,res,id)
  dbSendQuery(db,query1)
}

calc_prob <- function(q,d){
  d1 <- c(0,0,0,0,0)
  levels_frame <- c(0,0,0,0,0)
  query <- sprintf("select mailid from prsnt_student where name like 'shravya';")
  res <- dbGetQuery(db,query)
  print(res)
  query <- sprintf("select * from studentmodel_final where mail_id = '%s';",res)
  d1<- dbGetQuery(db,query)
  print(d1)
  for(i in 1:5){
    print("NOW VALUE OF I IS +++++++++++++++++++++++++++")
    print(i)
    print(d1[i+1])
    print(q)
    print("kaeufkuea")
    print(d[i])
    if(d1[i+1] == 0 && q == 1){
      d1[i+1] = d[i]
    }
    else if(d1[i+1] == 0 && q == 0){
      print("first else if")
      next
    }
    else if(d[i]==0 && q==1){
      print("Second else if")
      next
    }
    else{
      data1 <- c(0,0,0,0,0,0)
      data2 <- c(0,0,0,0,0,0)
      data1 <- get_dataframe(d1[i+1],1)
      data2 <- get_dataframe(d[i],q)
      print(data1)
      print(data2)
      
      den <- calc_den (data1,data2)
      data_frame_res <- calc_numr (data1,data2,den)
      print("your probabilty of answering a question related to the topic 'probability' is")
      print(data_frame_res)
      d1[i+1] = data_frame_res[2]
      levels_frame[i] = data_frame_res[1]
    }
  }
  print(d1)
  print(levels)
  #now insert data into database
  query <- sprintf("update studentmodel_final set ratio = %s ,probability = %s, geometry = %s, pNc =%s, aptitude = %s,ratio_level=%s,probability_level = %s,geometry_level = %s,pNc_level=%s,aptitude_level = %s where mail_id like '%s'",
                   d1[2],d1[3],d1[4],d1[5],d1[6],levels_frame[1],levels_frame[2],levels_frame[3],levels_frame[4],levels_frame[5],res)
  print(query)
  dbSendQuery(db,query)
}

dempster_shafer<-function(id)
{
  query <- sprintf("select result from demotest_questions where id=%s",id)
  print(query)
  result <- dbGetQuery(db,query)
  query1 <- sprintf("select ratio from demotest_questions where id=%s",id)
  query2 <- sprintf("select probability from demotest_questions where id=%s",id)
  query3 <- sprintf("select geometry from demotest_questions where id=%s",id)
  query4 <- sprintf("select pNc from demotest_questions where id=%s",id)
  query5 <- sprintf("select aptitude from demotest_questions where id=%s",id)
  ratio <- dbGetQuery(db,query1)
  probability <- dbGetQuery(db,query2)
  geometry <- dbGetQuery(db,query3)
  pNc <- dbGetQuery(db,query4)
  aptitude <- dbGetQuery(db,query5)
  d <- c(ratio,probability,geometry,pNc,aptitude)
  print("hello there")
  print(d)
  calc_prob(result,d)
}

m_dempster_shafer<-function(id,data_base)
{
  table_name <- paste0("demotest_",data_base)
  query <- sprintf("select result from %s where id=%s",table_name,id)
  print(query)
  result <- dbGetQuery(db,query)
  query1 <- sprintf("select ratio from %s where id=%s",table_name,id)
  query2 <- sprintf("select probability from %s where id=%s",table_name,id)
  query3 <- sprintf("select geometry from %s where id=%s",table_name,id)
  query4 <- sprintf("select pNc from %s where id=%s",table_name,id)
  query5 <- sprintf("select aptitude from %s where id=%s",table_name,id)
  ratio <- dbGetQuery(db,query1)
  probability <- dbGetQuery(db,query2)
  geometry <- dbGetQuery(db,query3)
  pNc <- dbGetQuery(db,query4)
  aptitude <- dbGetQuery(db,query5)
  d <- c(ratio,probability,geometry,pNc,aptitude)
  print("hello there")
  print(d)
  calc_prob(result,d)
}

m2_dempster_shafer<-function(id,data_base)
{
  table_name <- paste0("mocktest2_test",data_base)
  query <- sprintf("select result from %s where id=%s",table_name,id)
  print(query)
  result <- dbGetQuery(db,query)
  query1 <- sprintf("select ratio from %s where id=%s",table_name,id)
  query2 <- sprintf("select probability from %s where id=%s",table_name,id)
  query3 <- sprintf("select geometry from %s where id=%s",table_name,id)
  query4 <- sprintf("select pNc from %s where id=%s",table_name,id)
  query5 <- sprintf("select aptitude from %s where id=%s",table_name,id)
  ratio <- dbGetQuery(db,query1)
  probability <- dbGetQuery(db,query2)
  geometry <- dbGetQuery(db,query3)
  pNc <- dbGetQuery(db,query4)
  aptitude <- dbGetQuery(db,query5)
  d <- c(ratio,probability,geometry,pNc,aptitude)
  print("hello there")
  print(d)
  calc_prob(result,d)
}
m3_dempster_shafer<-function(id,data_base)
{
  table_name <- paste0("mocktest3_test",data_base)
  query <- sprintf("select result from %s where id=%s",table_name,id)
  print(query)
  result <- dbGetQuery(db,query)
  query1 <- sprintf("select ratio from %s where id=%s",table_name,id)
  query2 <- sprintf("select probability from %s where id=%s",table_name,id)
  query3 <- sprintf("select geometry from %s where id=%s",table_name,id)
  query4 <- sprintf("select pNc from %s where id=%s",table_name,id)
  query5 <- sprintf("select aptitude from %s where id=%s",table_name,id)
  ratio <- dbGetQuery(db,query1)
  probability <- dbGetQuery(db,query2)
  geometry <- dbGetQuery(db,query3)
  pNc <- dbGetQuery(db,query4)
  aptitude <- dbGetQuery(db,query5)
  d <- c(ratio,probability,geometry,pNc,aptitude)
  print("hello there")
  print(d)
  calc_prob(result,d)
}

get_dataframe<- function(v,n){
  v <- as.numeric(v)
  data <- c(0,0,0,0,0,0)
  print("inside insert_prob")
  if(n==0){
    print("question attempted wrong")
    #v in low
    data[1] <- v
    rem <- (1-v)*10
    if(v == 0.8 ){
      first_part <- 0.2
      second_part <- 0
      
      data[4] <- first_part
      data[6] <- second_part
    }
    else if(rem %% 2 ==0){
      #if even 
      first_part <- (rem/2)+1
      second_part <- (rem/2)-1
      
      data[4] <- first_part/10
      data[6] <- second_part/10
    }
    else{
      if(v==0.9){
        first_part <- 1
        second_part <- 0
      }
      else{
        first_part <- (rem/2)-0.5
        second_part <- rem - first_part
      }
      data[6] <- first_part/10
      data[4] <- second_part/10
    }
  }
  else{
    #v in master 
    print("question attempted correctly")
    data[3] <- v
    rem <- (1-v)*10
    print(rem)
    if(v == 0.8 ){
      first_part <- 0.2
      second_part <- 0
      
      data[6] <- first_part
      data[5] <- second_part
    }
    else if(rem %% 2 ==0){
      #if even 
      first_part <- (rem/2)-1
      second_part <- (rem/2)+1
      
      data[5] <- first_part/10
      data[6] <- second_part/10
    }
    else{
      if(v==0.9){
        first_part <- 1
        second_part <- 0
      }
      else{
        first_part <- (rem/2)-0.5
        second_part <- rem - first_part
      }
      data[5] <- first_part/10
      data[6] <- second_part/10
    }
  }
  return (data)
}
calc_den <- function(data1,data2){
  print("inside calc_den")
  den1 <- (data1[1]*data2[2])+(data1[1]*data2[3])+(data1[1]*data2[6])
  den2 <- (data1[2]*data2[1])+(data1[2]*data2[3])+(data1[2]*data2[5])
  den3 <- (data1[3]*data2[1])+(data1[3]*data2[2])+(data1[3]*data2[4])
  den4 <- (data1[4]*data2[3])
  den5 <- (data1[5]*data2[2])
  den6 <- (data1[6]*data2[1])
  
  print(den1)
  print(den2)
  print(den3)
  print(den4)
  print(den5)
  print(den6)
  den <- (den1+den2+den3+den4+den5+den6)
  print(den)
  den7 <- 1.00-den
  print(den7)
  return(den7)
}

calc_numr <- function(data1,data2,den){
  print("inside calc_numr")
  numr1 <- (data1[1]*data2[1])+(data1[1]*data2[4])+(data1[1]*data2[5])+(data1[1]*data2[6])+(data1[4]*data2[1])+(data1[4]*data2[5])+(data1[5]*data2[1])
  numr2 <- (data1[2]*data2[2])+(data1[2]*data2[4])+(data1[2]*data2[6])+(data1[4]*data2[2])+(data1[4]*data2[6])+(data1[6]*data2[2])+(data1[6]*data2[4])
  numr3 <- (data1[3]*data2[3])+(data1[3]*data2[5])+(data1[3]*data2[6])+(data1[5]*data2[3])+(data1[5]*data2[6])+(data1[6]*data2[3])+(data1[6]*data2[5])
  numr4 <- (data1[4]*data2[4])
  numr5 <- (data1[5]*data2[5])
  numr6 <- (data1[6]*data2[6])
  
  print(numr1)
  print(numr2)
  print(numr3)
  print(numr4)
  print(numr5)
  print(numr6)
  
  print("you now and see m1 xor m2 values")
  m1_xor_m2 <- c((numr1/den),(numr2/den),(numr3/den),(numr4/den),(numr5/den),(numr6/den))
  print(m1_xor_m2)
  m_index=1
  for(k in 2:5){
    if(m1_xor_m2[k]>m1_xor_m2[m_index]){
      m_index = k
    }
  }
  print(m_index)
  print(m1_xor_m2[m_index])
  m <- max(m1_xor_m2)
  print("well max is")
  print(m)
  print(m1_xor_m2[m_index])
  val <- m *100
  print(val)
  p <- floor(val %% 10)
  print(p)
  
  if(p < 5 ){
    res <- (floor(m*10))/10
  }
  else{
    
    res <- (ceiling(m*10))/10
  }
  print(res)
  data_frame_result <- c(m_index,res)
  return(data_frame_result)
}
get_levels <- function(){
  result<-dbGetQuery(db,"select * from studentmodel_final where mail_id='shravyayadavk@gmail.com'")
  print(result)
  
  query <- sprintf("select mailid from prsnt_student where name like 'shravya';")
  res <- dbGetQuery(db,query)
  print(res)
  query1 <- sprintf("select ratio_level from studentmodel_final where mail_id = '%s'",res)
  print(query1)
  r <- dbGetQuery(db,query1)
  print(r)
  
  query2 <- sprintf("select probability_level from studentmodel_final where mail_id =' %s'",res)
  print(query2)
  p <- dbGetQuery(db,query2)
  print(p)
  
  query3 <- sprintf("select geometry_level from studentmodel_final where mail_id =' %s'",res)
  print(query3)
  g <- dbGetQuery(db,query3)
  print(g)
  
  query4 <- sprintf("select pNc_level from studentmodel_final where mail_id ='%s'",res)
  print(query4)
  pNc <- dbGetQuery(db,query4)
  print(pNc)
  
  query5 <- sprintf("select aptitude_level from studentmodel_final where mail_id = '%s'",res)
  print(query5)
  a <- dbGetQuery(db,query5)
  print(a)
  h <- c(as.numeric(r),as.numeric(p),as.numeric(g),as.numeric(pNc),as.numeric(a))
  print(h)
  return(h)
}
get_plot_data <- function(){
  result<-dbGetQuery(db,"select * from studentmodel_final where mail_id='shravyayadavk@gmail.com'")
  print(result)
  
  query <- sprintf("select mailid from prsnt_student where name like 'shravya';")
  res <- dbGetQuery(db,query)
  print(res)
  query1 <- sprintf("select ratio from studentmodel_final where mail_id = '%s'",res)
  print(query1)
  r <- dbGetQuery(db,query1)
  print(r)
  
  query2 <- sprintf("select probability from studentmodel_final where mail_id =' %s'",res)
  print(query2)
  p <- dbGetQuery(db,query2)
  print(p)
  
  query3 <- sprintf("select geometry from studentmodel_final where mail_id =' %s'",res)
  print(query3)
  g <- dbGetQuery(db,query3)
  print(g)
  
  query4 <- sprintf("select pNc from studentmodel_final where mail_id ='%s'",res)
  print(query4)
  pNc <- dbGetQuery(db,query4)
  print(pNc)
  
  query5 <- sprintf("select aptitude from studentmodel_final where mail_id = '%s'",res)
  print(query5)
  a <- dbGetQuery(db,query5)
  print(a)
  h <- c(as.numeric(r),as.numeric(p),as.numeric(g),as.numeric(pNc),as.numeric(a))
  print(h)
  return(h)
}


get_data_frame <- function(probs,x){
  print(probs)
  df <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  for(j in 1:5){
    print(j)
    level <- probs[j]
    print(level)
    i = level
    new_x <- x() + level
    x(new_x)
    print("NOW STARTED ;)")
    print(x())
    while(i<=6){
      print("value of i is:")
      print(i)
      if(i%%2==0){
        #if i is even
        df[x()] <- 1
        new_x <- x() + 1
        x(new_x)
        print("now value of x is")
        print(x())
        print(df)
        i=i+1
      }
      else{
        #if i is odd
        df[x()] <-1
        print("df contents are")
        print(df)
        new_x <- x() + 1
        x(new_x)
        print("now value of X is")
        print(x())
        
        
        df[x()] <-1
        print("df contents are")
        print(df)
        new_x <- x() + 1
        x(new_x)
        print("now value of X is")
        print(x())
        i=i+2
      }
    }
    new_x <- x() - 1
    x(new_x)
    print("now value of X is")
    print(x())
  }
  print("well now final df is")
  print(df)
  
  return(df)
}

get_next_level <- function(data){
  d <- c(0,0,0,0,0,0)
  #to construct a vector 
  for (i in data){
    d[i] = d[i] + 1
  }
  print("d vector contents are:")
  print(d)
  #get max
  max_index = 1
  for(i in 2:6){
    if(d[i] > d[max_index]){
      max_index = i
    }
  }
  print("max value is:")
  print(max_index)
  print(max_index)
}