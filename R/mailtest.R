install.packages("sendmailR")

library(sendmailR)



subject <- "Testnachricht"
body <- "This is the result of the test"                     
mailControl=list(smtpServer=host.name, smtpPortSMTP=port)
sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl, verbose=TRUE)


#library(devtools)
#install_github("rpremraj/mailR")

library(mailR)
attach(email_config)
email_config = read_json("config/email1.conf",simplifyVector = TRUE)
send.mail(from = from,
          to = test_to,
          subject = "test test",
          body = "Body of the email",
          smtp = list(host.name = host.name, port = as.numeric(port), tls=TRUE, ssl=FALSE, user.name=from, passwd=pwd),
          authenticate = TRUE,
          encoding = "utf-8",
          send = TRUE,
          debug = TRUE)
detach(email_config)

if(exists("config/email.conf")){
  library(jsonlite)
  email_config = read_json("config/email1.conf",simplifyVector = TRUE)
  with(email_config,
	  send.mail(from = from,
		    to = test_to,
		    subject = "test test",
		    body = "testneu",
		    smtp = email_config,
		    authenticate = TRUE,
		    send = TRUE,
		    debug = FALSE))
}

send.mail(from = "tal.galili@gmail.com",
          to = "tal.galili@gmail.com",
          subject = "Subject of the email",
          body = "Body of the email",
          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "tal.galili", passwd = "PASSWORD", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)



install.packages("sendmailR")

library(sendmailR)


if(exists("config/email1.conf")){
  message = readLines("config/thanks_message.txt")
  
  library(jsonlite)
  email_config = read_json("config/email1.conf",simplifyVector = TRUE)
  attach(email_config)
  sendmail_options("message-content-type" = "text/html")
  mailControl=list(smtpServer=host.name, smtpPortSMTP=port)
  sendmail(from=from,
           to=test_to,
           subject=subject,
           msg=message,
           control=mailControl, 
           verbose=TRUE)
  detach(email_config, unload = TRUE)
  rm(email_config)
}


library(blastula)

email_config
attach(email_config)
create_smtp_creds_key("ogmail",user=from, host=host.name, use_ssl=TRUE, port=port, overwrite = TRUE)
creds_key("ogmail")


test_message



library(blastula)
attach(email_config)
test_message <- prepare_test_message()
test_message %>%
  smtp_send(
    from = from,
    to = test_to,
    subject = "test email2",
    credentials = creds_key("ogmail"),verbose = TRUE
  )

detach(email_config)


message = readLines("config/thanks_message.txt")
library(mailR)
send.mail(from=from,
          to=test_to,
          encoding = "utf-8",
          body = paste(message, collapse = "\n"),
          html = TRUE,
          inline = TRUE,
          subject = subject,
          authenticate = TRUE,
          smtp = list(host.name=host.name, port=as.numeric(email_config$port), tls=T,ssl=F, user.name=from, passwd=pwd), debug = TRUE
)

