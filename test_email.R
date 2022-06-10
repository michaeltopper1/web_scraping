library(tidyverse)
library(gmailr)

test_email <-
  gm_mime() %>%
  gm_to("8059144285@vtext.com") %>%
  gm_from("miketopper123@gmail.com") %>%
  gm_subject("145 Reminder") %>%
  gm_text_body("Eli Review is due today at 11:45pm. Do it or you fail you fucking idiot.")

gm_auth_configure(key = "AIzaSyABdgLBK3-gs-CAjsBZDXGUCFiMQXpk9T4",
                  path = "rental_housing/client_email.json")
gm_send_message(test_email)

