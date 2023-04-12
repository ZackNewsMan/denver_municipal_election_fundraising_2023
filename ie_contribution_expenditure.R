library(tidyverse)

library(readr)
ie_contributions_010120_040723 <- read_csv("ie_contributions_010120_040723.csv")
View(ie_contributions_010120_040723)

contribution <- ie_contributions_010120_040723

library(readr)
ie_expenditures_010120_040723 <- read_csv("ie_expenditures_010120_040723.csv")
View(ie_expenditures_010120_040723)

expenditure <- ie_expenditures_010120_040723

#### I want to know who has contributed the most to Mike Johnston IE's #####

  # We need to ID the IE's first by the expenditure data

      expenditure %>% 
      filter(candidate_name == "Mike Johnston") %>% 
      View()

     johnston_ie_spending <- expenditure %>% 
     filter(candidate_name == "Mike Johnston")

      # Most of it is going to "Advancing Denver." Also CWA-COPE Treasury spent money on opposition research 
    
     support_johnston_ie_spending <- johnston_ie_spending %>%
       filter(position == "Support")
     
       support_johnston_ie_spending %>%
       group_by(committee_name) %>% 
       summarize(total = sum(amount)) 

       # Advancing Denver spent $2,234,680 to support Johnston 
          # SQL verifies, $2,234,679.89 from Advancing Denver
       
           # CREATE TABLE "johnston_ie_spending" AS
           # SELECT *
           #   FROM expenditure
           # where candidate_name = "Mike Johnston"
           
           # CREATE TABLE "support_johnston_ie_spending" AS
           # SELECT *
           #   FROM johnston_ie_spending
           # where position = "Support"
           
           # select committee_name, sum(amount)
           # from support_johnston_ie_spending
           #  group by committee_name
       
       # What did they spend it on?
       
       support_johnston_ie_spending %>%
         group_by(purpose) %>% 
         summarize(total = sum(amount))
       
       # Looks like media buys but lets check 
          # Same in SQL
             # select purpose, sum(amount)
             # from support_johnston_ie_spending
             # group by purpose
             
           1140000+739000+200000
           
           # Total was 2079000
           
           (2079000/2234680)*100
       
           # 93% of spending by the IE went to media buys. 
 
########### Kelly Brough IE tracking ################
           
           brough_ie_spending <- expenditure %>% 
             filter(candidate_name == "Kelly Brough")     
            
           support_brough_ie_spending <- brough_ie_spending %>% 
             filter(position == "Support")    
           
           support_brough_ie_spending %>%
             group_by(committee_name) %>% 
             summarize(total = sum(amount)) %>% 
             View()
           
           # The IE A Better Denver spent $984,284 to back Brough. 
            # Looks like mostly ads
           
            # SQL verifies, 984284.16
               # CREATE TABLE "brough_ie_spending" AS
               # SELECT *
               #   FROM expenditure
               # where candidate_name = "Kelly Brough"
               
               
               # CREATE TABLE "support_brough_ie_spending" AS
               # SELECT *
               #   FROM brough_ie_spending
               # where position = "Support"
               
               # select committee_name, sum(amount)
               # from support_brough_ie_spending
               # group by committee_name
           
           # what spend on? 
           
               support_brough_ie_spending %>%
                 group_by(purpose) %>% 
                 summarize(total = sum(amount))
                    
           # For A Better Denver ad totals:
            
           6663+172509+625980 
           
            # Total of 805,152
           
           (805152/984284)*100
           
              # 82% of spending was on media buys
           
   
############# Who funded Johnston IE Advancing Denver? ##############       
           
  # Put together first and last name into one column 
     # To get the columns together: https://www.marsja.se/how-to-concatenate-two-columns-or-more-in-r-stringr-tidyr/      
           
           contribution$full_name <- paste(contribution$contributor_first_name,contribution$contributor_last_name)       
           
     
          advancing_denver_contribution <- contribution %>% 
   filter(recipient_committee == "Advancing Denver") %>% 
   View()
           
           # Highest donor?
          
         advancing_denver_contribution %>%
            group_by(full_name) %>% 
            summarize(total = sum(amount)) %>% 
           View()
           
         # Advancing Denver was bankrolled by about 28 people that include LinkedIn founder Reid Hoffman [https://www.forbes.com/profile/reid-hoffman/?sh=29ce9a4e1849], who gave a total $904,679.90, former DaVita CEO Kent Thiry, who gave $300,000, and Conneticut-based hedge fund manager Steve Mandel, who gave $250,450.


 
############# Who funded Brough IE -- A Better Denver? ############## 
   
             
           better_denver_contribution <- contribution %>% 
           filter(recipient_committee == "A Better Denver") %>% 
           View()
         
         # Highest donor?
         
         better_denver_contribution %>%
           group_by(full_name) %>% 
           summarize(total = sum(amount)) %>% 
           View()     
         
         # Looks like most money came from organizations, not people
         
         better_denver_contribution %>%
           group_by(organization_name) %>% 
           summarize(total = sum(amount)) %>% 
           View()     
         
#### Who funded the IE that helped Diana Romero Campbell win? ######
         
         expenditure %>% 
           filter(candidate_name == "Diana Romero Campbell") %>% 
           View()             
         
          campbell_ie_spending <- expenditure %>% 
           filter(candidate_name == "Diana Romero Campbell")
         
         support_campbell_ie_spending <- campbell_ie_spending %>% 
           filter(position == "Support")    
         
         support_campbell_ie_spending %>%
           group_by(committee_name) %>% 
           summarize(total = sum(amount)) %>% 
           View()         
         
         # Looks like main IE spenders supporting Campbell is Servicios Sigue, a branch ofServicios de La Raza
              #  https://twitter.com/serviciosdlraza/status/1509276370798579719
          #  A main contributor to the One Main Street IE is an organization that says it is comprised of trade union and first responders:   https://www.onemainstreetcolorado.org/about-us/
         
             # CREATE TABLE "campbell_ie_spending" AS
             # SELECT *
             #   FROM expenditure
             # where candidate_name = "Diana Romero Campbell"
             
             # CREATE TABLE "support_campbell_ie_spending" AS
             # SELECT *
             #   FROM campbell_ie_spending
             # where position = "Support"
             
             # select committee_name, sum(amount)
             # from support_campbell_ie_spending
             # group by committee_name
             # order by sum(amount) DESC
             
             # SELECT *
             #  from contribution
             # where recipient_committee = "One Main Street Denver"

#### Who funded the IE that helped Travis Leiker? ######         
                  
         expenditure %>% 
           filter(candidate_name == "Travis Leiker") %>% 
           View()             
         
         leiker_ie_spending <- expenditure %>% 
           filter(candidate_name == "Travis Leiker")
         
         support_leiker_ie_spending <- leiker_ie_spending %>% 
           filter(position == "Support")    
         
         support_leiker_ie_spending %>%
           group_by(committee_name) %>% 
           summarize(total = sum(amount)) %>% 
           View()         
         
         
########### Who funded the largest IE's that helped Leiker? ################
    # Citizens for a Great Denver     
         
         great_denver_contribution <- contribution %>% 
           filter(recipient_committee == "Citizens for a Great Denver") %>% 
           View()
         
         # Highest donor?
         
         great_denver_contribution %>%
           group_by(organization_name) %>% 
           summarize(total = sum(amount)) %>% 
           View()     
         
         # All of it came from another group, "Forward Denver"
          # We have a shell game on our hands. 
         
         contribution %>% 
           filter(recipient_committee == "Forward Denver") %>% 
           View()
         
         
         
    # A Safe and Prosperous Denver     
         
         safe_prosperous_denver_contribution <- contribution %>% 
           filter(recipient_committee == "A Safe and Prosperous Denver") %>% 
           View()
         
         # Highest donor?
         
         safe_prosperous_denver_contribution %>%
           group_by(organization_name) %>% 
           summarize(total = sum(amount)) %>% 
           View()    
         
#### Who funded the IE that helped Lisa Calderon? ######    
    # Lisa Calderon     
         
         
         expenditure %>% 
           filter(candidate_name == "Lisa Calderon") %>% 
           View()             
         
         calderon_ie_spending <- expenditure %>% 
           filter(candidate_name == "Lisa Calderon")
         
         support_calderon_ie_spending <- calderon_ie_spending %>% 
           filter(position == "Support")    
        
         support_calderon_ie_spending %>%
           group_by(committee_name) %>% 
           summarize(total = sum(amount)) %>% 
           View()         
          
         # Lagged in IE spending:
           # Colorado Working Families Party Independent Expenditure Committee 23159.
           # CWA-COPE Treasury                                                  1546
         
         
         23159+1546 
         
          # Total of $24,705 in IE spending supporting Calderon
            # When you add it all up from the support, there are some decimals. $24,704.95
         
         
### how much of contributions to IE's were Fair Election Funds ###