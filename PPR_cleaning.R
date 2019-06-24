library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)

setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")

clean <- function(vector) {
  return (as.numeric(str_remove_all(trimws(vector), "\\D")))
  }

individual_cases <- 
  read.csv('Individual Cases CAP.csv', stringsAsFactors = FALSE) %>% 
  rename(Year = ï..Fiscal.Year) %>% 
  mutate(ID = factor(paste(Year, State, Name, sep = ', '))) %>% 
  mutate(State = as.character(trimws(State)),
         agency_name = Name,
         #renaming/cleaning section A, related to individual counts
         a_total_indiv = clean(Total.individuals.served),
         a_multiple_cases = clean(Individuals.Multiple.Cases),
          
         #renaming/cleaning section B, related to problem areas
         b_1_info_request = clean(Individual.requests.information),
         b_2_comm_prob = clean(Communication.problems.between.individual.and.counselor),
         b_3_services_conflict = clean(Conflict.about.services.to.be.provided),
         b_4_app_process = clean(Related.to.application.eligibility.process),
         b_5_priority_category = clean(Related.to.assignment.to.order.of.selection.priority.category), 
         b_6_IPE = clean(Related.to.IPE.development.implementation),
         b_7_living_services = clean(Related.to.independent.living.services),
         b_8_non_rehabil_act = clean(Non.Rehabilitation.Act.related),
         b_9_rehabil_act = clean(Other.Rehabilitation.Act.related.problems),
         b_10_title_I_ADA = clean(Related.to.Title.I.of.the.ADA),
         #renaming/cleaning section C, related to strategy for closing
         c_1_short_assistance = clean(Information.referral..pre.2014..or.Short.Term.Technical.Assistance..2015..),
         c_2_investigate = clean(Advisory.interpretational..pre.2014..or.Investigation.Monitoring..2015..),
         c_3_negotiation = clean(Negotiation),
         c_4_alt_disput_res = clean(Alternative.dispute.resolution),
         c_5_informal_review = clean(Administrative.informal.review),
         c_6_formal_review = clean(Formal.appeal.fair.hearing),
         c_7_legal = clean(Legal.remedy),
         c_8_num_closed = clean(Int.Strategies.Total),
         #renaming/cleaning sectoin d, related to reasons for closing
         d_1_favorable = clean(All.issues.resolved.in.individuals.favor),
         d_2_some_favor = clean(Some.issues.resolved.in.individuals.favor),
         d_3_approp = clean(CAP.determines.VR.agency.position.decision.appropriate),
         d_4_legal_merit = clean(Individuals.case.lacks.legal.merit.),
         d_5_alt_rep = clean(Individual.chose.alternative.representation),
         d_6_withdrew = clean(Individual.decided.not.to.pursue..to.2014...or.withdrew.complaint..2015..),
         d_7_not_favor = clean(Appeals.unsuccessful..to.2014..or.Issue.not.resolved.in.clients.favor..2015..),
         d_8_not_needed = clean(CAP.services.not.needed.due.to.individuals.death..relocation..etc.),
         d_9_not_resp = clean(Individual.refused.to.cooperate.with.CAP),
         d_10_lack_resources = clean(CAP.unable.to.take.case.due.to.lack.of.resources),
         d_11_conflict = clean(Conflict.Interest),
         d_12_other = clean(Other),
        
         #renaming/cleaning section e, related to results achieved for individuals
         e_1_explanation = clean(Controlling.law.policy.explained.to.individual),
         e_2_app_completed = clean(Application.for.services.completed.),
         e_3_determination_exped = clean(Eligibility.determination.expedited),
         e_4_individ_partip = clean(Individual.participated.in.evaluation),
         e_5_IPE_proceeded = clean(IPE.developed.implemented),
         e_6_comm_restab = clean(Communication.re.established.between.individual.and.other.party),
         e_7_assigned_new_counsel = clean(Individual.assigned.to.new.counselor.office),
         e_8_alt_resource_ident = clean(Alternative.resources.identified.for.individual),
         e_9_ADA_504_EEO_OCR = clean(ADA.504.EEO.OCR..complaint.made),
         e_10_other = clean(Other.1),
         e_cases_closed = e_1_explanation + e_2_app_completed + e_3_determination_exped + e_4_individ_partip + e_5_IPE_proceeded +
           e_6_comm_restab + e_7_assigned_new_counsel + e_8_alt_resource_ident + e_9_ADA_504_EEO_OCR + e_10_other
         
  ) %>% 
  replace_na(list(d_1_favorable = 0,
                  d_2_some_favor = 0,
                  d_3_approp = 0,
                  d_4_legal_merit = 0,
                  d_5_alt_rep = 0,
                  d_6_withdrew = 0,
                  d_7_not_favor  = 0,
                  d_8_not_needed  = 0,
                  d_9_not_resp = 0,
                  d_10_lack_resources = 0,
                  d_11_conflict = 0,
                  d_12_other = 0
                  )) %>% 
  #adds all results achieved to measure the number of cases closed
  mutate(d_cases_closed = d_1_favorable + d_2_some_favor + d_3_approp + d_4_legal_merit + d_5_alt_rep + d_6_withdrew + d_7_not_favor + 
         d_8_not_needed + d_9_not_resp + d_10_lack_resources + d_11_conflict + d_12_other)   %>% 
  select(-Status, -Individual.still.being.served.as.of.September.30,
         -Individuals.who.are.still.being.served.as.of.October.1, -Additional.individuals.who.were.served.during.the.year) %>% 
  filter(agency_name != 'Native American Disability Law Center')
  


indiv_cases_stdzed <-
  individual_cases %>% 
  mutate(#workload to be a measure of cases taken on 
         workload = a_total_indiv + a_multiple_cases,
         
         #expresses each reason for closing a case as a ratio fo the number of cases closed
         d_1_per_closed = round(d_1_favorable / workload,2),
         d_2_per_closed = round(d_2_some_favor / workload,2),
         d_3_per_closed = round(d_3_approp / workload,2),
         d_4_per_closed = round(d_4_legal_merit / workload,2),
         d_7_per_closed = round(d_7_not_favor / workload,2),
         #cases closed due to lack of resources may be worth looking at a different way
         d_10_per_closed = round(d_10_lack_resources / workload,2),
         d_11_per_closed = round(d_11_conflict / workload,2)
         ) 

remove(individual_cases)


