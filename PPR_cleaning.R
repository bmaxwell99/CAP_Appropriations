library(dplyr)
library(magrittr)
library(tidyr)

individual_cases <- 
  read.csv('Individual Cases CAP.csv', stringsAsFactors = FALSE) %>% 
  rename(Year = ï..Fiscal.Year) %>% 
  mutate(ID = factor(paste(Year, State, Name, sep = ', '))) %>% 
  rename(agency_name = Name,
         #renaming section A, related to individual counts
         a_total_indiv = Total.individuals.served,
         a_multiple_cases = Individuals.Multiple.Cases,
         #renaming section B, related to problem areas
         b_info_request = Individual.requests.information,
         b_comm_prob = Communication.problems.between.individual.and.counselor,
         b_services_conflict = Conflict.about.services.to.be.provided,
         b_app_process = Related.to.application.eligibility.process,
         b_priority_category = Related.to.assignment.to.order.of.selection.priority.category, 
         b_IPE = Related.to.IPE.development.implementation,
         b_living_services = Related.to.independent.living.services,
         b_non_rehabil_act = Non.Rehabilitation.Act.related,
         b_rehabil_act = Other.Rehabilitation.Act.related.problems,
         
         #renaming section C, related to strategy for closing
         c_short_assistance = Information.referral..pre.2014..or.Short.Term.Technical.Assistance..2015..,
         c_investigate = Advisory.interpretational..pre.2014..or.Investigation.Monitoring..2015..,
         c_negotiation = Negotiation,
         c_alt_disput_res = Alternative.dispute.resolution,
         c_informal_review = Administrative.informal.review,
         c_formal_review = 
         #d
         Resolved.Out.Favor = Appeals.unsuccessful..to.2014..or.Issue.not.resolved.in.clients.favor..2015..,
         lack.of.resources = CAP.unable.to.take.case.due.to.lack.of.resources
         ) %>% 
  select(-Status, -Other.1, -Individual.still.being.served.as.of.September.30,
         -Individuals.who.are.still.being.served.as.of.October.1, -Additional.individuals.who.were.served.during.the.year)

c_closing_strategy <-
  individual_cases %>% 
  select(Year, State, Name, Short.Term.Technical.Assistance, Investigation.Monitoring, Negotiation, Alternative.dispute.resolution,
         Administrative.informal.review, Formal.appeal.fair.hearing, Legal.remedy, ID)

d_reason_closed <-
  individual_cases %>% 
  select(Year, State, Name, All.issues.resolved.in.individuals.favor, Some.issues.resolved.in.individuals.favor, CAP.determines.VR.agency.position.decision.appropriate,
         Individuals.case.lacks.legal.merit., Resolved.Out.Favor,
         Individual.refused.to.cooperate.with.CAP, lack.of.resources, Conflict.Interest, ID)

i_r <- 
  read.csv('I&R CAP.csv', stringsAsFactors = FALSE)

systemic <-
  read.csv('Systemic Data CAP.csv', stringsAsFactors = FALSE)

demogrpahic <-
  read.csv('demographic data CAP.csv', stringsAsFactors = FALSE)

