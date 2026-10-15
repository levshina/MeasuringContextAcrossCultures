library(reshape2)
library(brms)
library(ggplot2)
library(ggdist)
library(readxl)
data <- read_xlsx("Dataset.xlsx")

table(data$Language)


#Figure 5: distribution of UPOS and Dep_Rel


table(data$UPOS)
data$VERB <- rep("No", nrow(data))
data$VERB[data$UPOS %in% c("AUX", "VERB")] <- "Yes"
data$VERB <- as.factor(data$VERB)
summary(data$VERB)
data$VERB <- relevel(data$VERB, ref = "Yes")
table(data$Dep_Rel)
data$CLAUSE <- rep("Other", nrow(data))
data$CLAUSE[data$Dep_Rel %in% c("root", "parataxis")] <- "Main"
data$CLAUSE[data$Dep_Rel %in% c("acl", "acl:relcl", "advcl", "ccomp", "csubj")] <- "Subord"

data$Construction <- paste(data$VERB, data$CLAUSE, sep = "_")
data$Construction[data$Construction %in% c("No_Main", "Yes_Main")] <- "Main_Predicate"
data$Construction[data$Construction %in% c("No_Subord", "Yes_Subord")] <- "Subord_Predicate"
data$Construction[data$Construction == "No_Other"] <- "Not_Verb"
data$Construction[data$Construction == "Yes_Other"] <- "Other_Verb"

cxn_tab <- table(data$Language, data$Construction)
cxn_prop <- prop.table(cxn_tab, 1)
cxn_long <- melt(cxn_prop)
colnames(cxn_long) <- c("Language", "GramFunction", "Proportion")
cxn_long$Percentage <- 100*cxn_long$Proportion
cxn_long$Language <- factor(cxn_long$Language, levels = c("Chinese", "English", "German", "Hebrew", "Italian", "Japanese", "Portuguese", "Russian", "Tamil"))
cxn_long$GramFunction <- factor(cxn_long$GramFunction, levels = c("Not_Verb", "Other_Verb", "Subord_Predicate", "Main_Predicate"))

cxn_plot <- ggplot(cxn_long, aes(x = Language, y = Percentage, fill = GramFunction))+
  geom_bar(stat = "identity") + scale_fill_manual(name = "GramFunction", values=c("#E69F00", "#0072B2", "#CC79A7", "#009E73")) + 
  theme_bw() 
cxn_plot
#ggsave("Cxn.jpg", cxn_plot, w = 8, h = 4)


#Analysis of Agent expression

agentive_df <- data[data$Agentive == "Yes"&data$Agent_Form != "Unclear",]

prop.table(table(data$Language, data$Agentive), 1)

agentive_df$Agent_Expression <- ifelse(agentive_df$Agent_Form == "Implicit", "Implicit", "Explicit")


#Figure 6

agent_tab <- table(agentive_df$Language, agentive_df$Agent_Expression)
agent_prop <- prop.table(agent_tab, 1)
agent_long <- melt(agent_prop)
colnames(agent_long) <- c("Language", "Agent_Expression", "Proportion")
agent_long$Percentage <- 100*agent_long$Proportion
agent_long$Language <- factor(agent_long$Language, levels = c("German", "English", "Russian", "Tamil", "Portuguese", "Hebrew", "Italian", "Japanese", "Chinese"))

agent_plot <- ggplot(agent_long, aes(x = Language, y = Percentage, fill = Agent_Expression))+
  geom_bar(stat = "identity") + scale_fill_manual(name = "Agent_Expression", values=c("#8856a7", "#9ebcda")) + 
  theme_bw() 
agent_plot
#ggsave("Agent_Expression.jpg", agent_plot, w = 7, h = 4)


agentive_df$Agent_Expression <- as.factor(agentive_df$Agent_Expression)
agentive_df$Language <- as.factor(agentive_df$Language)
agentive_df$Language <- relevel(agentive_df$Language, ref = "German")

#Bayesian regression analysis of agent expression

mypriors <- prior(cauchy(0, 2), class = "b") + prior(normal(0, 1), class = "sd")

agent_brm <- brm(Agent_Expression ~ 0 + Language + (1|Topic) + (1|Newspaper), family = bernoulli, prior = mypriors, data  = agentive_df, iter = 4000)
summary(agent_brm)
ranef(agent_brm)


#Figure 7: posterior distributions 

agent_posteriors <- as_draws_df(agent_brm)[, 1:9]
colnames(agent_posteriors) <- c("German", "Chinese", "English", "Hebrew", "Italian", "Japanese",
                                "Portuguese", "Russian",
                                "Tamil")
agent_posteriors_long <- melt(agent_posteriors)
colnames(agent_posteriors_long) <- c("Language", "logodds")
agent_posteriors_long$Language <- factor(agent_posteriors_long$Language, levels = c("German", "English", "Russian",  "Tamil", "Portuguese", "Hebrew", "Italian", "Chinese", "Japanese"))
agent_posterior_plot <- ggplot(data = agent_posteriors_long, aes(x = logodds, y = Language))+
  stat_halfeye(fill = "skyblue", alpha = 0.5) + theme_bw()  + xlab("Logodds of Implicit Agent")
agent_posterior_plot
ggsave("Agent_posterior1.jpg", agent_posterior_plot, w = 5, h = 5)


#Table 3: Pairwise comparisons between languages

hypothesis(agent_brm, "LanguageRussian = LanguageGerman")
#etc. for all possible pairs


#Figure 8: implicitness  by cxn, with size reflecting proportions of constructions

cxn_agent_all <- data.frame(cxn_long_ag, Proportion_Implicit_Agent = cx_agent_long$Proportion)

cx_agent_plot <- ggplot(cxn_agent_all, aes(y = Language, x = Proportion_Implicit_Agent,  fill = GramFunction, group = GramFunction)) +
  geom_point(size = 15*cxn_agent_all$Proportion, shape = 22, alpha = 0.6) + xlab("Proportion of Implicit Agents") + 
  scale_fill_manual(name = "GramFunction", values=c("#009E73",  "#CC79A7", "#0072B2", "#E69F00")) + 
  xlim(0, 1) + theme_bw()
cx_agent_plot
#ggsave("Agent_Implicit_by_Cxn_with_Size.jpg", cx_agent_plot, w = 8, h = 4)


#Analysis of expression of Time reference

time_df <- data[data$Time_Ref != "Unclear",]

###Proportions of lexical vs. grammatical time reference - exploratory analysis

prop.table(table(time_df$Time_Lex))
prop.table(table(time_df$Time_Gram))
table(time_df$Time_Gram, time_df$Time_Lex)
93/nrow(time_df)

#Implicitness of Time Reference

time_df$Time_Expression <- ifelse(time_df$Time_Gram== "No"& time_df$Time_Lex== "No", "Implicit", "Explicit")

#Figure 9: explicit vs. implicit time reference (grammatical or lexical)

time_tab <- table(time_df$Language, time_df$Time_Expression)
time_prop <- prop.table(time_tab, 1)
time_long <- melt(time_prop)
colnames(time_long) <- c("Language", "Time_Expression", "Proportion")
time_long$Percentage <- 100*time_long$Proportion
time_long$Language <- factor(time_long$Language, levels = c("Hebrew", "Portuguese", "Russian", "German", "Tamil", "Italian", "English", "Japanese", "Chinese"))

time_plot <- ggplot(time_long, aes(x = Language, y = Percentage, fill = Time_Expression))+
  geom_bar(stat = "identity") + scale_fill_manual(name = "Time_Expression", values=c("#8856a7", "#9ebcda")) + 
  theme_bw() 
time_plot
#ggsave("Time_Expression.jpg", time_plot, w = 7, h = 4)

time_df$Time_Expression <- as.factor(time_df$Time_Expression)
time_df$Language <- as.factor(time_df$Language)
time_df$Language <- relevel(time_df$Language, ref = "German")

#Bayesian regression

time_brm <- brm(Time_Expression ~ 0 + Language + (1|Topic) + (1|Newspaper), family = bernoulli, prior = mypriors, data  = time_df, iter = 4000)
summary(time_brm)
ranef(time_brm)

#Figure 10: posterior distributions

time_posteriors <- as_draws_df(time_brm)[, 1:9]
colnames(time_posteriors) <- c("German", "Chinese", "English", "Hebrew", "Italian", "Japanese",
                               "Portuguese", "Russian",
                               "Tamil")
time_posteriors_long <- melt(time_posteriors)
colnames(time_posteriors_long) <- c("Language", "logodds")
time_posteriors_long$Language <- factor(time_posteriors_long$Language, levels = c("Hebrew", "Portuguese", "Russian",  "German", "Tamil", "Italian", "English",  "Japanese", "Chinese"))
time_posterior_plot <- ggplot(data = time_posteriors_long, aes(x = logodds, y = Language))+
  stat_halfeye(fill = "skyblue", alpha = 0.5) + theme_bw()  
time_posterior_plot
#ggsave("Time_posterior.jpg", time_posterior_plot, w = 5, h = 5)

hypothesis(time_brm, "LanguageChinese= LanguageJapanese")
#etc.


#Figure 11: implicitness  by cxn, with size reflecting proportions of constructions

cxn_time_all <- data.frame(cxn_long, Proportion_Implicit_Time = cx_time_long$Proportion)
cxn_time_all$GramFunction <- factor(cxn_time_all$GramFunction, levels = c("Main_Predicate", "Subord_Predicate", "Other_Verb", "Not_Verb"))

cx_time_plot <- ggplot(cxn_time_all, aes(y = Language, x = Proportion_Implicit_Time,  fill = GramFunction)) +
  geom_point(size = 15*cxn_time_all$Proportion, shape = 22, alpha = 0.6) + xlab("Proportion of Implicit Time Reference") + 
  scale_fill_manual(name = "GramFunction", values=c("#009E73",  "#CC79A7", "#0072B2", "#E69F00")) + 
  xlim(0, 1) + theme_bw()

cx_time_plot
#ggsave("Time_Implicit_by_Cxn_with_Size.jpg", cx_time_plot, w = 8, h = 4)

#Sensitivity analysis for English

sen_df <- time_df[time_df$Sensitivity_Exclude == "No",]
sen_tab <- table(sen_df$Language, sen_df$Time_Expression)
sen_prop <- prop.table(sen_tab, 1)
sen_prop

sen_df_mainclause <- sen_df[sen_df$Dep_Rel == "root"&sen_df$UPOS %in% c("VERB", "AUX"),]
sen_tab_mainclause <- table(sen_df_mainclause$Language, sen_df_mainclause$Time_Expression)
sen_prop_mainclause <- prop.table(sen_tab_mainclause, 1)
sen_prop_mainclause
