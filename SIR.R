# Erick Gordón B.
# stochastic, discrete-time, individual contact models (ICMs) SIR model
# Modelling the effects of public health interventions
N <- 4.2 * 1000000 #20000 #
n_steps   <- 45   # days simulations
n_sims    <- 100    # number of simulations
i_num     <- 445  # infected individuals (I), 
r_num     <- 0     # recovered individuals (R) 
s_num     <- (N - i_num - r_num)    # susceptible individuals (S), 
act_rate  <- 5    # mean number of act  between susceptible individuals are exposed to potential infection per day. ** ( isolation )
inf_prob  <- 0.05  # probability of infection at each occasion of exposure ** (hands and face masks)
rec_rate  <- 1/20  # is the probability that an infected person recovers at a given time step ( 1 in 20 days ) 
a_rate    <- (14.0/365)/1000       # arrival rate. demographic processes, including births or immigration  10.5
ds_rate   <- (4.67/365)/1000       # daily crude death rate  ( 7 each 1000 .. over the year ) =->  (7/365)/1000
di_rate   <- (14.0/365)/1000       # 
dr_rate   <- (4.67/365)/1000       #10.5
si_mean_value   <- 4.5   # serial interval (SI). Which is the time between the onset of a primary case and the time of onset in its secondary cases
si_sd_value     <- 3.4   # SD of SI


#beta  = 0.004, # infectious contact rate (/person/day)
#gamma = 0.5    # recovery rate (/day)


# Load EpiModel
suppressMessages(library(EpiModel))

# control.icm() = ontrol parameters 
# nsteps =  number of time steps to be simulated. We’ll specify an SIR model, 
# nsims = número de simulaciones
control <- control.icm(type = "SIR", nsteps = n_steps, nsims = n_sims)


# The simulated population of individuals is initialised via init.icm().
# We’ll specify a total population of 1,000, initially divided into 
#   997 susceptible individuals,              = s.num
#     3 infected individuals,                 = i.num
#     0 no-one in the recovered compartment.  = r.num
init <- init.icm(s.num = s_num, i.num = i_num, r.num = r_num)


# param.icm()
# act.rate = rate at which susceptible individuals are exposed to potential infection
# inf.prob = probability of infection at each occasion of exposure
# rate at which infected individual recover. say from infected to recovered, per unit of time (each day in this case).

# exposure-to-infection rate at 10 times per day = act.rate = 10
# overall probability of infection across all those exposures at only a 5% = nf.prob = 0.05
# recovery rate to 0.05, which will results in a mean time to recovery of about 20 days = rec.rate
# crude death rate for Australia is about 7 per 1,000 population per annum = (7/365)/1000
# arrival rate at 50% higher than the death rate = di.rate = (14/365)/1000

param <- param.icm(inf.prob = inf_prob, act.rate = act_rate, rec.rate = rec_rate, 
									 a.rate = a_rate, ds.rate = ds_rate, di.rate = di_rate, 
									 dr.rate = dr_rate)

# simulate
sim <- icm(param, init, control)
sim
# The default plot gives the number of individuals in each of the S. I and R compartments at each time point (each day, in this simulation
plot(sim)
# plot the flows. si.flow
plot(sim, y = "si.flow", mean.col = "red", qnts.col = "red")


library(incidence)
library(earlyR)
incidence_counts <- as.data.frame(sim, out="mean") %>%
	select(time, si.flow)
incident_case_dates <- incidence_counts %>%
	uncount(si.flow) %>%
	pull(time) 
incidence_all <- incident_case_dates %>%
	incidence(.)
peak_of_epidemic_curve <- find_peak(incidence_all)
incident_case_dates_growth_phase <- incidence_counts %>%
	filter(time <= peak_of_epidemic_curve) %>%
	select(time, si.flow) %>%
	uncount(si.flow) %>%
	pull(time)
incidence_growth_phase <- incident_case_dates_growth_phase %>%
	incidence(., last_date=peak_of_epidemic_curve)

# specify serial interval mean and SD since the last blog
# post new studies have appeared suggesting 4.5 is a better
# mean for the SI
#si_mean <- 4.5
#si_sd <- 3.4

res <- get_R(incidence_growth_phase, si_mean =si_mean_value, si_sd = si_sd_value)
res$R_ml
plot(res, "R")


# ---- social distancing
# The objective of social distancing is to reduce the probability of contact 
# between persons carrying an infection, and others who are not infected, so as to minimize disease transmission, morbidity and ultimately, mortality.



# Now we can run our simulation function in a loop, stepping through the levels of our 
# two “public health intervention” parameters, and collect the results in an omnibus data frame.

# set up an empty data frame to which to append results from
# each simulation
sims_incidence_rates <- tibble(time = integer(0), si.flow = numeric(0), 
															 i.num = numeric(0), act_rate = numeric(0), inf_prob = numeric(0), 
															 total_cases = numeric(0), max_prev = numeric(0), mle_R0 = numeric(0))

# the parameters to step through
act.rates <- c(10, 5, 2)
inf.probs <- c(0.05, 0.025, 0.01)

# loop through the parameter space
for (act.rate in act.rates) {
	for (inf.prob in inf.probs) {
		sims_incidence_rates <- sims_incidence_rates %>% bind_rows(run_sir_sim(inf.prob, 
																																					 act.rate))
	}
}

# create facet columns as descending ordered factors
sims_incidence_rates <- sims_incidence_rates %>% mutate(act_rate_facet_label = paste(act_rate, 
																																										 "exposures per day"), inf_prob_facet_label = paste("Probability of infection\nat each exposure:", 
																																										 																									 inf_prob)) %>% arrange(desc(act_rate)) %>% mutate_at(vars(act_rate_facet_label), 
																																										 																									 																										 funs(factor(., levels = unique(.)))) %>% arrange(desc(inf_prob)) %>% 
	mutate_at(vars(inf_prob_facet_label), funs(factor(., levels = unique(.)))) %>% 
	arrange(desc(act_rate), desc(inf_prob), time)

# add annotation text for each facet
sims_incidence_rates_facet_annotations <- sims_incidence_rates %>% 
	mutate(label = paste("R0 =", format(mle_R0, digits = 3), 
											 "\n", round(100 * total_cases/1000, digits = 0), "% of population infected")) %>% 
	select(inf_prob_facet_label, act_rate_facet_label, label) %>% 
	distinct()

sims_incidence_rates %>% filter(time <= 365) %>% ggplot(aes(x = time, 
																														y = si.flow)) + geom_line(colour = "blue", size = 1.5) + 
	facet_grid(inf_prob_facet_label ~ act_rate_facet_label) + 
	geom_text(data = sims_incidence_rates_facet_annotations, 
						mapping = aes(x = 50, y = 0.8 * max(sims_incidence_rates$si.flow, 
																								na.rm = TRUE), label = label), parse = FALSE, hjust = 0, 
						vjust = 0, size = 3) + labs(x = "Days since start of epidemic", 
																				y = "New cases per day", title = "Modelling of new cases of COVID-19 per day: incidence rate", 
																				subtitle = paste("with varying levels of social mixing (exposures per day)", 
																												 "and probabilities of infection at each exposure")) + 
	theme(legend.position = "top", strip.text = element_text(size = 14))


# Comparing the two simulations, we can see that the effects of the varying levels of the interventions have broadly similar effects in both small (1k) and larger (100k) populations, although the spread not unexpectedly takes longer in the larger population, particularly at the higher levels of social distancing (fewer exposures per day) and at the higher hygiene levels (lower probabilities of infection if exposed).

sims_incidence_rates %>%
	filter(time <= 365) %>%
	ggplot(aes(x=time, y=i.num, fill=act_rate_facet_label)) +
	geom_area(stat="identity", alpha=0.6) +
	facet_grid(inf_prob_facet_label ~ .) +
	labs(x="Days since start of epidemic", y="Prevalent (current number of active) cases",
			 title="Modelling of prevalence of COVID-19 cases per day",
			 subtitle=paste("with varying levels of social mixing (exposures per day)",
			 							 "and probabilities of infection at each exposure")) +
	theme_minimal() +
	theme(legend.position = "top", 
				legend.title = element_blank(),
				strip.text = element_text(size=12)) +
	scale_fill_brewer(type="seq", palette="Oranges")


