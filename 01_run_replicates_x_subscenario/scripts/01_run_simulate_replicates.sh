#!/bin/bash

# ------------------------------------------------------------------------------
#      Run 01_simulate_replicates.R for each subscenario of a main scenario
# ------------------------------------------------------------------------------

# All scenarios (uncomment according to main scenario run)
PARAM_FILE="../../00_define_scenarios/outputs/scenarios.01.csv"
# PARAM_FILE="../../00_define_scenarios/outputs/scenario.01.csv"
# PARAM_FILE="../../00_define_scenarios/outputs/scenario.10.csv"

# Num of subscenarios
n=$(cat  $PARAM_FILE | wc -l)

export PARAM_FILE

seq 1 5 | xargs -P 4 -I {} bash -c '
  LINE=$(awk -v row={} "NR==row {print \$0}" "$PARAM_FILE")
  echo $LINE
  read -r main_scenario main_scenario_val sub_sce_varying_par sub_sce_varying_par_value \
  alpha_G alpha_U beta_U beta_A beta_X beta_XA gamma_0 gamma_X gamma_A gamma_XA gamma_U <<< "$LINE"
  echo "Running subscenario {} of main $main_scenario"
  Rscript ../scripts/01_simulate_replicates.R {} $main_scenario
'



# for i in $(seq 1 $n)
# do

# # Row for scenario
# LINE=$(awk -v row=$i 'NR==row {print $0}' "$PARAM_FILE")
# # Split line into args
# read -r main_scenario main_scenario_val sub_sce_varying_par sub_sce_varying_par_value \
# alpha_G alpha_U beta_U beta_A beta_X beta_XA gamma_0 gamma_X gamma_A gamma_XA gamma_U <<< "$LINE"
# 
# # Print scenario args
# echo "Running task $i with parameters: main_scenario = $main_scenario, main_scenario_val = $main_scenario_val, sub_sce_varying_par = $sub_sce_varying_par sub_sce_varying_par_value = $sub_sce_varying_par_value" 

# Run the R script with index of row containing args x scenario
# Rscript ../scripts/01_simulate_replicates.R "$i" 

# done