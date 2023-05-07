extensions [matrix gis csv rnd table bitmap py]

; global variables
; italy-dataset: GIS dataset representing the shapefile
; distance_table: stores the municipality-to-hospital distances [id_municipality [id_hospital distance] ]
; municipality_table: stores all the information needed to describe the municipality
; population_table: lists of patients (and relevant information) to be involved in the simulation as extracted from the whole population
; province_table, region_table: data on province and distance for computing the r_squared
; comulative_data, comulative_data_region: it contains, for each province/region, the number of patients that move or remain in their region of residence to access the service
; index: counts the progressive number of patients accessing the service in the relevant week/tick
; r_passive, r_active: coefficient of correlation considering the passive and active mobility
globals [italy-dataset distance_table municipality_table population_table province_table region_table comulative_data comulative_data_region r_passive r_active index fileName]

; create the two breeds: people and houses adopted to represent respectively patients and hospitals distributed over the territory
breed [people person]
breed [houses house]

; patient/person attributes
; id_patient: progressive index of the patient when extracted from the population
; id_municipality: ISTAT code of the municipality where the patient resides
; region, province, north: geo characteristics of the patient
; income, education: socio-economic characteristics of the specific patient. education: 1 = secondary school grad, 0 = not graduated
; outcome_intervention_intra, outcome_return_intra, outcome_beds_intra: structural, process and outcome indicators computed using the gravity model
; liability: the probability that the patient travels outside his/her region of residence for accessing the healthcare service
; cared: it tracks whether the patient has been already hospitalized and had hip transplant
; target: the hospital where the patient has been hospitalized and had hip transplant
; week_on: week when the patient required the service
; week_treated: week when the patient has been treated
people-own [id_patient id_municipality region province north age income education waiting satisfaction outcome_intervention_intra outcome_return_intra outcome_beds_intra liability cared target week_on week_treated]

; hospital/territorial attributes
; id_hospital: ISTAT code of the municipality where the hospital belongs
; region, province, north: ... of the hospital
; return, n_return: adjusted percentage and number of patients re-hospitalized within 2 years from the surgery procedure (considering the year 2019 as a reference)
; intervention: number of interventions carried out by the hospital (considering the year 2019 as a reference)
; capacity: it is based on the number of interventions and the number of patients involved in the simulation to track the number of surgery procedures the hospital can host
; Rj: weighted hospital-to-population index that is proportional to the catchment population of the hospital based on the number of individuals and their distance
; prob_weight: instantiated when a patient activated. its value represents the probability that the patient chooses this hospital to be cared
; waiting_week: first week available
; capacity_week: contains the availabilities of the specific waiting_week
houses-own [id_hospital region province north n_return return intervention Rj beds waiting satisfaction prob_weight capacity_week waiting_week capacity]

; information needed to define each patch of the map
; municipality_code: ISTAT code of the municipality that matches with the relevant patch
patches-own [municipality_code]

; clear environment and variables
to setup
  reset-ticks
  clear-all
  clear-turtles
  clear-output
  set comulative_data table:make ; it contains, for each province the number of patients that move/remain to access the service. it is adopted to compare the simulation passive mobility with the theoretical model.
  table:clear comulative_data
  set comulative_data_region table:make ; it contains, for each region the number of patients that move/remain to access the service. it is adopted to compare the simulation active mobility with the theoretical model.
  table:clear comulative_data_region
  let image bitmap:import "database_ABM4healthcare/mobility_scale.png"
  let height bitmap:height image
  let width bitmap:width image
  let legend bitmap:scaled image (width * 0.5) (height * 0.5)
  bitmap:copy-to-drawing legend 600 40
end
; END SETUP

; the name of the output file is composed by the date and time to avoid overwriting of session results.
to setFileName
  set fileName (word "output/output-" date-and-time)
  set fileName remove " " fileName
  set fileName remove "." fileName
  set fileName remove ":" fileName
  set fileName (word fileName ".csv")
end
; END

; load the map of municipalities (GIS vector shapefile) and relevant codes
; the map has been gathered from the ISTAT website and is detailed at municipality level
; for the purpose of our analysis the version of the map is 2011 so that the distances between municipalitis can be computed using the ISTAT distance table (last version of the table is 2011)
to load_gis
  reset-timer
  resize-world 0 250 0 250
  set-patch-size 3
  set italy-dataset gis:load-dataset "database_ABM4healthcare/Com2011/Com2011_WGS84_ASL.shp"
  gis:set-world-envelope gis:envelope-of italy-dataset
  gis:apply-coverage italy-dataset "PRO_COM" municipality_code ; PROC_COM represents the municipality code based on ISTAT year 2011
  ask patches [set pcolor white]
  gis:set-drawing-color grey
  gis:draw italy-dataset 1
  output-print (word "gis: " round timer " seconds")
end
; END LOAD_GIS FUNCTION

; load data: distance and municipality tables
to load_data
  reset-timer
  clear-output
  ; LOAD DISTANCE TABLE
  set distance_table table:make
  table:clear distance_table
  set distance_table load_matrix "database_ABM4healthcare/matrice_distanze.csv"
  ; END LOAD DISTANCE TABLE

  ; LOAD PROVINCE AND REGION TABLE
  set province_table table:make
  table:clear province_table
  set province_table load_matrix "database_ABM4healthcare/dati_province.csv"
  set region_table table:make
  table:clear region_table
  set region_table load_matrix "database_ABM4healthcare/dati_regioni.csv"

  ; load the table with updated information of hospitals and updates the municipality table
  ; note that the update_table lists all the municipalities with updated information
  if update_capacity = TRUE [
    let update_table load_matrix "database_ABM4healthcare/dati_province_upgrade.csv"
    foreach table:keys update_table [
      key -> let info table:get update_table key
      table:put province_table key info
    ]
  ]
  ; END LOAD PROVINCE AND REGION TABLE

  ; LOAD MUNICIPALITY TABLE
  set municipality_table table:make
  table:clear municipality_table
  set municipality_table load_matrix "database_ABM4healthcare/dati_comuni.csv"
  ; load the table with updated information of hospitals and updates the municipality table
  ; note that the update_table lists all the municipalities with updated information
  if update_capacity = TRUE [
    let update_table load_matrix "database_ABM4healthcare/dati_comuni_upgrade.csv"
    foreach table:keys update_table [
      key -> let info table:get update_table key
      table:put municipality_table key info
    ]
  ]
  ; updates the municipality table with territorial information (outcome_intervention, outcome_return, outcome_closest)
  ; these measures are computed in a separate function and not stored in the CSV file so that they can be changed to test the simulation process
  compute_quality_measures
  ; END LOAD MUNICIPALITY DATA
  output-print (word "data: " round timer " seconds")
end
; END LOAD_DATA FUNCTION

; load data of the target population to be involved in the simulation
; two alternatives are proposed.
; 1) load_population_fixed: the application load a specific file "elenco_popolazione.csv" composed by a fixed number of patients. this function is called if the random_queue is set to OFF, so that every simulation is based on the same patients
; 2) load_population_sample: the application load a file that contains the rules for extracting the population. In this case the N patients are extracted in real-time, randomly.
; Both functions create a table with the N patients extracted and involved in the study. The CSV file contains the list of patients extracted from the whole italian population.
; Each row of the file is a potential patient described by characteristics needed to describe him/her and to perform the simulation.
to load_population
  reset-ticks
  ifelse random_queue = true [
    load_population_sample
    ; load_population_fixed
  ] [
    load_population_fixed
    ; load_population_sample
  ]
end

; so that the patients involved are the same in each repeated session
to load_population_fixed
  reset-timer
  set population_table table:make
  table:clear population_table
  set population_table load_matrix "database_ABM4healthcare/elenco_popolazione_fixed.csv"
  ; updates the population table with information of the territory where the patient resides (outcome_intervention, outcome_return, outcome_closest)
  ; these measures are computed in a separate function and not stored in the CSV file so that they can be changed to test the simulation process
  let keys table:keys population_table
  foreach keys [
    key -> let info table:get population_table key ; key is the progressive index identifying the patient
    let municipality_patient table:get info "idMunicipality"
    let info_municipality table:get municipality_table municipality_patient
    table:put info "outcome_intervention_intra" table:get info_municipality "outcome_intervention_intra"
    table:put info "outcome_return_intra" table:get info_municipality "outcome_return_intra"
    table:put info "outcome_beds_intra" table:get info_municipality "outcome_beds_intra"
  ]
  ; once the population is loaded the ticks are resetted before starting the simulation
  output-print (word "population: " round timer " seconds")
end

; so that the patients involved are randomly extracted from the whole population
to load_population_sample
  reset-timer
  set population_table table:make
  table:clear population_table
  let population_table_sample table:make
  table:clear population_table_sample
  set population_table_sample csv:from-file "database_ABM4healthcare/elenco_popolazione_sample.csv"
  let population_table_sample_header first population_table_sample
  let population_table_sample_data but-first population_table_sample
  let population_matrix_sample matrix:from-row-list population_table_sample_data
  let id_index position "id" population_table_sample_header
  let idMunicipality_index position "idMunicipality" population_table_sample_header
  let probability_index position "probability" population_table_sample_header
  let weight_index position "weight" population_table_sample_header
  ; extract the patients based on the number of weeks and sample in each week
  let id matrix:get-column population_matrix_sample id_index
  let municipality matrix:get-column population_matrix_sample idMunicipality_index
  let probability matrix:get-column population_matrix_sample probability_index
  let weight matrix:get-row population_matrix_sample weight_index
  let pairs (map list id probability)
  let pair_list rnd:weighted-n-of-list-with-repeats (2 * n_patients) pairs [ [p] -> last p ]
  let counter 0
  foreach pair_list [
    pair -> let indice item 0 pair
    let row item (indice - 1) population_table_sample_data
    let row_table table:make
    foreach population_table_sample_header [
      a -> let pos position a population_table_sample_header
      let dist item pos row
      let index_item item pos population_table_sample_header
      table:put row_table index_item dist
    ]
    set counter counter + 1
    table:put population_table counter row_table
  ]
  ; updates the population table with information of the territory where the patient resides (outcome_intervention, outcome_return, outcome_closest)
  ; these measures are computed in a separate function and not stored in the CSV file so that they can be changed to test the simulation process
  let keys table:keys population_table
  foreach keys [
    key -> let info table:get population_table key ; key is the progressive index identifying the patient
    let municipality_patient table:get info "idMunicipality"
    let info_municipality table:get municipality_table municipality_patient
    table:put info "outcome_intervention_intra" table:get info_municipality "outcome_intervention_intra"
    table:put info "outcome_return_intra" table:get info_municipality "outcome_return_intra"
    table:put info "outcome_beds_intra" table:get info_municipality "outcome_beds_intra"
  ]
  ; once the population is loaded the ticks are resetted before starting the simulation
  output-print (word "population: " round timer " seconds")
end
; END LOAD POPULATION

; this function read the municipality information (municipality_table) and place the hospital structures in a specific patch over the world (territory)
; each hospital is described by a set of attributes among which the capacity that is adopted to capture if the hospital can host a patient or not in a specific period of time
; the capacity is computed considering the number of interventions done in the year 2019 and the number of patients involved in the simulation
to place_hosps
  reset-timer
  set-default-shape houses "house"
  ask turtles with [shape = "house"] [ die ]
  let keys table:keys municipality_table
  (foreach keys [
    key -> let info table:get municipality_table key
    let has_hospital table:get info "intervention"
    ; if the municipality has a hospital, a new house is created and placed over the relevant patch
    if has_hospital > 0 [
      let polygon gis:find-one-feature italy-dataset "PRO_COM" key
      let centroid gis:location-of gis:centroid-of polygon
      ask patch item 0 centroid item 1 centroid [
        set municipality_code key
        set pcolor white
      ]
      ; creates the house, initializes the variables and places it over the relevant patch
      create-houses 1 [
        set size 3
        set id_hospital key
        set region table:get info "region"
        set province table:get info "province"
        set north table:get info "position"
        set return table:get info "return"
        set n_return table:get info "n_return"
        set Rj table:get info "Rj"
        set intervention table:get info "intervention"
        set beds table:get info "beds"
        set waiting table:get table:get province_table province "waiting"
        set satisfaction table:get table:get province_table province "satisfaction"
        set capacity 0
        set color green
        let my_patch one-of (patches with [municipality_code = key])
        move-to my_patch
      ]
    ]
  ])
  output-print (word "hosps: " round timer " seconds") ; this is useful for determing the time needed for implementing the whole simulation
end
; END PLACE_HOSPS

; prepares the environment, loads the data and initializes hospitals
to set_environment
  setup
  load_gis
  load_data
  load_population
  place_hosps
  setFileName
end
; END

; execute the simulation by placing the hospitals (it generally resets the capacity), placing the patients over the territory and moving the patients toward the hospital
; not that at the moment the simulation stops when all the patients reported in the population data file are cared
; unless the remaining patients are less than those needed for the simulation or the number of ticks required is reached
to go
  output-print (word "table: " table:length population_table " - pats: " n_patients " - n. ticks: " ticks)
  ifelse (n_ticks > ticks) [
    place_pats
    polygon_color
    tick
  ] [
    ifelse (sessions > 0) [
      output-print (word "sessioni: " sessions)
      set sessions sessions - 1
      set_environment
    ] [
      stop
    ]
  ]
end
; END GO

; read the population information (population_table), select the relevant sample (see n_patient input) and place the patients over the world (territory)
; for each patient the target is defined on the basis of the liability index and subsequently moved toward the chosen hospital
to place_pats
  reset-timer
  output-print fileName
  file-open fileName ; this CSV file contains the result of the analysis. It tracks the main territorial information of patients and hospitals as well as other variables.
  if (ticks = 0) [
    file-print "ticks|id_municipality|regpaz|propaz|reghos|prohosp|stay|liability|waiting|satisf|int_intra|ret_intra|beds_intra|a|b|c|week_on|week_treated"
  ]
  ; removes all the patients from the world
  ask turtles with [shape = "person"] [ die ]
  set-default-shape people "person"
  set index 0
  ; update hospital capacity
  let whole_intervention sum [intervention] of houses
  ask houses [
    set capacity capacity + ((intervention * n_patients) / whole_intervention) * 1.05
    ; set capacity_week ((intervention * n_patients) / whole_intervention) * 1.05
    if (waiting_week < ticks + round (waiting / 7)) [
      set waiting_week ticks + round (waiting / 7)
    ]
  ]
  ; it places n_patients turtles (see input) over the territory.
  if (table:length population_table < n_patients) [
    ifelse random_queue = true [
      load_population_sample
    ] [
      load_population_fixed
    ]
  ]
  repeat n_patients [
    ; randomly choose one of the patients contained in the population table and get its information
    let code_picked_patient first table:keys population_table
    if random_queue = true [
      set code_picked_patient one-of table:keys population_table
    ]
    let picked_patient table:get population_table code_picked_patient
    ; find the patch/polygon where to put the turtle
    let key table:get picked_patient "idMunicipality"
    let polygon gis:find-one-feature italy-dataset "PRO_COM" key
    let centroid gis:location-of gis:centroid-of polygon
    ask patch item 0 centroid item 1 centroid [
      set municipality_code key
      set pcolor white ; the patch is colored in white
    ]
    ; create the relevant turtle and set all the needed information
    create-people 1 [
      set size 3
      set id_patient code_picked_patient
      ;set id_patient table:get picked_patient "idPatient"
      set id_municipality table:get picked_patient "idMunicipality"
      set region table:get picked_patient "region"
      set province table:get picked_patient "province"
      set north table:get picked_patient "position"
      set income table:get picked_patient "income"
      set education table:get picked_patient "education"
      set age table:get picked_patient "age"
      set waiting table:get table:get province_table province "waiting"
      set satisfaction table:get table:get province_table province "satisfaction"
      set outcome_intervention_intra table:get picked_patient "outcome_intervention_intra"
      set outcome_return_intra table:get picked_patient "outcome_return_intra"
      set outcome_beds_intra table:get picked_patient "outcome_beds_intra"

      ; liability level: between 0 (the patient is not inclined to move outside his/her region of residence for accessing care) and 1 (the patient is willing to move)
      set liability (0.5751594 + 0.0005169 * waiting - 0.0060547 * satisfaction - 0.0006994 * outcome_intervention_intra + 0.5006558 * outcome_return_intra - 0.0029291 * outcome_beds_intra)

      ; considering that the liability is computed based on a regression model it can be lower than 0 or higher than 1. in both cases the value is normalized between 0 an 1
      if liability < 0 [set liability 0]
      if liability > 1 [set liability 1]
      set color blue ; the patient is colored in blue
      set cared FALSE ; the patient is set as not cared/hospitalized yet
      let my_patch one-of (patches with [municipality_code = key]) ; place the turtle over the patch previously identified
      move-to my_patch
      set index index + 1

      ; move the patient
      set week_on ticks
      let choosed_table get_target id_patient
      let choosed table:get choosed_table 0
      ; just a control that the hospital exists (to be peaceful!)
      ifelse choosed != -1 [
        ; find the patch and place the patient over it
        ; note that the code of the hospital coincides with the code of the municipality where the hospital belongs
        set target one-of houses with [id_hospital = choosed]
        move-to target
        ; removes one place from the capacity of the hospital
        let week_index -1
        set week_treated [waiting_week] of target
        ask target [
          ;set week_index get_available_week capacity_week ticks waiting
          ;let cap item week_index capacity_week
          set capacity_week capacity_week - 1
          set capacity capacity - 1
          if (capacity_week < 0) [
            set capacity_week ((intervention * n_patients) / whole_intervention) * 1.05
            set waiting_week waiting_week + 1
          ]
        ]

        set color black + 6 ; the cared patient is moved and colored in black
        ; save the information on the CSV file
        file-print (word ticks "|" id_municipality "|" region "|" province "|" [region] of target "|" [province] of target "|" (region = [region] of target) "|" liability "|" waiting "|" satisfaction "|" outcome_intervention_intra "|" outcome_return_intra "|" outcome_beds_intra "|" (table:get choosed_table 0) "|" (table:get choosed_table 1) "|" (table:get choosed_table 2) "|" week_on "|" week_treated)
        ; updates the counter for computing the mobility index and colouring the map
        update_counter province region ([region] of target) ([province] of target)
        set cared TRUE ; the patient is set as cared/hospitalized
      ] [
        set cared FALSE ; the patient has not hospital to go trough is set as cared/hospitalized
        file-print (word ticks "|" id_municipality "|" region "|" province "|" "|" "|" "|" liability "|" waiting "|" satisfaction "|" outcome_intervention_intra "|" outcome_return_intra "|" outcome_beds_intra "|" (table:get choosed_table 0) "|" (table:get choosed_table 1) "|" (table:get choosed_table 2) "|" week_on "|" week_treated)
      ]

    ]
    table:remove population_table code_picked_patient ; remove the picked patient so that he/she cannot be extracted twice
  ]
  file-close
  output-print (word "pats: " round timer " seconds")
end
; END PLACE_PATS

; report the hospital choosed by the patient on the basis of the liability of the patient
to-report get_target[idp]
  ; load the patient information
  let my_patient one-of people with [cared = FALSE and id_patient = idp]
  let my_id_municipality [id_municipality] of my_patient
  let my_region [region] of my_patient
  let my_liability [liability] of my_patient
  ; let my_waiting [waiting] of my_patient
  ; let my_satisfaction [satisfaction] of my_patient
  ; let my_north [north] of my_patient
  let my_education [education] of my_patient
  let my_age [age] of my_patient
  let my_income [income] of my_patient
  let distances table:get distance_table my_id_municipality ; list of distances between the patients and all the hospitals
  let keys table:keys distances ; list of hospital codes
  let max_dist max table:values distances
  let std_dist standard-deviation table:values distances
  ; initialize and set the relevant variables
  let weight_values table:make
  table:put weight_values 0 0
  table:put weight_values 1 0
  table:put weight_values 2 0
  let pairs table:make
  table:put pairs "intra" (1 - my_liability)
  table:put pairs "inter" my_liability
  table:put weight_values 1 (1 - my_liability)
  table:put weight_values 2 my_liability
  let stay first rnd:weighted-one-of-list table:to-list pairs [ [p] -> last p ]
  ; first step: should I stay or should I go?
  ifelse (regional_border = TRUE) [
    ifelse (stay = "intra") [
      let total_return (sum [n_return] of houses with [region = [ region ] of myself])
      ask houses with [region = [ region ] of myself] [ ; all hospital variables directly accessible. Agent variables must be accessed under myself (see same_region as example)
        let dist table:get distances id_hospital ; get the distance
        ifelse (dist >= 0) [
          ifelse (waiting_week > 0) [
            let multiplier std_dist
            if (my_education = 1) [set multiplier multiplier * 2]
            if (my_age != 2) [set multiplier multiplier * 2]
            if (my_income > 18000) [set multiplier multiplier * 2]
            ; if (my_north != 2) [set multiplier multiplier * 2]
            let p_weight get_weight dist (multiplier / 4) ; instead of 8
            let weight p_weight * Rj
            let int_value (0.0006994 * weight * intervention)
            let ret_value ((0.5006558 * weight * return * n_return) / total_return)
            let bed_value (0.0029291 * weight * beds)
            let wait_value (0.0005169 * (table:get table:get province_table province "waiting")) ; let wait_value (0.0005169 * ((ticks - waiting_week) * 7)) ;
            let sati_value (0.0060547 * (table:get table:get province_table province "satisfaction"))
            set prob_weight (1 - 0.5751594) + int_value - ret_value + bed_value - wait_value + sati_value
            set prob_weight capacity * prob_weight ; capacity?
            if (prob_weight < 0) [ set prob_weight 0 ]
          ] [
            set prob_weight 0
          ]
        ] [
          set prob_weight 0
        ]
      ]
      let hosp rnd:weighted-one-of houses with [region = [ my_region ] of myself] [prob_weight]
      table:put weight_values 0 [id_hospital] of hosp
    ] [
      let total_return (sum [n_return] of houses with [region != [ region ] of myself])
      ask houses with [region != [ region ] of myself] [ ; all hospital variables directly accessible. Agent variables must be accessed under myself (see same_region as example)
        let dist table:get distances id_hospital ; get the distance
        ifelse (dist >= 0) [
          ifelse (waiting_week > 0) [
            let multiplier std_dist
            if (my_education = 1) [set multiplier multiplier * 2]
            if (my_age != 2) [set multiplier multiplier * 2]
            if (my_income > 18000) [set multiplier multiplier * 2]
            ; if (my_north != 2) [set multiplier multiplier * 2]
            let p_weight get_weight dist (multiplier / 4)
            let weight p_weight * Rj
            let int_value (0.0006994 * weight * intervention)
            let ret_value ((0.5006558 * weight * return * n_return) / total_return)
            let bed_value (0.0029291 * weight * beds)
            let wait_value (0.0005169 * (table:get table:get province_table province "waiting")) ; let wait_value (0.0005169 * ((ticks - waiting_week) * 7)) ;
            let sati_value (0.0060547 * (table:get table:get province_table province "satisfaction"))
            set prob_weight (1 - 0.5751594) + int_value - ret_value + bed_value - wait_value + sati_value
            set prob_weight capacity * prob_weight
            if (prob_weight < 0) [ set prob_weight 0 ]
          ] [
            set prob_weight 0
          ]
        ] [
          set prob_weight 0
        ]
      ]
      let hosp rnd:weighted-one-of houses with [region != [ my_region ] of myself] [prob_weight]
      table:put weight_values 0 [id_hospital] of hosp
    ]
  ] [
    let total_return (sum [n_return] of houses)
    ask houses with [region = [ region ] of myself] [ ; all hospital variables directly accessible. Agent variables must be accessed under myself (see same_region as example)
      let dist table:get distances id_hospital ; get the distance
      ifelse (dist >= 0) [
        ifelse (waiting_week > 0) [
          let multiplier std_dist
          if (my_education = 1) [set multiplier multiplier * 2]
          if (my_age != 2) [set multiplier multiplier * 2]
          if (my_income > 18000) [set multiplier multiplier * 2]
          ; if (my_north != 2) [set multiplier multiplier * 2]
          let p_weight get_weight dist (multiplier / 4)
          let weight p_weight * Rj
          let int_value (0.0006994 * weight * intervention)
          let ret_value ((0.5006558 * weight * return * n_return) / total_return)
          let bed_value (0.0029291 * weight * beds)
          let wait_value (0.0005169 * (table:get table:get province_table province "waiting")) ; let wait_value (0.0005169 * ((ticks - waiting_week) * 7)) ;
          let sati_value (0.0060547 * (table:get table:get province_table province "satisfaction"))
          set prob_weight (1 - 0.5751594) + int_value - ret_value + bed_value - wait_value + sati_value
          ;let capacity sum capacity_week
          set prob_weight capacity * prob_weight
          if (prob_weight < 0) [ set prob_weight 0 ]
        ] [
          set prob_weight 0
        ]
      ] [
        set prob_weight 0
      ]
    ]
    let hosp rnd:weighted-one-of houses [prob_weight]
    table:put weight_values 0 [id_hospital] of hosp
  ]

  report weight_values
end

; read a csv file and load information on a table of table.
; each row of the master table contains information about a specific municipality
; in general the generic_table is structured as follows: <municipality <characteristic, value> >
; see load_data function for specificities
to-report load_matrix [file_name]
  let counter 0
  let generic_table table:make
  let load_table csv:from-file file_name
  ;let header first load_table
  ;let data but-first load_table
  let header but-first item 0 load_table
  let data but-first load_table
  foreach data [
    row -> let key item 0 row
    set row remove-item 0 row
    let row_table table:make
    foreach header [
      a -> let pos position a header
      let dist item pos row
      let index_item item pos header
      table:put row_table index_item dist
    ]
    table:put generic_table key row_table
    set counter counter + 1
  ]
  output-print table:length generic_table
  report generic_table
end
; END LOAD_MATRIX

; compute the relevant weight considering
; the liability index of the patient, the attraction index of the structure and the distance matrix between the patient and the hospitals
to-report get_weight [my_distance my_factor_distance]
; ACTUAL FORMULA BASED ON A STD DISTANCE
  let b1 1
  let b2 0.1
  let my_weight exp(-(b1 * my_distance * my_distance)/(2 * b2 * my_factor_distance * my_factor_distance))
  report my_weight
end
; END GET_WEIGHT

; for each municipality calculates the quality metrics on the basis of the gravity model (availability of intra- and extra-regional resources weighted by distance)
to compute_quality_measures
  ; file-open "output/municipality_table_quality_measures.csv"
  let municipalities table:keys municipality_table
  ; for each municipality
  foreach municipalities [
    ; get distances and information of the relevant municipality
    my_id_municipality -> let distances table:get distance_table my_id_municipality ; list of distances between the patients and all the hospitals
    let municipality_info table:get municipality_table my_id_municipality
    let my_region table:get municipality_info "region" ; region of the municipality
    let my_province table:get municipality_info "province" ; pronvince of the municipality
    let my_population table:get municipality_info "POP_2011"
    let keys table:keys distances ; list of hospital codes
    ; initialize the variables
    let my_interventions_intra 0
    let my_beds_intra 0
    let my_int_beds_intra 0
    let my_return_intra 0
    let my_n_return_intra 0
    let my_closest_intra 0
    ; for each hospital code
    foreach keys [
      key -> let dist table:get distances key ; get the distance between the municipality and the hospital
      if dist >= 0 [ ; if the hospital can be reached by the municipality
        if table:has-key? municipality_table key [
          ; get the hospital information
          let info table:get municipality_table key
          let h_region table:get info "region"
          let h_intervention table:get info "intervention"
          let h_return table:get info "return"
          let h_n_return table:get info "n_return"
          let h_Rj table:get info "Rj"
          let h_beds table:get info "beds"
          ; calculates the weight considering the distance between the hospital and the patient and multiplies it with the capacity of the hospital
          let p_weight get_weight dist standard-deviation table:values distances
          let weight p_weight * h_Rj
          let same_region my_region = h_region
          ; if the municipality and hospital are connected by driving roads
          if weight > 0 [
            ; intra- and inter- regional indicators are computed considering, for instamce, the number of interventions of the hospital and the weight between the hospital and the municipality
            if same_region = TRUE [
              set my_beds_intra my_beds_intra + (weight * h_beds)
              set my_interventions_intra my_interventions_intra + (weight * h_intervention)
              set my_return_intra my_return_intra + (weight * h_return * h_n_return)
              set my_n_return_intra my_n_return_intra + (h_n_return)
            ]
          ]
        ]
      ]
    ]
    let my_return_intra_ratio 0
    if my_n_return_intra > 0 [
      set my_return_intra_ratio my_return_intra / my_n_return_intra
    ]
    table:put municipality_info "outcome_intervention_intra" my_interventions_intra
    table:put municipality_info "outcome_return_intra" my_return_intra_ratio
    table:put municipality_info "outcome_beds_intra" my_beds_intra
    table:put municipality_table my_id_municipality municipality_info
    ; id|reg|prov|pop|north|inc|edu|int_intra|int_extra|ret_intra|ret_extra|clos_intra|clos_extra|bed_intra|bed_extra
    ; file-print (word my_id_municipality "|" my_region "|" my_province "|" my_population "|" my_interventions_intra "|" my_interventions_extra "|" my_return_intra_ratio "|" my_return_extra_ratio "|" my_beds_intra "|" my_beds_extra)
  ]
  ; file-close
end
; END COMPUTE QUALITY MEASURES

to polygon_color
  py:setup py:python
  let p_actual []
  let p_predict []

  output-print (word "region: " (sum_column comulative_data_region) " - province: " (sum_column comulative_data))

  foreach table:keys comulative_data [
    key -> let info table:get comulative_data key
    let intra item 0 info
    let extra item 1 info
    let mobility_passive extra / (intra + extra)
    let passive table:get table:get province_table key "passive"
    set p_actual insert-item 0 p_actual mobility_passive
    set p_predict insert-item 0 p_predict passive
    let poly gis:find-features italy-dataset "dc_provinc" key
    gis:set-drawing-color black + 10
    if mobility_passive < 0.50 [gis:set-drawing-color black + 11]
    if mobility_passive < 0.45 [gis:set-drawing-color black + 12]
    if mobility_passive < 0.40 [gis:set-drawing-color black + 13]
    if mobility_passive < 0.35 [gis:set-drawing-color black + 14]
    if mobility_passive < 0.30 [gis:set-drawing-color black + 15]
    if mobility_passive < 0.25 [gis:set-drawing-color black + 16]
    if mobility_passive < 0.20 [gis:set-drawing-color black + 17]
    if mobility_passive < 0.15 [gis:set-drawing-color black + 18]
    if mobility_passive < 0.10 [gis:set-drawing-color black + 19]
    if mobility_passive < 0.05 [gis:set-drawing-color white]
    foreach poly [
      munic -> gis:fill munic 2.0
    ]
  ]
  py:set "p_predict" p_predict
  py:set "p_actual" p_actual
  let a_actual []
  let a_predict []
  foreach table:keys comulative_data_region [
    key -> let info table:get comulative_data_region key
    let intra item 0 info
    let extra item 1 info
    let mobility_active extra / (intra + extra)
    let active table:get table:get region_table key "active"
    set a_actual insert-item 0 a_actual mobility_active
    set a_predict insert-item 0 a_predict active
  ]
  py:set "a_predict" a_predict
  py:set "a_actual" a_actual

  (py:run
  "import numpy as np"
  "p_corr_matrix = np.corrcoef(p_actual, p_predict)"
  "p_corr = p_corr_matrix[0,1]"
  "p_R_sq = p_corr**2"
  "a_corr_matrix = np.corrcoef(a_actual, a_predict)"
  "a_corr = a_corr_matrix[0,1]"
  "a_R_sq = a_corr**2"

  )
  set r_passive (py:runresult "p_R_sq")
  set r_active (py:runresult "a_R_sq")

  output-print (word "r_passive: " (round (r_passive * 100)) " - r_active: " (round (r_active * 100)))
end

; compute the number of patients cared in their region or outside their region
; the information are stored in the cumulative_data table to be used for updating the map with colors
to update_counter[my_province my_region my_target_region my_target_province]
  let info [0 0]
  let value_intra 0
  let value_extra 0
  if table:has-key? comulative_data my_province [
    set info table:get comulative_data my_province
    set value_intra item 0 info
    set value_extra item 1 info
  ]
  ifelse my_region = my_target_region [
    set value_intra value_intra + 1
  ] [
    set value_extra value_extra + 1
  ]
  set info replace-item 0 info value_intra
  set info replace-item 1 info value_extra
  table:put comulative_data my_province info

  set info [0 0]
  set value_intra 0
  set value_extra 0
  if table:has-key? comulative_data_region my_target_region [
    set info table:get comulative_data_region my_target_region
    set value_intra item 0 info
    set value_extra item 1 info
  ]
  ifelse my_region = my_target_region [
    set value_intra value_intra + 1
  ] [
    set value_extra value_extra + 1
  ]
  set info replace-item 0 info value_intra
  set info replace-item 1 info value_extra
  table:put comulative_data_region my_target_region info
end
; END UPDATE_COUNTER

; this function is executed when the button test is pressed and is adopted to test functions and istructions
to test
  set fileName (word "output/output-" date-and-time)
  set fileName remove " " fileName
  set fileName remove "." fileName
  set fileName remove ":" fileName
  set fileName (word fileName ".csv")
  output-print fileName
end

to-report sum_column[table]
  let num 0
  foreach table:keys table [
    key -> let info table:get table key
    let extra item 1 info
    let intra item 0 info
    set num num + intra + extra
  ]
  report num
end
@#$#@#$#@
GRAPHICS-WINDOW
327
10
1088
772
-1
-1
3.0
1
10
1
1
1
0
1
1
1
0
250
0
250
1
1
1
ticks
30.0

BUTTON
72
86
135
119
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
196
154
315
199
patients enrolled
index
0
1
11

INPUTBOX
7
144
93
204
n_patients
1000.0
1
0
Number

INPUTBOX
101
144
186
204
n_ticks
104.0
1
0
Number

OUTPUT
1122
51
1869
363
11

BUTTON
40
19
165
52
NIL
set_environment
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1121
11
1184
44
NIL
test
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
11
323
159
356
update_capacity
update_capacity
1
1
-1000

SWITCH
15
248
154
281
random_queue
random_queue
1
1
-1000

TEXTBOX
168
473
321
557
regional_border: control whether to consider the liability (Yes) or to choose directly the target hospital without considering the regional borders (No)
11
0.0
1

SWITCH
6
405
158
438
manage_capacity
manage_capacity
0
1
-1000

PLOT
10
602
311
752
Correlation b/w expected & simulated mobility
# ticks
R-squared
0.0
104.0
0.0
1.0
true
false
"" ""
PENS
"green" 1.0 0 -13840069 false "plotxy ticks r_passive" "plotxy ticks r_passive"
"red" 1.0 0 -5298144 true "" "plotxy ticks r_active"

SWITCH
11
501
154
534
regional_border
regional_border
0
1
-1000

TEXTBOX
164
236
320
292
random_queue: defines whether the population involved in the study is selected randomly (Yes) or is fixed (No)
11
0.0
1

TEXTBOX
170
309
320
379
update_capacity: if set to Yes the analysis loads the hospital updated information with provisional data on interventions, beds, etc.
11
0.0
1

TEXTBOX
169
397
319
453
manage_capacity: if set to Yes it the simulation controls the hospital capacity otherwise no inpatient limits are considered
11
0.0
1

INPUTBOX
1122
379
1277
439
sessions
0.0
1
0
Number

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
