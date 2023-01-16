; extentions imported
extensions [matrix gis csv array rnd table bitmap]

; global variables
; italy-dataset: GIS dataset representing the shapefile
; distance_table: stores the municipality-to-hospital distances [id_municipality [id_hospital distance] ]
; municipality_table: stores all the information needed to describe the municipality
; population_table: lists of patients (and relevant information) to be involved in the simulation as extracted from the whole population
; comulative_data: it contains, for each region, the number of patients that move or remain in their region of residence to access the service
globals [italy-dataset distance_table municipality_table population_table province_table comulative_data r_active r_passive]

; create the two breeds: people and houses adopted to represent respectively patients and hospitals distributed over the territory
breed [people person]

; patient/person attributes
; id_patient: progressive index of the patient when extracted from the population
; id_municipality: ISTAT code of the municipality where the patient resides
; region, province: ... of the municipality where the patient resides - north: reports whether the municipality is located in the north of Italy or not
; income: ... of the specific patient
; education: level of education of the patient. 1 = graduated (at least) from high school, 0 = not graduated
; outcome_closest, outcome_intervention, outcome_return: structural, process and outcome indicators computed using the gravity model
; liability: the probability that the patient travels outside his/her region of residence for accessing the healthcare service
; cared: it tracks whether the patient has been already hospitalized and had hip transplant
; target: the hospital where the patient has been hospitalized and had hip transplant
people-own [id_patient id_municipality region province north income education waiting satisfaction outcome_intervention outcome_closest outcome_return liability cared target outcome_intervention_intra outcome_return_intra outcome_beds_intra]

breed [houses house]
; id_hospital: ISTAT code of the municipality where the hospital belongs
; region, province: ... of the hospital
; return, n_return: adjusted percentage and number of patients re-hospitalized within 2 years from the surgery procedure
; intervention: number of interventions carried out by the hospital
; capacity: it is based on the number of interventions and the number of patients involved in the simulation to track the number of surgery procedures the hospital can host
; Rj: weighted hospital-to-population index that is proportional to the catchment population of the hospital based on the number of individuals and their distance
houses-own [id_hospital region province n_return return intervention capacity total_capacity Rj beds waiting satisfaction]

; information needed to define each patch of the map
; municipality_code: ISTAT code of the municipality that matches with the relevant patch
patches-own [municipality_code]

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

  ; LOAD PROVINCE TABLE
  set province_table table:make
  table:clear province_table
  set province_table load_matrix "database_ABM4healthcare/dati_province.csv"
  ; load the table with updated information of hospitals and updates the municipality table
  ; note that the update_table lists all the municipalities with updated information
  if update_capacity = TRUE [
    let update_table load_matrix "database_ABM4healthcare/dati_province_upgrade.csv"
    foreach table:keys update_table [
      key -> let info table:get update_table key
      table:put province_table key info
    ]
  ]

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
; the CSV file contains the list of patients extracted from the whole italian population.
; each row of the file is a potential patient described by characteristics needed to describe him/her and to perform the simulation
; note that the extraction of the list of patients is performed by an external algorithm
to load_population
  reset-timer
  set population_table table:make
  table:clear population_table
  set population_table load_matrix "database_ABM4healthcare/elenco_popolazione.csv"
  ; updates the population table with information of the territory where the patient resides (outcome_intervention, outcome_return, outcome_closest)
  ; these measures are computed in a separate function and not stored in the CSV file so that they can be changed to test the simulation process
  let keys table:keys population_table
  foreach keys [
    key -> let info table:get population_table key ; key is the progressive index identifying the patient
    let municipality_patient table:get info "idMunicipality"
    let info_municipality table:get municipality_table municipality_patient
    table:put info "outcome_intervention" table:get info_municipality "outcome_intervention"
    table:put info "outcome_return" table:get info_municipality "outcome_return"
    table:put info "outcome_closest" table:get info_municipality "outcome_closest"
    table:put info "outcome_intervention_intra" table:get info_municipality "outcome_intervention_intra"
    table:put info "outcome_return_intra" table:get info_municipality "outcome_return_intra"
    table:put info "outcome_beds_intra" table:get info_municipality "outcome_beds_intra"
  ]
  ; once the population is loaded the ticks are resetted before starting the simulation
  reset-ticks
  set comulative_data table:make ; it contains, for each region the number of patients that move/remain to access the service
  table:clear comulative_data
  output-print (word "population: " round timer " seconds")
end
; END LOAD POPULATION DATA

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
      ; creates the house, place it over the patch with the needed information
      create-houses 1 [
        set size 3
        set id_hospital key
        set region table:get info "region"
        set province table:get info "province"
        set return table:get info "return"
        set n_return table:get info "n_return"
        set Rj table:get info "Rj"
        set intervention table:get info "intervention"
        set beds table:get info "beds"
        set waiting table:get table:get province_table province "waiting"
        set satisfaction table:get table:get province_table province "satisfaction"
        set color green
        set capacity 0
        set total_capacity 0
        let my_patch one-of (patches with [municipality_code = key])
        move-to my_patch
      ]
    ]
  ])
  ; configure the total capacity (year)
  let total_intervention sum [intervention] of houses
  ask houses [
    set total_capacity (round ((intervention * n_patients * n_ticks) / total_intervention)) + 1
    ; set capacity round ((intervention * n_patients) / total_intervention)
    ; set capacity capacity * 1.05 ; add 5% of extra-capacity for each hospital
    set color green
  ]
  output-print (word "hosp1: " (sum [intervention] of houses) " - " sum [total_capacity] of houses)
  output-print (word "hosps: " round timer " seconds") ; this is useful for determing the time needed for implementing the whole simulation
end
; END PLACE_HOSPS

; read the population information (population_table), select the relevant sample (see n_patient input) and place the patients over the world (territory)
to place_pats
  reset-timer
  ; at the beginning the hospitals have their full capacity based on their number of intervention and number of patients involved in the simulation
  ; the hospital is colored in green
  ;let whole_capacity sum [total_capacity] of houses
  let whole_intervention sum [intervention] of houses
  ask houses [
    ; set capacity (round ((total_capacity * n_patients) / whole_capacity)) + 1
    set capacity round ((intervention * n_patients) / whole_intervention) + 1
    ; set capacity capacity * 1.05 ; add 5% of extra-capacity for each hospital
    set color green
  ]

  output-print (word whole_intervention " - " sum [capacity] of houses)

  ; output-print (word "total interv: " total_intervention " - " sum [capacity] of houses)
  ; removes all the patients from the world
  ask turtles with [shape = "person"] [ die ]
  set-default-shape people "person"
  let index 0
  ; it places n_patients turtles (see input) over the territory.
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
      set north table:get picked_patient "north"
      set income table:get picked_patient "income"
      set education table:get picked_patient "education"
      set waiting table:get table:get province_table province "waiting"
      set satisfaction table:get table:get province_table province "satisfaction"
      set outcome_intervention table:get picked_patient "outcome_intervention"
      set outcome_return table:get picked_patient "outcome_return"
      set outcome_closest table:get picked_patient "outcome_closest"
      ; added for the last model
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
    ]
    table:remove population_table code_picked_patient ; remove the picked patient so that he/she cannot be extracted twice
  ]
  output-print (word "pats: " round timer " seconds")
end
; END PLACE_PATS

; move each patient of the relevant tick toward a specific hospital based on the patient's liability index (being cared in his region or move to another region)
; and on the level of attraction of each hospital (distance and capacity)
to move_pats
  reset-timer
  ; the results of each simulation step are stored in the following CSV file structured as follows:
  ; municipality of patient|region of patient|province of patient|region of hospital|province of hospital|remains in his/her region|liability value
  file-open "output/output.csv"
  if (ticks = 0) [
    file-print "ticks|id_municipality|regpaz|propaz|reghos|prohosp|stay|liability|waiting|satisf|int_intra|ret_intra|beds_intra"
  ]

  ; verifies whether there are still patients to be hospitalized otherwise the simulation finishes
  while [count people with [cared = FALSE] > 0] [
    ; select one of the remaining patients (not hospitalized yet)
    let patient_to_move one-of people with [cared = FALSE]
    if random_queue = false [
      let idp min [id_patient] of people with [cared = FALSE]
      set patient_to_move one-of people with [cared = FALSE and id_patient = idp]
    ]
    ask patient_to_move [
      ; select the hospital calling the GET_TARGET function with that returns the code of the hospital choosed by the patient
      let choosed_table get_target id_patient
      let choosed table:get choosed_table 0
      ; just a control that the hospital exists (to be peaceful!)
      ifelse choosed != -1 [
        ; find the patch and place the patient over it
        ; note that the code of the hospital coincides with the code of the municipality where the hospital belongs
        set target one-of houses with [id_hospital = choosed]
        move-to target
        ; removes one place from the capacity of the hospital
        ask target [
          set capacity capacity - 1
          set total_capacity total_capacity - 1
          if capacity <= 0 [
            set color red - 2
          ]
        ]
        set color black + 6 ; the cared patient is moved and colored in black
        ; save the information on the CSV file
        file-print (word ticks "|" id_municipality "|" region "|" province "|" [region] of target "|" [province] of target "|" (region = [region] of target) "|" liability "|" waiting "|" satisfaction "|" outcome_intervention_intra "|" outcome_return_intra "|" outcome_beds_intra "|" (table:get choosed_table 0) "|" (table:get choosed_table 1) "|" (table:get choosed_table 2))
        ; updates the counter for computing the mobility index and colouring the map
        update_counter province region [region] of target
        set cared TRUE ; the patient is set as cared/hospitalized
      ] [
        set cared TRUE ; the patient has not hospital to go trough is set as cared/hospitalized
        file-print (word ticks "|" id_municipality "|" region "|" province "|" "|" "|" "|" liability "|" waiting "|" satisfaction "|" outcome_intervention_intra "|" outcome_return_intra "|" outcome_beds_intra "|" (table:get choosed_table 0) "|" (table:get choosed_table 1) "|" (table:get choosed_table 2))
      ]
    ]
  ]
  file-close
  output-print (word "simulation: " round timer " seconds")
end
; END MOVE PATIENTS

; clear the environment and other stuff
to setup
  reset-ticks
  clear-all
  clear-turtles
  clear-output
  set comulative_data table:make ; it contains, for each region the number of patients that move/remain to access the service
  table:clear comulative_data

  let image bitmap:import "database_ABM4healthcare/mobility_scale.png"
  let height bitmap:height image
  let width bitmap:width image
  let legend bitmap:scaled image (width * 0.5) (height * 0.5)
  bitmap:copy-to-drawing legend 600 40
end
; END SETUP

; execute the simulation by placing the hospitals (it generally resets the capacity), placing the patients over the territory and moving the patients toward the hospital
; not that at the moment the simulation stops when all the patients reported in the population data file are cared
; unless the remaining patients are less than those needed for the simulation or the number of ticks required is reached
to go
  output-print (word "table: " table:length population_table " - pats: " n_patients " - n. ticks: " ticks)
  ifelse (n_ticks > ticks) [
    output-print (word "capacity pre: " sum [capacity] of houses)
    place_pats
    output-print (word "capacity post: " sum [capacity] of houses)
    move_pats
    polygon_color
    tick
  ] [
    stop
  ]
end
; END GO

;
to set_environment
  setup
  load_gis
  load_data
  load_population
  place_hosps
  ; go
end

; report the hospital choosed by the patient on the basis of the liability of the patient. In particular the algorithm:
; 1) verify wheater the patient remains in his region or migrate for healthcare service. Note that the probability that the patient remains in his region or go outside is described by the liability index
; 2) extract the hospital from the set of intra- or inter-regional hospitals. the extraction is based on the attactive of the hospital and the distance between the hospital and the patient
; 3.1) the total accessibility of intra- hospitals is proportional to (1 - liability) that is the probability of the patient to stay in his/her region,
; 3.2) the total accessibility of inter- hospital is proportional to (liability) that is the probability of the patient to go outside the region of residence
; 4) the simulation extract the hospital among the intra- and the inter-regional hospitals, each one with a specific weight as calculated in points 3.1 and 3.2.
; 5) not that each hospital may be saturated (capacity lower than or equal to 0). in this case its probability is 0 and cannot be accessed by the patient (unless the switch manage_capacity is set to FALSE in this case the capacity is infinitive)
to-report get_target [idp]
  ; load the patient information
  let my_patient one-of people with [cared = FALSE and id_patient = idp]
  let my_id_municipality [id_municipality] of my_patient
  let my_region [region] of my_patient
  let my_liability [liability] of my_patient
  let my_waiting [waiting] of my_patient
  let my_satisfaction [satisfaction] of my_patient
  ; initialize and set the relevant variables
  let weight_values table:make
  table:put weight_values 0 0
  table:put weight_values 1 0
  table:put weight_values 2 0
  let weight_list_intra table:make ; list of weigths considering the intra-regional hospitals
  let weight_list_extra table:make ; list of weigths considering the extra-regional hospitals
  let distances table:get distance_table my_id_municipality ; list of distances between the patients and all the hospitals
  let keys table:keys distances ; list of hospital codes
  let min_dist min table:values distances
  let max_dist max table:values distances
  let dev_dist standard-deviation table:values distances
  ; for each hospital code
  foreach keys [
    key -> let dist table:get distances key ; get the distance
    if dist >= 0 [
      let my_hospital one-of houses with [id_hospital = key] ; get the hospital with the specific key
      ; if the hospital exists (this control may be removed)
      if my_hospital != nobody [
        ; load and set the hospital information and variables
        let h_capacity [capacity] of my_hospital
        let h_total_capacity [total_capacity] of my_hospital
        let h_region [region] of my_hospital
        let h_intervention [intervention] of my_hospital
        let h_return [return] of my_hospital
        let h_n_return [n_return] of my_hospital
        let h_Rj [Rj] of my_hospital
        let h_beds [beds] of my_hospital
        let h_waiting [waiting] of my_hospital
        let h_satisfaction [satisfaction] of my_hospital
        let same_region my_region = h_region ; hospital and patient are from the same region
        let p_weight get_weight dist dev_dist ; comput the weight of the hospital (proportional to the patient-to-hospital distance)
        ; if the hospital is situated in the same region of the patient, the patient can access it even if it is saturated
        ; let weight (h_intervention * p_weight)
        let weight (1 - (0.5751594 + 0.0005169 * waiting - 0.0060547 * satisfaction - 0.0006994 * h_intervention * p_weight * h_Rj + 0.5006558 * h_return * p_weight * h_Rj - 0.0029291 * h_beds * p_weight * h_Rj))
        if weight < 0 [
          output-print (word "attention weight negative: " weight)
        ]
        ifelse (manage_capacity = true) [
          ; if ((h_capacity > 0) and (h_total_capacity > 0)) [
          if (h_capacity > 0) [
            ifelse same_region = TRUE [
              table:put weight_list_intra key weight
            ] [
              table:put weight_list_extra key weight
            ]
          ]
        ] [
          ifelse same_region = TRUE [
            table:put weight_list_intra key weight
          ] [
            table:put weight_list_extra key weight
          ]
        ]
      ]
    ]
  ]
  ; each hospital placed in the same region (and in the other regions) of the patient has a specific weight that depend on the total accessibility and the liability
  ; the summed probability of all the intra-regional hospitals is 1-liability while the summed probability of all the extra-regional hospitals is liability
  ; in this way when an hospital is randomisely extracted the probability that the hospital is intra or extra regional depends on the liability index, while the
  ; specific hospital depends on the distance and capacity
  let weight_list table:make
  ifelse table:length weight_list_intra > 0 [
    let sum_v sum table:values weight_list_intra
    ifelse sum_v > 0 [
      foreach (table:keys weight_list_intra) [
        [k] -> let v table:get weight_list_intra k
        let vn (v / sum_v) * (1 - my_liability) ; probability to stay
        if vn < 0 [
          set vn 0
          output-print (word "vn: " my_region " - " k " - " v " - " sum_v " - " vn " - " my_liability)
        ]
        table:put weight_list_intra k vn
        table:put weight_list k vn
      ]
      table:put weight_values 1 sum table:values weight_list_intra
    ] [
      table:put weight_values 1 -1
      output-print (word "intra: " my_region " - " sum_v " - " (1 - my_liability))
    ]
  ] [
    table:put weight_values 1 -2
  ]
  ifelse table:length weight_list_extra > 0 [
    let sum_v sum table:values weight_list_extra
    ifelse sum_v > 0 [
      foreach (table:keys weight_list_extra) [
        [k] -> let v table:get weight_list_extra k
        let vn (v / sum_v) * (my_liability) ; probability to go
        if vn < 0 [
          set vn 0
          output-print (word "vn: " my_region " - " k " - " v " - " sum_v " - " vn " - " my_liability)
        ]
        table:put weight_list_extra k vn
        table:put weight_list k vn
      ]
      table:put weight_values 2 sum table:values weight_list_extra
    ] [
      table:put weight_values 2 -1
      output-print (word "extra: " my_region " - " sum_v " - " (1 - my_liability))
    ]
  ] [
    table:put weight_values 2 -2
  ]
  ifelse table:length weight_list != 0 [
    let value rnd:weighted-one-of-list table:to-list weight_list [[t] -> last t]
    table:put weight_values 0 item 0 value
  ] [
    table:put weight_values 0 -1
  ]
  ; the variable reported by this function contains 3 information:
  ; place 0) the hospital code where the patient are cared
  ; place 1) the total amount of intra-regional probability
  ; place 2) the total amount of inter-regional probability
  report weight_values
end
; END GET_TARGET

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
  file-open "output/municipality_table_quality_measures.csv"
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
    let my_interventions_extra 0
    let my_beds_intra 0
    let my_beds_extra 0
    let my_int_beds_intra 0
    let my_int_beds_extra 0
    let my_return_intra 0
    let my_return_extra 0
    let my_n_return_intra 0
    let my_n_return_extra 0
    let my_closest_intra 0
    let my_closest_extra 0
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

          ;output-print (word key " - " h_beds " - " h_intervention)

          let h_int_per_bed 0
          if h_beds > 0 [
            set h_int_per_bed h_intervention / h_beds
          ]
          ; calculates the weight considering the distance between the hospital and the patient and multiplies it with the capacity of the hospital
          let p_weight get_weight dist standard-deviation table:values distances
          let weight p_weight * h_Rj
          let same_region my_region = h_region
          ; if the municipality and hospital are connected by driving roads
          if weight > 0 [
            ; intra- and inter- regional indicators are computed considering, for instamce, the number of interventions of the hospital and the weight between the hospital and the municipality
            ifelse same_region = TRUE [
              set my_int_beds_intra my_int_beds_intra + (weight * h_int_per_bed)
              set my_beds_intra my_beds_intra + (weight * h_beds)
              set my_interventions_intra my_interventions_intra + (weight * h_intervention)
              set my_return_intra my_return_intra + (weight * h_return * h_n_return)
              set my_n_return_intra my_n_return_intra + (h_n_return)
              if p_weight > my_closest_intra [
                set my_closest_intra p_weight
              ]
            ]
            [
              set my_int_beds_extra my_int_beds_extra + (weight * h_int_per_bed)
              set my_beds_extra my_beds_extra + (weight * h_beds)
              set my_interventions_extra my_interventions_extra + (weight * h_intervention)
              set my_return_extra my_return_extra + (weight * h_return * h_n_return)
              set my_n_return_extra my_n_return_extra + (h_n_return)
              if p_weight > my_closest_extra [
                set my_closest_extra p_weight
              ]
            ]
          ]
        ]
      ]
    ]
    let my_return_intra_ratio 0
    let my_return_extra_ratio 0
    if my_n_return_intra > 0 [
      set my_return_intra_ratio my_return_intra / my_n_return_intra
    ]
    if my_n_return_extra > 0 [
      set my_return_extra_ratio my_return_extra / my_n_return_extra
    ]
    ; the function reports the quality indicators
    ; each indicator is computed as the intra-regional component minus the extra-regional component
    ; if, for instance, the outcome_intervention is lower than 0, the patients has a higher access to extra-regional hospitals than intra-regional
    table:put municipality_info "outcome_intervention" my_interventions_intra - my_interventions_extra
    table:put municipality_info "outcome_return" my_return_intra_ratio - my_return_extra_ratio
    table:put municipality_info "outcome_closest" my_closest_intra - my_closest_extra
    table:put municipality_info "outcome_beds" my_beds_intra - my_beds_extra
    table:put municipality_info "outcome_int_beds" my_int_beds_intra - my_int_beds_extra

    ; added for the last model
    table:put municipality_info "outcome_intervention_intra" my_interventions_intra
    table:put municipality_info "outcome_return_intra" my_return_intra_ratio
    table:put municipality_info "outcome_beds_intra" my_beds_intra
    ;

    table:put municipality_table my_id_municipality municipality_info
    ; id|reg|prov|pop|north|inc|edu|int_intra|int_extra|ret_intra|ret_extra|clos_intra|clos_extra|bed_intra|bed_extra
    file-print (word my_id_municipality "|" my_region "|" my_province "|" my_population "|" my_interventions_intra "|" my_interventions_extra "|" my_return_intra_ratio "|" my_return_extra_ratio "|" my_beds_intra "|" my_beds_extra)
  ]
  file-close
end
; END COMPUTE QUALITY MEASURES

; colors each region depending on the mobility index
to polygon_color
  let num1 0
  let x_mean 0
  let y_mean 0
  let den1 0
  let den2 0
  foreach table:keys comulative_data [
    key -> let info table:get comulative_data key
    let intra table:get info "intra"
    let extra table:get info "extra"
    let mobility extra / (intra + extra)
    let passive table:get table:get province_table key "passive"
    set num1 (num1 + mobility * passive)
    set x_mean (x_mean + mobility)
    set y_mean (y_mean + passive)
    set den1 (den1 + mobility * mobility)
    set den2 (den2 + passive * passive)
    ; print (word key " = " (mobility * 100))
    let poly gis:find-features italy-dataset "dc_provinc" key
    gis:set-drawing-color black + 10
    if mobility < 0.50 [gis:set-drawing-color black + 11]
    if mobility < 0.45 [gis:set-drawing-color black + 12]
    if mobility < 0.40 [gis:set-drawing-color black + 13]
    if mobility < 0.35 [gis:set-drawing-color black + 14]
    if mobility < 0.30 [gis:set-drawing-color black + 15]
    if mobility < 0.25 [gis:set-drawing-color black + 16]
    if mobility < 0.20 [gis:set-drawing-color black + 17]
    if mobility < 0.15 [gis:set-drawing-color black + 18]
    if mobility < 0.10 [gis:set-drawing-color black + 19]
    if mobility < 0.05 [gis:set-drawing-color white]

;    gis:set-drawing-color black
;    if mobility < 0.5 [gis:set-drawing-color red]
;    if mobility < 0.4 [gis:set-drawing-color orange]
;    if mobility < 0.3 [gis:set-drawing-color yellow]
;    if mobility < 0.2 [gis:set-drawing-color green]
;    if mobility < 0.1 [gis:set-drawing-color sky]
    foreach poly [
      munic -> gis:fill munic 2.0
    ]
  ]

  ; compute the regression coefficient
  let n_provinces table:length comulative_data
  let num2 (n_provinces * (x_mean / n_provinces) * (y_mean / n_provinces))
  let num num1 - num2
  set den1 (den1 - n_provinces * ((x_mean / n_provinces) * (x_mean / n_provinces)))
  set den2 (den2 - n_provinces * ((y_mean / n_provinces) * (y_mean / n_provinces)))
  let den den1 * den2
  set r_passive (num / sqrt(den))
  output-print (word "r_passive - " (round (r_passive * 100)) " " num1 " " num2 " " den1 " " den2)
  output-print (word "capacity end: " sum [capacity] of houses)
end
; END POLYGON_COLOR

; compute the number of patients cared in their region or outside their region
; the information are stored in the cumulative_data table to be used for updating the map with colors
to update_counter[my_province my_region my_target_region]
  let intra 0
  let extra 0
  let info table:make
  ifelse table:has-key? comulative_data my_province [
    set info table:get comulative_data my_province
  ] [
    table:put info "intra" 0
    table:put info "extra" 0
  ]
  ifelse my_region = my_target_region [
    table:put info "intra" table:get info "intra" + 1
  ] [
    table:put info "extra" table:get info "extra" + 1
  ]
  table:put comulative_data my_province info
end
; END UPDATE_COUNTER

to test

  ;place_pats

  output-print count houses with [capacity > 0 and total_capacity > 0]

  ;set_environment
  ;loop [
  ;  ifelse (n_ticks > ticks) [ go ] [ stop ]
  ;]


  ;setup
  ;load_gis
  ;load_data
  ;load_population
  ;go

  ;output-print province_table

  ;output-print table:keys distance_table
  ;let distances table:get distance_table 77014
  ;output-print standard-deviation table:values distances
  ;compute_quality_measures

;  set update_capacity false
;  set random_queue false
;  set_environment
;  go
;
;
;  set update_capacity true
;  set random_queue false
;  set_environment
;  go


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
70
10
133
43
NIL
setup
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
63
287
126
320
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

BUTTON
59
51
136
84
NIL
load_gis
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
53
95
141
129
NIL
load_data
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
195
192
314
237
patients TO BE placed
count people with [cared = FALSE]
0
1
11

BUTTON
38
140
157
173
NIL
load_population
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
5
182
91
242
n_patients
1000.0
1
0
Number

INPUTBOX
104
182
189
242
n_ticks
52.0
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
193
98
318
131
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

TEXTBOX
191
10
341
94
Set_environment performs the:\n- setup\n- load_gis\n- load_data\n- load_population
11
0.0
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
4
352
152
385
update_capacity
update_capacity
1
1
-1000

SWITCH
166
350
305
383
random_queue
random_queue
1
1
-1000

TEXTBOX
14
447
310
577
- update_capacity: load the hospital updated information with provisional data on interventions, beds, etc.\n- random_queue: select the population to be involved in the simulation randomly \n- manage_capacity: control whether hospitals saturate their capacity 
11
0.0
1

SWITCH
1
395
153
428
manage_capacity
manage_capacity
0
1
-1000

PLOT
14
551
315
701
plot 1
NIL
NIL
0.0
52.0
0.0
1.0
true
false
"plotxy ticks r_passive" "plotxy ticks r_passive"
PENS
"default" 1.0 0 -16777216 true "plot r_passive" "plot r_passive"

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
