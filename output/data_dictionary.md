`participant_id` is a string that contains the anonymized IDs of Prolific participants
`trial_number` is an ordinal between 0 and 59 that represent each of the 60 trials that participants saw. 
`drawing_filename` is a string that links to the drawing being shown in a trial. 
`condition` is categorical and is either "category" or "delayed_recall".
`category` is categorical and specifies which of the 17 categories this trial belongs to. 
`memorability` is categorical, either `high` or `low` if `condition` is `delayed_recall`, and `NA` if `condition` is `category`.
`target_image` is a string that links to the correct image that matches the drawing. It is `NA` if `condition` is `category`.
`selected_image` is a string that links to the image that participant selected.
`correct` is a binary measure if `condition` is `delayed_recall` and `NA` if `condition` is `category`.
`selected_type` is categorical and represents which type of `high`, `low`, or `foil` memorability image the participant selected. It is `NA` if `condition` is `delayed_recall`. 
`rt` is in milliseconds.
