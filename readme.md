# Extract EQA registration data

This is a small script that extracts your EQA registration data from
[RfB](https://www.rfb.bio) and [Instand](https://www.instand-ev.de). Use at your own risk. 

## Requirements

This script expects

* your RfB order confirmation (called overview_xxxx.pdf)
* the RfB survey program (please rename it to RfB-Programmheft.pdf)
* the Instand order confirmation (called something like Anmeldebestätigung_XXXX_tln_XXX.pdf)

in the folder ./data/the_year/ where you replace 'the_year' with the actual year (e.g. 2021) . I have only used German language files, other languages might fail.

Also, please complete the settings in the first part of the script.

## Output
This script generates a csv file for each sub-lab in ./generated/the_year . Columns are 

* eqa - Instand or RfB
* rv - the name of the ring trial
* round - the round of the ring trial
* von - from, date of sample shipping
* bis - to, date of result entry deadline


