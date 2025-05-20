# README

To replicate the results, run the scripts in the following order:

- `prepare_data.R` - prepare data
- `charts.R` - make charts
- `models.R` - estimate models
- `wcn.R` - calculate average WCN (weighted candidate novelty) for genuinely and partially new parties

The dataset containing the list of new parties is stored in `data/new_parties_list.xlsx`. It contains the following variables:  
- `country_name_short` - country  
- `election_year` - year of elections  
- `party_name_short` - party abbreviation  
- `party_name` - party name  
- `parlgov_id` - ParlGov ID  
- `partyfacts_id` - PartyFacts ID  
- `vote_share` - share of votes received  
- `gnp_1` - genuinely new party, 1% threshold (1=Yes)  
- `pnp_1` - partially new party, 1% threshold (1=Yes)  
- `gnp_leg` - genuinely new party, legislative threshold (1=Yes)  
- `pnp_leg` - partially new party, legislative threshold (1=Yes)  

The dataset containing the electoral support and number of new parties is stored in `data/new_parties_aggregated.xlsx`, it contains the following variables:  
- `country_name_short` - country  
- `election_year` - year of elections  
- `np_share_cv_1` - electoral share of genuinely new parties, 1% threshold  
- `np_share_pnp_1` - electoral share of partially new parties, 1% threshold  
- `np_share_cv_leg` - electoral share of genuinely new parties, legislative threshold  
- `np_share_pnp_leg` - electoral share of partially new parties, legislative threshold  
- `np_share_nc_leg` - electoral share of all new parties, legislative threshold  
- `np_share_nc_1` - electoral share of all new parites, 1% threshold  
- `np_number_cv_1` - number of genuinelly new parties, 1% threshold  
- `np_number_pnp_1` - number of partially new parties, 1% threshold  
- `np_number_cv_leg` - number of genuinelly new parties, legislative threshold  
- `np_number_pnp_leg` - number of partially new parties, legislative threhold  
- `np_number_nc_leg` - number of all new parties, legislative threshold  
- `np_number_nc_1` - number of all new parties, 1% threshold  