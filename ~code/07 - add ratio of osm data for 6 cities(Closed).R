LV_spatial_census$ratio_retail <- LV_spatial_census$count_retail/length(LV_spatial_census$count_retail)[1]
LV_spatial_census$ratio_office <- LV_spatial_census$count_office/length(LV_spatial_census$count_office)[1]
LV_spatial_census$ratio_restaurant <- LV_spatial_census$count_restaurant/length(LV_spatial_census$count_office)[1]
LV_spatial_census$ratio_public_transport <- LV_spatial_census$count_pubtran/length(LV_spatial_census$count_pubtran)[1]
LV_spatial_census$ratio_leisure <- LV_spatial_census$count_leisure/length(LV_spatial_census$count_leisure)[1]
LV_spatial_census$ratio_tourism <- LV_spatial_census$count_tourism/length(LV_spatial_census$count_tourism)[1]
LV_spatial_census$ratio_college <- LV_spatial_census$count_college/length(LV_spatial_census$count_college)[1]
LV_spatial_census$ratio_cycleway <- LV_spatial_census$total_length/sum(LV_spatial_census$total_length)
LV_spatial_census$ratio_street <- LV_spatial_census$street_length/sum(LV_spatial_census$street_length)

LV_spatial_census_RDS <- file.path(data_directory, "~RData/Louisville/LV_spatial_census")
saveRDS(LV_spatial_census,
        file = LV_spatial_census_RDS)
LV_spatial_census <- readRDS(LV_spatial_census_RDS)

AU_spatial_census$ratio_retail <- AU_spatial_census$count_retail/length(AU_spatial_census$count_retail)[1]
AU_spatial_census$ratio_office <- AU_spatial_census$count_office/length(AU_spatial_census$count_office)[1]
AU_spatial_census$ratio_restaurant <- AU_spatial_census$count_restaurant/length(AU_spatial_census$count_office)[1]
AU_spatial_census$ratio_public_transport <- AU_spatial_census$count_pubtran/length(AU_spatial_census$count_pubtran)[1]
AU_spatial_census$ratio_leisure <- AU_spatial_census$count_leisure/length(AU_spatial_census$count_leisure)[1]
AU_spatial_census$ratio_tourism <- AU_spatial_census$count_tourism/length(AU_spatial_census$count_tourism)[1]
AU_spatial_census$ratio_college <- AU_spatial_census$count_college/length(AU_spatial_census$count_college)[1]
AU_spatial_census$ratio_cycleway <- AU_spatial_census$total_length/sum(AU_spatial_census$total_length)
AU_spatial_census$ratio_street <- AU_spatial_census$street_length/sum(AU_spatial_census$street_length)

AU_spatial_census_RDS <- file.path(data_directory, "~RData/Austin/AU_spatial_census")
saveRDS(AU_spatial_census,
        file = AU_spatial_census_RDS)
AU_spatial_census <- readRDS(AU_spatial_census_RDS)


MNP_spatial_census$ratio_retail <- MNP_spatial_census$count_retail/length(MNP_spatial_census$count_retail)[1]
MNP_spatial_census$ratio_office <- MNP_spatial_census$count_office/length(MNP_spatial_census$count_office)[1]
MNP_spatial_census$ratio_restaurant <- MNP_spatial_census$count_restaurant/length(MNP_spatial_census$count_office)[1]
MNP_spatial_census$ratio_public_transport <- MNP_spatial_census$count_pubtran/length(MNP_spatial_census$count_pubtran)[1]
MNP_spatial_census$ratio_leisure <- MNP_spatial_census$count_leisure/length(MNP_spatial_census$count_leisure)[1]
MNP_spatial_census$ratio_tourism <- MNP_spatial_census$count_tourism/length(MNP_spatial_census$count_tourism)[1]
MNP_spatial_census$ratio_college <- MNP_spatial_census$count_college/length(MNP_spatial_census$count_college)[1]
MNP_spatial_census$ratio_cycleway <- MNP_spatial_census$total_length/sum(MNP_spatial_census$total_length)
MNP_spatial_census$ratio_street <- MNP_spatial_census$street_length/sum(MNP_spatial_census$street_length)

MNP_spatial_census_RDS <- file.path(data_directory, "~RData/MNP/MNP_spatial_census")
saveRDS(MNP_spatial_census,
        file = MNP_spatial_census_RDS)
MNP_spatial_census <- readRDS(MNP_spatial_census_RDS)


CH_spatial_census$ratio_retail <- CH_spatial_census$count_retail/length(CH_spatial_census$count_retail)[1]
CH_spatial_census$ratio_office <- CH_spatial_census$count_office/length(CH_spatial_census$count_office)[1]
CH_spatial_census$ratio_restaurant <- CH_spatial_census$count_restaurant/length(CH_spatial_census$count_office)[1]
CH_spatial_census$ratio_public_transport <- CH_spatial_census$count_pubtran/length(CH_spatial_census$count_pubtran)[1]
CH_spatial_census$ratio_leisure <- CH_spatial_census$count_leisure/length(CH_spatial_census$count_leisure)[1]
CH_spatial_census$ratio_tourism <- CH_spatial_census$count_tourism/length(CH_spatial_census$count_tourism)[1]
CH_spatial_census$ratio_college <- CH_spatial_census$count_college/length(CH_spatial_census$count_college)[1]
CH_spatial_census$ratio_cycleway <- CH_spatial_census$total_length/sum(CH_spatial_census$total_length)
CH_spatial_census$ratio_street <- CH_spatial_census$street_length/sum(CH_spatial_census$street_length)

CH_spatial_census_RDS <- file.path(data_directory, "~RData/Chicago/CH_spatial_census")
saveRDS(CH_spatial_census,
        file = CH_spatial_census_RDS)
CH_spatial_census <- readRDS(CH_spatial_census_RDS)

DC_spatial_census$ratio_retail <- DC_spatial_census$count_retail/length(DC_spatial_census$count_retail)[1]
DC_spatial_census$ratio_office <- DC_spatial_census$count_office/length(DC_spatial_census$count_office)[1]
DC_spatial_census$ratio_restaurant <- DC_spatial_census$count_restaurant/length(DC_spatial_census$count_office)[1]
DC_spatial_census$ratio_public_transport <- DC_spatial_census$count_pubtran/length(DC_spatial_census$count_pubtran)[1]
DC_spatial_census$ratio_leisure <- DC_spatial_census$count_leisure/length(DC_spatial_census$count_leisure)[1]
DC_spatial_census$ratio_tourism <- DC_spatial_census$count_tourism/length(DC_spatial_census$count_tourism)[1]
DC_spatial_census$ratio_college <- DC_spatial_census$count_college/length(DC_spatial_census$count_college)[1]
DC_spatial_census$ratio_cycleway <- DC_spatial_census$total_length/sum(DC_spatial_census$total_length)
DC_spatial_census$ratio_street <- DC_spatial_census$street_length/sum(DC_spatial_census$street_length)

DC_spatial_census_RDS <- file.path(data_directory, "~RData/DC/DC_spatial_census")
saveRDS(DC_spatial_census,
        file = DC_spatial_census_RDS)
DC_spatial_census <- readRDS(DC_spatial_census_RDS)

KC_spatial_census$ratio_retail <- KC_spatial_census$count_retail/length(KC_spatial_census$count_retail)[1]
KC_spatial_census$ratio_office <- KC_spatial_census$count_office/length(KC_spatial_census$count_office)[1]
KC_spatial_census$ratio_restaurant <- KC_spatial_census$count_restaurant/length(KC_spatial_census$count_office)[1]
KC_spatial_census$ratio_public_transport <- KC_spatial_census$count_pubtran/length(KC_spatial_census$count_pubtran)[1]
KC_spatial_census$ratio_leisure <- KC_spatial_census$count_leisure/length(KC_spatial_census$count_leisure)[1]
KC_spatial_census$ratio_tourism <- KC_spatial_census$count_tourism/length(KC_spatial_census$count_tourism)[1]
KC_spatial_census$ratio_college <- KC_spatial_census$count_college/length(KC_spatial_census$count_college)[1]
KC_spatial_census$ratio_cycleway <- KC_spatial_census$total_length/sum(KC_spatial_census$total_length)
KC_spatial_census$ratio_street <- KC_spatial_census$street_length/sum(KC_spatial_census$street_length)
