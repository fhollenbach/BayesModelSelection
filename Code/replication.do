clear
import delimited "/Users/florianhollenbach/Documents/GitHub/BayesModelSelection/Data/Haber_Menaldo_2011_APSR_Dataset.csv"
rename ïhmccode hmccode

xtset hmccode year

gen log_gdp_per_cap_haber_men_2 = log(gdp_per_cap_haber_men_2)
by hmccode: ipolate log_gdp_per_cap_haber_men_2 year, gen(LogPerCapGDP_interp)
by hmccode: ipolate fiscal_reliance year, gen(Fiscal_Reliance_interpolate)
by hmccode: ipolate polity_s year, gen(polity_s_interpolate) 
by hmccode: ipolate total_oil_income_pc year, gen(Total_Oil_Income_PC_interp) 


keep if Fiscal_Reliance_interpolate != . & cnamehabmen != "Saudi Arabia"
keep if polity_s_interpolate != .



//Then I create variables with lags and differences, because xtscc command cannot support time-series commands:
generate D_polity_s_interp = D.polity_s_interpolate
generate L_Polity_s_interp = L.polity_s_interpolate
generate TOI_INC = Total_Oil_Income_PC_interp
generate L_tot_oil_inc_interp = L.TOI_INC
generate D_tot_oil_inc_interp = D.TOI_INC
generate L_LogPerCapGDP_interp = L.LogPerCapGDP_interp
generate D_LogperCapGDP_int = D.LogPerCapGDP_interp
generate L_logGDPPERCAP = L.log_gdp_per_cap_haber_men_2
generate L_CivilWar = L.civil_war
generate D_Region_Dem_Diffuse = D.region_dem_diffuse
generate D_World_Dem_Diffuse = D.world_dem_diffuse
generate D_Fiscal_Rel_Interp = D.Fiscal_Reliance_interpolate
generate L_Fiscal_Rel_interp = L.Fiscal_Reliance_interpolate
generate L_D_Fiscal_Rel_Interp = L.D_Fiscal_Rel_Interp
generate FD_Fiscal_Rel_interp = F.D_Fiscal_Rel_Interp
generate L_REGION_DEM_DIFFUSE = L.region_dem_diffuse
generate L_WORLD_DEM_DIFFUSE = L.world_dem_diffuse
generate D_GDPPERCAP = D.gdp_per_cap_haber_men_2
generate D_RegionalDiffusion = D.region_dem_diffuse
generate D_WORLD_DEM_DIFFUSE = D.world_dem_diffuse
replace D_GDPPERCAP = D.log_gdp_per_cap_haber_men_2

tabulate hmccode, generate(Ihmccode_)
tabulate year, generate(year_)


xtscc D_polity_s_interp L_Polity_s_interp L_Fiscal_Rel_interp D_Fiscal_Rel_Interp L_D_Fiscal_Rel_Interp L_logGDPPERCAP L_CivilWar L_REGION_DEM_DIFFUSE L_WORLD_DEM_DIFFUSE  D_GDPPERCAP D_RegionalDiffusion D_WORLD_DEM_DIFFUSE year_2-year_207 Ihmccode_2-Ihmccode_18
estimates store model_1
xtscc D_polity_s_interp L_Polity_s_interp L_Fiscal_Rel_interp D_Fiscal_Rel_Interp L_D_Fiscal_Rel_Interp L_logGDPPERCAP L_CivilWar L_REGION_DEM_DIFFUSE L_WORLD_DEM_DIFFUSE  D_GDPPERCAP D_RegionalDiffusion D_WORLD_DEM_DIFFUSE Ihmccode_2-Ihmccode_18
estimates store model_2

save "~/Documents/GitHub/BayesModelSelection/Data/HaberMenaldoRepl.dta", replace

/// full model with longer time series
clear
import delimited "/Users/florianhollenbach/Documents/GitHub/BayesModelSelection/Data/Haber_Menaldo_2011_APSR_Dataset.csv"
rename ïhmccode hmccode

xtset hmccode year

gen log_gdp_per_cap_haber_men_2 = log(gdp_per_cap_haber_men_2)
by hmccode: ipolate log_gdp_per_cap_haber_men_2 year, gen(LogPerCapGDP_interp)
by hmccode: ipolate fiscal_reliance year, gen(Fiscal_Reliance_interpolate)
by hmccode: ipolate polity_s year, gen(polity_s_interpolate) 
by hmccode: ipolate total_oil_income_pc year, gen(Total_Oil_Income_PC_interp) 
by hmccode: ipolate very_unequal_utip year, gen(very_unequal_utip_interp) 
by hmccode: ipolate civil_war year, gen(CivilWar_Interp)

replace Total_Oil_Income_PC_interp = Total_Oil_Income_PC_interp/1000




//Then I create variables with lags and differences, because xtscc command cannot support time-series commands:
generate D_polity_s_interp = D.polity_s_interpolate
generate L_Polity_s_interp = L.polity_s_interpolate
generate TOI_INC = Total_Oil_Income_PC_interp
generate L_tot_oil_inc_interp = L.TOI_INC
generate D_tot_oil_inc_interp = D.TOI_INC
generate L_LogPerCapGDP_interp = L.LogPerCapGDP_interp
generate D_LogperCapGDP_int = D.LogPerCapGDP_interp
generate L_logGDPPERCAP = L.log_gdp_per_cap_haber_men_2
generate L_CivilWar = L.civil_war
generate L_CivilWar_interp = L.CivilWar_Interp
generate D_Region_Dem_Diffuse = D.region_dem_diffuse
generate D_World_Dem_Diffuse = D.world_dem_diffuse
generate D_Fiscal_Rel_Interp = D.Fiscal_Reliance_interpolate
generate L_Fiscal_Rel_interp = L.Fiscal_Reliance_interpolate
generate L_D_Fiscal_Rel_Interp = L.D_Fiscal_Rel_Interp
generate FD_Fiscal_Rel_interp = F.D_Fiscal_Rel_Interp
generate L_REGION_DEM_DIFFUSE = L.region_dem_diffuse
generate L_WORLD_DEM_DIFFUSE = L.world_dem_diffuse
generate D_GDPPERCAP = D.gdp_per_cap_haber_men_2
generate D_RegionalDiffusion = D.region_dem_diffuse
generate D_WORLD_DEM_DIFFUSE = D.world_dem_diffuse
replace D_GDPPERCAP = D.log_gdp_per_cap_haber_men_2


tabulate hmccode, generate(Ihmccode_)
tabulate year, generate(year_)

xtscc D_polity_s_interp L_Polity_s_interp L_tot_oil_inc_interp D_tot_oil_inc_interp L_LogPerCapGDP_interp L_CivilWar_interp L_REGION_DEM_DIFFUSE L_WORLD_DEM_DIFFUSE D_LogperCapGDP_int D_Region_Dem_Diffuse D_World_Dem_Diffuse year_2-year_207 Ihmccode_3-Ihmccode_168, lag(1)
save "~/Documents/GitHub/BayesModelSelection/Data/HaberMenaldoRepl_full.dta", replace
