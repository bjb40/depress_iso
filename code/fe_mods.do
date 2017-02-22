*dev Stata 14
*Analyze fixed effects

*import data
import delimited using "H:/projects/depress_iso/output/cleandat~/cleandat.csv", clear

xtset fid

xtreg depress dvindegc dvoutdegc egodenuc alterdistress c.alterdistress#c.egodenuc ///
       psamesexuc freelunch cmsr grade, fe
