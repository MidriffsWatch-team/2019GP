# 2019GP
University of California Santa Barbara Bren group project 2019 with Community and Biodiversity (COBI), a civil society organization, to quantify the cost of delayed management intervention in the Midriffs Islands, Gulf of California.


conapesca.rds file contains monthly landings reported to CONAPESCA by fishers/cooperatives, by state and landing site for years 2000-2015.Price in Mexican pesos.  
  Variables:
  1. NombreActivo- vessel name 
  2. SitioDesembarque- Landing site 
  3. UnidadEconomica- cooperative or fisher folk name
  4. Estado- State (our focus is on Sonora, Baja California)
  5. Officina- office 
  6. LugardeCaptura- fishing grounds 
  7. Mes- Month 
  8. Ano- Year 
  9. Nombre principal- Group name of species 
  10. NombreCommum- Common names of species 
  11. NombreCientifico- Scientific name 
  12. PesoDesembaracdo- Weight of landed product, may be whole, or gutted or filleted
  13. PesoVivo- Live weight of landings in kilograms 
  14. Precio- Price per kg, not sure of referecnce year  
  15. Valor- Total value  landed (PesoDesembaracdo * price per kg)

focalspecies.csv contains a list of target species
  Variables:
  1. NombreCientifico- scientific name
  2. CommonName- common name 
  3. Commercial- refers to commercial scale ie small scale or large scale for which all are small scale 
  4. Targetted- refers to whether the species is the primary targetted fisheries, yes/no variable 
  5. Quality- corresponds to targetted, yes= primary, no=secondary 
  6. Site- RGI (Region de las grandes islas) and Baja (Baja California)
  7. Min 2014- Minimum market price of species in pesos/kilogram in 2014
  8. Max2014- Maximum market price of species in pesos/kilogram in 2014
  9. Average 2014- Average market price of species in pesos/kilogram in 2014
  10. Min2018- Minimum market price of species in pesos/kilogram in 2018
  11.Max2018- Maximum market price of species in pesos/kilogram in 2018
  12. Average2018- Average market price of species in pesos/kilogram in 2018
  13.Additional biological information- refers to whether the species was included in Tracey's (2018) cost of delay paper 
  14. Quotas- refers to whether there is a quota system in place to manage species 
  15. Satus_of_fishery- Based on CONAPESCA assessmet 
  16. Abundance- Yes indicates that abundance data is available for the species from COBI for 2010/2011
  17. Biomass-Yes indicates that biomass data is available for the species from COBI for 2010/2011
  18. SPAGs- Yes indicates that spawing aggregation data is available for the species from COBI that fall within the proposed MPA design