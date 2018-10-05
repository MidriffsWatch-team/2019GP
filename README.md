# 2019GP
University of California Santa Barbara Bren group project 2019 with Community and Biodiversity (COBI), a civil society organization, to quantify the cost of delayed management intervention in the Midriffs Islands, Gulf of California.


conapesca.rds file contains monthly landings reported to CONAPESCA by fishers/cooperatives, by state and landing site for years 2000-2015.Price in Mexican pesos.  
  Variables:
  1. NombreActivo- vessel name 
  2. SitioDesembarque- Landing site 
  3. UnidadEconomica- cooperative or fisher folk name
  4. Estado- state (Sonora, Baja California Sur)
  5. Officina- office 
  6. LugardeCaptura- fishing grounds 
  7. Mes- Month 
  8. Ano- Year 
  9. Nombre principal- Group name of species 
  10. NombreCommum- Common names of species 
  11. NombreCientifico- Scientific name 
  12. PesoDesembaracdo- weight of landings (kilograms)
  13. PesoVivo- 
  14. Precio- Price per kg, not sure of what year 
  15. Valor- Total value  landed (PesoDesembaracdo * price per kg)

species.csv contains a list of target species sent to us by Stuart 15/05/2018.
  Variables:
  1. NombreCientifico- scientific name
  2. NombreCommum- common names of species
  3. Group- fish or invertebrate 
  4. Tracy- Yes/No inicating if this species was included in Tracey's delayed model.
  5. CommonName- common name as listed in Tracy's paper
  6. ImportanciaComercial- Pesca artesanal (small scale fisheries) or Ornato (Ornamentals)
  7. EspecieObjetive- Si/No is the species is targeted
  8. Calidad-
  9. Sitio- Site listed as RGI, Baja or RGI/Baja
  10. PrecioMinimo- minimum market price 2014
  11. PrecioMaximo- maximum market price 2014
  12. Promedio- Average market price 2014 
  13. Tendencia- market value trend (increasing, decreasing or stable) 