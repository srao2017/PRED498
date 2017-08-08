copy(
SELECT "Station", COUNT("Station") FROM (SELECT DISTINCT 
  turnstile2017."C/A", 
  turnstile2017."Unit", 
  turnstile2017."SCP", 
  turnstile2017."Station"
FROM 
  public.turnstile2017
GROUP BY
  turnstile2017."C/A",
  turnstile2017."Unit",
  turnstile2017."SCP",
  turnstile2017."Station" 
ORDER BY
  turnstile2017."C/A" ASC,
  turnstile2017."Unit" ASC,
  turnstile2017."SCP" ASC,
  turnstile2017."Station" ASC) as foo GROUP BY "Station" ORDER BY count DESC
  )
 to 'C:/EDUCATION/NWU/498 - Capstone/TEAM SMARTCITY/DATA_ANALYTICS/DATA/TRAINING/TurnstilesPerStation17.csv' DELIMITER ',' CSV HEADER
  ;
 