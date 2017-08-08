copy(
SELECT "Station", COUNT("Station") FROM (SELECT DISTINCT 
  turnstile16."C/A", 
  turnstile16."Unit", 
  turnstile16."SCP", 
  turnstile16."Station"
FROM 
  public.turnstile16
GROUP BY
  turnstile16."C/A",
  turnstile16."Unit",
  turnstile16."SCP",
  turnstile16."Station" 
ORDER BY
  turnstile16."C/A" ASC,
  turnstile16."Unit" ASC,
  turnstile16."SCP" ASC,
  turnstile16."Station" ASC) as foo GROUP BY "Station" ORDER BY count DESC
  )
 to 'C:/EDUCATION/NWU/498 - Capstone/TEAM SMARTCITY/DATA_ANALYTICS/DATA/TRAINING/TurnstilesPerStation16.csv' DELIMITER ',' CSV HEADER
  ;
 