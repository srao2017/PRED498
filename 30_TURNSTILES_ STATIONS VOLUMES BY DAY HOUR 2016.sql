copy (
SELECT * FROM 
(SELECT "Station","Date", EXTRACT(MONTH FROM CAST ("Date" AS DATE)) as "Month", EXTRACT(WEEK FROM CAST ("Date" AS DATE)) as "Week", EXTRACT(DOW FROM CAST ("Date" AS DATE)) as "Day", EXTRACT(HOUR FROM CAST ("Time" AS TIME)) as "Hour","Time", SUM("act_entries") as "Ent", SUM("act_exits") as "Ext" 
FROM 
(SELECT DISTINCT 
  turnstile16."C/A", 
  turnstile16."Unit", 
  turnstile16."SCP", 
  turnstile16."Station",
  turnstile16."Exits", 
  turnstile16."Entries", 
  turnstile16."Time", 
  turnstile16."Date",
  turnstile16."Entries" - lag(turnstile16."Entries") over (order by turnstile16."C/A" ASC,
  turnstile16."Unit" ASC, turnstile16."SCP" ASC, turnstile16."Station" ASC, turnstile16."Date" ASC, turnstile16."Time" ASC) as act_entries,
  turnstile16."Exits" - lag(turnstile16."Exits") over (order by turnstile16."C/A" ASC, 
  turnstile16."Unit" ASC, turnstile16."SCP" ASC, turnstile16."Station" ASC, turnstile16."Date" ASC, turnstile16."Time" ASC) as act_exits  
FROM 
  public.turnstile16
WHERE
    turnstile16."Station" in (
'34 ST-PENN STA',
'FULTON ST',
'GRD CNTRL-42 ST',
'23 ST',
'CANAL ST',
'34 ST-HERALD SQ',
'CHAMBERS ST',
'59 ST',
'86 ST',
'42 ST-PORT AUTH',
'WALL ST',
'TIMES SQ-42 ST',
'ATL AV-BARCLAY',
'14 ST',
'125 ST',
'59 ST COLUMBUS',
'14 ST-UNION SQ',
'28 ST',
'CHURCH AV',
'96 ST',
'161/YANKEE STAD',
'47-50 STS ROCK',
'METS-WILLETS PT',
'50 ST',
'JAY ST-METROTEC',
'LEXINGTON AV/53',
'CORTLANDT ST',
'BOWLING GREEN',
'7 AV',
'KINGS HWY'
  )

ORDER BY
  turnstile16."C/A" ASC,
  turnstile16."Unit" ASC,
  turnstile16."SCP" ASC,
  turnstile16."Station" ASC, 
  turnstile16."Date" ASC,
  turnstile16."Time" ASC
) as foo 
GROUP BY foo."Station", foo."Date", foo."Time"
ORDER BY foo."Station", foo."Date", foo."Time"
) as bar 
WHERE "Ent" < 100000 AND "Ent" > 0 AND "Ext" < 100000 AND "Ext" > 0
) 
to  'C:/EDUCATION/NWU/498 - Capstone/TEAM SMARTCITY/DATA_ANALYTICS/DATA/TRAINING/DailyExitsEntries-30.csv' DELIMITER ',' CSV HEADER
;
