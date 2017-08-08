copy (
SELECT * FROM 
(SELECT "Station","Date", EXTRACT(MONTH FROM CAST ("Date" AS DATE)) as "Month", EXTRACT(WEEK FROM CAST ("Date" AS DATE)) as "Week", EXTRACT(DOW FROM CAST ("Date" AS DATE)) as "Day", EXTRACT(HOUR FROM CAST ("Time" AS TIME)) as "Hour","Time", SUM("act_entries") as "Ent", SUM("act_exits") as "Ext" 
FROM 
(SELECT DISTINCT 
  turnstile_170722."C/A", 
  turnstile_170722."Unit", 
  turnstile_170722."SCP", 
  turnstile_170722."Station",
  turnstile_170722."Exits", 
  turnstile_170722."Entries", 
  turnstile_170722."Time", 
  turnstile_170722."Date",
  turnstile_170722."Entries" - lag(turnstile_170722."Entries") over (order by turnstile_170722."C/A" ASC,
  turnstile_170722."Unit" ASC, turnstile_170722."SCP" ASC, turnstile_170722."Station" ASC, turnstile_170722."Date" ASC, turnstile_170722."Time" ASC) as act_entries,
  turnstile_170722."Exits" - lag(turnstile_170722."Exits") over (order by turnstile_170722."C/A" ASC, 
  turnstile_170722."Unit" ASC, turnstile_170722."SCP" ASC, turnstile_170722."Station" ASC, turnstile_170722."Date" ASC, turnstile_170722."Time" ASC) as act_exits  
FROM 
  public.turnstile_170722
WHERE
    turnstile_170722."Station" in (
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
  turnstile_170722."C/A" ASC,
  turnstile_170722."Unit" ASC,
  turnstile_170722."SCP" ASC,
  turnstile_170722."Station" ASC, 
  turnstile_170722."Date" ASC,
  turnstile_170722."Time" ASC
) as foo 
GROUP BY foo."Station", foo."Date", foo."Time"
ORDER BY foo."Station", foo."Date", foo."Time"
) as bar 
WHERE "Ent" < 50000 AND "Ent" > 0 AND "Ext" < 50000 AND "Ext" > 0
) 
to  'C:/EDUCATION/NWU/498 - Capstone/TEAM SMARTCITY/DATA_ANALYTICS/DATA/VALIDATION/processed_30_20170722.csv' DELIMITER ',' CSV HEADER
;
-- The nested SQL is needed to use the alias within in a query's group by or order by - act_entries or act_exits in the above query

-- MTA published 2016 top 10 stations by volume 
-- http://web.mta.info/nyct/facts/ffsubway.htm 

-- 1. 'TIMES SQ-42 ST', 	Times Sq-42 St  	N Line icon Q Line icon R Line icon W Line icon S  Line icon 1  Line icon 2  Line icon 3  Line icon 7  Line icon / 42 St A  Line icon C  Line icon E  Line icon	Manhattan	64,531,511
-- 2. 'GRD CNTRL-42 ST', 	Grand Central-42 St  	S Line icon 4 Line icon 5 Line icon 6 Line icon 7  Line icon	Manhattan						46,121,509
-- 3. '34 ST-HERALD SQ' 	34 St-Herald Sq 	B Line icon D Line icon F Line icon M Line icon N Line icon Q Line icon R Line icon W Line icon	Manhattan	39,000,352
-- 4. '14 ST-UNION SQ' 		14 St-Union Sq 		L Line icon N Line icon Q Line icon R Line icon W Line icon 4 Line icon 5 Line icon 6 Line icon	Manhattan		34,289,822
-- 5. '34 ST-PENN STA' 		34 St-Penn Station 	1 Line icon 2  Line icon 3  Line icon	Manhattan									27,741,367
-- 6. 				34 St-Penn Station   	A Line icon C  Line icon E  Line icon	Manhattan									25,183,869
-- 7. 'FULTON ST'		Fulton St 		A Line icon C Line icon J  Line icon Z Line icon 2  Line icon 3  Line icon 4 Line icon 5 Line icon	Manhattan	25,162,937
-- 8. '59 ST-COLUMBUS'		59 St-Columbus Circle 	A Line icon B Line icon C  Line icon D Line icon 1  Line icon	Manhattan						23,203,443
-- 9. 'LEXINGTON AVE'		Lexington Av 		W Line icon  / 59 St 4 Line icon 5 Line icon 6 Line icon	Manhattan						21,000,635
-- 10. '86 ST'			86 St 			4 Line icon 5 Line icon 6 Line icon	Manhattan									20,337,593

-- To get the day of week associated with the date we use EXTRACT(DOW FROM CAST ("Date" AS DATE))
-- here Sunday is 0 and Sat is 6