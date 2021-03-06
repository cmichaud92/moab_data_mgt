/*
v_analysis_antenna

Combines the antenna fact table with the STReaMS pittag table to 
associate SpeciesCodes to antenna detections

Requires annual updates to the STReaMS pittag table to maintain
current pittag deployment.
*/


WITH t as (

    SELECT 
      a.StudyCode, 
      a.RiverCode, 
      a.SiteName, 
      a.AntennaLocation_X, 
      a.AntennaLocation_Y, 
      a.AntennaLocationEPSGCode, 
      d.DetectionTimeStamp_UTC, 
      REGEXP_REPLACE(d.PitTagHex, '[.]', '') AS PitTagString,            --MFO stores pittag with '.' STReaMS does not...
      EXTRACT (YEAR FROM a.DeploymentTimeStamp_UTC) AS Year              --Year for easy query
    FROM `ut-dnr-moabfieldoffice-dev.moab_test.f_antenna` a
    CROSS JOIN UNNEST(Detection) d
  
)

SELECT t.StudyCode, t.Year, t.RiverCode, t.SiteName, 
  t.DetectionTimeStamp_UTC, t.PitTagString, p.SpeciesCode, 
  t.AntennaLocation_X, t.AntennaLocation_Y, t.AntennaLocationEPSGCode

FROM t

LEFT JOIN moab_test.d_pittag p
  ON t.PitTagString = p.PitTagString


