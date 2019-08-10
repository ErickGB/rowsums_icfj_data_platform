-- Summary steps
-- load data to staging tables:
  -- staging_historical_employee: 
  -- staging_travel_expenses: 
  -- staging_central_gov_salaries *:
  -- staging_people: All active employees this month. It is used to validate if there are new employees or some who have stopped.
  
-- Load data to final tables: 
-- Step 0: new historical data 
--      0.1: delete final table: historical_gov_employees
--      0.2: uplodad from staging table - data_test.staging_historical_employee

-- Step 1: central gov salaries (1 month)
--      0.1: delete final table: central_gov_salaries 
--      0.2: uplodad from staging table - data_test.staging_central_gov_salaries

-- Step 2: insert new employees. 




-- Step 2: If previews employee not in new table, and if that in previews. Go to out employee.
-- insert into :f_employee_salary_out from rowsums.journalists.f_employee_salary if not in this

-- Step 3: add control date. 
-- Step 4: Add new people
-- Step 5: Add new records to f_salary_employee


-- *******************************************************************************************
-- Step 0: new historical
delete from journalists.historical_gov_employees where 1 = 1

INSERT INTO journalists.historical_gov_employees 
SELECT * FROM data_test.staging_historical_employee

-----------------------------------------
-- Step 1: central gov salaries (1 month)
DELETE FROM journalists.central_gov_salaries WHERE 1 = 1

INSERT INTO journalists.central_gov_salaries 
SELECT * FROM data_test.staging_central_gov_salaries


-- Step 2: If previews employee not in new table, and if that in previews. Go to out employee.
INSERT INTO journalists.f_employee_salary_out 
 SELECT ROW_NUMBER() OVER (),  p.people_id, p.person_id, e.entity_id, e.entity_name, url_source, 
  p.first_name, p.last_name, p.job_title, p.salary, p.expenses, p.status, 
  p.start_date, p.record_date, '' sex , d.record_date as finsh_date, d.record_id
  FROM `rowsums.journalists.f_employee_salary` p
  INNER JOIN `rowsums.journalists.d_entity` e ON e.entity_name = p.entity_name
  INNER JOIN `rowsums.journalists.d_date_upload` d ON d.record_id = p.record_id 
  WHERE d.is_last = 1 AND -- mes anterior
  CONCAT(trim(p.person_id),  trim(p.job_title)) NOT IN (SELECT CONCAT(trim(person_id),  trim(position)) FROM data_test.staging_central_gov_salaries)
  
  
-- Step 3: add control date. 
update journalists.d_date_upload set is_last = 0 where 1 = 1
INSERT INTO rowsums.journalists.d_date_upload
SELECT '2019-07-15', '2019-07-20', 1, 156634, 0, 'Carga de datos para el mes número 7', 4

-- Step 4: Add new people


-- Step 5: Add new records to f_salary_employee
INSERT INTO journalists.f_employee_salary
 SELECT  ROW_NUMBER() OVER (), p.people_id, po.cedula, e.entity_id, e.entity_name, url, 
  p.first_name, p.last_name, po.cargo, po.salario, po.gasto, po.estado, 
  po.fecha_inicio, po.record_date, po.sex, po.finish_date
  FROM `rowsums.staging_employee_actual_month' p
  INNER JOIN `rowsums.journalists.d_entity` e ON e. entity_name = p.entidad

-------------------
-- Insert expenses
INSERT INTO journalists.f_travel_expenses
SELECT p.people_id, p.person_id, UPPER(nombre), UPPER(cargo), pais, ciudad, descripcion, 
fecha_salida, fecha_regreso, tipo, dias, viatico_diario, key, 
value, d.entity_id, d.entity_name
FROM `rowsums.data_test.staging_travel_expenses` e 
INNER JOIN journalists.d_entity d ON e.entidad = d.entity_name
LEFT JOIN journalists.d_people p ON p.person_id = e.person_id 





  
  
-- updating employees with your code number in d_people table
  update journalists.f_employee_salary f
   set f.people_id = d.people_id
  from journalists.d_people d
  where f.person_id = d.person_id
  
-- mark the active month in the platform
update journalists.d_date_upload
set is_last = 1
where record_date = '2019-06-08'

update journalists.d_date_upload
set out_employee = 2910
where record_date = '2019-05-18'


-- In-out employee
SELECT person_id, entity_name, 
  first_name, last_name, job_title, salary, expenses, status, 
  start_date, finish_date, record_date, record_type
FROM(
(SELECT  f.person_id, e.entity_name, 
  f.first_name, f.last_name, f.job as job_title, f.salary, f.expenses, f.status, 
  f.start_date, finish_date, f.record_date, 'out' as record_type--, is_last
FROM journalists.f_employee_salary_out f 
--INNER JOIN journalists.d_date_upload u ON f.record_id = u.record_id
INNER JOIN journalists.d_entity e ON f.entity_id = e.entity_id 
)UNION ALL
(SELECT  f.person_id, e.entity_name, 
  f.first_name, f.last_name, f.job_title, f.salary, f.expenses, f.status, 
  f.start_date, CAST('2099-12-31' AS DATE) finish_date, f.record_date, 'in' as record_type--, is_last
FROM journalists.f_employee_salary f 
--INNER JOIN journalists.d_date_upload u ON f.record_id = u.record_id
INNER JOIN journalists.d_entity e ON f.entity_id = e.entity_id
WHERE 
EXTRACT(MONTH FROM start_date) = EXTRACT(MONTH FROM f.record_date) 
))


-- vista out 
SELECT  f.person_id, e.entity_name, 
  f.first_name, f.last_name, f.job as job_title, 
  CAST(f.salary AS FLOAT64) salary, CAST(f.expenses AS FLOAT64) expenses, f.status,
  f.start_date, finish_date, f.record_date, 'out' as record_type, is_last
FROM journalists.f_employee_salary_out f 
INNER JOIN journalists.d_date_upload u ON f.record_id = u.record_id
INNER JOIN journalists.d_entity e ON f.entity_id = e.entity_id 

-- vista in
SELECT  f.person_id, e.entity_name, 
  f.first_name, f.last_name, f.job_title, 
  CAST(f.salary AS FLOAT64) salary, CAST(f.expenses AS FLOAT64) expenses, f.status, 
  f.start_date, CAST('2099-12-31' AS DATE) finish_date, f.record_date, 'in' as record_type, is_last
FROM rowsums.journalists.f_employee_salary f 
INNER JOIN rowsums.journalists.d_date_upload u ON f.record_id = u.record_id
INNER JOIN rowsums.journalists.d_entity e ON f.entity_id = e.entity_id
WHERE start_date >= '2019-04-01'





