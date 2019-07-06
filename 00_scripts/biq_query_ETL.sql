-- pass data from staging to final model
INSERT INTO journalists.f_employee_salary_out 
 SELECT  ROW_NUMBER() OVER (), p.people_id, po.cedula, e.entity_id, e.entity_name, url, 
  p.first_name, p.last_name, po.cargo, po.salario, po.gasto, po.estado, 
  po.fecha_inicio, po.record_date, po.sex, po.finish_date
  FROM `rowsums.journalists.d_people` p
  INNER JOIN data_test.staging_people_out po ON 
  p. person_id = po.cedula
  INNER JOIN `rowsums.journalists.d_entity` e ON 
  e. entity_name = po. entidad
  
  
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


SELECT  f.person_id, e.entity_name, 
  f.first_name, f.last_name, f.job as job_title, 
  CAST(f.salary AS FLOAT64) salary, CAST(f.expenses AS FLOAT64) expenses, f.status,
  f.start_date, finish_date, f.record_date, 'out' as record_type, is_last
FROM journalists.f_employee_salary_out f 
INNER JOIN journalists.d_date_upload u ON f.record_id = u.record_id
INNER JOIN journalists.d_entity e ON f.entity_id = e.entity_id 



-- 1754
SELECT  f.person_id, e.entity_name, 
  f.first_name, f.last_name, f.job_title, 
  CAST(f.salary AS FLOAT64) salary, CAST(f.expenses AS FLOAT64) expenses, f.status, 
  f.start_date, CAST('2099-12-31' AS DATE) finish_date, f.record_date, 'in' as record_type, is_last
FROM rowsums.journalists.f_employee_salary f 
INNER JOIN rowsums.journalists.d_date_upload u ON f.record_id = u.record_id
INNER JOIN rowsums.journalists.d_entity e ON f.entity_id = e.entity_id
WHERE start_date >= '2019-04-01'



-- Insert expenses
INSERT INTO journalists.f_travel_expenses
SELECT p.people_id, p.person_id, UPPER(nombre), UPPER(cargo), pais, ciudad, descripcion, 
fecha_salida, fecha_regreso, tipo, dias, viatico_diario, key, 
value, d.entity_id, d.entity_name
FROM `rowsums.data_test.staging_travel_expenses` e 
INNER JOIN journalists.d_entity d ON e.entidad = d.entity_name
LEFT JOIN journalists.d_people p ON p.person_id = e.person_id 
