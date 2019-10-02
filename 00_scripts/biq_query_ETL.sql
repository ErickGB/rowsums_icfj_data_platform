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

-- 1. están todos los jobs? .. agregar  .. manual 
-- 2. Hay personas nuevas? ... agregar .. ok 
-- 3. crear tabla lote .. 

-- 4. Hay personas desvinculadas? ... pasar a tabla histórica
-- 5. Pasar a nueva tabla f_employee_salary

-- Step 2: insert new employees. 
code: STRING, complete_name: STRING, last_name:STRING, person_id: STRING, position: STRING, 
salary: FLOAT, expenses: FLOAT, total_income: FLOAT, status: STRING, 
start_date: DATE, first_name: STRING, entity: STRING, update_date: DATE, sex: STRING, url: STRING, record_date: DATE, key: STRING

INSERT INTO journalists.d_entity
SELECT entity_id, entity_code, entity_name, s from data_test.staging_entity 

-- people_id:INTEGER,person_id:STRING,fist_name:STRING,last_name:STRING,start_date:DATE,sex:STRING,record_date:DATE
INSERT INTO journalists.d_people 
SELECT SAFE_CAST(people_id AS INT64), person_id, fist_name, last_name,  start_date , sex, CURRENT_DATE() as record_date 
from data_test.staging_people 

-- jobs_id:INTEGER,job_position:STRING,job_title:STRING,record_date:DATE
INSERT INTO journalists.d_jobs 
SELECT id, job_position, job_title, CURRENT_DATE() as record_date 
from data_test.staging_jobs 

-- record_id:INTEGER,record_date:DATE,processed_date:DATE,is_actual:INTEGER
INSERT INTO journalists.d_date_upload
SELECT record_id, record_date, date_processed , 0 FROM data_test.staging_recods

-- employee_salary_id:INTEGER,people_id:INTEGER,person_id:STRING,entity_id:INTEGER,entity_name:STRING,url_source:STRING,first_name:STRING,last_name:STRING,job_id:INTEGER,job_title:STRING,job_position:STRING,salary:FLOAT,expenses:FLOAT,total:FLOAT,status:STRING,start_date:DATE,record_id:INTEGER,date_processed:DATE,record_date:DATE,sex:STRING,key:STRING,key1:STRING
INSERT INTO journalists.f_employee_salary
SELECT * FROM data_test.staging_employee_salaries

-- 17401
DELETE FROM journalists.f_employee_salary_out WHERE 1 = 1
insert into journalists.f_employee_salary_out
SELECT ROW_NUMBER() OVER() row_number, p.people_id, cedula, e.entity_id, e.entity_name, url, 
nombre, apellido, j.jobs_id, j.job_title, j.job_position, 
salario, gasto, total, estado, fecha_inicio, u.record_id, o.last_update, o.record_date,
o.sex, key, key1
FROM data_test.staging_employee_out o
LEFT JOIN journalists.d_people p ON p.person_id = o.cedula 
LEFT JOIN journalists.d_entity e ON SAFE_CAST(e.entity_code AS INT64) = o.codigo
LEFT JOIN journalists.d_jobs j ON j.job_title = cargo
LEFT JOIN journalists.d_date_upload u ON u.processed_date = o.last_update

-- Step 2: If previews employee not in new table, and if that in previews. Go to out employee.
-- insert into :f_employee_salary_out from rowsums.journalists.f_employee_salary if not in this

-- Step 3: add control date. 
-- Step 4: Add new people
-- Step 5: Add new records to f_salary_employee


central_gov_salaries_ago.csv:  principal dashboard
f_salary_ago.csv: actual month
out_people.csv: all peaople of the actual month
out_entities.csv:  actual month, entities list

entities_tbl: out_entities_final.csv .. entities
people_tbl: out_people_all.csv
jobs_tbl: out_final_jobs.csv
records_tbl: out_records.csv dates upload data
final_tbl: out_may-ago.csv - 782,772 x 22



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


update journalists.d_jobs
 set job_position = 'AGENTE_DE INSTRUCCIÓN DELEGADO', job_title = 'AGENTE_DE INSTRUCCIÓN DELEGADO'
 where jobs_id = 23


questions
porque 

-- 157753
INSERT INTO journalists.f_employee_salary
SELECT c.employee_salary_id,
p.people_id, c.person_id, e.entity_id, e.entity_name, 
c.url, c.first_name, c.last_name, 
j. jobs_id , j.job_title, j.job_position, 
c.salary, c.expenses, c. total_income , c.status, c.start_date, 
d.record_id, d. processed_date , d.record_date, 
c.sex, c.key, 
concat(c.person_id, " ", j.job_title) as key1
FROM data_test.staging_central_gov_salaries c
INNER JOIN journalists.d_people p ON p.person_id = c.person_id # 155390
INNER JOIN journalists.d_entity e ON e.entity_code = c.code  # ok - 157753
INNER JOIN journalists.d_jobs j ON j.job_title = c.position # ok - 157753
INNER JOIN journalists.d_date_upload d ON d.record_date = c.record_date
limit 10

select record_id, count(*) as count, sum( total) total, (sum( total) / count(*) ) salary, count(distinct(entity_id)) entities
from journalists.f_employee_salary 
group by record_id
order by record_id asc
limit 7



/* 
select count(*), cost_type, sum(expenses) salary, entity_name 
from journalists.f_travel_expenses 
where entity_name = 'Asamblea Legislativa'
group by entity_name, cost_type
--2890
*/

select date_processed, record_date, job_title, job_position, entity_name, 
       count(*) count, sum(total) total, avg(total) avg_total, min(total) min_total, max(total) max_total, STDDEV(total) sd_total
  from journalists.f_employee_salary 
  group by date_processed, record_date, job_title, job_position, entity_name
  limit 100



/*
update journalists.f_employee_salary_out
   set with_change = 'S'
   where key1 in (
select key1 from journalists.f_employee_salary 
 where record_id = 6
)

select record_date, entity_name, count(*) total from journalists.f_employee_salary_out where key1 in (
select key1 from journalists.f_employee_salary 
 where record_id = 6
)
group by record_date, entity_name 
order by count(*) desc
*/

1
2019-04-03
2019-03-31
0.0
2	
2
2019-05-18
2019-04-30
0.0
3	
3
2019-06-08
2019-05-31
0.0
4	
4
2019-07-15
2019-06-30
0.0
5	
5
2019-08-01
2019-07-31
0.0
6	
6
2019-09-02
2019-08-31
1.0


(153849 + 157304 + 158416 + 156577 + 156518 + 157753) / 6




