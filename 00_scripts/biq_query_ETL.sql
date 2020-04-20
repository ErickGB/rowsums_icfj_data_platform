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

-- 1. están todos los jobs? .. agregar
-- 2. Hay personas nuevas? ... agregar
-- 3. Hay personas desvinculadas? ... pasar a tabla histórica
-- 4. crear tabla lote
-- 5. Pasar a nueva tabla f_employee_salary


-- Step 2: insert new employees.
code: STRING, complete_name: STRING, last_name:STRING, person_id: STRING, position: STRING,
salary: NUMERIC, expenses: NUMERIC, total_income: NUMERIC, status: STRING,
start_date: DATE, first_name: STRING, entity: STRING, update_date: DATE, sex: STRING

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
  FROM rowsums.staging_employee_actual_month p
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




--  new jobs positions
SELECT position, entity, count(*) as total, avg(salary) salary
FROM data_test.staging_central_gov_salaries where position not in (
  SELECT job_title FROM journalists.d_jobs
) GROUP BY position, entity

UPDATE data_test.staging_central_gov_salaries
   SET position = 'AGENTE_DE INSTRUCCIÓN DELEGADO'
 WHERE position = 'AGENTE_DE INSTRUCCI`N DELEGADO';


-- new employees
SELECT count(*) as total, avg(salary) salary, max(salary) max, min(salary) min
FROM data_test.staging_central_gov_salaries where person_id not in (
  select  person_id from journalists.d_people
)

SELECT person_id
FROM data_test.staging_central_gov_salaries where person_id not in (
  select  person_id from journalists.d_people
)


-- total,salary,max,min
-- 1735,1563.244887608,6500.000000000,202.000000000

-- ex-employees
UPDATE data_test.staging_central_gov_salaries
   set key = concat(person_id, " ", position, " _")
  where 1 = 1;

SELECT count(*) as total, avg(salary) salary, max(salary) max, min(salary) min
FROM journalists.f_employee_salary where record_id = 1 and key1 not in (
  select key from data_test.staging_central_gov_salaries
);

1-12066
2-10726
3 -10133
4 -4089

SELECT count(*) as total, avg(salary) salary, max(salary) max, min(salary) min
FROM journalists.f_employee_salary where record_id = 3 and key1 not in (
  select key1 FROM journalists.f_employee_salary where record_id = 2
);

select count(*), person_id from journalists.f_employee_salary where record_id = 3 group by person_id having count(*) > 1;

select * from journalists.f_employee_salary where person_id = '9-0704-00279' and record_id = 3;
'4-0222-00007'

--total,salary,max,min
--2566,1613.380997662,8884.000000000,202.000000000

select count(*), record_id from journalists.f_employee_salary_out group by record_id
2098 + 892 + 6671 + 1260 + 2919




SELECT * FROM journalists.d_jobs where job_title = 'AGENTE_DE INSTRUCCION DELEGADO'

SELECT person_id, entity_name, first_name, last_name, job_title,
salary, expenses, (salary + expenses) as total, start_date, record_date
FROM `rowsums.journalists.f_employee_salary` where person_id = '8-0344-00140'

SELECT * FROM `rowsums.journalists.central_gov_salaries` where ltrim(rtrim(person_id))  = '8-0344-00140'


--
-- dos salarios
'4-0222-00007'  educador y asesor..
otros gastos de la administración = representantes con dos salarios..


select * from journalists.f_employee_salary where person_id = '7-0084-00473' and record_id = 6;


 select record_id, count(*) as count, sum( total) total, (sum( total) / count(*) ) salary, count(distinct(entity_id)) entities
from journalists.f_employee_salary
group by record_id
order by record_id asc
limit 7


SELECT * FROM journalists.f_employee_salary where job_title = 'DIRECTOR_GENERAL DEL SERVICIO NACIONAL DE MIGRACION'

SELECT COUNT(*) FROM journalists.d_jobs




SELECT * FROM journalists.f_employee_salary;

select * from journalists.f_travel_expenses;

select * from journalists.historical_gov_employees;


--
select record_id, date_processed , count(*) count, round(sum(total)/1000000, 2) total_en_mm, round(sum(total) / count(*), 0) average,
count(distinct entity_id) entities
from journalists.f_employee_salary
group by record_id, date_processed
order by date_processed asc


select count(*) from journalists.view_out_employee where person_id in
(select person_id from journalists.f_employee_salary where record_id = 7);

-- 7257 - 1483

group by job_title order by count(*) desc;

select
  from journalists.f_employee_salary
 where entity_id = 5
   and record_id = 7
 group by start_date
 order by start_date



select start_date, count(*) as count
  from journalists.f_employee_salary
 where entity_id = 5
   and record_id = 7
 group by start_date
 order by start_date


 select start_date, count(*) as count
  from journalists.f_employee_salary
 where entity_id = 5
   and record_id = 3
 group by start_date
 order by start_date




-- 5,949
select count(*) count, round(avg(total), 2) avg_total
from journalists.f_employee_salary e
inner join journalists.d_date_upload d ON e.record_id = d.record_id
where start_date >= '2019-07-01'
  and is_actual = 1;

-- 5,115
select count(*) count, round(avg(total), 2) avg_total
from journalists.f_employee_salary e
where record_id = 4
  and person_id not in (
  select person_id
  from journalists.f_employee_salary f
  inner join journalists.d_date_upload d ON f.record_id = d.record_id
  where is_actual = 1);


-- in
SELECT  f.person_id, e.entity_name,
  f.first_name, f.last_name, f.job_title, f.job_position,
  CAST(f.salary AS FLOAT64) salary, CAST(f.expenses AS FLOAT64) expenses, f.status,
  f.start_date, CAST('2099-12-31' AS DATE) finish_date, f.record_date, 'in' as record_type, is_actual, 'N' with_change
FROM rowsums.journalists.f_employee_salary f
INNER JOIN rowsums.journalists.d_date_upload u ON f.record_id = u.record_id
INNER JOIN rowsums.journalists.d_entity e ON f.entity_id = e.entity_id
WHERE start_date >= '2019-04-01'

-- out
SELECT  f.person_id, e.entity_name,
  f.first_name, f.last_name, f.job_title, f.job_position,
  CAST(f.salary AS FLOAT64) salary, CAST(f.expenses AS FLOAT64) expenses, f.status,
  f.start_date, f.date_processed as finish_date, f.record_date, 'out' as record_type, is_actual, with_change
FROM  rowsums.journalists.f_employee_salary_out f
INNER JOIN rowsums.journalists.d_date_upload u ON f.record_id = u.record_id
INNER JOIN rowsums.journalists.d_entity e ON f.entity_id = e.entity_id


select entity_id, entity_name, s.record_date, u.processed_date,  sum(total) as total
  from journalists.f_employee_salary s
  inner join journalists.d_date_upload u on u.record_id = s.record_id
  where entity_id = 9
  group by entity_id, record_date, entity_name, u.processed_date
  order by record_date

-- ******************************************
-- insert trade

insert into trade.fact_import
 select GENERATE_UUID(), ca.category_id, co.country_id, 
 cast(concat(substr(cast(im.date as string), 1, 4), substr(cast(im.date as string), 6, 2), substr(cast(im.date as string), 9, 2)) as INT64), 
 link, date, RUC, company, country_origin_code, 
 description, key, original_text, quantity_text, 
 tariff_fraction, import_taxes, oil_protection_taxes, isc_taxes, itbms_taxes, 
 gross_weight_text, net_weight_text, port, total_to_pay, cif, freight_value, insurance, fob, quantity, gross_weight, net_weight,
 day, month, cast(extract(year from im.date) as int64), country_data, year_month_date, 
  cast(concat(substr(cast(im.date as string), 1, 8),'01') as date), 2
   from data_test.staging_imports im 
   inner join trade.dim_category ca on ca.sub_category_code = substr(tariff_fraction, 1, 2)
   inner join trade.dim_country co on co.alpha_2 = country_origin_code 
   where input_date > cast('2019-12-01' as date) 


insert into trade.fact_agg_product
select co. alpha_2 , co.name, region, sub_region, latitude, longitude, company, RUC, category_code, 
sub_category_code, category, sub_category, port, 
count(*), sum(total_to_pay), sum(cif), sum(freight_value), sum(insurance), 
sum(fob), sum(quantity), sum(gross_weight), sum(net_weight), 0, 0, 
month, year, year_month_date, year_date
from trade.fact_import fi
inner join trade.dim_category ca on  fi.category_id = ca.category_id
inner join trade.dim_country co on  fi.country_id = co.country_id
where record_id = 2 
group by co. alpha_2 , co.name, region, sub_region, latitude, longitude, company, RUC, category_code, 
sub_category_code, category, sub_category,month, year, year_month_date, year_date 



select person_id cedula, entity_name entidad, first_name nombre, last_name apellido, job_title cargo, 
       salary salario, expenses gastos, total, start_date fecha_inicio, sex sexo_estimado_x_nombre, 
       record_date as process_date, date_processed as record_date
  from journalists.f_employee_salary 
 where record_id in (9, 10) 
   and entity_id in (14)
 order by record_id, entity_name asc 




-- ******************************************
-- consultas doble salario

/*
select * from journalists.view_jobs_summary limit 10

 
 select entity_name, date_processed as data_date, sum(count) count, round(sum(sum_total), 2) as total
  from journalists.view_jobs_summary 
 group by entity_name,date_processed 
 order by date_processed desc

select entity_name, job_position, job_title, date_processed as data_date, sum(count) count, round(sum(sum_total), 2) as total
  from journalists.view_jobs_summary 
 where entity_name in ('Asamblea Legislativa', 'Presidencia de la República')
 group by entity_name, job_position, job_title, date_processed 
 
 
 select entity_name, person_id, first_name, last_name, job_position, job_title, salary, expenses, overtime, total, status, start_date, date_processed
  from journalists.f_employee_salary 
 where record_id = 9 
   and entity_id = 8 
 */


select entity_name, person_id, first_name, last_name, job_position, job_title, salary, expenses, overtime, total, status, start_date, date_processed
  from journalists.f_employee_salary 
 where record_id = 9 
   and entity_id = 8 
   and job_title = 'AYUDANTE_GENERAL'
   
   





