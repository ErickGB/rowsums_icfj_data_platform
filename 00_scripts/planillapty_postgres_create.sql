 CREATE DATABASE rowsums_db;
-----------------------------------------------------------------------------
-- Create extensions
-----------------------------------------------------------------------------

-- Enable PostGIS (includes raster)
CREATE EXTENSION postgis;
-- Enable Topology
CREATE EXTENSION postgis_topology;
-- Enable PostGIS Advanced 3D
-- and other geoprocessing algorithms
-- sfcgal not available with all distributions
CREATE EXTENSION postgis_sfcgal; -- error postgis_sfcgal.control": No such file or directory

-----------------------------------------------------------------------------
-- Create schemas
-----------------------------------------------------------------------------

CREATE SCHEMA staging;
CREATE SCHEMA salaries;

CREATE SEQUENCE seq_salary_temp START 1 INCREMENT BY 1;
CREATE SEQUENCE seq_salary_fact START 1 INCREMENT BY 1;
CREATE SEQUENCE seq_people_dim START 1 INCREMENT BY 1;
CREATE SEQUENCE seq_entity_dim START 1 INCREMENT BY 1;

-----------------------------------------------------------------------------
-- Create stagin tables
-----------------------------------------------------------------------------
DROP TABLE rowsums_db.staging.employee_salary;

CREATE TABLE rowsums_db.staging.employee_salary (
	"code" character varying(30) NOT NULL,
	"entity_name" character varying NOT NULL,
	"url_source" character varying NOT NULL,
	"first_name" character varying(60) NOT NULL,
	"last_name" character varying(60) NOT NULL,
	"person_id" character varying(30) NOT NULL,
	"job_title" character varying(255) NOT NULL,
	"salary" money NOT NULL,
	"expenses" money NOT NULL,
	"status" character varying NOT NULL,
	"start_date" DATE NOT NULL,
  "filter_name" character varying(60) NOT NULL,
  "total_income" money NOT NULL,
	"last_update" DATE NOT NULL,
	"record_date" DATE NOT NULL,
	"sex" character varying(255) NOT NULL
) WITH (
  OIDS=FALSE
);
-----------------------------------------------------------------------------
-- Create dimensions and business tables
-----------------------------------------------------------------------------
CREATE TABLE salaries."d_people" (
	"people_id" serial NOT NULL,
	"person_id" character varying(30) NOT NULL UNIQUE,
	"first_name" character varying(60) NOT NULL,
	"last_name" character varying(60) NOT NULL,
	"country_code" character(6) NOT NULL,
	"country_name" character(60) NOT NULL,
	"start_date" DATE NOT NULL,
	"finish_date" DATE NULL,
	"records_count" int NOT NULL,
	"record_date" DATE NOT NULL,
	CONSTRAINT people_pk PRIMARY KEY ("people_id")
) WITH (
  OIDS=FALSE
);

 ALTER TABLE salaries.d_people alter column finish_date drop not null;

CREATE TABLE salaries."d_entity" (
	"entity_id" serial NOT NULL,
	"entity_code" character(3) NOT NULL,
	"entity_name" character varying NOT NULL,
	"record_date" DATE NOT NULL,
	CONSTRAINT entity_pk PRIMARY KEY ("entity_id")
) WITH (
  OIDS=FALSE
);


CREATE TABLE salaries."f_employee_salary" (
	"employee_salary_id" serial NOT NULL,
	"people_id" bigint NOT NULL,
	"person_id" character varying(30) NOT NULL,
	"entity_id" bigint NOT NULL,
	"entity_name" character varying NOT NULL,
	"url_source" character varying NOT NULL,
	"first_name" character varying(60) NOT NULL,
	"last_name" character varying(60) NOT NULL,
	"job_title" character varying(255) NOT NULL,
	"salary" money NOT NULL,
	"expenses" money NOT NULL,
	"status" character varying NOT NULL,
	"start_date" DATE NOT NULL,
	"record_date" DATE NOT NULL
	--,CONSTRAINT fk_employee_id FOREIGN KEY (people_id) REFERENCES d_people (people_id),
	-- CONSTRAINT fk_entity_id FOREIGN KEY (entity_id) REFERENCES d_entity (entity_id)
) WITH (
  OIDS=FALSE
);
ALTER TABLE salaries."f_employee_salary" ADD CONSTRAINT "f_employee_salary_fk0" FOREIGN KEY ("entity_id") REFERENCES salaries."d_entity"("entity_id");
ALTER TABLE salaries."f_employee_salary" ADD CONSTRAINT "f_employee_salary_fk1" FOREIGN KEY ("person_id") REFERENCES salaries."d_people"("person_id");

CREATE TABLE salaries."f_job_resume" (
	"entity_name" character varying NOT NULL,
	"job_title" character varying(255) NOT NULL,
	"date" DATE NOT NULL,
	"records_count" int2 NOT NULL,
	"record_date" DATE NOT NULL
) WITH (
  OIDS=FALSE
);

-----------------------------------------------------------------------------
-- load data
-----------------------------------------------------------------------------
psql -U postgres
\connect rowsums_db
\copy staging.employee_salary from 'out_jun-may-mar.csv' with DELIMITER ',' CSV HEADER;

SELECT COUNT(*) AS TOTAL FROM staging.employee_salary;

CREATE SEQUENCE seq_people_id START 1 INCREMENT BY 1;
CREATE SEQUENCE seq_entity_id START 1 INCREMENT BY 1;
CREATE SEQUENCE seq_employee_id START 1 INCREMENT BY 1;

-----------------------------------------------------------------------------
-- load people
-----------------------------------------------------------------------------
BEGIN TRANSACTION;
INSERT INTO salaries.d_people
SELECT
	nextval('seq_people_id'),
	person_id, 'NULL', 'NULL',
	--first_name,
	--last_name,
	'507',
	'PANAMA',
	min(start_date),
	NULL,
	count(*),
	min(record_date)
 FROM staging.employee_salary
 GROUP BY person_id;

select count(*) from salaries.d_people;
-- 312,128
-- ROLLBACK;

UPDATE salaries.d_people p
	 SET first_name = s.first_name,
		   last_name = s.last_name
  FROM staging.employee_salary s WHERE s.person_id = p.person_id;

COMMIT;

-----------------------------------------------------------------------------
-- load entities
-----------------------------------------------------------------------------
BEGIN TRANSACTION;

INSERT INTO salaries.d_entity
SELECT
	nextval('seq_entity_id'),
	code,
	entity_name,
	MIN(record_date)
 FROM staging.employee_salary
WHERE entity_name NOT IN ('Otros Gastos de la Administracion', 'Ministerio de Comercio e Industria')
 group by  code, entity_name;
SELECT * FROM salaries.d_entity;

COMMIT;

-----------------------------------------------------------------------------
-- load employees
-----------------------------------------------------------------------------
BEGIN TRANSACTION;

INSERT INTO salaries.f_employee_salary
SELECT
	nextval('seq_employee_id'),
	p.people_id, p.person_id,
	e.entity_id,
	s.entity_name,
	s.url_source,
	s.first_name, s.last_name,
	s.job_title,
	s.salary, s.expenses,
	s.status,
	s.start_date,
	s.record_date
 FROM staging.employee_salary s
	INNER JOIN salaries.d_people p ON p.person_id = s.person_id
	INNER JOIN salaries.d_entity e ON e.entity_code = s.code;

 commit ;



 -- 469740
SELECT COUNT(*) FROM staging.employee_salary;
 -- 469740
SELECT COUNT(*) FROM salaries.f_employee_salary;


SELECT record_date, COUNT(*)
	FROM salaries.d_people
 GROUP BY record_date;

SELECT record_date, COUNT(*)
	FROM staging.employee_salary
 GROUP BY record_date;

