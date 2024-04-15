----------------------------------
-- Reconstruct the database template
----------------------------------

-- First create the userrole, user and an empty database (e.g. in pgAdmin or psql)

-- create role for birdsong management
DROP ROLE IF EXISTS birdsong_role;
CREATE ROLE birdsong_role WITH
  NOLOGIN
  NOSUPERUSER
  INHERIT
  NOCREATEDB
  NOCREATEROLE
  NOREPLICATION;

COMMENT ON ROLE birdsong_role IS 'birdsong management user role';

-- create user for birdsong management. The name of the user is 'birdsong_user', the passwort is 'birdsong'.
DROP ROLE IF EXISTS birdsong_user;
CREATE ROLE birdsong_user WITH
  LOGIN
  NOSUPERUSER
  INHERIT
  NOCREATEDB
  NOCREATEROLE
  NOREPLICATION
  ENCRYPTED PASSWORD 'md5c713f856bf400b7eee90729a2056dfdc';

GRANT birdsong_role TO birdsong_user;

COMMENT ON ROLE birdsong_user IS 'birdsong management user';

-- Create database

CREATE DATABASE birdsong_db
    WITH 
    OWNER = postgres
    ENCODING = 'UTF8'
    LC_COLLATE = 'de_DE.UTF-8'
    LC_CTYPE = 'de_DE.UTF-8'
    TABLESPACE = pg_default
    CONNECTION LIMIT = -1
    TEMPLATE template0;

COMMENT ON DATABASE birdsong_db
    IS 'Database for birdsong management';

ALTER DATABASE birdsong_db
    SET "TimeZone" TO 'UTC';

GRANT ALL ON DATABASE birdsong_db TO postgres;

GRANT TEMPORARY, CONNECT ON DATABASE birdsong_db TO PUBLIC;

GRANT CONNECT ON DATABASE birdsong_db TO birdsong_role;
DROP EXTENSION IF EXISTS postgis;
CREATE EXTENSION postgis;
