--
-- Copyright (c) 2016 Todd Kover
-- All rights reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

/*
Invoked:

	-suffix=v71
	--pre
	pre
	--pre
	../opensource/database/ddl/schema/pgsql/create_schema_support_tables.sql
	--post
	../opensource/database/pkg/pgsql/backend_utils.sql
	--post
	post
	--first
	schema_support
*/

\set ON_ERROR_STOP
SELECT schema_support.begin_maintenance();
select timeofday(), now();


-- BEGIN Misc that does not apply to above
DROP INDEX IF EXISTS ak_netblock_params;


-- END Misc that does not apply to above


-- BEGIN Misc that does not apply to above
/*
 * Copyright (c) 2016 Todd Kover
 * All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


--
-- $HeadURL$
-- $Id$
--


-- Create schema if it does not exist, do nothing otherwise.
DO $$
DECLARE
	_tal INTEGER;
BEGIN
	select count(*) 
	from pg_catalog.pg_namespace 
	into _tal
	where nspname = 'schema_support';
	IF _tal = 0 THEN
		DROP SCHEMA IF EXISTS schema_support;
		CREATE SCHEMA schema_support AUTHORIZATION jazzhands;
		COMMENT ON SCHEMA schema_support IS 'part of jazzhands';

	END IF;
END;
$$;

--
-- These tables are meant to be used solely by schema_support functions.
--
CREATE TABLE schema_support.schema_audit_map (
	schema	text,
	audit_schema text,
	primary key(schema, audit_schema)
);

CREATE TABLE schema_support.mv_refresh (
	schema	text,
	view 	text,
	refresh	timestamp,
	primary key(schema, view)
);


CREATE TABLE schema_support.schema_version (
	schema	text,
	version	text,
	primary key(schema)
);


-- END Misc that does not apply to above
CREATE SCHEMA backend_utils AUTHORIZATION jazzhands;
--
-- Process middle (non-trigger) schema jazzhands
--
-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'dns_a_rec_validation');
CREATE OR REPLACE FUNCTION jazzhands.dns_a_rec_validation()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_ip		netblock.ip_address%type;
	_sing	netblock.is_single_address%type;
BEGIN
	IF NEW.dns_type in ('A', 'AAAA') AND NEW.netblock_id IS NULL THEN
		RAISE EXCEPTION 'Attempt to set % record without a Netblock',
			NEW.dns_type
			USING ERRCODE = 'not_null_violation';
	END IF;

	IF NEW.netblock_Id is not NULL and 
			( NEW.dns_value IS NOT NULL OR NEW.dns_value_record_id IS NOT NULL ) THEN
		RAISE EXCEPTION 'Both dns_value and netblock_id may not be set'
			USING ERRCODE = 'JH001';
	END IF;

	IF NEW.dns_value IS NOT NULL AND NEW.dns_value_record_id IS NOT NULL THEN
		RAISE EXCEPTION 'Both dns_value and dns_value_record_id may not be set'
			USING ERRCODE = 'JH001';
	END IF;

	IF NEW.netblock_id IS NOT NULL AND NEW.dns_value_record_id IS NOT NULL THEN
		RAISE EXCEPTION 'Both netblock_id and dns_value_record_id may not be set'
			USING ERRCODE = 'JH001';
	END IF;

	-- XXX need to deal with changing a netblock type and breaking dns_record.. 
	IF NEW.netblock_id IS NOT NULL THEN
		SELECT ip_address, is_single_address
		  INTO _ip, _sing
		  FROM netblock
		 WHERE netblock_id = NEW.netblock_id;

		IF NEW.dns_type = 'A' AND family(_ip) != '4' THEN
			RAISE EXCEPTION 'A records must be assigned to non-IPv4 records'
				USING ERRCODE = 'JH200';
		END IF;

		IF NEW.dns_type = 'AAAA' AND family(_ip) != '6' THEN
			RAISE EXCEPTION 'AAAA records must be assigned to non-IPv6 records'
				USING ERRCODE = 'JH200';
		END IF;

		IF _sing = 'N' AND NEW.dns_type IN ('A','AAAA') THEN
			RAISE EXCEPTION 'Non-single addresses may not have % records', NEW.dns_type 
				USING ERRCODE = 'foreign_key_violation';
		END IF;

	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'validate_device_component_assignment');
CREATE OR REPLACE FUNCTION jazzhands.validate_device_component_assignment()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	dtid		device_type.device_type_id%TYPE;
	dt_ctid		component.component_type_id%TYPE;
	ctid		component.component_type_id%TYPE;
BEGIN
	-- If no component_id is set, then we're done

	IF NEW.component_id IS NULL THEN
		RETURN NEW;
	END IF;

	SELECT
		device_type_id, component_type_id 
	INTO
		dtid, dt_ctid
	FROM
		device_type
	WHERE
		device_type_id = NEW.device_type_id;
	
	IF NOT FOUND OR dt_ctid IS NULL THEN
		RAISE EXCEPTION 'No component_type_id set for device type'
		USING ERRCODE = 'foreign_key_violation';
	END IF;

	SELECT
		component_type_id INTO ctid
	FROM
		component
	WHERE
		component_id = NEW.component_id;
	
	IF NOT FOUND OR ctid IS DISTINCT FROM dt_ctid THEN
		RAISE EXCEPTION 'Component type of component_id % (%s) does not match component_type for device_type_id % (%)',
			NEW.component_id, ctid, dtid, dt_ctid
		USING ERRCODE = 'foreign_key_violation';
	END IF;

	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.company_insert_function_nudge()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	BEGIN
		IF current_setting('jazzhands.permit_company_insert') != 'permit' THEN
			RAISE EXCEPTION  'You may not directly insert into company.'
				USING ERRCODE = 'insufficient_privilege';
		END IF;
	EXCEPTION WHEN undefined_object THEN
			RAISE EXCEPTION  'You may not directly insert into company'
				USING ERRCODE = 'insufficient_privilege';
	END;
	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.dns_record_check_name()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	IF NEW.DNS_NAME IS NOT NULL THEN
		-- rfc rfc952
		IF NEW.DNS_NAME !~ '[-a-zA-Z0-9\._]*' THEN
			RAISE EXCEPTION 'Invalid DNS NAME %', 
				NEW.DNS_NAME
				USING ERRCODE = 'integrity_constraint_violation';
		END IF;
	END IF;
	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.l2_net_coll_member_enforce_on_type_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	l2ct		val_layer2_network_coll_type%ROWTYPE;
	old_l2ct	val_layer2_network_coll_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	l2ct
	FROM	val_layer2_network_coll_type
	WHERE	layer2_network_collection_type = NEW.layer2_network_collection_type;

	SELECT *
	INTO	old_l2ct
	FROM	val_layer2_network_coll_type
	WHERE	layer2_network_collection_type = OLD.layer2_network_collection_type;

	--
	-- We only need to check this if we are enforcing now where we didn't used
	-- to need to
	--
	IF l2ct.max_num_members IS NOT NULL AND 
			l2ct.max_num_members IS DISTINCT FROM old_l2ct.max_num_members THEN
		select count(*)
		  into tally
		  from l2_network_coll_l2_network
		  where layer2_network_collection_id = NEW.layer2_network_collection_id;
		IF tally > l2ct.max_num_members THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF l2ct.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		SELECT MAX(l2count) FROM (
			SELECT
				COUNT(*) AS l2count
			FROM
				l2_network_coll_l2_network JOIN
				layer2_network_collection USING (layer2_network_collection_id)
			WHERE
				layer2_network_collection_type = NEW.layer2_network_collection_type
			GROUP BY
				layer2_network_id
		) x INTO tally;

		IF tally > l2ct.max_num_collections THEN
			RAISE EXCEPTION 'Layer2 network may not be a member of more than % collections of type %',
				l2ct.MAX_NUM_COLLECTIONS, l2ct.layer2_network_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.l3_net_coll_member_enforce_on_type_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	l3ct		val_layer3_network_coll_type%ROWTYPE;
	old_l3ct	val_layer3_network_coll_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	l3ct
	FROM	val_layer3_network_coll_type
	WHERE	layer3_network_collection_type = NEW.layer3_network_collection_type;

	SELECT *
	INTO	old_l3ct
	FROM	val_layer3_network_coll_type
	WHERE	layer3_network_collection_type = OLD.layer3_network_collection_type;

	--
	-- We only need to check this if we are enforcing now where we didn't used
	-- to need to
	--
	IF l3ct.max_num_members IS NOT NULL AND 
			l3ct.max_num_members IS DISTINCT FROM old_l3ct.max_num_members THEN
		select count(*)
		  into tally
		  from l3_network_coll_l3_network
		  where layer3_network_collection_id = NEW.layer3_network_collection_id;
		IF tally > l3ct.max_num_members THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF l3ct.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		SELECT MAX(l3count) FROM (
			SELECT
				COUNT(*) AS l3count
			FROM
				l3_network_coll_l3_network JOIN
				layer3_network_collection USING (layer3_network_collection_id)
			WHERE
				layer3_network_collection_type = NEW.layer3_network_collection_type
			GROUP BY
				layer3_network_id
		) x INTO tally;

		IF tally > l3ct.max_num_collections THEN
			RAISE EXCEPTION 'Layer2 network may not be a member of more than % collections of type %',
				l3ct.MAX_NUM_COLLECTIONS, l3ct.layer3_network_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.nb_dns_a_rec_validation()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tal	integer;
BEGIN
	IF family(OLD.ip_address) != family(NEW.ip_address) THEN
		IF family(NEW.ip_address) == 6 THEN
			SELECT count(*)
			INTO	_tal
			FROM	dns_record
			WHERE	netblock_id = NEW.netblock_id
			AND		dns_type = 'A';

			IF _tal > 0 THEN
				RAISE EXCEPTION 'A records must be assigned to IPv4 records'
					USING ERRCODE = 'JH200';
			END IF;
		END IF;
	END IF;

	IF family(OLD.ip_address) != family(NEW.ip_address) THEN
		IF family(NEW.ip_address) == 4 THEN
			SELECT count(*)
			INTO	_tal
			FROM	dns_record
			WHERE	netblock_id = NEW.netblock_id
			AND		dns_type = 'AAAA';

			IF _tal > 0 THEN
				RAISE EXCEPTION 'AAAA records must be assigned to IPv6 records'
					USING ERRCODE = 'JH200';
			END IF;
		END IF;
	END IF;

	IF NEW.is_single_address = 'N' THEN
			SELECT count(*)
			INTO	_tal
			FROM	dns_record
			WHERE	netblock_id = NEW.netblock_id
			AND		dns_type IN ('A', 'AAAA');

		IF _tal > 0 THEN
			RAISE EXCEPTION 'Non-single addresses may not have % records', NEW.dns_type 
				USING ERRCODE = 'foreign_key_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.pgnotify_account_collection_account_token_changes()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	IF TG_OP = 'UPDATE' OR TG_OP = 'DELETE' THEN
		PERFORM	*
		FROM	property_collection
				JOIN property_collection_property pcp 
					USING (property_collection_id)
				JOIN property p
					USING (property_name, property_type)
		WHERE	p.account_collection_id = OLD.account_collection_id
		AND		property_collection_type = 'jazzhands-internal'
		AND		property_collection_name = 'notify-account_collection_account'
		;

		IF FOUND THEN
			PERFORM pg_notify('account_change', concat('account_id=', OLD.account_id));
		END IF;
	END IF;
	IF TG_OP = 'UPDATE' OR TG_OP = 'INSERT' THEN
		PERFORM	*
		FROM	property_collection
				JOIN property_collection_property pcp 
					USING (property_collection_id)
				JOIN property p
					USING (property_name, property_type)
		WHERE	p.account_collection_id = NEW.account_collection_id
		AND		property_collection_type = 'jazzhands-internal'
		AND		property_collection_name = 'notify-account_collection_account'
		;

		IF FOUND THEN
			PERFORM pg_notify('account_change', concat('account_id=', NEW.account_id));
		END IF;
	END IF;

	IF TG_OP = 'DELETE' THEN
		RETURN OLD;
	ELSE
		RETURN NEW;
	END IF;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.pgnotify_account_password_changes()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	PERFORM pg_notify ('account_password_change', 'account_id=' || NEW.account_id);
	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.pgnotify_account_token_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	PERFORM pg_notify ('account_id', 'account_id=' || NEW.account_id);
	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.pgnotify_token_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	PERFORM pg_notify ('token_change', 'token_id=' || NEW.token_id);
	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.unrequire_password_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	DELETE FROM account_collection_account
	WHERE (account_collection_id, account_id)
	IN (
		SELECT	a.account_collection_id, a.account_id
		FROM	v_acct_coll_acct_expanded a
				JOIN account_collection ac USING (account_collection_id)
				JOIN property p USING (account_collection_id)
		WHERE	p.property_type = 'UserMgmt'
		AND		p.property_name = 'NeedsPasswdChange'
		AND	 	a.account_id = NEW.account_id
	);
	RETURN NEW;
END;
$function$
;

--
-- Process middle (non-trigger) schema net_manip
--
--
-- Process middle (non-trigger) schema network_strings
--
--
-- Process middle (non-trigger) schema time_util
--
--
-- Process middle (non-trigger) schema dns_utils
--
--
-- Process middle (non-trigger) schema person_manip
--
-- New function
CREATE OR REPLACE FUNCTION person_manip.add_user(company_id integer, person_company_relation character varying, login character varying DEFAULT NULL::character varying, first_name character varying DEFAULT NULL::character varying, middle_name character varying DEFAULT NULL::character varying, last_name character varying DEFAULT NULL::character varying, name_suffix character varying DEFAULT NULL::character varying, gender character varying DEFAULT NULL::character varying, preferred_last_name character varying DEFAULT NULL::character varying, preferred_first_name character varying DEFAULT NULL::character varying, birth_date date DEFAULT NULL::date, external_hr_id character varying DEFAULT NULL::character varying, person_company_status character varying DEFAULT 'enabled'::character varying, is_management character varying DEFAULT 'N'::character varying, is_manager character varying DEFAULT NULL::character varying, is_exempt character varying DEFAULT 'Y'::character varying, is_full_time character varying DEFAULT 'Y'::character varying, employee_id text DEFAULT NULL::text, hire_date date DEFAULT NULL::date, termination_date date DEFAULT NULL::date, position_title character varying DEFAULT NULL::character varying, job_title character varying DEFAULT NULL::character varying, department_name character varying DEFAULT NULL::character varying, manager_person_id integer DEFAULT NULL::integer, site_code character varying DEFAULT NULL::character varying, physical_address_id integer DEFAULT NULL::integer, person_location_type character varying DEFAULT 'office'::character varying, description character varying DEFAULT NULL::character varying, unix_uid character varying DEFAULT NULL::character varying, INOUT person_id integer DEFAULT NULL::integer, OUT dept_account_collection_id integer, OUT account_id integer)
 RETURNS record
 LANGUAGE plpgsql
 SECURITY DEFINER
AS $function$
DECLARE
    _account_realm_id INTEGER;
    _account_type VARCHAR;
    _uid INTEGER;
    _uxaccountid INTEGER;
    _companyid INTEGER;
    _personid INTEGER;
    _accountid INTEGER;
BEGIN
	IF is_manager IS NOT NULL THEN
		is_management := is_manager;
	END IF;

	IF job_title IS NOT NULL THEN
		position_title := job_title;
	END IF;

    IF company_id is NULL THEN
        RAISE EXCEPTION 'Must specify company id';
    END IF;
    _companyid := company_id;

    SELECT arc.account_realm_id 
      INTO _account_realm_id 
      FROM account_realm_company arc
     WHERE arc.company_id = _companyid;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'Cannot find account_realm_id with company id %', company_id;
    END IF;

    IF login is NULL THEN
        IF first_name IS NULL or last_name IS NULL THEN 
            RAISE EXCEPTION 'Must specify login name or first name+last name';
        ELSE 
            login := person_manip.pick_login(
                in_account_realm_id := _account_realm_id,
                in_first_name := coalesce(preferred_first_name, first_name),
                in_middle_name := middle_name,
                in_last_name := coalesce(preferred_last_name, last_name)
            );
        END IF;
    END IF;

    IF person_company_relation = 'pseudouser' THEN
        person_id := 0;
        _account_type := 'pseudouser';
    ELSE
        _account_type := 'person';
        IF person_id IS NULL THEN
            INSERT INTO person (first_name, middle_name, last_name, name_suffix, gender, preferred_first_name, preferred_last_name, birth_date)
                VALUES (first_name, middle_name, last_name, name_suffix, gender, preferred_first_name, preferred_last_name, birth_date)
            RETURNING person.person_id into _personid;
            person_id = _personid;
        ELSE
            INSERT INTO person (person_id, first_name, middle_name, last_name, name_suffix, gender, preferred_first_name, preferred_last_name, birth_date)
                VALUES (person_id, first_name, middle_name, last_name, name_suffix, gender, preferred_first_name, preferred_last_name, birth_date);
        END IF;
        INSERT INTO person_company
            (person_id, company_id, external_hr_id, person_company_status, is_management, is_exempt, is_full_time, employee_id, hire_date, termination_date, person_company_relation, position_title, manager_person_id)
            VALUES
            (person_id, company_id, external_hr_id, person_company_status, is_management, is_exempt, is_full_time, employee_id, hire_date, termination_date, person_company_relation, position_title, manager_person_id);
        INSERT INTO person_account_realm_company ( person_id, company_id, account_realm_id) VALUES ( person_id, company_id, _account_realm_id);
    END IF;

    INSERT INTO account ( login, person_id, company_id, account_realm_id, account_status, description, account_role, account_type)
        VALUES (login, person_id, company_id, _account_realm_id, person_company_status, description, 'primary', _account_type)
    RETURNING account.account_id INTO account_id;

    IF department_name IS NOT NULL THEN
        dept_account_collection_id = person_manip.get_account_collection_id(department_name, 'department');
        INSERT INTO account_collection_account (account_collection_id, account_id) VALUES ( dept_account_collection_id, account_id);
    END IF;

    IF site_code IS NOT NULL AND physical_address_id IS NOT NULL THEN
        RAISE EXCEPTION 'You must provide either site_code or physical_address_id NOT both';
    END IF;

    IF site_code IS NULL AND physical_address_id IS NOT NULL THEN
        site_code = person_manip.get_site_code_from_physical_address_id(physical_address_id);
    END IF;

    IF physical_address_id IS NULL AND site_code IS NOT NULL THEN
        physical_address_id = person_manip.get_physical_address_from_site_code(site_code);
    END IF;

    IF physical_address_id IS NOT NULL AND site_code IS NOT NULL THEN
        INSERT INTO person_location 
            (person_id, person_location_type, site_code, physical_address_id)
        VALUES
            (person_id, person_location_type, site_code, physical_address_id);
    END IF;


    IF unix_uid IS NOT NULL THEN
        _accountid = account_id;
        SELECT  aui.account_id
          INTO  _uxaccountid
          FROM  account_unix_info aui
        WHERE  aui.account_id = _accountid;

        --
        -- This is creatd by trigger for non-pseudousers, which will
        -- eventually change, so this is here once it goes away.
        --
        IF _uxaccountid IS NULL THEN
            IF unix_uid = 'auto' THEN
                _uid :=  person_manip.get_unix_uid(_account_type);
            ELSE
                _uid := unix_uid::int;
            END IF;

            PERFORM person_manip.setup_unix_account(
                in_account_id := account_id,
                in_account_type := _account_type,
                in_uid := _uid
            );
        END IF;
    END IF;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION person_manip.get_physical_address_from_site_code(_site_code character varying)
 RETURNS integer
 LANGUAGE plpgsql
 SECURITY DEFINER
AS $function$
DECLARE
	_physical_address_id INTEGER;
BEGIN
	SELECT physical_address_id INTO _physical_address_id
		FROM physical_address
		INNER JOIN site USING(physical_address_id)
		WHERE site_code = _site_code;
	RETURN _physical_address_id;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION person_manip.get_site_code_from_physical_address_id(_physical_address_id integer)
 RETURNS character varying
 LANGUAGE plpgsql
 SECURITY DEFINER
AS $function$
DECLARE
	_site_code VARCHAR;
BEGIN
	SELECT site_code INTO _site_code
		FROM physical_address
		INNER JOIN site USING(physical_address_id)
		WHERE physical_address_id = _physical_address_id;
	RETURN _site_code;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION person_manip.set_location(person_id integer, new_site_code character varying DEFAULT NULL::character varying, new_physical_address_id integer DEFAULT NULL::integer, person_location_type character varying DEFAULT 'office'::character varying)
 RETURNS void
 LANGUAGE plpgsql
 SECURITY DEFINER
AS $function$
DECLARE
	_person_id INTEGER;
	_person_location_type VARCHAR;
	_existing_person_location_id INTEGER;
BEGIN
	_person_id = person_id;
	_person_location_type = person_location_type;

	IF ( new_site_code IS NULL AND new_physical_address_id IS NULL )
		OR ( new_site_code IS NOT NULL AND new_physical_address_id IS NOT NULL ) THEN
			RAISE EXCEPTION 'Must specify either new_site_code or new_physical_address';
	END IF;

	IF new_site_code IS NOT NULL AND new_physical_address_id IS NULL THEN
		new_physical_address_id = person_manip.get_physical_address_from_site_code(new_site_code);
	END IF;

	IF new_physical_address_id IS NOT NULL AND new_site_code IS NULL THEN
		new_site_code = person_manip.get_site_code_from_physical_address_id(new_physical_address_id);
	END IF;

	SELECT person_location_id INTO _existing_person_location_id
	FROM person_location pl
	WHERE pl.person_id = _person_id AND pl.person_location_type = _person_location_type;

	IF _existing_person_location_id IS NULL THEN
		INSERT INTO person_location
			(person_id, person_location_type, site_code, physical_address_id)
		VALUES
			(_person_id, _person_location_type, new_site_code, new_physical_address_id);
	ELSE
		UPDATE person_location
		SET (site_code, physical_address_id, building, floor, section, seat_number)
		= (new_site_code, new_physical_address_id, NULL, NULL, NULL, NULL)
		WHERE person_location_id = _existing_person_location_id;
	END IF;
END;
$function$
;

--
-- Process middle (non-trigger) schema auto_ac_manip
--
--
-- Process middle (non-trigger) schema company_manip
--
-- Changed function
SELECT schema_support.save_grants_for_replay('company_manip', 'add_company');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS company_manip.add_company ( _company_name text, _company_types text[], _parent_company_id integer, _account_realm_id integer, _company_short_name text, _description text );
CREATE OR REPLACE FUNCTION company_manip.add_company(_company_name text, _company_types text[] DEFAULT NULL::text[], _parent_company_id integer DEFAULT NULL::integer, _account_realm_id integer DEFAULT NULL::integer, _company_short_name text DEFAULT NULL::text, _description text DEFAULT NULL::text)
 RETURNS integer
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_cmpid	company.company_id%type;
	_short	text;
	_isfam	char(1);
	_perm	text;
BEGIN
	IF _company_types @> ARRAY['corporate family'] THEN
		_isfam := 'Y';
	ELSE
		_isfam := 'N';
	END IF;
	IF _company_short_name IS NULL and _isfam = 'Y' THEN
		_short := lower(regexp_replace(
				regexp_replace(
					regexp_replace(_company_name, 
						E'\\s+(ltd|sarl|limited|pt[ye]|GmbH|ag|ab|inc)', 
						'', 'gi'),
					E'[,\\.\\$#@]', '', 'mg'),
				E'\\s+', '_', 'gi'));
	ELSE
		_short := _company_short_name;
	END IF;

	BEGIN
		_perm := current_setting('jazzhands.permit_company_insert');
	EXCEPTION WHEN undefined_object THEN
		_perm := '';
	END;

	SET jazzhands.permit_company_insert = 'permit';

	INSERT INTO company (
		company_name, company_short_name,
		parent_company_id, description
	) VALUES (
		_company_name, _short,
		_parent_company_id, _description
	) RETURNING company_id INTO _cmpid;

	SET jazzhands.permit_company_insert = _perm;

	IF _account_realm_id IS NOT NULL THEN
		INSERT INTO account_realm_company (
			account_realm_id, company_id
		) VALUES (
			_account_realm_id, _cmpid
		);
	END IF;

	IF _company_types IS NOT NULL THEN
		PERFORM company_manip.add_company_types(_cmpid, _account_realm_id, _company_types);
	END IF;

	RETURN _cmpid;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('company_manip', 'add_company_types');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS company_manip.add_company_types ( _company_id integer, _account_realm_id integer, _company_types text[] );
CREATE OR REPLACE FUNCTION company_manip.add_company_types(_company_id integer, _account_realm_id integer DEFAULT NULL::integer, _company_types text[] DEFAULT NULL::text[])
 RETURNS integer
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	x		text;
	count	integer;
BEGIN
	count := 0;
	FOREACH x IN ARRAY _company_types
	LOOP
		INSERT INTO company_type (company_id, company_type)
			VALUES (_company_id, x);
		IF _account_realm_id IS NOT NULL THEN
			PERFORM company_manip.add_auto_collections(_company_id, _account_realm_id, x);
		END IF;
		count := count + 1;
	END LOOP;
	return count;
END;
$function$
;

--
-- Process middle (non-trigger) schema token_utils
--
--
-- Process middle (non-trigger) schema port_support
--
--
-- Process middle (non-trigger) schema port_utils
--
--
-- Process middle (non-trigger) schema device_utils
--
-- Changed function
SELECT schema_support.save_grants_for_replay('device_utils', 'retire_device');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS device_utils.retire_device ( in_device_id integer, retire_modules boolean );
CREATE OR REPLACE FUNCTION device_utils.retire_device(in_device_id integer, retire_modules boolean DEFAULT false)
 RETURNS boolean
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	tally		INTEGER;
	_r			RECORD;
	_d			DEVICE%ROWTYPE;
	_mgrid		DEVICE.DEVICE_ID%TYPE;
	_purgedev	boolean;
BEGIN
	_purgedev := false;

	BEGIN
		PERFORM local_hooks.device_retire_early(in_Device_Id, false);
	EXCEPTION WHEN invalid_schema_name OR undefined_function THEN
		PERFORM 1;
	END;

	SELECT * INTO _d FROM device WHERE device_id = in_Device_id;
	delete from dns_record where netblock_id in (
		select netblock_id 
		from network_interface where device_id = in_Device_id
	);

	delete from network_interface_purpose where device_id = in_Device_id;

	DELETE FROM network_interface_netblock
	WHERE network_interface_id IN (
			SELECT network_interface_id
		 	FROM network_interface
			WHERE device_id = in_Device_id
	);

	DELETE FROM network_interface WHERE device_id = in_Device_id;

	PERFORM device_utils.purge_physical_ports( in_Device_id);
--	PERFORM device_utils.purge_power_ports( in_Device_id);

	delete from property where device_collection_id in (
		SELECT	dc.device_collection_id 
		  FROM	device_collection dc
				INNER JOIN device_collection_device dcd
		 			USING (device_collection_id)
		WHERE	dc.device_collection_type = 'per-device'
		  AND	dcd.device_id = in_Device_id
	);

	delete from device_collection_device where device_id = in_Device_id;
	delete from snmp_commstr where device_id = in_Device_id;

		
	IF _d.rack_location_id IS NOT NULL  THEN
		UPDATE device SET rack_location_id = NULL 
		WHERE device_id = in_Device_id;

		-- This should not be permitted based on constraints, but in case
		-- that constraint had to be disabled...
		SELECT	count(*)
		  INTO	tally
		  FROM	device
		 WHERE	rack_location_id = _d.RACK_LOCATION_ID;

		IF tally = 0 THEN
			DELETE FROM rack_location 
			WHERE rack_location_id = _d.RACK_LOCATION_ID;
		END IF;
	END IF;

	IF _d.chassis_location_id IS NOT NULL THEN
		RAISE EXCEPTION 'Retiring modules is not supported yet.';
	END IF;

	SELECT	manager_device_id
	INTO	_mgrid
	 FROM	device_management_controller
	WHERE	device_id = in_Device_id AND device_mgmt_control_type = 'bmc'
	LIMIT 1;

	IF _mgrid IS NOT NULL THEN
		DELETE FROM device_management_controller
		WHERE	device_id = in_Device_id AND device_mgmt_control_type = 'bmc'
			AND manager_device_id = _mgrid;

		PERFORM device_utils.retire_device( manager_device_id)
		  FROM	device_management_controller
		WHERE	device_id = in_Device_id AND device_mgmt_control_type = 'bmc';
	END IF;

	BEGIN
		PERFORM local_hooks.device_retire_late(in_Device_Id, false);
	EXCEPTION WHEN invalid_schema_name OR undefined_function THEN
		PERFORM 1;
	END;

	SELECT count(*)
	INTO tally
	FROM device_note
	WHERE device_id = in_Device_id;

	--
	-- If there is no notes or serial number its save to remove
	-- 
	IF tally = 0 AND _d.ASSET_ID is NULL THEN
		_purgedev := true;
	END IF;

	IF _purgedev THEN
		--
		-- If there is an fk violation, we just preserve the record but
		-- delete all the identifying characteristics
		--
		BEGIN
			DELETE FROM device where device_id = in_Device_Id;
			return false;
		EXCEPTION WHEN foreign_key_violation THEN
			PERFORM 1;
		END;
	END IF;

	UPDATE device SET 
		device_name =NULL,
		service_environment_id = (
			select service_environment_id from service_environment
			where service_environment_name = 'unallocated'),
		device_status = 'removed',
		voe_symbolic_track_id = NULL,
		is_monitored = 'N',
		should_fetch_config = 'N',
		description = NULL
	WHERE device_id = in_Device_id;

	return true;
END;
$function$
;

--
-- Process middle (non-trigger) schema netblock_utils
--
--
-- Process middle (non-trigger) schema netblock_manip
--
-- Changed function
SELECT schema_support.save_grants_for_replay('netblock_manip', 'allocate_netblock');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS netblock_manip.allocate_netblock ( parent_netblock_id integer, netmask_bits integer, address_type text, can_subnet boolean, allocation_method text, rnd_masklen_threshold integer, rnd_max_count integer, ip_address inet, description character varying, netblock_status character varying );
CREATE OR REPLACE FUNCTION netblock_manip.allocate_netblock(parent_netblock_id integer, netmask_bits integer DEFAULT NULL::integer, address_type text DEFAULT 'netblock'::text, can_subnet boolean DEFAULT true, allocation_method text DEFAULT NULL::text, rnd_masklen_threshold integer DEFAULT 110, rnd_max_count integer DEFAULT 1024, ip_address inet DEFAULT NULL::inet, description character varying DEFAULT NULL::character varying, netblock_status character varying DEFAULT 'Allocated'::character varying)
 RETURNS SETOF netblock
 LANGUAGE plpgsql
 SET search_path TO jazzhands
AS $function$
DECLARE
	netblock_rec	RECORD;
BEGIN
	RETURN QUERY 
		SELECT * into netblock_rec FROM netblock_manip.allocate_netblock(
		parent_netblock_list := ARRAY[parent_netblock_id],
		netmask_bits := netmask_bits,
		address_type := address_type,
		can_subnet := can_subnet,
		description := description,
		allocation_method := allocation_method,
		ip_address := ip_address,
		rnd_masklen_threshold := rnd_masklen_threshold,
		rnd_max_count := rnd_max_count,
		netblock_status := netblock_status
	);
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('netblock_manip', 'allocate_netblock');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS netblock_manip.allocate_netblock ( parent_netblock_list integer[], netmask_bits integer, address_type text, can_subnet boolean, allocation_method text, rnd_masklen_threshold integer, rnd_max_count integer, ip_address inet, description character varying, netblock_status character varying );
CREATE OR REPLACE FUNCTION netblock_manip.allocate_netblock(parent_netblock_list integer[], netmask_bits integer DEFAULT NULL::integer, address_type text DEFAULT 'netblock'::text, can_subnet boolean DEFAULT true, allocation_method text DEFAULT NULL::text, rnd_masklen_threshold integer DEFAULT 110, rnd_max_count integer DEFAULT 1024, ip_address inet DEFAULT NULL::inet, description character varying DEFAULT NULL::character varying, netblock_status character varying DEFAULT 'Allocated'::character varying)
 RETURNS SETOF netblock
 LANGUAGE plpgsql
 SET search_path TO jazzhands
AS $function$
DECLARE
	parent_rec		RECORD;
	netblock_rec	RECORD;
	inet_rec		RECORD;
	loopback_bits	integer;
	inet_family		integer;
	ip_addr			ALIAS FOR ip_address;
BEGIN
	IF parent_netblock_list IS NULL THEN
		RAISE 'parent_netblock_list must be specified'
		USING ERRCODE = 'null_value_not_allowed';
	END IF;

	IF address_type NOT IN ('netblock', 'single', 'loopback') THEN
		RAISE 'address_type must be one of netblock, single, or loopback'
		USING ERRCODE = 'invalid_parameter_value';
	END IF;

	IF netmask_bits IS NULL AND address_type = 'netblock' THEN
		RAISE EXCEPTION
			'You must specify a netmask when address_type is netblock'
			USING ERRCODE = 'invalid_parameter_value';
	END IF;

	IF ip_address IS NOT NULL THEN
		SELECT 
			array_agg(netblock_id)
		INTO
			parent_netblock_list
		FROM
			netblock n
		WHERE
			ip_addr <<= n.ip_address AND
			netblock_id = ANY(parent_netblock_list);

		IF parent_netblock_list IS NULL THEN
			RETURN;
		END IF;
	END IF;

	-- Lock the parent row, which should keep parallel processes from
	-- trying to obtain the same address

	FOR parent_rec IN SELECT * FROM jazzhands.netblock WHERE netblock_id = 
			ANY(allocate_netblock.parent_netblock_list) ORDER BY netblock_id
			FOR UPDATE LOOP

		IF parent_rec.is_single_address = 'Y' THEN
			RAISE EXCEPTION 'parent_netblock_id refers to a single_address netblock'
				USING ERRCODE = 'invalid_parameter_value';
		END IF;

		IF inet_family IS NULL THEN
			inet_family := family(parent_rec.ip_address);
		ELSIF inet_family != family(parent_rec.ip_address) 
				AND ip_address IS NULL THEN
			RAISE EXCEPTION 'Allocation may not mix IPv4 and IPv6 addresses'
			USING ERRCODE = 'JH10F';
		END IF;

		IF address_type = 'loopback' THEN
			loopback_bits := 
				CASE WHEN 
					family(parent_rec.ip_address) = 4 THEN 32 ELSE 128 END;

			IF parent_rec.can_subnet = 'N' THEN
				RAISE EXCEPTION 'parent subnet must have can_subnet set to Y'
					USING ERRCODE = 'JH10B';
			END IF;
		ELSIF address_type = 'single' THEN
			IF parent_rec.can_subnet = 'Y' THEN
				RAISE EXCEPTION
					'parent subnet for single address must have can_subnet set to N'
					USING ERRCODE = 'JH10B';
			END IF;
		ELSIF address_type = 'netblock' THEN
			IF parent_rec.can_subnet = 'N' THEN
				RAISE EXCEPTION 'parent subnet must have can_subnet set to Y'
					USING ERRCODE = 'JH10B';
			END IF;
		END IF;
	END LOOP;

 	IF NOT FOUND THEN
 		RETURN;
 	END IF;

	IF address_type = 'loopback' THEN
		-- If we're allocating a loopback address, then we need to create
		-- a new parent to hold the single loopback address

		SELECT * INTO inet_rec FROM netblock_utils.find_free_netblocks(
			parent_netblock_list := parent_netblock_list,
			netmask_bits := loopback_bits,
			single_address := false,
			allocation_method := allocation_method,
			desired_ip_address := ip_address,
			max_addresses := 1
			);

		IF NOT FOUND THEN
			RETURN;
		END IF;

		INSERT INTO jazzhands.netblock (
			ip_address,
			netblock_type,
			is_single_address,
			can_subnet,
			ip_universe_id,
			description,
			netblock_status
		) VALUES (
			inet_rec.ip_address,
			inet_rec.netblock_type,
			'N',
			'N',
			inet_rec.ip_universe_id,
			allocate_netblock.description,
			allocate_netblock.netblock_status
		) RETURNING * INTO parent_rec;

		INSERT INTO jazzhands.netblock (
			ip_address,
			netblock_type,
			is_single_address,
			can_subnet,
			ip_universe_id,
			description,
			netblock_status
		) VALUES (
			inet_rec.ip_address,
			parent_rec.netblock_type,
			'Y',
			'N',
			inet_rec.ip_universe_id,
			allocate_netblock.description,
			allocate_netblock.netblock_status
		) RETURNING * INTO netblock_rec;

		PERFORM dns_utils.add_domains_from_netblock(
			netblock_id := netblock_rec.netblock_id);

		RETURN NEXT netblock_rec;
		RETURN;
	END IF;

	IF address_type = 'single' THEN
		SELECT * INTO inet_rec FROM netblock_utils.find_free_netblocks(
			parent_netblock_list := parent_netblock_list,
			single_address := true,
			allocation_method := allocation_method,
			desired_ip_address := ip_address,
			rnd_masklen_threshold := rnd_masklen_threshold,
			rnd_max_count := rnd_max_count,
			max_addresses := 1
			);

		IF NOT FOUND THEN
			RETURN;
		END IF;

		RAISE DEBUG 'ip_address is %', inet_rec.ip_address;

		INSERT INTO jazzhands.netblock (
			ip_address,
			netblock_type,
			is_single_address,
			can_subnet,
			ip_universe_id,
			description,
			netblock_status
		) VALUES (
			inet_rec.ip_address,
			inet_rec.netblock_type,
			'Y',
			'N',
			inet_rec.ip_universe_id,
			allocate_netblock.description,
			allocate_netblock.netblock_status
		) RETURNING * INTO netblock_rec;

		RETURN NEXT netblock_rec;
		RETURN;
	END IF;
	IF address_type = 'netblock' THEN
		SELECT * INTO inet_rec FROM netblock_utils.find_free_netblocks(
			parent_netblock_list := parent_netblock_list,
			netmask_bits := netmask_bits,
			single_address := false,
			allocation_method := allocation_method,
			desired_ip_address := ip_address,
			max_addresses := 1);

		IF NOT FOUND THEN
			RETURN;
		END IF;

		INSERT INTO jazzhands.netblock (
			ip_address,
			netblock_type,
			is_single_address,
			can_subnet,
			ip_universe_id,
			description,
			netblock_status
		) VALUES (
			inet_rec.ip_address,
			inet_rec.netblock_type,
			'N',
			CASE WHEN can_subnet THEN 'Y' ELSE 'N' END,
			inet_rec.ip_universe_id,
			allocate_netblock.description,
			allocate_netblock.netblock_status
		) RETURNING * INTO netblock_rec;
		
		RAISE DEBUG 'Allocated netblock_id % for %',
			netblock_rec.netblock_id,
			netblock_rec.ip_address;

		PERFORM dns_utils.add_domains_from_netblock(
			netblock_id := netblock_rec.netblock_id);

		RETURN NEXT netblock_rec;
		RETURN;
	END IF;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('netblock_manip', 'create_network_range');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS netblock_manip.create_network_range ( start_ip_address inet, stop_ip_address inet, network_range_type character varying, parent_netblock_id integer, description character varying, allow_assigned boolean );
CREATE OR REPLACE FUNCTION netblock_manip.create_network_range(start_ip_address inet, stop_ip_address inet, network_range_type character varying, parent_netblock_id integer DEFAULT NULL::integer, description character varying DEFAULT NULL::character varying, allow_assigned boolean DEFAULT false)
 RETURNS network_range
 LANGUAGE plpgsql
 SET search_path TO jazzhands
AS $function$
DECLARE
	par_netblock	RECORD;
	start_netblock	RECORD;
	stop_netblock	RECORD;
	netrange		RECORD;
	nrtype			ALIAS FOR network_range_type;
	pnbid			ALIAS FOR parent_netblock_id;
BEGIN
	--
	-- If the network range already exists, then just return it
	--
	SELECT 
		nr.* INTO netrange
	FROM
		jazzhands.network_range nr JOIN
		jazzhands.netblock startnb ON (nr.start_netblock_id = 
			startnb.netblock_id) JOIN
		jazzhands.netblock stopnb ON (nr.stop_netblock_id = stopnb.netblock_id)
	WHERE
		nr.network_range_type = nrtype AND
		host(startnb.ip_address) = host(start_ip_address) AND
		host(stopnb.ip_address) = host(stop_ip_address) AND
		CASE WHEN pnbid IS NOT NULL THEN 
			(pnbid = nr.parent_netblock_id)
		ELSE
			true
		END;

	IF FOUND THEN
		RETURN netrange;
	END IF;

	--
	-- If any other network ranges exist that overlap this, then error
	--
	PERFORM 
		*
	FROM
		jazzhands.network_range nr JOIN
		jazzhands.netblock startnb ON 
			(nr.start_netblock_id = startnb.netblock_id) JOIN
		jazzhands.netblock stopnb ON (nr.stop_netblock_id = stopnb.netblock_id)
	WHERE
		nr.network_range_type = nrtype AND ((
			host(startnb.ip_address)::inet <= host(start_ip_address)::inet AND
			host(stopnb.ip_address)::inet >= host(start_ip_address)::inet
		) OR (
			host(startnb.ip_address)::inet <= host(stop_ip_address)::inet AND
			host(stopnb.ip_address)::inet >= host(stop_ip_address)::inet
		));

	IF FOUND THEN
		RAISE 'create_network_range: a network_range of type % already exists that has addresses between % and %',
			nrtype, start_ip_address, stop_ip_address
			USING ERRCODE = 'check_violation';
	END IF;

	IF parent_netblock_id IS NOT NULL THEN
		SELECT * INTO par_netblock FROM jazzhands.netblock WHERE 
			netblock_id = pnbid;
		IF NOT FOUND THEN
			RAISE 'create_network_range: parent_netblock_id % does not exist',
				parent_netblock_id USING ERRCODE = 'foreign_key_violation';
		END IF;
	ELSE
		SELECT * INTO par_netblock FROM jazzhands.netblock WHERE netblock_id = (
			SELECT 
				*
			FROM
				netblock_utils.find_best_parent_id(
					in_ipaddress := start_ip_address,
					in_is_single_address := 'Y'
				)
		);

		IF NOT FOUND THEN
			RAISE 'create_network_range: valid parent netblock for start_ip_address % does not exist',
				start_ip_address USING ERRCODE = 'check_violation';
		END IF;
	END IF;

	IF par_netblock.can_subnet != 'N' OR 
			par_netblock.is_single_address != 'N' THEN
		RAISE 'create_network_range: parent netblock % must not be subnettable or a single address',
			par_netblock.netblock_id USING ERRCODE = 'check_violation';
	END IF;

	IF NOT (start_ip_address <<= par_netblock.ip_address) THEN
		RAISE 'create_network_range: start_ip_address % is not contained by parent netblock % (%)',
			start_ip_address, par_netblock.ip_address,
			par_netblock.netblock_id USING ERRCODE = 'check_violation';
	END IF;

	IF NOT (stop_ip_address <<= par_netblock.ip_address) THEN
		RAISE 'create_network_range: stop_ip_address % is not contained by parent netblock % (%)',
			stop_ip_address, par_netblock.ip_address,
			par_netblock.netblock_id USING ERRCODE = 'check_violation';
	END IF;

	IF NOT (start_ip_address <= stop_ip_address) THEN
		RAISE 'create_network_range: start_ip_address % is not lower than stop_ip_address %',
			start_ip_address, stop_ip_address
			USING ERRCODE = 'check_violation';
	END IF;

	--
	-- Validate that there are not currently any addresses assigned in the
	-- range, unless allow_assigned is set
	--
	IF NOT allow_assigned THEN
		PERFORM 
			*
		FROM
			jazzhands.netblock n
		WHERE
			n.parent_netblock_id = par_netblock.netblock_id AND
			host(n.ip_address)::inet > host(start_ip_address)::inet AND
			host(n.ip_address)::inet < host(stop_ip_address)::inet;

		IF FOUND THEN
			RAISE 'create_network_range: netblocks are already present for parent netblock % betweeen % and %',
			par_netblock.netblock_id,
			start_ip_address, stop_ip_address
			USING ERRCODE = 'check_violation';
		END IF;
	END IF;

	--
	-- Ok, well, we should be able to insert things now
	--

	SELECT
		*
	FROM
		jazzhands.netblock n
	INTO
		start_netblock
	WHERE
		host(n.ip_address)::inet = start_ip_address AND
		n.netblock_type = 'network_range' AND
		n.can_subnet = 'N' AND
		n.is_single_address = 'Y' AND
		n.ip_universe_id = par_netblock.ip_universe_id;

	IF NOT FOUND THEN
		INSERT INTO netblock (
			ip_address,
			netblock_type,
			is_single_address,
			can_subnet,
			netblock_status,
			ip_universe_id
		) VALUES (
			host(start_ip_address)::inet,
			'network_range',
			'Y',
			'N',
			'Allocated',
			par_netblock.ip_universe_id
		) RETURNING * INTO start_netblock;
	END IF;

	SELECT
		*
	FROM
		jazzhands.netblock n
	INTO
		stop_netblock
	WHERE
		host(n.ip_address)::inet = stop_ip_address AND
		n.netblock_type = 'network_range' AND
		n.can_subnet = 'N' AND
		n.is_single_address = 'Y' AND
		n.ip_universe_id = par_netblock.ip_universe_id;

	IF NOT FOUND THEN
		INSERT INTO netblock (
			ip_address,
			netblock_type,
			is_single_address,
			can_subnet,
			netblock_status,
			ip_universe_id
		) VALUES (
			host(stop_ip_address)::inet,
			'network_range',
			'Y',
			'N',
			'Allocated',
			par_netblock.ip_universe_id
		) RETURNING * INTO stop_netblock;
	END IF;

	INSERT INTO network_range (
		network_range_type,
		description,
		parent_netblock_id,
		start_netblock_id,
		stop_netblock_id
	) VALUES (
		nrtype,
		description,
		par_netblock.netblock_id,
		start_netblock.netblock_id,
		stop_netblock.netblock_id
	) RETURNING * INTO netrange;

	RETURN netrange;

	RETURN NULL;
END;
$function$
;

--
-- Process middle (non-trigger) schema physical_address_utils
--
--
-- Process middle (non-trigger) schema component_utils
--
-- Changed function
SELECT schema_support.save_grants_for_replay('component_utils', 'insert_pci_component');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS component_utils.insert_pci_component ( pci_vendor_id integer, pci_device_id integer, pci_sub_vendor_id integer, pci_subsystem_id integer, pci_vendor_name text, pci_device_name text, pci_sub_vendor_name text, pci_sub_device_name text, component_function_list text[], slot_type text, serial_number text );
CREATE OR REPLACE FUNCTION component_utils.insert_pci_component(pci_vendor_id integer, pci_device_id integer, pci_sub_vendor_id integer DEFAULT NULL::integer, pci_subsystem_id integer DEFAULT NULL::integer, pci_vendor_name text DEFAULT NULL::text, pci_device_name text DEFAULT NULL::text, pci_sub_vendor_name text DEFAULT NULL::text, pci_sub_device_name text DEFAULT NULL::text, component_function_list text[] DEFAULT NULL::text[], slot_type text DEFAULT 'unknown'::text, serial_number text DEFAULT NULL::text)
 RETURNS component
 LANGUAGE plpgsql
 SET search_path TO jazzhands
AS $function$
DECLARE
	sn			ALIAS FOR serial_number;
	ctid		integer;
	comp_id		integer;
	sub_comp_id	integer;
	stid		integer;
	vendor_name	text;
	sub_vendor_name	text;
	model_name	text;
	c			RECORD;
BEGIN
	IF (pci_sub_vendor_id IS NULL AND pci_subsystem_id IS NOT NULL) OR
			(pci_sub_vendor_id IS NOT NULL AND pci_subsystem_id IS NULL) THEN
		RAISE EXCEPTION
			'pci_sub_vendor_id and pci_subsystem_id must be set together';
	END IF;

	--
	-- See if we have this component type in the database already
	--
	SELECT
		vid.component_type_id INTO ctid
	FROM
		component_property vid JOIN
		component_property did ON (
			vid.component_property_name = 'PCIVendorID' AND
			vid.component_property_type = 'PCI' AND
			did.component_property_name = 'PCIDeviceID' AND
			did.component_property_type = 'PCI' AND
			vid.component_type_id = did.component_type_id ) LEFT JOIN
		component_property svid ON (
			svid.component_property_name = 'PCISubsystemVendorID' AND
			svid.component_property_type = 'PCI' AND
			svid.component_type_id = did.component_type_id ) LEFT JOIN
		component_property sid ON (
			sid.component_property_name = 'PCISubsystemID' AND
			sid.component_property_type = 'PCI' AND
			sid.component_type_id = did.component_type_id )
	WHERE
		vid.property_value = pci_vendor_id::varchar AND
		did.property_value = pci_device_id::varchar AND
		svid.property_value IS NOT DISTINCT FROM pci_sub_vendor_id::varchar AND
		sid.property_value IS NOT DISTINCT FROM pci_subsystem_id::varchar;

	--
	-- The device type doesn't exist, so attempt to insert it
	--

	IF NOT FOUND THEN	
		IF pci_device_name IS NULL OR component_function_list IS NULL THEN
			RAISE EXCEPTION 'component_id not found and pci_device_name or component_function_list was not passed' USING ERRCODE = 'JH501';
		END IF;

		--
		-- Ensure that there's a company linkage for the PCI (subsystem)vendor
		--
		SELECT
			company_id, company_name INTO comp_id, vendor_name
		FROM
			property p JOIN
			company c USING (company_id)
		WHERE
			property_type = 'DeviceProvisioning' AND
			property_name = 'PCIVendorID' AND
			property_value = pci_vendor_id::text;
		
		IF NOT FOUND THEN
			IF pci_vendor_name IS NULL THEN
				RAISE EXCEPTION 'PCI vendor id mapping not found and pci_vendor_name was not passed' USING ERRCODE = 'JH501';
			END IF;
			SELECT company_id INTO comp_id FROM company
			WHERE company_name = pci_vendor_name;
		
			IF NOT FOUND THEN
				SELECT company_manip.add_company(
					_company_name := pci_vendor_name,
					_company_types := ARRAY['hardware provider'],
					 _description := 'PCI vendor auto-insert'
				) INTO comp_id;
			END IF;

			INSERT INTO property (
				property_name,
				property_type,
				property_value,
				company_id
			) VALUES (
				'PCIVendorID',
				'DeviceProvisioning',
				pci_vendor_id,
				comp_id
			);
			vendor_name := pci_vendor_name;
		END IF;

		SELECT
			company_id, company_name INTO sub_comp_id, sub_vendor_name
		FROM
			property JOIN
			company c USING (company_id)
		WHERE
			property_type = 'DeviceProvisioning' AND
			property_name = 'PCIVendorID' AND
			property_value = pci_sub_vendor_id::text;
		
		IF NOT FOUND THEN
			IF pci_sub_vendor_name IS NULL THEN
				RAISE EXCEPTION 'PCI subsystem vendor id mapping not found and pci_sub_vendor_name was not passed' USING ERRCODE = 'JH501';
			END IF;
			SELECT company_id INTO sub_comp_id FROM company
			WHERE company_name = pci_sub_vendor_name;
		
			IF NOT FOUND THEN
				SELECT company_manip.add_company(
					_company_name := pci_sub_vendor_name,
					_company_types := ARRAY['hardware provider'],
					 _description := 'PCI vendor auto-insert'
				) INTO comp_id;
			END IF;

			INSERT INTO property (
				property_name,
				property_type,
				property_value,
				company_id
			) VALUES (
				'PCIVendorID',
				'DeviceProvisioning',
				pci_sub_vendor_id,
				sub_comp_id
			);
			sub_vendor_name := pci_sub_vendor_name;
		END IF;

		--
		-- Fetch the slot type
		--

		SELECT 
			slot_type_id INTO stid
		FROM
			slot_type st
		WHERE
			st.slot_type = insert_pci_component.slot_type AND
			slot_function = 'PCI';

		IF NOT FOUND THEN
			RAISE EXCEPTION 'slot type % with function PCI not found adding component_type',
				insert_pci_component.slot_type
				USING ERRCODE = 'JH501';
		END IF;

		--
		-- Figure out the best name/description to insert this component with
		--
		IF pci_sub_device_name IS NOT NULL AND pci_sub_device_name != 'Device' THEN
			model_name = concat_ws(' ', 
				sub_vendor_name, pci_sub_device_name,
				'(' || vendor_name, pci_device_name || ')');
		ELSIF pci_sub_device_name = 'Device' THEN
			model_name = concat_ws(' ', 
				vendor_name, '(' || sub_vendor_name || ')', pci_device_name);
		ELSE
			model_name = concat_ws(' ', vendor_name, pci_device_name);
		END IF;
		INSERT INTO component_type (
			company_id,
			model,
			slot_type_id,
			asset_permitted,
			description
		) VALUES (
			CASE WHEN 
				sub_comp_id IS NULL OR
				pci_sub_device_name IS NULL OR
				pci_sub_device_name = 'Device'
			THEN
				comp_id
			ELSE
				sub_comp_id
			END,
			CASE WHEN
				pci_sub_device_name IS NULL OR
				pci_sub_device_name = 'Device'
			THEN
				pci_device_name
			ELSE
				pci_sub_device_name
			END,
			stid,
			'Y',
			model_name
		) RETURNING component_type_id INTO ctid;
		--
		-- Insert properties for the PCI vendor/device IDs
		--
		INSERT INTO component_property (
			component_property_name,
			component_property_type,
			component_type_id,
			property_value
		) VALUES 
			('PCIVendorID', 'PCI', ctid, pci_vendor_id),
			('PCIDeviceID', 'PCI', ctid, pci_device_id);
		
		IF (pci_subsystem_id IS NOT NULL) THEN
			INSERT INTO component_property (
				component_property_name,
				component_property_type,
				component_type_id,
				property_value
			) VALUES 
				('PCISubsystemVendorID', 'PCI', ctid, pci_sub_vendor_id),
				('PCISubsystemID', 'PCI', ctid, pci_subsystem_id);
		END IF;
		--
		-- Insert the component functions
		--

		INSERT INTO component_type_component_func (
			component_type_id,
			component_function
		) SELECT DISTINCT
			ctid,
			cf
		FROM
			unnest(array_append(component_function_list, 'PCI')) x(cf);
	END IF;


	--
	-- We have a component_type_id now, so look to see if this component
	-- serial number already exists
	--
	IF serial_number IS NOT NULL THEN
		SELECT 
			component.* INTO c
		FROM
			component JOIN
			asset a USING (component_id)
		WHERE
			component_type_id = ctid AND
			a.serial_number = sn;

		IF FOUND THEN
			RETURN c;
		END IF;
	END IF;

	INSERT INTO jazzhands.component (
		component_type_id
	) VALUES (
		ctid
	) RETURNING * INTO c;

	IF serial_number IS NOT NULL THEN
		INSERT INTO asset (
			component_id,
			serial_number,
			ownership_status
		) VALUES (
			c.component_id,
			serial_number,
			'unknown'
		);
	END IF;

	RETURN c;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION component_utils.fetch_component(component_type_id integer, serial_number text, no_create boolean DEFAULT false, ownership_status text DEFAULT 'unknown'::text)
 RETURNS component
 LANGUAGE plpgsql
 SET search_path TO jazzhands
AS $function$
DECLARE
	ctid		ALIAS FOR component_type_id;
	sn			ALIAS FOR serial_number;
	os			ALIAS FOR ownership_status;
	c			RECORD;
	cid			integer;
BEGIN
	cid := NULL;

	IF sn IS NOT NULL THEN
		SELECT 
			comp.* INTO c
		FROM
			component comp JOIN
			asset a USING (component_id)
		WHERE
			comp.component_type_id = ctid AND
			a.serial_number = sn;

		IF FOUND THEN
			return c;
		END IF;
	END IF;

	IF no_create THEN
		RETURN NULL;
	END IF;

	INSERT INTO jazzhands.component (
		component_type_id
	) VALUES (
		ctid
	) RETURNING * INTO c;

	IF serial_number IS NOT NULL THEN
		INSERT INTO asset (
			component_id,
			serial_number,
			ownership_status
		) VALUES (
			c.component_id,
			serial_number,
			os
		);
	END IF;

	RETURN c;
END;
$function$
;

--
-- Process middle (non-trigger) schema snapshot_manip
--
--
-- Process middle (non-trigger) schema lv_manip
--
--
-- Process middle (non-trigger) schema approval_utils
--
--
-- Process middle (non-trigger) schema account_collection_manip
--
--
-- Process middle (non-trigger) schema schema_support
--
-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'begin_maintenance');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.begin_maintenance ( shouldbesuper boolean );
CREATE OR REPLACE FUNCTION schema_support.begin_maintenance(shouldbesuper boolean DEFAULT true)
 RETURNS boolean
 LANGUAGE plpgsql
AS $function$
DECLARE
	issuper	boolean;
	_tally	integer;
BEGIN
	IF shouldbesuper THEN
		SELECT usesuper INTO issuper FROM pg_user where usename = current_user;
		IF issuper IS false THEN
			RAISE EXCEPTION 'User must be a super user.';
		END IF;
	END IF;
	-- Not sure how reliable this is.
	-- http://www.postgresql.org/docs/9.3/static/monitoring-stats.html
	SELECT count(*)
	  INTO _tally
	  FROM	pg_stat_activity
	 WHERE	pid = pg_backend_pid()
	   AND	query_start = xact_start;
	IF _tally > 0 THEN
		RAISE EXCEPTION 'Must run maintenance in a transaction.';
	END IF;
	RETURN true;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'build_audit_table');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.build_audit_table ( aud_schema character varying, tbl_schema character varying, table_name character varying, first_time boolean );
CREATE OR REPLACE FUNCTION schema_support.build_audit_table(aud_schema character varying, tbl_schema character varying, table_name character varying, first_time boolean DEFAULT true)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	keys	RECORD;
BEGIN
    IF first_time THEN
	EXECUTE 'CREATE SEQUENCE ' || quote_ident(aud_schema) || '.'
	    || quote_ident(table_name || '_seq');
    END IF;

    EXECUTE 'CREATE TABLE ' || quote_ident(aud_schema) || '.'
	|| quote_ident(table_name) || ' AS '
	|| 'SELECT *, NULL::char(3) as "aud#action", now() as "aud#timestamp", '
	|| 'clock_timestamp() as "aud#realtime", '
	|| 'txid_current() as "aud#txid", '
	|| 'NULL::varchar(255) AS "aud#user", NULL::integer AS "aud#seq" '
	|| 'FROM ' || quote_ident(tbl_schema) || '.' || quote_ident(table_name)
	|| ' LIMIT 0';

    EXECUTE 'ALTER TABLE ' || quote_ident(aud_schema) || '.'
	|| quote_ident(table_name)
	|| $$ ALTER COLUMN "aud#seq" SET NOT NULL, $$
	|| $$ ALTER COLUMN "aud#seq" SET DEFAULT nextval('$$
	|| quote_ident(aud_schema) || '.' || quote_ident(table_name || '_seq')
	|| $$')$$;

    EXECUTE 'CREATE INDEX '
	|| quote_ident( table_name || '_aud#timestamp_idx')
	|| ' ON ' || quote_ident(aud_schema) || '.'
	|| quote_ident(table_name) || '("aud#timestamp")';

	EXECUTE 'ALTER TABLE ' || quote_ident(aud_schema) || '.'
		|| quote_ident( table_name )
		|| ' ADD PRIMARY KEY ("aud#seq")';

	-- one day, I will want to construct the list of columns by hand rather
	-- than use pg_get_constraintdef.  watch me...
	FOR keys IN
		SELECT con.conname, c2.relname as index_name,
			pg_catalog.pg_get_constraintdef(con.oid, true) as condef,
				regexp_replace(
			pg_catalog.pg_get_constraintdef(con.oid, true),
					'^.*(\([^\)]+\)).*$', '\1') as cols,
			con.condeferrable,
			con.condeferred
		FROM pg_catalog.pg_class c
			INNER JOIN pg_namespace n
				ON relnamespace = n.oid
			INNER JOIN pg_catalog.pg_index i
				ON c.oid = i.indrelid
			INNER JOIN pg_catalog.pg_class c2
				ON i.indexrelid = c2.oid
			INNER JOIN pg_catalog.pg_constraint con ON
				(con.conrelid = i.indrelid
				AND con.conindid = i.indexrelid )
		WHERE c.relname =  table_name
		AND     n.nspname = tbl_schema
		AND con.contype in ('p', 'u')
	LOOP
		EXECUTE 'CREATE INDEX '
			|| 'aud_' || quote_ident( table_name || '_' || keys.conname)
			|| ' ON ' || quote_ident(aud_schema) || '.'
			|| quote_ident(table_name) || keys.cols;
	END LOOP;

    IF first_time THEN
		PERFORM schema_support.rebuild_audit_trigger
			( aud_schema, tbl_schema, table_name );
    END IF;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'build_audit_tables');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.build_audit_tables ( aud_schema character varying, tbl_schema character varying );
CREATE OR REPLACE FUNCTION schema_support.build_audit_tables(aud_schema character varying, tbl_schema character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
     table_list RECORD;
BEGIN
    FOR table_list IN
	SELECT table_name FROM information_schema.tables
	WHERE table_type = 'BASE TABLE' AND table_schema = tbl_schema
	AND NOT (
	    table_name IN (
		SELECT table_name FROM information_schema.tables
		WHERE table_schema = aud_schema
	    )
	)
	ORDER BY table_name
    LOOP
	PERFORM schema_support.build_audit_table
	    ( aud_schema, tbl_schema, table_list.table_name );
    END LOOP;

    PERFORM schema_support.rebuild_audit_triggers(aud_schema, tbl_schema);
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'get_common_columns');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.get_common_columns ( _schema text, _table1 text, _table2 text );
CREATE OR REPLACE FUNCTION schema_support.get_common_columns(_schema text, _table1 text, _table2 text)
 RETURNS text[]
 LANGUAGE plpgsql
AS $function$
DECLARE
	_q			text;
    cols        text[];
BEGIN
    _q := 'WITH cols AS (
        SELECT  n.nspname as schema, c.relname as relation, a.attname as colname,
		a.attnum
            FROM    pg_catalog.pg_attribute a
                INNER JOIN pg_catalog.pg_class c
                    on a.attrelid = c.oid
                INNER JOIN pg_catalog.pg_namespace n
                    on c.relnamespace = n.oid
            WHERE   a.attnum > 0
            AND   NOT a.attisdropped
            ORDER BY a.attnum
       ) SELECT array_agg(colname ORDER BY o.attnum) as cols
        FROM cols  o
            INNER JOIN cols n USING (schema, colname)
		WHERE
			o.schema = $1
		and o.relation = $2
		and n.relation =$3
	';
	EXECUTE _q INTO cols USING _schema, _table1, _table2;
	RETURN cols;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'rebuild_audit_trigger');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.rebuild_audit_trigger ( aud_schema character varying, tbl_schema character varying, table_name character varying );
CREATE OR REPLACE FUNCTION schema_support.rebuild_audit_trigger(aud_schema character varying, tbl_schema character varying, table_name character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
BEGIN
    EXECUTE 'CREATE OR REPLACE FUNCTION ' || quote_ident(tbl_schema)
	|| '.' || quote_ident('perform_audit_' || table_name)
	|| $ZZ$() RETURNS TRIGGER AS $TQ$
	    DECLARE
		appuser VARCHAR;
	    BEGIN
		BEGIN
		    appuser := session_user
			|| '/' || current_setting('jazzhands.appuser');
		EXCEPTION WHEN OTHERS THEN
		    appuser := session_user;
		END;

    		appuser = substr(appuser, 1, 255);

		IF TG_OP = 'DELETE' THEN
		    INSERT INTO $ZZ$ || quote_ident(aud_schema)
			|| '.' || quote_ident(table_name) || $ZZ$
		    VALUES ( OLD.*, 'DEL', now(),
			clock_timestamp(), txid_current(), appuser );
		    RETURN OLD;
		ELSIF TG_OP = 'UPDATE' THEN
		    INSERT INTO $ZZ$ || quote_ident(aud_schema)
			|| '.' || quote_ident(table_name) || $ZZ$
		    VALUES ( NEW.*, 'UPD', now(),
			clock_timestamp(), txid_current(), appuser );
		    RETURN NEW;
		ELSIF TG_OP = 'INSERT' THEN
		    INSERT INTO $ZZ$ || quote_ident(aud_schema)
			|| '.' || quote_ident(table_name) || $ZZ$
		    VALUES ( NEW.*, 'INS', now(),
			clock_timestamp(), txid_current(), appuser );
		    RETURN NEW;
		END IF;
		RETURN NULL;
	    END;
	$TQ$ LANGUAGE plpgsql SECURITY DEFINER
    $ZZ$;

    EXECUTE 'DROP TRIGGER IF EXISTS ' || quote_ident('trigger_audit_'
	|| table_name) || ' ON ' || quote_ident(tbl_schema) || '.'
	|| quote_ident(table_name);

    EXECUTE 'CREATE TRIGGER ' || quote_ident('trigger_audit_' || table_name)
	|| ' AFTER INSERT OR UPDATE OR DELETE ON ' || quote_ident(tbl_schema)
	|| '.' || quote_ident(table_name) || ' FOR EACH ROW EXECUTE PROCEDURE '
	|| quote_ident(tbl_schema) || '.' || quote_ident('perform_audit_'
	|| table_name) || '()';
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'rebuild_stamp_triggers');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.rebuild_stamp_triggers ( tbl_schema character varying );
CREATE OR REPLACE FUNCTION schema_support.rebuild_stamp_triggers(tbl_schema character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
BEGIN
    DECLARE
	tab RECORD;
    BEGIN
	FOR tab IN
	    SELECT table_name FROM information_schema.tables
	    WHERE table_schema = tbl_schema AND table_type = 'BASE TABLE'
	    AND table_name NOT LIKE 'aud$%'
	LOOP
	    PERFORM schema_support.rebuild_stamp_trigger
		(tbl_schema, tab.table_name);
	END LOOP;
    END;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'relation_diff');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.relation_diff ( schema text, old_rel text, new_rel text, key_relation text, prikeys text[], raise_exception boolean );
CREATE OR REPLACE FUNCTION schema_support.relation_diff(schema text, old_rel text, new_rel text, key_relation text DEFAULT NULL::text, prikeys text[] DEFAULT NULL::text[], raise_exception boolean DEFAULT true)
 RETURNS boolean
 LANGUAGE plpgsql
AS $function$
DECLARE
	_or	RECORD;
	_nr	RECORD;
	_t1	integer;
	_t2	integer;
	_cols TEXT[];
	_q TEXT;
	_f TEXT;
	_c RECORD;
	_w TEXT[];
	_ctl TEXT[];
	_rv	boolean;
BEGIN
	-- do a simple row count
	EXECUTE 'SELECT count(*) FROM ' || schema || '."' || old_rel || '"' INTO _t1;
	EXECUTE 'SELECT count(*) FROM ' || schema || '."' || new_rel || '"' INTO _t2;

	_rv := true;

	IF _t1 IS NULL THEN
		RAISE NOTICE 'table %.% does not seem to exist', schema, old_rel;
		_rv := false;
	END IF;
	IF _t2 IS NULL THEN
		RAISE NOTICE 'table %.% does not seem to exist', schema, new_rel;
		_rv := false;
	END IF;

	IF _t1 != _t2 THEN
		RAISE NOTICE 'table % has % rows; table % has % rows', old_rel, _t1, new_rel, _t2;
		_rv := false;
	END IF;

	IF NOT _rv THEN
		IF raise_exception THEN
			RAISE EXCEPTION 'Relations do not match';
		END IF;
		RETURN false;
	END IF;

	IF prikeys IS NULL THEN
		-- read into prikeys the primary key for the table
		IF key_relation IS NULL THEN
			key_relation := old_rel;
		END IF;
		prikeys := schema_support.get_pk_columns(schema, key_relation);
	END IF;

	-- read into _cols the column list in common between old_rel and new_rel
	_cols := schema_support.get_common_columns(schema, old_rel, new_rel);

	FOREACH _f IN ARRAY _cols
	LOOP
		SELECT array_append(_ctl,
			quote_ident(_f) || '::text') INTO _ctl;
	END LOOP;

	_cols := _ctl;

	_q := 'SELECT '|| array_to_string(_cols,',') ||' FROM ' || quote_ident(schema) || '.' ||
		quote_ident(old_rel);

	FOR _or IN EXECUTE _q
	LOOP
		_w = NULL;
		FOREACH _f IN ARRAY prikeys
		LOOP
			FOR _c IN SELECT * FROM json_each_text( row_to_json(_or) )
			LOOP
				IF _c.key = _f THEN
					SELECT array_append(_w,
						quote_ident(_f) || '::text = ' || quote_literal(_c.value))
					INTO _w;
				END IF;
			END LOOP;
		END LOOP;
		_q := 'SELECT ' || array_to_string(_cols,',') ||
			' FROM ' || quote_ident(schema) || '.' ||
			quote_ident(new_rel) || ' WHERE ' ||
			array_to_string(_w, ' AND ' );
		EXECUTE _q INTO _nr;

		IF _or != _nr THEN
			RAISE NOTICE 'mismatched row:';
			RAISE NOTICE 'OLD: %', row_to_json(_or);
			RAISE NOTICE 'NEW: %', row_to_json(_nr);
			_rv := false;
		END IF;

	END LOOP;

	IF NOT _rv AND raise_exception THEN
		RAISE EXCEPTION 'Relations do not match';
	END IF;
	return _rv;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'replay_object_recreates');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.replay_object_recreates ( beverbose boolean );
CREATE OR REPLACE FUNCTION schema_support.replay_object_recreates(beverbose boolean DEFAULT false)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_r		RECORD;
	_tally	integer;
BEGIN
	SELECT	count(*)
	  INTO	_tally
	  FROM	pg_catalog.pg_class
	 WHERE	relname = '__recreate'
	   AND	relpersistence = 't';

	IF _tally > 0 THEN
		FOR _r in SELECT * from __recreate ORDER BY id DESC FOR UPDATE
		LOOP
			IF beverbose THEN
				RAISE NOTICE 'Regrant: %.%', _r.schema, _r.object;
			END IF;
			EXECUTE _r.ddl;
			IF _r.owner is not NULL THEN
				IF _r.type = 'view' THEN
					EXECUTE 'ALTER VIEW ' || _r.schema || '.' || _r.object ||
						' OWNER TO ' || _r.owner || ';';
				ELSIF _r.type = 'function' THEN
					EXECUTE 'ALTER FUNCTION ' || _r.schema || '.' || _r.object ||
						'(' || _r.idargs || ') OWNER TO ' || _r.owner || ';';
				ELSE
					RAISE EXCEPTION 'Unable to restore grant for %', _r;
				END IF;
			END IF;
			DELETE from __recreate where id = _r.id;
		END LOOP;

		SELECT count(*) INTO _tally from __recreate;
		IF _tally > 0 THEN
			RAISE EXCEPTION '% objects still exist for recreating after a complete loop', _tally;
		ELSE
			DROP TABLE __recreate;
		END IF;
	ELSE
		IF beverbose THEN
			RAISE NOTICE '**** WARNING: replay_object_recreates did NOT have anything to regrant!';
		END IF;
	END IF;

END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'replay_saved_grants');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.replay_saved_grants ( beverbose boolean );
CREATE OR REPLACE FUNCTION schema_support.replay_saved_grants(beverbose boolean DEFAULT false)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_r		RECORD;
	_tally	integer;
BEGIN
	 SELECT  count(*)
      INTO  _tally
      FROM  pg_catalog.pg_class
     WHERE  relname = '__regrants'
       AND  relpersistence = 't';

	IF _tally > 0 THEN
	    FOR _r in SELECT * from __regrants FOR UPDATE
	    LOOP
		    IF beverbose THEN
			    RAISE NOTICE 'Regrant Executing: %', _r.regrant;
		    END IF;
		    EXECUTE _r.regrant;
		    DELETE from __regrants where id = _r.id;
	    END LOOP;

	    SELECT count(*) INTO _tally from __regrants;
	    IF _tally > 0 THEN
		    RAISE EXCEPTION 'Grant extractions were run while replaying grants - %.', _tally;
	    ELSE
		    DROP TABLE __regrants;
	    END IF;
	ELSE
		IF beverbose THEN
			RAISE NOTICE '**** WARNING: replay_saved_grants did NOT have anything to regrant!';
		END IF;
	END IF;

END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'retrieve_functions');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.retrieve_functions ( schema character varying, object character varying, dropit boolean );
CREATE OR REPLACE FUNCTION schema_support.retrieve_functions(schema character varying, object character varying, dropit boolean DEFAULT false)
 RETURNS text[]
 LANGUAGE plpgsql
AS $function$
DECLARE
	_r		RECORD;
	_fn		TEXT;
	_cmd	TEXT;
	_rv		TEXT[];
BEGIN
	FOR _r IN SELECT n.nspname, p.proname,
				coalesce(u.usename, 'public') as owner,
				pg_get_functiondef(p.oid) as funcdef,
				pg_get_function_identity_arguments(p.oid) as idargs
		FROM    pg_catalog.pg_proc  p
				INNER JOIN pg_catalog.pg_namespace n on n.oid = p.pronamespace
				INNER JOIN pg_catalog.pg_language l on l.oid = p.prolang
				INNER JOIN pg_catalog.pg_user u on u.usesysid = p.proowner
		WHERE   n.nspname = schema
		  AND	p.proname = object
	LOOP
		_fn = _r.nspname || '.' || _r.proname || '(' || _r.idargs || ')';
		_rv = _rv || _fn;

		IF dropit  THEN
			_cmd = 'DROP FUNCTION ' || _fn || ';';
			EXECUTE _cmd;
		END IF;
	END LOOP;
	RETURN _rv;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'save_constraint_for_replay');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.save_constraint_for_replay ( schema character varying, object character varying, dropit boolean );
CREATE OR REPLACE FUNCTION schema_support.save_constraint_for_replay(schema character varying, object character varying, dropit boolean DEFAULT true)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_r		RECORD;
	_cmd	TEXT;
	_ddl	TEXT;
BEGIN
	PERFORM schema_support.prepare_for_object_replay();

	FOR _r in 	SELECT n.nspname, c.relname, con.conname,
				pg_get_constraintdef(con.oid, true) as def
		FROM pg_constraint con
			INNER JOIN pg_class c on (c.relnamespace, c.oid) =
				(con.connamespace, con.conrelid)
			INNER JOIN pg_namespace n on n.oid = c.relnamespace
		WHERE con.confrelid in (
			select c.oid
			from pg_class c
				inner join pg_namespace n on n.oid = c.relnamespace
			WHERE c.relname = object
			AND n.nspname = schema
		) AND n.nspname != schema
	LOOP
		_ddl := 'ALTER TABLE ' || _r.nspname || '.' || _r.relname ||
			' ADD CONSTRAINT ' || _r.conname || ' ' || _r.def;
		IF _ddl is NULL THEN
			RAISE EXCEPTION 'Unable to define constraint for %', _r;
		END IF;
		INSERT INTO __recreate (schema, object, type, ddl )
			VALUES (
				_r.nspname, _r.relname, 'constraint', _ddl
			);
		IF dropit  THEN
			_cmd = 'ALTER TABLE ' || _r.nspname || '.' || _r.relname ||
				' DROP CONSTRAINT ' || _r.conname || ';';
			EXECUTE _cmd;
		END IF;
	END LOOP;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'save_dependent_objects_for_replay');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.save_dependent_objects_for_replay ( schema character varying, object character varying, dropit boolean, doobjectdeps boolean );
CREATE OR REPLACE FUNCTION schema_support.save_dependent_objects_for_replay(schema character varying, object character varying, dropit boolean DEFAULT true, doobjectdeps boolean DEFAULT false)
 RETURNS void
 LANGUAGE plpgsql
 SET search_path TO schema_support
AS $function$

DECLARE
	_r		RECORD;
	_cmd	TEXT;
	_ddl	TEXT;
BEGIN
	RAISE DEBUG 'processing %.%', schema, object;
	-- process stored procedures
	FOR _r in SELECT  distinct np.nspname::text, dependent.proname::text
		FROM   pg_depend dep
			INNER join pg_type dependee on dependee.oid = dep.refobjid
			INNER join pg_namespace n on n.oid = dependee.typnamespace
			INNER join pg_proc dependent on dependent.oid = dep.objid
			INNER join pg_namespace np on np.oid = dependent.pronamespace
			WHERE   dependee.typname = object
			  AND	  n.nspname = schema
	LOOP
		-- RAISE NOTICE '1 dealing with  %.%', _r.nspname, _r.proname;
		PERFORM schema_support.save_constraint_for_replay(_r.nspname, _r.proname, dropit);
		PERFORM schema_support.save_dependent_objects_for_replay(_r.nspname, _r.proname, dropit);
		PERFORM schema_support.save_function_for_replay(_r.nspname, _r.proname, dropit);
	END LOOP;

	-- save any triggers on the view
	FOR _r in SELECT distinct n.nspname::text, dependee.relname::text, dependee.relkind
		FROM pg_depend
		JOIN pg_rewrite ON pg_depend.objid = pg_rewrite.oid
		JOIN pg_class as dependee ON pg_rewrite.ev_class = dependee.oid
		JOIN pg_class as dependent ON pg_depend.refobjid = dependent.oid
		JOIN pg_namespace n on n.oid = dependee.relnamespace
		JOIN pg_namespace sn on sn.oid = dependent.relnamespace
		JOIN pg_attribute ON pg_depend.refobjid = pg_attribute.attrelid
   			AND pg_depend.refobjsubid = pg_attribute.attnum
		WHERE dependent.relname = object
  		AND sn.nspname = schema
	LOOP
		IF _r.relkind = 'v' THEN
			-- RAISE NOTICE '2 dealing with  %.%', _r.nspname, _r.relname;
			PERFORM * FROM save_dependent_objects_for_replay(_r.nspname, _r.relname, dropit);
			PERFORM schema_support.save_view_for_replay(_r.nspname, _r.relname, dropit);
		END IF;
	END LOOP;
	IF doobjectdeps THEN
		PERFORM schema_support.save_trigger_for_replay(schema, object, dropit);
		PERFORM schema_support.save_constraint_for_replay('jazzhands', 'table');
	END IF;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'save_function_for_replay');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.save_function_for_replay ( schema character varying, object character varying, dropit boolean );
CREATE OR REPLACE FUNCTION schema_support.save_function_for_replay(schema character varying, object character varying, dropit boolean DEFAULT true)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_r		RECORD;
	_cmd	TEXT;
BEGIN
	PERFORM schema_support.prepare_for_object_replay();

	-- implicitly save regrants
	PERFORM schema_support.save_grants_for_replay(schema, object);
	FOR _r IN SELECT n.nspname, p.proname,
				coalesce(u.usename, 'public') as owner,
				pg_get_functiondef(p.oid) as funcdef,
				pg_get_function_identity_arguments(p.oid) as idargs
		FROM    pg_catalog.pg_proc  p
				INNER JOIN pg_catalog.pg_namespace n on n.oid = p.pronamespace
				INNER JOIN pg_catalog.pg_language l on l.oid = p.prolang
				INNER JOIN pg_catalog.pg_user u on u.usesysid = p.proowner
		WHERE   n.nspname = schema
		  AND	p.proname = object
	LOOP
		INSERT INTO __recreate (schema, object, type, owner, ddl, idargs )
		VALUES (
			_r.nspname, _r.proname, 'function', _r.owner, _r.funcdef, _r.idargs
		);
		IF dropit  THEN
			_cmd = 'DROP FUNCTION ' || _r.nspname || '.' ||
				_r.proname || '(' || _r.idargs || ');';
			EXECUTE _cmd;
		END IF;

	END LOOP;

END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'save_grants_for_replay_functions');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.save_grants_for_replay_functions ( schema character varying, object character varying, newname character varying );
CREATE OR REPLACE FUNCTION schema_support.save_grants_for_replay_functions(schema character varying, object character varying, newname character varying DEFAULT NULL::character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_schema		varchar;
	_object		varchar;
	_procs		RECORD;
	_perm		RECORD;
	_grant		varchar;
	_role		varchar;
	_fullgrant		varchar;
BEGIN
	_schema := schema;
	_object := object;
	if newname IS NULL THEN
		newname := _object;
	END IF;
	PERFORM schema_support.prepare_for_grant_replay();
	FOR _procs IN SELECT  n.nspname as schema, p.proname,
			pg_get_function_identity_arguments(p.oid) as args,
			proacl as privs
		FROM    pg_catalog.pg_proc  p
				inner join pg_catalog.pg_namespace n on n.oid = p.pronamespace
		WHERE   n.nspname = _schema
	 	 AND    p.proname = _object
	LOOP
		-- NOTE:  We lose who granted it.  Oh Well.
		FOR _perm IN SELECT * FROM pg_catalog.aclexplode(acl := _procs.privs)
		LOOP
			--  grantor | grantee | privilege_type | is_grantable
			IF _perm.is_grantable THEN
				_grant = ' WITH GRANT OPTION';
			ELSE
				_grant = '';
			END IF;
			IF _perm.grantee = 0 THEN
				_role := 'PUBLIC';
			ELSE
				_role := pg_get_userbyid(_perm.grantee);
			END IF;
			_fullgrant := 'GRANT ' ||
				_perm.privilege_type || ' on FUNCTION ' ||
				_schema || '.' ||
				newname || '(' || _procs.args || ')  to ' ||
				_role || _grant;
			-- RAISE DEBUG 'inserting % for %', _fullgrant, _perm;
			INSERT INTO __regrants (schema, object, newname, regrant) values (schema,object, newname, _fullgrant );
		END LOOP;
	END LOOP;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'save_grants_for_replay_relations');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.save_grants_for_replay_relations ( schema character varying, object character varying, newname character varying );
CREATE OR REPLACE FUNCTION schema_support.save_grants_for_replay_relations(schema character varying, object character varying, newname character varying DEFAULT NULL::character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_schema		varchar;
	_object	varchar;
	_tabs		RECORD;
	_perm		RECORD;
	_grant		varchar;
	_fullgrant		varchar;
	_role		varchar;
BEGIN
	_schema := schema;
	_object := object;
	if newname IS NULL THEN
		newname := _object;
	END IF;
	PERFORM schema_support.prepare_for_grant_replay();

	-- Handle table wide grants
	FOR _tabs IN SELECT  n.nspname as schema,
			c.relname as name,
			CASE c.relkind
				WHEN 'r' THEN 'table'
				WHEN 'v' THEN 'view'
				WHEN 'S' THEN 'sequence'
				WHEN 'f' THEN 'foreign table'
				END as "Type",
			c.relacl as privs
		FROM    pg_catalog.pg_class c
			INNER JOIN pg_catalog.pg_namespace n
				ON n.oid = c.relnamespace
		WHERE c.relkind IN ('r', 'v', 'S', 'f')
		  AND c.relname = _object
		  AND n.nspname = _schema
		ORDER BY 1, 2
	LOOP
		-- NOTE:  We lose who granted it.  Oh Well.
		FOR _perm IN SELECT * FROM pg_catalog.aclexplode(acl := _tabs.privs)
		LOOP
			--  grantor | grantee | privilege_type | is_grantable
			IF _perm.is_grantable THEN
				_grant = ' WITH GRANT OPTION';
			ELSE
				_grant = '';
			END IF;
			IF _perm.grantee = 0 THEN
				_role := 'PUBLIC';
			ELSE
				_role := pg_get_userbyid(_perm.grantee);
			END IF;
			_fullgrant := 'GRANT ' ||
				_perm.privilege_type || ' on ' ||
				_schema || '.' ||
				newname || ' to ' ||
				_role || _grant;
			IF _fullgrant IS NULL THEN
				RAISE EXCEPTION 'built up grant for %.% (%) is NULL',
					schema, object, newname;
	    END IF;
			INSERT INTO __regrants (schema, object, newname, regrant) values (schema,object, newname, _fullgrant );
		END LOOP;
	END LOOP;

	-- Handle column specific wide grants
	FOR _tabs IN SELECT  n.nspname as schema,
			c.relname as name,
			CASE c.relkind
				WHEN 'r' THEN 'table'
				WHEN 'v' THEN 'view'
				WHEN 'S' THEN 'sequence'
				WHEN 'f' THEN 'foreign table'
				END as "Type",
			a.attname as col,
			a.attacl as privs
		FROM    pg_catalog.pg_class c
			INNER JOIN pg_catalog.pg_namespace n
				ON n.oid = c.relnamespace
			INNER JOIN pg_attribute a
                ON a.attrelid = c.oid
		WHERE c.relkind IN ('r', 'v', 'S', 'f')
		  AND a.attacl IS NOT NULL
		  AND c.relname = _object
		  AND n.nspname = _schema
		ORDER BY 1, 2
	LOOP
		-- NOTE:  We lose who granted it.  Oh Well.
		FOR _perm IN SELECT * FROM pg_catalog.aclexplode(acl := _tabs.privs)
		LOOP
			--  grantor | grantee | privilege_type | is_grantable
			IF _perm.is_grantable THEN
				_grant = ' WITH GRANT OPTION';
			ELSE
				_grant = '';
			END IF;
			IF _perm.grantee = 0 THEN
				_role := 'PUBLIC';
			ELSE
				_role := pg_get_userbyid(_perm.grantee);
			END IF;
			_fullgrant := 'GRANT ' ||
				_perm.privilege_type || '(' || _tabs.col || ')'
				' on ' ||
				_schema || '.' ||
				newname || ' to ' ||
				_role || _grant;
			IF _fullgrant IS NULL THEN
				RAISE EXCEPTION 'built up grant for %.% (%) is NULL',
					schema, object, newname;
	    END IF;
			INSERT INTO __regrants (schema, object, newname, regrant) values (schema,object, newname, _fullgrant );
		END LOOP;
	END LOOP;

END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'save_trigger_for_replay');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.save_trigger_for_replay ( schema character varying, object character varying, dropit boolean );
CREATE OR REPLACE FUNCTION schema_support.save_trigger_for_replay(schema character varying, object character varying, dropit boolean DEFAULT true)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_r		RECORD;
	_cmd	TEXT;
BEGIN
	PERFORM schema_support.prepare_for_object_replay();

	FOR _r in
		SELECT n.nspname, c.relname, trg.tgname,
				pg_get_triggerdef(trg.oid, true) as def
		FROM pg_trigger trg
			INNER JOIN pg_class c on trg.tgrelid =  c.oid
			INNER JOIN pg_namespace n on n.oid = c.relnamespace
		WHERE n.nspname = schema and c.relname = object
	LOOP
		INSERT INTO __recreate (schema, object, type, ddl )
			VALUES (
				_r.nspname, _r.relname, 'trigger', _r.def
			);
		IF dropit  THEN
			_cmd = 'DROP TRIGGER ' || _r.tgname || ' ON ' ||
				_r.nspname || '.' || _r.relname || ';';
			EXECUTE _cmd;
		END IF;
	END LOOP;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'undo_audit_row');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.undo_audit_row ( in_table text, in_audit_schema text, in_schema text, in_start_time timestamp without time zone, in_end_time timestamp without time zone, in_aud_user text, in_audit_ids integer[] );
CREATE OR REPLACE FUNCTION schema_support.undo_audit_row(in_table text, in_audit_schema text DEFAULT 'audit'::text, in_schema text DEFAULT 'jazzhands'::text, in_start_time timestamp without time zone DEFAULT NULL::timestamp without time zone, in_end_time timestamp without time zone DEFAULT NULL::timestamp without time zone, in_aud_user text DEFAULT NULL::text, in_audit_ids integer[] DEFAULT NULL::integer[])
 RETURNS integer
 LANGUAGE plpgsql
AS $function$
DECLARE
	tally	integer;
	pks		text[];
	cols	text[];
	q		text;
	val		text;
	x		text;
	_whcl	text;
	_eq		text;
	setstr	text;
	_r		record;
	_c		record;
	_br		record;
	_vals	text[];
BEGIN
	tally := 0;
	pks := schema_support.get_pk_columns(in_schema, in_table);
	cols := schema_support.get_columns(in_schema, in_table);
	q = '';
	IF in_start_time is not NULL THEN
		IF q = '' THEN
			q := q || 'WHERE ';
		ELSE
			q := q || 'AND ';
		END IF;
		q := q || quote_ident('aud#timestamp') || ' >= ' || quote_literal(in_start_time);
	END IF;
	IF in_end_time is not NULL THEN
		IF q = '' THEN
			q := q || 'WHERE ';
		ELSE
			q := q || 'AND ';
		END IF;
		q := q || quote_ident('aud#timestamp') || ' <= ' || quote_literal(in_end_time);
	END IF;
	IF in_aud_user is not NULL THEN
		IF q = '' THEN
			q := q || 'WHERE ';
		ELSE
			q := q || 'AND ';
		END IF;
		q := q || quote_ident('aud#user') || ' = ' || quote_literal(in_aud_user);
	END IF;
	IF in_audit_ids is not NULL THEN
		IF q = '' THEN
			q := q || 'WHERE ';
		ELSE
			q := q || 'AND ';
		END IF;
		q := q || quote_ident('aud#seq') || ' IN ( ' ||
			array_to_string(in_audit_ids, ',') || ')';
	END IF;

	-- Iterate over all the rows that need to be replayed
	q := 'SELECT * from ' || quote_ident(in_audit_schema) || '.' ||
			quote_ident(in_table) || ' ' || q || ' ORDER BY "aud#seq" desc';
	FOR _r IN EXECUTE q
	LOOP
		IF _r."aud#action" = 'DEL' THEN
			-- Build up a list of rows that need to be inserted
			_vals = NULL;
			FOR _c IN SELECT * FROM json_each_text( row_to_json(_r) )
			LOOP
				IF _c.key !~ 'data|aud' THEN
					IF _c.value IS NULL THEN
						SELECT array_append(_vals, 'NULL') INTO _vals;
					ELSE
						SELECT array_append(_vals, quote_literal(_c.value)) INTO _vals;
					END IF;
				END IF;
			END LOOP;
			_eq := 'INSERT INTO ' || quote_ident(in_schema) || '.' ||
				quote_ident(in_table) || ' ( ' ||
				array_to_string(
					schema_support.quote_ident_array(cols), ',') ||
					') VALUES (' ||  array_to_string(_vals, ',', NULL) || ')';
		ELSIF _r."aud#action" in ('INS', 'UPD') THEN
			-- Build up a where clause for this table to get a unique row
			-- based on the primary key
			FOREACH x IN ARRAY pks
			LOOP
				_whcl := '';
				FOR _c IN SELECT * FROM json_each_text( row_to_json(_r) )
				LOOP
					IF _c.key = x THEN
						IF _whcl != '' THEN
							_whcl := _whcl || ', ';
						END IF;
						IF _c.value IS NULL THEN
							_whcl = _whcl || quote_ident(_c.key) || ' = NULL ';
						ELSE
							_whcl = _whcl || quote_ident(_c.key) || ' =  ' ||
								quote_nullable(_c.value);
						END IF;
					END IF;
				END LOOP;
			END LOOP;

			IF _r."aud#action" = 'INS' THEN
				_eq := 'DELETE FROM ' || quote_ident(in_schema) || '.' ||
					quote_ident(in_table) || ' WHERE ' || _whcl;
			ELSIF _r."aud#action" = 'UPD' THEN
				-- figure out what rows have changed and do an update if
				-- they have.  NOTE:  This may result in no change being
				-- replayed if a row did not actually change
				setstr = '';
				FOR _c IN SELECT * FROM json_each_text( row_to_json(_r) )
				LOOP
					--
					-- Iterate over all the columns and if they have changed,
					-- then build an update statement
					--
					IF _c.key !~ 'aud#|data_(ins|upd)_(user|date)' THEN
						EXECUTE 'SELECT ' || _c.key || ' FROM ' ||
							quote_ident(in_schema) || '.' ||
								quote_ident(in_table)  ||
							' WHERE ' || _whcl
							INTO val;
						IF ( _c.value IS NULL  AND val IS NOT NULL) OR
							( _c.value IS NOT NULL AND val IS NULL) OR
							(_c.value::text NOT SIMILAR TO val::text) THEN
							IF char_length(setstr) > 0 THEN
								setstr = setstr || ',
								';
							END IF;
							IF _c.value IS NOT  NULL THEN
								setstr = setstr || _c.key || ' = ' ||
									quote_nullable(_c.value) || ' ' ;
							ELSE
								setstr = setstr || _c.key || ' = ' ||
									' NULL ' ;
							END IF;
						END IF;
					END IF;
				END LOOP;
				IF char_length(setstr) > 0 THEN
					_eq := 'UPDATE ' || quote_ident(in_schema) || '.' ||
						quote_ident(in_table) ||
						' SET ' || setstr || ' WHERE ' || _whcl;
				END IF;
			END IF;
		END IF;
		IF _eq IS NOT NULL THEN
			tally := tally + 1;
			RAISE NOTICE '%', _eq;
			EXECUTE _eq;
		END IF;
	END LOOP;
	RETURN tally;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION schema_support.mv_last_updated(relation text, schema text DEFAULT 'jazzhands'::text, debug boolean DEFAULT false)
 RETURNS timestamp without time zone
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO schema_support
AS $function$
DECLARE
	rv	timestamp;
BEGIN
	IF debug THEN
		RAISE NOTICE 'selecting for update...';
	END IF;

	SELECT	refresh
	INTO	rv
	FROM	schema_support.mv_refresh r
	WHERE	r.schema = mv_last_updated.schema
	AND	r.view = relation
	FOR UPDATE;

	IF debug THEN
		RAISE NOTICE 'returning %', rv;
	END IF;

	RETURN rv;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION schema_support.rebuild_audit_table(aud_schema character varying, tbl_schema character varying, table_name character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	idx	 text[];
	keys text[];
	cols text[];
	i	text;
BEGIN
	-- rename all the old indexes and constraints on the old audit table
	SELECT	array_agg(c2.relname)
		INTO	 idx
		  FROM	pg_catalog.pg_index i
			LEFT JOIN pg_catalog.pg_class c
				ON c.oid = i.indrelid
			LEFT JOIN pg_catalog.pg_class c2
				ON i.indexrelid = c2.oid
			LEFT JOIN pg_catalog.pg_namespace n
				ON c2.relnamespace = n.oid
			LEFT JOIN pg_catalog.pg_constraint con
				ON (conrelid = i.indrelid
				AND conindid = i.indexrelid
				AND contype IN ('p','u','x'))
		 WHERE n.nspname = quote_ident(aud_schema)
		  AND	c.relname = quote_ident(table_name)
		  AND	contype is NULL
	;

	SELECT array_agg(con.conname)
	INTO	keys
    FROM pg_catalog.pg_class c
		INNER JOIN pg_namespace n
			ON relnamespace = n.oid
		INNER JOIN pg_catalog.pg_index i
			ON c.oid = i.indrelid
		INNER JOIN pg_catalog.pg_class c2
			ON i.indexrelid = c2.oid
		INNER JOIN pg_catalog.pg_constraint con ON
			(con.conrelid = i.indrelid
			AND con.conindid = i.indexrelid )
	WHERE  	n.nspname = quote_ident(aud_schema)
	AND		c.relname = quote_ident(table_name)
	AND con.contype in ('p', 'u')
	;

	FOREACH i IN ARRAY idx
	LOOP
		EXECUTE 'ALTER INDEX '
			|| quote_ident(aud_schema) || '.'
			|| quote_ident(i)
			|| ' RENAME TO '
			|| quote_ident('_' || i);
	END LOOP;

	IF array_length(keys, 1) > 0 THEN
		FOREACH i IN ARRAY keys
		LOOP
			EXECUTE 'ALTER TABLE '
				|| quote_ident(aud_schema) || '.'
				|| quote_ident(table_name)
				|| ' RENAME CONSTRAINT '
				|| quote_ident(i)
				|| ' TO '
			|| quote_ident('__old__' || i);
		END LOOP;
	END IF;

	--
	-- get columns
	--
	SELECT	array_agg(quote_ident(a.attname) ORDER BY a.attnum)
	INTO	cols
	FROM	pg_catalog.pg_attribute a
	INNER JOIN pg_catalog.pg_class c on a.attrelid = c.oid
	INNER JOIN pg_catalog.pg_namespace n on n.oid = c.relnamespace
	LEFT JOIN pg_catalog.pg_description d
			on d.objoid = a.attrelid
			and d.objsubid = a.attnum
	WHERE  	n.nspname = quote_ident(aud_schema)
	  AND	c.relname = quote_ident(table_name)
	  AND 	a.attnum > 0
	  AND 	NOT a.attisdropped
	;

	--
	-- rename table
	--
	EXECUTE 'ALTER TABLE '
		|| quote_ident(aud_schema) || '.'
		|| quote_ident(table_name)
		|| ' RENAME TO '
		|| quote_ident('__old__' || table_name);


	--
	-- RENAME sequence
	--
	EXECUTE 'ALTER SEQUENCE '
		|| quote_ident(aud_schema) || '.'
		|| quote_ident(table_name || '_seq')
		|| ' RENAME TO '
		|| quote_ident('_old_' || table_name || '_seq');

	--
	-- create a new audit table
	--
	PERFORM schema_support.build_audit_table(aud_schema,tbl_schema,table_name);

	EXECUTE 'INSERT INTO '
		|| quote_ident(aud_schema) || '.'
		|| quote_ident(table_name) || ' ( '
		|| array_to_string(cols, ',') || ' ) SELECT '
		|| array_to_string(cols, ',') || ' FROM '
		|| quote_ident(aud_schema) || '.'
		|| quote_ident('__old__' || table_name)
		|| ' ORDER BY '
		|| quote_ident('aud#seq');

	EXECUTE 'DROP TABLE '
		|| quote_ident(aud_schema) || '.'
		|| quote_ident('__old__' || table_name);

	--
	-- drop audit sequence, in case it was nto dropped with table.
	--
	EXECUTE 'DROP SEQUENCE IF EXISTS '
		|| quote_ident(aud_schema) || '.'
		|| quote_ident('_old_' || table_name || '_seq');

	--
	-- drop constraints and indexes found before
	--
	FOR i IN SELECT	c2.relname
		  FROM	pg_catalog.pg_index i
			LEFT JOIN pg_catalog.pg_class c
				ON c.oid = i.indrelid
			LEFT JOIN pg_catalog.pg_class c2
				ON i.indexrelid = c2.oid
			LEFT JOIN pg_catalog.pg_namespace n
				ON c2.relnamespace = n.oid
			LEFT JOIN pg_catalog.pg_constraint con
				ON (conrelid = i.indrelid
				AND conindid = i.indexrelid
				AND contype IN ('p','u','x'))
		 WHERE n.nspname = quote_ident(aud_schema)
		  AND	c.relname = quote_ident('__old__' || table_name)
		  AND	contype is NULL
	LOOP
		EXECUTE 'DROP INDEX ' 
			|| quote_ident(aud_schema) || '.'
			|| quote_ident('_' || i);
	END LOOP;
	

	--
	-- recreate audit trigger
	--
	PERFORM schema_support.rebuild_audit_trigger (
		aud_schema, tbl_schema, table_name );

END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION schema_support.rebuild_audit_tables(aud_schema character varying, tbl_schema character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
     table_list RECORD;
BEGIN
    FOR table_list IN
	SELECT b.table_name
	FROM information_schema.tables b
		INNER JOIN information_schema.tables a
			USING (table_name,table_type)
	WHERE table_type = 'BASE TABLE'
	AND a.table_schema = aud_schema
	AND b.table_schema = tbl_schema
	ORDER BY table_name
    LOOP
	PERFORM schema_support.save_dependent_objects_for_replay(aud_schema::varchar, table_list.table_name::varchar);
	PERFORM schema_support.rebuild_audit_table
	    ( aud_schema, tbl_schema, table_list.table_name );
	PERFORM schema_support.replay_object_recreates();
	PERFORM schema_support.replay_saved_grants();
    END LOOP;

    PERFORM schema_support.rebuild_audit_triggers(aud_schema, tbl_schema);
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION schema_support.refresh_mv_if_needed(relation text, schema text DEFAULT 'jazzhands'::text, debug boolean DEFAULT false)
 RETURNS void
 LANGUAGE plpgsql
 SET search_path TO schema_support
AS $function$
DECLARE 
	lastref	timestamp;
	lastdat	timestamp;
BEGIN
	SELECT coalesce(schema_support.mv_last_updated(relation, schema,debug),'-infinity') INTO lastref;
	SELECT coalesce(schema_support.relation_last_changed(relation, schema,debug),'-infinity') INTO lastdat;
	IF lastdat > lastref THEN
		EXECUTE 'REFRESH MATERIALIZED VIEW ' || quote_ident(schema)||'.'||quote_ident(relation);
		PERFORM schema_support.set_mv_last_updated(relation, schema);
	END IF;
	RETURN;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION schema_support.relation_last_changed(relation text, schema text DEFAULT 'jazzhands'::text, debug boolean DEFAULT false)
 RETURNS timestamp without time zone
 LANGUAGE plpgsql
 SET search_path TO schema_support
AS $function$
DECLARE
	audsch	text;
	rk	char;
	rv	timestamp;
	ts	timestamp;
	obj	text;
	objaud text;
BEGIN
	SELECT	audit_schema
	INTO	audsch
	FROM	schema_support.schema_audit_map m
	WHERE	m.schema = relation_last_changed.schema;

	IF NOT FOUND THEN
		RAISE EXCEPTION 'Schema % not configured for this', schema;
	END IF;

	SELECT 	relkind
	INTO	rk
	FROM	pg_catalog.pg_class c
		JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
	WHERE	n.nspname = relation_last_changed.schema
	AND	c.relname = relation_last_changed.relation;

	IF NOT FOUND THEN
		RAISE EXCEPTION 'No such object %.%', schema, relation;
	END IF;

	IF rk = 'r' THEN
		EXECUTE '
			SELECT	max("aud#timestamp")
			FROM	'||quote_ident(audsch)||'.'||quote_ident(relation)
		INTO rv;

		IF rv IS NULL THEN
			RETURN '-infinity'::interval;
		ELSE
			RETURN rv;
		END IF;
	END IF;

	IF rk = 'v' OR rk = 'm' THEN
		FOR obj,objaud IN WITH RECURSIVE recur AS (
				SELECT distinct rewrite.ev_class as root_oid, d.refobjid as oid
				FROM pg_depend d
        			JOIN pg_rewrite rewrite ON d.objid = rewrite.oid
					JOIN pg_class c on rewrite.ev_class = c.oid
					JOIN pg_namespace n on n.oid = c.relnamespace
				WHERE c.relname = relation
				AND n.nspname = relation_last_changed.schema
				AND d.refobjsubid > 0
			UNION ALL
				SELECT recur.root_oid, d.refobjid as oid
				FROM pg_depend d
        			JOIN pg_rewrite rewrite ON d.objid = rewrite.oid
				JOIN recur ON recur.oid = rewrite.ev_class
				AND d.refobjsubid > 0
			), list AS ( select distinct m.audit_schema, c.relname, c.relkind, recur.*
				FROM pg_class c
					JOIN recur on recur.oid = c.oid
					JOIN pg_namespace n on c.relnamespace = n.oid
					JOIN schema_support.schema_audit_map m
						ON m.schema = n.nspname
				WHERE relkind = 'r'
			) SELECT relname, audit_schema from list
		LOOP
			-- if there is no audit table, assume its kept current.  This is
			-- likely some sort of cache table.  XXX - should probably be
			-- updated to use the materialized view update bits
			BEGIN
				EXECUTE 'SELECT max("aud#timestamp") 
					FROM '||quote_ident(objaud)||'.'|| quote_ident(obj) 
					INTO ts;
				IF debug THEN
					RAISE NOTICE '%.% -> %', objaud, obj, ts;
				END IF;
				IF rv IS NULL OR ts > rv THEN
					rv := ts;
				END IF;
			EXCEPTION WHEN undefined_table THEN
				IF debug THEN
					RAISE NOTICE 'skipping %.%', schema, obj;
				END IF;
			END;
		END LOOP;
		RETURN rv;
	END IF;

	RAISE EXCEPTION 'Unable to process relkind %', rk;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION schema_support.set_mv_last_updated(relation text, schema text DEFAULT 'jazzhands'::text, debug boolean DEFAULT false)
 RETURNS timestamp without time zone
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO schema_support
AS $function$
DECLARE
	rv	timestamp;
BEGIN
	INSERT INTO schema_support.mv_refresh AS r (
		schema, view, refresh
	) VALUES (
		set_mv_last_updated.schema, relation, now()
	) ON CONFLICT ON CONSTRAINT mv_refresh_pkey DO UPDATE
		SET		refresh = now()
		WHERE	r.schema = set_mv_last_updated.schema
		AND		r.view = relation
	;

	RETURN rv;
END;
$function$
;

--
-- Process middle (non-trigger) schema script_hooks
--
--
-- Process middle (non-trigger) schema backend_utils
--
-- New function
CREATE OR REPLACE FUNCTION backend_utils.refresh_if_needed(object text)
 RETURNS void
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	rk char;
BEGIN
	SELECT  relkind
	INTO    rk
	FROM    pg_catalog.pg_class c
		JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
	WHERE   n.nspname = 'jazzhands'
	AND     c.relname = relation_last_changed.relation;

	-- silently ignore things that are not materialized views
	IF rk = 'm' THEN
		PERFORM schema_support.refresh_mv_if_needed(object, 'jazzhands');
	END IF;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION backend_utils.relation_last_changed(view text)
 RETURNS timestamp without time zone
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	RETURN schema_support.relation_last_changed(view);
END;
$function$
;

-- Creating new sequences....


--------------------------------------------------------------------
-- DEALING WITH NEW TABLE mv_unix_passwd_mappings
DROP MATERIALIZED VIEW IF EXISTS jazzhands.mv_unix_passwd_mappings;
CREATE MATERIALIZED VIEW jazzhands.mv_unix_passwd_mappings AS
 SELECT v_unix_passwd_mappings.device_collection_id,
    v_unix_passwd_mappings.account_id,
    v_unix_passwd_mappings.login,
    v_unix_passwd_mappings.crypt,
    v_unix_passwd_mappings.unix_uid,
    v_unix_passwd_mappings.unix_group_name,
    v_unix_passwd_mappings.gecos,
    v_unix_passwd_mappings.home,
    v_unix_passwd_mappings.shell,
    v_unix_passwd_mappings.ssh_public_key,
    v_unix_passwd_mappings.setting,
    v_unix_passwd_mappings.mclass_setting,
    v_unix_passwd_mappings.extra_groups
   FROM v_unix_passwd_mappings;

-- DONE DEALING WITH TABLE mv_unix_passwd_mappings [1777290]
--------------------------------------------------------------------
--------------------------------------------------------------------
-- DEALING WITH NEW TABLE mv_unix_group_mappings
DROP MATERIALIZED VIEW IF EXISTS jazzhands.mv_unix_group_mappings;
CREATE MATERIALIZED VIEW jazzhands.mv_unix_group_mappings AS
 SELECT v_unix_group_mappings.device_collection_id,
    v_unix_group_mappings.account_collection_id,
    v_unix_group_mappings.group_name,
    v_unix_group_mappings.unix_gid,
    v_unix_group_mappings.group_password,
    v_unix_group_mappings.setting,
    v_unix_group_mappings.mclass_setting,
    v_unix_group_mappings.members
   FROM v_unix_group_mappings;

-- DONE DEALING WITH TABLE mv_unix_group_mappings [1777297]
--------------------------------------------------------------------
--------------------------------------------------------------------
-- DEALING WITH TABLE v_person_company_audit_map [1677188]
SELECT schema_support.save_dependent_objects_for_replay
        ('audit', 'person_company');
SELECT schema_support.rebuild_audit_table
        ('audit', 'jazzhands', 'person_company');

-- Save grants for later reapplication
SELECT schema_support.save_grants_for_replay('jazzhands', 'v_person_company_audit_map', 'v_person_company_audit_map');
SELECT schema_support.save_dependent_objects_for_replay('approval_utils', 'v_person_company_audit_map');
DROP VIEW IF EXISTS approval_utils.v_person_company_audit_map;
CREATE VIEW approval_utils.v_person_company_audit_map AS
 WITH all_audrecs AS (
         SELECT pca.company_id,
            pca.person_id,
            pca.person_company_status,
            pca.person_company_relation,
            pca.is_exempt,
            pca.is_management,
            pca.is_full_time,
            pca.description,
            pca.employee_id,
            pca.payroll_id,
            pca.external_hr_id,
            pca.position_title,
            pca.badge_system_id,
            pca.hire_date,
            pca.termination_date,
            pca.manager_person_id,
            pca.supervisor_person_id,
            pca.nickname,
            pca.data_ins_user,
            pca.data_ins_date,
            pca.data_upd_user,
            pca.data_upd_date,
            pca."aud#action",
            pca."aud#timestamp",
            pca."aud#realtime",
            pca."aud#txid",
            pca."aud#user",
            pca."aud#seq",
            row_number() OVER (PARTITION BY pc.person_id, pc.company_id ORDER BY pca."aud#timestamp" DESC) AS rownum
           FROM person_company pc
             JOIN audit.person_company pca USING (person_id, company_id)
          WHERE pca."aud#action" = ANY (ARRAY['UPD'::bpchar, 'INS'::bpchar])
        )
 SELECT all_audrecs."aud#seq" AS audit_seq_id,
    all_audrecs.company_id,
    all_audrecs.person_id,
    all_audrecs.person_company_status,
    all_audrecs.person_company_relation,
    all_audrecs.is_exempt,
    all_audrecs.is_management,
    all_audrecs.is_full_time,
    all_audrecs.description,
    all_audrecs.employee_id,
    all_audrecs.payroll_id,
    all_audrecs.external_hr_id,
    all_audrecs.position_title,
    all_audrecs.badge_system_id,
    all_audrecs.hire_date,
    all_audrecs.termination_date,
    all_audrecs.manager_person_id,
    all_audrecs.supervisor_person_id,
    all_audrecs.nickname,
    all_audrecs.data_ins_user,
    all_audrecs.data_ins_date,
    all_audrecs.data_upd_user,
    all_audrecs.data_upd_date,
    all_audrecs."aud#action",
    all_audrecs."aud#timestamp",
    all_audrecs."aud#realtime",
    all_audrecs."aud#txid",
    all_audrecs."aud#user",
    all_audrecs."aud#seq",
    all_audrecs.rownum
   FROM all_audrecs
  WHERE all_audrecs.rownum = 1;

delete from __recreate where type = 'view' and object = 'v_person_company_audit_map';
-- DONE DEALING WITH TABLE v_person_company_audit_map [1777335]
--------------------------------------------------------------------
--------------------------------------------------------------------
-- DEALING WITH TABLE v_account_collection_account_audit_map [1677183]
SELECT schema_support.save_dependent_objects_for_replay
        ('audit', 'account_collection_account');
SELECT schema_support.rebuild_audit_table
        ('audit', 'jazzhands', 'account_collection_account');

-- Save grants for later reapplication
SELECT schema_support.save_grants_for_replay('jazzhands', 'v_account_collection_account_audit_map', 'v_account_collection_account_audit_map');
SELECT schema_support.save_dependent_objects_for_replay('approval_utils', 'v_account_collection_account_audit_map');
DROP VIEW IF EXISTS approval_utils.v_account_collection_account_audit_map;
CREATE VIEW approval_utils.v_account_collection_account_audit_map AS
 WITH all_audrecs AS (
         SELECT acaa.account_collection_id,
            acaa.account_id,
            acaa.account_id_rank,
            acaa.start_date,
            acaa.finish_date,
            acaa.data_ins_user,
            acaa.data_ins_date,
            acaa.data_upd_user,
            acaa.data_upd_date,
            acaa."aud#action",
            acaa."aud#timestamp",
            acaa."aud#realtime",
            acaa."aud#txid",
            acaa."aud#user",
            acaa."aud#seq",
            row_number() OVER (PARTITION BY aca.account_collection_id, aca.account_id ORDER BY acaa."aud#timestamp" DESC) AS rownum
           FROM account_collection_account aca
             JOIN audit.account_collection_account acaa USING (account_collection_id, account_id)
          WHERE acaa."aud#action" = ANY (ARRAY['UPD'::bpchar, 'INS'::bpchar])
        )
 SELECT all_audrecs."aud#seq" AS audit_seq_id,
    all_audrecs.account_collection_id,
    all_audrecs.account_id,
    all_audrecs.account_id_rank,
    all_audrecs.start_date,
    all_audrecs.finish_date,
    all_audrecs.data_ins_user,
    all_audrecs.data_ins_date,
    all_audrecs.data_upd_user,
    all_audrecs.data_upd_date,
    all_audrecs."aud#action",
    all_audrecs."aud#timestamp",
    all_audrecs."aud#realtime",
    all_audrecs."aud#txid",
    all_audrecs."aud#user",
    all_audrecs."aud#seq",
    all_audrecs.rownum
   FROM all_audrecs
  WHERE all_audrecs.rownum = 1;

delete from __recreate where type = 'view' and object = 'v_account_collection_account_audit_map';
-- DONE DEALING WITH TABLE v_account_collection_account_audit_map [1777330]
--------------------------------------------------------------------
--
-- Process drops in jazzhands
--
-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'dns_a_rec_validation');
CREATE OR REPLACE FUNCTION jazzhands.dns_a_rec_validation()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_ip		netblock.ip_address%type;
	_sing	netblock.is_single_address%type;
BEGIN
	IF NEW.dns_type in ('A', 'AAAA') AND NEW.netblock_id IS NULL THEN
		RAISE EXCEPTION 'Attempt to set % record without a Netblock',
			NEW.dns_type
			USING ERRCODE = 'not_null_violation';
	END IF;

	IF NEW.netblock_Id is not NULL and 
			( NEW.dns_value IS NOT NULL OR NEW.dns_value_record_id IS NOT NULL ) THEN
		RAISE EXCEPTION 'Both dns_value and netblock_id may not be set'
			USING ERRCODE = 'JH001';
	END IF;

	IF NEW.dns_value IS NOT NULL AND NEW.dns_value_record_id IS NOT NULL THEN
		RAISE EXCEPTION 'Both dns_value and dns_value_record_id may not be set'
			USING ERRCODE = 'JH001';
	END IF;

	IF NEW.netblock_id IS NOT NULL AND NEW.dns_value_record_id IS NOT NULL THEN
		RAISE EXCEPTION 'Both netblock_id and dns_value_record_id may not be set'
			USING ERRCODE = 'JH001';
	END IF;

	-- XXX need to deal with changing a netblock type and breaking dns_record.. 
	IF NEW.netblock_id IS NOT NULL THEN
		SELECT ip_address, is_single_address
		  INTO _ip, _sing
		  FROM netblock
		 WHERE netblock_id = NEW.netblock_id;

		IF NEW.dns_type = 'A' AND family(_ip) != '4' THEN
			RAISE EXCEPTION 'A records must be assigned to non-IPv4 records'
				USING ERRCODE = 'JH200';
		END IF;

		IF NEW.dns_type = 'AAAA' AND family(_ip) != '6' THEN
			RAISE EXCEPTION 'AAAA records must be assigned to non-IPv6 records'
				USING ERRCODE = 'JH200';
		END IF;

		IF _sing = 'N' AND NEW.dns_type IN ('A','AAAA') THEN
			RAISE EXCEPTION 'Non-single addresses may not have % records', NEW.dns_type 
				USING ERRCODE = 'foreign_key_violation';
		END IF;

	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'validate_device_component_assignment');
CREATE OR REPLACE FUNCTION jazzhands.validate_device_component_assignment()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	dtid		device_type.device_type_id%TYPE;
	dt_ctid		component.component_type_id%TYPE;
	ctid		component.component_type_id%TYPE;
BEGIN
	-- If no component_id is set, then we're done

	IF NEW.component_id IS NULL THEN
		RETURN NEW;
	END IF;

	SELECT
		device_type_id, component_type_id 
	INTO
		dtid, dt_ctid
	FROM
		device_type
	WHERE
		device_type_id = NEW.device_type_id;
	
	IF NOT FOUND OR dt_ctid IS NULL THEN
		RAISE EXCEPTION 'No component_type_id set for device type'
		USING ERRCODE = 'foreign_key_violation';
	END IF;

	SELECT
		component_type_id INTO ctid
	FROM
		component
	WHERE
		component_id = NEW.component_id;
	
	IF NOT FOUND OR ctid IS DISTINCT FROM dt_ctid THEN
		RAISE EXCEPTION 'Component type of component_id % (%s) does not match component_type for device_type_id % (%)',
			NEW.component_id, ctid, dtid, dt_ctid
		USING ERRCODE = 'foreign_key_violation';
	END IF;

	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.company_insert_function_nudge()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	BEGIN
		IF current_setting('jazzhands.permit_company_insert') != 'permit' THEN
			RAISE EXCEPTION  'You may not directly insert into company.'
				USING ERRCODE = 'insufficient_privilege';
		END IF;
	EXCEPTION WHEN undefined_object THEN
			RAISE EXCEPTION  'You may not directly insert into company'
				USING ERRCODE = 'insufficient_privilege';
	END;
	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.dns_record_check_name()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	IF NEW.DNS_NAME IS NOT NULL THEN
		-- rfc rfc952
		IF NEW.DNS_NAME !~ '[-a-zA-Z0-9\._]*' THEN
			RAISE EXCEPTION 'Invalid DNS NAME %', 
				NEW.DNS_NAME
				USING ERRCODE = 'integrity_constraint_violation';
		END IF;
	END IF;
	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.l2_net_coll_member_enforce_on_type_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	l2ct		val_layer2_network_coll_type%ROWTYPE;
	old_l2ct	val_layer2_network_coll_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	l2ct
	FROM	val_layer2_network_coll_type
	WHERE	layer2_network_collection_type = NEW.layer2_network_collection_type;

	SELECT *
	INTO	old_l2ct
	FROM	val_layer2_network_coll_type
	WHERE	layer2_network_collection_type = OLD.layer2_network_collection_type;

	--
	-- We only need to check this if we are enforcing now where we didn't used
	-- to need to
	--
	IF l2ct.max_num_members IS NOT NULL AND 
			l2ct.max_num_members IS DISTINCT FROM old_l2ct.max_num_members THEN
		select count(*)
		  into tally
		  from l2_network_coll_l2_network
		  where layer2_network_collection_id = NEW.layer2_network_collection_id;
		IF tally > l2ct.max_num_members THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF l2ct.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		SELECT MAX(l2count) FROM (
			SELECT
				COUNT(*) AS l2count
			FROM
				l2_network_coll_l2_network JOIN
				layer2_network_collection USING (layer2_network_collection_id)
			WHERE
				layer2_network_collection_type = NEW.layer2_network_collection_type
			GROUP BY
				layer2_network_id
		) x INTO tally;

		IF tally > l2ct.max_num_collections THEN
			RAISE EXCEPTION 'Layer2 network may not be a member of more than % collections of type %',
				l2ct.MAX_NUM_COLLECTIONS, l2ct.layer2_network_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.l3_net_coll_member_enforce_on_type_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	l3ct		val_layer3_network_coll_type%ROWTYPE;
	old_l3ct	val_layer3_network_coll_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	l3ct
	FROM	val_layer3_network_coll_type
	WHERE	layer3_network_collection_type = NEW.layer3_network_collection_type;

	SELECT *
	INTO	old_l3ct
	FROM	val_layer3_network_coll_type
	WHERE	layer3_network_collection_type = OLD.layer3_network_collection_type;

	--
	-- We only need to check this if we are enforcing now where we didn't used
	-- to need to
	--
	IF l3ct.max_num_members IS NOT NULL AND 
			l3ct.max_num_members IS DISTINCT FROM old_l3ct.max_num_members THEN
		select count(*)
		  into tally
		  from l3_network_coll_l3_network
		  where layer3_network_collection_id = NEW.layer3_network_collection_id;
		IF tally > l3ct.max_num_members THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF l3ct.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		SELECT MAX(l3count) FROM (
			SELECT
				COUNT(*) AS l3count
			FROM
				l3_network_coll_l3_network JOIN
				layer3_network_collection USING (layer3_network_collection_id)
			WHERE
				layer3_network_collection_type = NEW.layer3_network_collection_type
			GROUP BY
				layer3_network_id
		) x INTO tally;

		IF tally > l3ct.max_num_collections THEN
			RAISE EXCEPTION 'Layer2 network may not be a member of more than % collections of type %',
				l3ct.MAX_NUM_COLLECTIONS, l3ct.layer3_network_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.nb_dns_a_rec_validation()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tal	integer;
BEGIN
	IF family(OLD.ip_address) != family(NEW.ip_address) THEN
		IF family(NEW.ip_address) == 6 THEN
			SELECT count(*)
			INTO	_tal
			FROM	dns_record
			WHERE	netblock_id = NEW.netblock_id
			AND		dns_type = 'A';

			IF _tal > 0 THEN
				RAISE EXCEPTION 'A records must be assigned to IPv4 records'
					USING ERRCODE = 'JH200';
			END IF;
		END IF;
	END IF;

	IF family(OLD.ip_address) != family(NEW.ip_address) THEN
		IF family(NEW.ip_address) == 4 THEN
			SELECT count(*)
			INTO	_tal
			FROM	dns_record
			WHERE	netblock_id = NEW.netblock_id
			AND		dns_type = 'AAAA';

			IF _tal > 0 THEN
				RAISE EXCEPTION 'AAAA records must be assigned to IPv6 records'
					USING ERRCODE = 'JH200';
			END IF;
		END IF;
	END IF;

	IF NEW.is_single_address = 'N' THEN
			SELECT count(*)
			INTO	_tal
			FROM	dns_record
			WHERE	netblock_id = NEW.netblock_id
			AND		dns_type IN ('A', 'AAAA');

		IF _tal > 0 THEN
			RAISE EXCEPTION 'Non-single addresses may not have % records', NEW.dns_type 
				USING ERRCODE = 'foreign_key_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.pgnotify_account_collection_account_token_changes()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	IF TG_OP = 'UPDATE' OR TG_OP = 'DELETE' THEN
		PERFORM	*
		FROM	property_collection
				JOIN property_collection_property pcp 
					USING (property_collection_id)
				JOIN property p
					USING (property_name, property_type)
		WHERE	p.account_collection_id = OLD.account_collection_id
		AND		property_collection_type = 'jazzhands-internal'
		AND		property_collection_name = 'notify-account_collection_account'
		;

		IF FOUND THEN
			PERFORM pg_notify('account_change', concat('account_id=', OLD.account_id));
		END IF;
	END IF;
	IF TG_OP = 'UPDATE' OR TG_OP = 'INSERT' THEN
		PERFORM	*
		FROM	property_collection
				JOIN property_collection_property pcp 
					USING (property_collection_id)
				JOIN property p
					USING (property_name, property_type)
		WHERE	p.account_collection_id = NEW.account_collection_id
		AND		property_collection_type = 'jazzhands-internal'
		AND		property_collection_name = 'notify-account_collection_account'
		;

		IF FOUND THEN
			PERFORM pg_notify('account_change', concat('account_id=', NEW.account_id));
		END IF;
	END IF;

	IF TG_OP = 'DELETE' THEN
		RETURN OLD;
	ELSE
		RETURN NEW;
	END IF;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.pgnotify_account_password_changes()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	PERFORM pg_notify ('account_password_change', 'account_id=' || NEW.account_id);
	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.pgnotify_account_token_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	PERFORM pg_notify ('account_id', 'account_id=' || NEW.account_id);
	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.pgnotify_token_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	PERFORM pg_notify ('token_change', 'token_id=' || NEW.token_id);
	RETURN NEW;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.unrequire_password_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	DELETE FROM account_collection_account
	WHERE (account_collection_id, account_id)
	IN (
		SELECT	a.account_collection_id, a.account_id
		FROM	v_acct_coll_acct_expanded a
				JOIN account_collection ac USING (account_collection_id)
				JOIN property p USING (account_collection_id)
		WHERE	p.property_type = 'UserMgmt'
		AND		p.property_name = 'NeedsPasswdChange'
		AND	 	a.account_id = NEW.account_id
	);
	RETURN NEW;
END;
$function$
;

--
-- Process drops in net_manip
--
--
-- Process drops in network_strings
--
--
-- Process drops in time_util
--
--
-- Process drops in dns_utils
--
--
-- Process drops in person_manip
--
DROP FUNCTION IF EXISTS person_manip.add_user ( company_id integer, person_company_relation character varying, login character varying, first_name character varying, middle_name character varying, last_name character varying, name_suffix character varying, gender character varying, preferred_last_name character varying, preferred_first_name character varying, birth_date date, external_hr_id character varying, person_company_status character varying, is_manager character varying, is_exempt character varying, is_full_time character varying, employee_id text, hire_date date, termination_date date, job_title character varying, department_name character varying, description character varying, unix_uid character varying, INOUT person_id integer, OUT dept_account_collection_id integer, OUT account_id integer );
-- New function
CREATE OR REPLACE FUNCTION person_manip.add_user(company_id integer, person_company_relation character varying, login character varying DEFAULT NULL::character varying, first_name character varying DEFAULT NULL::character varying, middle_name character varying DEFAULT NULL::character varying, last_name character varying DEFAULT NULL::character varying, name_suffix character varying DEFAULT NULL::character varying, gender character varying DEFAULT NULL::character varying, preferred_last_name character varying DEFAULT NULL::character varying, preferred_first_name character varying DEFAULT NULL::character varying, birth_date date DEFAULT NULL::date, external_hr_id character varying DEFAULT NULL::character varying, person_company_status character varying DEFAULT 'enabled'::character varying, is_management character varying DEFAULT 'N'::character varying, is_manager character varying DEFAULT NULL::character varying, is_exempt character varying DEFAULT 'Y'::character varying, is_full_time character varying DEFAULT 'Y'::character varying, employee_id text DEFAULT NULL::text, hire_date date DEFAULT NULL::date, termination_date date DEFAULT NULL::date, position_title character varying DEFAULT NULL::character varying, job_title character varying DEFAULT NULL::character varying, department_name character varying DEFAULT NULL::character varying, manager_person_id integer DEFAULT NULL::integer, site_code character varying DEFAULT NULL::character varying, physical_address_id integer DEFAULT NULL::integer, person_location_type character varying DEFAULT 'office'::character varying, description character varying DEFAULT NULL::character varying, unix_uid character varying DEFAULT NULL::character varying, INOUT person_id integer DEFAULT NULL::integer, OUT dept_account_collection_id integer, OUT account_id integer)
 RETURNS record
 LANGUAGE plpgsql
 SECURITY DEFINER
AS $function$
DECLARE
    _account_realm_id INTEGER;
    _account_type VARCHAR;
    _uid INTEGER;
    _uxaccountid INTEGER;
    _companyid INTEGER;
    _personid INTEGER;
    _accountid INTEGER;
BEGIN
	IF is_manager IS NOT NULL THEN
		is_management := is_manager;
	END IF;

	IF job_title IS NOT NULL THEN
		position_title := job_title;
	END IF;

    IF company_id is NULL THEN
        RAISE EXCEPTION 'Must specify company id';
    END IF;
    _companyid := company_id;

    SELECT arc.account_realm_id 
      INTO _account_realm_id 
      FROM account_realm_company arc
     WHERE arc.company_id = _companyid;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'Cannot find account_realm_id with company id %', company_id;
    END IF;

    IF login is NULL THEN
        IF first_name IS NULL or last_name IS NULL THEN 
            RAISE EXCEPTION 'Must specify login name or first name+last name';
        ELSE 
            login := person_manip.pick_login(
                in_account_realm_id := _account_realm_id,
                in_first_name := coalesce(preferred_first_name, first_name),
                in_middle_name := middle_name,
                in_last_name := coalesce(preferred_last_name, last_name)
            );
        END IF;
    END IF;

    IF person_company_relation = 'pseudouser' THEN
        person_id := 0;
        _account_type := 'pseudouser';
    ELSE
        _account_type := 'person';
        IF person_id IS NULL THEN
            INSERT INTO person (first_name, middle_name, last_name, name_suffix, gender, preferred_first_name, preferred_last_name, birth_date)
                VALUES (first_name, middle_name, last_name, name_suffix, gender, preferred_first_name, preferred_last_name, birth_date)
            RETURNING person.person_id into _personid;
            person_id = _personid;
        ELSE
            INSERT INTO person (person_id, first_name, middle_name, last_name, name_suffix, gender, preferred_first_name, preferred_last_name, birth_date)
                VALUES (person_id, first_name, middle_name, last_name, name_suffix, gender, preferred_first_name, preferred_last_name, birth_date);
        END IF;
        INSERT INTO person_company
            (person_id, company_id, external_hr_id, person_company_status, is_management, is_exempt, is_full_time, employee_id, hire_date, termination_date, person_company_relation, position_title, manager_person_id)
            VALUES
            (person_id, company_id, external_hr_id, person_company_status, is_management, is_exempt, is_full_time, employee_id, hire_date, termination_date, person_company_relation, position_title, manager_person_id);
        INSERT INTO person_account_realm_company ( person_id, company_id, account_realm_id) VALUES ( person_id, company_id, _account_realm_id);
    END IF;

    INSERT INTO account ( login, person_id, company_id, account_realm_id, account_status, description, account_role, account_type)
        VALUES (login, person_id, company_id, _account_realm_id, person_company_status, description, 'primary', _account_type)
    RETURNING account.account_id INTO account_id;

    IF department_name IS NOT NULL THEN
        dept_account_collection_id = person_manip.get_account_collection_id(department_name, 'department');
        INSERT INTO account_collection_account (account_collection_id, account_id) VALUES ( dept_account_collection_id, account_id);
    END IF;

    IF site_code IS NOT NULL AND physical_address_id IS NOT NULL THEN
        RAISE EXCEPTION 'You must provide either site_code or physical_address_id NOT both';
    END IF;

    IF site_code IS NULL AND physical_address_id IS NOT NULL THEN
        site_code = person_manip.get_site_code_from_physical_address_id(physical_address_id);
    END IF;

    IF physical_address_id IS NULL AND site_code IS NOT NULL THEN
        physical_address_id = person_manip.get_physical_address_from_site_code(site_code);
    END IF;

    IF physical_address_id IS NOT NULL AND site_code IS NOT NULL THEN
        INSERT INTO person_location 
            (person_id, person_location_type, site_code, physical_address_id)
        VALUES
            (person_id, person_location_type, site_code, physical_address_id);
    END IF;


    IF unix_uid IS NOT NULL THEN
        _accountid = account_id;
        SELECT  aui.account_id
          INTO  _uxaccountid
          FROM  account_unix_info aui
        WHERE  aui.account_id = _accountid;

        --
        -- This is creatd by trigger for non-pseudousers, which will
        -- eventually change, so this is here once it goes away.
        --
        IF _uxaccountid IS NULL THEN
            IF unix_uid = 'auto' THEN
                _uid :=  person_manip.get_unix_uid(_account_type);
            ELSE
                _uid := unix_uid::int;
            END IF;

            PERFORM person_manip.setup_unix_account(
                in_account_id := account_id,
                in_account_type := _account_type,
                in_uid := _uid
            );
        END IF;
    END IF;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION person_manip.get_physical_address_from_site_code(_site_code character varying)
 RETURNS integer
 LANGUAGE plpgsql
 SECURITY DEFINER
AS $function$
DECLARE
	_physical_address_id INTEGER;
BEGIN
	SELECT physical_address_id INTO _physical_address_id
		FROM physical_address
		INNER JOIN site USING(physical_address_id)
		WHERE site_code = _site_code;
	RETURN _physical_address_id;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION person_manip.get_site_code_from_physical_address_id(_physical_address_id integer)
 RETURNS character varying
 LANGUAGE plpgsql
 SECURITY DEFINER
AS $function$
DECLARE
	_site_code VARCHAR;
BEGIN
	SELECT site_code INTO _site_code
		FROM physical_address
		INNER JOIN site USING(physical_address_id)
		WHERE physical_address_id = _physical_address_id;
	RETURN _site_code;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION person_manip.set_location(person_id integer, new_site_code character varying DEFAULT NULL::character varying, new_physical_address_id integer DEFAULT NULL::integer, person_location_type character varying DEFAULT 'office'::character varying)
 RETURNS void
 LANGUAGE plpgsql
 SECURITY DEFINER
AS $function$
DECLARE
	_person_id INTEGER;
	_person_location_type VARCHAR;
	_existing_person_location_id INTEGER;
BEGIN
	_person_id = person_id;
	_person_location_type = person_location_type;

	IF ( new_site_code IS NULL AND new_physical_address_id IS NULL )
		OR ( new_site_code IS NOT NULL AND new_physical_address_id IS NOT NULL ) THEN
			RAISE EXCEPTION 'Must specify either new_site_code or new_physical_address';
	END IF;

	IF new_site_code IS NOT NULL AND new_physical_address_id IS NULL THEN
		new_physical_address_id = person_manip.get_physical_address_from_site_code(new_site_code);
	END IF;

	IF new_physical_address_id IS NOT NULL AND new_site_code IS NULL THEN
		new_site_code = person_manip.get_site_code_from_physical_address_id(new_physical_address_id);
	END IF;

	SELECT person_location_id INTO _existing_person_location_id
	FROM person_location pl
	WHERE pl.person_id = _person_id AND pl.person_location_type = _person_location_type;

	IF _existing_person_location_id IS NULL THEN
		INSERT INTO person_location
			(person_id, person_location_type, site_code, physical_address_id)
		VALUES
			(_person_id, _person_location_type, new_site_code, new_physical_address_id);
	ELSE
		UPDATE person_location
		SET (site_code, physical_address_id, building, floor, section, seat_number)
		= (new_site_code, new_physical_address_id, NULL, NULL, NULL, NULL)
		WHERE person_location_id = _existing_person_location_id;
	END IF;
END;
$function$
;

--
-- Process drops in auto_ac_manip
--
--
-- Process drops in company_manip
--
-- Changed function
SELECT schema_support.save_grants_for_replay('company_manip', 'add_company');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS company_manip.add_company ( _company_name text, _company_types text[], _parent_company_id integer, _account_realm_id integer, _company_short_name text, _description text );
CREATE OR REPLACE FUNCTION company_manip.add_company(_company_name text, _company_types text[] DEFAULT NULL::text[], _parent_company_id integer DEFAULT NULL::integer, _account_realm_id integer DEFAULT NULL::integer, _company_short_name text DEFAULT NULL::text, _description text DEFAULT NULL::text)
 RETURNS integer
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_cmpid	company.company_id%type;
	_short	text;
	_isfam	char(1);
	_perm	text;
BEGIN
	IF _company_types @> ARRAY['corporate family'] THEN
		_isfam := 'Y';
	ELSE
		_isfam := 'N';
	END IF;
	IF _company_short_name IS NULL and _isfam = 'Y' THEN
		_short := lower(regexp_replace(
				regexp_replace(
					regexp_replace(_company_name, 
						E'\\s+(ltd|sarl|limited|pt[ye]|GmbH|ag|ab|inc)', 
						'', 'gi'),
					E'[,\\.\\$#@]', '', 'mg'),
				E'\\s+', '_', 'gi'));
	ELSE
		_short := _company_short_name;
	END IF;

	BEGIN
		_perm := current_setting('jazzhands.permit_company_insert');
	EXCEPTION WHEN undefined_object THEN
		_perm := '';
	END;

	SET jazzhands.permit_company_insert = 'permit';

	INSERT INTO company (
		company_name, company_short_name,
		parent_company_id, description
	) VALUES (
		_company_name, _short,
		_parent_company_id, _description
	) RETURNING company_id INTO _cmpid;

	SET jazzhands.permit_company_insert = _perm;

	IF _account_realm_id IS NOT NULL THEN
		INSERT INTO account_realm_company (
			account_realm_id, company_id
		) VALUES (
			_account_realm_id, _cmpid
		);
	END IF;

	IF _company_types IS NOT NULL THEN
		PERFORM company_manip.add_company_types(_cmpid, _account_realm_id, _company_types);
	END IF;

	RETURN _cmpid;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('company_manip', 'add_company_types');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS company_manip.add_company_types ( _company_id integer, _account_realm_id integer, _company_types text[] );
CREATE OR REPLACE FUNCTION company_manip.add_company_types(_company_id integer, _account_realm_id integer DEFAULT NULL::integer, _company_types text[] DEFAULT NULL::text[])
 RETURNS integer
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	x		text;
	count	integer;
BEGIN
	count := 0;
	FOREACH x IN ARRAY _company_types
	LOOP
		INSERT INTO company_type (company_id, company_type)
			VALUES (_company_id, x);
		IF _account_realm_id IS NOT NULL THEN
			PERFORM company_manip.add_auto_collections(_company_id, _account_realm_id, x);
		END IF;
		count := count + 1;
	END LOOP;
	return count;
END;
$function$
;

--
-- Process drops in token_utils
--
--
-- Process drops in port_support
--
--
-- Process drops in port_utils
--
--
-- Process drops in device_utils
--
-- Changed function
SELECT schema_support.save_grants_for_replay('device_utils', 'retire_device');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS device_utils.retire_device ( in_device_id integer, retire_modules boolean );
CREATE OR REPLACE FUNCTION device_utils.retire_device(in_device_id integer, retire_modules boolean DEFAULT false)
 RETURNS boolean
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	tally		INTEGER;
	_r			RECORD;
	_d			DEVICE%ROWTYPE;
	_mgrid		DEVICE.DEVICE_ID%TYPE;
	_purgedev	boolean;
BEGIN
	_purgedev := false;

	BEGIN
		PERFORM local_hooks.device_retire_early(in_Device_Id, false);
	EXCEPTION WHEN invalid_schema_name OR undefined_function THEN
		PERFORM 1;
	END;

	SELECT * INTO _d FROM device WHERE device_id = in_Device_id;
	delete from dns_record where netblock_id in (
		select netblock_id 
		from network_interface where device_id = in_Device_id
	);

	delete from network_interface_purpose where device_id = in_Device_id;

	DELETE FROM network_interface_netblock
	WHERE network_interface_id IN (
			SELECT network_interface_id
		 	FROM network_interface
			WHERE device_id = in_Device_id
	);

	DELETE FROM network_interface WHERE device_id = in_Device_id;

	PERFORM device_utils.purge_physical_ports( in_Device_id);
--	PERFORM device_utils.purge_power_ports( in_Device_id);

	delete from property where device_collection_id in (
		SELECT	dc.device_collection_id 
		  FROM	device_collection dc
				INNER JOIN device_collection_device dcd
		 			USING (device_collection_id)
		WHERE	dc.device_collection_type = 'per-device'
		  AND	dcd.device_id = in_Device_id
	);

	delete from device_collection_device where device_id = in_Device_id;
	delete from snmp_commstr where device_id = in_Device_id;

		
	IF _d.rack_location_id IS NOT NULL  THEN
		UPDATE device SET rack_location_id = NULL 
		WHERE device_id = in_Device_id;

		-- This should not be permitted based on constraints, but in case
		-- that constraint had to be disabled...
		SELECT	count(*)
		  INTO	tally
		  FROM	device
		 WHERE	rack_location_id = _d.RACK_LOCATION_ID;

		IF tally = 0 THEN
			DELETE FROM rack_location 
			WHERE rack_location_id = _d.RACK_LOCATION_ID;
		END IF;
	END IF;

	IF _d.chassis_location_id IS NOT NULL THEN
		RAISE EXCEPTION 'Retiring modules is not supported yet.';
	END IF;

	SELECT	manager_device_id
	INTO	_mgrid
	 FROM	device_management_controller
	WHERE	device_id = in_Device_id AND device_mgmt_control_type = 'bmc'
	LIMIT 1;

	IF _mgrid IS NOT NULL THEN
		DELETE FROM device_management_controller
		WHERE	device_id = in_Device_id AND device_mgmt_control_type = 'bmc'
			AND manager_device_id = _mgrid;

		PERFORM device_utils.retire_device( manager_device_id)
		  FROM	device_management_controller
		WHERE	device_id = in_Device_id AND device_mgmt_control_type = 'bmc';
	END IF;

	BEGIN
		PERFORM local_hooks.device_retire_late(in_Device_Id, false);
	EXCEPTION WHEN invalid_schema_name OR undefined_function THEN
		PERFORM 1;
	END;

	SELECT count(*)
	INTO tally
	FROM device_note
	WHERE device_id = in_Device_id;

	--
	-- If there is no notes or serial number its save to remove
	-- 
	IF tally = 0 AND _d.ASSET_ID is NULL THEN
		_purgedev := true;
	END IF;

	IF _purgedev THEN
		--
		-- If there is an fk violation, we just preserve the record but
		-- delete all the identifying characteristics
		--
		BEGIN
			DELETE FROM device where device_id = in_Device_Id;
			return false;
		EXCEPTION WHEN foreign_key_violation THEN
			PERFORM 1;
		END;
	END IF;

	UPDATE device SET 
		device_name =NULL,
		service_environment_id = (
			select service_environment_id from service_environment
			where service_environment_name = 'unallocated'),
		device_status = 'removed',
		voe_symbolic_track_id = NULL,
		is_monitored = 'N',
		should_fetch_config = 'N',
		description = NULL
	WHERE device_id = in_Device_id;

	return true;
END;
$function$
;

--
-- Process drops in netblock_utils
--
--
-- Process drops in netblock_manip
--
-- Changed function
SELECT schema_support.save_grants_for_replay('netblock_manip', 'allocate_netblock');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS netblock_manip.allocate_netblock ( parent_netblock_id integer, netmask_bits integer, address_type text, can_subnet boolean, allocation_method text, rnd_masklen_threshold integer, rnd_max_count integer, ip_address inet, description character varying, netblock_status character varying );
CREATE OR REPLACE FUNCTION netblock_manip.allocate_netblock(parent_netblock_id integer, netmask_bits integer DEFAULT NULL::integer, address_type text DEFAULT 'netblock'::text, can_subnet boolean DEFAULT true, allocation_method text DEFAULT NULL::text, rnd_masklen_threshold integer DEFAULT 110, rnd_max_count integer DEFAULT 1024, ip_address inet DEFAULT NULL::inet, description character varying DEFAULT NULL::character varying, netblock_status character varying DEFAULT 'Allocated'::character varying)
 RETURNS SETOF netblock
 LANGUAGE plpgsql
 SET search_path TO jazzhands
AS $function$
DECLARE
	netblock_rec	RECORD;
BEGIN
	RETURN QUERY 
		SELECT * into netblock_rec FROM netblock_manip.allocate_netblock(
		parent_netblock_list := ARRAY[parent_netblock_id],
		netmask_bits := netmask_bits,
		address_type := address_type,
		can_subnet := can_subnet,
		description := description,
		allocation_method := allocation_method,
		ip_address := ip_address,
		rnd_masklen_threshold := rnd_masklen_threshold,
		rnd_max_count := rnd_max_count,
		netblock_status := netblock_status
	);
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('netblock_manip', 'allocate_netblock');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS netblock_manip.allocate_netblock ( parent_netblock_list integer[], netmask_bits integer, address_type text, can_subnet boolean, allocation_method text, rnd_masklen_threshold integer, rnd_max_count integer, ip_address inet, description character varying, netblock_status character varying );
CREATE OR REPLACE FUNCTION netblock_manip.allocate_netblock(parent_netblock_list integer[], netmask_bits integer DEFAULT NULL::integer, address_type text DEFAULT 'netblock'::text, can_subnet boolean DEFAULT true, allocation_method text DEFAULT NULL::text, rnd_masklen_threshold integer DEFAULT 110, rnd_max_count integer DEFAULT 1024, ip_address inet DEFAULT NULL::inet, description character varying DEFAULT NULL::character varying, netblock_status character varying DEFAULT 'Allocated'::character varying)
 RETURNS SETOF netblock
 LANGUAGE plpgsql
 SET search_path TO jazzhands
AS $function$
DECLARE
	parent_rec		RECORD;
	netblock_rec	RECORD;
	inet_rec		RECORD;
	loopback_bits	integer;
	inet_family		integer;
	ip_addr			ALIAS FOR ip_address;
BEGIN
	IF parent_netblock_list IS NULL THEN
		RAISE 'parent_netblock_list must be specified'
		USING ERRCODE = 'null_value_not_allowed';
	END IF;

	IF address_type NOT IN ('netblock', 'single', 'loopback') THEN
		RAISE 'address_type must be one of netblock, single, or loopback'
		USING ERRCODE = 'invalid_parameter_value';
	END IF;

	IF netmask_bits IS NULL AND address_type = 'netblock' THEN
		RAISE EXCEPTION
			'You must specify a netmask when address_type is netblock'
			USING ERRCODE = 'invalid_parameter_value';
	END IF;

	IF ip_address IS NOT NULL THEN
		SELECT 
			array_agg(netblock_id)
		INTO
			parent_netblock_list
		FROM
			netblock n
		WHERE
			ip_addr <<= n.ip_address AND
			netblock_id = ANY(parent_netblock_list);

		IF parent_netblock_list IS NULL THEN
			RETURN;
		END IF;
	END IF;

	-- Lock the parent row, which should keep parallel processes from
	-- trying to obtain the same address

	FOR parent_rec IN SELECT * FROM jazzhands.netblock WHERE netblock_id = 
			ANY(allocate_netblock.parent_netblock_list) ORDER BY netblock_id
			FOR UPDATE LOOP

		IF parent_rec.is_single_address = 'Y' THEN
			RAISE EXCEPTION 'parent_netblock_id refers to a single_address netblock'
				USING ERRCODE = 'invalid_parameter_value';
		END IF;

		IF inet_family IS NULL THEN
			inet_family := family(parent_rec.ip_address);
		ELSIF inet_family != family(parent_rec.ip_address) 
				AND ip_address IS NULL THEN
			RAISE EXCEPTION 'Allocation may not mix IPv4 and IPv6 addresses'
			USING ERRCODE = 'JH10F';
		END IF;

		IF address_type = 'loopback' THEN
			loopback_bits := 
				CASE WHEN 
					family(parent_rec.ip_address) = 4 THEN 32 ELSE 128 END;

			IF parent_rec.can_subnet = 'N' THEN
				RAISE EXCEPTION 'parent subnet must have can_subnet set to Y'
					USING ERRCODE = 'JH10B';
			END IF;
		ELSIF address_type = 'single' THEN
			IF parent_rec.can_subnet = 'Y' THEN
				RAISE EXCEPTION
					'parent subnet for single address must have can_subnet set to N'
					USING ERRCODE = 'JH10B';
			END IF;
		ELSIF address_type = 'netblock' THEN
			IF parent_rec.can_subnet = 'N' THEN
				RAISE EXCEPTION 'parent subnet must have can_subnet set to Y'
					USING ERRCODE = 'JH10B';
			END IF;
		END IF;
	END LOOP;

 	IF NOT FOUND THEN
 		RETURN;
 	END IF;

	IF address_type = 'loopback' THEN
		-- If we're allocating a loopback address, then we need to create
		-- a new parent to hold the single loopback address

		SELECT * INTO inet_rec FROM netblock_utils.find_free_netblocks(
			parent_netblock_list := parent_netblock_list,
			netmask_bits := loopback_bits,
			single_address := false,
			allocation_method := allocation_method,
			desired_ip_address := ip_address,
			max_addresses := 1
			);

		IF NOT FOUND THEN
			RETURN;
		END IF;

		INSERT INTO jazzhands.netblock (
			ip_address,
			netblock_type,
			is_single_address,
			can_subnet,
			ip_universe_id,
			description,
			netblock_status
		) VALUES (
			inet_rec.ip_address,
			inet_rec.netblock_type,
			'N',
			'N',
			inet_rec.ip_universe_id,
			allocate_netblock.description,
			allocate_netblock.netblock_status
		) RETURNING * INTO parent_rec;

		INSERT INTO jazzhands.netblock (
			ip_address,
			netblock_type,
			is_single_address,
			can_subnet,
			ip_universe_id,
			description,
			netblock_status
		) VALUES (
			inet_rec.ip_address,
			parent_rec.netblock_type,
			'Y',
			'N',
			inet_rec.ip_universe_id,
			allocate_netblock.description,
			allocate_netblock.netblock_status
		) RETURNING * INTO netblock_rec;

		PERFORM dns_utils.add_domains_from_netblock(
			netblock_id := netblock_rec.netblock_id);

		RETURN NEXT netblock_rec;
		RETURN;
	END IF;

	IF address_type = 'single' THEN
		SELECT * INTO inet_rec FROM netblock_utils.find_free_netblocks(
			parent_netblock_list := parent_netblock_list,
			single_address := true,
			allocation_method := allocation_method,
			desired_ip_address := ip_address,
			rnd_masklen_threshold := rnd_masklen_threshold,
			rnd_max_count := rnd_max_count,
			max_addresses := 1
			);

		IF NOT FOUND THEN
			RETURN;
		END IF;

		RAISE DEBUG 'ip_address is %', inet_rec.ip_address;

		INSERT INTO jazzhands.netblock (
			ip_address,
			netblock_type,
			is_single_address,
			can_subnet,
			ip_universe_id,
			description,
			netblock_status
		) VALUES (
			inet_rec.ip_address,
			inet_rec.netblock_type,
			'Y',
			'N',
			inet_rec.ip_universe_id,
			allocate_netblock.description,
			allocate_netblock.netblock_status
		) RETURNING * INTO netblock_rec;

		RETURN NEXT netblock_rec;
		RETURN;
	END IF;
	IF address_type = 'netblock' THEN
		SELECT * INTO inet_rec FROM netblock_utils.find_free_netblocks(
			parent_netblock_list := parent_netblock_list,
			netmask_bits := netmask_bits,
			single_address := false,
			allocation_method := allocation_method,
			desired_ip_address := ip_address,
			max_addresses := 1);

		IF NOT FOUND THEN
			RETURN;
		END IF;

		INSERT INTO jazzhands.netblock (
			ip_address,
			netblock_type,
			is_single_address,
			can_subnet,
			ip_universe_id,
			description,
			netblock_status
		) VALUES (
			inet_rec.ip_address,
			inet_rec.netblock_type,
			'N',
			CASE WHEN can_subnet THEN 'Y' ELSE 'N' END,
			inet_rec.ip_universe_id,
			allocate_netblock.description,
			allocate_netblock.netblock_status
		) RETURNING * INTO netblock_rec;
		
		RAISE DEBUG 'Allocated netblock_id % for %',
			netblock_rec.netblock_id,
			netblock_rec.ip_address;

		PERFORM dns_utils.add_domains_from_netblock(
			netblock_id := netblock_rec.netblock_id);

		RETURN NEXT netblock_rec;
		RETURN;
	END IF;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('netblock_manip', 'create_network_range');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS netblock_manip.create_network_range ( start_ip_address inet, stop_ip_address inet, network_range_type character varying, parent_netblock_id integer, description character varying, allow_assigned boolean );
CREATE OR REPLACE FUNCTION netblock_manip.create_network_range(start_ip_address inet, stop_ip_address inet, network_range_type character varying, parent_netblock_id integer DEFAULT NULL::integer, description character varying DEFAULT NULL::character varying, allow_assigned boolean DEFAULT false)
 RETURNS network_range
 LANGUAGE plpgsql
 SET search_path TO jazzhands
AS $function$
DECLARE
	par_netblock	RECORD;
	start_netblock	RECORD;
	stop_netblock	RECORD;
	netrange		RECORD;
	nrtype			ALIAS FOR network_range_type;
	pnbid			ALIAS FOR parent_netblock_id;
BEGIN
	--
	-- If the network range already exists, then just return it
	--
	SELECT 
		nr.* INTO netrange
	FROM
		jazzhands.network_range nr JOIN
		jazzhands.netblock startnb ON (nr.start_netblock_id = 
			startnb.netblock_id) JOIN
		jazzhands.netblock stopnb ON (nr.stop_netblock_id = stopnb.netblock_id)
	WHERE
		nr.network_range_type = nrtype AND
		host(startnb.ip_address) = host(start_ip_address) AND
		host(stopnb.ip_address) = host(stop_ip_address) AND
		CASE WHEN pnbid IS NOT NULL THEN 
			(pnbid = nr.parent_netblock_id)
		ELSE
			true
		END;

	IF FOUND THEN
		RETURN netrange;
	END IF;

	--
	-- If any other network ranges exist that overlap this, then error
	--
	PERFORM 
		*
	FROM
		jazzhands.network_range nr JOIN
		jazzhands.netblock startnb ON 
			(nr.start_netblock_id = startnb.netblock_id) JOIN
		jazzhands.netblock stopnb ON (nr.stop_netblock_id = stopnb.netblock_id)
	WHERE
		nr.network_range_type = nrtype AND ((
			host(startnb.ip_address)::inet <= host(start_ip_address)::inet AND
			host(stopnb.ip_address)::inet >= host(start_ip_address)::inet
		) OR (
			host(startnb.ip_address)::inet <= host(stop_ip_address)::inet AND
			host(stopnb.ip_address)::inet >= host(stop_ip_address)::inet
		));

	IF FOUND THEN
		RAISE 'create_network_range: a network_range of type % already exists that has addresses between % and %',
			nrtype, start_ip_address, stop_ip_address
			USING ERRCODE = 'check_violation';
	END IF;

	IF parent_netblock_id IS NOT NULL THEN
		SELECT * INTO par_netblock FROM jazzhands.netblock WHERE 
			netblock_id = pnbid;
		IF NOT FOUND THEN
			RAISE 'create_network_range: parent_netblock_id % does not exist',
				parent_netblock_id USING ERRCODE = 'foreign_key_violation';
		END IF;
	ELSE
		SELECT * INTO par_netblock FROM jazzhands.netblock WHERE netblock_id = (
			SELECT 
				*
			FROM
				netblock_utils.find_best_parent_id(
					in_ipaddress := start_ip_address,
					in_is_single_address := 'Y'
				)
		);

		IF NOT FOUND THEN
			RAISE 'create_network_range: valid parent netblock for start_ip_address % does not exist',
				start_ip_address USING ERRCODE = 'check_violation';
		END IF;
	END IF;

	IF par_netblock.can_subnet != 'N' OR 
			par_netblock.is_single_address != 'N' THEN
		RAISE 'create_network_range: parent netblock % must not be subnettable or a single address',
			par_netblock.netblock_id USING ERRCODE = 'check_violation';
	END IF;

	IF NOT (start_ip_address <<= par_netblock.ip_address) THEN
		RAISE 'create_network_range: start_ip_address % is not contained by parent netblock % (%)',
			start_ip_address, par_netblock.ip_address,
			par_netblock.netblock_id USING ERRCODE = 'check_violation';
	END IF;

	IF NOT (stop_ip_address <<= par_netblock.ip_address) THEN
		RAISE 'create_network_range: stop_ip_address % is not contained by parent netblock % (%)',
			stop_ip_address, par_netblock.ip_address,
			par_netblock.netblock_id USING ERRCODE = 'check_violation';
	END IF;

	IF NOT (start_ip_address <= stop_ip_address) THEN
		RAISE 'create_network_range: start_ip_address % is not lower than stop_ip_address %',
			start_ip_address, stop_ip_address
			USING ERRCODE = 'check_violation';
	END IF;

	--
	-- Validate that there are not currently any addresses assigned in the
	-- range, unless allow_assigned is set
	--
	IF NOT allow_assigned THEN
		PERFORM 
			*
		FROM
			jazzhands.netblock n
		WHERE
			n.parent_netblock_id = par_netblock.netblock_id AND
			host(n.ip_address)::inet > host(start_ip_address)::inet AND
			host(n.ip_address)::inet < host(stop_ip_address)::inet;

		IF FOUND THEN
			RAISE 'create_network_range: netblocks are already present for parent netblock % betweeen % and %',
			par_netblock.netblock_id,
			start_ip_address, stop_ip_address
			USING ERRCODE = 'check_violation';
		END IF;
	END IF;

	--
	-- Ok, well, we should be able to insert things now
	--

	SELECT
		*
	FROM
		jazzhands.netblock n
	INTO
		start_netblock
	WHERE
		host(n.ip_address)::inet = start_ip_address AND
		n.netblock_type = 'network_range' AND
		n.can_subnet = 'N' AND
		n.is_single_address = 'Y' AND
		n.ip_universe_id = par_netblock.ip_universe_id;

	IF NOT FOUND THEN
		INSERT INTO netblock (
			ip_address,
			netblock_type,
			is_single_address,
			can_subnet,
			netblock_status,
			ip_universe_id
		) VALUES (
			host(start_ip_address)::inet,
			'network_range',
			'Y',
			'N',
			'Allocated',
			par_netblock.ip_universe_id
		) RETURNING * INTO start_netblock;
	END IF;

	SELECT
		*
	FROM
		jazzhands.netblock n
	INTO
		stop_netblock
	WHERE
		host(n.ip_address)::inet = stop_ip_address AND
		n.netblock_type = 'network_range' AND
		n.can_subnet = 'N' AND
		n.is_single_address = 'Y' AND
		n.ip_universe_id = par_netblock.ip_universe_id;

	IF NOT FOUND THEN
		INSERT INTO netblock (
			ip_address,
			netblock_type,
			is_single_address,
			can_subnet,
			netblock_status,
			ip_universe_id
		) VALUES (
			host(stop_ip_address)::inet,
			'network_range',
			'Y',
			'N',
			'Allocated',
			par_netblock.ip_universe_id
		) RETURNING * INTO stop_netblock;
	END IF;

	INSERT INTO network_range (
		network_range_type,
		description,
		parent_netblock_id,
		start_netblock_id,
		stop_netblock_id
	) VALUES (
		nrtype,
		description,
		par_netblock.netblock_id,
		start_netblock.netblock_id,
		stop_netblock.netblock_id
	) RETURNING * INTO netrange;

	RETURN netrange;

	RETURN NULL;
END;
$function$
;

--
-- Process drops in physical_address_utils
--
--
-- Process drops in component_utils
--
-- Changed function
SELECT schema_support.save_grants_for_replay('component_utils', 'insert_pci_component');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS component_utils.insert_pci_component ( pci_vendor_id integer, pci_device_id integer, pci_sub_vendor_id integer, pci_subsystem_id integer, pci_vendor_name text, pci_device_name text, pci_sub_vendor_name text, pci_sub_device_name text, component_function_list text[], slot_type text, serial_number text );
CREATE OR REPLACE FUNCTION component_utils.insert_pci_component(pci_vendor_id integer, pci_device_id integer, pci_sub_vendor_id integer DEFAULT NULL::integer, pci_subsystem_id integer DEFAULT NULL::integer, pci_vendor_name text DEFAULT NULL::text, pci_device_name text DEFAULT NULL::text, pci_sub_vendor_name text DEFAULT NULL::text, pci_sub_device_name text DEFAULT NULL::text, component_function_list text[] DEFAULT NULL::text[], slot_type text DEFAULT 'unknown'::text, serial_number text DEFAULT NULL::text)
 RETURNS component
 LANGUAGE plpgsql
 SET search_path TO jazzhands
AS $function$
DECLARE
	sn			ALIAS FOR serial_number;
	ctid		integer;
	comp_id		integer;
	sub_comp_id	integer;
	stid		integer;
	vendor_name	text;
	sub_vendor_name	text;
	model_name	text;
	c			RECORD;
BEGIN
	IF (pci_sub_vendor_id IS NULL AND pci_subsystem_id IS NOT NULL) OR
			(pci_sub_vendor_id IS NOT NULL AND pci_subsystem_id IS NULL) THEN
		RAISE EXCEPTION
			'pci_sub_vendor_id and pci_subsystem_id must be set together';
	END IF;

	--
	-- See if we have this component type in the database already
	--
	SELECT
		vid.component_type_id INTO ctid
	FROM
		component_property vid JOIN
		component_property did ON (
			vid.component_property_name = 'PCIVendorID' AND
			vid.component_property_type = 'PCI' AND
			did.component_property_name = 'PCIDeviceID' AND
			did.component_property_type = 'PCI' AND
			vid.component_type_id = did.component_type_id ) LEFT JOIN
		component_property svid ON (
			svid.component_property_name = 'PCISubsystemVendorID' AND
			svid.component_property_type = 'PCI' AND
			svid.component_type_id = did.component_type_id ) LEFT JOIN
		component_property sid ON (
			sid.component_property_name = 'PCISubsystemID' AND
			sid.component_property_type = 'PCI' AND
			sid.component_type_id = did.component_type_id )
	WHERE
		vid.property_value = pci_vendor_id::varchar AND
		did.property_value = pci_device_id::varchar AND
		svid.property_value IS NOT DISTINCT FROM pci_sub_vendor_id::varchar AND
		sid.property_value IS NOT DISTINCT FROM pci_subsystem_id::varchar;

	--
	-- The device type doesn't exist, so attempt to insert it
	--

	IF NOT FOUND THEN	
		IF pci_device_name IS NULL OR component_function_list IS NULL THEN
			RAISE EXCEPTION 'component_id not found and pci_device_name or component_function_list was not passed' USING ERRCODE = 'JH501';
		END IF;

		--
		-- Ensure that there's a company linkage for the PCI (subsystem)vendor
		--
		SELECT
			company_id, company_name INTO comp_id, vendor_name
		FROM
			property p JOIN
			company c USING (company_id)
		WHERE
			property_type = 'DeviceProvisioning' AND
			property_name = 'PCIVendorID' AND
			property_value = pci_vendor_id::text;
		
		IF NOT FOUND THEN
			IF pci_vendor_name IS NULL THEN
				RAISE EXCEPTION 'PCI vendor id mapping not found and pci_vendor_name was not passed' USING ERRCODE = 'JH501';
			END IF;
			SELECT company_id INTO comp_id FROM company
			WHERE company_name = pci_vendor_name;
		
			IF NOT FOUND THEN
				SELECT company_manip.add_company(
					_company_name := pci_vendor_name,
					_company_types := ARRAY['hardware provider'],
					 _description := 'PCI vendor auto-insert'
				) INTO comp_id;
			END IF;

			INSERT INTO property (
				property_name,
				property_type,
				property_value,
				company_id
			) VALUES (
				'PCIVendorID',
				'DeviceProvisioning',
				pci_vendor_id,
				comp_id
			);
			vendor_name := pci_vendor_name;
		END IF;

		SELECT
			company_id, company_name INTO sub_comp_id, sub_vendor_name
		FROM
			property JOIN
			company c USING (company_id)
		WHERE
			property_type = 'DeviceProvisioning' AND
			property_name = 'PCIVendorID' AND
			property_value = pci_sub_vendor_id::text;
		
		IF NOT FOUND THEN
			IF pci_sub_vendor_name IS NULL THEN
				RAISE EXCEPTION 'PCI subsystem vendor id mapping not found and pci_sub_vendor_name was not passed' USING ERRCODE = 'JH501';
			END IF;
			SELECT company_id INTO sub_comp_id FROM company
			WHERE company_name = pci_sub_vendor_name;
		
			IF NOT FOUND THEN
				SELECT company_manip.add_company(
					_company_name := pci_sub_vendor_name,
					_company_types := ARRAY['hardware provider'],
					 _description := 'PCI vendor auto-insert'
				) INTO comp_id;
			END IF;

			INSERT INTO property (
				property_name,
				property_type,
				property_value,
				company_id
			) VALUES (
				'PCIVendorID',
				'DeviceProvisioning',
				pci_sub_vendor_id,
				sub_comp_id
			);
			sub_vendor_name := pci_sub_vendor_name;
		END IF;

		--
		-- Fetch the slot type
		--

		SELECT 
			slot_type_id INTO stid
		FROM
			slot_type st
		WHERE
			st.slot_type = insert_pci_component.slot_type AND
			slot_function = 'PCI';

		IF NOT FOUND THEN
			RAISE EXCEPTION 'slot type % with function PCI not found adding component_type',
				insert_pci_component.slot_type
				USING ERRCODE = 'JH501';
		END IF;

		--
		-- Figure out the best name/description to insert this component with
		--
		IF pci_sub_device_name IS NOT NULL AND pci_sub_device_name != 'Device' THEN
			model_name = concat_ws(' ', 
				sub_vendor_name, pci_sub_device_name,
				'(' || vendor_name, pci_device_name || ')');
		ELSIF pci_sub_device_name = 'Device' THEN
			model_name = concat_ws(' ', 
				vendor_name, '(' || sub_vendor_name || ')', pci_device_name);
		ELSE
			model_name = concat_ws(' ', vendor_name, pci_device_name);
		END IF;
		INSERT INTO component_type (
			company_id,
			model,
			slot_type_id,
			asset_permitted,
			description
		) VALUES (
			CASE WHEN 
				sub_comp_id IS NULL OR
				pci_sub_device_name IS NULL OR
				pci_sub_device_name = 'Device'
			THEN
				comp_id
			ELSE
				sub_comp_id
			END,
			CASE WHEN
				pci_sub_device_name IS NULL OR
				pci_sub_device_name = 'Device'
			THEN
				pci_device_name
			ELSE
				pci_sub_device_name
			END,
			stid,
			'Y',
			model_name
		) RETURNING component_type_id INTO ctid;
		--
		-- Insert properties for the PCI vendor/device IDs
		--
		INSERT INTO component_property (
			component_property_name,
			component_property_type,
			component_type_id,
			property_value
		) VALUES 
			('PCIVendorID', 'PCI', ctid, pci_vendor_id),
			('PCIDeviceID', 'PCI', ctid, pci_device_id);
		
		IF (pci_subsystem_id IS NOT NULL) THEN
			INSERT INTO component_property (
				component_property_name,
				component_property_type,
				component_type_id,
				property_value
			) VALUES 
				('PCISubsystemVendorID', 'PCI', ctid, pci_sub_vendor_id),
				('PCISubsystemID', 'PCI', ctid, pci_subsystem_id);
		END IF;
		--
		-- Insert the component functions
		--

		INSERT INTO component_type_component_func (
			component_type_id,
			component_function
		) SELECT DISTINCT
			ctid,
			cf
		FROM
			unnest(array_append(component_function_list, 'PCI')) x(cf);
	END IF;


	--
	-- We have a component_type_id now, so look to see if this component
	-- serial number already exists
	--
	IF serial_number IS NOT NULL THEN
		SELECT 
			component.* INTO c
		FROM
			component JOIN
			asset a USING (component_id)
		WHERE
			component_type_id = ctid AND
			a.serial_number = sn;

		IF FOUND THEN
			RETURN c;
		END IF;
	END IF;

	INSERT INTO jazzhands.component (
		component_type_id
	) VALUES (
		ctid
	) RETURNING * INTO c;

	IF serial_number IS NOT NULL THEN
		INSERT INTO asset (
			component_id,
			serial_number,
			ownership_status
		) VALUES (
			c.component_id,
			serial_number,
			'unknown'
		);
	END IF;

	RETURN c;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION component_utils.fetch_component(component_type_id integer, serial_number text, no_create boolean DEFAULT false, ownership_status text DEFAULT 'unknown'::text)
 RETURNS component
 LANGUAGE plpgsql
 SET search_path TO jazzhands
AS $function$
DECLARE
	ctid		ALIAS FOR component_type_id;
	sn			ALIAS FOR serial_number;
	os			ALIAS FOR ownership_status;
	c			RECORD;
	cid			integer;
BEGIN
	cid := NULL;

	IF sn IS NOT NULL THEN
		SELECT 
			comp.* INTO c
		FROM
			component comp JOIN
			asset a USING (component_id)
		WHERE
			comp.component_type_id = ctid AND
			a.serial_number = sn;

		IF FOUND THEN
			return c;
		END IF;
	END IF;

	IF no_create THEN
		RETURN NULL;
	END IF;

	INSERT INTO jazzhands.component (
		component_type_id
	) VALUES (
		ctid
	) RETURNING * INTO c;

	IF serial_number IS NOT NULL THEN
		INSERT INTO asset (
			component_id,
			serial_number,
			ownership_status
		) VALUES (
			c.component_id,
			serial_number,
			os
		);
	END IF;

	RETURN c;
END;
$function$
;

--
-- Process drops in snapshot_manip
--
--
-- Process drops in lv_manip
--
--
-- Process drops in approval_utils
--
--
-- Process drops in account_collection_manip
--
--
-- Process drops in script_hooks
--
--
-- Process drops in schema_support
--
-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'begin_maintenance');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.begin_maintenance ( shouldbesuper boolean );
CREATE OR REPLACE FUNCTION schema_support.begin_maintenance(shouldbesuper boolean DEFAULT true)
 RETURNS boolean
 LANGUAGE plpgsql
AS $function$
DECLARE
	issuper	boolean;
	_tally	integer;
BEGIN
	IF shouldbesuper THEN
		SELECT usesuper INTO issuper FROM pg_user where usename = current_user;
		IF issuper IS false THEN
			RAISE EXCEPTION 'User must be a super user.';
		END IF;
	END IF;
	-- Not sure how reliable this is.
	-- http://www.postgresql.org/docs/9.3/static/monitoring-stats.html
	SELECT count(*)
	  INTO _tally
	  FROM	pg_stat_activity
	 WHERE	pid = pg_backend_pid()
	   AND	query_start = xact_start;
	IF _tally > 0 THEN
		RAISE EXCEPTION 'Must run maintenance in a transaction.';
	END IF;
	RETURN true;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'build_audit_table');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.build_audit_table ( aud_schema character varying, tbl_schema character varying, table_name character varying, first_time boolean );
CREATE OR REPLACE FUNCTION schema_support.build_audit_table(aud_schema character varying, tbl_schema character varying, table_name character varying, first_time boolean DEFAULT true)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	keys	RECORD;
BEGIN
    IF first_time THEN
	EXECUTE 'CREATE SEQUENCE ' || quote_ident(aud_schema) || '.'
	    || quote_ident(table_name || '_seq');
    END IF;

    EXECUTE 'CREATE TABLE ' || quote_ident(aud_schema) || '.'
	|| quote_ident(table_name) || ' AS '
	|| 'SELECT *, NULL::char(3) as "aud#action", now() as "aud#timestamp", '
	|| 'clock_timestamp() as "aud#realtime", '
	|| 'txid_current() as "aud#txid", '
	|| 'NULL::varchar(255) AS "aud#user", NULL::integer AS "aud#seq" '
	|| 'FROM ' || quote_ident(tbl_schema) || '.' || quote_ident(table_name)
	|| ' LIMIT 0';

    EXECUTE 'ALTER TABLE ' || quote_ident(aud_schema) || '.'
	|| quote_ident(table_name)
	|| $$ ALTER COLUMN "aud#seq" SET NOT NULL, $$
	|| $$ ALTER COLUMN "aud#seq" SET DEFAULT nextval('$$
	|| quote_ident(aud_schema) || '.' || quote_ident(table_name || '_seq')
	|| $$')$$;

    EXECUTE 'CREATE INDEX '
	|| quote_ident( table_name || '_aud#timestamp_idx')
	|| ' ON ' || quote_ident(aud_schema) || '.'
	|| quote_ident(table_name) || '("aud#timestamp")';

	EXECUTE 'ALTER TABLE ' || quote_ident(aud_schema) || '.'
		|| quote_ident( table_name )
		|| ' ADD PRIMARY KEY ("aud#seq")';

	-- one day, I will want to construct the list of columns by hand rather
	-- than use pg_get_constraintdef.  watch me...
	FOR keys IN
		SELECT con.conname, c2.relname as index_name,
			pg_catalog.pg_get_constraintdef(con.oid, true) as condef,
				regexp_replace(
			pg_catalog.pg_get_constraintdef(con.oid, true),
					'^.*(\([^\)]+\)).*$', '\1') as cols,
			con.condeferrable,
			con.condeferred
		FROM pg_catalog.pg_class c
			INNER JOIN pg_namespace n
				ON relnamespace = n.oid
			INNER JOIN pg_catalog.pg_index i
				ON c.oid = i.indrelid
			INNER JOIN pg_catalog.pg_class c2
				ON i.indexrelid = c2.oid
			INNER JOIN pg_catalog.pg_constraint con ON
				(con.conrelid = i.indrelid
				AND con.conindid = i.indexrelid )
		WHERE c.relname =  table_name
		AND     n.nspname = tbl_schema
		AND con.contype in ('p', 'u')
	LOOP
		EXECUTE 'CREATE INDEX '
			|| 'aud_' || quote_ident( table_name || '_' || keys.conname)
			|| ' ON ' || quote_ident(aud_schema) || '.'
			|| quote_ident(table_name) || keys.cols;
	END LOOP;

    IF first_time THEN
		PERFORM schema_support.rebuild_audit_trigger
			( aud_schema, tbl_schema, table_name );
    END IF;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'build_audit_tables');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.build_audit_tables ( aud_schema character varying, tbl_schema character varying );
CREATE OR REPLACE FUNCTION schema_support.build_audit_tables(aud_schema character varying, tbl_schema character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
     table_list RECORD;
BEGIN
    FOR table_list IN
	SELECT table_name FROM information_schema.tables
	WHERE table_type = 'BASE TABLE' AND table_schema = tbl_schema
	AND NOT (
	    table_name IN (
		SELECT table_name FROM information_schema.tables
		WHERE table_schema = aud_schema
	    )
	)
	ORDER BY table_name
    LOOP
	PERFORM schema_support.build_audit_table
	    ( aud_schema, tbl_schema, table_list.table_name );
    END LOOP;

    PERFORM schema_support.rebuild_audit_triggers(aud_schema, tbl_schema);
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'get_common_columns');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.get_common_columns ( _schema text, _table1 text, _table2 text );
CREATE OR REPLACE FUNCTION schema_support.get_common_columns(_schema text, _table1 text, _table2 text)
 RETURNS text[]
 LANGUAGE plpgsql
AS $function$
DECLARE
	_q			text;
    cols        text[];
BEGIN
    _q := 'WITH cols AS (
        SELECT  n.nspname as schema, c.relname as relation, a.attname as colname,
		a.attnum
            FROM    pg_catalog.pg_attribute a
                INNER JOIN pg_catalog.pg_class c
                    on a.attrelid = c.oid
                INNER JOIN pg_catalog.pg_namespace n
                    on c.relnamespace = n.oid
            WHERE   a.attnum > 0
            AND   NOT a.attisdropped
            ORDER BY a.attnum
       ) SELECT array_agg(colname ORDER BY o.attnum) as cols
        FROM cols  o
            INNER JOIN cols n USING (schema, colname)
		WHERE
			o.schema = $1
		and o.relation = $2
		and n.relation =$3
	';
	EXECUTE _q INTO cols USING _schema, _table1, _table2;
	RETURN cols;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'rebuild_audit_trigger');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.rebuild_audit_trigger ( aud_schema character varying, tbl_schema character varying, table_name character varying );
CREATE OR REPLACE FUNCTION schema_support.rebuild_audit_trigger(aud_schema character varying, tbl_schema character varying, table_name character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
BEGIN
    EXECUTE 'CREATE OR REPLACE FUNCTION ' || quote_ident(tbl_schema)
	|| '.' || quote_ident('perform_audit_' || table_name)
	|| $ZZ$() RETURNS TRIGGER AS $TQ$
	    DECLARE
		appuser VARCHAR;
	    BEGIN
		BEGIN
		    appuser := session_user
			|| '/' || current_setting('jazzhands.appuser');
		EXCEPTION WHEN OTHERS THEN
		    appuser := session_user;
		END;

    		appuser = substr(appuser, 1, 255);

		IF TG_OP = 'DELETE' THEN
		    INSERT INTO $ZZ$ || quote_ident(aud_schema)
			|| '.' || quote_ident(table_name) || $ZZ$
		    VALUES ( OLD.*, 'DEL', now(),
			clock_timestamp(), txid_current(), appuser );
		    RETURN OLD;
		ELSIF TG_OP = 'UPDATE' THEN
		    INSERT INTO $ZZ$ || quote_ident(aud_schema)
			|| '.' || quote_ident(table_name) || $ZZ$
		    VALUES ( NEW.*, 'UPD', now(),
			clock_timestamp(), txid_current(), appuser );
		    RETURN NEW;
		ELSIF TG_OP = 'INSERT' THEN
		    INSERT INTO $ZZ$ || quote_ident(aud_schema)
			|| '.' || quote_ident(table_name) || $ZZ$
		    VALUES ( NEW.*, 'INS', now(),
			clock_timestamp(), txid_current(), appuser );
		    RETURN NEW;
		END IF;
		RETURN NULL;
	    END;
	$TQ$ LANGUAGE plpgsql SECURITY DEFINER
    $ZZ$;

    EXECUTE 'DROP TRIGGER IF EXISTS ' || quote_ident('trigger_audit_'
	|| table_name) || ' ON ' || quote_ident(tbl_schema) || '.'
	|| quote_ident(table_name);

    EXECUTE 'CREATE TRIGGER ' || quote_ident('trigger_audit_' || table_name)
	|| ' AFTER INSERT OR UPDATE OR DELETE ON ' || quote_ident(tbl_schema)
	|| '.' || quote_ident(table_name) || ' FOR EACH ROW EXECUTE PROCEDURE '
	|| quote_ident(tbl_schema) || '.' || quote_ident('perform_audit_'
	|| table_name) || '()';
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'rebuild_stamp_triggers');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.rebuild_stamp_triggers ( tbl_schema character varying );
CREATE OR REPLACE FUNCTION schema_support.rebuild_stamp_triggers(tbl_schema character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
BEGIN
    DECLARE
	tab RECORD;
    BEGIN
	FOR tab IN
	    SELECT table_name FROM information_schema.tables
	    WHERE table_schema = tbl_schema AND table_type = 'BASE TABLE'
	    AND table_name NOT LIKE 'aud$%'
	LOOP
	    PERFORM schema_support.rebuild_stamp_trigger
		(tbl_schema, tab.table_name);
	END LOOP;
    END;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'relation_diff');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.relation_diff ( schema text, old_rel text, new_rel text, key_relation text, prikeys text[], raise_exception boolean );
CREATE OR REPLACE FUNCTION schema_support.relation_diff(schema text, old_rel text, new_rel text, key_relation text DEFAULT NULL::text, prikeys text[] DEFAULT NULL::text[], raise_exception boolean DEFAULT true)
 RETURNS boolean
 LANGUAGE plpgsql
AS $function$
DECLARE
	_or	RECORD;
	_nr	RECORD;
	_t1	integer;
	_t2	integer;
	_cols TEXT[];
	_q TEXT;
	_f TEXT;
	_c RECORD;
	_w TEXT[];
	_ctl TEXT[];
	_rv	boolean;
BEGIN
	-- do a simple row count
	EXECUTE 'SELECT count(*) FROM ' || schema || '."' || old_rel || '"' INTO _t1;
	EXECUTE 'SELECT count(*) FROM ' || schema || '."' || new_rel || '"' INTO _t2;

	_rv := true;

	IF _t1 IS NULL THEN
		RAISE NOTICE 'table %.% does not seem to exist', schema, old_rel;
		_rv := false;
	END IF;
	IF _t2 IS NULL THEN
		RAISE NOTICE 'table %.% does not seem to exist', schema, new_rel;
		_rv := false;
	END IF;

	IF _t1 != _t2 THEN
		RAISE NOTICE 'table % has % rows; table % has % rows', old_rel, _t1, new_rel, _t2;
		_rv := false;
	END IF;

	IF NOT _rv THEN
		IF raise_exception THEN
			RAISE EXCEPTION 'Relations do not match';
		END IF;
		RETURN false;
	END IF;

	IF prikeys IS NULL THEN
		-- read into prikeys the primary key for the table
		IF key_relation IS NULL THEN
			key_relation := old_rel;
		END IF;
		prikeys := schema_support.get_pk_columns(schema, key_relation);
	END IF;

	-- read into _cols the column list in common between old_rel and new_rel
	_cols := schema_support.get_common_columns(schema, old_rel, new_rel);

	FOREACH _f IN ARRAY _cols
	LOOP
		SELECT array_append(_ctl,
			quote_ident(_f) || '::text') INTO _ctl;
	END LOOP;

	_cols := _ctl;

	_q := 'SELECT '|| array_to_string(_cols,',') ||' FROM ' || quote_ident(schema) || '.' ||
		quote_ident(old_rel);

	FOR _or IN EXECUTE _q
	LOOP
		_w = NULL;
		FOREACH _f IN ARRAY prikeys
		LOOP
			FOR _c IN SELECT * FROM json_each_text( row_to_json(_or) )
			LOOP
				IF _c.key = _f THEN
					SELECT array_append(_w,
						quote_ident(_f) || '::text = ' || quote_literal(_c.value))
					INTO _w;
				END IF;
			END LOOP;
		END LOOP;
		_q := 'SELECT ' || array_to_string(_cols,',') ||
			' FROM ' || quote_ident(schema) || '.' ||
			quote_ident(new_rel) || ' WHERE ' ||
			array_to_string(_w, ' AND ' );
		EXECUTE _q INTO _nr;

		IF _or != _nr THEN
			RAISE NOTICE 'mismatched row:';
			RAISE NOTICE 'OLD: %', row_to_json(_or);
			RAISE NOTICE 'NEW: %', row_to_json(_nr);
			_rv := false;
		END IF;

	END LOOP;

	IF NOT _rv AND raise_exception THEN
		RAISE EXCEPTION 'Relations do not match';
	END IF;
	return _rv;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'replay_object_recreates');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.replay_object_recreates ( beverbose boolean );
CREATE OR REPLACE FUNCTION schema_support.replay_object_recreates(beverbose boolean DEFAULT false)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_r		RECORD;
	_tally	integer;
BEGIN
	SELECT	count(*)
	  INTO	_tally
	  FROM	pg_catalog.pg_class
	 WHERE	relname = '__recreate'
	   AND	relpersistence = 't';

	IF _tally > 0 THEN
		FOR _r in SELECT * from __recreate ORDER BY id DESC FOR UPDATE
		LOOP
			IF beverbose THEN
				RAISE NOTICE 'Regrant: %.%', _r.schema, _r.object;
			END IF;
			EXECUTE _r.ddl;
			IF _r.owner is not NULL THEN
				IF _r.type = 'view' THEN
					EXECUTE 'ALTER VIEW ' || _r.schema || '.' || _r.object ||
						' OWNER TO ' || _r.owner || ';';
				ELSIF _r.type = 'function' THEN
					EXECUTE 'ALTER FUNCTION ' || _r.schema || '.' || _r.object ||
						'(' || _r.idargs || ') OWNER TO ' || _r.owner || ';';
				ELSE
					RAISE EXCEPTION 'Unable to restore grant for %', _r;
				END IF;
			END IF;
			DELETE from __recreate where id = _r.id;
		END LOOP;

		SELECT count(*) INTO _tally from __recreate;
		IF _tally > 0 THEN
			RAISE EXCEPTION '% objects still exist for recreating after a complete loop', _tally;
		ELSE
			DROP TABLE __recreate;
		END IF;
	ELSE
		IF beverbose THEN
			RAISE NOTICE '**** WARNING: replay_object_recreates did NOT have anything to regrant!';
		END IF;
	END IF;

END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'replay_saved_grants');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.replay_saved_grants ( beverbose boolean );
CREATE OR REPLACE FUNCTION schema_support.replay_saved_grants(beverbose boolean DEFAULT false)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_r		RECORD;
	_tally	integer;
BEGIN
	 SELECT  count(*)
      INTO  _tally
      FROM  pg_catalog.pg_class
     WHERE  relname = '__regrants'
       AND  relpersistence = 't';

	IF _tally > 0 THEN
	    FOR _r in SELECT * from __regrants FOR UPDATE
	    LOOP
		    IF beverbose THEN
			    RAISE NOTICE 'Regrant Executing: %', _r.regrant;
		    END IF;
		    EXECUTE _r.regrant;
		    DELETE from __regrants where id = _r.id;
	    END LOOP;

	    SELECT count(*) INTO _tally from __regrants;
	    IF _tally > 0 THEN
		    RAISE EXCEPTION 'Grant extractions were run while replaying grants - %.', _tally;
	    ELSE
		    DROP TABLE __regrants;
	    END IF;
	ELSE
		IF beverbose THEN
			RAISE NOTICE '**** WARNING: replay_saved_grants did NOT have anything to regrant!';
		END IF;
	END IF;

END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'retrieve_functions');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.retrieve_functions ( schema character varying, object character varying, dropit boolean );
CREATE OR REPLACE FUNCTION schema_support.retrieve_functions(schema character varying, object character varying, dropit boolean DEFAULT false)
 RETURNS text[]
 LANGUAGE plpgsql
AS $function$
DECLARE
	_r		RECORD;
	_fn		TEXT;
	_cmd	TEXT;
	_rv		TEXT[];
BEGIN
	FOR _r IN SELECT n.nspname, p.proname,
				coalesce(u.usename, 'public') as owner,
				pg_get_functiondef(p.oid) as funcdef,
				pg_get_function_identity_arguments(p.oid) as idargs
		FROM    pg_catalog.pg_proc  p
				INNER JOIN pg_catalog.pg_namespace n on n.oid = p.pronamespace
				INNER JOIN pg_catalog.pg_language l on l.oid = p.prolang
				INNER JOIN pg_catalog.pg_user u on u.usesysid = p.proowner
		WHERE   n.nspname = schema
		  AND	p.proname = object
	LOOP
		_fn = _r.nspname || '.' || _r.proname || '(' || _r.idargs || ')';
		_rv = _rv || _fn;

		IF dropit  THEN
			_cmd = 'DROP FUNCTION ' || _fn || ';';
			EXECUTE _cmd;
		END IF;
	END LOOP;
	RETURN _rv;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'save_constraint_for_replay');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.save_constraint_for_replay ( schema character varying, object character varying, dropit boolean );
CREATE OR REPLACE FUNCTION schema_support.save_constraint_for_replay(schema character varying, object character varying, dropit boolean DEFAULT true)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_r		RECORD;
	_cmd	TEXT;
	_ddl	TEXT;
BEGIN
	PERFORM schema_support.prepare_for_object_replay();

	FOR _r in 	SELECT n.nspname, c.relname, con.conname,
				pg_get_constraintdef(con.oid, true) as def
		FROM pg_constraint con
			INNER JOIN pg_class c on (c.relnamespace, c.oid) =
				(con.connamespace, con.conrelid)
			INNER JOIN pg_namespace n on n.oid = c.relnamespace
		WHERE con.confrelid in (
			select c.oid
			from pg_class c
				inner join pg_namespace n on n.oid = c.relnamespace
			WHERE c.relname = object
			AND n.nspname = schema
		) AND n.nspname != schema
	LOOP
		_ddl := 'ALTER TABLE ' || _r.nspname || '.' || _r.relname ||
			' ADD CONSTRAINT ' || _r.conname || ' ' || _r.def;
		IF _ddl is NULL THEN
			RAISE EXCEPTION 'Unable to define constraint for %', _r;
		END IF;
		INSERT INTO __recreate (schema, object, type, ddl )
			VALUES (
				_r.nspname, _r.relname, 'constraint', _ddl
			);
		IF dropit  THEN
			_cmd = 'ALTER TABLE ' || _r.nspname || '.' || _r.relname ||
				' DROP CONSTRAINT ' || _r.conname || ';';
			EXECUTE _cmd;
		END IF;
	END LOOP;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'save_dependent_objects_for_replay');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.save_dependent_objects_for_replay ( schema character varying, object character varying, dropit boolean, doobjectdeps boolean );
CREATE OR REPLACE FUNCTION schema_support.save_dependent_objects_for_replay(schema character varying, object character varying, dropit boolean DEFAULT true, doobjectdeps boolean DEFAULT false)
 RETURNS void
 LANGUAGE plpgsql
 SET search_path TO schema_support
AS $function$

DECLARE
	_r		RECORD;
	_cmd	TEXT;
	_ddl	TEXT;
BEGIN
	RAISE DEBUG 'processing %.%', schema, object;
	-- process stored procedures
	FOR _r in SELECT  distinct np.nspname::text, dependent.proname::text
		FROM   pg_depend dep
			INNER join pg_type dependee on dependee.oid = dep.refobjid
			INNER join pg_namespace n on n.oid = dependee.typnamespace
			INNER join pg_proc dependent on dependent.oid = dep.objid
			INNER join pg_namespace np on np.oid = dependent.pronamespace
			WHERE   dependee.typname = object
			  AND	  n.nspname = schema
	LOOP
		-- RAISE NOTICE '1 dealing with  %.%', _r.nspname, _r.proname;
		PERFORM schema_support.save_constraint_for_replay(_r.nspname, _r.proname, dropit);
		PERFORM schema_support.save_dependent_objects_for_replay(_r.nspname, _r.proname, dropit);
		PERFORM schema_support.save_function_for_replay(_r.nspname, _r.proname, dropit);
	END LOOP;

	-- save any triggers on the view
	FOR _r in SELECT distinct n.nspname::text, dependee.relname::text, dependee.relkind
		FROM pg_depend
		JOIN pg_rewrite ON pg_depend.objid = pg_rewrite.oid
		JOIN pg_class as dependee ON pg_rewrite.ev_class = dependee.oid
		JOIN pg_class as dependent ON pg_depend.refobjid = dependent.oid
		JOIN pg_namespace n on n.oid = dependee.relnamespace
		JOIN pg_namespace sn on sn.oid = dependent.relnamespace
		JOIN pg_attribute ON pg_depend.refobjid = pg_attribute.attrelid
   			AND pg_depend.refobjsubid = pg_attribute.attnum
		WHERE dependent.relname = object
  		AND sn.nspname = schema
	LOOP
		IF _r.relkind = 'v' THEN
			-- RAISE NOTICE '2 dealing with  %.%', _r.nspname, _r.relname;
			PERFORM * FROM save_dependent_objects_for_replay(_r.nspname, _r.relname, dropit);
			PERFORM schema_support.save_view_for_replay(_r.nspname, _r.relname, dropit);
		END IF;
	END LOOP;
	IF doobjectdeps THEN
		PERFORM schema_support.save_trigger_for_replay(schema, object, dropit);
		PERFORM schema_support.save_constraint_for_replay('jazzhands', 'table');
	END IF;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'save_function_for_replay');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.save_function_for_replay ( schema character varying, object character varying, dropit boolean );
CREATE OR REPLACE FUNCTION schema_support.save_function_for_replay(schema character varying, object character varying, dropit boolean DEFAULT true)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_r		RECORD;
	_cmd	TEXT;
BEGIN
	PERFORM schema_support.prepare_for_object_replay();

	-- implicitly save regrants
	PERFORM schema_support.save_grants_for_replay(schema, object);
	FOR _r IN SELECT n.nspname, p.proname,
				coalesce(u.usename, 'public') as owner,
				pg_get_functiondef(p.oid) as funcdef,
				pg_get_function_identity_arguments(p.oid) as idargs
		FROM    pg_catalog.pg_proc  p
				INNER JOIN pg_catalog.pg_namespace n on n.oid = p.pronamespace
				INNER JOIN pg_catalog.pg_language l on l.oid = p.prolang
				INNER JOIN pg_catalog.pg_user u on u.usesysid = p.proowner
		WHERE   n.nspname = schema
		  AND	p.proname = object
	LOOP
		INSERT INTO __recreate (schema, object, type, owner, ddl, idargs )
		VALUES (
			_r.nspname, _r.proname, 'function', _r.owner, _r.funcdef, _r.idargs
		);
		IF dropit  THEN
			_cmd = 'DROP FUNCTION ' || _r.nspname || '.' ||
				_r.proname || '(' || _r.idargs || ');';
			EXECUTE _cmd;
		END IF;

	END LOOP;

END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'save_grants_for_replay_functions');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.save_grants_for_replay_functions ( schema character varying, object character varying, newname character varying );
CREATE OR REPLACE FUNCTION schema_support.save_grants_for_replay_functions(schema character varying, object character varying, newname character varying DEFAULT NULL::character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_schema		varchar;
	_object		varchar;
	_procs		RECORD;
	_perm		RECORD;
	_grant		varchar;
	_role		varchar;
	_fullgrant		varchar;
BEGIN
	_schema := schema;
	_object := object;
	if newname IS NULL THEN
		newname := _object;
	END IF;
	PERFORM schema_support.prepare_for_grant_replay();
	FOR _procs IN SELECT  n.nspname as schema, p.proname,
			pg_get_function_identity_arguments(p.oid) as args,
			proacl as privs
		FROM    pg_catalog.pg_proc  p
				inner join pg_catalog.pg_namespace n on n.oid = p.pronamespace
		WHERE   n.nspname = _schema
	 	 AND    p.proname = _object
	LOOP
		-- NOTE:  We lose who granted it.  Oh Well.
		FOR _perm IN SELECT * FROM pg_catalog.aclexplode(acl := _procs.privs)
		LOOP
			--  grantor | grantee | privilege_type | is_grantable
			IF _perm.is_grantable THEN
				_grant = ' WITH GRANT OPTION';
			ELSE
				_grant = '';
			END IF;
			IF _perm.grantee = 0 THEN
				_role := 'PUBLIC';
			ELSE
				_role := pg_get_userbyid(_perm.grantee);
			END IF;
			_fullgrant := 'GRANT ' ||
				_perm.privilege_type || ' on FUNCTION ' ||
				_schema || '.' ||
				newname || '(' || _procs.args || ')  to ' ||
				_role || _grant;
			-- RAISE DEBUG 'inserting % for %', _fullgrant, _perm;
			INSERT INTO __regrants (schema, object, newname, regrant) values (schema,object, newname, _fullgrant );
		END LOOP;
	END LOOP;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'save_grants_for_replay_relations');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.save_grants_for_replay_relations ( schema character varying, object character varying, newname character varying );
CREATE OR REPLACE FUNCTION schema_support.save_grants_for_replay_relations(schema character varying, object character varying, newname character varying DEFAULT NULL::character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_schema		varchar;
	_object	varchar;
	_tabs		RECORD;
	_perm		RECORD;
	_grant		varchar;
	_fullgrant		varchar;
	_role		varchar;
BEGIN
	_schema := schema;
	_object := object;
	if newname IS NULL THEN
		newname := _object;
	END IF;
	PERFORM schema_support.prepare_for_grant_replay();

	-- Handle table wide grants
	FOR _tabs IN SELECT  n.nspname as schema,
			c.relname as name,
			CASE c.relkind
				WHEN 'r' THEN 'table'
				WHEN 'v' THEN 'view'
				WHEN 'S' THEN 'sequence'
				WHEN 'f' THEN 'foreign table'
				END as "Type",
			c.relacl as privs
		FROM    pg_catalog.pg_class c
			INNER JOIN pg_catalog.pg_namespace n
				ON n.oid = c.relnamespace
		WHERE c.relkind IN ('r', 'v', 'S', 'f')
		  AND c.relname = _object
		  AND n.nspname = _schema
		ORDER BY 1, 2
	LOOP
		-- NOTE:  We lose who granted it.  Oh Well.
		FOR _perm IN SELECT * FROM pg_catalog.aclexplode(acl := _tabs.privs)
		LOOP
			--  grantor | grantee | privilege_type | is_grantable
			IF _perm.is_grantable THEN
				_grant = ' WITH GRANT OPTION';
			ELSE
				_grant = '';
			END IF;
			IF _perm.grantee = 0 THEN
				_role := 'PUBLIC';
			ELSE
				_role := pg_get_userbyid(_perm.grantee);
			END IF;
			_fullgrant := 'GRANT ' ||
				_perm.privilege_type || ' on ' ||
				_schema || '.' ||
				newname || ' to ' ||
				_role || _grant;
			IF _fullgrant IS NULL THEN
				RAISE EXCEPTION 'built up grant for %.% (%) is NULL',
					schema, object, newname;
	    END IF;
			INSERT INTO __regrants (schema, object, newname, regrant) values (schema,object, newname, _fullgrant );
		END LOOP;
	END LOOP;

	-- Handle column specific wide grants
	FOR _tabs IN SELECT  n.nspname as schema,
			c.relname as name,
			CASE c.relkind
				WHEN 'r' THEN 'table'
				WHEN 'v' THEN 'view'
				WHEN 'S' THEN 'sequence'
				WHEN 'f' THEN 'foreign table'
				END as "Type",
			a.attname as col,
			a.attacl as privs
		FROM    pg_catalog.pg_class c
			INNER JOIN pg_catalog.pg_namespace n
				ON n.oid = c.relnamespace
			INNER JOIN pg_attribute a
                ON a.attrelid = c.oid
		WHERE c.relkind IN ('r', 'v', 'S', 'f')
		  AND a.attacl IS NOT NULL
		  AND c.relname = _object
		  AND n.nspname = _schema
		ORDER BY 1, 2
	LOOP
		-- NOTE:  We lose who granted it.  Oh Well.
		FOR _perm IN SELECT * FROM pg_catalog.aclexplode(acl := _tabs.privs)
		LOOP
			--  grantor | grantee | privilege_type | is_grantable
			IF _perm.is_grantable THEN
				_grant = ' WITH GRANT OPTION';
			ELSE
				_grant = '';
			END IF;
			IF _perm.grantee = 0 THEN
				_role := 'PUBLIC';
			ELSE
				_role := pg_get_userbyid(_perm.grantee);
			END IF;
			_fullgrant := 'GRANT ' ||
				_perm.privilege_type || '(' || _tabs.col || ')'
				' on ' ||
				_schema || '.' ||
				newname || ' to ' ||
				_role || _grant;
			IF _fullgrant IS NULL THEN
				RAISE EXCEPTION 'built up grant for %.% (%) is NULL',
					schema, object, newname;
	    END IF;
			INSERT INTO __regrants (schema, object, newname, regrant) values (schema,object, newname, _fullgrant );
		END LOOP;
	END LOOP;

END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'save_trigger_for_replay');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.save_trigger_for_replay ( schema character varying, object character varying, dropit boolean );
CREATE OR REPLACE FUNCTION schema_support.save_trigger_for_replay(schema character varying, object character varying, dropit boolean DEFAULT true)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	_r		RECORD;
	_cmd	TEXT;
BEGIN
	PERFORM schema_support.prepare_for_object_replay();

	FOR _r in
		SELECT n.nspname, c.relname, trg.tgname,
				pg_get_triggerdef(trg.oid, true) as def
		FROM pg_trigger trg
			INNER JOIN pg_class c on trg.tgrelid =  c.oid
			INNER JOIN pg_namespace n on n.oid = c.relnamespace
		WHERE n.nspname = schema and c.relname = object
	LOOP
		INSERT INTO __recreate (schema, object, type, ddl )
			VALUES (
				_r.nspname, _r.relname, 'trigger', _r.def
			);
		IF dropit  THEN
			_cmd = 'DROP TRIGGER ' || _r.tgname || ' ON ' ||
				_r.nspname || '.' || _r.relname || ';';
			EXECUTE _cmd;
		END IF;
	END LOOP;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'undo_audit_row');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.undo_audit_row ( in_table text, in_audit_schema text, in_schema text, in_start_time timestamp without time zone, in_end_time timestamp without time zone, in_aud_user text, in_audit_ids integer[] );
CREATE OR REPLACE FUNCTION schema_support.undo_audit_row(in_table text, in_audit_schema text DEFAULT 'audit'::text, in_schema text DEFAULT 'jazzhands'::text, in_start_time timestamp without time zone DEFAULT NULL::timestamp without time zone, in_end_time timestamp without time zone DEFAULT NULL::timestamp without time zone, in_aud_user text DEFAULT NULL::text, in_audit_ids integer[] DEFAULT NULL::integer[])
 RETURNS integer
 LANGUAGE plpgsql
AS $function$
DECLARE
	tally	integer;
	pks		text[];
	cols	text[];
	q		text;
	val		text;
	x		text;
	_whcl	text;
	_eq		text;
	setstr	text;
	_r		record;
	_c		record;
	_br		record;
	_vals	text[];
BEGIN
	tally := 0;
	pks := schema_support.get_pk_columns(in_schema, in_table);
	cols := schema_support.get_columns(in_schema, in_table);
	q = '';
	IF in_start_time is not NULL THEN
		IF q = '' THEN
			q := q || 'WHERE ';
		ELSE
			q := q || 'AND ';
		END IF;
		q := q || quote_ident('aud#timestamp') || ' >= ' || quote_literal(in_start_time);
	END IF;
	IF in_end_time is not NULL THEN
		IF q = '' THEN
			q := q || 'WHERE ';
		ELSE
			q := q || 'AND ';
		END IF;
		q := q || quote_ident('aud#timestamp') || ' <= ' || quote_literal(in_end_time);
	END IF;
	IF in_aud_user is not NULL THEN
		IF q = '' THEN
			q := q || 'WHERE ';
		ELSE
			q := q || 'AND ';
		END IF;
		q := q || quote_ident('aud#user') || ' = ' || quote_literal(in_aud_user);
	END IF;
	IF in_audit_ids is not NULL THEN
		IF q = '' THEN
			q := q || 'WHERE ';
		ELSE
			q := q || 'AND ';
		END IF;
		q := q || quote_ident('aud#seq') || ' IN ( ' ||
			array_to_string(in_audit_ids, ',') || ')';
	END IF;

	-- Iterate over all the rows that need to be replayed
	q := 'SELECT * from ' || quote_ident(in_audit_schema) || '.' ||
			quote_ident(in_table) || ' ' || q || ' ORDER BY "aud#seq" desc';
	FOR _r IN EXECUTE q
	LOOP
		IF _r."aud#action" = 'DEL' THEN
			-- Build up a list of rows that need to be inserted
			_vals = NULL;
			FOR _c IN SELECT * FROM json_each_text( row_to_json(_r) )
			LOOP
				IF _c.key !~ 'data|aud' THEN
					IF _c.value IS NULL THEN
						SELECT array_append(_vals, 'NULL') INTO _vals;
					ELSE
						SELECT array_append(_vals, quote_literal(_c.value)) INTO _vals;
					END IF;
				END IF;
			END LOOP;
			_eq := 'INSERT INTO ' || quote_ident(in_schema) || '.' ||
				quote_ident(in_table) || ' ( ' ||
				array_to_string(
					schema_support.quote_ident_array(cols), ',') ||
					') VALUES (' ||  array_to_string(_vals, ',', NULL) || ')';
		ELSIF _r."aud#action" in ('INS', 'UPD') THEN
			-- Build up a where clause for this table to get a unique row
			-- based on the primary key
			FOREACH x IN ARRAY pks
			LOOP
				_whcl := '';
				FOR _c IN SELECT * FROM json_each_text( row_to_json(_r) )
				LOOP
					IF _c.key = x THEN
						IF _whcl != '' THEN
							_whcl := _whcl || ', ';
						END IF;
						IF _c.value IS NULL THEN
							_whcl = _whcl || quote_ident(_c.key) || ' = NULL ';
						ELSE
							_whcl = _whcl || quote_ident(_c.key) || ' =  ' ||
								quote_nullable(_c.value);
						END IF;
					END IF;
				END LOOP;
			END LOOP;

			IF _r."aud#action" = 'INS' THEN
				_eq := 'DELETE FROM ' || quote_ident(in_schema) || '.' ||
					quote_ident(in_table) || ' WHERE ' || _whcl;
			ELSIF _r."aud#action" = 'UPD' THEN
				-- figure out what rows have changed and do an update if
				-- they have.  NOTE:  This may result in no change being
				-- replayed if a row did not actually change
				setstr = '';
				FOR _c IN SELECT * FROM json_each_text( row_to_json(_r) )
				LOOP
					--
					-- Iterate over all the columns and if they have changed,
					-- then build an update statement
					--
					IF _c.key !~ 'aud#|data_(ins|upd)_(user|date)' THEN
						EXECUTE 'SELECT ' || _c.key || ' FROM ' ||
							quote_ident(in_schema) || '.' ||
								quote_ident(in_table)  ||
							' WHERE ' || _whcl
							INTO val;
						IF ( _c.value IS NULL  AND val IS NOT NULL) OR
							( _c.value IS NOT NULL AND val IS NULL) OR
							(_c.value::text NOT SIMILAR TO val::text) THEN
							IF char_length(setstr) > 0 THEN
								setstr = setstr || ',
								';
							END IF;
							IF _c.value IS NOT  NULL THEN
								setstr = setstr || _c.key || ' = ' ||
									quote_nullable(_c.value) || ' ' ;
							ELSE
								setstr = setstr || _c.key || ' = ' ||
									' NULL ' ;
							END IF;
						END IF;
					END IF;
				END LOOP;
				IF char_length(setstr) > 0 THEN
					_eq := 'UPDATE ' || quote_ident(in_schema) || '.' ||
						quote_ident(in_table) ||
						' SET ' || setstr || ' WHERE ' || _whcl;
				END IF;
			END IF;
		END IF;
		IF _eq IS NOT NULL THEN
			tally := tally + 1;
			RAISE NOTICE '%', _eq;
			EXECUTE _eq;
		END IF;
	END LOOP;
	RETURN tally;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION schema_support.mv_last_updated(relation text, schema text DEFAULT 'jazzhands'::text, debug boolean DEFAULT false)
 RETURNS timestamp without time zone
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO schema_support
AS $function$
DECLARE
	rv	timestamp;
BEGIN
	IF debug THEN
		RAISE NOTICE 'selecting for update...';
	END IF;

	SELECT	refresh
	INTO	rv
	FROM	schema_support.mv_refresh r
	WHERE	r.schema = mv_last_updated.schema
	AND	r.view = relation
	FOR UPDATE;

	IF debug THEN
		RAISE NOTICE 'returning %', rv;
	END IF;

	RETURN rv;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION schema_support.rebuild_audit_table(aud_schema character varying, tbl_schema character varying, table_name character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
	idx	 text[];
	keys text[];
	cols text[];
	i	text;
BEGIN
	-- rename all the old indexes and constraints on the old audit table
	SELECT	array_agg(c2.relname)
		INTO	 idx
		  FROM	pg_catalog.pg_index i
			LEFT JOIN pg_catalog.pg_class c
				ON c.oid = i.indrelid
			LEFT JOIN pg_catalog.pg_class c2
				ON i.indexrelid = c2.oid
			LEFT JOIN pg_catalog.pg_namespace n
				ON c2.relnamespace = n.oid
			LEFT JOIN pg_catalog.pg_constraint con
				ON (conrelid = i.indrelid
				AND conindid = i.indexrelid
				AND contype IN ('p','u','x'))
		 WHERE n.nspname = quote_ident(aud_schema)
		  AND	c.relname = quote_ident(table_name)
		  AND	contype is NULL
	;

	SELECT array_agg(con.conname)
	INTO	keys
    FROM pg_catalog.pg_class c
		INNER JOIN pg_namespace n
			ON relnamespace = n.oid
		INNER JOIN pg_catalog.pg_index i
			ON c.oid = i.indrelid
		INNER JOIN pg_catalog.pg_class c2
			ON i.indexrelid = c2.oid
		INNER JOIN pg_catalog.pg_constraint con ON
			(con.conrelid = i.indrelid
			AND con.conindid = i.indexrelid )
	WHERE  	n.nspname = quote_ident(aud_schema)
	AND		c.relname = quote_ident(table_name)
	AND con.contype in ('p', 'u')
	;

	FOREACH i IN ARRAY idx
	LOOP
		EXECUTE 'ALTER INDEX '
			|| quote_ident(aud_schema) || '.'
			|| quote_ident(i)
			|| ' RENAME TO '
			|| quote_ident('_' || i);
	END LOOP;

	IF array_length(keys, 1) > 0 THEN
		FOREACH i IN ARRAY keys
		LOOP
			EXECUTE 'ALTER TABLE '
				|| quote_ident(aud_schema) || '.'
				|| quote_ident(table_name)
				|| ' RENAME CONSTRAINT '
				|| quote_ident(i)
				|| ' TO '
			|| quote_ident('__old__' || i);
		END LOOP;
	END IF;

	--
	-- get columns
	--
	SELECT	array_agg(quote_ident(a.attname) ORDER BY a.attnum)
	INTO	cols
	FROM	pg_catalog.pg_attribute a
	INNER JOIN pg_catalog.pg_class c on a.attrelid = c.oid
	INNER JOIN pg_catalog.pg_namespace n on n.oid = c.relnamespace
	LEFT JOIN pg_catalog.pg_description d
			on d.objoid = a.attrelid
			and d.objsubid = a.attnum
	WHERE  	n.nspname = quote_ident(aud_schema)
	  AND	c.relname = quote_ident(table_name)
	  AND 	a.attnum > 0
	  AND 	NOT a.attisdropped
	;

	--
	-- rename table
	--
	EXECUTE 'ALTER TABLE '
		|| quote_ident(aud_schema) || '.'
		|| quote_ident(table_name)
		|| ' RENAME TO '
		|| quote_ident('__old__' || table_name);


	--
	-- RENAME sequence
	--
	EXECUTE 'ALTER SEQUENCE '
		|| quote_ident(aud_schema) || '.'
		|| quote_ident(table_name || '_seq')
		|| ' RENAME TO '
		|| quote_ident('_old_' || table_name || '_seq');

	--
	-- create a new audit table
	--
	PERFORM schema_support.build_audit_table(aud_schema,tbl_schema,table_name);

	EXECUTE 'INSERT INTO '
		|| quote_ident(aud_schema) || '.'
		|| quote_ident(table_name) || ' ( '
		|| array_to_string(cols, ',') || ' ) SELECT '
		|| array_to_string(cols, ',') || ' FROM '
		|| quote_ident(aud_schema) || '.'
		|| quote_ident('__old__' || table_name)
		|| ' ORDER BY '
		|| quote_ident('aud#seq');

	EXECUTE 'DROP TABLE '
		|| quote_ident(aud_schema) || '.'
		|| quote_ident('__old__' || table_name);

	--
	-- drop audit sequence, in case it was nto dropped with table.
	--
	EXECUTE 'DROP SEQUENCE IF EXISTS '
		|| quote_ident(aud_schema) || '.'
		|| quote_ident('_old_' || table_name || '_seq');

	--
	-- drop constraints and indexes found before
	--
	FOR i IN SELECT	c2.relname
		  FROM	pg_catalog.pg_index i
			LEFT JOIN pg_catalog.pg_class c
				ON c.oid = i.indrelid
			LEFT JOIN pg_catalog.pg_class c2
				ON i.indexrelid = c2.oid
			LEFT JOIN pg_catalog.pg_namespace n
				ON c2.relnamespace = n.oid
			LEFT JOIN pg_catalog.pg_constraint con
				ON (conrelid = i.indrelid
				AND conindid = i.indexrelid
				AND contype IN ('p','u','x'))
		 WHERE n.nspname = quote_ident(aud_schema)
		  AND	c.relname = quote_ident('__old__' || table_name)
		  AND	contype is NULL
	LOOP
		EXECUTE 'DROP INDEX ' 
			|| quote_ident(aud_schema) || '.'
			|| quote_ident('_' || i);
	END LOOP;
	

	--
	-- recreate audit trigger
	--
	PERFORM schema_support.rebuild_audit_trigger (
		aud_schema, tbl_schema, table_name );

END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION schema_support.rebuild_audit_tables(aud_schema character varying, tbl_schema character varying)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
DECLARE
     table_list RECORD;
BEGIN
    FOR table_list IN
	SELECT b.table_name
	FROM information_schema.tables b
		INNER JOIN information_schema.tables a
			USING (table_name,table_type)
	WHERE table_type = 'BASE TABLE'
	AND a.table_schema = aud_schema
	AND b.table_schema = tbl_schema
	ORDER BY table_name
    LOOP
	PERFORM schema_support.save_dependent_objects_for_replay(aud_schema::varchar, table_list.table_name::varchar);
	PERFORM schema_support.rebuild_audit_table
	    ( aud_schema, tbl_schema, table_list.table_name );
	PERFORM schema_support.replay_object_recreates();
	PERFORM schema_support.replay_saved_grants();
    END LOOP;

    PERFORM schema_support.rebuild_audit_triggers(aud_schema, tbl_schema);
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION schema_support.refresh_mv_if_needed(relation text, schema text DEFAULT 'jazzhands'::text, debug boolean DEFAULT false)
 RETURNS void
 LANGUAGE plpgsql
 SET search_path TO schema_support
AS $function$
DECLARE 
	lastref	timestamp;
	lastdat	timestamp;
BEGIN
	SELECT coalesce(schema_support.mv_last_updated(relation, schema,debug),'-infinity') INTO lastref;
	SELECT coalesce(schema_support.relation_last_changed(relation, schema,debug),'-infinity') INTO lastdat;
	IF lastdat > lastref THEN
		EXECUTE 'REFRESH MATERIALIZED VIEW ' || quote_ident(schema)||'.'||quote_ident(relation);
		PERFORM schema_support.set_mv_last_updated(relation, schema);
	END IF;
	RETURN;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION schema_support.relation_last_changed(relation text, schema text DEFAULT 'jazzhands'::text, debug boolean DEFAULT false)
 RETURNS timestamp without time zone
 LANGUAGE plpgsql
 SET search_path TO schema_support
AS $function$
DECLARE
	audsch	text;
	rk	char;
	rv	timestamp;
	ts	timestamp;
	obj	text;
	objaud text;
BEGIN
	SELECT	audit_schema
	INTO	audsch
	FROM	schema_support.schema_audit_map m
	WHERE	m.schema = relation_last_changed.schema;

	IF NOT FOUND THEN
		RAISE EXCEPTION 'Schema % not configured for this', schema;
	END IF;

	SELECT 	relkind
	INTO	rk
	FROM	pg_catalog.pg_class c
		JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
	WHERE	n.nspname = relation_last_changed.schema
	AND	c.relname = relation_last_changed.relation;

	IF NOT FOUND THEN
		RAISE EXCEPTION 'No such object %.%', schema, relation;
	END IF;

	IF rk = 'r' THEN
		EXECUTE '
			SELECT	max("aud#timestamp")
			FROM	'||quote_ident(audsch)||'.'||quote_ident(relation)
		INTO rv;

		IF rv IS NULL THEN
			RETURN '-infinity'::interval;
		ELSE
			RETURN rv;
		END IF;
	END IF;

	IF rk = 'v' OR rk = 'm' THEN
		FOR obj,objaud IN WITH RECURSIVE recur AS (
				SELECT distinct rewrite.ev_class as root_oid, d.refobjid as oid
				FROM pg_depend d
        			JOIN pg_rewrite rewrite ON d.objid = rewrite.oid
					JOIN pg_class c on rewrite.ev_class = c.oid
					JOIN pg_namespace n on n.oid = c.relnamespace
				WHERE c.relname = relation
				AND n.nspname = relation_last_changed.schema
				AND d.refobjsubid > 0
			UNION ALL
				SELECT recur.root_oid, d.refobjid as oid
				FROM pg_depend d
        			JOIN pg_rewrite rewrite ON d.objid = rewrite.oid
				JOIN recur ON recur.oid = rewrite.ev_class
				AND d.refobjsubid > 0
			), list AS ( select distinct m.audit_schema, c.relname, c.relkind, recur.*
				FROM pg_class c
					JOIN recur on recur.oid = c.oid
					JOIN pg_namespace n on c.relnamespace = n.oid
					JOIN schema_support.schema_audit_map m
						ON m.schema = n.nspname
				WHERE relkind = 'r'
			) SELECT relname, audit_schema from list
		LOOP
			-- if there is no audit table, assume its kept current.  This is
			-- likely some sort of cache table.  XXX - should probably be
			-- updated to use the materialized view update bits
			BEGIN
				EXECUTE 'SELECT max("aud#timestamp") 
					FROM '||quote_ident(objaud)||'.'|| quote_ident(obj) 
					INTO ts;
				IF debug THEN
					RAISE NOTICE '%.% -> %', objaud, obj, ts;
				END IF;
				IF rv IS NULL OR ts > rv THEN
					rv := ts;
				END IF;
			EXCEPTION WHEN undefined_table THEN
				IF debug THEN
					RAISE NOTICE 'skipping %.%', schema, obj;
				END IF;
			END;
		END LOOP;
		RETURN rv;
	END IF;

	RAISE EXCEPTION 'Unable to process relkind %', rk;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION schema_support.set_mv_last_updated(relation text, schema text DEFAULT 'jazzhands'::text, debug boolean DEFAULT false)
 RETURNS timestamp without time zone
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO schema_support
AS $function$
DECLARE
	rv	timestamp;
BEGIN
	INSERT INTO schema_support.mv_refresh AS r (
		schema, view, refresh
	) VALUES (
		set_mv_last_updated.schema, relation, now()
	) ON CONFLICT ON CONSTRAINT mv_refresh_pkey DO UPDATE
		SET		refresh = now()
		WHERE	r.schema = set_mv_last_updated.schema
		AND		r.view = relation
	;

	RETURN rv;
END;
$function$
;

--
-- Process drops in backend_utils
--
-- New function
CREATE OR REPLACE FUNCTION backend_utils.refresh_if_needed(object text)
 RETURNS void
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	rk char;
BEGIN
	SELECT  relkind
	INTO    rk
	FROM    pg_catalog.pg_class c
		JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
	WHERE   n.nspname = 'jazzhands'
	AND     c.relname = relation_last_changed.relation;

	-- silently ignore things that are not materialized views
	IF rk = 'm' THEN
		PERFORM schema_support.refresh_mv_if_needed(object, 'jazzhands');
	END IF;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION backend_utils.relation_last_changed(view text)
 RETURNS timestamp without time zone
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	RETURN schema_support.relation_last_changed(view);
END;
$function$
;

-- Dropping obsoleted sequences....


-- Dropping obsoleted audit sequences....


-- Processing tables with no structural changes
-- Some of these may be redundant
-- fk constraints
ALTER TABLE netblock DROP CONSTRAINT IF EXISTS ak_netblock_params;
ALTER TABLE netblock
	ADD CONSTRAINT ak_netblock_params
	UNIQUE (ip_address, netblock_type, ip_universe_id, is_single_address);

-- index
-- triggers
DROP TRIGGER IF EXISTS trigger_pgnotify_account_collection_account_token_changes ON account_collection_account;
CREATE TRIGGER trigger_pgnotify_account_collection_account_token_changes AFTER INSERT OR DELETE OR UPDATE ON account_collection_account FOR EACH ROW EXECUTE PROCEDURE pgnotify_account_collection_account_token_changes();
DROP TRIGGER IF EXISTS trigger_pgnotify_account_password_changes ON account_password;
CREATE TRIGGER trigger_pgnotify_account_password_changes AFTER INSERT OR UPDATE ON account_password FOR EACH ROW EXECUTE PROCEDURE pgnotify_account_password_changes();
DROP TRIGGER IF EXISTS trigger_unrequire_password_change ON account_password;
CREATE TRIGGER trigger_unrequire_password_change BEFORE INSERT OR UPDATE OF password ON account_password FOR EACH ROW EXECUTE PROCEDURE unrequire_password_change();
DROP TRIGGER IF EXISTS trigger_pgnotify_account_token_change ON account_token;
CREATE TRIGGER trigger_pgnotify_account_token_change AFTER INSERT OR UPDATE ON account_token FOR EACH ROW EXECUTE PROCEDURE pgnotify_account_token_change();
DROP TRIGGER IF EXISTS trigger_company_insert_function_nudge ON company;
CREATE TRIGGER trigger_company_insert_function_nudge BEFORE INSERT ON company FOR EACH ROW EXECUTE PROCEDURE company_insert_function_nudge();
DROP TRIGGER IF EXISTS trigger_dns_record_check_name ON dns_record;
CREATE TRIGGER trigger_dns_record_check_name BEFORE INSERT OR UPDATE OF dns_name ON dns_record FOR EACH ROW EXECUTE PROCEDURE dns_record_check_name();
DROP TRIGGER IF EXISTS l2_net_coll_member_enforce_on_type_change ON layer2_network_collection;
CREATE CONSTRAINT TRIGGER l2_net_coll_member_enforce_on_type_change AFTER UPDATE OF layer2_network_collection_type ON layer2_network_collection DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE PROCEDURE l2_net_coll_member_enforce_on_type_change();
DROP TRIGGER IF EXISTS l3_net_coll_member_enforce_on_type_change ON layer3_network_collection;
CREATE CONSTRAINT TRIGGER l3_net_coll_member_enforce_on_type_change AFTER UPDATE OF layer3_network_collection_type ON layer3_network_collection DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE PROCEDURE l3_net_coll_member_enforce_on_type_change();
DROP TRIGGER IF EXISTS trigger_nb_dns_a_rec_validation ON netblock;
CREATE TRIGGER trigger_nb_dns_a_rec_validation BEFORE UPDATE OF ip_address, is_single_address ON netblock FOR EACH ROW EXECUTE PROCEDURE nb_dns_a_rec_validation();
DROP TRIGGER IF EXISTS trigger_pgnotify_token_change ON token;
CREATE TRIGGER trigger_pgnotify_token_change AFTER INSERT OR UPDATE ON token FOR EACH ROW EXECUTE PROCEDURE pgnotify_token_change();


-- BEGIN Misc that does not apply to above
/*
 * Copyright (c) 2016 Todd Kover
 * All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * $Id$
 */

\set ON_ERROR_STOP

-- Create schema if it does not exist, do nothing otherwise.
DO $$
DECLARE
	_tal INTEGER;
BEGIN
	select count(*)
	from pg_catalog.pg_namespace
	into _tal
	where nspname = 'backend_utils';
	IF _tal = 0 THEN
		DROP SCHEMA IF EXISTS backend_utils;
		CREATE SCHEMA backend_utils AUTHORIZATION jazzhands;
		COMMENT ON SCHEMA backend_utils IS 'part of jazzhands';
	END IF;
END;
$$;

		COMMENT ON SCHEMA backend_utils IS 'part of jazzhands';
------------------------------------------------------------------------------

--
-- used to trigger refreshes of materialized views
--
CREATE OR REPLACE FUNCTION backend_utils.refresh_if_needed(object text)
RETURNS void AS
$$
DECLARE
	rk char;
BEGIN
	SELECT  relkind
	INTO    rk
	FROM    pg_catalog.pg_class c
		JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
	WHERE   n.nspname = 'jazzhands'
	AND     c.relname = relation_last_changed.relation;

	-- silently ignore things that are not materialized views
	IF rk = 'm' THEN
		PERFORM schema_support.refresh_mv_if_needed(object, 'jazzhands');
	END IF;
END;
$$ 
SET search_path=jazzhands
LANGUAGE plpgsql SECURITY DEFINER;

--
-- returns the last time an object was changed, based on audit tables, either
-- for the object itself in the case of tables, or dependent objects, in the
-- case of materialized views and views.
--
CREATE OR REPLACE FUNCTION backend_utils.relation_last_changed(view text)
RETURNS timestamp AS
$$
BEGIN
	RETURN schema_support.relation_last_changed(view);
END;
$$ 
SET search_path=jazzhands
LANGUAGE plpgsql SECURITY DEFINER;

grant select on all tables in schema backend_utils to iud_role;
grant usage on schema backend_utils to iud_role;
revoke all on schema backend_utils from public;
revoke all on  all functions in schema backend_utils from public;
grant execute on all functions in schema backend_utils to iud_role;



-- END Misc that does not apply to above


-- BEGIN Misc that does not apply to above
ALTER TABLE network_interface DROP CONSTRAINT IF EXISTS check_any_yes_no_1926994056;

ALTER TABLE network_interface ADD CONSTRAINT 
CHECK_ANY_YES_NO_1926994056 CHECK (SHOULD_MONITOR IN ('Y', 'N', 'ANY'));

SELECT schema_support.rebuild_audit_tables('audit', 'jazzhands');


-- END Misc that does not apply to above


-- Clean Up
SELECT schema_support.replay_object_recreates();
SELECT schema_support.replay_saved_grants();
GRANT select on all tables in schema jazzhands to ro_role;
GRANT insert,update,delete on all tables in schema jazzhands to iud_role;
GRANT select on all sequences in schema jazzhands to ro_role;
GRANT usage on all sequences in schema jazzhands to iud_role;
GRANT select on all tables in schema audit to ro_role;
GRANT select on all sequences in schema audit to ro_role;
SELECT schema_support.end_maintenance();
select timeofday(), now();
