/*
Invoked:

	--suffix=v69
	--scan-tables
*/

\set ON_ERROR_STOP
SELECT schema_support.begin_maintenance();
select timeofday(), now();
CREATE SCHEMA account_collection_manip AUTHORIZATION jazzhands;
CREATE SCHEMA script_hooks AUTHORIZATION jazzhands;
--
-- Process middle (non-trigger) schema jazzhands
--
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
-- Changed function
SELECT schema_support.save_grants_for_replay('person_manip', 'change_company');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS person_manip.change_company ( final_company_id integer, _person_id integer, initial_company_id integer, _account_realm_id integer );
CREATE OR REPLACE FUNCTION person_manip.change_company(final_company_id integer, _person_id integer, initial_company_id integer, _account_realm_id integer DEFAULT NULL::integer)
 RETURNS void
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands, pg_temp
AS $function$
DECLARE
	initial_person_company  person_company%ROWTYPE;
	_arid			account_realm.account_realm_id%TYPE;
BEGIN
	IF _account_realm_id IS NULL THEN
		SELECT	account_realm_id
		INTO	_arid
		FROM	property
		WHERE	property_type = 'Defaults'
		AND	property_name = '_root_account_realm_id';
	ELSE
		_arid := _account_realm_id;
	END IF;
	set constraints fk_ac_ac_rlm_cpy_act_rlm_cpy DEFERRED;
	set constraints fk_account_prsn_cmpy_acct DEFERRED;
	set constraints fk_account_company_person DEFERRED;
	set constraints fk_pers_comp_attr_person_comp_ DEFERRED;

	UPDATE person_account_realm_company
		SET company_id = final_company_id
	WHERE person_id = _person_id
	AND company_id = initial_company_id
	AND account_realm_id = _arid;

	SELECT * 
	INTO initial_person_company 
	FROM person_company 
	WHERE person_id = _person_id 
	AND company_id = initial_company_id;

	UPDATE person_company
	SET company_id = final_company_id
	WHERE company_id = initial_company_id
	AND person_id = _person_id;

	UPDATE person_company_attr
	SET company_id = final_company_id
	WHERE company_id = initial_company_id
	AND person_id = _person_id;

	UPDATE account 
	SET company_id = final_company_id 
	WHERE company_id = initial_company_id 
	AND person_id = _person_id
	AND account_realm_id = _arid;

	set constraints fk_ac_ac_rlm_cpy_act_rlm_cpy IMMEDIATE;
	set constraints fk_account_prsn_cmpy_acct IMMEDIATE;
	set constraints fk_account_company_person IMMEDIATE;
	set constraints fk_pers_comp_attr_person_comp_ IMMEDIATE;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION person_manip.guess_person_id(first_name text, last_name text, login text, company_id integer DEFAULT NULL::integer)
 RETURNS integer
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands, pg_temp
AS $function$
DECLARE
	pid		person.person_id%TYPE;
	_l		text;
BEGIN
	-- see if that login name is alradeady associated with someone with the
	-- same first and last name
	EXECUTE '
		SELECT person_id
		FROM	person
				JOIN account USING (person_id,$2)
		WHERE	login = $1
		AND		first_name = $3
		AND		last_name = $4
	' INTO pid USING login, company_id, first_name, last_name;

	IF pid IS NOT NULL THEN
		RETURN pid;
	END IF;

	_l = regexp_replace(login, '@.*$', '');

	IF _l != login THEN
		EXECUTE '
			SELECT person_id
			FROM	person
					JOIN account USING (person_id,$2)
			WHERE	login = $1
			AND		first_name = $3
			AND		last_name = $4
		' INTO pid USING _l, company_id, first_name, last_name;
	
		IF pid IS NOT NULL THEN
			RETURN pid;
		END IF;
	END IF;

	RETURN NULL;
END;
$function$
;

--
-- Process middle (non-trigger) schema auto_ac_manip
--
-- Changed function
SELECT schema_support.save_grants_for_replay('auto_ac_manip', 'get_num_direct_reports');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS auto_ac_manip.get_num_direct_reports ( account_id integer, account_realm_id integer );
CREATE OR REPLACE FUNCTION auto_ac_manip.get_num_direct_reports(account_id integer, account_realm_id integer)
 RETURNS integer
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_numrpt	INTEGER;
BEGIN
	-- get number of direct reports
	EXECUTE '
		WITH peeps AS (
			SELECT	account_realm_id, account_id, login, person_id, 
					manager_person_id
			FROM	account a
				INNER JOIN person_company USING (person_id, company_id)
			WHERE	account_role = $3
			AND		account_type = ''person''
			AND		person_company_relation = ''employee''
			AND		a.is_enabled = ''Y''
		) SELECT count(*)
		FROM peeps reports
			INNER JOIN peeps managers on  
				managers.person_id = reports.manager_person_id
			AND	managers.account_realm_id = reports.account_realm_id
		WHERE	managers.account_id = $1
		AND		managers.account_realm_id = $2
	' INTO _numrpt USING account_id, account_realm_id, 'primary';

	RETURN _numrpt;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('auto_ac_manip', 'get_num_reports_with_reports');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS auto_ac_manip.get_num_reports_with_reports ( account_id integer, account_realm_id integer );
CREATE OR REPLACE FUNCTION auto_ac_manip.get_num_reports_with_reports(account_id integer, account_realm_id integer)
 RETURNS integer
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_numrlup	INTEGER;
BEGIN
	EXECUTE '
		WITH peeps AS (
			SELECT	account_realm_id, account_id, login, person_id, 
					manager_person_id
			FROM	account a
				INNER JOIN person_company USING (person_id, company_id)
			WHERE	account_role = $3
			AND		account_type = ''person''
			AND		person_company_relation = ''employee''
			AND		account_realm_id = $2
			AND		a.is_enabled = ''Y''
		), agg AS ( SELECT reports.*, managers.account_id as manager_account_id,
				managers.login as manager_login, p.property_name,
				p.property_value_account_coll_id as account_collection_id
			FROM peeps reports
			INNER JOIN peeps managers
				ON managers.person_id = reports.manager_person_id
				AND	managers.account_realm_id = reports.account_realm_id
			INNER JOIN property p 
				ON p.account_id = reports.account_id
				AND p.account_realm_id = reports.account_realm_id
				AND p.property_name IN ($4,$5)
				AND p.property_type = $6
		), rank AS (
			SELECT *,
				rank() OVER (partition by account_id ORDER BY property_name desc)
					as rank
			FROM agg
		) SELECT count(*) from rank
		WHERE	manager_account_id =  $1
		AND 	account_realm_id = $2
		AND	rank = 1;
	' INTO _numrlup USING account_id, account_realm_id, 'primary',
				'AutomatedDirectsAC','AutomatedRollupsAC','auto_acct_coll';

	RETURN _numrlup;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('auto_ac_manip', 'populate_direct_report_ac');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS auto_ac_manip.populate_direct_report_ac ( account_id integer, account_realm_id integer, login character varying );
CREATE OR REPLACE FUNCTION auto_ac_manip.populate_direct_report_ac(account_id integer, account_realm_id integer DEFAULT NULL::integer, login character varying DEFAULT NULL::character varying)
 RETURNS integer
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_directac	account_collection.account_collection_id%TYPE;
BEGIN
	_directac := auto_ac_manip.find_or_create_automated_ac(
		account_id := account_id,
		account_realm_id := account_realm_id,
		ac_type := 'AutomatedDirectsAC'
	);

	--
	-- Make membership right
	--
	EXECUTE '
		WITH peeps AS (
			SELECT	account_realm_id, account_id, login, person_id, 
					manager_person_id
			FROM	account a
				INNER JOIN person_company USING (person_id, company_id)
			WHERE	account_role = $2
			AND		account_type = ''person''
			AND		person_company_relation = ''employee''
			AND		a.is_enabled = ''Y''
		), arethere AS (
			SELECT account_collection_id, account_id FROM
				account_collection_account
				WHERE account_collection_id = $3
		), shouldbethere AS (
			SELECT $3 as account_collection_id, reports.account_id
			FROM peeps reports
				INNER JOIN peeps managers on  
					managers.person_id = reports.manager_person_id
				AND	managers.account_realm_id = reports.account_realm_id
			WHERE	managers.account_id =  $1
			UNION SELECT $3, $1
		), ins AS (
			INSERT INTO account_collection_account 
				(account_collection_id, account_id)
			SELECT account_collection_id, account_id 
			FROM shouldbethere
			WHERE (account_collection_id, account_id)
				NOT IN (select account_collection_id, account_id FROM arethere)
			RETURNING *
		), del AS (
			DELETE FROM account_collection_account
			WHERE (account_collection_id, account_id)
			IN (
				SELECT account_collection_id, account_id 
				FROM arethere
			) AND (account_collection_id, account_id) NOT IN (
				SELECT account_collection_id, account_id 
				FROM shouldbethere
			) RETURNING *
		) SELECT * from ins UNION SELECT * from del
		'USING account_id, 'primary', _directac;

	RETURN _directac;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('auto_ac_manip', 'populate_rollup_report_ac');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS auto_ac_manip.populate_rollup_report_ac ( account_id integer, account_realm_id integer, login character varying );
CREATE OR REPLACE FUNCTION auto_ac_manip.populate_rollup_report_ac(account_id integer, account_realm_id integer DEFAULT NULL::integer, login character varying DEFAULT NULL::character varying)
 RETURNS integer
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_rollupac	account_collection.account_collection_id%TYPE;
BEGIN
	_rollupac := auto_ac_manip.find_or_create_automated_ac(
		account_id := account_id,
		account_realm_id := account_realm_id,
		ac_type := 'AutomatedRollupsAC'
	);

	EXECUTE '
		WITH peeps AS (
			SELECT	account_realm_id, account_id, login, person_id, 
					manager_person_id
			FROM	account a
				INNER JOIN person_company USING (person_id, company_id)
			WHERE	account_role = $2
			AND		account_type = ''person''
			AND		person_company_relation = ''employee''
			AND		a.is_enabled = ''Y''
		), agg AS ( SELECT reports.*, managers.account_id as manager_account_id,
				managers.login as manager_login, p.property_name,
				p.property_value_account_coll_id as account_collection_id
			FROM peeps reports
			INNER JOIN peeps managers
				ON managers.person_id = reports.manager_person_id
				AND	managers.account_realm_id = reports.account_realm_id
			INNER JOIN property p 
				ON p.account_id = reports.account_id
				AND p.account_realm_id = reports.account_realm_id
				AND p.property_name IN ($3,$4)
				AND p.property_type = $5
		), rank AS (
			SELECT *,
				rank() OVER (partition by account_id ORDER BY property_name desc)
					as rank
			FROM agg
		), shouldbethere AS (
			SELECT $6 as account_collection_id,
					account_collection_id as child_account_collection_id
			FROM rank
	 		WHERE	manager_account_id =  $1
			AND	rank = 1
		), arethere AS (
			SELECT account_collection_id, child_account_collection_id FROM
				account_collection_hier
			WHERE account_collection_id = $6
		), ins AS (
			INSERT INTO account_collection_hier 
				(account_collection_id, child_account_collection_id)
			SELECT account_collection_id, child_account_collection_id
			FROM shouldbethere
			WHERE (account_collection_id, child_account_collection_id)
				NOT IN (SELECT * from arethere)
			RETURNING *
		), del AS (
			DELETE FROM account_collection_hier
			WHERE (account_collection_id, child_account_collection_id)
				IN (SELECT * from arethere)
			AND (account_collection_id, child_account_collection_id)
				NOT IN (SELECT * FROM shouldbethere)
			RETURNING *
		) select * from ins UNION select * from del;

	' USING account_id, 'primary',
				'AutomatedDirectsAC','AutomatedRollupsAC','auto_acct_coll',
				_rollupac;

	RETURN _rollupac;
END;
$function$
;

--
-- Process middle (non-trigger) schema company_manip
--
-- Changed function
SELECT schema_support.save_grants_for_replay('company_manip', 'add_auto_collections');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS company_manip.add_auto_collections ( _company_id integer, _account_realm_id integer, _company_type text );
CREATE OR REPLACE FUNCTION company_manip.add_auto_collections(_company_id integer, _account_realm_id integer, _company_type text)
 RETURNS void
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_ar		account_realm.account_realm_name%TYPE;
	_csn	company.company_short_name%TYPE;
	_r		RECORD;
	_v		text[];
	i		text;
	acname	account_collection.account_collection_name%TYPE;
	acid	account_collection.account_collection_id%TYPE;
	propv	text;
	tally	integer;
BEGIN
	PERFORM *
	FROM	account_realm_company
	WHERE	company_id = _company_id
	AND		account_realm_id = _account_realm_id;
	IF NOT FOUND THEN
		RAISE EXCEPTION 'Company and Account Realm are not associated together'
			USING ERRCODE = 'not_null_violation';
	END IF;

	PERFORM *
	FROM	company_type
	WHERE	company_id = _company_id
	AND		company_type = _company_type;
	IF NOT FOUND THEN
		RAISE EXCEPTION 'Company % is not of type %', _company_id, _company_type
			USING ERRCODE = 'not_null_violation';
	END IF;
	
	tally := 0;
	FOR _r IN SELECT	property_name, property_type, permit_company_id
				FROM    property_collection_property pcp
				INNER JOIN property_collection pc
					USING (property_collection_id)
				INNER JOIN val_property vp USING (property_name,property_type)
				WHERE pc.property_collection_type = 'auto_ac_assignment'
				AND pc.property_collection_name = _company_type
				AND property_name != 'site'
	LOOP
		IF _r.property_name = 'account_type' THEN
			SELECT array_agg( account_type)
			INTO _v
			FROM val_account_type
			WHERE account_type != 'blacklist';
		ELSE
			_v := ARRAY[NULL]::text[];
		END IF;

	SELECT	account_realm_name
	INTO	_ar
	FROM	account_realm
	WHERE	account_realm_id = _account_realm_id;

	SELECT	company_short_name
	INTO	_csn
	FROM	company
	WHERE	company_id = _company_id;

		FOREACH i IN ARRAY _v
		LOOP
			IF i IS NULL THEN
				acname := concat(_ar, '_', _csn, '_', _r.property_name);
				propv := NULL;
			ELSE
				acname := concat(_ar, '_', _csn, '_', i);
				propv := i;
			END IF;

			INSERT INTO account_collection (
				account_collection_name, account_collection_type
			) VALUES (
				acname, 'automated'
			) RETURNING account_collection_id INTO acid;

			INSERT INTO property (
				property_name, property_type, account_realm_id,
				account_collection_id,
				company_id, property_value
			) VALUES (
				_r.property_name, _r.property_type, _account_realm_id,
				acid,
				_company_id, propv
			);
			tally := tally + 1;
		END LOOP;
	END LOOP;
END;
$function$
;

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

	INSERT INTO company (
		company_name, company_short_name,
		parent_company_id, description
	) VALUES (
		_company_name, _short,
		_parent_company_id, _description
	) RETURNING company_id INTO _cmpid;

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

-- New function
CREATE OR REPLACE FUNCTION company_manip.remove_company(_company_id integer, raise_exception boolean DEFAULT true)
 RETURNS boolean
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	IF raise_exception THEN
		DELETE FROM company_type
		WHERE company_id = _company_id;

		DELETE FROM account_realm_company
		WHERE company_id = _company_id;

		DELETE FROM account_realm_company
		WHERE company_id = _company_id;

		DELETE FROM company
		WHERE company_id = _company_id;
	ELSE
		BEGIN
			DELETE FROM company_type
			WHERE company_id = _company_id;

			DELETE FROM account_realm_company
			WHERE company_id = _company_id;

			DELETE FROM account_realm_company
			WHERE company_id = _company_id;

			DELETE FROM company
			WHERE company_id = _company_id;
		EXCEPTION WHEN foreign_key_violation THEN
			RETURN false;
		END;
	END IF;
	RETURN true;
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
--
-- Process middle (non-trigger) schema netblock_utils
--
--
-- Process middle (non-trigger) schema netblock_manip
--
--
-- Process middle (non-trigger) schema physical_address_utils
--
--
-- Process middle (non-trigger) schema component_utils
--
--
-- Process middle (non-trigger) schema snapshot_manip
--
--
-- Process middle (non-trigger) schema lv_manip
--
--
-- Process middle (non-trigger) schema schema_support
--
-- Changed function
SELECT schema_support.save_grants_for_replay('schema_support', 'save_dependant_objects_for_replay');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS schema_support.save_dependant_objects_for_replay ( schema character varying, object character varying, dropit boolean, doobjectdeps boolean );
CREATE OR REPLACE FUNCTION schema_support.save_dependant_objects_for_replay(schema character varying, object character varying, dropit boolean DEFAULT true, doobjectdeps boolean DEFAULT false)
 RETURNS void
 LANGUAGE plpgsql
AS $function$
BEGIN
	PERFORM schema_support.save_dependent_objects_for_replay(
		schema, object, dropit, doobjectdeps);
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

-- New function
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
	RAISE NOTICE 'processing %.%', schema, object;
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
		RAISE NOTICE '1 dealing with  %.%', _r.nspname, _r.proname;
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
			RAISE NOTICE '2 dealing with  %.%', _r.nspname, _r.relname;
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

--
-- Process middle (non-trigger) schema approval_utils
--
--
-- Process middle (non-trigger) schema account_collection_manip
--
-- New function
CREATE OR REPLACE FUNCTION account_collection_manip.id_tag()
 RETURNS character varying
 LANGUAGE plpgsql
AS $function$
BEGIN
	RETURN('<-- $Id$ -->');
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION account_collection_manip.manip_membership(account_collection_name character varying, account_collection_type character varying, account_id integer, is_member boolean)
 RETURNS boolean
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	tally	INTEGER;
	acid	account_collection.account_collection_id%TYPE;
BEGIN
	IF is_member IS NULL THEN
		is_member := false;
	END IF;

	EXECUTE '
		SELECT	account_collection_id
		FROM	account_collection
		WHERE	account_collection_name = $1
		AND		account_collection_type = $2
	' INTO acid USING account_collection_name, account_collection_type;

	IF acid IS NULL THEN
		RAISE EXCEPTION 'Unknown account collection %:%',
			account_collection_type, account_collection_name
			USING ERRCODE = 'invalid_parameter_value';
	END IF;


	IF is_member THEN
		EXECUTE '
			SELECT count(*)
			FROM account_collection_account
			WHERE account_collection_id = $1
			AND account_id = $2
		' INTO tally USING acid, account_id;

		IF tally = 0 THEN
			EXECUTE 'INSERT INTO account_collection_account (
				account_collection_id, account_id
				) VALUES (
					$1, $2
				)
			' USING acid, account_id;
		END IF;
		RETURN true;
	ELSE
		EXECUTE '
			DELETE FROM account_collection_account
			WHERE account_collection_id = $1
			AND account_id = $2
		' USING acid, account_id;
		return false;
	END IF;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION account_collection_manip.manip_membership(account_collection_name character varying, account_collection_type character varying, account_id integer, is_member character)
 RETURNS boolean
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	forced boolean;
BEGIN
	IF is_member IS NULL OR is_member = 'N' THEN
		forced = false;
	ELSE
		forced = true;
	END IF;
	RETURN account_collection_manip.manip_membership(
		account_collection_name, account_collection_type, account_id,
		forced);
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION account_collection_manip.manip_membership(account_collection_name character varying, account_collection_type character varying, account_id integer, is_member integer)
 RETURNS boolean
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	forced boolean;
BEGIN
	IF is_member IS NULL OR is_member = 0 THEN
		forced = false;
	ELSE
		forced = true;
	END IF;
	RETURN account_collection_manip.manip_membership(
		account_collection_name, account_collection_type, account_id,
		forced);
END;
$function$
;

--
-- Process middle (non-trigger) schema script_hooks
--
-- New function
CREATE OR REPLACE FUNCTION script_hooks.id_tag()
 RETURNS character varying
 LANGUAGE plpgsql
AS $function$
BEGIN
	RETURN('<-- $Id$ -->');
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION script_hooks.mkpasswdfiles_post()
 RETURNS void
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	BEGIN
		PERFORM local_hooks.mkpasswdfiles_post();
	EXCEPTION WHEN invalid_schema_name OR undefined_function THEN
		PERFORM 1;
	END;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION script_hooks.mkpasswdfiles_pre()
 RETURNS void
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	BEGIN
		PERFORM local_hooks.mkpasswdfiles_pre();
	EXCEPTION WHEN invalid_schema_name OR undefined_function THEN
		PERFORM 1;
	END;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION script_hooks.zonegen_post()
 RETURNS void
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	BEGIN
		PERFORM local_hooks.zonegen_post();
	EXCEPTION WHEN invalid_schema_name OR undefined_function THEN
		PERFORM 1;
	END;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION script_hooks.zonegen_pre()
 RETURNS void
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	BEGIN
		PERFORM local_hooks.zonegen_pre();
	EXCEPTION WHEN invalid_schema_name OR undefined_function THEN
		PERFORM 1;
	END;
END;
$function$
;

-- Creating new sequences....


--
-- Process trigger procs in jazzhands
--
-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'account_collection_hier_enforce');
CREATE OR REPLACE FUNCTION jazzhands.account_collection_hier_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	act	val_account_collection_type%ROWTYPE;
BEGIN
	SELECT *
	INTO	act
	FROM	val_account_collection_type
	WHERE	account_collection_type =
		(select account_collection_type from account_collection
			where account_collection_id = NEW.account_collection_id);

	IF act.can_have_hierarchy = 'N' THEN
		RAISE EXCEPTION 'Account Collections of type % may not be hierarcical',
			act.account_collection_type
			USING ERRCODE= 'unique_violation';
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'account_collection_member_enforce');
CREATE OR REPLACE FUNCTION jazzhands.account_collection_member_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	act	val_account_collection_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	act
	FROM	val_account_collection_type
	WHERE	account_collection_type =
		(select account_collection_type from account_collection
			where account_collection_id = NEW.account_collection_id);

	IF act.MAX_NUM_MEMBERS IS NOT NULL THEN
		select count(*)
		  into tally
		  from account_collection_account
		  where account_collection_id = NEW.account_collection_id;
		IF tally > act.MAX_NUM_MEMBERS THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF act.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		select count(*)
		  into tally
		  from account_collection_account
		  		inner join account_collection using (account_collection_id)
		  where account_id = NEW.account_id
		  and	account_collection_type = act.account_collection_type;
		IF tally > act.MAX_NUM_COLLECTIONS THEN
			RAISE EXCEPTION 'Account may not be a member of more than % collections of type %',
				act.MAX_NUM_COLLECTIONS, act.account_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'account_enforce_is_enabled');
CREATE OR REPLACE FUNCTION jazzhands.account_enforce_is_enabled()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	correctval	char(1);
BEGIN
	SELECT is_enabled INTO correctval
	FROM val_person_status
	WHERE person_status = NEW.account_status;

	IF TG_OP = 'INSERT' THEN
		IF NEW.is_enabled is NULL THEN
			NEW.is_enabled = correctval;
		ELSIF NEW.is_enabled != correctval THEN
			RAISE EXCEPTION 'May not set IS_ENABLED to an invalid value (%) for given account_status: %', NEW.is_enabled, NEW.account_status
				USING errcode = 'integrity_constraint_violation';
		END IF;
	ELSIF TG_OP = 'UPDATE' THEN
		IF NEW.account_status != OLD.account_status THEN
			IF NEW.is_enabled != correctval THEN
				NEW.is_enabled := correctval;
			END IF;
		ELSIF NEW.is_enabled != correctval THEN
			RAISE EXCEPTION 'May not update IS_ENABLED to an invalid value (%->%) for given account_status: %', OLD.account_status, NEW.account_status, NEW.is_enabled
			USING ERRCODE = 'integrity_constraint_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'account_validate_login');
CREATE OR REPLACE FUNCTION jazzhands.account_validate_login()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	regexp		text;
	correctval	char(1);
BEGIN
	SELECT property_value
	INTO   regexp
	FROM	property
	WHERE	account_realm_id = NEW.account_realm_id
	AND		property_name = 'login_restriction'
	AND		property_type = 'Defaults';

	IF FOUND THEN
		-- ~ '[^-/@a-z0-9_]+' THEN
		IF NEW.login  ~ regexp THEN
			RAISE EXCEPTION 'May not set login to an invalid value (%)', NEW.login
				USING errcode = 'integrity_constraint_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'automated_ac_on_person_company');
CREATE OR REPLACE FUNCTION jazzhands.automated_ac_on_person_company()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally	INTEGER;
	_r		RECORD;
BEGIN
	IF ( TG_OP = 'INSERT' OR TG_OP = 'UPDATE' ) THEN
		PERFORM	auto_ac_manip.make_personal_acs_right(account_id)
		FROM	v_corp_family_account
				INNER JOIN person_company USING (person_id,company_id)
		WHERE	account_role = 'primary'
		AND		person_id = NEW.person_id
		AND		company_id = NEW.company_id;

		IF ( TG_OP = 'INSERT' OR ( TG_OP = 'UPDATE' AND 
				NEW.manager_person_id != OLD.manager_person_id ) 
		) THEN
			-- update the person's manager to match
			WITH RECURSIVE map As (
				SELECT account_id as root_account_id,
					account_id, login, manager_account_id, manager_login
				FROM v_account_manager_map
				UNION
				SELECT map.root_account_id, m.account_id, m.login,
					m.manager_account_id, m.manager_login 
					from v_account_manager_map m
						join map on m.account_id = map.manager_account_id
			), x AS ( SELECT auto_ac_manip.make_auto_report_acs_right(
						account_id := manager_account_id,
						account_realm_id := account_realm_id,
						login := manager_login)
					FROM map m
							join v_corp_family_account a ON
								a.account_id = m.root_account_id
					WHERE a.person_id = NEW.person_id
					AND a.company_id = NEW.company_id
			) SELECT count(*) into _tally from x;
			IF TG_OP = 'UPDATE' THEN
				PERFORM auto_ac_manip.make_auto_report_acs_right(
							account_id := account_id)
				FROM    v_corp_family_account
				WHERE   account_role = 'primary'
				AND     is_enabled = 'Y'
				AND     person_id = OLD.manager_person_id;
			END IF;
		END IF;
	END IF;

	IF ( TG_OP = 'DELETE' OR TG_OP = 'UPDATE' ) THEN
		PERFORM	auto_ac_manip.make_personal_acs_right(account_id)
		FROM	v_corp_family_account
				INNER JOIN person_company USING (person_id,company_id)
		WHERE	account_role = 'primary'
		AND		person_id = OLD.person_id
		AND		company_id = OLD.company_id;
	END IF;
	IF ( TG_OP = 'UPDATE' ) THEN
		PERFORM	auto_ac_manip.make_personal_acs_right(account_id)
		FROM	v_corp_family_account
				INNER JOIN person_company USING (person_id,company_id)
		WHERE	account_role = 'primary'
		AND		person_id = NEW.person_id
		AND		company_id = NEW.company_id;
	END IF;

	IF TG_OP = 'DELETE' THEN
		RETURN OLD;
	ELSE
		RETURN NEW;
	END IF;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'check_account_colllection_hier_loop');
CREATE OR REPLACE FUNCTION jazzhands.check_account_colllection_hier_loop()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	IF NEW.account_collection_id = NEW.child_account_collection_id THEN
		RAISE EXCEPTION 'Account Collection Loops Not Pernitted '
			USING ERRCODE = 20704;	/* XXX */
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'check_device_colllection_hier_loop');
CREATE OR REPLACE FUNCTION jazzhands.check_device_colllection_hier_loop()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	IF NEW.device_collection_id = NEW.parent_device_collection_id THEN
		RAISE EXCEPTION 'device Collection Loops Not Pernitted '
			USING ERRCODE = 20704;	/* XXX */
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'check_netblock_colllection_hier_loop');
CREATE OR REPLACE FUNCTION jazzhands.check_netblock_colllection_hier_loop()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	IF NEW.netblock_collection_id = NEW.child_netblock_collection_id THEN
		RAISE EXCEPTION 'Netblock Collection Loops Not Pernitted '
			USING ERRCODE = 20704;	/* XXX */
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'check_svcenv_colllection_hier_loop');
CREATE OR REPLACE FUNCTION jazzhands.check_svcenv_colllection_hier_loop()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	IF NEW.service_env_collection_id = 
		NEW.child_service_env_coll_id THEN
			RAISE EXCEPTION 'svcenv Collection Loops Not Pernitted '
			USING ERRCODE = 20704;	/* XXX */
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'check_token_colllection_hier_loop');
CREATE OR REPLACE FUNCTION jazzhands.check_token_colllection_hier_loop()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	IF NEW.token_collection_id = NEW.child_token_collection_id THEN
		RAISE EXCEPTION 'token Collection Loops Not Pernitted '
			USING ERRCODE = 20704;	/* XXX */
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'company_collection_hier_enforce');
CREATE OR REPLACE FUNCTION jazzhands.company_collection_hier_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	dct	val_company_collection_type%ROWTYPE;
BEGIN
	SELECT *
	INTO	dct
	FROM	val_company_collection_type
	WHERE	company_collection_type =
		(select company_collection_type from company_collection
			where company_collection_id = NEW.company_collection_id);

	IF dct.can_have_hierarchy = 'N' THEN
		RAISE EXCEPTION 'Company Collections of type % may not be hierarcical',
			dct.company_collection_type
			USING ERRCODE= 'unique_violation';
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'company_collection_member_enforce');
CREATE OR REPLACE FUNCTION jazzhands.company_collection_member_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	dct	val_company_collection_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	dct
	FROM	val_company_collection_type
	WHERE	company_collection_type =
		(select company_collection_type from company_collection
			where company_collection_id = NEW.company_collection_id);

	IF dct.MAX_NUM_MEMBERS IS NOT NULL THEN
		select count(*)
		  into tally
		  from company_collection_company
		  where company_collection_id = NEW.company_collection_id;
		IF tally > dct.MAX_NUM_MEMBERS THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF dct.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		select count(*)
		  into tally
		  from company_collection_company
		  		inner join company_collection using (company_collection_id)
		  where company_id = NEW.company_id
		  and	company_collection_type = dct.company_collection_type;
		IF tally > dct.MAX_NUM_COLLECTIONS THEN
			RAISE EXCEPTION 'Company may not be a member of more than % collections of type %',
				dct.MAX_NUM_COLLECTIONS, dct.company_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'create_new_unix_account');
CREATE OR REPLACE FUNCTION jazzhands.create_new_unix_account()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	unix_id 		INTEGER;
	_account_collection_id 	INTEGER;
	_arid			INTEGER;
BEGIN
	--
	-- This should be a property that shows which account collections
	-- get unix accounts created by default, but the mapping of unix-groups
	-- to account collection across realms needs to be resolved
	--
	SELECT  account_realm_id
	INTO    _arid
	FROM    property
	WHERE   property_name = '_root_account_realm_id'
	AND     property_type = 'Defaults';

	IF _arid IS NOT NULL AND NEW.account_realm_id = _arid THEN
		IF NEW.person_id != 0 THEN
			PERFORM person_manip.setup_unix_account(
				in_account_id := NEW.account_id,
				in_account_type := NEW.account_type
			);
		END IF;
	END IF;
	RETURN NEW;	
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'del_v_corp_family_account');
CREATE OR REPLACE FUNCTION jazzhands.del_v_corp_family_account()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	acct_realm_id	account_realm.account_realm_id%TYPE;
BEGIN
	SELECT	account_realm_id
	INTO	acct_realm_id
	FROM	property
	WHERE	property_name = '_root_account_realm_id'
	AND	property_type = 'Defaults';

	IF acct_realm_id != OLD.account_realm_id THEN
		RAISE EXCEPTION 'Invalid account_realm_id'
		USING ERRCODE = 'foreign_key_violation';
	END IF;

	DELETE FROM account where account_id = OLD.account_id;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'dns_domain_collection_hier_enforce');
CREATE OR REPLACE FUNCTION jazzhands.dns_domain_collection_hier_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	dct	val_dns_domain_collection_type%ROWTYPE;
BEGIN
	SELECT *
	INTO	dct
	FROM	val_dns_domain_collection_type
	WHERE	dns_domain_collection_type =
		(select dns_domain_collection_type from dns_domain_collection
			where dns_domain_collection_id = NEW.dns_domain_collection_id);

	IF dct.can_have_hierarchy = 'N' THEN
		RAISE EXCEPTION 'DNS Domain Collections of type % may not be hierarcical',
			dct.dns_domain_collection_type
			USING ERRCODE= 'unique_violation';
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'dns_domain_collection_member_enforce');
CREATE OR REPLACE FUNCTION jazzhands.dns_domain_collection_member_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	dct	val_dns_domain_collection_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	dct
	FROM	val_dns_domain_collection_type
	WHERE	dns_domain_collection_type =
		(select dns_domain_collection_type from dns_domain_collection
			where dns_domain_collection_id = NEW.dns_domain_collection_id);

	IF dct.MAX_NUM_MEMBERS IS NOT NULL THEN
		select count(*)
		  into tally
		  from dns_domain_collection_dns_dom
		  where dns_domain_collection_id = NEW.dns_domain_collection_id;
		IF tally > dct.MAX_NUM_MEMBERS THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF dct.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		select count(*)
		  into tally
		  from dns_domain_collection_dns_dom
		  		inner join dns_domain_collection using (dns_domain_collection_id)
		  where dns_domain_id = NEW.dns_domain_id
		  and	dns_domain_collection_type = dct.dns_domain_collection_type;
		IF tally > dct.MAX_NUM_COLLECTIONS THEN
			RAISE EXCEPTION 'DNS Domain may not be a member of more than % collections of type %',
				dct.MAX_NUM_COLLECTIONS, dct.dns_domain_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'fix_person_image_oid_ownership');
CREATE OR REPLACE FUNCTION jazzhands.fix_person_image_oid_ownership()
 RETURNS trigger
 LANGUAGE plpgsql
 SET search_path TO jazzhands
AS $function$
DECLARE
   b	integer;
   str	varchar;
BEGIN
	b := NEW.image_blob; 
	BEGIN
		str := 'GRANT SELECT on LARGE OBJECT ' || b || ' to picture_image_ro';
		EXECUTE str;
		str :=  'GRANT UPDATE on LARGE OBJECT ' || b || ' to picture_image_rw';
		EXECUTE str;
	EXCEPTION WHEN OTHERS THEN
		RAISE NOTICE 'Unable to grant on %', b;
	END;

	BEGIN
		EXECUTE 'ALTER large object ' || b || ' owner to jazzhands';
	EXCEPTION WHEN OTHERS THEN
		RAISE NOTICE 'Unable to adjust ownership of %', b;
	END;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'ins_v_corp_family_account');
CREATE OR REPLACE FUNCTION jazzhands.ins_v_corp_family_account()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	acct_realm_id	account_realm.account_realm_id%TYPE;
BEGIN
	SELECT	account_realm_id
	INTO	acct_realm_id
	FROM	property
	WHERE	property_name = '_root_account_realm_id'
	AND	property_type = 'Defaults';

	IF acct_realm_id != NEW.account_realm_id THEN
		RAISE EXCEPTION 'Invalid account_realm_id'
		USING ERRCODE = 'foreign_key_violation';
	END IF;

	INSERT INTO account VALUES (NEW.*);

END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'layer2_network_collection_hier_enforce');
CREATE OR REPLACE FUNCTION jazzhands.layer2_network_collection_hier_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	act	val_layer2_network_coll_type%ROWTYPE;
BEGIN
	SELECT *
	INTO	act
	FROM	val_layer2_network_coll_type
	WHERE	layer2_network_collection_type =
		(select layer2_network_collection_type from layer2_network_collection
			where layer2_network_collection_id = NEW.layer2_network_collection_id);

	IF act.can_have_hierarchy = 'N' THEN
		RAISE EXCEPTION 'Layer2 Network Collections of type % may not be hierarcical',
			act.layer2_network_collection_type
			USING ERRCODE= 'unique_violation';
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'layer2_network_collection_member_enforce');
CREATE OR REPLACE FUNCTION jazzhands.layer2_network_collection_member_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	act	val_layer2_network_coll_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	act
	FROM	val_layer2_network_coll_type
	WHERE	layer2_network_collection_type =
		(select layer2_network_collection_type from layer2_network_collection
			where layer2_network_collection_id = NEW.layer2_network_collection_id);

	IF act.MAX_NUM_MEMBERS IS NOT NULL THEN
		select count(*)
		  into tally
		  from l2_network_coll_l2_network
		  where layer2_network_collection_id = NEW.layer2_network_collection_id;
		IF tally > act.MAX_NUM_MEMBERS THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF act.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		select count(*)
		  into tally
		  from l2_network_coll_l2_network
		  		inner join layer2_network_collection using (layer2_network_collection_id)
		  where layer2_network_id = NEW.layer2_network_id
		  and	layer2_network_collection_type = act.layer2_network_collection_type;
		IF tally > act.MAX_NUM_COLLECTIONS THEN
			RAISE EXCEPTION 'Layer2 network may not be a member of more than % collections of type %',
				act.MAX_NUM_COLLECTIONS, act.layer2_network_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'layer3_network_collection_hier_enforce');
CREATE OR REPLACE FUNCTION jazzhands.layer3_network_collection_hier_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	act	val_layer3_network_coll_type%ROWTYPE;
BEGIN
	SELECT *
	INTO	act
	FROM	val_layer3_network_coll_type
	WHERE	layer3_network_collection_type =
		(select layer3_network_collection_type from layer3_network_collection
			where layer3_network_collection_id = NEW.layer3_network_collection_id);

	IF act.can_have_hierarchy = 'N' THEN
		RAISE EXCEPTION 'Layer3 Network Collections of type % may not be hierarcical',
			act.layer3_network_collection_type
			USING ERRCODE= 'unique_violation';
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'layer3_network_collection_member_enforce');
CREATE OR REPLACE FUNCTION jazzhands.layer3_network_collection_member_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	act	val_layer3_network_coll_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	act
	FROM	val_layer3_network_coll_type
	WHERE	layer3_network_collection_type =
		(select layer3_network_collection_type from layer3_network_collection
			where layer3_network_collection_id = NEW.layer3_network_collection_id);

	IF act.MAX_NUM_MEMBERS IS NOT NULL THEN
		select count(*)
		  into tally
		  from l3_network_coll_l3_network
		  where layer3_network_collection_id = NEW.layer3_network_collection_id;
		IF tally > act.MAX_NUM_MEMBERS THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF act.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		select count(*)
		  into tally
		  from l3_network_coll_l3_network
		  		inner join layer3_network_collection using (layer3_network_collection_id)
		  where layer3_network_id = NEW.layer3_network_id
		  and	layer3_network_collection_type = act.layer3_network_collection_type;
		IF tally > act.MAX_NUM_COLLECTIONS THEN
			RAISE EXCEPTION 'Layer3 Network may not be a member of more than % collections of type %',
				act.MAX_NUM_COLLECTIONS, act.layer3_network_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'netblock_collection_hier_enforce');
CREATE OR REPLACE FUNCTION jazzhands.netblock_collection_hier_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	nct	val_netblock_collection_type%ROWTYPE;
BEGIN
	SELECT *
	INTO	nct
	FROM	val_netblock_collection_type
	WHERE	netblock_collection_type =
		(select netblock_collection_type from netblock_collection
			where netblock_collection_id = NEW.netblock_collection_id);

	IF nct.can_have_hierarchy = 'N' THEN
		RAISE EXCEPTION 'Netblock Collections of type % may not be hierarcical',
			nct.netblock_collection_type
			USING ERRCODE= 'unique_violation';
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'netblock_collection_member_enforce');
CREATE OR REPLACE FUNCTION jazzhands.netblock_collection_member_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	nct	val_netblock_collection_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	nct
	FROM	val_netblock_collection_type
	WHERE	netblock_collection_type =
		(select netblock_collection_type from netblock_collection
			where netblock_collection_id = NEW.netblock_collection_id);

	IF nct.MAX_NUM_MEMBERS IS NOT NULL THEN
		select count(*)
		  into tally
		  from netblock_collection_netblock
		  where netblock_collection_id = NEW.netblock_collection_id;
		IF tally > nct.MAX_NUM_MEMBERS THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF nct.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		select count(*)
		  into tally
		  from netblock_collection_netblock
		  		inner join netblock_collection using (netblock_collection_id)
		  where netblock_id = NEW.netblock_id
		  and	netblock_collection_type = nct.netblock_collection_type;
		IF tally > nct.MAX_NUM_COLLECTIONS THEN
			RAISE EXCEPTION 'Netblock may not be a member of more than % collections of type %',
				nct.MAX_NUM_COLLECTIONS, nct.netblock_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'property_collection_hier_enforce');
CREATE OR REPLACE FUNCTION jazzhands.property_collection_hier_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	pct	val_property_collection_type%ROWTYPE;
BEGIN
	SELECT *
	INTO	pct
	FROM	val_property_collection_type
	WHERE	property_collection_type =
		(select property_collection_type from property_collection
			where property_collection_id = NEW.property_collection_id);

	IF pct.can_have_hierarchy = 'N' THEN
		RAISE EXCEPTION 'Property Collections of type % may not be hierarcical',
			pct.property_collection_type
			USING ERRCODE= 'unique_violation';
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'property_collection_member_enforce');
CREATE OR REPLACE FUNCTION jazzhands.property_collection_member_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	pct	val_property_collection_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	pct
	FROM	val_property_collection_type
	WHERE	property_collection_type =
		(select property_collection_type from property_collection
			where property_collection_id = NEW.property_collection_id);

	IF pct.MAX_NUM_MEMBERS IS NOT NULL THEN
		select count(*)
		  into tally
		  from property_collection_property
		  where property_collection_id = NEW.property_collection_id;
		IF tally > pct.MAX_NUM_MEMBERS THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF pct.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		select count(*)
		  into tally
		  from property_collection_property
		  		inner join property_collection using (property_collection_id)
		  where	
				property_name = NEW.property_name
		  and	property_type = NEW.property_type
		  and	property_collection_type = pct.property_collection_type;
		IF tally > pct.MAX_NUM_COLLECTIONS THEN
			RAISE EXCEPTION 'Property may not be a member of more than % collections of type %',
				pct.MAX_NUM_COLLECTIONS, pct.property_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'service_environment_coll_hier_enforce');
CREATE OR REPLACE FUNCTION jazzhands.service_environment_coll_hier_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	svcenvt	val_service_env_coll_type%ROWTYPE;
BEGIN
	SELECT *
	INTO	svcenvt
	FROM	val_service_env_coll_type
	WHERE	service_env_collection_type =
		(select service_env_collection_type 
			from service_environment_collection
			where service_env_collection_id = 
				NEW.service_env_collection_id);

	IF svcenvt.can_have_hierarchy = 'N' THEN
		RAISE EXCEPTION 'Service Environment Collections of type % may not be hierarcical',
			svcenvt.service_env_collection_type
			USING ERRCODE= 'unique_violation';
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'service_environment_collection_member_enforce');
CREATE OR REPLACE FUNCTION jazzhands.service_environment_collection_member_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	svcenvt	val_service_env_coll_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	svcenvt
	FROM	val_service_env_coll_type
	WHERE	service_env_collection_type =
		(select service_env_collection_type 
			from service_environment_collection
			where service_env_collection_id = 
				NEW.service_env_collection_id);

	IF svcenvt.MAX_NUM_MEMBERS IS NOT NULL THEN
		select count(*)
		  into tally
		  from svc_environment_coll_svc_env
		  where service_env_collection_id = NEW.service_env_collection_id;
		IF tally > svcenvt.MAX_NUM_MEMBERS THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF svcenvt.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		select count(*)
		  into tally
		  from svc_environment_coll_svc_env
		  		inner join service_environment_collection 
					USING (service_env_collection_id)
		  where service_environment_id = NEW.service_environment_id
		  and	service_env_collection_type = 
					svcenvt.service_env_collection_type;
		IF tally > svcenvt.MAX_NUM_COLLECTIONS THEN
			RAISE EXCEPTION 'Service Environment may not be a member of more than % collections of type %',
				svcenvt.MAX_NUM_COLLECTIONS, svcenvt.service_env_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'token_collection_hier_enforce');
CREATE OR REPLACE FUNCTION jazzhands.token_collection_hier_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	tct	val_token_collection_type%ROWTYPE;
BEGIN
	SELECT *
	INTO	tct
	FROM	val_token_collection_type
	WHERE	token_collection_type =
		(select token_collection_type from token_collection
			where token_collection_id = NEW.token_collection_id);

	IF tct.can_have_hierarchy = 'N' THEN
		RAISE EXCEPTION 'Token Collections of type % may not be hierarcical',
			tct.token_collection_type
			USING ERRCODE= 'unique_violation';
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'token_collection_member_enforce');
CREATE OR REPLACE FUNCTION jazzhands.token_collection_member_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	tct	val_token_collection_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	tct
	FROM	val_token_collection_type
	WHERE	token_collection_type =
		(select token_collection_type from token_collection
			where token_collection_id = NEW.token_collection_id);

	IF tct.MAX_NUM_MEMBERS IS NOT NULL THEN
		select count(*)
		  into tally
		  from token_collection_token
		  where token_collection_id = NEW.token_collection_id;
		IF tally > tct.MAX_NUM_MEMBERS THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF tct.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		select count(*)
		  into tally
		  from token_collection_token
		  		inner join token_collection using (token_collection_id)
		  where token_id = NEW.token_id
		  and	token_collection_type = tct.token_collection_type;
		IF tally > tct.MAX_NUM_COLLECTIONS THEN
			RAISE EXCEPTION 'Token may not be a member of more than % collections of type %',
				tct.MAX_NUM_COLLECTIONS, tct.token_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'upd_v_corp_family_account');
CREATE OR REPLACE FUNCTION jazzhands.upd_v_corp_family_account()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	acct_realm_id	account_realm.account_realm_id%TYPE;
	setstr		TEXT;
	_r		RECORD;
	val		TEXT;
BEGIN
	SELECT	account_realm_id
	INTO	acct_realm_id
	FROM	property
	WHERE	property_name = '_root_account_realm_id'
	AND	property_type = 'Defaults';

	IF acct_realm_id != OLD.account_realm_id OR
			acct_realm_id != NEW.account_realm_id THEN
		RAISE EXCEPTION 'Invalid account_realm_id'
		USING ERRCODE = 'foreign_key_violation';
	END IF;

	setstr = '';
	FOR _r IN SELECT * FROM json_each_text( row_to_json(NEW) )
	LOOP
		IF _r.key NOT SIMILAR TO 'data_(ins|upd)_(user|date)' THEN
			EXECUTE 'SELECT ' || _r.key ||' FROM account
				WHERE account_id = ' || OLD.account_id
				INTO val;
			IF ( _r.value IS NULL  AND val IS NOT NULL) OR
				( _r.value IS NOT NULL AND val IS NULL) OR
				(_r.value::text NOT SIMILAR TO val::text) THEN
				-- RAISE NOTICE 'Changing %: "%" to "%"', _r.key, val, _r.value;
				IF char_length(setstr) > 0 THEN
					setstr = setstr || ',
					';
				END IF;
				IF _r.value IS NOT  NULL THEN
					setstr = setstr || _r.key || ' = ' ||  
						quote_nullable(_r.value) || ' ' ;
				ELSE
					setstr = setstr || _r.key || ' = ' ||  
						' NULL ' ;
				END IF;
			END IF;
		END IF;
	END LOOP;


	IF char_length(setstr) > 0 THEN
		setstr = 'UPDATE account SET ' || setstr || '
			WHERE	account_id = ' || OLD.account_id;
		-- RAISE NOTICE 'executing %', setstr;
		EXECUTE setstr;
	END IF;
	RETURN NEW;

END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'upd_v_hotpants_token');
CREATE OR REPLACE FUNCTION jazzhands.upd_v_hotpants_token()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	acct_realm_id	account_realm.account_realm_id%TYPE;
BEGIN
	IF OLD.token_sequence IS DISTINCT FROM NEW.token_sequence THEN
		PERFORM token_utils.set_sequence(
			p_token_id := NEW.token_id,
			p_token_sequence := NEW.token_sequence,
			p_reset_time := NEW.last_updated::timestamp
		);
	END IF;

	IF OLD.bad_logins IS DISTINCT FROM NEW.bad_logins THEN
		PERFORM token_utils.set_lock_status(
			p_token_id := NEW.token_id,
			p_lock_status := NEW.is_token_locked,
			p_unlock_time := NEW.token_unlock_time,
			p_bad_logins := NEW.bad_logins,
			p_last_updated :=NEW.last_updated::timestamp
		);
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'update_peraccount_account_collection');
CREATE OR REPLACE FUNCTION jazzhands.update_peraccount_account_collection()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	def_acct_rlm	account_realm.account_realm_id%TYPE;
	acid			account_collection.account_collection_id%TYPE;
DECLARE
	newname	TEXT;
BEGIN
	newname = concat(NEW.login, '_', NEW.account_id);
	if TG_OP = 'INSERT' THEN
		insert into account_collection
			(account_collection_name, account_collection_type)
		values
			(newname, 'per-account')
		RETURNING account_collection_id INTO acid;
		insert into account_collection_account
			(account_collection_id, account_id)
		VALUES
			(acid, NEW.account_id);
	END IF;

	IF TG_OP = 'UPDATE' AND OLD.login != NEW.login THEN
		UPDATE	account_collection
		    set	account_collection_name = newname
		  where	account_collection_type = 'per-account'
		    and	account_collection_id = (
				SELECT	account_collection_id
		  		FROM	account_collection ac
						INNER JOIN account_collection_account aca
							USING (account_collection_id)
		 		WHERE	aca.account_id = OLD.account_Id
		   		AND	ac.account_collection_type = 'per-account'
			);
	END IF;
	return NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'val_person_status_enabled_migration_enforce');
CREATE OR REPLACE FUNCTION jazzhands.val_person_status_enabled_migration_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	IF TG_OP = 'INSERT' THEN
		IF ( NEW.is_disabled IS NOT NULL AND NEW.is_enabled IS NOT NULL ) THEN
			RAISE EXCEPTION 'May not set both IS_ENABLED and IS_DISABLED.  Set IS_ENABLED only.'
				USING errcode = 'integrity_constraint_violation';
		END IF;

		IF NEW.is_enabled IS NOT NULL THEN
			IF NEW.is_enabled = 'Y' THEN
				NEW.is_disabled := 'N';
			ELSE
				NEW.is_disabled := 'Y';
			END IF;
		ELSIF NEW.is_disabled IS NOT NULL THEN
			IF NEW.is_disabled = 'Y' THEN
				NEW.is_enabled := 'N';
			ELSE
				NEW.is_enabled := 'Y';
			END IF;
		END IF;
	ELSIF TG_OP = 'UPDATE' THEN
		IF ( OLD.is_disabled != NEW.is_disabled AND
				OLD.is_enabled != NEW.is_enabled ) THEN
			RAISE EXCEPTION 'May not update both IS_ENABLED and IS_DISABLED.  Update IS_ENABLED only.'
				USING errcode = 'integrity_constraint_violation';
		END IF;

		IF OLD.is_enabled != NEW.is_enabled THEN
			IF NEW.is_enabled = 'Y' THEN
				NEW.is_disabled := 'N';
			ELSE
				NEW.is_disabled := 'Y';
			END IF;
		ELSIF OLD.is_disabled != NEW.is_disabled THEN
			IF NEW.is_disabled = 'Y' THEN
				NEW.is_enabled := 'N';
			ELSE
				NEW.is_enabled := 'Y';
			END IF;
		END IF;
	END IF;

	IF NEW.is_enabled = NEW.is_disabled THEN
		RAISE NOTICE 'is_enabled=is_disabled.  This should never happen'
			USING  errcode = 'integrity_constraint_violation';
	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'validate_account_collection_type_change');
CREATE OR REPLACE FUNCTION jazzhands.validate_account_collection_type_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally	integer;
BEGIN
	IF OLD.account_collection_type != NEW.account_collection_type THEN
		SELECT	COUNT(*)
		INTO	_tally
		FROM	property p
			join val_property vp USING (property_name,property_type)
		WHERE	vp.account_collection_type = OLD.account_collection_type
		AND	p.account_collection_id = NEW.account_collection_id;

		IF _tally > 0 THEN
			RAISE EXCEPTION 'account_collection % of type % is used by % restricted properties.',
				NEW.account_collection_id, NEW.account_collection_type, _tally
				USING ERRCODE = 'foreign_key_violation';
		END IF;
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'validate_company_collection_type_change');
CREATE OR REPLACE FUNCTION jazzhands.validate_company_collection_type_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally	integer;
BEGIN
	IF OLD.company_collection_type != NEW.company_collection_type THEN
		SELECT	COUNT(*)
		INTO	_tally
		FROM	property p
			join val_property vp USING (property_name,property_type)
		WHERE	vp.company_collection_type = OLD.company_collection_type
		AND	p.company_collection_id = NEW.company_collection_id;

		IF _tally > 0 THEN
			RAISE EXCEPTION 'company_collection % of type % is used by % restricted properties.',
				NEW.company_collection_id, NEW.company_collection_type, _tally
				USING ERRCODE = 'foreign_key_violation';
		END IF;
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'validate_device_collection_type_change');
CREATE OR REPLACE FUNCTION jazzhands.validate_device_collection_type_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally	integer;
BEGIN
	IF OLD.device_collection_type != NEW.device_collection_type THEN
		SELECT	COUNT(*)
		INTO	_tally
		FROM	property p
			join val_property vp USING (property_name,property_type)
		WHERE	vp.device_collection_type = OLD.device_collection_type
		AND	p.device_collection_id = NEW.device_collection_id;

		IF _tally > 0 THEN
			RAISE EXCEPTION 'device_collection % of type % is used by % restricted properties.',
				NEW.device_collection_id, NEW.device_collection_type, _tally
				USING ERRCODE = 'foreign_key_violation';
		END IF;
	END IF;
	RETURN NEW;	
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'validate_dns_domain_collection_type_change');
CREATE OR REPLACE FUNCTION jazzhands.validate_dns_domain_collection_type_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally	integer;
BEGIN
	IF OLD.dns_domain_collection_type != NEW.dns_domain_collection_type THEN
		SELECT	COUNT(*)
		INTO	_tally
		FROM	property p
			join val_property vp USING (property_name,property_type)
		WHERE	vp.dns_domain_collection_type = OLD.dns_domain_collection_type
		AND	p.dns_domain_collection_id = NEW.dns_domain_collection_id;

		IF _tally > 0 THEN
			RAISE EXCEPTION 'dns_domain_collection % of type % is used by % restricted properties.',
				NEW.dns_domain_collection_id, NEW.dns_domain_collection_type, _tally
				USING ERRCODE = 'foreign_key_violation';
		END IF;
	END IF;
	RETURN NEW;	
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'validate_layer2_network_collection_type_change');
CREATE OR REPLACE FUNCTION jazzhands.validate_layer2_network_collection_type_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally	integer;
BEGIN
	IF OLD.layer2_network_collection_type != NEW.layer2_network_collection_type THEN
		SELECT	COUNT(*)
		INTO	_tally
		FROM	property p
			join val_property vp USING (property_name,property_type)
		WHERE	vp.layer2_network_collection_type = OLD.layer2_network_collection_type
		AND	p.layer2_network_collection_id = NEW.layer2_network_collection_id;

		IF _tally > 0 THEN
			RAISE EXCEPTION 'layer2_network_collection % of type % is used by % restricted properties.',
				NEW.layer2_network_collection_id, NEW.layer2_network_collection_type, _tally
				USING ERRCODE = 'foreign_key_violation';
		END IF;
	END IF;
	RETURN NEW;	
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'validate_layer3_network_collection_type_change');
CREATE OR REPLACE FUNCTION jazzhands.validate_layer3_network_collection_type_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally	integer;
BEGIN
	IF OLD.layer3_network_collection_type != NEW.layer3_network_collection_type THEN
		SELECT	COUNT(*)
		INTO	_tally
		FROM	property p
			join val_property vp USING (property_name,property_type)
		WHERE	vp.layer3_network_collection_type = OLD.layer3_network_collection_type
		AND	p.layer3_network_collection_id = NEW.layer3_network_collection_id;

		IF _tally > 0 THEN
			RAISE EXCEPTION 'layer3_network_collection % of type % is used by % restricted properties.',
				NEW.layer3_network_collection_id, NEW.layer3_network_collection_type, _tally
				USING ERRCODE = 'foreign_key_violation';
		END IF;
	END IF;
	RETURN NEW;	
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'validate_netblock_collection_type_change');
CREATE OR REPLACE FUNCTION jazzhands.validate_netblock_collection_type_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally	integer;
BEGIN
	IF OLD.netblock_collection_type != NEW.netblock_collection_type THEN
		SELECT	COUNT(*)
		INTO	_tally
		FROM	property p
			join val_property vp USING (property_name,property_type)
		WHERE	vp.netblock_collection_type = OLD.netblock_collection_type
		AND	p.netblock_collection_id = NEW.netblock_collection_id;

		IF _tally > 0 THEN
			RAISE EXCEPTION 'netblock_collection % of type % is used by % restricted properties.',
				NEW.netblock_collection_id, NEW.netblock_collection_type, _tally
				USING ERRCODE = 'foreign_key_violation';
		END IF;
	END IF;
	RETURN NEW;	
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'validate_network_range');
CREATE OR REPLACE FUNCTION jazzhands.validate_network_range()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	v_nrt	val_network_range_type%ROWTYPE;
BEGIN
	SELECT	*
	INTO	v_nrt
	FROM	val_network_range_type
	WHERE	network_range_type = NEW.network_range_type;

	IF NEW.dns_domain_id IS NULL AND v_nrt.dns_domain_required = 'REQUIRED' THEN
		RAISE EXCEPTION 'For type %, dns_domain_id is required.',
			NEW.network_range_type
			USING ERRCODE = 'not_null_violation';
	ELSIF NEW.dns_domain_id IS NOT NULL AND
			v_nrt.dns_domain_required = 'PROHIBITED' THEN
		RAISE EXCEPTION 'For type %, dns_domain_id is prohibited.',
			NEW.network_range_type
			USING ERRCODE = 'not_null_violation';
	END IF;
	RETURN NEW;
END; $function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'validate_property_collection_type_change');
CREATE OR REPLACE FUNCTION jazzhands.validate_property_collection_type_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally	integer;
BEGIN
	IF OLD.property_collection_type != NEW.property_collection_type THEN
		SELECT	COUNT(*)
		INTO	_tally
		FROM	property p
			join val_property vp USING (property_name,property_type)
		WHERE	vp.property_collection_type = OLD.property_collection_type
		AND	p.property_collection_id = NEW.property_collection_id;

		IF _tally > 0 THEN
			RAISE EXCEPTION 'property_collection % of type % is used by % restricted properties.',
				NEW.property_collection_id, NEW.property_collection_type, _tally
				USING ERRCODE = 'foreign_key_violation';
		END IF;
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'validate_service_env_collection_type_change');
CREATE OR REPLACE FUNCTION jazzhands.validate_service_env_collection_type_change()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally	integer;
BEGIN
	IF OLD.service_env_collection_type != NEW.service_env_collection_type THEN
		SELECT	COUNT(*)
		INTO	_tally
		FROM	property p
			join val_property vp USING (property_name,property_type)
		WHERE	vp.service_env_collection_type = OLD.service_env_collection_type
		AND	p.service_env_collection_id = NEW.service_env_collection_id;

		IF _tally > 0 THEN
			RAISE EXCEPTION 'service_env_collection % of type % is used by % restricted properties.',
				NEW.service_env_collection_id, NEW.service_env_collection_type, _tally
				USING ERRCODE = 'foreign_key_violation';
		END IF;
	END IF;
	RETURN NEW;	
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'validate_val_network_range_type');
CREATE OR REPLACE FUNCTION jazzhands.validate_val_network_range_type()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	IF NEW.dns_domain_required = 'REQUIRED' THEN
		PERFORM
		FROM	network_range
		WHERE	network_range_type = NEW.network_range_type
		AND		dns_domain_id IS NULL;

		IF FOUND THEN
			RAISE EXCEPTION 'dns_domain_id is not set on some ranges'
				USING ERRCODE = 'not_null_violation';
		END IF;
	ELSIF NEW.dns_domain_required = 'PROHIBITED' THEN
		PERFORM
		FROM	network_range
		WHERE	network_range_type = NEW.network_range_type
		AND		dns_domain_id IS NOT NULL;

		IF FOUND THEN
			RAISE EXCEPTION 'dns_domain_id is set on some ranges'
				USING ERRCODE = 'not_null_violation';
		END IF;
	END IF;
	RETURN NEW;
END; $function$
;

--
-- Process trigger procs in net_manip
--
--
-- Process trigger procs in network_strings
--
--
-- Process trigger procs in time_util
--
--
-- Process trigger procs in dns_utils
--
--
-- Process trigger procs in person_manip
--
--
-- Process trigger procs in auto_ac_manip
--
--
-- Process trigger procs in company_manip
--
--
-- Process trigger procs in token_utils
--
--
-- Process trigger procs in port_support
--
--
-- Process trigger procs in port_utils
--
--
-- Process trigger procs in device_utils
--
--
-- Process trigger procs in netblock_utils
--
--
-- Process trigger procs in netblock_manip
--
--
-- Process trigger procs in physical_address_utils
--
--
-- Process trigger procs in component_utils
--
--
-- Process trigger procs in snapshot_manip
--
--
-- Process trigger procs in lv_manip
--
--
-- Process trigger procs in approval_utils
--
--
-- Process trigger procs in account_collection_manip
--
--
-- Process trigger procs in schema_support
--
--
-- Process trigger procs in script_hooks
--
-- Dropping obsoleted sequences....


-- Dropping obsoleted audit sequences....


-- Processing tables with no structural changes
-- Some of these may be redundant
-- fk constraints
ALTER TABLE person_company_attr DROP CONSTRAINT IF EXISTS fk_pers_comp_attr_person_comp_;
ALTER TABLE person_company_attr
	ADD CONSTRAINT fk_pers_comp_attr_person_comp_
	FOREIGN KEY (company_id, person_id) REFERENCES person_company(company_id, person_id) DEFERRABLE;

-- index
-- triggers

--- misc
-- backwards compatibility
insert into val_property (
        property_name, property_type, is_multivalue, property_data_type,
        description,
        permit_account_realm_id
) values (
        'login_restriction', 'Defaults', 'N', 'string',
        'per-account realm validation of login names',
        'REQUIRED'
);
INSERT INTO property (
	property_type, property_name, property_value, account_realm_id
) SELECT 'Defaults', 'login_restriction', '[^-/@a-z0-9_]+', account_realm_id
FROM account_realm;

COMMENT ON SCHEMA account_collection_manip IS 'part of jazzhands';
COMMENT ON SCHEMA script_hooks IS 'part of jazzhands';


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
