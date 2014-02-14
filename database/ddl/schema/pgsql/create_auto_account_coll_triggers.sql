/*
 * Copyright (c) 2013-2014 Todd Kover
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

CREATE OR REPLACE FUNCTION automated_ac() RETURNS TRIGGER AS $_$
DECLARE
	acr	VARCHAR;
	c_name VARCHAR;
	sc VARCHAR;
	ac_ids INTEGER[];
	delete_aca BOOLEAN;
	_gender VARCHAR;
	_person_company RECORD;
	acr_c_name VARCHAR;
	gender_string VARCHAR;
	_status RECORD;
BEGIN
	IF TG_OP = 'INSERT' THEN
		IF NEW.account_role != 'primary' THEN
			RETURN NEW;
		END IF;
		PERFORM 1 FROM jazzhands.val_person_status WHERE NEW.account_status = person_status AND is_disabled = 'N';
		IF NOT FOUND THEN
			RETURN NEW;
		END IF;
	-- The triggers need not deal with account realms companies or sites being renamed, although we may want to revisit this later.
	ELSIF NEW.account_id != OLD.account_id THEN
		RAISE NOTICE 'This trigger does not handle changing account id';
		RETURN NEW;
	ELSIF NEW.account_realm_id != OLD.account_realm_id THEN
		RAISE NOTICE 'This trigger does not handle changing account_realm_id';
		RETURN NEW;
	ELSIF NEW.company_id != OLD.company_id THEN
		RAISE NOTICE 'This trigger does not handle changing company_id';
		RETURN NEW;
	END IF;
	ac_ids = '{-1,-1,-1,-1,-1,-1,-1}';
	SELECT account_realm_name INTO acr FROM jazzhands.account_realm WHERE account_realm_id = NEW.account_realm_id;
	ac_ids[0] = acct_coll_manip.get_automated_account_collection_id(acr || '_' || NEW.account_type);
	SELECT company_short_name INTO c_name FROM jazzhands.company WHERE company_id = NEW.company_id AND company_short_name IS NOT NULL;
	IF NOT FOUND THEN
		RAISE NOTICE 'Company short name cannot be determined from company_id % in %', NEW.company_id, TG_NAME;
	ELSE
		acr_c_name = acr || '_' || c_name;
		ac_ids[1] = acct_coll_manip.get_automated_account_collection_id(acr_c_name || '_' || NEW.account_type);
		SELECT
			pc.*
		INTO
			_person_company
		FROM
			jazzhands.person_company pc
		JOIN
			jazzhands.account a
		USING
			(person_id)
		WHERE
			a.person_id != 0 AND account_id = NEW.account_id;
		IF FOUND THEN
			IF _person_company.is_exempt IS NOT NULL THEN
				SELECT * INTO _status FROM acct_coll_manip.person_company_flags_to_automated_ac_name(_person_company.is_exempt, 'exempt');
				-- will remove account from old account collection
				ac_ids[2] = acct_coll_manip.get_automated_account_collection_id(acr_c_name || '_' || _status.name);
			END IF;
			SELECT * INTO _status FROM acct_coll_manip.person_company_flags_to_automated_ac_name(_person_company.is_full_time, 'full_time');
			ac_ids[3] = acct_coll_manip.get_automated_account_collection_id(acr_c_name || '_' || _status.name);
			SELECT * INTO _status FROM acct_coll_manip.person_company_flags_to_automated_ac_name(_person_company.is_management, 'management');
			ac_ids[4] = acct_coll_manip.get_automated_account_collection_id(acr_c_name || '_' || _status.name);
		END IF;
		SELECT
			gender
		INTO
			_gender
		FROM
			jazzhands.person
		JOIN
			jazzhands.account a
		USING
			(person_id)
		WHERE
			account_id = NEW.account_id AND a.person_id !=0 AND gender IS NOT NULL;
		IF FOUND THEN
			gender_string = acct_coll_manip.person_gender_char_to_automated_ac_name(_gender);
			IF gender_string IS NOT NULL THEN
				ac_ids[5] = acct_coll_manip.get_automated_account_collection_id(acr_c_name || '_' || gender_string);
			END IF;
		END IF;
	END IF;
	SELECT site_code INTO sc FROM jazzhands.person_location WHERE person_id = NEW.person_id AND site_code IS NOT NULL;
	IF FOUND THEN
		ac_ids[6] = acct_coll_manip.get_automated_account_collection_id(acr || '_' || sc);
	END IF;
	delete_aca = 't';
	IF TG_OP = 'INSERT' THEN
		delete_aca = 'f';
	ELSE
		IF NEW.account_role != 'primary' AND NEW.account_role != OLD.account_role THEN
			-- reaching here means account must be removed from all automated account collections
			PERFORM acct_coll_manip.insert_or_delete_automated_ac('t', OLD.account_id, ac_ids);
			RETURN NEW;
		END IF;
		PERFORM 1 FROM jazzhands.val_person_status WHERE NEW.account_status = person_status AND is_disabled = 'N';
		IF NOT FOUND THEN
			-- reaching here means account must be removed from all automated account collections
			PERFORM acct_coll_manip.insert_or_delete_automated_ac('t', OLD.account_id, ac_ids);
			RETURN NEW;
		END IF;
		IF NEW.account_role = 'primary' AND NEW.account_role != OLD.account_role OR
			NEW.account_status != OLD.account_status THEN
			-- reaching here means there were no automated account collection for this account
			-- and this is the first time this account goes into the automated collections even though this is not SQL insert
			-- notice that NEW.account_status here is 'enabled' or similar type
			delete_aca = 'f';
		END IF;
	END IF;
	IF NOT delete_aca THEN
		-- do all inserts
		PERFORM acct_coll_manip.insert_or_delete_automated_ac('f', NEW.account_id, ac_ids);
	END IF;
	RETURN NEW;
END;
$_$ LANGUAGE plpgsql SECURITY DEFINER;
DROP TRIGGER IF EXISTS trig_automated_ac ON account;
CREATE TRIGGER trig_automated_ac AFTER INSERT OR UPDATE ON account FOR EACH ROW EXECUTE PROCEDURE automated_ac();

CREATE OR REPLACE FUNCTION automated_ac_on_person_company() RETURNS TRIGGER AS $_$
DECLARE
	ac_id INTEGER[];
	c_name VARCHAR;
	old_acr_c_name VARCHAR;
	acr_c_name VARCHAR;
	exempt_status RECORD;
	new_exempt_status RECORD;
	full_time_status RECORD;
	manager_status RECORD;
	old_r RECORD;
	r RECORD;
BEGIN
	-- at this time person_company.is_exempt column can be null.
	-- take into account of is_exempt going from null to not null
	IF (NEW.is_exempt IS NOT NULL AND OLD.is_exempt IS NOT NULL AND NEW.is_exempt = OLD.is_exempt OR NEW.is_exempt IS NULL AND OLD.is_exempt IS NULL)
		AND NEW.is_management = OLD.is_management AND NEW.is_full_time = OLD.is_full_time
		OR (NEW.person_id = 0 AND OLD.person_id = 0) THEN
		RETURN NEW;
	END IF;
	IF NEW.person_id != OLD.person_id THEN
		RAISE NOTICE 'This trigger % does not support changing person_id', TG_NAME;
		RETURN NEW;
	ELSIF NEW.company_id != OLD.company_id THEN
		RAISE NOTICE 'This trigger % does not support changing company_id', TG_NAME;
		RETURN NEW;
	END IF;
	SELECT company_short_name INTO c_name FROM jazzhands.company WHERE company_id = OLD.company_id AND company_short_name IS NOT NULL;
	IF NOT FOUND THEN
		RAISE NOTICE 'Company short name cannot be determined from company_id % in trigger %', OLD.company_id, TG_NAME;
		RETURN NEW;
	END IF;
	FOR old_r
		IN SELECT
			account_realm_name, account_id
		FROM
			jazzhands.account_realm ar
		JOIN
			jazzhands.account a
		USING
			(account_realm_id)
		JOIN
			jazzhands.val_person_status vps
		ON
			account_status = vps.person_status AND vps.is_disabled='N'
		WHERE
			a.person_id = OLD.person_id AND a.company_id = OLD.company_id
	LOOP
		old_acr_c_name = old_r.account_realm_name || '_' || c_name;
		IF coalesce(NEW.is_exempt, '') != coalesce(OLD.is_exempt, '') THEN
			IF OLD.is_exempt IS NOT NULL THEN
				SELECT * INTO exempt_status FROM acct_coll_manip.person_company_flags_to_automated_ac_name(OLD.is_exempt, 'exempt');
				DELETE FROM jazzhands.account_collection_account WHERE account_id = old_r.account_id
					AND account_collection_id = acct_coll_manip.get_automated_account_collection_id(old_acr_c_name || '_' || exempt_status.name);
			END IF;
		END IF;
		IF NEW.is_full_time != OLD.is_full_time THEN
			SELECT * INTO full_time_status FROM acct_coll_manip.person_company_flags_to_automated_ac_name(OLD.is_full_time, 'full_time');
			DELETE FROM jazzhands.account_collection_account WHERE account_id = old_r.account_id
				AND account_collection_id = acct_coll_manip.get_automated_account_collection_id(old_acr_c_name || '_' || full_time_status.name);
		END IF;
		IF NEW.is_management != OLD.is_management THEN
			SELECT * INTO manager_status FROM acct_coll_manip.person_company_flags_to_automated_ac_name(OLD.is_management, 'management');
			DELETE FROM jazzhands.account_collection_account WHERE account_id = old_r.account_id
				AND account_collection_id = acct_coll_manip.get_automated_account_collection_id(old_acr_c_name || '_' || manager_status.name);
		END IF;
		-- looping over the same set of data.  TODO: optimize for speed
		FOR r
			IN SELECT
				account_realm_name, account_id
			FROM
				jazzhands.account_realm ar
			JOIN
				jazzhands.account a
			USING
				(account_realm_id)
			JOIN
				jazzhands.val_person_status vps
			ON
				account_status = vps.person_status AND vps.is_disabled='N'
			WHERE
				a.person_id = NEW.person_id AND a.company_id = NEW.company_id
		LOOP
			acr_c_name = r.account_realm_name || '_' || c_name;
			IF coalesce(NEW.is_exempt, '') != coalesce(OLD.is_exempt, '') THEN
				IF NEW.is_exempt IS NOT NULL THEN
					SELECT * INTO new_exempt_status FROM acct_coll_manip.person_company_flags_to_automated_ac_name(NEW.is_exempt, 'exempt');
					ac_id[0] = acct_coll_manip.get_automated_account_collection_id(acr_c_name || '_' || new_exempt_status.name);
					PERFORM acct_coll_manip.insert_or_delete_automated_ac('f', r.account_id, ac_id);
				END IF;
			END IF;
			IF NEW.is_full_time != OLD.is_full_time THEN
				ac_id[0] = acct_coll_manip.get_automated_account_collection_id(acr_c_name || '_' || full_time_status.non_name);
				PERFORM acct_coll_manip.insert_or_delete_automated_ac('f', r.account_id, ac_id);
			END IF;
			IF NEW.is_management != OLD.is_management THEN
				ac_id[0] = acct_coll_manip.get_automated_account_collection_id(acr_c_name || '_' || manager_status.non_name);
				PERFORM acct_coll_manip.insert_or_delete_automated_ac('f', r.account_id, ac_id);
			END IF;
		END LOOP;
	END LOOP;
	RETURN NEW;
END;
$_$ LANGUAGE plpgsql SECURITY DEFINER;
DROP TRIGGER IF EXISTS trigger_automated_ac_on_person_company ON person_company;
CREATE TRIGGER trigger_automated_ac_on_person_company 
	AFTER UPDATE ON person_company 
	FOR EACH ROW EXECUTE PROCEDURE 
	automated_ac_on_person_company();

CREATE OR REPLACE FUNCTION automated_ac_on_person() RETURNS TRIGGER AS $_$
DECLARE
	ac_id INTEGER[];
	c_name VARCHAR;
	old_c_name VARCHAR;
	old_acr_c_name VARCHAR;
	acr_c_name VARCHAR;
	gender_string VARCHAR;
	r RECORD;
	old_r RECORD;
BEGIN
	IF NEW.gender = OLD.gender OR NEW.person_id = 0 AND OLD.person_id = 0 THEN
		RETURN NEW;
	END IF;
	IF OLD.person_id != NEW.person_id THEN
		RAISE NOTICE 'This trigger % does not support changing person_id.  old person_id % new person_id %', TG_NAME, OLD.person_id, NEW.person_id;
		RETURN NEW;
	END IF;
	FOR old_r
		IN SELECT
			account_realm_name, account_id, company_id
		FROM
			jazzhands.account_realm ar
		JOIN
			jazzhands.account a
		USING
			(account_realm_id)
		JOIN
			jazzhands.val_person_status vps
		ON
			account_status = vps.person_status AND vps.is_disabled='N'
		WHERE
			a.person_id = OLD.person_id
	LOOP
		SELECT company_short_name INTO old_c_name FROM jazzhands.company WHERE company_id = old_r.company_id AND company_short_name IS NOT NULL;
		IF FOUND THEN
			old_acr_c_name = old_r.account_realm_name || '_' || old_c_name;
			gender_string = acct_coll_manip.person_gender_char_to_automated_ac_name(OLD.gender);
			IF gender_string IS NOT NULL THEN
				DELETE FROM jazzhands.account_collection_account WHERE account_id = old_r.account_id
					AND account_collection_id = acct_coll_manip.get_automated_account_collection_id(old_acr_c_name || '_' ||  gender_string);
			END IF;
		ELSE
			RAISE NOTICE 'Company short name cannot be determined from company_id % in %', old_r.company_id, TG_NAME;
		END IF;
		-- looping over the same set of data.  TODO: optimize for speed
		FOR r
			IN SELECT
				account_realm_name, account_id, company_id
			FROM
				jazzhands.account_realm ar
			JOIN
				jazzhands.account a
			USING
				(account_realm_id)
			JOIN
				jazzhands.val_person_status vps
			ON
				account_status = vps.person_status AND vps.is_disabled='N'
			WHERE
				a.person_id = NEW.person_id
		LOOP
			IF old_r.company_id = r.company_id THEN
				IF old_c_name IS NULL THEN
					RAISE NOTICE 'The new company short name is null like the old company short name. Going to the next record if there is any';
					CONTINUE;
				END IF;
				c_name = old_c_name;
			ELSE
				SELECT company_short_name INTO c_name FROM jazzhands.company WHERE company_id = r.company_id AND company_short_name IS NOT NULL;
				IF NOT FOUND THEN
					RAISE NOTICE 'New company short name cannot be determined from company_id % in %', r.company_id, TG_NAME;
					CONTINUE;
				END IF;
			END IF;
			acr_c_name = r.account_realm_name || '_' || c_name;
			gender_string = acct_coll_manip.person_gender_char_to_automated_ac_name(NEW.gender);
			IF gender_string IS NULL THEN
				CONTINUE;
			END IF;
			ac_id[0] = acct_coll_manip.get_automated_account_collection_id(acr_c_name || '_' || gender_string);
			PERFORM acct_coll_manip.insert_or_delete_automated_ac('f', r.account_id, ac_id);
		END LOOP;
	END LOOP;
	RETURN NEW;
END;
$_$ LANGUAGE plpgsql SECURITY DEFINER;
DROP TRIGGER IF EXISTS trigger_automated_ac_on_person ON person;
CREATE TRIGGER trigger_automated_ac_on_person 
	AFTER UPDATE ON person 
	FOR EACH ROW 
	EXECUTE PROCEDURE automated_ac_on_person();

CREATE OR REPLACE FUNCTION automated_realm_site_ac_pl() RETURNS TRIGGER AS $_$
DECLARE
	sc VARCHAR;
	r RECORD;
	ac_id INTEGER;
	ac_name VARCHAR;
	p_id INTEGER;
BEGIN
	IF TG_OP = 'UPDATE' THEN
		IF NEW.person_location_id != OLD.person_location_id THEN
			RAISE NOTICE 'This trigger % does not support changing person_location_id', TG_NAME;
			RETURN NEW;
		END IF;
		IF NEW.person_id IS NOT NULL AND OLD.person_id IS NOT NULL AND NEW.person_id != OLD.person_id THEN
			RAISE NOTICE 'This trigger % does not support changing person_id', TG_NAME;
			RETURN NEW;
		END IF;
		IF NEW.person_id IS NULL OR OLD.person_id IS NULL THEN
			-- setting person_id to NULL is done by 'usermgr merge'
			-- RAISE NOTICE 'This trigger % does not support null person_id', TG_NAME;
			RETURN NEW;
		END IF;
		IF NEW.site_code IS NOT NULL AND OLD.site_code IS NOT NULL AND NEW.site_code = OLD.site_code
			OR NEW.person_location_type != 'office' AND OLD.person_location_type != 'office' THEN
			RETURN NEW;
		END IF;
	END IF;

	IF TG_OP = 'INSERT' AND NEW.person_location_type != 'office' THEN
		RETURN NEW;
	END IF;

	IF TG_OP = 'DELETE' THEN
		IF OLD.person_location_type != 'office' THEN
			RETURN OLD;
		END IF;
		p_id = OLD.person_id;
		sc = OLD.site_code;
	ELSE
		p_id = NEW.person_id;
		sc = NEW.site_code;
	END IF;

	FOR r IN SELECT account_realm_name, account_id
		FROM
			jazzhands.account_realm ar
		JOIN
			jazzhands.account a
		ON
			ar.account_realm_id=a.account_realm_id AND a.account_role = 'primary' AND a.person_id = p_id 
		JOIN
			jazzhands.val_person_status vps
		ON
			vps.person_status = a.account_status AND vps.is_disabled='N'
		JOIN
			jazzhands.site s
		ON
			s.site_code = sc AND a.company_id = s.colo_company_id
	LOOP
		IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
			ac_name = r.account_realm_name || '_' || sc;
			ac_id = acct_coll_manip.get_automated_account_collection_id( r.account_realm_name || '_' || sc );
			IF TG_OP != 'UPDATE' OR NEW.person_location_type = 'office' THEN
				PERFORM 1 FROM jazzhands.account_collection_account WHERE account_collection_id = ac_id AND account_id = r.account_id;
				IF NOT FOUND THEN
					INSERT INTO account_collection_account (account_collection_id, account_id) VALUES (ac_id, r.account_id);
				END IF;
			END IF;
		END IF;
		IF TG_OP = 'UPDATE' OR TG_OP = 'DELETE' THEN
			IF OLD.site_code IS NULL THEN
				CONTINUE;
			END IF;
			ac_name = r.account_realm_name || '_' || OLD.site_code;
			SELECT account_collection_id INTO ac_id FROM jazzhands.account_collection WHERE account_collection_name = ac_name AND account_collection_type ='automated';
			IF NOT FOUND THEN
				RAISE NOTICE 'Account collection name % of type "automated" not found in %', ac_name, TG_NAME;
				CONTINUE;
			END IF;
			DELETE FROM jazzhands.account_collection_account WHERE account_collection_id = ac_id AND account_id = r.account_id;
		END IF;
	END LOOP;
	IF TG_OP = 'DELETE' THEN
		RETURN OLD;
	END IF;
	RETURN NEW;
END;
$_$ LANGUAGE plpgsql SECURITY DEFINER;
DROP TRIGGER IF EXISTS trig_automated_realm_site_ac_pl ON person_location;
CREATE TRIGGER trig_automated_realm_site_ac_pl AFTER DELETE OR INSERT OR UPDATE ON person_location FOR EACH ROW EXECUTE PROCEDURE automated_realm_site_ac_pl();
