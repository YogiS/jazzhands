/*
Invoked:

	--scan-tables
	--suffix=v70
*/

\set ON_ERROR_STOP
SELECT schema_support.begin_maintenance();
select timeofday(), now();
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
--
-- Process middle (non-trigger) schema auto_ac_manip
--
--
-- Process middle (non-trigger) schema company_manip
--
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

	WITH ni AS  (
		delete from network_interface where device_id = in_Device_id
		RETURNING *
	) delete from network_interface_netblock where network_interface_id 
		IN (
			SELECT network_interface_id
		 	FROM ni
		); 

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
-- Changed function
SELECT schema_support.save_grants_for_replay('netblock_utils', 'find_free_netblocks');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS netblock_utils.find_free_netblocks ( parent_netblock_list integer[], netmask_bits integer, single_address boolean, allocation_method text, max_addresses integer, desired_ip_address inet, rnd_masklen_threshold integer, rnd_max_count integer );
CREATE OR REPLACE FUNCTION netblock_utils.find_free_netblocks(parent_netblock_list integer[], netmask_bits integer DEFAULT NULL::integer, single_address boolean DEFAULT false, allocation_method text DEFAULT NULL::text, max_addresses integer DEFAULT 1024, desired_ip_address inet DEFAULT NULL::inet, rnd_masklen_threshold integer DEFAULT 110, rnd_max_count integer DEFAULT 1024)
 RETURNS TABLE(ip_address inet, netblock_type character varying, ip_universe_id integer)
 LANGUAGE plpgsql
AS $function$
DECLARE
	parent_nbid		jazzhands.netblock.netblock_id%TYPE;
	netblock_rec	jazzhands.netblock%ROWTYPE;
	netrange_rec	RECORD;
	inet_list		inet[];
	current_ip		inet;
	saved_method	text;
	min_ip			inet;
	max_ip			inet;
	matches			integer;
	rnd_matches		integer;
	max_rnd_value	bigint;
	rnd_value		bigint;
	family_bits		integer;
BEGIN
	matches := 0;
	saved_method = allocation_method;

	IF allocation_method IS NOT NULL AND allocation_method
			NOT IN ('top', 'bottom', 'random', 'default') THEN
		RAISE 'address_type must be one of top, bottom, random, or default'
		USING ERRCODE = 'invalid_parameter_value';
	END IF;

	--
	-- Sanitize masklen input.  This is a little complicated.
	--
	-- If a single address is desired, we always use a /32 or /128
	-- in the parent loop and everything else is ignored
	--
	-- Otherwise, if netmask_bits is passed, that wins, otherwise
	-- the netmask of whatever is passed with desired_ip_address wins
	--
	-- If none of these are the case, then things are wrong and we
	-- bail
	--

	IF NOT single_address THEN 
		IF desired_ip_address IS NOT NULL AND netmask_bits IS NULL THEN
			netmask_bits := masklen(desired_ip_address);
		ELSIF desired_ip_address IS NOT NULL AND 
				netmask_bits IS NOT NULL THEN
			desired_ip_address := set_masklen(desired_ip_address,
				netmask_bits);
		END IF;
		IF netmask_bits IS NULL THEN
			RAISE EXCEPTION 'netmask_bits must be set'
			USING ERRCODE = 'invalid_parameter_value';
		END IF;
		IF allocation_method = 'random' THEN
			RAISE EXCEPTION 'random netblocks may only be returned for single addresses'
			USING ERRCODE = 'invalid_parameter_value';
		END IF;
	END IF;

	FOREACH parent_nbid IN ARRAY parent_netblock_list LOOP
		rnd_matches := 0;
		--
		-- Restore this, because we may have overrridden it for a previous
		-- block
		--
		allocation_method = saved_method;
		SELECT 
			* INTO netblock_rec
		FROM
			jazzhands.netblock n
		WHERE
			n.netblock_id = parent_nbid;

		IF NOT FOUND THEN
			RAISE EXCEPTION 'Netblock % does not exist', parent_nbid;
		END IF;

		family_bits := 
			(CASE family(netblock_rec.ip_address) WHEN 4 THEN 32 ELSE 128 END);

		-- If desired_ip_address is passed, then allocation_method is
		-- irrelevant

		IF desired_ip_address IS NOT NULL THEN
			--
			-- If the IP address is not the same family as the parent block,
			-- we aren't going to find it
			--
			IF family(desired_ip_address) != 
					family(netblock_rec.ip_address) THEN
				CONTINUE;
			END IF;
			allocation_method := 'bottom';
		END IF;

		--
		-- If allocation_method is 'default' or NULL, then use 'bottom'
		-- unless it's for a single IPv6 address in a netblock larger than 
		-- rnd_masklen_threshold
		--
		IF allocation_method IS NULL OR allocation_method = 'default' THEN
			allocation_method := 
				CASE WHEN 
					single_address AND 
					family(netblock_rec.ip_address) = 6 AND
					masklen(netblock_rec.ip_address) <= rnd_masklen_threshold
				THEN
					'random'
				ELSE
					'bottom'
				END;
		END IF;

		IF allocation_method = 'random' AND 
				family_bits - masklen(netblock_rec.ip_address) < 2 THEN
			-- Random allocation doesn't work if we don't have enough
			-- bits to play with, so just do sequential.
			allocation_method := 'bottom';
		END IF;

		IF single_address THEN 
			netmask_bits := family_bits;
			IF desired_ip_address IS NOT NULL THEN
				desired_ip_address := set_masklen(desired_ip_address,
					masklen(netblock_rec.ip_address));
			END IF;
		ELSIF netmask_bits <= masklen(netblock_rec.ip_address) THEN
			-- If the netmask is not for a smaller netblock than this parent,
			-- then bounce to the next one, because maybe it's larger
			RAISE DEBUG
				'netblock (%) is not larger than netmask_bits of % - skipping',
				masklen(netblock_rec.ip_address),
				netmask_bits;
			CONTINUE;
		END IF;

		IF netmask_bits > family_bits THEN
			RAISE EXCEPTION 'netmask_bits must be no more than % for netblock %',
				family_bits,
				netblock_rec.ip_address;
		END IF;

		--
		-- Short circuit the check if we're looking for a specific address
		-- and it's not in this netblock
		--

		IF desired_ip_address IS NOT NULL AND
				NOT (desired_ip_address <<= netblock_rec.ip_address) THEN
			RAISE DEBUG 'desired_ip_address % is not in netblock %',
				desired_ip_address,
				netblock_rec.ip_address;
			CONTINUE;
		END IF;

		IF single_address AND netblock_rec.can_subnet = 'Y' THEN
			RAISE EXCEPTION 'single addresses may not be assigned to to a block where can_subnet is Y';
		END IF;

		IF (NOT single_address) AND netblock_rec.can_subnet = 'N' THEN
			RAISE EXCEPTION 'Netblock % (%) may not be subnetted',
				netblock_rec.ip_address,
				netblock_rec.netblock_id;
		END IF;

		RAISE DEBUG 'Searching netblock % (%) using the % allocation method',
			netblock_rec.netblock_id,
			netblock_rec.ip_address,
			allocation_method;

		IF desired_ip_address IS NOT NULL THEN
			min_ip := desired_ip_address;
			max_ip := desired_ip_address + 1;
		ELSE
			min_ip := netblock_rec.ip_address;
			max_ip := broadcast(min_ip) + 1;
		END IF;

		IF allocation_method = 'top' THEN
			current_ip := network(set_masklen(max_ip - 1, netmask_bits));
		ELSIF allocation_method = 'random' THEN
			max_rnd_value := (x'7fffffffffffffff'::bigint >> CASE 
				WHEN family_bits - masklen(netblock_rec.ip_address) >= 63
				THEN 0
				ELSE 63 - (family_bits - masklen(netblock_rec.ip_address))
				END) - 2;
			-- random() appears to only do 32-bits, which is dumb
			-- I'm pretty sure that all of the casts are not required here,
			-- but better to make sure
			current_ip := min_ip + 
					((((random() * x'7fffffff'::bigint)::bigint << 32) + 
					(random() * x'ffffffff'::bigint)::bigint + 1)
					% max_rnd_value) + 1;
		ELSE -- it's 'bottom'
			current_ip := set_masklen(min_ip, netmask_bits);
		END IF;

		-- For single addresses, make the netmask match the netblock of the
		-- containing block, and skip the network and broadcast addresses
		-- We shouldn't need to skip for IPv6 addresses, but some things
		-- apparently suck

		IF single_address THEN
			current_ip := set_masklen(current_ip, 
				masklen(netblock_rec.ip_address));
			--
			-- If we're not allocating a single /31 or /32 for IPv4 or
			-- /127 or /128 for IPv6, then we want to skip the all-zeros
			-- and all-ones addresses
			--
			IF masklen(netblock_rec.ip_address) < (family_bits - 1) AND
					desired_ip_address IS NULL THEN
				current_ip := current_ip + 
					CASE WHEN allocation_method = 'top' THEN -1 ELSE 1 END;
				min_ip := min_ip + 1;
				max_ip := max_ip - 1;
			END IF;
		END IF;

		RAISE DEBUG 'Starting with IP address % with step masklen of %',
			current_ip,
			netmask_bits;

		WHILE (
				current_ip >= min_ip AND
				current_ip < max_ip AND
				matches < max_addresses AND
				rnd_matches < rnd_max_count
		) LOOP
			RAISE DEBUG '   Checking netblock %', current_ip;

			IF single_address THEN
				--
				-- Check to see if netblock is in a network_range, and if it is,
				-- then set the value to the top or bottom of the range, or
				-- another random value as appropriate
				--
				SELECT 
					network_range_id,
					start_nb.ip_address AS start_ip_address,
					stop_nb.ip_address AS stop_ip_address
				INTO netrange_rec
				FROM
					jazzhands.network_range nr,
					jazzhands.netblock start_nb,
					jazzhands.netblock stop_nb
				WHERE
					nr.start_netblock_id = start_nb.netblock_id AND
					nr.stop_netblock_id = stop_nb.netblock_id AND
					nr.parent_netblock_id = netblock_rec.netblock_id AND
					start_nb.ip_address <= current_ip AND
					stop_nb.ip_address >= current_ip;

				IF FOUND THEN
					current_ip := CASE 
						WHEN allocation_method = 'bottom' THEN
							netrange_rec.stop_ip_address + 1
						WHEN allocation_method = 'top' THEN
							netrange_rec.start_ip_address - 1
						ELSE min_ip + ((
							((random() * x'7fffffff'::bigint)::bigint << 32) 
							+ 
							(random() * x'ffffffff'::bigint)::bigint + 1
							) % max_rnd_value) + 1 
					END;
					CONTINUE;
				END IF;
			END IF;
							
				
			PERFORM * FROM jazzhands.netblock n WHERE
				n.ip_universe_id = netblock_rec.ip_universe_id AND
				n.netblock_type = netblock_rec.netblock_type AND
				-- A block with the parent either contains or is contained
				-- by this block
				n.parent_netblock_id = netblock_rec.netblock_id AND
				CASE WHEN single_address THEN
					n.ip_address = current_ip
				ELSE
					(n.ip_address >>= current_ip OR current_ip >>= n.ip_address)
				END;
			IF NOT FOUND AND (inet_list IS NULL OR
					NOT (current_ip = ANY(inet_list))) THEN
				find_free_netblocks.netblock_type :=
					netblock_rec.netblock_type;
				find_free_netblocks.ip_universe_id :=
					netblock_rec.ip_universe_id;
				find_free_netblocks.ip_address := current_ip;
				RETURN NEXT;
				inet_list := array_append(inet_list, current_ip);
				matches := matches + 1;
				-- Reset random counter if we found something
				rnd_matches := 0;
			ELSIF allocation_method = 'random' THEN
				-- Increase random counter if we didn't find something
				rnd_matches := rnd_matches + 1;
			END IF;

			-- Select the next IP address
			current_ip := 
				CASE WHEN single_address THEN
					CASE 
						WHEN allocation_method = 'bottom' THEN current_ip + 1
						WHEN allocation_method = 'top' THEN current_ip - 1
						ELSE min_ip + ((
							((random() * x'7fffffff'::bigint)::bigint << 32) 
							+ 
							(random() * x'ffffffff'::bigint)::bigint + 1
							) % max_rnd_value) + 1 
					END
				ELSE
					CASE WHEN allocation_method = 'bottom' THEN 
						network(broadcast(current_ip) + 1)
					ELSE 
						network(current_ip - 1)
					END
				END;
		END LOOP;
	END LOOP;
	RETURN;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('netblock_utils', 'list_unallocated_netblocks');
-- Dropped in case type changes.
DROP FUNCTION IF EXISTS netblock_utils.list_unallocated_netblocks ( netblock_id integer, ip_address inet, ip_universe_id integer, netblock_type text );
CREATE OR REPLACE FUNCTION netblock_utils.list_unallocated_netblocks(netblock_id integer DEFAULT NULL::integer, ip_address inet DEFAULT NULL::inet, ip_universe_id integer DEFAULT 0, netblock_type text DEFAULT 'default'::text)
 RETURNS TABLE(ip_addr inet)
 LANGUAGE plpgsql
AS $function$
DECLARE
	ip_array		inet[];
	netblock_rec	RECORD;
	parent_nbid		jazzhands.netblock.netblock_id%TYPE;
	family_bits		integer;
	idx				integer;
	subnettable		boolean;
BEGIN
	subnettable := true;
	IF netblock_id IS NOT NULL THEN
		SELECT * INTO netblock_rec FROM jazzhands.netblock n WHERE n.netblock_id = 
			list_unallocated_netblocks.netblock_id;
		IF NOT FOUND THEN
			RAISE EXCEPTION 'netblock_id % not found', netblock_id;
		END IF;
		IF netblock_rec.is_single_address = 'Y' THEN
			RETURN;
		END IF;
		ip_address := netblock_rec.ip_address;
		ip_universe_id := netblock_rec.ip_universe_id;
		netblock_type := netblock_rec.netblock_type;
		subnettable := CASE WHEN netblock_rec.can_subnet = 'N' 
			THEN false ELSE true
			END;
	ELSIF ip_address IS NOT NULL THEN
		ip_universe_id := 0;
		netblock_type := 'default';
	ELSE
		RAISE EXCEPTION 'netblock_id or ip_address must be passed';
	END IF;
	IF (subnettable) THEN
		SELECT ARRAY(
			SELECT 
				n.ip_address
			FROM
				netblock n
			WHERE
				n.ip_address <<= list_unallocated_netblocks.ip_address AND
				n.ip_universe_id = list_unallocated_netblocks.ip_universe_id AND
				n.netblock_type = list_unallocated_netblocks.netblock_type AND
				is_single_address = 'N' AND
				can_subnet = 'N'
			ORDER BY
				n.ip_address
		) INTO ip_array;
	ELSE
		SELECT ARRAY(
			SELECT 
				set_masklen(n.ip_address, 
					CASE WHEN family(n.ip_address) = 4 THEN 32
					ELSE 128
					END)
			FROM
				netblock n
			WHERE
				n.ip_address <<= list_unallocated_netblocks.ip_address AND
				n.ip_address != list_unallocated_netblocks.ip_address AND
				n.ip_universe_id = list_unallocated_netblocks.ip_universe_id AND
				n.netblock_type = list_unallocated_netblocks.netblock_type
			ORDER BY
				n.ip_address
		) INTO ip_array;
	END IF;

	IF array_length(ip_array, 1) IS NULL THEN
		ip_addr := ip_address;
		RETURN NEXT;
		RETURN;
	END IF;

	ip_array := array_prepend(
		list_unallocated_netblocks.ip_address - 1, 
		array_append(
			ip_array, 
			broadcast(list_unallocated_netblocks.ip_address) + 1
			));

	idx := 1;
	WHILE idx < array_length(ip_array, 1) LOOP
		RETURN QUERY SELECT cin.ip_addr FROM
			netblock_utils.calculate_intermediate_netblocks(ip_array[idx], ip_array[idx + 1]) cin;
		idx := idx + 1;
	END LOOP;

	RETURN;
END;
$function$
;

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
-- Process middle (non-trigger) schema approval_utils
--
--
-- Process middle (non-trigger) schema account_collection_manip
--
--
-- Process middle (non-trigger) schema schema_support
--
DROP FUNCTION IF EXISTS schema_support.save_dependant_objects_for_replay ( schema character varying, object character varying, dropit boolean, doobjectdeps boolean );
--
-- Process middle (non-trigger) schema script_hooks
--
-- Creating new sequences....


--------------------------------------------------------------------
-- DEALING WITH TABLE person_company_attr [3934545]
-- Save grants for later reapplication
SELECT schema_support.save_grants_for_replay('jazzhands', 'person_company_attr', 'person_company_attr');

-- FOREIGN KEYS FROM

-- FOREIGN KEYS TO
ALTER TABLE jazzhands.person_company_attr DROP CONSTRAINT IF EXISTS fk_pers_comp_attr_person_comp_;
ALTER TABLE jazzhands.person_company_attr DROP CONSTRAINT IF EXISTS fk_person_comp_att_pers_person;
ALTER TABLE jazzhands.person_company_attr DROP CONSTRAINT IF EXISTS fk_person_comp_attr_val_name;

-- EXTRA-SCHEMA constraints
SELECT schema_support.save_constraint_for_replay('jazzhands', 'person_company_attr');

-- PRIMARY and ALTERNATE KEYS
ALTER TABLE jazzhands.person_company_attr DROP CONSTRAINT IF EXISTS ak_person_company_attr_name;
ALTER TABLE jazzhands.person_company_attr DROP CONSTRAINT IF EXISTS pk_person_company_attr;
-- INDEXES
DROP INDEX IF EXISTS "jazzhands"."xif1person_company_attr";
DROP INDEX IF EXISTS "jazzhands"."xif2person_company_attr";
DROP INDEX IF EXISTS "jazzhands"."xif3person_company_attr";
-- CHECK CONSTRAINTS, etc
-- TRIGGERS, etc
DROP TRIGGER IF EXISTS trig_userlog_person_company_attr ON jazzhands.person_company_attr;
DROP TRIGGER IF EXISTS trigger_audit_person_company_attr ON jazzhands.person_company_attr;
DROP TRIGGER IF EXISTS trigger_validate_pers_company_attr ON jazzhands.person_company_attr;
SELECT schema_support.save_dependent_objects_for_replay('jazzhands', 'person_company_attr');
---- BEGIN audit.person_company_attr TEARDOWN
-- Save grants for later reapplication
SELECT schema_support.save_grants_for_replay('audit', 'person_company_attr', 'person_company_attr');

-- FOREIGN KEYS FROM

-- FOREIGN KEYS TO

-- EXTRA-SCHEMA constraints
SELECT schema_support.save_constraint_for_replay('audit', 'person_company_attr');

-- PRIMARY and ALTERNATE KEYS
-- INDEXES
DROP INDEX IF EXISTS "audit"."person_company_attr_aud#timestamp_idx";
-- CHECK CONSTRAINTS, etc
-- TRIGGERS, etc
SELECT schema_support.save_dependent_objects_for_replay('audit', 'person_company_attr');
---- DONE audit.person_company_attr TEARDOWN


ALTER TABLE person_company_attr RENAME TO person_company_attr_v70;
ALTER TABLE audit.person_company_attr RENAME TO person_company_attr_v70;

CREATE TABLE person_company_attr
(
	company_id	integer NOT NULL,
	person_id	integer NOT NULL,
	person_company_attr_name	varchar(50) NOT NULL,
	attribute_value	varchar(50)  NULL,
	attribute_value_timestamp	timestamp with time zone  NULL,
	attribute_value_person_id	integer  NULL,
	start_date	timestamp with time zone  NULL,
	finish_date	timestamp with time zone  NULL,
	data_ins_user	varchar(255)  NULL,
	data_ins_date	timestamp with time zone  NULL,
	data_upd_user	varchar(255)  NULL,
	data_upd_date	timestamp with time zone  NULL
);
SELECT schema_support.build_audit_table('audit', 'jazzhands', 'person_company_attr', false);
INSERT INTO person_company_attr (
	company_id,
	person_id,
	person_company_attr_name,
	attribute_value,
	attribute_value_timestamp,
	attribute_value_person_id,
	start_date,		-- new column (start_date)
	finish_date,		-- new column (finish_date)
	data_ins_user,
	data_ins_date,
	data_upd_user,
	data_upd_date
) SELECT
	company_id,
	person_id,
	person_company_attr_name,
	attribute_value,
	attribute_value_timestamp,
	attribute_value_person_id,
	NULL,		-- new column (start_date)
	NULL,		-- new column (finish_date)
	data_ins_user,
	data_ins_date,
	data_upd_user,
	data_upd_date
FROM person_company_attr_v70;

INSERT INTO audit.person_company_attr (
	company_id,
	person_id,
	person_company_attr_name,
	attribute_value,
	attribute_value_timestamp,
	attribute_value_person_id,
	start_date,		-- new column (start_date)
	finish_date,		-- new column (finish_date)
	data_ins_user,
	data_ins_date,
	data_upd_user,
	data_upd_date,
	"aud#action",
	"aud#timestamp",
	"aud#user",
	"aud#seq"
) SELECT
	company_id,
	person_id,
	person_company_attr_name,
	attribute_value,
	attribute_value_timestamp,
	attribute_value_person_id,
	NULL,		-- new column (start_date)
	NULL,		-- new column (finish_date)
	data_ins_user,
	data_ins_date,
	data_upd_user,
	data_upd_date,
	"aud#action",
	"aud#timestamp",
	"aud#user",
	"aud#seq"
FROM audit.person_company_attr_v70;


-- PRIMARY AND ALTERNATE KEYS
ALTER TABLE person_company_attr ADD CONSTRAINT ak_person_company_attr_name UNIQUE (company_id, person_id, person_company_attr_name);
ALTER TABLE person_company_attr ADD CONSTRAINT pk_person_company_attr PRIMARY KEY (company_id, person_id, person_company_attr_name);

-- Table/Column Comments
COMMENT ON COLUMN person_company_attr.attribute_value IS 'string value of the attribute.';
COMMENT ON COLUMN person_company_attr.attribute_value_person_id IS 'person_id value of the attribute.';
-- INDEXES
CREATE INDEX xif1person_company_attr ON person_company_attr USING btree (company_id, person_id);
CREATE INDEX xif2person_company_attr ON person_company_attr USING btree (attribute_value_person_id);
CREATE INDEX xif3person_company_attr ON person_company_attr USING btree (person_company_attr_name);

-- CHECK CONSTRAINTS

-- FOREIGN KEYS FROM

-- FOREIGN KEYS TO
-- consider FK person_company_attr and person_company
ALTER TABLE person_company_attr
	ADD CONSTRAINT fk_pers_comp_attr_person_comp_
	FOREIGN KEY (company_id, person_id) REFERENCES person_company(company_id, person_id) DEFERRABLE;
-- consider FK person_company_attr and person
ALTER TABLE person_company_attr
	ADD CONSTRAINT fk_person_comp_att_pers_person
	FOREIGN KEY (attribute_value_person_id) REFERENCES person(person_id);
-- consider FK person_company_attr and val_person_company_attr_name
ALTER TABLE person_company_attr
	ADD CONSTRAINT fk_person_comp_attr_val_name
	FOREIGN KEY (person_company_attr_name) REFERENCES val_person_company_attr_name(person_company_attr_name);

-- TRIGGERS
-- consider NEW oid 3881193
CREATE OR REPLACE FUNCTION jazzhands.validate_pers_company_attr()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	tally			integer;
	v_pc_atr		val_person_company_attr_name%ROWTYPE;
	v_listvalue		Property.Property_Value%TYPE;
BEGIN

	SELECT	*
	INTO	v_pc_atr
	FROM	val_person_company_attr_name
	WHERE	person_company_attr_name = NEW.person_company_attr_name;

	IF v_pc_atr.person_company_attr_data_type IN
			('boolean', 'number', 'string', 'list') THEN
		IF NEW.attribute_value IS NULL THEN
			RAISE EXCEPTION 'attribute_value must be set for %',
				v_pc_atr.person_company_attr_data_type
				USING ERRCODE = 'not_null_violation';
		END IF;
		IF v_pc_atr.person_company_attr_data_type = 'boolean' THEN
			IF NEW.attribute_value NOT IN ('Y', 'N') THEN
				RAISE EXCEPTION 'attribute_value must be boolean (Y,N)'
					USING ERRCODE = 'integrity_constraint_violation';
			END IF;
		ELSIF v_pc_atr.person_company_attr_data_type = 'number' THEN
			IF NEW.attribute_value !~ '^-?(\d*\.?\d*){1}$' THEN
				RAISE EXCEPTION 'attribute_value must be a number'
					USING ERRCODE = 'integrity_constraint_violation';
			END IF;
		ELSIF v_pc_atr.person_company_attr_data_type = 'timestamp' THEN
			IF NEW.attribute_value_timestamp IS NULL THEN
				RAISE EXCEPTION 'attribute_value_timestamp must be set for %',
					v_pc_atr.person_company_attr_data_type
					USING ERRCODE = 'not_null_violation';
			END IF;
		ELSIF v_pc_atr.person_company_attr_data_type = 'list' THEN
			PERFORM 1
			FROM	val_person_company_attr_value
			WHERE	(person_company_attr_name,person_company_attr_value)
					IN
					(NEW.person_company_attr_name,NEW.person_company_attr_value)
			;
			IF NOT FOUND THEN
				RAISE EXCEPTION 'attribute_value must be valid'
					USING ERRCODE = 'integrity_constraint_violation';
			END IF;
		END IF;
	ELSIF v_pc_atr.person_company_attr_data_type = 'person_id' THEN
		IF NEW.attribute_value_timestamp IS NULL THEN
			RAISE EXCEPTION 'attribute_value_timestamp must be set for %',
				v_pc_atr.person_company_attr_data_type
				USING ERRCODE = 'not_null_violation';
		END IF;
	END IF;

	IF NEW.attribute_value IS NOT NULL AND
			(NEW.attribute_value_person_id IS NOT NULL OR
			NEW.attribute_value_timestamp IS NOT NULL) THEN
		RAISE EXCEPTION 'only one attribute_value may be set'
			USING ERRCODE = 'integrity_constraint_violation';
	ELSIF NEW.attribute_value_person_id IS NOT NULL AND
			(NEW.attribute_value IS NOT NULL OR
			NEW.attribute_value_timestamp IS NOT NULL) THEN
		RAISE EXCEPTION 'only one attribute_value may be set'
			USING ERRCODE = 'integrity_constraint_violation';
	ELSIF NEW.attribute_value_timestamp IS NOT NULL AND
			(NEW.attribute_value_person_id IS NOT NULL OR
			NEW.attribute_value IS NOT NULL) THEN
		RAISE EXCEPTION 'only one attribute_value may be set'
			USING ERRCODE = 'integrity_constraint_violation';
	END IF;
	RETURN NEW;
END;
$function$
;
CREATE TRIGGER trigger_validate_pers_company_attr BEFORE INSERT OR UPDATE ON person_company_attr FOR EACH ROW EXECUTE PROCEDURE validate_pers_company_attr();

-- XXX - may need to include trigger function
-- this used to be at the end...
SELECT schema_support.replay_object_recreates();
SELECT schema_support.rebuild_stamp_trigger('jazzhands', 'person_company_attr');
SELECT schema_support.rebuild_audit_trigger('audit', 'jazzhands', 'person_company_attr');
DROP TABLE IF EXISTS person_company_attr_v70;
DROP TABLE IF EXISTS audit.person_company_attr_v70;
-- DONE DEALING WITH TABLE person_company_attr [3865064]
--------------------------------------------------------------------
--------------------------------------------------------------------
-- DEALING WITH TABLE v_account_manager_map [3942538]
-- Save grants for later reapplication
SELECT schema_support.save_grants_for_replay('jazzhands', 'v_account_manager_map', 'v_account_manager_map');
SELECT schema_support.save_dependent_objects_for_replay('jazzhands', 'v_account_manager_map');
DROP VIEW IF EXISTS jazzhands.v_account_manager_map;
CREATE VIEW jazzhands.v_account_manager_map AS
 WITH dude_base AS (
         SELECT a_1.login,
            a_1.account_id,
            a_1.person_id,
            a_1.company_id,
            a_1.account_realm_id,
            COALESCE(p.preferred_first_name, p.first_name) AS first_name,
            COALESCE(p.preferred_last_name, p.last_name) AS last_name,
            pc.manager_person_id
           FROM account a_1
             JOIN person_company pc USING (company_id, person_id)
             JOIN person p USING (person_id)
          WHERE a_1.is_enabled = 'Y'::bpchar AND pc.person_company_relation::text = 'employee'::text AND a_1.account_role::text = 'primary'::text AND a_1.account_type::text = 'person'::text
        ), dude AS (
         SELECT dude_base.login,
            dude_base.account_id,
            dude_base.person_id,
            dude_base.company_id,
            dude_base.account_realm_id,
            dude_base.first_name,
            dude_base.last_name,
            dude_base.manager_person_id,
            concat(dude_base.first_name, ' ', dude_base.last_name, ' (', dude_base.login, ')') AS human_readable
           FROM dude_base
        )
 SELECT a.login,
    a.account_id,
    a.person_id,
    a.company_id,
    a.account_realm_id,
    a.first_name,
    a.last_name,
    a.manager_person_id,
    a.human_readable,
    mp.account_id AS manager_account_id,
    mp.login AS manager_login,
    concat(mp.first_name, ' ', mp.last_name, ' (', mp.login, ')') AS manager_human_readable
   FROM dude a
     JOIN dude mp ON mp.person_id = a.manager_person_id AND mp.account_realm_id = a.account_realm_id;

delete from __recreate where type = 'view' and object = 'v_account_manager_map';
-- DONE DEALING WITH TABLE v_account_manager_map [3880353]
--------------------------------------------------------------------
--------------------------------------------------------------------
-- DEALING WITH NEW TABLE v_l3_network_coll_expanded
DROP VIEW IF EXISTS jazzhands.v_l3_network_coll_expanded;
CREATE VIEW jazzhands.v_l3_network_coll_expanded AS
 WITH RECURSIVE l3_network_coll_recurse(level, root_l3_network_coll_id, layer3_network_collection_id, array_path, rvs_array_path, cycle) AS (
         SELECT 0 AS level,
            l3.layer3_network_collection_id AS root_l3_network_coll_id,
            l3.layer3_network_collection_id,
            ARRAY[l3.layer3_network_collection_id] AS array_path,
            ARRAY[l3.layer3_network_collection_id] AS rvs_array_path,
            false AS bool
           FROM layer3_network_collection l3
        UNION ALL
         SELECT x.level + 1 AS level,
            x.root_l3_network_coll_id,
            l3h.layer3_network_collection_id,
            x.array_path || l3h.layer3_network_collection_id AS array_path,
            l3h.layer3_network_collection_id || x.rvs_array_path AS rvs_array_path,
            l3h.layer3_network_collection_id = ANY (x.array_path) AS cycle
           FROM l3_network_coll_recurse x
             JOIN layer3_network_collection_hier l3h ON x.layer3_network_collection_id = l3h.child_l3_network_coll_id
          WHERE NOT x.cycle
        )
 SELECT l3_network_coll_recurse.level,
    l3_network_coll_recurse.layer3_network_collection_id,
    l3_network_coll_recurse.root_l3_network_coll_id,
    array_to_string(l3_network_coll_recurse.array_path, '/'::text) AS text_path,
    l3_network_coll_recurse.array_path,
    l3_network_coll_recurse.rvs_array_path
   FROM l3_network_coll_recurse;

-- DONE DEALING WITH TABLE v_l3_network_coll_expanded [3880427]
--------------------------------------------------------------------
--------------------------------------------------------------------
-- DEALING WITH NEW TABLE v_l2_network_coll_expanded
DROP VIEW IF EXISTS jazzhands.v_l2_network_coll_expanded;
CREATE VIEW jazzhands.v_l2_network_coll_expanded AS
 WITH RECURSIVE l2_network_coll_recurse(level, root_l2_network_coll_id, layer2_network_collection_id, array_path, rvs_array_path, cycle) AS (
         SELECT 0 AS level,
            l2.layer2_network_collection_id AS root_l2_network_coll_id,
            l2.layer2_network_collection_id,
            ARRAY[l2.layer2_network_collection_id] AS array_path,
            ARRAY[l2.layer2_network_collection_id] AS rvs_array_path,
            false AS bool
           FROM layer2_network_collection l2
        UNION ALL
         SELECT x.level + 1 AS level,
            x.root_l2_network_coll_id,
            l2h.layer2_network_collection_id,
            x.array_path || l2h.layer2_network_collection_id AS array_path,
            l2h.layer2_network_collection_id || x.rvs_array_path AS rvs_array_path,
            l2h.layer2_network_collection_id = ANY (x.array_path) AS cycle
           FROM l2_network_coll_recurse x
             JOIN layer2_network_collection_hier l2h ON x.layer2_network_collection_id = l2h.child_l2_network_coll_id
          WHERE NOT x.cycle
        )
 SELECT l2_network_coll_recurse.level,
    l2_network_coll_recurse.layer2_network_collection_id,
    l2_network_coll_recurse.root_l2_network_coll_id,
    array_to_string(l2_network_coll_recurse.array_path, '/'::text) AS text_path,
    l2_network_coll_recurse.array_path,
    l2_network_coll_recurse.rvs_array_path
   FROM l2_network_coll_recurse;

-- DONE DEALING WITH TABLE v_l2_network_coll_expanded [3880422]
--------------------------------------------------------------------
--------------------------------------------------------------------
-- DEALING WITH TABLE v_account_collection_audit_results [3942559]
-- Save grants for later reapplication
SELECT schema_support.save_grants_for_replay('jazzhands', 'v_account_collection_audit_results', 'v_account_collection_audit_results');
SELECT schema_support.save_dependent_objects_for_replay('approval_utils', 'v_account_collection_audit_results');
DROP VIEW IF EXISTS approval_utils.v_account_collection_audit_results;
CREATE VIEW approval_utils.v_account_collection_audit_results AS
 WITH membermap AS (
         SELECT aca.audit_seq_id,
            ac.account_collection_id,
            ac.account_collection_name,
            ac.account_collection_type,
            a.login,
            a.account_id,
            a.person_id,
            a.company_id,
            a.account_realm_id,
            a.first_name,
            a.last_name,
            a.manager_person_id,
            a.human_readable,
            a.manager_account_id,
            a.manager_login,
            a.manager_human_readable
           FROM v_account_manager_map a
             JOIN approval_utils.v_account_collection_account_audit_map aca USING (account_id)
             JOIN account_collection ac USING (account_collection_id)
          WHERE a.account_id <> a.manager_account_id
          ORDER BY a.manager_login, a.last_name, a.first_name, a.account_id
        )
 SELECT membermap.audit_seq_id,
    membermap.account_collection_id,
    membermap.account_collection_name,
    membermap.account_collection_type,
    membermap.login,
    membermap.account_id,
    membermap.person_id,
    membermap.company_id,
    membermap.account_realm_id,
    membermap.first_name,
    membermap.last_name,
    membermap.manager_person_id,
    membermap.human_readable,
    membermap.manager_account_id,
    membermap.manager_login,
    membermap.manager_human_readable
   FROM membermap;

delete from __recreate where type = 'view' and object = 'v_account_collection_audit_results';
-- DONE DEALING WITH TABLE v_account_collection_audit_results [3880377]
--------------------------------------------------------------------
--------------------------------------------------------------------
-- DEALING WITH TABLE v_account_collection_approval_process [3942564]
-- Save grants for later reapplication
SELECT schema_support.save_grants_for_replay('jazzhands', 'v_account_collection_approval_process', 'v_account_collection_approval_process');
SELECT schema_support.save_dependent_objects_for_replay('approval_utils', 'v_account_collection_approval_process');
DROP VIEW IF EXISTS approval_utils.v_account_collection_approval_process;
CREATE VIEW approval_utils.v_account_collection_approval_process AS
 WITH combo AS (
         WITH foo AS (
                 SELECT mm.audit_seq_id,
                    mm.account_collection_id,
                    mm.account_collection_name,
                    mm.account_collection_type,
                    mm.login,
                    mm.account_id,
                    mm.person_id,
                    mm.company_id,
                    mm.account_realm_id,
                    mm.first_name,
                    mm.last_name,
                    mm.manager_person_id,
                    mm.human_readable,
                    mm.manager_account_id,
                    mm.manager_login,
                    mm.manager_human_readable,
                    mx.approval_process_id,
                    mx.first_apprvl_process_chain_id,
                    mx.approval_process_name,
                    mx.approval_response_period,
                    mx.approval_expiration_action,
                    mx.attestation_frequency,
                    mx.attestation_offset,
                    mx.current_attestation_name,
                    mx.current_attestation_begins,
                    mx.property_id,
                    mx.property_name,
                    mx.property_type,
                    mx.property_value,
                    mx.property_val_lhs,
                    mx.property_val_rhs,
                    mx.approval_process_chain_id,
                    mx.approving_entity,
                    mx.approval_process_chain_name,
                    mx.approval_process_description,
                    mx.approval_chain_description
                   FROM approval_utils.v_account_collection_audit_results mm
                     JOIN approval_utils.v_approval_matrix mx ON mx.property_val_lhs = mm.account_collection_type::text
                  ORDER BY mm.manager_account_id, mm.account_id
                )
         SELECT foo.login,
            foo.account_id,
            foo.person_id,
            foo.company_id,
            foo.manager_account_id,
            foo.manager_login,
            'account_collection_account'::text AS audit_table,
            foo.audit_seq_id,
            foo.approval_process_id,
            foo.approval_process_chain_id,
            foo.approving_entity,
            foo.approval_process_description,
            foo.approval_chain_description,
            foo.approval_response_period,
            foo.approval_expiration_action,
            foo.attestation_frequency,
            foo.current_attestation_name,
            foo.current_attestation_begins,
            foo.attestation_offset,
            foo.approval_process_chain_name,
            foo.account_collection_type AS approval_category,
            concat('Verify ', foo.account_collection_type) AS approval_label,
            foo.human_readable AS approval_lhs,
            foo.account_collection_name AS approval_rhs
           FROM foo
        UNION
         SELECT mm.login,
            mm.account_id,
            mm.person_id,
            mm.company_id,
            mm.manager_account_id,
            mm.manager_login,
            'account_collection_account'::text AS audit_table,
            mm.audit_seq_id,
            mx.approval_process_id,
            mx.approval_process_chain_id,
            mx.approving_entity,
            mx.approval_process_description,
            mx.approval_chain_description,
            mx.approval_response_period,
            mx.approval_expiration_action,
            mx.attestation_frequency,
            mx.current_attestation_name,
            mx.current_attestation_begins,
            mx.attestation_offset,
            mx.approval_process_chain_name,
            mx.approval_process_name AS approval_category,
            'Verify Manager'::text AS approval_label,
            mm.human_readable AS approval_lhs,
            concat('Reports to ', mm.manager_human_readable) AS approval_rhs
           FROM approval_utils.v_approval_matrix mx
             JOIN property p ON p.property_name::text = mx.property_val_rhs AND p.property_type::text = mx.property_val_lhs
             JOIN approval_utils.v_account_collection_audit_results mm ON mm.account_collection_id = p.property_value_account_coll_id
          WHERE p.account_id <> mm.account_id
        UNION
         SELECT mm.login,
            mm.account_id,
            mm.person_id,
            mm.company_id,
            mm.manager_account_id,
            mm.manager_login,
            'person_company'::text AS audit_table,
            pcm.audit_seq_id,
            am.approval_process_id,
            am.approval_process_chain_id,
            am.approving_entity,
            am.approval_process_description,
            am.approval_chain_description,
            am.approval_response_period,
            am.approval_expiration_action,
            am.attestation_frequency,
            am.current_attestation_name,
            am.current_attestation_begins,
            am.attestation_offset,
            am.approval_process_chain_name,
            am.property_val_rhs AS approval_category,
                CASE
                    WHEN am.property_val_rhs = 'position_title'::text THEN 'Verify Position Title'::text
                    ELSE NULL::text
                END AS aproval_label,
            mm.human_readable AS approval_lhs,
                CASE
                    WHEN am.property_val_rhs = 'position_title'::text THEN pcm.position_title
                    ELSE NULL::character varying
                END AS approval_rhs
           FROM v_account_manager_map mm
             JOIN approval_utils.v_person_company_audit_map pcm USING (person_id, company_id)
             JOIN approval_utils.v_approval_matrix am ON am.property_val_lhs = 'person_company'::text AND am.property_val_rhs = 'position_title'::text
        )
 SELECT combo.login,
    combo.account_id,
    combo.person_id,
    combo.company_id,
    combo.manager_account_id,
    combo.manager_login,
    combo.audit_table,
    combo.audit_seq_id,
    combo.approval_process_id,
    combo.approval_process_chain_id,
    combo.approving_entity,
    combo.approval_process_description,
    combo.approval_chain_description,
    combo.approval_response_period,
    combo.approval_expiration_action,
    combo.attestation_frequency,
    combo.current_attestation_name,
    combo.current_attestation_begins,
    combo.attestation_offset,
    combo.approval_process_chain_name,
    combo.approval_category,
    combo.approval_label,
    combo.approval_lhs,
    combo.approval_rhs
   FROM combo
  WHERE combo.manager_account_id <> combo.account_id
  ORDER BY combo.manager_login, combo.account_id, combo.approval_label;

delete from __recreate where type = 'view' and object = 'v_account_collection_approval_process';
-- DONE DEALING WITH TABLE v_account_collection_approval_process [3880383]
--------------------------------------------------------------------
--
-- Process trigger procs in jazzhands
--
-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'account_change_realm_aca_realm');
CREATE OR REPLACE FUNCTION jazzhands.account_change_realm_aca_realm()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally	integer;
BEGIN
	SELECT	count(*)
	INTO	_tally
	FROM	account_collection_account
			JOIN account_collection USING (account_collection_id)
			JOIN val_account_collection_type vt USING (account_collection_type)
	WHERE	vt.account_realm_id IS NOT NULL
	AND		vt.account_realm_id != NEW.account_realm_id
	AND		account_id = NEW.account_id;
	
	IF _tally > 0 THEN
		RAISE EXCEPTION 'New account realm (%) is part of % account collections with a type restriction',
			NEW.account_realm_id,
			_tally
			USING ERRCODE = 'integrity_constraint_violation';
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'device_collection_hier_enforce');
CREATE OR REPLACE FUNCTION jazzhands.device_collection_hier_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	dct	val_device_collection_type%ROWTYPE;
BEGIN
	SELECT *
	INTO	dct
	FROM	val_device_collection_type
	WHERE	device_collection_type =
		(select device_collection_type from device_collection
			where device_collection_id = NEW.parent_device_collection_id);

	IF dct.can_have_hierarchy = 'N' THEN
		RAISE EXCEPTION 'Device Collections of type % may not be hierarcical',
			dct.device_collection_type
			USING ERRCODE= 'unique_violation';
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'device_collection_member_enforce');
CREATE OR REPLACE FUNCTION jazzhands.device_collection_member_enforce()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	dct	val_device_collection_type%ROWTYPE;
	tally integer;
BEGIN
	SELECT *
	INTO	dct
	FROM	val_device_collection_type
	WHERE	device_collection_type =
		(select device_collection_type from device_collection
			where device_collection_id = NEW.device_collection_id);

	IF dct.MAX_NUM_MEMBERS IS NOT NULL THEN
		select count(*)
		  into tally
		  from device_collection_device
		  where device_collection_id = NEW.device_collection_id;
		IF tally > dct.MAX_NUM_MEMBERS THEN
			RAISE EXCEPTION 'Too many members'
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	IF dct.MAX_NUM_COLLECTIONS IS NOT NULL THEN
		select count(*)
		  into tally
		  from device_collection_device
		  		inner join device_collection using (device_collection_id)
		  where device_id = NEW.device_id
		  and	device_collection_type = dct.device_collection_type;
		IF tally > dct.MAX_NUM_COLLECTIONS THEN
			RAISE EXCEPTION 'Device may not be a member of more than % collections of type %',
				dct.MAX_NUM_COLLECTIONS, dct.device_collection_type
				USING ERRCODE = 'unique_violation';
		END IF;
	END IF;

	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'net_int_netblock_to_nbn_compat_after');
CREATE OR REPLACE FUNCTION jazzhands.net_int_netblock_to_nbn_compat_after()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally	INTEGER;
BEGIN
	SELECT  count(*)
	  INTO  _tally
	  FROM  pg_catalog.pg_class
	 WHERE  relname = '__network_interface_netblocks'
	   AND  relpersistence = 't';

	IF _tally = 0 THEN
		CREATE TEMPORARY TABLE IF NOT EXISTS __network_interface_netblocks (
			network_interface_id INTEGER, netblock_id INTEGER
		);
	END IF;

	IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
		SELECT count(*) INTO _tally FROM __network_interface_netblocks
		WHERE network_interface_id = NEW.network_interface_id
		AND netblock_id IS NOT DISTINCT FROM ( NEW.netblock_id );
		if _tally >  0 THEN
			RETURN NEW;
		END IF;
		INSERT INTO __network_interface_netblocks
			(network_interface_id, netblock_id)
		VALUES (NEW.network_interface_id,NEW.netblock_id);
	ELSIF TG_OP = 'DELETE' THEN
		SELECT count(*) INTO _tally FROM __network_interface_netblocks
		WHERE network_interface_id = OLD.network_interface_id
		AND netblock_id IS NOT DISTINCT FROM ( OLD.netblock_id );
		if _tally >  0 THEN
			RETURN OLD;
		END IF;
		INSERT INTO __network_interface_netblocks
			(network_interface_id, netblock_id)
		VALUES (OLD.network_interface_id,OLD.netblock_id);
	END IF;

	IF TG_OP = 'INSERT' THEN
		IF NEW.netblock_id IS NOT NULL THEN
			SELECT COUNT(*)
			INTO _tally
			FROM	network_interface_netblock
			WHERE	network_interface_id = NEW.network_interface_id
			AND		netblock_id = NEW.netblock_id;

			IF _tally = 0 THEN
				SELECT COUNT(*)
				INTO _tally
				FROM	network_interface_netblock
				WHERE	network_interface_id != NEW.network_interface_id
				AND		netblock_id = NEW.netblock_id;

				IF _tally != 0  THEN
					UPDATE network_interface_netblock
					SET network_interface_id = NEW.network_interface_id
					WHERE netblock_id = NEW.netblock_id;
				ELSE
					INSERT INTO network_interface_netblock
						(network_interface_id, netblock_id)
					VALUES
						(NEW.network_interface_id, NEW.netblock_id);
				END IF;
			END IF;
		END IF;
	ELSIF TG_OP = 'UPDATE'  THEN
		IF OLD.netblock_id is NULL and NEW.netblock_ID is NOT NULL THEN
			SELECT COUNT(*)
			INTO _tally
			FROM	network_interface_netblock
			WHERE	network_interface_id = NEW.network_interface_id
			AND		netblock_id = NEW.netblock_id;

			IF _tally = 0 THEN
				INSERT INTO network_interface_netblock
					(network_interface_id, netblock_id)
				VALUES
					(NEW.network_interface_id, NEW.netblock_id);
			END IF;
		ELSIF OLD.netblock_id IS NOT NULL and NEW.netblock_id is NOT NULL THEN
			IF OLD.netblock_id != NEW.netblock_id THEN
				UPDATE network_interface_netblock
					SET network_interface_id = NEW.network_interface_Id,
						netblock_id = NEW.netblock_id
						WHERE network_interface_id = OLD.network_interface_id
						AND netblock_id = OLD.netblock_id
						AND netblock_id != NEW.netblock_id
				;
			END IF;
		END IF;
	ELSIF TG_OP = 'DELETE' THEN
		IF OLD.netblock_id IS NOT NULL THEN
			DELETE from network_interface_netblock
				WHERE network_interface_id = OLD.network_interface_id
				AND netblock_id = OLD.netblock_id;
		END IF;
		RETURN OLD;
	END IF;
	RETURN NEW;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'net_int_netblock_to_nbn_compat_before');
CREATE OR REPLACE FUNCTION jazzhands.net_int_netblock_to_nbn_compat_before()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally	INTEGER;
BEGIN
	SET CONSTRAINTS FK_NETINT_NB_NETINT_ID DEFERRED;
	SET CONSTRAINTS FK_NETINT_NB_NBLK_ID DEFERRED;

	RETURN OLD;
END;
$function$
;

-- Changed function
SELECT schema_support.save_grants_for_replay('jazzhands', 'network_interface_drop_tt');
CREATE OR REPLACE FUNCTION jazzhands.network_interface_drop_tt()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
DECLARE
	_tally INTEGER;
BEGIN
	SELECT  count(*)
	  INTO  _tally
	  FROM  pg_catalog.pg_class
	 WHERE  relname = '__network_interface_netblocks'
	   AND  relpersistence = 't';

	SET CONSTRAINTS FK_NETINT_NB_NETINT_ID IMMEDIATE;
	SET CONSTRAINTS FK_NETINT_NB_NBLK_ID IMMEDIATE;

	IF _tally > 0 THEN
		DROP TABLE IF EXISTS __network_interface_netblocks;
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
CREATE OR REPLACE FUNCTION jazzhands.device_collection_after_hooks()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	BEGIN
		PERFORM local_hooks.device_collection_after_hooks();
	EXCEPTION WHEN invalid_schema_name OR undefined_function THEN
			PERFORM 1;
	END;
	RETURN NULL;
END;
$function$
;

-- New function
CREATE OR REPLACE FUNCTION jazzhands.layer2_network_collection_after_hooks()
 RETURNS trigger
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO jazzhands
AS $function$
BEGIN
	BEGIN
		PERFORM local_hooks.layer2_network_collection_after_hooks();
	EXCEPTION WHEN invalid_schema_name OR undefined_function THEN
			PERFORM 1;
	END;
	RETURN NULL;
END;
$function$
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
DROP FUNCTION IF EXISTS schema_support.save_dependant_objects_for_replay ( schema character varying, object character varying, dropit boolean, doobjectdeps boolean );
--
-- Process trigger procs in script_hooks
--
-- Dropping obsoleted sequences....


-- Dropping obsoleted audit sequences....


-- Processing tables with no structural changes
-- Some of these may be redundant
-- fk constraints
-- index
-- triggers
CREATE TRIGGER trigger_member_device_collection_after_hooks AFTER INSERT OR DELETE OR UPDATE ON device_collection_device FOR EACH STATEMENT EXECUTE PROCEDURE device_collection_after_hooks();
CREATE TRIGGER trigger_hier_device_collection_after_hooks AFTER INSERT OR DELETE OR UPDATE ON device_collection_hier FOR EACH STATEMENT EXECUTE PROCEDURE device_collection_after_hooks();
CREATE TRIGGER trigger_member_layer2_network_collection_after_hooks AFTER INSERT OR DELETE OR UPDATE ON l2_network_coll_l2_network FOR EACH STATEMENT EXECUTE PROCEDURE layer2_network_collection_after_hooks();
CREATE TRIGGER trigger_hier_layer2_network_collection_after_hooks AFTER INSERT OR DELETE OR UPDATE ON layer2_network_collection_hier FOR EACH STATEMENT EXECUTE PROCEDURE layer2_network_collection_after_hooks();


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
