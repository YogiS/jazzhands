-- Copyright (c) 2013-2015, Todd M. Kover
-- All rights reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

\set ON_ERROR_STOP

DO $$
DECLARE
        _tal INTEGER;
BEGIN
        select count(*)
        from pg_catalog.pg_namespace
        into _tal
        where nspname = 'dns_utils';
        IF _tal = 0 THEN
                DROP SCHEMA IF EXISTS dns_utils;
                CREATE SCHEMA dns_utils AUTHORIZATION jazzhands;
		COMMENT ON SCHEMA dns_utils IS 'part of jazzhands';
        END IF;
END;
$$;

CREATE OR REPLACE FUNCTION dns_utils.add_ns_records(
	dns_domain_id	dns_domain.dns_domain_id%type
) RETURNS void AS
$$
BEGIN
	EXECUTE '
		INSERT INTO dns_record (
			dns_domain_id, dns_class, dns_type, dns_value
		) select $1, $2, $3, property_value
		FROM property
		WHERE property_name = $4
		AND property_type = $5
	' USING dns_domain_id, 'IN', 'NS', '_authdns', 'Defaults';
END;
$$ 
SET search_path=jazzhands
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION dns_utils.get_or_create_rvs_netblock_link(
	soa_name		dns_domain.soa_name%type,
	dns_domain_id	dns_domain.dns_domain_id%type
) RETURNS netblock.netblock_id%type AS $$
DECLARE
	nblk_id	netblock.netblock_id%type;
	blk text;
	root	text;
	brk	text[];
	ipmember text[];
	ip	inet;
	j text;
BEGIN
	brk := regexp_matches(soa_name, '^(.+)\.(in-addr|ip6)\.arpa$');
	IF brk[2] = 'in-addr' THEN
		j := '.';
	ELSE
		j := ':';
		-- The only thing missing is mapping the number of octets to bits
		RAISE EXCEPTION 'Do not properly handle ipv6 addresses yet.';
	END IF;

	EXECUTE 'select array_agg(member order by rn desc), $2
		from (
        select
			row_number() over () as rn, *
			from
			unnest(regexp_split_to_array($1, $3)) as member
		) x
	' INTO ipmember USING brk[1], j, '\.';

	IF brk[2] = 'in-addr' THEN
		IF array_length(ipmember, 1) > 4 THEN
			RAISE EXCEPTION 'Unable to work with anything smaller than a /24';
		END IF;
		WHILE array_length(ipmember, 1) < 4
		LOOP
			ipmember := array_append(ipmember, '0');
		END LOOP;
		ip := concat(array_to_string(ipmember, j),'/24')::inet;
	ELSE
		ip := concat(array_to_string(ipmember, j),'::')::inet;
	END IF;

	SELECT netblock_id
		INTO	nblk_id
		FROM	netblock
		WHERE	netblock_type = 'dns'
		AND		is_single_address = 'N'
		AND		can_subnet = 'N'
		AND		netblock_status = 'Allocated'
		AND		ip_universe_id = 0
		AND		ip_address = ip;

	IF NOT FOUND THEN
		INSERT INTO netblock (
			ip_address, netblock_type, is_single_address,
			can_subnet, netblock_status, ip_universe_id
		) VALUES (
			ip, 'dns', 'N',
			'N', 'Allocated', 0
		) RETURNING netblock_id INTO nblk_id;
	END IF;

	EXECUTE '
		INSERT INTO dns_record(
			dns_domain_id, dns_class, dns_type, netblock_id
		) values (
			$1, $2, $3, $4
		)
	' USING dns_domain_id, 'IN', 'REVERSE_ZONE_BLOCK_PTR', nblk_id;

	RETURN nblk_id;
END;
$$ 
SET search_path=jazzhands
LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION dns_utils.add_dns_domain(
	soa_name		dns_domain.soa_name%type,
	dns_domain_type	dns_domain.dns_domain_type%type DEFAULT NULL,
	add_nameservers		boolean DEFAULT true
) RETURNS dns_domain.dns_domain_id%type AS $$
DECLARE
	elements		text[];
	parent_zone		text;
	parent_id		dns_domain.dns_domain_id%type;
	domain_id		dns_domain.dns_domain_id%type;
	elem			text;
	sofar			text;
	rvs_nblk_id		netblock.netblock_id%type;
BEGIN
	elements := regexp_split_to_array(soa_name, '\.');
	sofar := '';
	FOREACH elem in ARRAY elements
	LOOP
		IF octet_length(sofar) > 0 THEN
			sofar := sofar || '.';
		END IF;
		sofar := sofar || elem;
		parent_zone := regexp_replace(soa_name, '^'||sofar||'.', '');
		EXECUTE 'SELECT dns_domain_id FROM dns_domain 
			WHERE soa_name = $1' INTO parent_id USING soa_name;
		IF parent_id IS NOT NULL THEN
			EXIT;
		END IF;
	END LOOP;

	IF dns_domain_type IS NULL THEN
		IF soa_name ~ '^.*(in-addr|ip6)\.arpa$' THEN
			dns_domain_type := 'reverse';
		END IF;
	END IF;

	IF dns_domain_type IS NULL THEN
		RAISE EXCEPTION 'Unable to guess dns_domain_type for %',
			soa_name USING ERRCODE = 'not_null_violation'; 
	END IF;

	EXECUTE '
		INSERT INTO dns_domain (
			soa_name,
			soa_class,
			soa_mname,
			soa_rname,
			parent_dns_domain_id,
			should_generate,
			dns_domain_type
		) VALUES (
			$1,
			$2,
			$3,
			$4,
			$5,
			$6,
			$7
		) RETURNING dns_domain_id' INTO domain_id 
		USING soa_name, 
			'IN',
			(select property_value from property where property_type = 'Defaults'
				and property_name = '_dnsmname'),
			(select property_value from property where property_type = 'Defaults'
				and property_name = '_dnsrname'),
			parent_id,
			'Y',
			dns_domain_type
	;

	IF dns_domain_type = 'reverse' THEN
		rvs_nblk_id := dns_utils.get_or_create_rvs_netblock_link(
			soa_name, domain_id);
	END IF;

	IF add_nameservers THEN
		PERFORM dns_utils.add_ns_records(domain_id);
	END IF;

	RETURN domain_id;
END;
$$ 
SET search_path=jazzhands
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION dns_utils.add_domain_from_cidr(
	block		inet
) returns dns_domain.dns_domain_id%TYPE
AS
$$
DECLARE
	ipaddr		text;
	ipnodes		text[];
	domain		text;
	domain_id	dns_domain.dns_domain_id%TYPE;
	j			text;
BEGIN
	-- silently fail for ipv6
	IF family(block) != 4 THEN
		RETURN NULL;
	END IF;
	IF family(block) != 4 THEN
		j := '';
		-- this needs to be tweaked to expand ::, which postgresql does
		-- not easily do.  This requires more thinking than I was up for today.
		ipaddr := regexp_replace(host(block)::text, ':', '', 'g');
	ELSE
		j := '\.';
		ipaddr := host(block);
	END IF;

	EXECUTE 'select array_agg(member order by rn desc)
		from (
        select
			row_number() over () as rn, *
			from
			unnest(regexp_split_to_array($1, $2)) as member
		) x
	' INTO ipnodes USING ipaddr, j;

	IF family(block) = 4 THEN
		domain := array_to_string(ARRAY[ipnodes[2],ipnodes[3],ipnodes[4]], '.')
			|| '.in-addr.arpa';
	ELSE
		domain := array_to_string(ipnodes, '.') 
			|| '.ip6.arpa';
	END IF;

	SELECT dns_domain_id INTO domain_id FROM dns_domain where soa_name = domain;
	IF NOT FOUND THEN
		domain_id := dns_utils.add_dns_domain(domain);
	END IF;

	RETURN domain_id;
END;
$$
SET search_path=jazzhands
LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION dns_utils.add_domains_from_netblock(
	netblock_id		netblock.netblock_id%TYPE
) returns void
AS
$$
DECLARE
	block		inet;
	cur			inet;
BEGIN
	EXECUTE 'SELECT ip_address FROM netblock WHERE netblock_id = $1'
		INTO block
		USING netblock_id;

	IF family(block) = 4 THEN
		FOR cur IN SELECT set_masklen((block + o), 24) 
					FROM generate_series(0, (256 * (2 ^ (24 - 
						masklen(block))) - 1)::integer, 256) as x(o)
		LOOP
			PERFORM * FROM dns_utils.add_domain_from_cidr(cur);
		END LOOP;
	ELSIF family(block) = 6 THEN
			cur := set_masklen(block, 64);
			PERFORM * FROM dns_utils.add_domain_from_cidr(cur);
	ELSE
		RAISE EXCEPTION 'Not IPv% aware.', family(block);
	END IF;
END;
$$
SET search_path=jazzhands
LANGUAGE plpgsql;
