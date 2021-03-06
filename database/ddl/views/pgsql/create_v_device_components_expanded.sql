-- Copyright (c) 2016, Matthew Ragan
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

CREATE OR REPLACE VIEW v_device_components_expanded AS 
WITH ctf AS (
	SELECT 
		ctcf.component_type_id,
		array_agg(ctcf.component_function ORDER BY ctcf.component_function) 
			AS functions
	FROM
		jazzhands.component_type_component_func ctcf
	GROUP BY
		ctcf.component_type_id
), ds AS (
	SELECT
		cp.component_type_id,
		cp.property_value::bigint AS disk_size
	FROM
		jazzhands.component_property cp
	WHERE
		cp.component_property_name::text = 'DiskSize'::text AND
		cp.component_property_type::text = 'disk'::text
), ms AS (
	SELECT
	 	cp.component_type_id,
		cp.property_value::bigint AS memory_size
	FROM
		jazzhands.component_property cp
	WHERE
		cp.component_property_name::text = 'MemorySize'::text AND
		cp.component_property_type::text = 'memory'::text
)
SELECT
	dc.device_id,
    c.component_id,
	s.slot_id,
    ct.model,
    a.serial_number,
    ctf.functions,
    s.slot_name,
    ms.memory_size,
    ds.disk_size
FROM
	jazzhands.v_device_components dc JOIN
	jazzhands.component c ON dc.component_id = c.component_id LEFT JOIN
	jazzhands.asset a ON c.component_id = a.component_id JOIN
	jazzhands.component_type ct USING (component_type_id) JOIN
	ctf USING (component_type_id) LEFT JOIN
	ds USING (component_type_id) LEFT JOIN
	ms USING (component_type_id) LEFT JOIN
	jazzhands.slot s ON c.parent_slot_id = s.slot_id;
