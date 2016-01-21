-- Copyright (c) 2016, Todd M. Kover
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

-- This likely needs to die in favor of making hotpants less aware of device
-- collection

CREATE OR REPLACE VIEW v_hotpants_attribute AS
SELECT	login,
	property_name,
	property_type,
	property_value,
	is_boolean,
	device_collection_id,
	'account' as attribute_style
FROM	v_dev_col_user_prop_expanded 
	INNER JOIN Device_Collection USING (Device_Collection_ID)
WHERE	is_enabled = 'Y'
AND	(
		Device_Collection_Type IN ('HOTPants-app', 'HOTPants')
	OR
		Property_Type IN ('RADIUS', 'HOTPants') 
	)
UNION
SELECT  NULL			as login,
	property_name,
	property_type,
	Property_Value_Password_Type	as property_value,
	0				as is_boolean,
	device_collection_id,
	'device'
        FROM    property
        WHERE   Property_Name = 'PWType'
        AND     Property_Type = 'HOTPants'
        AND     account_collection_id IS NULL

;
