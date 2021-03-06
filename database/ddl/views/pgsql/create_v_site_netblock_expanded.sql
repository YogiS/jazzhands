-- Copyright (c) 2013, Todd M. Kover
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
--
-- $Id$
--


-- This view shows the site code for each entry in the netblock table
-- even when it's one of the ancestor netblocks that has the
-- site_netblock assignments

CREATE OR REPLACE VIEW v_site_netblock_expanded AS
WITH RECURSIVE parent_netblock AS (
  SELECT n.netblock_id, n.parent_netblock_id, n.ip_address, sn.site_code
  FROM netblock n LEFT JOIN site_netblock sn on n.netblock_id = sn.netblock_id
  WHERE n.parent_netblock_id IS NULL
  UNION
  SELECT n.netblock_id, n.parent_netblock_id, n.ip_address,
    coalesce(sn.site_code, p.site_code)
  FROM netblock n JOIN parent_netblock p ON n.parent_netblock_id = p.netblock_id
  LEFT JOIN site_netblock sn ON n.netblock_id = sn.netblock_id
)
SELECT site_code, netblock_id FROM parent_netblock;
