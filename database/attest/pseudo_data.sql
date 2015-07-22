delete from property_collection_property where property_collection_id IN
	(select property_collection_id from property_collection
	 where property_collection_type = 'attestation'
	);

delete from property_collection 
	 where property_collection_type = 'attestation';

delete from val_property_collection_type
	 where property_collection_type = 'attestation';

delete from property where property_type = 'attestation';
delete from val_property where property_type = 'attestation';
delete from val_property_type where property_type = 'attestation';

WITH newptype AS (
	INSERT INTO val_property_type (
		property_type, description
	) VALUES (
		'attestation', 'properties related to regular attestation process'
	) RETURNING *
), newprops AS (
	INSERT INTO val_property (
		property_name, property_type, property_data_type
	) SELECT unnest(ARRAY['ReportAttest', 'FieldAttest', 
'account_collection_membership']),
		property_type, 'string'
	FROM newptype
	RETURNING *
), newpct AS (
	INSERT INTO val_property_collection_type (
		property_collection_type, description
	) VALUES (
		'attestation', 'define elements of regular attestation process'
	) RETURNING *
), newpc AS (
	INSERT INTO property_collection (
		property_collection_name, property_collection_type
	) SELECT 'ReportingAttestation', property_collection_type
	FROM newpct
	RETURNING *
), propcollprop AS (
	INSERT INTO property_collection_property (
		property_collection_id, property_name, property_type
	) SELECT property_collection_id, property_name, property_type
	FROM newpc, newprops
	RETURNING *
), rejchain as (
	INSERT INTO approval_process_chain ( approving_entity
	) VALUES ('jira-hr') 
	RETURNING *
), chain2 as (
	INSERT into approval_process_chain ( approving_entity 
	) values ('manager') RETURNING *
), chain as (
	INSERT into approval_process_chain (
		approving_entity, 
		accept_approval_process_chain_id,
		reject_approval_process_chain_id )
	SELECT 'manager', c.approval_process_chain_id,
		r.approval_process_chain_id
	FROM chain2 c, rejchain r
	RETURNING *
), process as  (
	INSERT INTO approval_process (
		first_approval_process_chain_id,
		approval_process_name,
		approval_process_type,
		property_collection_id
	) SELECT approval_process_chain_id, 
		'ReportingAttest',
		'attestation',
		property_collection_id
		FROM newpc, chain
	RETURNING *
) select * FROM process
;

INSERT INTO property (
	property_name, property_type, property_value
) values
	('ReportAttest', 'attestation', 'auto_acct_coll:AutomatedDirectsAC'),
	('FieldAttest', 'attestation', 'person_company:position_title'),
	('account_collection_membership', 'attestation', 'department');
