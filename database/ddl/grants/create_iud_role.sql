-- Copyright (c) 2005-2010, Vonage Holdings Corp.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY VONAGE HOLDINGS CORP. ''AS IS'' AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL VONAGE HOLDINGS CORP. BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--  Insert Update Delete role
-- $Id$
--



create role iud_role;

grant create session to iud_role;





------


GRANT SELECT ON JAZZHANDS.SEQ_APPLICATION_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_APPLICATION_INSTANCE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_BADGE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_BADGE_TYPE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_CIRCUIT_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_COMPANY_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_COMPOSITE_OS_VERSION_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_DEPT_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_DEVICE_COLLECTION_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_DEVICE_COLLECTION_TYPE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_DEVICE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_DEVICE_POWER_CONNECTION_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_DEVICE_TYPE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_DHCP_RANGE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_DNS_DOMAIN_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_DNS_RECORD_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_EMPLOYEE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_ENCAPSULATION_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_KLOGIN_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_KRB_REALM_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_LAYER1_CONNECTION_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_LAYER2_ENCAPSULATION_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_LOCATION_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_NETBLOCK_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_NETWORK_INTERFACE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_NETWORK_SERVICE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_NOTE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_OPERATING_SYSTEM_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_PARTNER_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_PATCH_PANEL_CONNECTION_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_PHYSICAL_PORT_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_PSEUDO_KLOGIN_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_SECONDARY_NETBLOCK_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_SMTP_AUTH_PASSWORD_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_SNMP_COMMSTR_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_STATIC_ROUTE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_SWITCH_PORT_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_SYSTEM_PARKING_PASS_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_SYSTEM_USER_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_SYSTEM_USER_IMAGE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_SYSTEM_USER_LOCATION_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_SYSTEM_USER_PHONE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_SYSTEM_USER_VEHICLE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_TOKEN_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_UCLASS_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_UCLASS_PROPERTY_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_UNIX_GID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_UNIX_GROUP_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_UNIX_GROUP_PROP_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_UNIX_UID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_VLAN_RANGE_ID TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.SEQ_WWW_GROUP_ID TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$APPLICATION TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$APPLICATION_INSTANCE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$BADGE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$BADGE_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$CIRCUIT TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$COMPANY TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$COMPOSITE_OS_VERSION TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEPT TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEPT_MEMBER TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEVICE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEVICE_COLLECTION TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEVICE_COLLECTION_HIER TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEVICE_COLLECTION_MEMBER TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEVICE_COLLECTION_UCLASS TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEVICE_FUNCTION TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEVICE_NOTE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEVICE_POWER_CONNECTION TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEVICE_POWER_INTERFACE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEVICE_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEVICE_TYPE_POWER_PORT_TEM TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DEVICE_TYPE_SERIAL_PORT_TE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DHCP_RANGE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DNS_DOMAIN TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$DNS_RECORD TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$ENCAPSULATION TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$KERBEROS_REALM TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$KLOGIN TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$KLOGIN_MCLASS TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$LAYER1_CONNECTION TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$LAYER2_ENCAPSULATION TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$LOCATION TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$MCLASS_APPLICATION TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$MCLASS_EXTRA_FILE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$MCLASS_GROUP TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$MCLASS_PROPERTY_OVERRIDE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$MCLASS_UNIX_PROP TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$MCLASS_WWW_GROUP TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$NETBLOCK TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$NETWORK_INTERFACE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$NETWORK_SERVICE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$OPERATING_SYSTEM TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$PARTNER TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$PHYSICAL_CONNECTION TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$PHYSICAL_PORT TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$PSEUDO_KLOGIN TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SECONDARY_NETBLOCK TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SITE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SITE_NETBLOCK TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SNMP_COMMSTR TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$STATIC_ROUTE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$STATIC_ROUTE_TEMPLATE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SYSTEM_PARKING_PASS TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SYSTEM_PASSWORD TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SYSTEM_USER TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SYSTEM_USER_IMAGE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SYSTEM_USER_LOCATION TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SYSTEM_USER_PHONE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SYSTEM_USER_TOKEN TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SYSTEM_USER_VEHICLE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SYSTEM_USER_XREF TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$TOKEN TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$UCLASS TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$UCLASS_DEPT TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$UCLASS_HIER TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$UCLASS_PROPERTY_OVERRIDE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$UCLASS_USER TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$UNIX_GROUP TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$UNIX_GROUP_MEMBER TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$UNIX_GROUP_PROPERTY TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$UNIX_GROUP_UCLASS TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$USER_UNIX_INFO TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_AUTHENTICATION_METHOD TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_BADGE_STATUS TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_CABLE_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_COUNTRY_CODE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_DATABASE_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_DEVICE_COLLECTION_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_DEVICE_FUNCTION_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_DNS_CLASS TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_DNS_SRV_SERVICE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_DNS_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_ENCAPSULATION_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_IMAGE_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_MCLASS_EXTRA_FILE_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_MCLASS_UNIX_HOME_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_MCLASS_UNIX_PW_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_NETBLOCK_STATUS TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_NETWORK_INTERFACE_PURP TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_NETWORK_INTERFACE_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_NETWORK_SERVICE_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_OFFICE_SITE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_OWNERSHIP_STATUS TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_PHONE_NUMBER_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_PLUG_STYLE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_PORT_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_PRODUCTION_STATE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_REPORTING_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_SNMP_COMMSTR_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_STATUS TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_SYSTEM_USER_STATUS TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_SYSTEM_USER_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_TOKEN_STATUS TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_TOKEN_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_UCLASS_PROPERTY_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_UCLASS_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_UNIX_GROUP_PROP_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VAL_USER_LOCATION_TYPE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$VLAN_RANGE TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$WWW_GROUP TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$WWW_GROUP_MEMBER TO IUD_ROLE;
GRANT SELECT                      ON JAZZHANDS.AUD$SYSTEM_USER_NOTE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.APPLICATION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.APPLICATION_INSTANCE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.BADGE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.BADGE_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.CIRCUIT TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.COMPANY TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.COMPOSITE_OS_VERSION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.CREATE$JAVA$LOB$TABLE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEPT TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEPT_MEMBER TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEVICE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEVICE_COLLECTION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEVICE_COLLECTION_HIER TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEVICE_COLLECTION_MEMBER TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEVICE_COLLECTION_UCLASS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEVICE_FUNCTION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEVICE_NOTE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEVICE_POWER_CONNECTION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEVICE_POWER_INTERFACE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEVICE_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEVICE_TYPE_POWER_PORT_TEMPLT TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DEVICE_TYPE_SERIAL_PORT_TEMPLT TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DHCP_RANGE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DNS_DOMAIN TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.DNS_RECORD TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.ENCAPSULATION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.KERBEROS_REALM TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.KLOGIN TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.KLOGIN_MCLASS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.LAYER1_CONNECTION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.LAYER2_ENCAPSULATION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.LOCATION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.MCLASS_APPLICATION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.MCLASS_EXTRA_FILE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.MCLASS_GROUP TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.MCLASS_PROPERTY_OVERRIDE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.MCLASS_UNIX_PROP TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.MCLASS_WWW_GROUP TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.NETBLOCK TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.NETWORK_INTERFACE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.NETWORK_SERVICE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.OPERATING_SYSTEM TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.PARTNER TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.PHYSICAL_CONNECTION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.PHYSICAL_PORT TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.PSEUDO_KLOGIN TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SECONDARY_NETBLOCK TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SITE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SITE_NETBLOCK TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SNMP_COMMSTR TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.STATIC_ROUTE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.STATIC_ROUTE_TEMPLATE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SYSTEM_PARKING_PASS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SYSTEM_PASSWORD TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SYSTEM_USER TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SYSTEM_USER_IMAGE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SYSTEM_USER_LOCATION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SYSTEM_USER_PHONE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SYSTEM_USER_TOKEN TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SYSTEM_USER_VEHICLE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SYSTEM_USER_XREF TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.TOKEN TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.UCLASS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.UCLASS_DEPT TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.UCLASS_HIER TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.UCLASS_PROPERTY_OVERRIDE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.UCLASS_USER TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.UNIX_GROUP TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.UNIX_GROUP_MEMBER TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.UNIX_GROUP_PROPERTY TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.UNIX_GROUP_UCLASS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.USER_UNIX_INFO TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_AUTHENTICATION_METHOD TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_BADGE_STATUS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_CABLE_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_COUNTRY_CODE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_DATABASE_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_DEVICE_COLLECTION_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_DEVICE_FUNCTION_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_DNS_CLASS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_DNS_SRV_SERVICE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_DNS_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_ENCAPSULATION_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_IMAGE_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_MCLASS_EXTRA_FILE_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_MCLASS_UNIX_HOME_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_MCLASS_UNIX_PW_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_NETBLOCK_STATUS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_NETWORK_INTERFACE_PURPOSE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_NETWORK_INTERFACE_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_NETWORK_SERVICE_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_OFFICE_SITE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_OWNERSHIP_STATUS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_PHONE_NUMBER_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_PLUG_STYLE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_PORT_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_PRODUCTION_STATE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_REPORTING_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_SNMP_COMMSTR_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_STATUS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_SYSTEM_USER_STATUS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_SYSTEM_USER_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_TOKEN_STATUS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_TOKEN_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_UCLASS_PROPERTY_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_UCLASS_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_UNIX_GROUP_PROP_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_USER_LOCATION_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VLAN_RANGE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.WWW_GROUP TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.WWW_GROUP_MEMBER TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.SYSTEM_USER_NOTE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_DEVICE_AUTO_MGMT_PROTOCOL TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE ON JAZZHANDS.VAL_UCLASS_PROPERTY_NAME TO IUD_ROLE;
GRANT SELECT			 ON JAZZHANDS.AUD$VAL_UCLASS_PROPERTY_NAME TO IUD_ROLE;
GRANT SELECT			ON JAZZHANDS.AUD$VAL_DEVICE_AUTO_MGMT_PROTO TO IUD_ROLE;
GRANT SELECT			ON JAZZHANDS.AUD$SYSTEM_USER_NOTE TO IUD_ROLE;

GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.SW_PACKAGE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.SW_PACKAGE_RELATION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.SW_PACKAGE_RELEASE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.SW_PACKAGE_REPOSITORY TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.TOKEN TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.TOKEN_SEQUENCE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_BAUD TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_DATA_BITS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_FLOW_CONTROL TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_PACKAGE_RELATION_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_PARITY TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_PROCESSOR_ARCHITECTURE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_STOP_BITS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_SW_PACKAGE_FORMAT TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_SYMBOLIC_TRACK_NAME TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_UPGRADE_SEVERITY TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_VOE_STATE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VOE_RELATION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VOE_SW_PACKAGE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VOE_SYMBOLIC_TRACK TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VOE TO IUD_ROLE;


GRANT SELECT                            ON JAZZHANDS.AUD$SW_PACKAGE TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$SW_PACKAGE_RELATION TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$SW_PACKAGE_RELEASE TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$SW_PACKAGE_REPOSITORY TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$TOKEN TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_BAUD TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_DATA_BITS TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_FLOW_CONTROL TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_PACKAGE_RELATION_TYPE TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_PARITY TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_PROCESSOR_ARCHITECTURE TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_STOP_BITS TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_SW_PACKAGE_FORMAT TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_SYMBOLIC_TRACK_NAME TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_UPGRADE_SEVERITY TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_VOE_STATE TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VOE_RELATION TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VOE_SW_PACKAGE TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VOE_SYMBOLIC_TRACK TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VOE TO IUD_ROLE;

GRANT SELECT ON JAZZHANDS.V_LOGIN_CHANGES TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.V_LOGIN_CHANGES_EXTRACT TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.V_SYSTEM_USER TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.V_SYSTEM_USER_PHONE TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.V_TOKEN TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.V_USER_DELETIONS TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.V_USER_DELETIONS_EXTRACT TO IUD_ROLE;
GRANT SELECT ON JAZZHANDS.V_USER_EXTRACT TO IUD_ROLE;


GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.DNS_RECORD_RELATION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.SUDO_ALIAS TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.SUDO_DEFAULT TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.SUDO_UCLASS_DEVICE_COLLECTION TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_DNS_RECORD_RELATION_TYPE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_PORT_PURPOSE TO IUD_ROLE;
GRANT SELECT,INSERT,UPDATE,DELETE       ON JAZZHANDS.VAL_SW_PACKAGE_TYPE TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$DNS_RECORD_RELATION TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$SUDO_ALIAS TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$SUDO_DEFAULT TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$SUDO_UCLASS_DEVICE_COLLECT TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_DNS_RECORD_RELATION_TY TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_PORT_PURPOSE TO IUD_ROLE;
GRANT SELECT                            ON JAZZHANDS.AUD$VAL_SW_PACKAGE_TYPE TO IUD_ROLE;



