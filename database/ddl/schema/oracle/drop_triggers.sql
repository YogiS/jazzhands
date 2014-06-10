
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

--
-- Copyright (c) 2010-2014 Todd Kover
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



--
-- $Id$
--

DROP TRIGGER C_TIUBR_ACCOUNT;

DROP TRIGGER C_TIB_ACCOUNT;

DROP TRIGGER TUB_ACCOUNT;

DROP TRIGGER K_TAIU_ACCOUNT_COMPANY;

DROP TRIGGER K_TAU_ACCOUNT_STATUS;

DROP TRIGGER K_TAIU_ACCOUNT_TYPE;

DROP TRIGGER C_TIUBR_SYS_USER_ASSIGND_CERT;

DROP TRIGGER TUB_SYS_USER_ASSIGND_CERT;

DROP TRIGGER C_TIUBR_ACCOUNT_AUTH_LOG;

DROP TRIGGER TUB_ACCOUNT_AUTH_LOG;

DROP TRIGGER TIB_ACCOUNT_AUTH_LOG;

DROP TRIGGER C_TIUBR_ACCOUNT_COLLECTION;

DROP TRIGGER TIB_ACCOUNT_COLLECTION;

DROP TRIGGER TUB_ACCOUNT_COLLECTION;

DROP TRIGGER C_TIUBR_ACCOUNT_COLLECTION_ACC;

DROP TRIGGER TUB_ACCOUNT_COLLECTION_ACCOUNT;

DROP TRIGGER C_TIUBR_ACCOUNT_COLLECTION_HIE;

DROP TRIGGER TUB_ACCOUNT_COLLECTION_HIER;

DROP TRIGGER C_TIUBR_ACCOUNT_PASSWORD;

DROP TRIGGER TUB_ACCOUNT_PASSWORD;

DROP TRIGGER C_TIUBR_ACCOUNT_REALM;

DROP TRIGGER TIB_ACCOUNT_REALM;

DROP TRIGGER TUB_ACCOUNT_REALM;

DROP TRIGGER C_TIUBR_ACCOUNT_REALM_ACCT_COL;

DROP TRIGGER TUB_ACCOUNT_REALM_ACCT_COLLECT;

DROP TRIGGER C_TIUBR_ACCOUNT_REALM_COMPANY;

DROP TRIGGER TUB_ACCOUNT_REALM_COMPANY;

DROP TRIGGER C_TIUBR_ACCOUNT_SSH_KEY;

DROP TRIGGER TUB_ACCOUNT_SSH_KEY;

DROP TRIGGER TUB_ACCOUNT_TOKEN;

DROP TRIGGER C_TIUBR_ACCOUNT_TOKEN;

DROP TRIGGER TIB_ACCOUNT_TOKEN;

DROP TRIGGER C_TIUBR_USER_UNIX_INFO;

DROP TRIGGER TUB_USER_UNIX_INFO;

DROP TRIGGER C_TIUBR_APPAAL;

DROP TRIGGER TUB_APPAAL;

DROP TRIGGER TIB_APPAAL;

DROP TRIGGER C_TIUBR_APPAAL_INSTANCE;

DROP TRIGGER TIB_APPAAL_INSTANCE;

DROP TRIGGER TUB_APPAAL_INSTANCE;

DROP TRIGGER C_TIUBR_APPAAL_INSTANCE_DEVICE;

DROP TRIGGER TUB_APPAAL_INSTANCE_DEVICE_COL;

DROP TRIGGER C_TIUBR_APPAAL_INSTANCE_PROPER;

DROP TRIGGER TUB_APPAAL_INSTANCE_PROPERTY;

DROP TRIGGER C_TIUBR_ASSET;

DROP TRIGGER TUB_ASSET;

DROP TRIGGER TIB_ASSET;

DROP TRIGGER C_TIUBR_BADGE;

DROP TRIGGER TUB_BADGE;

DROP TRIGGER C_TIUBR_BADGE_TYPE;

DROP TRIGGER TIB_BADGE_TYPE;

DROP TRIGGER TUB_BADGE_TYPE;

DROP TRIGGER C_TIUBR_CHASSIS_LOCATION;

DROP TRIGGER TIB_CHASSIS_LOCATION;

DROP TRIGGER TUB_CHASSIS_LOCATION;

DROP TRIGGER C_TIUBR_CIRCUIT;

DROP TRIGGER TIB_CIRCUIT;

DROP TRIGGER TUB_CIRCUIT;

DROP TRIGGER C_TIUBR_COMPANY;

DROP TRIGGER TIB_COMPANY;

DROP TRIGGER TUB_COMPANY;

DROP TRIGGER Trigger_13102;

DROP TRIGGER Trigger_13103;

DROP TRIGGER C_TIUBR_CONTRACT;

DROP TRIGGER TUB_CONTRACT;

DROP TRIGGER TIB_CONTRACT;

DROP TRIGGER TUB_CONTRACT_TYPE;

DROP TRIGGER C_TIUBR_CONTRACT_TYPE;

DROP TRIGGER C_TIUBR_DEPT;

DROP TRIGGER TIB_DEPT;

DROP TRIGGER TUB_DEPT;

DROP TRIGGER C_TIUBR_DEVICE;

DROP TRIGGER K_TIUBR_DEVICE;

DROP TRIGGER TIB_DEVICE;

DROP TRIGGER TUB_DEVICE;

DROP TRIGGER C_TIUBR_DEVICE_COLLECTION;

DROP TRIGGER TIB_DEVICE_COLLECTION;

DROP TRIGGER TUB_DEVICE_COLLECTION;

DROP TRIGGER C_TIUBR_DEV_COLL_ASSIGND_CERT;

DROP TRIGGER TUB_DEV_COLL_ASSIGND_CERT;

DROP TRIGGER C_TIUBR_DEVICE_COLLECTION_DEVI;

DROP TRIGGER TUB_DEVICE_COLLECTION_DEVICE;

DROP TRIGGER C_TIUBR_DEVICE_COLLECTION_HIER;

DROP TRIGGER TUB_DEVICE_COLLECTION_HIER;

DROP TRIGGER C_TIUBR_DEVICE_COLLECTION_SSH_;

DROP TRIGGER TUB_DEVICE_COLLECTION_SSH_KEY;

DROP TRIGGER C_TIUBR_DEVICE_ENCAPSULATION_D;

DROP TRIGGER TUB_DEVICE_ENCAPSULATION_DOMAI;

DROP TRIGGER C_TIUBR_DEVICE_LAYER2_NETWORK;

DROP TRIGGER TUB_DEVICE_LAYER2_NETWORK;

DROP TRIGGER C_CIUBR_DEVICE_MANAGEMENT_CONT;

DROP TRIGGER TUB_DEVICE_MANAGEMENT_CONTROLL;

DROP TRIGGER C_TIUBR_DEVICE_NOTE;

DROP TRIGGER TIB_DEVICE_NOTE;

DROP TRIGGER TUB_DEVICE_NOTE;

DROP TRIGGER C_TIUBR_DEVICE_POWER_CONNECTIO;

DROP TRIGGER TIB_DEVICE_POWER_CONNECTION;

DROP TRIGGER TUB_DEVICE_POWER_CONNECTION;

DROP TRIGGER C_TIUBR_DEVICE_POWER_INTERFACE;

DROP TRIGGER TUB_DEVICE_POWER_INTERFACE;

DROP TRIGGER C_TIUBR_DEVICE_SSH_KEY;

DROP TRIGGER TUB_DEVICE_SSH_KEY;

DROP TRIGGER C_TIBUR_DEVICE_TICKET;

DROP TRIGGER TUB_DEVICE_TICKET;

DROP TRIGGER C_TIUBR_DEVICE_TYPE;

DROP TRIGGER TIB_DEVICE_TYPE;

DROP TRIGGER TUB_DEVICE_TYPE;

DROP TRIGGER C_TIUBR_DEVICE_TYPE_MODULE;

DROP TRIGGER TUB_DEVICE_TYPE_MODULE;

DROP TRIGGER Trigger_18706;

DROP TRIGGER Trigger_18707;

DROP TRIGGER C_TIUBR_DEVICE_TYPE_PHYS_PORT_;

DROP TRIGGER TUB_DEVICE_TYPE_PHYS_PORT_TEMP;

DROP TRIGGER C_TIUBR_DEVICE_TYPE_POWER_PORT;

DROP TRIGGER TUB_DEVICE_TYPE_POWER_PORT_TEM;

DROP TRIGGER C_TIUBR_DNS_CHANGE_RECORD;

DROP TRIGGER TIB_DNS_CHANGE_RECORD;

DROP TRIGGER TUB_DNS_CHANGE_RECORD;

DROP TRIGGER C_TIUBR_DNS_DOMAIN;

DROP TRIGGER K_TIUBR_DNS_DOMAIN;

DROP TRIGGER TIB_DNS_DOMAIN;

DROP TRIGGER TUB_DNS_DOMAIN;

DROP TRIGGER C_TIUBR_DNS_RECORD;

DROP TRIGGER K_TIUDBR_DNS_RECORD;

DROP TRIGGER TIB_DNS_RECORD;

DROP TRIGGER TUB_DNS_RECORD;

DROP TRIGGER TUB_DNS_RECORD_RELATION;

DROP TRIGGER C_TIUBR_DNS_RECORD_RELATION;

DROP TRIGGER C_TIUBR_ENCAPSULATION_DOMAIN;

DROP TRIGGER TUB_ENCAPSULATION_DOMAIN;

DROP TRIGGER TIB_ENCAPSULATION_RANGE;

DROP TRIGGER TUB_ENCAPSULATION_RANGE;

DROP TRIGGER C_TIUBR_ENCAPSULATION_RANGE;

DROP TRIGGER C_TIUBR_ENCRYPTION_KEY;

DROP TRIGGER TIB_ENCRYPTION_KEY;

DROP TRIGGER TUB_ENCRYPTION_KEY;

DROP TRIGGER C_CIUBR_IP_GROUP;

DROP TRIGGER TUB_IP_GROUP;

DROP TRIGGER C_CIUBR_IP_GROUP_NET_INTERFACE;

DROP TRIGGER TUB_IP_GROUP_NET_INTERFACE;

DROP TRIGGER C_TIUBR_IP_UNIVERSE;

DROP TRIGGER TUB_IP_UNIVERSE;

DROP TRIGGER TIB_IP_UNIVERSE;

DROP TRIGGER C_TIUBR_KERBEROS_REALM;

DROP TRIGGER TIB_KERBEROS_REALM;

DROP TRIGGER TUB_KERBEROS_REALM;

DROP TRIGGER C_TIUBR_KLOGIN;

DROP TRIGGER TIB_KLOGIN;

DROP TRIGGER TUB_KLOGIN;

DROP TRIGGER C_TIUBR_KLOGIN_MCLASS;

DROP TRIGGER TUB_KLOGIN_MCLASS;

DROP TRIGGER C_TIUBR_LAYER1_CONNECTION;

DROP TRIGGER TIB_LAYER1_CONNECTION;

DROP TRIGGER TUB_LAYER1_CONNECTION;

DROP TRIGGER C_TIUA_LAYER1_CONNECTION;

DROP TRIGGER TIB_LAYER2_CONNECTION;

DROP TRIGGER TUB_LAYER2_CONNECTION;

DROP TRIGGER C_TIUBR_LAYER2_CONNECTION;

DROP TRIGGER C_TIUBR_VAL_LAYER2_ENCAPSULATI;

DROP TRIGGER TUB_LAYER2_ENCAPSULATION_TYPE;

DROP TRIGGER TIB_LAYER2_NETWORK;

DROP TRIGGER TUB_LAYER2_NETWORK;

DROP TRIGGER C_TIUBR_LAYER2_NETWORK;

DROP TRIGGER TIB_LAYER3_NETWORK;

DROP TRIGGER TUB_LAYER3_NETWORK;

DROP TRIGGER C_TIUBR_LAYER3_NETWORK;

DROP TRIGGER TIB_LOGICAL_PORT;

DROP TRIGGER TUB_LOGICAL_PORT;

DROP TRIGGER C_TIUBR_LOGICAL_PORT;

DROP TRIGGER TIB_MLAG_PEERING;

DROP TRIGGER TUB_MLAG_PEERING;

DROP TRIGGER C_TIUBR_MLAG_PEERING;

DROP TRIGGER C_TIUBR_NETBLOCK;

DROP TRIGGER K_TAIU_NONROW_NETBLOCK;

DROP TRIGGER K_TBIU_NETBLOCK;

DROP TRIGGER K_TBIU_NONROW_NETBLOCK;

DROP TRIGGER TIB_NETBLOCK;

DROP TRIGGER TUB_NETBLOCK;

DROP TRIGGER K_TIUB_NETBLOCK;

DROP TRIGGER C_TIUBR_NETBLOCK_COLLECTION;

DROP TRIGGER TIB_NETBLOCK_COLLECTION;

DROP TRIGGER TUB_NETBLOCK_COLLECTION;

DROP TRIGGER C_TIUBR_NETBLOCK_COLLECTION_HI;

DROP TRIGGER TUB_NETBLOCK_COLLECTION_HIER;

DROP TRIGGER C_TIUBR_ACCOUNT_COLLECTION_ACC;

DROP TRIGGER TUB_ACCOUNT_COLLECTION_ACCOUNT;

DROP TRIGGER C_TIUBR_NETWORK_INTERFACE;

DROP TRIGGER TIB_NETWORK_INTERFACE;

DROP TRIGGER TUB_NETWORK_INTERFACE;

DROP TRIGGER TUB_NETWORK_INTERFACE_NETBLOCK;

DROP TRIGGER C_TIUBR_NETWORK_INTERFACE_NETB;

DROP TRIGGER C_CIUBR_NETWORK_INTERFACE_PURP;

DROP TRIGGER TUB_NETWORK_INTERFACE_PURPOSE;

DROP TRIGGER C_TIUBR_NETWORK_RANGE;

DROP TRIGGER TIB_NETWORK_RANGE;

DROP TRIGGER TUB_NETWORK_RANGE;

DROP TRIGGER C_TIUBR_NETWORK_SERVICE;

DROP TRIGGER TIB_NETWORK_SERVICE;

DROP TRIGGER TUB_NETWORK_SERVICE;

DROP TRIGGER C_TIUBR_OPERATING_SYSTEM;

DROP TRIGGER TIB_OPERATING_SYSTEM;

DROP TRIGGER TUB_OPERATING_SYSTEM;

DROP TRIGGER C_TIUBR_PERSON;

DROP TRIGGER C_TIB_PERSON;

DROP TRIGGER TUB_PERSON;

DROP TRIGGER C_TIUBR_PERSON_ACCT_REALM_COMP;

DROP TRIGGER TUB_PERSON_ACCT_REALM_COMPANY;

DROP TRIGGER TUB_PERSON_AUTH_QUESTION;

DROP TRIGGER C_TIUBR_PERSON_AUTH_QUESTION;

DROP TRIGGER C_TIUBR_PERSON_COMPANY;

DROP TRIGGER TUB_PERSON_COMPANY;

DROP TRIGGER C_TIUBR_PERSON_COMPANY_BADGE;

DROP TRIGGER TUB_PERSON_COMPANY_BADGE;

DROP TRIGGER C_TIUBR_PERSON_CONTACT;

DROP TRIGGER TIB_PERSON_CONTACT;

DROP TRIGGER TUB_PERSON_CONTACT;

DROP TRIGGER C_TIUBR_PERSON_IMAGE;

DROP TRIGGER TIB_PERSON_IMAGE;

DROP TRIGGER TUB_PERSON_IMAGE;

DROP TRIGGER C_TIUBR_PERSON_IMAGE_USAGE;

DROP TRIGGER TUB_PERSON_IMAGE_USAGE;

DROP TRIGGER C_TIUBR_PERSON_LOCATION;

DROP TRIGGER TIB_PERSON_LOCATION;

DROP TRIGGER TUB_PERSON_LOCATION;

DROP TRIGGER K_TAIUD_PERSON_SITE;

DROP TRIGGER TIB_PRESON_NOTE;

DROP TRIGGER TUB_PERSON_NOTE;

DROP TRIGGER C_TIUBR_PERSON_NOTE;

DROP TRIGGER C_TIUBR_PERSON_PARKING_PASS;

DROP TRIGGER TIB_PERSON_PARKING_PASS;

DROP TRIGGER TUB_PERSON_PARKING_PASS;

DROP TRIGGER C_TIUBR_PERSON_VEHICLE;

DROP TRIGGER TIB_PERSON_VEHICLE;

DROP TRIGGER TUB_PERSON_VEHICLE;

DROP TRIGGER C_TIUBR_PHYSICAL_ADDRESS;

DROP TRIGGER TUB_PHYSICAL_ADDRESS;

DROP TRIGGER TIB_PHYSICAL_ADDRESS;

DROP TRIGGER C_TIUBR_PHYSICAL_CONNECTION;

DROP TRIGGER TIB_PHYSICAL_CONNECTION;

DROP TRIGGER TUB_PHYSICAL_CONNECTION;

DROP TRIGGER C_TIUA_PHYSICAL_CONNECTION;

DROP TRIGGER C_TIUBR_PHYSICAL_PORT;

DROP TRIGGER TIB_PHYSICAL_PORT;

DROP TRIGGER TUB_PHYSICAL_PORT;

DROP TRIGGER TIB_PROPERTY;

DROP TRIGGER C_TIBUR_PROPERTY;

DROP TRIGGER TUB_PROPERTY;

DROP TRIGGER K_TAIU_NONROW_PROPERTY;

DROP TRIGGER K_TBIU_NONROW_PROPERTY;

DROP TRIGGER K_TBIU_PROPERTY;

DROP TRIGGER C_TIUBR_PSEUDO_KLOGIN;

DROP TRIGGER TIB_PSEUDO_KLOGIN;

DROP TRIGGER TUB_PSEUDO_KLOGIN;

DROP TRIGGER TIB_RACK;

DROP TRIGGER C_TIUBR_RACK;

DROP TRIGGER TUB_RACK;

DROP TRIGGER C_TIUBR_RACK_LOCATION;

DROP TRIGGER TIB_RACK_LOCATION;

DROP TRIGGER TUB_RACK_LOCATION;

DROP TRIGGER C_TIUBR_SERVICE_ENVIRONMEN;

DROP TRIGGER TUB_SERVICE_ENVIRONMENT;

DROP TRIGGER C_TIUBR_SVC_ENV_HIER;

DROP TRIGGER TAB_SVC_ENV_HIER;

DROP TRIGGER C_TIUBR_SVC_ENV_COLLECTION;

DROP TRIGGER TUB_SERVICE_ENVIRONMENT_COLLEC;

DROP TRIGGER C_TIUBR_SITE;

DROP TRIGGER TUB_SITE;

DROP TRIGGER C_TIUBR_SITE_NETBLOCK;

DROP TRIGGER TUB_SITE_NETBLOCK;

DROP TRIGGER C_TIUBR_SNMP_COMMSTR;

DROP TRIGGER K_TBIU_SNMP_COMMSTR;

DROP TRIGGER TIB_SNMP_COMMSTR;

DROP TRIGGER TUB_SNMP_COMMSTR;

DROP TRIGGER C_TIUBR_SSH_KEY;

DROP TRIGGER TUB_SSH_KEY;

DROP TRIGGER TIB_SSH_KEY;

DROP TRIGGER C_TIUBR_STATIC_ROUTE;

DROP TRIGGER TIB_STATIC_ROUTE;

DROP TRIGGER TUB_STATIC_ROUTE;

DROP TRIGGER C_TIUBR_STATIC_ROUTE_TEMPLATE;

DROP TRIGGER TIB_STATIC_ROUTE_TEMPLATE;

DROP TRIGGER TUB_STATIC_ROUTE_TEMPLATE;

DROP TRIGGER C_TIUBR_SUDO_USERCOL_DEVCOL;

DROP TRIGGER TUB_SUDO_USERCOL_DEVCOL;

DROP TRIGGER TUB_SUDO_ALIAS;

DROP TRIGGER C_TIUBR_SUDO_ALIAS;

DROP TRIGGER C_TIUBR_SVC_ENV_COLL_SVC_ENV;

DROP TRIGGER TUB_SVC_ENV_COLL_SVC_ENV;

DROP TRIGGER TIB_SW_PACKAGE;

DROP TRIGGER TUB_SW_PACKAGE;

DROP TRIGGER C_TIUBR_SW_PACKAGE;

DROP TRIGGER TIB_SW_PACKAGE_RELATION;

DROP TRIGGER TUB_SW_PACKAGE_RELATION;

DROP TRIGGER C_TIUBR_SW_PACKAGE_RELATION;

DROP TRIGGER C_TIUBR_SW_PACKAGE_RELEASE;

DROP TRIGGER TIB_SW_PACKAGE_RELEASE;

DROP TRIGGER TUB_SW_PACKAGE_RELEASE;

DROP TRIGGER TIB_SW_PACKAGE_REPOSITORY;

DROP TRIGGER TUB_SW_PACKAGE_REPOSITORY;

DROP TRIGGER C_TIUBR_SW_PACKAGE_REPOSITORY;

DROP TRIGGER C_TIUBR_TICKETING_SYSTEM;

DROP TRIGGER TIB_TICKETING_SYSTEM;

DROP TRIGGER TUB_TICKETING_SYSTEM;

DROP TRIGGER C_TIUBR_TOKEN;

DROP TRIGGER TIB_TOKEN;

DROP TRIGGER TUB_TOKEN;

DROP TRIGGER TIB_TOKEN_COLLECTION;

DROP TRIGGER TUB_TOKEN_COLLECTION;

DROP TRIGGER C_TIUBR_TOKEN_COLLECTION;

DROP TRIGGER TUB_TOKEN_COLLECTION_HIER;

DROP TRIGGER C_TIUBR_TOKEN_COLLECTION_HIER;

DROP TRIGGER TUB_TOKEN_COL_MEMBR;

DROP TRIGGER C_TIUBR_COL_MEMBR;

DROP TRIGGER C_TIUBR_UNIX_GROUP;

DROP TRIGGER TUB_UNIX_GROUP;

DROP TRIGGER C_TIUBR_VAL_ACCT_COL_TYPE;

DROP TRIGGER TUB_VAL_ACCT_COL_TYPE;

DROP TRIGGER C_TIUBR_VAL_ACCOUNT_ROLE;

DROP TRIGGER TUB_VAL_ACCOUNT_ROLE;

DROP TRIGGER C_TIUBR_VAL_ACCOUNT_TYPE;

DROP TRIGGER TUB_VAL_ACCOUNT_TYPE;

DROP TRIGGER C_TIUBR_VAL_APP_KEY;

DROP TRIGGER TUB_VAL_APP_KEY;

DROP TRIGGER C_TIUBR_VAL_APP_KEY_VALUES;

DROP TRIGGER TUB_VAL_APP_KEY_VALUES;

DROP TRIGGER C_TIUBR_VAL_AUTH_QUESTION;

DROP TRIGGER TIB_VAL_AUTH_QUESTION;

DROP TRIGGER TUB_VAL_AUTH_QUESTION;

DROP TRIGGER C_TIUBR_VAL_AUTH_RESOURCE;

DROP TRIGGER TUB_VAL_AUTH_RESOURCE;

DROP TRIGGER C_TIUBR_VAL_BADGE_STATUS;

DROP TRIGGER TUB_VAL_BADGE_STATUS;

DROP TRIGGER TUB_VAL_BAUD;

DROP TRIGGER C_TIUBR_VAL_BAUD;

DROP TRIGGER C_TIUBR_VAL_CABLE_TYPE;

DROP TRIGGER TUB_VAL_CABLE_TYPE;

DROP TRIGGER C_TIUBR_VAL_COMPANY_TYPE;

DROP TRIGGER TUB_VAL_COMPANY_TYPE;

DROP TRIGGER TUB_VAL_CONTRACT_TYPE;

DROP TRIGGER C_TIUBR_VAL_CONTRACT_TYPE;

DROP TRIGGER C_TIUBR_VAL_COUNTRY_CODE;

DROP TRIGGER TUB_VAL_COUNTRY_CODE;

DROP TRIGGER TUB_VAL_DATA_BITS;

DROP TRIGGER C_TIUBR_VAL_DATA_BITS;

DROP TRIGGER TUB_VAL_DEVICE_AUTO_MGMT_PROTO;

DROP TRIGGER C_TIUBR_VAL_DEVICE_AUTO_MGMT_P;

DROP TRIGGER C_TIUBR_VAL_DEVICE_COLLECTION_;

DROP TRIGGER TUB_VAL_DEVICE_COLLECTION_TYPE;

DROP TRIGGER C_TIUBR_VAL_DEVICE_MGMT_CTRL_T;

DROP TRIGGER TUB_VAL_DEVICE_MGMT_CTRL_TYPE;

DROP TRIGGER C_TIUBR_VAL_STATUS;

DROP TRIGGER TUB_VAL_STATUS;

DROP TRIGGER C_TIUBR_VAL_DIET;

DROP TRIGGER TUB_VAL_DIET;

DROP TRIGGER C_TIUBR_VAL_DNS_CLASS;

DROP TRIGGER TUB_VAL_DNS_CLASS;

DROP TRIGGER TUB_VAL_DNS_DOMAIN_TYPE;

DROP TRIGGER C_TIUBR_VAL_DNS_DOMAIN_TYPE;

DROP TRIGGER TUB_VAL_DNS_RECORD_RELATION_TY;

DROP TRIGGER C_TIUBR_VAL_DNS_RECORD_RELATIO;

DROP TRIGGER C_TIUBR_VAL_DNS_SRV_SERVICE;

DROP TRIGGER TUB_VAL_DNS_SRV_SERVICE;

DROP TRIGGER C_TIUBR_VAL_DNS_TYPE;

DROP TRIGGER TUB_VAL_DNS_TYPE;

DROP TRIGGER C_TIUBR_VAL_ENCAPSULATION_MODE;

DROP TRIGGER TUB_VAL_ENCAPSULATION_MODE;

DROP TRIGGER C_TIUBR_VAL_ENCAPSULATION_TYPE;

DROP TRIGGER TUB_VAL_ENCAPSULATION_TYPE;

DROP TRIGGER C_TIUBR_VAL_ENCRYPT_KEY_PURP;

DROP TRIGGER TUB_VAL_ENCRYPT_KEY_PURPOSE;

DROP TRIGGER C_TIUBR_VAL_ENCRYPT_METHOD;

DROP TRIGGER TUB_VAL_ENCRYPT_METHOD;

DROP TRIGGER TUB_VAL_FLOW_CONTROL;

DROP TRIGGER C_TIUBR_VAL_FLOW_CONTROL;

DROP TRIGGER C_TIUBR_VAL_IMAGE_TYPE;

DROP TRIGGER TUB_VAL_IMAGE_TYPE;

DROP TRIGGER C_CIUBR_VAL_IP_GROUP_PROTOCOL;

DROP TRIGGER TUB_VAL_IP_GROUP_PROTOCOL;

DROP TRIGGER C_TIUBR_REASON_FOR_ASSIGN;

DROP TRIGGER TUB_REASON_FOR_ASSIGN;

DROP TRIGGER C_TIUBR_VAL_LOGICAL_PORT_TYPE;

DROP TRIGGER TUB_LOGICAL_PORT_TYPE;

DROP TRIGGER C_TIUBR_VAL_NETBLOCK_COLLECTIO;

DROP TRIGGER TUB_VAL_NETBLOCK_COLLECTION_TY;

DROP TRIGGER C_TIUBR_VAL_NETBLOCK_STATUS;

DROP TRIGGER TUB_VAL_NETBLOCK_STATUS;

DROP TRIGGER C_TIUBR_VAL_NETBLOCK_TYPE;

DROP TRIGGER TUB_VAL_NETBLOCK_TYPE;

DROP TRIGGER C_TIUBR_VAL_NETWORK_INT_PURP;

DROP TRIGGER TUB_VAL_NETWORK_INTERFACE_PURP;

DROP TRIGGER C_TIUBR_VAL_NETWORK_INT_TYPE;

DROP TRIGGER TUB_VAL_NETWORK_INTERFACE_TYPE;

DROP TRIGGER C_TIUBR_VAL_NETWORK_SERVICE_TY;

DROP TRIGGER TUB_VAL_NETWORK_SERVICE_TYPE;

DROP TRIGGER C_TIUBR_VAL_OWNERSHIP_STATUS;

DROP TRIGGER TUB_VAL_OWNERSHIP_STATUS;

DROP TRIGGER TUB_VAL_PACKAGE_RELATION_TYPE;

DROP TRIGGER C_TIUBR_VAL_PACKAGE_RELATION_T;

DROP TRIGGER TUB_VAL_PARITY;

DROP TRIGGER C_TIUBR_VAL_PARITY;

DROP TRIGGER C_TIUBR_VAL_PASSWORD_TYPE;

DROP TRIGGER TUB_VAL_PASSWORD_TYPE;

DROP TRIGGER C_TIUBR_VAL_PERSON_COMPANY_REL;

DROP TRIGGER TUB_VAL_PRESON_COMPANY_RELATIO;

DROP TRIGGER C_TIUBR_VAL_PERSON_LOC_TYPE;

DROP TRIGGER TUB_VAL_PERSON_CONTACT_LOC_TYP;

DROP TRIGGER C_TIUBR_VAL_PERSON_CONTACT_TEC;

DROP TRIGGER TUB_VAL_PERSON_CONTACT_TECH;

DROP TRIGGER C_TIUBR_VAL_PERSON_CONTACT_TYP;

DROP TRIGGER TUB_VAL_PERSON_CONTACT_TYPE;

DROP TRIGGER C_TIUBR_VAL_PERSON_IMAGE_USAGE;

DROP TRIGGER TUB_VAL_PERSON_IMAGE_USAGE;

DROP TRIGGER C_TIUBR_VAL_USER_LOCATION_TYPE;

DROP TRIGGER TUB_VAL_USER_LOCATION_TYPE;

DROP TRIGGER C_TIUBR_VAL_PERSON_STATUS;

DROP TRIGGER TUB_VAL_PERSON_STATUS;

DROP TRIGGER C_TIUBR_VAL_PORT_MEDIUM;

DROP TRIGGER TUB_VAL_PORT_MEDIUM;

DROP TRIGGER C_TIUBR_VAL_PORT_PLUG_STYLE;

DROP TRIGGER C_TUB_PORT_PLUG_STYLE;

DROP TRIGGER C_TIUBR_VAL_PORT_PROTOCOL;

DROP TRIGGER TUB_VAL_PORT_PROTOCOL;

DROP TRIGGER C_TIUBR_VAL_PORT_PROTOCOL_SPEE;

DROP TRIGGER TUB_VAL_PORT_PROTOCOL_SPEED;

DROP TRIGGER TUB_VAL_PORT_PURPOSE;

DROP TRIGGER C_TIUBR_VAL_PORT_PURPOSE;

DROP TRIGGER C_TIUBR_VAL_PORT_SPEED;

DROP TRIGGER TUB_VAL_PORT_SPEED;

DROP TRIGGER C_TIUBR_VAL_PORT_TYPE;

DROP TRIGGER TUB_VAL_PORT_TYPE;

DROP TRIGGER C_TIUBR_VAL_POWER_PLUG_STYLE;

DROP TRIGGER TUB_VAL_POWER_PLUG_STYLE;

DROP TRIGGER TUB_VAL_PROCESSOR_ARCHITECTURE;

DROP TRIGGER C_TIUBR_VAL_PROCESSOR_ARCHITEC;

DROP TRIGGER C_TIUBR_VAL_PRODUCTION_STATE;

DROP TRIGGER TUB_VAL_PRODUCTION_STATE;

DROP TRIGGER C_TIUBR_VAL_PROPERTY;

DROP TRIGGER TUB_VAL_PROPERTY;

DROP TRIGGER C_TIUBR_VAL_PROPERTY_DATA_TYPE;

DROP TRIGGER TUB_VAL_PROPERTY_DATA_TYPE;

DROP TRIGGER C_TIUBR_VAL_PROPERTY_TYPE;

DROP TRIGGER TUB_VAL_PROPERTY_TYPE;

DROP TRIGGER C_TIUBR_VAL_PROPERTY_VALUE;

DROP TRIGGER TUB_VAL_PROPERTY_VALUE;

DROP TRIGGER C_TIUBR_VAL_RACK_TYPE;

DROP TRIGGER TUB_VAL_RACK_TYPE;

DROP TRIGGER C_TIUBR_VAL_SVC_ENV_COLL_TYPE;

DROP TRIGGER TUB_VAL_SVC_ENV_COLL_TYPE;

DROP TRIGGER C_TIUBR_VAL_SNMP_COMMSTR_TYPE;

DROP TRIGGER TUB_VAL_SNMP_COMMSTR_TYPE;

DROP TRIGGER C_TIUBR_VAL_SSH_KEY_TYPE;

DROP TRIGGER TUB_VAL_SSH_KEY_TYPE;

DROP TRIGGER TUB_VAL_STOP_BITS;

DROP TRIGGER C_TIUBR_VAL_STOP_BITS;

DROP TRIGGER TUB_VAL_SW_PACKAGE_FORMAT;

DROP TRIGGER C_TIUBR_VAL_SW_PACKAGE_FORMAT;

DROP TRIGGER TUB_VAL_SW_PACKAGE_TYPE;

DROP TRIGGER C_TIUBR_VAL_SW_PACKAGE_TYPE;

DROP TRIGGER TUB_VAL_SYMBOLIC_TRACK_NAME;

DROP TRIGGER C_TIUBR_VAL_SYMBOLIC_TRACK_NAM;

DROP TRIGGER TUB_TOKEN_COL_TYPE;

DROP TRIGGER C_TIUBR_VAL_TOKEN_COL_TYPE;

DROP TRIGGER TUB_VAL_TOKEN_STATUS;

DROP TRIGGER C_TIUBR_VAL_TOKEN_STATUS;

DROP TRIGGER TUB_VAL_TOKEN_TYPE;

DROP TRIGGER C_TIUBR_VAL_TOKEN_TYPE;

DROP TRIGGER TUB_VAL_UPGRADE_SEVERITY;

DROP TRIGGER C_TIUBR_VAL_UPGRADE_SEVERITY;

DROP TRIGGER TUB_VAL_VOE_STATE;

DROP TRIGGER C_TIUBR_VAL_VOE_STATE;

DROP TRIGGER C_TIUBR_CERT_FILE_FMT;

DROP TRIGGER TUB_CERT_FILE_FMT;

DROP TRIGGER C_TIUBR_X509_KEY_USAGE;

DROP TRIGGER TUB_X509_KEY_USAGE;

DROP TRIGGER C_TIUBR_KEY_USAGE_CATEGORY;

DROP TRIGGER TUB_X509_KEY_USAGE_CAT;

DROP TRIGGER C_TIUBR_VOE;

DROP TRIGGER TIB_VOE;

DROP TRIGGER TUB_VOE;

DROP TRIGGER C_TIUBR_VOE_RELATION;

DROP TRIGGER TUB_VOE_RELATION;

DROP TRIGGER C_TIUBR_VOE_SW_PACKAGE;

DROP TRIGGER TUB_VOE_SW_PACKAGE;

DROP TRIGGER TIB_VOE_SYMBOLIC_TRACK;

DROP TRIGGER TUB_VOE_SYMBOLIC_TRACK;

DROP TRIGGER C_TIUBR_VOE_SYMBOLIC_TRACK;

DROP TRIGGER C_TIUBR_X509_CERTIFICATE;

DROP TRIGGER TIB_X509_CERTIFICATE;

DROP TRIGGER TUB_X509_CERTIFICATE;

DROP TRIGGER C_TIUBR_KEY_USAGE_ATTRB;

DROP TRIGGER TUB_KEY_USAGE_ATTRB;

DROP TRIGGER C_TIUBR_KEY_USAGE_CTGRZTION;

DROP TRIGGER TUB_KEY_USAGE_CATEGRZTN;
