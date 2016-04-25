--
-- Copyright (c) 2016 Todd Kover
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
-- inserts the country data into the val table
--
-- $Id$
--

INSERT INTO val_country_code (
	iso_country_code,dial_country_code,primary_iso_currency_code,
	country_name,display_priority
) VALUES
	('AD','376',NULL,'Andorra',NULL),
	('AE','971',NULL,'United Arab Emirates',NULL),
	('AF','93','AFN','Afghanistan',NULL),
	('AG','1',NULL,'Antigua and Barbuda',NULL),
	('AI','1',NULL,'Anguilla',NULL),
	('AL','355','ALL','Albania',NULL),
	('AM','374',NULL,'Armenia',NULL),
	('AN','599','ANG','Netherlands Antilles',NULL),
	('AO','244',NULL,'Angola',NULL),
	('AQ','672',NULL,'Antarctica',NULL),
	('AR','54','ARS','Argentina',NULL),
	('AS','684',NULL,'American Samoa',NULL),
	('AT','43',NULL,'Austria',NULL),
	('AU','61','AUD','Australia',NULL),
	('AW','297','AWG','Aruba',NULL),
	('AZ','994','AZN','Azerbaijan',NULL),
	('BA','387','BAM','Bosnia and Herzegovina',NULL),
	('BB','1','BBD','Barbados',NULL),
	('BD','880',NULL,'Bangladesh',NULL),
	('BE','32',NULL,'Belgium',NULL),
	('BF','226',NULL,'Burkina Faso',NULL),
	('BG','359','BGN','Bulgaria',NULL),
	('BH','973',NULL,'Bahrain',NULL),
	('BI','257',NULL,'Burundi',NULL),
	('BJ','229',NULL,'Benin',NULL),
	('BM','1','BMD','Bermuda',NULL),
	('BN','673','BND','Brunei Darussalam',NULL),
	('BO','591','BOB','Bolivia',NULL),
	('BR','55','BRL','Brazil',NULL),
	('BS','1','BSD','Bahamas',NULL),
	('BT','975',NULL,'Bhutan',NULL),
	('BW','267','BWP','Botswana',NULL),
	('BY','375','BYR','Belarus',NULL),
	('BZ','501','BZD','Belize',NULL),
	('CA','1','CAD','Canada',NULL),
	('CC','61',NULL,'Cocos (Keeling) Islands',NULL),
	('CD','243',NULL,'Congo, The Democratic Republic of the',NULL),
	('CF','236',NULL,'Central African Republic',NULL),
	('CG','242',NULL,'Congo',NULL),
	('CH','41','CHF','Switzerland',NULL),
	('CI','225',NULL,'Cote d Ivoire',NULL),
	('CK','682',NULL,'Cook Islands',NULL),
	('CL','56','CLP','Chile',NULL),
	('CM','237',NULL,'Cameroon',NULL),
	('CN','86','CNY','China',NULL),
	('CO','57','COP','Colombia',NULL),
	('CR','506','CRC','Costa Rica',NULL),
	('CS','381',NULL,'Serbia and Montenegro',NULL),
	('CU','53','CUP','Cuba',NULL),
	('CV','238',NULL,'Cape Verde',NULL),
	('CX','61',NULL,'Christmas Island',NULL),
	('CY','357',NULL,'Cyprus',NULL),
	('CZ','420','CZK','Czech Republic',NULL),
	('DE','49',NULL,'Germany',NULL),
	('DJ','253',NULL,'Djibouti',NULL),
	('DK','45','DKK','Denmark',NULL),
	('DM','1',NULL,'Dominica',NULL),
	('DO','1','DOP','Dominican Republic',NULL),
	('DZ','213',NULL,'Algeria',NULL),
	('EC','593',NULL,'Ecuador',NULL),
	('EE','372',NULL,'Estonia',NULL),
	('EG','20','EGP','Egypt',NULL),
	('ER','291',NULL,'Eritrea',NULL),
	('ES','34',NULL,'Spain',NULL),
	('ET','251',NULL,'Ethiopia',NULL),
	('FI','358',NULL,'Finland',NULL),
	('FJ','679','FJD','Fiji',NULL),
	('FK','500','FKP','Falkland Islands (Malvinas)',NULL),
	('FM','691',NULL,'Federated States of Micronesia',NULL),
	('FO','298',NULL,'Faroe Islands',NULL),
	('FR','33',NULL,'France',NULL),
	('GA','241',NULL,'Gabon',NULL),
	('GB','44','GBP','United Kingdom',NULL),
	('GD','1',NULL,'Grenada',NULL),
	('GE','995',NULL,'Georgia',NULL),
	('GF','594',NULL,'French Guiana',NULL),
	('GH','233','GHS','Ghana',NULL),
	('GI','350','GIP','Gibraltar',NULL),
	('GL','299',NULL,'Greenland',NULL),
	('GM','220',NULL,'Gambia',NULL),
	('GN','224',NULL,'Guinea',NULL),
	('GP','590',NULL,'Guadeloupe',NULL),
	('GQ','240',NULL,'Equatorial Guinea',NULL),
	('GR','30',NULL,'Greece',NULL),
	('GT','502','GTQ','Guatemala',NULL),
	('GU','1',NULL,'Guam',NULL),
	('GW','245',NULL,'Guinea-Bissau',NULL),
	('GY','592','GYD','Guyana',NULL),
	('HK','852','HKD','Hong Kong',NULL),
	('HN','504','HNL','Honduras',NULL),
	('HR','385','HRK','Croatia',NULL),
	('HT','509',NULL,'Haiti',NULL),
	('HU','36','HUF','Hungary',NULL),
	('ID','62','IDR','Indonesia',NULL),
	('IE','353',NULL,'Ireland',NULL),
	('IL','972','ILS','Israel',NULL),
	('IN','91','INR','India',NULL),
	('IQ','964',NULL,'Iraq',NULL),
	('IR','98','IRR','Iran, Islamic Republic of',NULL),
	('IS','354','ISK','Iceland',NULL),
	('IT','39',NULL,'Italy',NULL),
	('JM','1','JMD','Jamaica',NULL),
	('JO','962',NULL,'Jordan',NULL),
	('JP','81','JPY','Japan',NULL),
	('KE','254',NULL,'Kenya',NULL),
	('KG','996','KGS','Kyrgyzstan',NULL),
	('KH','855','KHR','Cambodia',NULL),
	('KI','686',NULL,'Kiribati',NULL),
	('KM','269',NULL,'Comoros',NULL),
	('KN','1',NULL,'Saint Kitts and Nevis',NULL),
	('KP','82','KPW','Democratic Peoples Republic of Korea',NULL),
	('KR','850','KRW','Republic of Korea',NULL),
	('KW','965',NULL,'Kuwait',NULL),
	('KY','1','KYD','Cayman Islands',NULL),
	('KZ','7','KZT','Kazakhstan',NULL),
	('LA','856','LAK','Laos',NULL),
	('LB','961','LBP','Lebanon',NULL),
	('LC','1',NULL,'Saint Lucia',NULL),
	('LI','423',NULL,'Liechtenstein',NULL),
	('LK','34','LKR','Sri Lanka',NULL),
	('LR','231','LRD','Liberia',NULL),
	('LS','266',NULL,'Lesotho',NULL),
	('LT','370',NULL,'Lithuania',NULL),
	('LU','352',NULL,'Luxembourg',NULL),
	('LV','371',NULL,'Latvia',NULL),
	('LY','218',NULL,'Libya',NULL),
	('MA','212',NULL,'Morocco',NULL),
	('MC','377',NULL,'Monaco',NULL),
	('MD','373',NULL,'Moldova, Republic of',NULL),
	('MG','261',NULL,'Madagascar',NULL),
	('MH','692',NULL,'Marshall Islands',NULL),
	('MK','389','MKD','Macedonia',NULL),
	('ML','223',NULL,'Mali',NULL),
	('MM','95',NULL,'Myanmar',NULL),
	('MN','976','MNT','Mongolia',NULL),
	('MO','853',NULL,'Macao',NULL),
	('MP','1',NULL,'Northern Mariana Islands',NULL),
	('MQ','596',NULL,'Martinique',NULL),
	('MR','222',NULL,'Mauritania',NULL),
	('MS','1',NULL,'Montserrat',NULL),
	('MT','356',NULL,'Malta',NULL),
	('MU','230','MUR','Mauritius',NULL),
	('MV','960',NULL,'Maldives',NULL),
	('MW','265',NULL,'Malawi',NULL),
	('MX','52','MXN','Mexico',NULL),
	('MY','60','MYR','Malaysia',NULL),
	('MZ','258','MZN','Mozambique',NULL),
	('NA','264','NAD','Namibia',NULL),
	('NC','687',NULL,'New Caledonia',NULL),
	('NE','227',NULL,'Niger',NULL),
	('NF','672',NULL,'Norfolk Island',NULL),
	('NG','234','NGN','Nigeria',NULL),
	('NI','505','NIO','Nicaragua',NULL),
	('NL','31',NULL,'Netherlands',NULL),
	('NO','47','NOK','Norway',NULL),
	('NP','977','NPR','Nepal',NULL),
	('NR','674',NULL,'Nauru',NULL),
	('NU','683',NULL,'Niue',NULL),
	('NZ','64','NZD','New Zealand',NULL),
	('OM','968','OMR','Oman',NULL),
	('PA','507','PAB','Panama',NULL),
	('PE','51','PEN','Peru',NULL),
	('PF','689',NULL,'French Polynesia',NULL),
	('PG','675',NULL,'Papua New Guinea',NULL),
	('PH','63','PHP','Philippines',NULL),
	('PK','92','PKR','Pakistan',NULL),
	('PL','48','PLN','Poland',NULL),
	('PM','508',NULL,'Saint Pierre and Miquelon',NULL),
	('PR','1',NULL,'Puerto Rico',NULL),
	('PS','970',NULL,'Palestinian Settlements',NULL),
	('PT','351',NULL,'Portugal',NULL),
	('PW','680',NULL,'Palau',NULL),
	('PY','595','PYG','Paraguay',NULL),
	('QA','974','QAR','Qatar',NULL),
	('RE','262',NULL,'Reunion',NULL),
	('RO','40','RON','Romania',NULL),
	('RU','7','RUB','Russian Federation',NULL),
	('RW','250',NULL,'Rwanda',NULL),
	('SA','966','SAR','Saudi Arabia',NULL),
	('SB','677','SBD','Solomon Islands',NULL),
	('SC','248','SCR','Seychelles',NULL),
	('SD','249',NULL,'Sudan',NULL),
	('SE','46','SEK','Sweden',NULL),
	('SG','65','SGD','Singapore',NULL),
	('SH','290','SHP','Saint Helena',NULL),
	('SI','386',NULL,'Slovenia',NULL),
	('SK','421',NULL,'Slovakia',NULL),
	('SL','232',NULL,'Sierra Leone',NULL),
	('SM','378',NULL,'San Marino',NULL),
	('SN','221',NULL,'Senegal',NULL),
	('SO','252','SOS','Somalia',NULL),
	('SR','597','SRD','Suriname',NULL),
	('ST','239',NULL,'Sao Tome and Principe',NULL),
	('SV','503','SVC','El Salvador',NULL),
	('SY','963','SYP','Syria',NULL),
	('SZ','268',NULL,'Swaziland',NULL),
	('TC','1',NULL,'Turks and Caicos Islands',NULL),
	('TD','235',NULL,'Chad',NULL),
	('TG','228',NULL,'Togo',NULL),
	('TH','66','THB','Thailand',NULL),
	('TJ','992',NULL,'Tajikistan',NULL),
	('TK','690',NULL,'Tokelau',NULL),
	('TM','993',NULL,'Turkmenistan',NULL),
	('TN','216',NULL,'Tunisia',NULL),
	('TO','676',NULL,'Tonga',NULL),
	('TR','90','TRY','Turkey',NULL),
	('TT','1','TTD','Trinidad and Tobago',NULL),
	('TV','688','TVD','Tuvalu',NULL),
	('TW','886','TWD','Taiwan',NULL),
	('TZ','255',NULL,'Tanzania',NULL),
	('UA','380','UAH','Ukraine',NULL),
	('UG','256',NULL,'Uganda',NULL),
	('UM','1',NULL,'United States Minor Outlying Islands',NULL),
	('US','1','USD','United States',NULL),
	('UY','598','UYU','Uruguay',NULL),
	('UZ','998','UZS','Uzbekistan',NULL),
	('VA','39',NULL,'Vatican City',NULL),
	('VC','1',NULL,'Saint Vincent and the Grenadines',NULL),
	('VE','58','VEF','Venezuela',NULL),
	('VG','1',NULL,'British Virgin Islands',NULL),
	('VI','1',NULL,'US Virgin Islands',NULL),
	('VN','84','VND','Vietnam',NULL),
	('VU','678',NULL,'Vanuatu',NULL),
	('WF','681',NULL,'Wallis and Futuna',NULL),
	('WS','685',NULL,'Samoa',NULL),
	('YE','967','YER','Yemen',NULL),
	('YT','269',NULL,'Mayotte',NULL),
	('ZA','27','ZAR','South Africa',NULL),
	('ZM','260',NULL,'Zambia',NULL),
	('ZW','263','ZWD','Zimbabwe',NULL)
;
